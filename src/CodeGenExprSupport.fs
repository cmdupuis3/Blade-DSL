// Expression-emission support: the CUDA device dialect gate, complex-type
// vocabulary, operator rendering, capture forwarding, fresh-return facts,
// dep-idx extent solving, and the fp-reassoc lane/SIMD builders.
module Blade.CodeGenExprSupport

open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.Types
open Blade.EmitCpp
open Blade.ReynoldsCore
open Blade.CodeGenState

// CUDA device dialect (complex over CUDA): std::complex's operators are
// HOST-ONLY under nvcc, so device code speaks thrust::complex instead
// (layout-compatible, both T[2]). The extern "C" wrapper SIGNATURES keep the
// std:: spelling (text-copied into the host .cpp; cudaMemcpy's void* params
// reinterpret with no casts). The gate below lets kernel BODIES emit thrust
// vocabulary while host rendering is unaffected.

/// AsyncLocal dialect gate (mirrors the other per-flow emission cells).
let internal cudaDeviceDialectStorage =
    System.Threading.AsyncLocal<bool ref>()

let internal cudaDeviceDialectCell () : bool ref =
    let v = cudaDeviceDialectStorage.Value
    if isNull (box v) then
        let fresh = ref false
        cudaDeviceDialectStorage.Value <- fresh
        fresh
    else v

/// Whether device-dialect (thrust) complex rendering is active.
let inCudaDeviceDialect () : bool = (cudaDeviceDialectCell ()).Value

/// Run `f` with device-dialect complex rendering ON, restoring on exit.
let withCudaDeviceDialect (f: unit -> 'a) : 'a =
    let cell = cudaDeviceDialectCell ()
    let prev = cell.Value
    cell.Value <- true
    try f () finally cell.Value <- prev

/// Device (.cu kernel / device-buffer) spelling of an element type: complex
/// maps to thrust::complex; everything else matches elemTypeToCpp. Matched
/// via AnyPrimElem FIRST so a unit-annotated or idx-tagged complex erases to
/// the thrust type (the .cu has no host typedef aliases).
let cudaDevElemTypeToCpp (ty: IRType) : string =
    match ty with
    | AnyPrimElem ETComplex64 -> "thrust::complex<float>"
    | AnyPrimElem ETComplex128 -> "thrust::complex<double>"
    | _ -> elemTypeToCpp ty

/// Spelling of a complex scalar type in the current dialect.
let complexCppTypeName (et: ElemType) : string =
    match et, inCudaDeviceDialect () with
    | ETComplex64, false -> "std::complex<float>"
    | ETComplex64, true -> "thrust::complex<float>"
    | _, false -> "std::complex<double>"
    | _, true -> "thrust::complex<double>"


/// Extract the element type from an expression that should be array-shaped or scalar.
/// On failure (type not array/scalar after upstream inference), record a codegen
/// warning and emit a `#error` line as the rendered code. The point is to fail
/// loudly rather than silently emit code that might miscompile -- e.g., indexing
/// with a float, or narrowing int64 keys to double in a sort comparator.
/// Returns (elemType as IRType, optional error code).
let inferElemTypeStrict (ctx: CodeGenContext) (ind: string) (expr: IRExpr) (opName: string) : IRType * string list =
    match inferExprType expr with
    | ArrayElem arr -> (arr.ElemType, [])
    | IRTScalar _ as t -> (t, [])
    | t ->
        let msg = sprintf "%s: could not determine element type from expression (got %A) -- likely a typechecker or IR bug" opName t
        let errLines = codegenError ctx ind msg
        // Sentinel: the #error makes the C++ refuse to compile, so the
        // Float64 we return is never actually exercised in valid output.
        (IRTScalar ETFloat64, errLines)


/// Variant of inferElemTypeStrict for ctx-less callers (e.g. inside
/// `exprToCpp`). On failure, records a warning into the AsyncLocal
/// exprWarnings collector and returns the sentinel `BLADE_UNRESOLVED_INLINE_ELEM_TYPE`
/// (not a valid C++ identifier fragment in context) so a regression surfaces
/// as a precise g++ "unknown type name" error rather than a silent `double`.
let inferInlineElemTypeStr (opName: string) (form: IRExpr) : string =
    let arrExpr =
        match form with
        // IRMask deliberately NOT extracted: its result elem is Bool (the
        // presence array), independent of the source elem -- fall through to
        // `form` so inferExprType's IRMask arm answers.
        | IRSort (a, _)
        | IRIntersect (a, _) | IRUnion (a, _) -> a
        | IRUnique a -> a
        // stack/join carry no type of their own (Ir.CarriedType leaves the
        // rank-changing assembly combinators untyped); their result element
        // type is their operands', which TypeCheck has already unified.
        | IRStack (a :: _) | IRJoin (a :: _, _) -> a
        // eigh's RESULT is a tuple, which has no single element type -- asking
        // this function for one would collect a spurious "unresolvable element
        // type" warning and hand back the poison sentinel on a perfectly good
        // program. Answer the OPERAND's element type instead. The value is
        // never consumed (materializeEighForm ignores the caller's string and
        // derives BOTH element types itself, since Q's and LAM's can differ),
        // so this arm exists purely to keep the diagnostic honest.
        | IREigh a -> a
        | IRLu a -> a
        | _ -> form
    match inferExprType arrExpr with
    | ArrayElem a -> elemTypeToCpp a.ElemType
    // AnyPrimElem, not IRTScalar: a scalar under a TRANSPARENT wrapper --
    // IRTUnitAnnotated (physical units) or IRTIdxTagged (nominal index tag)
    // -- is a perfectly resolved element type. Both wrappers preserve their
    // inner type and erase at codegen (elemTypeToCpp/irTypeToCpp already
    // unwrap them), so matching only the bare form made a kernel-local
    // `let d = a - b` over Float<meters> elements collect a spurious
    // "likely a typechecker or IR bug" warning.
    | AnyPrimElem et -> primTypeToCpp et
    | t ->
        let cell = exprWarningsCell ()
        cell.Value <- cell.Value @
            [sprintf "%s: could not determine element type from inline form (got %A) -- likely a typechecker or IR bug" opName t]
        "BLADE_UNRESOLVED_INLINE_ELEM_TYPE"


// C++ Expression Generation

/// Convert binary operator to C++ string
let binOpToCpp = function
    | IRAdd -> "+" | IRSub -> "-" | IRMul -> "*" | IRDiv -> "/"
    | IRMod -> "%" | IRCaret -> "pow"  // Special handling needed
    | IREq -> "==" | IRNeq -> "!=" 
    | IRLt -> "<" | IRLe -> "<=" | IRGt -> ">" | IRGe -> ">="
    | IRAnd -> "&&" | IROr -> "||"
    | IRMath2 name -> name   // call-shaped, like IRCaret; see renderMath2

/// Render a BINARY math intrinsic. Call-shaped rather than infix, the same
/// exception `^` already needs (`pow(l, r)`), so every IRBinOp emission site
/// routes IRMath2 here before reaching binOpToCpp.
///   atan2(y, x)    -> std::atan2(y, x)
///   log_base(x, b) -> (std::log(x) / std::log(b))
/// There is no std::log_base; the quotient IS the definition, and emitting it
/// inline keeps the interpreter's mirror (Numerics.evalArith) a one-liner over
/// the same two std::log calls. Both operands are real by construction
/// (TypeCheck rejects complex ones), so no complex coercion is needed. In the
/// CUDA device dialect the names go UNQUALIFIED, matching renderUnaryOpTyped's
/// real-operand rule -- CUDA's device overloads live in the global namespace.
let renderMath2 (name: string) (lStr: string) (rStr: string) : string =
    let q (fn: string) = if inCudaDeviceDialect () then fn else "std::" + fn
    match name with
    | "log_base" -> $"""({(q "log")}({lStr}) / {(q "log")}({rStr}))"""
    | _ -> $"{(q name)}({lStr}, {rStr})"

/// Convert unary operator to C++ string
let unaryOpToCpp = function
    | IRNeg -> "-"
    | IRNot -> "!"
    | IRConj -> "std::conj"   // function-call form; exprToCppCore/exprToCppSimple
                              // special-case IRConj for the complex-vs-real
                              // decision (real conj is the identity)
    // std::real/std::imag/std::arg are <complex> free functions with C++11
    // arithmetic-type overloads (std::real(double)=x, std::imag(double)=0,
    // std::arg(double)=0/pi), so they render through the generic unary arm on
    // both complex and real operands with no special-casing.
    | IRReal -> "std::real"
    | IRImag -> "std::imag"
    | IRArg -> "std::arg"
    // lgamma and digamma are Blade's OWN (blade_runtime.hpp), not libm's: the
    // interpreter has to reproduce them bit for bit and can borrow neither
    // ucrtbase's nor .NET's (there is no .NET gamma, and no libm psi at all).
    // Both sides run the same hand-rolled series -- see the header, and
    // Interp/Numerics.fs lgammaLanczos / digammaSeries.
    | IRMath "lgamma" -> "blade_rt::lgamma"
    | IRMath "digamma" -> "blade_rt::digamma"
    | IRMath name -> "std::" + name  // function-call form via the generic
                                     // `op(expr)` unary arm
    // Explicit numeric cast. Complex targets use the constructor form
    // (std::complex<T>(x) -- also the licensed spelling for the EXPLICIT
    // complex<float>(complex<double>) narrowing ctor); everything else is
    // static_cast. complexCppTypeName picks thrust:: in the device dialect;
    // static_cast is dialect-neutral.
    | IRCast ((ETComplex64 | ETComplex128) as et) -> complexCppTypeName et
    | IRCast et -> $"static_cast<{primTypeToCpp et}>"

/// Namespace-qualified spelling of a <complex>-vocabulary function in the
/// current dialect (std:: on the host, thrust:: inside CUDA device bodies).
let complexFnName (name: string) : string =
    (if inCudaDeviceDialect () then "thrust::" else "std::") + name

/// True iff a type's underlying scalar is a complex element type. Used to
/// decide whether conj must emit std::conj (complex) or is the identity (real).
let rec isComplexType (t: IRType) : bool =
    match t with
    | IRTScalar (ETComplex64 | ETComplex128) -> true
    | IRTIdxTagged (inner, _) -> isComplexType inner
    | IRTUnitAnnotated (inner, _) -> isComplexType inner
    | _ -> false

/// Project an IRType to its underlying scalar element type, if any.
let rec scalarElemOf (t: IRType) : ElemType option =
    match t with
    | IRTScalar et -> Some et
    | IRTIdxTagged (inner, _) -> scalarElemOf inner
    | IRTUnitAnnotated (inner, _) -> scalarElemOf inner
    | _ -> None

/// Render a function-call unary op, choosing the spelling by operand type and
/// active dialect. Host rendering is unaryOpToCpp verbatim. In the CUDA device
/// dialect: complex operands use thrust:: (thrust has no free real()/imag(), so
/// those emit __host__ __device__ member accessors instead); real operands emit
/// math intrinsics UNQUALIFIED (`exp(x)`) since CUDA's device overloads live in
/// the global namespace. Shared by exprToCppCore and exprToCppSimple.
let renderUnaryOpTyped (op: IRUnaryOp) (operandTy: IRType) (inner: string) : string =
    if inCudaDeviceDialect () then
        if isComplexType operandTy then
            match op with
            | IRReal -> $"({inner}).real()"
            | IRImag -> $"({inner}).imag()"
            | IRArg -> $"thrust::arg({inner})"
            | IRMath name -> $"thrust::{name}({inner})"
            | _ -> $"{(unaryOpToCpp op)}({inner})"
        else
            match op with
            | IRMath name -> $"{name}({inner})"
            | _ -> $"{(unaryOpToCpp op)}({inner})"
    else
        $"{(unaryOpToCpp op)}({inner})"

/// Coerce a rendered scalar operand to match std::complex's SAME-TYPE-ONLY
/// operator overload set (`complex<double> * 2` and `complex<double> *
/// complex<float>` both fail template deduction): integers and mismatched-width
/// floats cast to the component real type; a narrower complex widens via
/// std::complex's converting constructor. Matching types pass through unchanged.
let coerceComplexOperand (resultElem: ElemType) (operandElem: ElemType) (rendered: string) : string =
    match resultElem with
    | ETComplex128 ->
        match operandElem with
        | ETComplex128 | ETFloat64 -> rendered
        | ETComplex64 -> $"{(complexCppTypeName ETComplex128)}({rendered})"
        | ETFloat32 | ETInt64 | ETInt32 -> $"(double)({rendered})"
        | _ -> rendered
    | ETComplex64 ->
        match operandElem with
        | ETComplex64 | ETFloat32 -> rendered
        | ETInt64 | ETInt32 -> $"(float)({rendered})"
        | _ -> rendered
    | _ -> rendered

/// Emit a binop, inserting complex-operand coercions when the promoted result
/// is complex. `renderBin` is the caller's fallback (`(l op r)`), used verbatim
/// for the common non-complex path so nothing changes there.
let emitBinOpWithComplexCoercion
        (op: IRBinOp) (l: IRExpr) (r: IRExpr) (lStr: string) (rStr: string)
        (inferTy: IRExpr -> IRType) (binToCpp: IRBinOp -> string) : string =
    match scalarElemOf (inferTy l), scalarElemOf (inferTy r) with
    | Some le, Some re ->
        match promoteElemType le re with
        | Some ((ETComplex64 | ETComplex128) as resElem) ->
            let lC = coerceComplexOperand resElem le lStr
            let rC = coerceComplexOperand resElem re rStr
            $"({lC} {(binToCpp op)} {rC})"
        | _ -> $"({lStr} {(binToCpp op)} {rStr})"
    | _ -> $"({lStr} {(binToCpp op)} {rStr})"

/// Render a float as a C++ double literal. `sprintf "%g"` would violate two
/// invariants: ROUND-TRIP precision (%g truncates to 6 sig figs, breaking any
/// test pinned finer than 1e-6) and FLOAT SPELLING (%g renders 2.0 as the bare
/// token `2`, an int literal, so `2.0 / 3.0` becomes integer division = 0).
/// "R" is shortest-round-trip; invariant culture guards decimal-comma locales;
/// the suffix check restores the `.0` spelling.
let floatToCppLiteral (f: float) : string =
    if System.Double.IsNaN f then "std::numeric_limits<double>::quiet_NaN()"
    elif System.Double.IsPositiveInfinity f then "std::numeric_limits<double>::infinity()"
    elif System.Double.IsNegativeInfinity f then "(-std::numeric_limits<double>::infinity())"
    else
        let s = f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
        if s.Contains "." || s.Contains "e" || s.Contains "E" then s else s + ".0"

/// The Float32 twin: an `f`-suffixed C++ literal, so float-context arithmetic
/// stays in float (a width-less `1.0` would promote the op to double and then
/// narrow at the store -- rejected by -Werror=float-conversion).
let float32ToCppLiteral (f: float32) : string =
    if System.Single.IsNaN f then "std::numeric_limits<float>::quiet_NaN()"
    elif System.Single.IsPositiveInfinity f then "std::numeric_limits<float>::infinity()"
    elif System.Single.IsNegativeInfinity f then "(-std::numeric_limits<float>::infinity())"
    else
        let s = f.ToString("R", System.Globalization.CultureInfo.InvariantCulture)
        (if s.Contains "." || s.Contains "e" || s.Contains "E" then s else s + ".0") + "f"

/// Quote a Blade string value as a C++ string literal. Escapes the minimal
/// set that would otherwise break the surrounding "..." token: backslash,
/// double-quote, and the four common control characters. Other characters
/// pass through, including UTF-8 multibyte sequences (which are valid as
/// raw bytes inside a C++ "..." literal).
let escapeStringLit (s: string) : string =
    let sb = System.Text.StringBuilder()
    sb.Append('"') |> ignore
    for c in s do
        match c with
        | '\\' -> sb.Append("\\\\") |> ignore
        | '"'  -> sb.Append("\\\"") |> ignore
        | '\n' -> sb.Append("\\n") |> ignore
        | '\r' -> sb.Append("\\r") |> ignore
        | '\t' -> sb.Append("\\t") |> ignore
        | '\000' -> sb.Append("\\0") |> ignore
        | _ -> sb.Append(c) |> ignore
    sb.Append('"') |> ignore
    sb.ToString()

/// Simplified exprToCpp that doesn't recurse into complex IR nodes
/// Used for kernel bodies in inline generation
let rec exprToCppSimple (names: Map<IRId, string>) (expr: IRExpr) : string =
    match expr with
    | IRLit (IRLitInt n) -> $"{n}L"
    | IRLit (IRLitFloat f) -> floatToCppLiteral f
    | IRLit (IRLitFloat32 f) -> float32ToCppLiteral f
    | IRLit (IRLitBool b) -> if b then "true" else "false"
    | IRLit (IRLitString s) -> $"std::string({(escapeStringLit s)})"
    | IRLit IRLitUnit -> "((void)0)"
    | IRVar (id, _) -> Map.tryFind id names |> Option.defaultValue ($"__v{id}")
    | IRParam (name, _, _) -> name
    | IRBinOp (_, op, l, r) ->
        let lStr = exprToCppSimple names l
        let rStr = exprToCppSimple names r
        match op with
        | IRCaret -> $"pow({lStr}, {rStr})"
        | IRMath2 name -> renderMath2 name lStr rStr
        | _ -> emitBinOpWithComplexCoercion op l r lStr rStr inferExprType binOpToCpp
    | IRFma (a, b, c) ->
        $"std::fma({(exprToCppSimple names a)}, {(exprToCppSimple names b)}, {(exprToCppSimple names c)})"
    | IRUnaryOp (IRConj, e) ->
        let inner = exprToCppSimple names e
        if isComplexType (inferExprType e) then $"""{(complexFnName "conj")}({inner})"""
        else inner
    | IRUnaryOp (op, e) -> renderUnaryOpTyped op (inferExprType e) (exprToCppSimple names e)
    | IRGuard (cond, body) ->
        $"({(exprToCppSimple names cond)} ? {(exprToCppSimple names body)} : 0.0)"
    | other -> $"BLADE_UNSUPPORTED_EXPR_{(other.GetType().Name.ToUpper())}"

/// Convert IRLit to C++ literal string
let litToCpp (lit: IRLit) : string =
    match lit with
    | IRLitInt n -> $"{n}L"
    | IRLitFloat f -> floatToCppLiteral f
    | IRLitFloat32 f -> float32ToCppLiteral f
    | IRLitBool b -> if b then "true" else "false"
    | IRLitString s -> $"std::string({(escapeStringLit s)})"
    | IRLitUnit -> "((void)0)"  // Valid C++ no-op; should be elided by callers

// isRaggedArrayType / isRaggedRowType / isDepIdxArrayType /
// isCompoundArrayType / isSparseArrayType are declared ABOVE the
// irTypeToCpp/elemTypeToCpp recursion group (search "Array-SHAPE predicates"),
// so that `cppArrayTypeStr` can join that group and every signature-rendering
// site inside it reaches the same wrapper decision.

/// Array types whose storage is the plain dense pool + pointer skeleton that
/// `deallocate` walks. Ragged, compound and dep-idx layouts carry side tables
/// (lens/offsets/row pointers, compound_index_t) with their own ownership rules.
/// (Declared here rather than in the deallocation registry below because the
/// copy-in-place mut eligibility analysis and the registry both need it.)
let isFreeableDenseArrayType (at: IRArrayType) : bool =
    not (isCompoundArrayType at) && not (isSparseArrayType at)
    && not (isRaggedArrayType at) && not (isDepIdxArrayType at)

// Copy-in-place mut cell (analysis lives with the deallocation block further down)

let internal copyInPlaceMutsStorage =
    System.Threading.AsyncLocal<Map<IRId, int> ref>()

/// Module-level mut array bindings whose whole-array reassignments compile to a
/// copy-into-place, mapped to the STATIC element count of the shared pool.
/// Filled by `computeCopyInPlaceMuts` (below, where the freshness helpers live);
/// read here so `exprToCppCore`'s IRAssign arm can consult it. AsyncLocal for the
/// same per-parallel-test-task isolation as exprWarningsCell.
let copyInPlaceMutsCell () : Map<IRId, int> ref =
    let v = copyInPlaceMutsStorage.Value
    if isNull (box v) then
        let fresh = ref Map.empty
        copyInPlaceMutsStorage.Value <- fresh
        fresh
    else v

/// The single decision point shared by the two IRAssign emission arms and
/// computeScopeEscapes: does THIS assign compile to a copy-into-place?
/// Returns (targetId, rhsId, element count). Eligibility already proved that
/// every whole-array assign to an eligible target has a plain-IRVar RHS of the
/// same static shape, so matching the two id slots here is sufficient.
let copyInPlaceAssign (target: IRExpr) (value: IRExpr) : (IRId * IRId * int) option =
    match target, value with
    | LVVar tid, IRVar (rid, _) ->
        match Map.tryFind tid (copyInPlaceMutsCell ()).Value with
        | Some n -> Some (tid, rid, n)
        | None -> None
    | _ -> None

// cppArrayTypeStr is defined ABOVE, inside the irTypeToCpp/elemTypeToCpp
// recursion group, so that arrowSlotTypeForFuncSig (which renders
// std::function<> slots and lives in that group) reaches the SAME wrapper
// decision the declaration sites use.

/// P0 (compound-index materialization keystone): emit the C++ that builds a
/// `compound_index_t<RANK>` from a Blade bool mask VALUE, independent of any
/// provider, so any source-level compound producer (dense-array scatter,
/// range<CompoundIdx> driver, fill_random-built compound) can materialize the
/// index the same way. `maskName` names an in-scope `Array<bool, RANK>` mask,
/// `rank` is RANK, `idxName` is the base name for the emitted index variable.
///
/// Emits (in order):
///   std::array<size_t, RANK> <idxName>_extents = { <maskName>.extents[0], ... };
///   size_t <idxName>_grid = <maskName>.extents[0] * ... ;
///   bool* <idxName>_pool = nested_array_utilities::pool_base(<maskName>.data);
///   std::vector<bool> <idxName>_maskvec(<idxName>_pool, <idxName>_pool + <idxName>_grid);
///   compound_index_t<RANK>* <idxName> = new compound_index_t<RANK>("<idxName>", <idxName>_extents, <idxName>_maskvec);
///
/// Flattening relies on allocate<>'s single-contiguous-pool invariant: pool_base
/// gives the row-major (DFS) flat buffer that compound_index_t's enumerate()
/// also walks, so the mask bit order matches the index's tuple enumeration.
/// Returns (emitted lines, index variable name). Heap-allocated; the caller owns
/// bundling it into a Compound<T,RANK> wrapper (P0b).
let genCompoundIndexFromMask (maskName: string) (rank: int) (idxName: string) : string list * string =
    let extentTerms = [ for d in 0 .. rank - 1 -> $"{maskName}.extents[{d}]" ]
    let extentsInit = String.concat ", " extentTerms
    let gridExpr = String.concat " * " extentTerms
    let lines =
        [ $$"""std::array<size_t, {{rank}}> {{idxName}}_extents = { {{extentsInit}} };"""
          $"size_t {idxName}_grid = {gridExpr};"
          $"bool* {idxName}_pool = nested_array_utilities::pool_base({maskName}.data);"
          $"std::vector<bool> {idxName}_maskvec({idxName}_pool, {idxName}_pool + {idxName}_grid);"
          $"compound_index_t<{rank}>* {idxName} = new compound_index_t<{rank}>(\"{idxName}\", {idxName}_extents, {idxName}_maskvec);" ]
    (lines, idxName)

/// Emit the construction of a standalone `sparse_index_t<RANK>` -- the sparse
/// twin of genCompoundIndexFromMask, keyed on the SparseKeysSource branch:
///
///   SkStatic entries -- the key table is a compile-time constant; emit it as a
///       braced vector literal. No runtime array is consulted (desync-proof).
///   SkRuntime keys   -- loop the named keys array (rank-1, std::tuple elements
///       for arity >= 2, plain integers for a rank-1 sparse) into the vector.
///       `keysName` is the resolved C++ name of that array.
///
/// Key order is preserved verbatim in both branches (given order == iteration
/// order). The index is heap-allocated; duplicate keys throw at construction
/// (sparse_index_t's ctor). Caller owns dealloc registration.
let genSparseIndexFromKeys (source: SparseKeysSource) (keysName: string option) (rank: int) (idxName: string) : string list =
    match source with
    | SkStatic entries ->
        let rows =
            entries
            |> List.map (fun e -> $$"""{ {{(e |> List.map string |> String.concat ", ")}} }""")
            |> String.concat ", "
        [ $$"""std::vector<std::array<size_t, {{rank}}>> {{idxName}}_keys = { {{rows}} };"""
          $"sparse_index_t<{rank}>* {idxName} = new sparse_index_t<{rank}>(\"{idxName}\", std::move({idxName}_keys));" ]
    | SkRuntime _ ->
        match keysName with
        | Some kn ->
            let entryInit =
                if rank = 1 then $$"""{ (size_t)({{kn}}[__r]) }"""
                else
                    [ for c in 0 .. rank - 1 -> $"(size_t)std::get<{c}>({kn}[__r])" ]
                    |> String.concat ", "
                    |> sprintf "{ %s }"
            [ $"std::vector<std::array<size_t, {rank}>> {idxName}_keys({kn}.extents[0]);"
              $"for (size_t __r = 0; __r < {kn}.extents[0]; __r++) {idxName}_keys[__r] = {entryInit};"
              $"sparse_index_t<{rank}>* {idxName} = new sparse_index_t<{rank}>(\"{idxName}\", std::move({idxName}_keys));" ]
        | None ->
            [ refusalErrorLine "" "SparseIdx: runtime keys variable not found in scope at codegen" ]

/// Resolve a capture to the C++ identifier naming it in the SCOPE where the
/// forwarding closure is emitted. A capture's `Name` is its SOURCE name,
/// correct for params/module-level bindings, but a block-local `let` is
/// renamed to `__v<id>` when its IRLet chain is flattened -- forwarding the
/// source name there would reference an undeclared identifier. Resolve
/// through the active name map (`names`/`ctx.VarNames`) and fall back to
/// the source name when unmapped.
let captureForwardName (names: Map<IRId, string>) (c: CaptureInfo) : string =
    Map.tryFind c.Id names |> Option.defaultValue c.Name

// GROUPED-CAPTURE FORWARDING.
//
// A `group_by` result is a row-pointer table (`Array<T*, 1>`, see
// genGroupByBinding) whose per-row lengths live OUTSIDE the value, in the
// driving `group_keys` binding's side state (`<gk>__ngroups`,
// `<gk>__offsets`). A lifted callable that captures one therefore needs
// three things the plain capture path cannot give it:
//
//   1. the row table typed as what it IS (`Array<T*, 1>&`) -- the IR rank-2
//      type would render `Array<T, 2>&`, which the call site's actual
//      `Array<T*, 1>` value cannot bind to;
//   2. the gk side state, forwarded as trailing hidden params (`size_t
//      __gk<id>__ngroups, const size_t* __gk<id>__offsets`), ONE pair per
//      DISTINCT gk across the captures -- the grouped-zip peel requires
//      co-grouped operands to resolve to the SAME stem, so two captures
//      grouped by one gk must share a pair;
//   3. a `ctx.GroupedArrays` seed in the callable's body context mapping the
//      capture's body-scope name to that stem, so every downstream consumer
//      (tryRaggedPeel, tryGroupedZipPeel, the peel-result extent sites)
//      resolves lengths exactly as it does in the frame that built the group.
//
// Parameter names and argument spellings agree by POSITION, not by text: the
// signature always spells the stem off the gk's VAR ID (`__gk<id>`), while a
// call site spells whatever the gk is named in ITS scope -- the emitted
// binding name where the gk is local (`__v<id>`/`gk`), or `__gk<id>` when the
// caller is itself a lifted callable holding the forwarded pair.
//
// Which capture is grouped comes from a module-level pre-pass
// (computeGroupedCaptureFacts): binding var id of a group_by result -> the
// gk binding's var id, harvested from `IRLet (id, IRGroupBy (_, IRVar gk), _)`
// in every function body plus module-level group_by bindings. HM
// specialization preserves body let ids (only param VarIds are remapped), so
// one fact set serves the original, the spec, and every lambda clone.
let internal groupedCaptureFactsStorage =
    System.Threading.AsyncLocal<Map<IRId, IRId> ref>()

let groupedCaptureFactsCell () : Map<IRId, IRId> ref =
    let v = groupedCaptureFactsStorage.Value
    if isNull (box v) then
        let fresh = ref Map.empty
        groupedCaptureFactsStorage.Value <- fresh
        fresh
    else v

let groupedCaptureGkOf (c: CaptureInfo) : IRId option =
    Map.tryFind c.Id (groupedCaptureFactsCell ()).Value

/// The C++ spelling of a gk's side-state stem in the CURRENT scope: the gk's
/// emitted name where it is in scope, `__gk<id>` (the forwarded-pair param
/// stem) where it is not.
let gkSidecarStem (names: Map<IRId, string>) (gkId: IRId) : string =
    match Map.tryFind gkId names with
    | Some n -> n
    | None -> $"__gk{gkId}"

/// Distinct gk ids across a callable's captures, in first-appearance order
/// (the order both the signature and every call site iterate).
let groupedCaptureGks (caps: CaptureInfo list) : IRId list =
    caps |> List.choose groupedCaptureGkOf |> List.distinct

/// Trailing hidden params carrying the gk side state, for genFuncDef and
/// genForwardDecls (which must agree token for token).
let gkSidecarParams (caps: CaptureInfo list) : string list =
    groupedCaptureGks caps
    |> List.collect (fun gkId ->
        [ $"size_t __gk{gkId}__ngroups"
          $"const size_t* __gk{gkId}__offsets" ])

/// Capture ARGUMENTS at a call/wrapper site: the regular captures resolved
/// through the active name map, then the gk side-state pairs in the same
/// order the signature declares them.
let captureForwardArgs (names: Map<IRId, string>) (caps: CaptureInfo list) : string list =
    (caps |> List.map (captureForwardName names))
    @ (groupedCaptureGks caps
       |> List.collect (fun gkId ->
           let stem = gkSidecarStem names gkId
           [ $"{stem}__ngroups"; $"{stem}__offsets" ]))

/// The pre-pass behind groupedCaptureGkOf: every `let <id> = group_by(_, gk)`
/// in any function body (and any module-level group_by binding) contributes
/// id -> gk's var id. Installed by genModule/genModuleSplit alongside the
/// other module-level fact caches.
let computeGroupedCaptureFacts (modul: IRModule) : Map<IRId, IRId> =
    let acc = System.Collections.Generic.Dictionary<IRId, IRId>()
    let strip e = match e with IRCompute inner -> inner | e -> e
    let note (id: IRId) (v: IRExpr) =
        match strip v with
        | IRGroupBy (_, gkE) ->
            (match strip gkE with
             | IRVar (gkId, _) -> acc.[id] <- gkId
             | _ -> ())
        | _ -> ()
    let walkBody (b: IRExpr) =
        iterIRExpr (fun e ->
            match e with
            | IRLet (id, v, _) -> note id v
            | _ -> ()) b
    for bind in modul.Bindings do
        note bind.Id bind.Value
        walkBody bind.Value
    for f in modul.Functions do
        walkBody f.Body
    acc |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> Map.ofSeq

/// EXTENTS-ONLY GROUP_BY: which group_by bindings never have their VALUES read.
///
/// A grouped array's only legal consumers are ragged peels, and a peel whose
/// kernel touches its row solely through `extents(row)` reads the row's LENGTH
/// -- which the emitter computes from the gk offsets, not from the gathered
/// buffer. So if EVERY use of a group_by result is such a peel, the gather is
/// dead: `genGroupByBinding` still emits the row-pointer table (the peel indexes
/// it to build each RaggedRow, and auto-print reads its extents) but skips the
/// per-group `new[]` and the O(n) copy, leaving the rows null. That is legal --
/// the pointer is read, never dereferenced -- and `delete[] nullptr` is a no-op,
/// so teardown is unchanged.
///
/// The user-facing point: `extents(gk)` is the direct spelling for per-group
/// sizes, and this makes the older `method_for(group_by(v, gk)) <@> lambda(r) ->
/// extents(r)` cost the same, so nobody has to know which one is fast.
///
/// FAIL-SAFE. The walk marks a binding BAD on any occurrence it cannot classify,
/// and only the classified-good set survives; an unrecognised use therefore
/// keeps the gather. Both directions are pinned (sql-group-by/043).
let computeExtentsOnlyGroupBys (modul: IRModule) : Set<IRId> =
    let strip e = match e with IRCompute inner -> inner | e -> e
    // Every id bound to a group_by, module-level or inside a body.
    let groupByIds = System.Collections.Generic.HashSet<IRId>()
    let noteBind (id: IRId) (v: IRExpr) =
        match strip v with IRGroupBy _ -> groupByIds.Add id |> ignore | _ -> ()
    for bind in modul.Bindings do
        noteBind bind.Id bind.Value
        iterIRExpr (fun e -> match e with IRLet (id, v, _) -> noteBind id v | _ -> ()) bind.Value
    for f in modul.Functions do
        iterIRExpr (fun e -> match e with IRLet (id, v, _) -> noteBind id v | _ -> ()) f.Body
    if groupByIds.Count = 0 then Set.empty else

    // A kernel is extents-only when its SOLE parameter is reached exclusively
    // through IRExtent. Counting works because iterIRExpr visits the IRExtent
    // node and its IRVar child both, so a row read any other way lifts `total`
    // without lifting `underExtent`.
    let kernelExtentsOnly (kernel: IRExpr) : bool =
        match resolveCallable kernel with
        | Some c when c.Params.Length = 1 ->
            let pid = c.Params.[0].VarId
            let mutable total = 0
            let mutable underExtent = 0
            iterIRExpr (fun e ->
                match e with
                | IRVar (i, _) when i = pid -> total <- total + 1
                | IRExtent (IRVar (i, _), _) when i = pid -> underExtent <- underExtent + 1
                | _ -> ()) c.Body
            total > 0 && total = underExtent
        | _ -> false

    // Sole grouped operand of a peel: co-iteration (Arrays > 1) binds rows from
    // several tables and is never elided.
    let soleGroupedOperand (info: ApplyInfo) : IRId option =
        match info.Arrays |> List.map strip with
        | [IRVar (gid, _)] when groupByIds.Contains gid -> Some gid
        | _ -> None

    // `bad` = the binding has at least one use that could read VALUES.
    //
    // Structural walk, not a count: an extents-only peel mentions its operand in
    // BOTH `Loop` and `Arrays`, so those subtrees are skipped wholesale rather
    // than tallied. Every other route to an `IRVar` naming a group_by marks it,
    // which is what makes the default "gather". The kernel is still walked -- it
    // is an IRVar naming a callable, and the callable's BODY is visited via
    // modul.Functions.
    let bad = System.Collections.Generic.HashSet<IRId>()
    // ExprShape is TOTAL (a leaf yields an empty child list), so it is the last
    // arm and needs no fallback after it.
    let rec scan (e: IRExpr) =
        match e with
        | IRApplyCombinator info ->
            (match soleGroupedOperand info with
             | Some gid when kernelExtentsOnly info.Kernel -> scan info.Kernel
             | _ -> (match e with ExprShape (cs, _) -> cs |> List.iter scan))
        | IRVar (i, _) when groupByIds.Contains i -> bad.Add i |> ignore
        | ExprShape (cs, _) -> cs |> List.iter scan
    for bind in modul.Bindings do scan bind.Value
    for f in modul.Functions do scan f.Body
    // Elidable is exactly the COMPLEMENT of `bad`, which is why a group_by
    // nothing consumes at all is elided too: its gather is dead for the same
    // reason, just more obviously. (Tracking the extents-only peels positively
    // and intersecting would exclude that case for no benefit -- an unused
    // binding has no use to classify, not an unclassifiable one.)
    Set.ofSeq groupByIds - Set.ofSeq bad

let internal extentsOnlyGroupBysStorage =
    System.Threading.AsyncLocal<Set<IRId> ref>()

let extentsOnlyGroupBysCell () : Set<IRId> ref =
    let v = extentsOnlyGroupBysStorage.Value
    if isNull (box v) then
        let fresh = ref Set.empty
        extentsOnlyGroupBysStorage.Value <- fresh
        fresh
    else v

/// Wrapper-emission helper: a local C++ closure mediating between a lifted
/// function's signature (regular + capture params) and a consumer's expected
/// shape (regular params only), so `IRVar(callable.Id, funcType)` can stand in
/// wherever the callable is called with just its regular params.
///
/// Shape: `auto __wrap_<id>_<suffix> = [&](P1 p1, P2 p2) { return <fnName>(p1, p2, c1, c2); };`
///
/// Captures are hidden from the wrapper's signature and forwarded by reference
/// via `[&]`, matching the `T& cap_name` capture-param signature `genFuncDef`
/// emits on the lifted side; resolved through `names` (`captureForwardName`) so
/// a renamed block-local `let` (`__v<id>`) forwards its EMITTED identifier.
///
/// Return type is `auto`: avoids computing an explicit type for IRTInfer/
/// synthesized RetTypes and handles void returns for free (C++14 deduces void).
///
/// Wrapper name = callable id + caller-supplied suffix, to disambiguate one
/// callable referenced from multiple consumer sites in the same C++ scope
/// (suffix is typically the let binding's name or a fresh counter; pass "" when
/// the caller guarantees a single emission per scope).
///
/// Returns (code lines, wrapper name); callers prepend the lines and use the
/// name wherever they'd otherwise inline the lambda body.
let genCallableWrapper (names: Map<IRId, string>) (suffix: string) (callable: IRCallable) : string list * string =
    let safeName = sanitizeCppName callable.Name
    let wrapperName =
        if suffix = "" then $"__wrap_{callable.Id}"
        else $"__wrap_{callable.Id}_{suffix}"
    let paramSig =
        callable.Params
        |> List.map (fun p ->
            match p.Type with
            | ArrayElem arr -> $"{(cppArrayTypeStr arr)} {p.Name}"
            | _ -> $"{(irTypeToCpp p.Type)} {p.Name}")
        |> String.concat ", "
    let regularArgs = callable.Params |> List.map (_.Name)
    let captureArgs = captureForwardArgs names callable.Captures
    let allArgs = (regularArgs @ captureArgs) |> String.concat ", "
    let code =
        [$$"""auto {{wrapperName}} = [&]({{paramSig}}) { return {{safeName}}({{allArgs}}); };"""]
    (code, wrapperName)

/// Whether a callee's return value is a pool the CALLER owns. `FreshPool` means
/// every return hands back storage this call allocated; `NotFresh` means it may
/// alias a parameter, a capture, or a view, so freeing it at the call site would
/// free storage the callee's caller still uses. Absent from the facts map reads
/// as `NotFresh` -- unproven means leaked, never freed.
///
/// Declared HERE, ahead of the loop emitters, rather than beside its classifier
/// (`computeFreshReturnFacts`, further down): stage S3's array-valued kernel
/// return frees the row the callee handed back, once per outer cell, and that
/// free is only sound for a `FreshPool` callee. The classifier still lives with
/// the rest of the deterministic-deallocation block; only the fact TYPE, its
/// cell and the lookup moved up.
type FreshReturn =
    | NotFresh
    | FreshPool

/// Per-module fresh-return facts, installed by genModule / genModuleSplit after
/// the callables table (freshReturnOf resolves through it). AsyncLocal for the
/// same per-parallel-test-task isolation as exprWarningsCell.
let internal freshReturnFactsStorage =
    System.Threading.AsyncLocal<Map<IRId, FreshReturn> ref>()

let freshReturnFactsCell () : Map<IRId, FreshReturn> ref =
    let v = freshReturnFactsStorage.Value
    if isNull (box v) then
        let fresh = ref Map.empty
        freshReturnFactsStorage.Value <- fresh
        fresh
    else v

/// The fresh-return fact for whatever callable an expression in callee position
/// resolves to (module function or synthetic). Unresolvable => NotFresh.
let freshReturnOf (calleeExpr: IRExpr) : FreshReturn =
    match resolveCallable calleeExpr with
    | Some c -> (freshReturnFactsCell ()).Value |> Map.tryFind c.Id |> Option.defaultValue NotFresh
    | None -> NotFresh

// ---------------------------------------------------------------------------
// Kernel body: expression-shaped, or call the lifted callable?
// (docs/plan-kernel-body-materialization.md sections 3 and 5, stage S2)
// ---------------------------------------------------------------------------
//
// The loop emitters that serve `<@> |> compute` render the kernel body as ONE
// C++ expression (`genKernelExprWithReynolds` -> `exprToCpp`). There is no
// statement scope there, so a body that MATERIALIZES an array -- a kernel-local
// `let` bound to a computed array -- has nowhere to put the allocation and the
// fill loop, and `exprToCppCore` refuses it with a sentinel.
//
// The lifted form of the SAME body already compiles correctly:
// `genFuncBodyScoped` is a full statement context (alloc, fill nest, scope-owned
// free) and the callable is emitted for every kernel regardless. So the fix is
// not new emission machinery -- it is ROUTING: when the body is not
// expression-shaped, call the lifted callable instead of inlining its text.
// `genObjectForApplication` already takes exactly this route for the sibling
// `IRApp(IRObjectFor ...)` shape.
//
// CONSERVATISM DIRECTION. A false "not expression-shaped" costs one non-inlined
// call per output cell. A false "expression-shaped" reintroduces a sentinel, i.e.
// a program that does not compile. So the predicate names only the node classes
// `exprToCppCore` refuses UNCONDITIONALLY -- which is also what makes this change
// unable to regress a passing test: every body it re-routes emits a
// `BLADE_CODEGEN_ERROR_` sentinel today, and a sentinel is already a failed
// compile. Inline forms (mask/sort/intersect/transpose/...) are deliberately NOT
// in the list: `renderLetExpr` materializes those into an IIFE prelude and they
// work inline today.

/// True iff `e` is a node `exprToCppCore` refuses outright in expression
/// position. Deliberately narrow -- see the conservatism note above.
let internal isNonExpressionNode (e: IRExpr) : bool =
    match e with
    | IRApplyCombinator _ | IRComposeApply _ -> true     // exprToCppCore: "unevaluated computation used as value"
    | IRMethodFor _ | IRObjectFor _ -> true              // exprToCppCore: "loop object used as value"
    | IRReduceCompute _ -> true                          // exprToCppCore: "reduce over a deferred computation ..."
    | IRCompute (IRApplyCombinator _) -> true            // genApplyCombinatorExpr: unconditional refusal
    | _ -> false

/// Does the kernel body render as a single C++ expression? False when a
/// materializing node appears anywhere in it (including as an `IRLet` RHS, which
/// is the shape stage S2's Lowering half now produces for a kernel-local array).
let rec kernelBodyIsExpressionShaped (body: IRExpr) : bool =
    if isNonExpressionNode body then false
    else childrenOf body |> List.forall kernelBodyIsExpressionShaped

/// True iff a RETURN expression branches (`if`/`match`) and at least one of the
/// leaves it can return is a materializing form. Follows only the return spine
/// -- branch arms and let-chain tails -- because that is the set of leaves the
/// return arm would have to find a destination for. Consumed by
/// `genFuncBodyScoped`'s return dispatch (stage S4) to refuse that shape with an
/// accurate message instead of letting each arm reach an unrelated sentinel.
let rec internal branchingReturnMaterializes (e: IRExpr) : bool =
    match e with
    | IRIf (_, t, f) -> branchingReturnMaterializes t || branchingReturnMaterializes f
    | IRMatch (_, cases) -> cases |> List.exists (fun c -> branchingReturnMaterializes c.Body)
    | IRLet (_, _, body) -> branchingReturnMaterializes body
    | leaf -> isNonExpressionNode leaf

/// Route a nest whose kernel body is not expression-shaped through a CALL to the
/// lifted callable: the body becomes `IRApp(IRVar(callable.Id, ...), params)`,
/// which `exprToCppCore`'s IRApp arm renders as `__lambda_N(<peeled args>,
/// <captures>)` -- capture arguments resolved through `captureForwardName` on the
/// SAME name map the emitter already builds, so a renamed block-local forwards
/// its EMITTED spelling (memory/block-local-capture-forwarding.md).
///
/// Applied at every CodeGen site that builds a `LoopNestCodeGen`, i.e. BEFORE
/// the flat-elementwise fast path, the nest, the fused tree and the device
/// emitters see it. That ordering is deliberate: `tryGenFlatElementwiseNest`'s
/// own gates (notably the OpenMP licence check at "OmpRequested && not
/// allParallel") still decide flat-vs-nest exactly as before -- this changes WHAT
/// the body renders as, never WHICH emitter renders it.
///
/// Abstains (leaving today's sentinel) for:
///   * a Reynolds kernel -- the permutation sum is rendered by substituting
///     param names, and while the call form survives that substitution, no
///     measured program combines Reynolds with a materializing body; keep the
///     loud refusal rather than ship an unexercised path;
///   * an unresolvable kernel, or one with no params.
///
/// An ARRAY-returning callable is NO LONGER an abstention (stage S3): the call
/// form is exactly how a whole row gets produced per outer cell, and
/// `genLoopNestStreamed`'s row-write arm copies the returned pool into the
/// output row. That pairing is deliberate -- the S0 typecheck guard
/// (`arrayValuedComputeBody`) and this abstention came out in the same change.
let routeKernelBodyThroughCall (info: ApplyInfo) (cg: LoopNestCodeGen) : LoopNestCodeGen =
    if cg.HasReynolds || cg.IsAntisymmetric then cg
    elif kernelBodyIsExpressionShaped cg.KernelExpr then cg
    else
        match resolveKernel info.Kernel with
        | Some rk when not (List.isEmpty rk.Callable.Params)
                       && not rk.Reynolds.HasReynolds ->
            let paramTypes = rk.Callable.Params |> List.map (_.Type)
            let funcTy = mkFuncArrow paramTypes rk.Callable.RetType
            let args = rk.Callable.Params |> List.map (fun p -> IRVar (p.VarId, p.Type))
            { cg with KernelExpr = IRApp (IRVar (rk.Callable.Id, funcTy), args, rk.Callable.RetType) }
        | _ -> cg

/// Evaluate a DepIdx inner-extent formula for a specific outer index value:
/// substitute the concrete integer `i` for the outer record's IRVar Id and
/// fold constants. None means something can't be reduced statically (free
/// variables, IRParam, non-arithmetic ops); used to produce the `_lens` table
/// for DepIdx literals, where None currently errors out (no runtime eval yet).
let rec evalDepIdxExtent (outerId: IRId) (i: int) (expr: IRExpr) : int option =
    match expr with
    | IRLit (IRLitInt n) -> Some (int n)
    | IRVar (vid, _) when vid = outerId -> Some i
    | IRBinOp (_, op, l, r) ->
        match evalDepIdxExtent outerId i l, evalDepIdxExtent outerId i r with
        | Some a, Some b ->
            match op with
            | IRAdd -> Some (a + b)
            | IRSub -> Some (a - b)
            | IRMul -> Some (a * b)
            | IRDiv when b <> 0 -> Some (a / b)
            | IRMod when b <> 0 -> Some (a % b)
            | _ -> None
        | _ -> None
    | IRUnaryOp (IRNeg, e) -> evalDepIdxExtent outerId i e |> Option.map (fun n -> -n)
    | _ -> None

/// The number of scalar cells an operand's backing pool is KNOWN to hold, as a
/// C++ expression, or `"0"` when the storage class doesn't statically settle it.
/// Consumed by the shim's `row_major_base` capacity bound: the contiguity probe
/// sees only `double**` (row STARTS, never LENGTHS), so it can't distinguish a
/// dense 2x2 (4 cells) from an n=2 packed-symmetric pool (3 cells) -- both
/// satisfy `rows[i] == base + i*ld`, and handing BLAS the packed pool's base
/// reads one element past it. The cell count is the only thing that separates
/// them. `"0"` (refuse) is deliberate for anything not provably dense rank-2:
/// unknown storage falls to staging, not a guessed capacity. Currently
/// unreachable in practice (only dense operands reach a linalg route; compact
/// ones are refused at typecheck, BL4004) -- guards a future surface widening.
let internal denseCellCountOfArray (arr: IRArrayType) (name: string) : string =
    if arr.IsVirtual then "0" else
    // Dense rank-2 <=> two stored slots, each a plain dense axis of rank 1.
    // `IxDense` is the compiler's one classification of "plain dense index";
    // a symmetry-like, compound, ragged or dependent slot lands in a sibling
    // arm and yields "0" here.
    let denseSlot (ix: IRIndexType) =
        (match ix with IxDense -> true | _ -> false) && ix.Rank <= 1
    match arr.IndexTypes with
    | [ i0; i1 ] when denseSlot i0 && denseSlot i1 ->
        $"({name}.extents[0] * {name}.extents[1])"
    | _ -> "0"

let internal denseCellCountExpr (ty: IRType) (name: string) : string =
    match ty with
    | ArrayElem arr -> denseCellCountOfArray arr name
    | _ -> "0"

/// The singular-matrix panic message, spelled ONCE. Four readers have to agree
/// on it byte for byte: `materializeSolveForm`'s native guard, the same form's
/// LAPACK `info` check, the interpreter's `Interp/ArrayOps.solveArray`, and any
/// corpus `// ABORT:` pin. Two of those four live in other files, so this
/// binding is the anchor a reviewer greps for, not a saving of characters.
///
/// Kept free of the operand's name and of the failing column index on purpose:
/// the LAPACK arm learns the column from `info` and the native arm from `k`,
/// but a message that differed between the arms would make the corpus pin
/// gate-dependent -- and a gate-dependent abort pin is a test that passes for
/// the wrong reason on exactly one machine.
let internal solveSingularMessage =
    "solve(A, b): the matrix is SINGULAR -- LU factorization found an exactly-zero pivot"

/// One dimension's extent, read off the OPERAND'S OWN index record. THE RULE
/// LIVES HERE; `literalOrRuntimeExtentOfArray` below is its string shape.
///
/// Two callers need the two different answers, which is why the rule is split
/// out rather than folded into the renderer: a loop bound wants TEXT with a
/// runtime fallback, while a `static constexpr` return-extents table has to
/// know STRUCTURALLY whether every axis is literal. Deciding the latter by
/// inspecting the rendered text would make an emitted table's storage class
/// depend on string formatting.
///
/// This is `genLoopBoundExpr`'s first arm (`IRLit (IRLitInt n) -> "%d"`),
/// factored so the loop NEST and the INTRINSIC/dispatch emitters cannot
/// disagree about a bound that is by construction the same number. The nest has
/// baked literals since Phase 4; the intrinsics did not, which is the whole of
/// the reported "runtime .extents[] read despite a literal Idx<n>" symptom at
/// those sites. The gemv and syrk dispatches each carried their own inline copy
/// of this match until they were routed through here.
///
/// SOUNDNESS. `IRMono.shapeMonomorphizeModule` writes a literal into `Extent` only
/// when every occurrence of that symbolic name was pinned to the SAME literal,
/// and `shapeRewriteType` confines the rewrite to the `Extent` field -- it never
/// touches a body's `extents(A)` read. So a literal in the record is a statement
/// about the runtime array, not a hope about it, and `<name>.extents[dim]` holds
/// that same value: the allocation's extents table is filled FROM this record.
///
/// RECORD POSITION IS NOT AXIS POSITION, and this is where that used to bite.
/// A packed multi-component record is ONE record covering SEVERAL dense axes
/// (`SymIdx<2, m>` is one record, two axes), so `List.item dim IndexTypes` is
/// the wrong record the moment any record ahead of `dim` has arity > 1. The
/// earlier rule sidestepped that by declining outright whenever the record at
/// the naive position had `Rank <> 1` -- safe, but it also declined the case it
/// was reading correctly, which is every axis of a compact operand (`decompact`
/// bounds itself by `A.extents[0]` over an `AntiIdx<r, n>` slot).
///
/// The mapping is a PREFIX-SUM WALK over record arities, the same walk
/// `IR.typeOf`'s decompact arm already does to find the record owning a given
/// dimension. It is exact rather than conservative because a record of arity r
/// contributes r dense axes that ALL carry that record's extent -- `IR.typeOf`
/// states this by construction, expanding a wreath slot into
/// `List.replicate axes { ... Extent = baseExtent ... }` -- and the emitted
/// extents TABLE is filled in that same dense-axis space (an n=3 antisymmetric
/// rank-2 operand emits `A_extents[0] = 3; A_extents[1] = 3;`). So the record's
/// `Extent` and `<name>.extents[dim]` are two spellings of one number for every
/// axis the record covers, which is exactly the licence this function needs.
///
/// Arity is read as `max 1 ix.Rank`, matching `IR.typeOf`'s walk and the
/// `sumBy (fun ix -> max 1 ix.Rank)` dense-rank count used by the array-form
/// emitters, so a 0/absent arity spans one axis here and everywhere else.
///
/// ONE DECLINE remains, to `.extents[]` (the pre-existing behaviour, never a
/// guess): the walk runs off the end of the record list, i.e. the operand has
/// fewer dense axes than `dim` -- nothing to read.
///
/// Non-dense storage classes are NOT special-cased here. A compound/sparse
/// operand has no `.extents` member at all and a `RaggedRow` carries `.len`, so
/// the runtime fallback is equally wrong for them; every caller therefore tests
/// its own storage class BEFORE reaching this (see `genReduceForm`'s
/// `isCompoundOperand`/`isRaggedRowOperand` pair for the canonical shape), and
/// adding a redundant guard here would only hide a caller that forgot to.
let internal literalExtentOfArray (arr: IRArrayType) (dim: int) : int64 option =
    // Walk records, accumulating the dense axes each one spans, until `dim`
    // falls inside one. Running off the end = fewer axes than `dim`.
    let rec ownerOf (axesSoFar: int) (remaining: IRIndexType list) : IRIndexType option =
        match remaining with
        | [] -> None
        | ix :: rest ->
            let span = max 1 ix.Rank
            if dim < axesSoFar + span then Some ix else ownerOf (axesSoFar + span) rest
    match ownerOf 0 arr.IndexTypes with
    | Some ix -> (match ix.Extent with IRLit (IRLitInt n) -> Some n | _ -> None)
    | None -> None

/// `literalExtentOfArray` as a C++ expression: the literal when the record
/// settles the axis, else the runtime `<name>.extents[dim]` read that every
/// intrinsic emitter used to hardcode. The payoff is not the removed load (any
/// compiler hoists an invariant one) but the TRIP COUNT -- see the note on the
/// `IRType` wrapper below.
let internal literalOrRuntimeExtentOfArray (arr: IRArrayType) (name: string) (dim: int) : string =
    match literalExtentOfArray arr dim with
    | Some n -> string n
    | None -> $"{name}.extents[{dim}]"

/// The same answer as `literalOrRuntimeExtentOfArray`, PAIRED with whether the
/// rendered text is a literal -- the `(value, isLiteral)` shape
/// `emitExtentsTable` consumes. The pairing has to come from the match that
/// chose the text (see emitExtentsTable's note), so it lives here rather than
/// at the extents sites, and `literalOrRuntimeExtentOfArray` above stays the
/// value-only spelling for loop bounds.
let internal extentDimOfArray (arr: IRArrayType) (name: string) (dim: int) : string * bool =
    match literalExtentOfArray arr dim with
    | Some n -> (string n, true)
    | None -> ($"{name}.extents[{dim}]", false)

/// THE shared companion-extents rule. Every emitter that materializes an
/// `Array<T,R>` needs a table for its shape; this decides which of the two
/// forms that table takes, and reports back whether the caller now OWNS it.
///
/// Default (any entry is a runtime read): a HEAP table. `Array<T,R>` stores
/// only a POINTER to its extents, so a stack `size_t[R]` would make the
/// wrapper non-returnable -- the pool outlives the frame but the extents
/// pointer dangles, and a caller reading `c.extents[d]` gets garbage. Heap
/// extents make the wrapper self-describing across a call boundary, at the
/// cost of a `delete[]` the scope has to remember (`Some name`).
///
/// WHEN EVERY ENTRY IS LITERAL none of that management is needed: a
/// `static constexpr const size_t[R]` table has STATIC storage duration, which
/// satisfies the same constraint strictly more safely -- it outlives every
/// wrapper naming it and every copy of that wrapper unconditionally, there is
/// nothing to free, and nothing to get wrong if a frame is torn down early.
/// It is the form the rectangular array-literal path already hands back across
/// a function return, chosen there for exactly this reason. `allocate<>` and
/// `deallocate<>` both take `const size_t extents[]` and `Array<T,R>::extents`
/// is a `const size_t*`, so the array-to-pointer decay leaves the stored
/// pointer identical in type and in every read (`NAME[d]` indexes an array the
/// same way it indexed a pointer). Being constexpr, the static also costs no
/// `__cxa_guard` even when the declaration sits inside a loop body.
/// `None` is returned as the owned name, which is what suppresses the free.
///
/// A MIXED table keeps the heap: a constexpr initializer cannot name a runtime
/// `.extents[]` read, and half-baking it would need two tables. RANK 0 keeps
/// the heap too -- `new size_t[0]` is legal where `const size_t t[0]` is not.
///
/// `dims` pairs each entry's RENDERED value with whether it is a literal. That
/// pairing is the caller's job precisely because the answer is structural: it
/// must fall out of the same match that chose the text (an `IRLit` arm vs an
/// `.extents[]` arm), never out of re-inspecting the rendered string.
///
/// DEFINED HERE, far above the allocation registry it used to sit beside,
/// because `materializeInlineForm`'s builders are in the `exprToCppCore` rec
/// group and every one of them needs this rule: their results are returned
/// out of function bodies, which is exactly the case a frame-local table
/// cannot survive.
let emitExtentsTable (ind: string) (extentsName: string) (rank: int)
                     (dims: (string * bool) list) : string list * string option =
    if rank > 0 && dims |> List.forall snd then
        ([ $"""{ind}static constexpr const size_t {extentsName}[{rank}] = {{ {(dims |> List.map fst |> String.concat ", ")} }};""" ],
         None)
    else
        (($"{ind}size_t* {extentsName} = new size_t[{rank}];")
         :: (dims |> List.mapi (fun d (e, _) -> $"{ind}{extentsName}[{d}] = {e};")),
         Some extentsName)

/// The word a dispatch marker comment leads with, for a route resolved by
/// `LinAlgPatterns.resolveNodeRoute`. Names the BACKEND because that's the only
/// place the choice is observable (host cblas / device cuBLAS / Blade's own
/// loops all compute the same matrix to within rounding). `None` is unreachable
/// from call sites (only built on the routed arm) and answers the host word
/// rather than raising -- a comment is not the place to fail a compile.
let internal dispatchMarkerTag (resolved: (Blade.LinAlgPatterns.LinAlgBackend * string) option) : string =
    match resolved with
    | Some (Blade.LinAlgPatterns.CudaBlas, _) -> "cublas"
    | _ -> "linalg"

/// Is this binary operation commutative? (a op b) = (b op a)
let isCommutativeOp (op: IRBinOp) : bool =
    match op with
    | IRAdd | IRMul | IREq | IRNeq | IRAnd | IROr -> true
    | _ -> false

/// Is this binary operation associative? (a op b) op c = a op (b op c)
/// Only ops that are BOTH commutative and associative get flattened.
let isAssociativeOp (op: IRBinOp) : bool =
    match op with
    | IRAdd | IRMul | IRAnd | IROr -> true
    | _ -> false

// ---- Parallel-fold reorder licence (docs/plan-cpp-perf-exploitation.md section 2) --
//
// `reduce(xs, k)` is parallelized only when `k` carries `where ... omp` AND the
// reorder is licensed. A chunked fold reassociates and reorders, so the licence
// is exactly "commutative and associative":
//
//   * `comm(a, b)` declared on the kernel -- the user's word, already
//     cross-checked against body parity at typecheck (CommContradictsBody);
//     associativity is the part `omp` itself asserts (the plan's trust model,
//     the same escape hatch comm's PBottom case uses);
//   * a recognised BUILTIN body, which carries both outright and needs nothing
//     declared.
//
// TypeCheck refuses `omp` with neither (BL4016), so reaching codegen unlicensed
// means the front end and this file disagreed; the emitters then fall back to
// the serial loop with a visible marker rather than parallelizing on a licence
// nobody granted.
//
// The SAME predicate answers "may BLADE_FP_REASSOC turn this serial chain into
// lanes?" -- the knob supplies the reproducibility opt-in that `omp` supplies
// on the parallel paths, never the licence itself. An unlicensed user kernel
// stays serial with the knob on.

/// The builtin binary op a 2-parameter fold callable's body IS, when the body is
/// exactly `p0 <op> p1` (either argument order) and `op` is both commutative and
/// associative. None for anything else -- including a body that merely CONTAINS
/// such an op, which carries no such guarantee.
///
/// Paired with TypeCheck.isBuiltinFoldBodySurface / isBuiltinFoldBodyTyped,
/// which answer the same question at the surface and typed levels for the
/// BL4016 diagnostic. Deliberately narrow at all three sites so "recognised
/// builtin" means one thing.
let foldKernelBuiltinOp (callable: IRCallable) : IRBinOp option =
    match callable.Params, callable.Body with
    | [p0; p1], IRBinOp (_, op, l, r) when isCommutativeOp op && isAssociativeOp op ->
        // A param reference lowers as IRVar over the param's VarId; IRParam
        // (positional) is accepted too so a callable built by either convention
        // is recognised.
        let slotOf (e: IRExpr) =
            match e with
            | IRVar (id, _) ->
                if id = p0.VarId then Some 0 elif id = p1.VarId then Some 1 else None
            | IRParam (_, idx, _) ->
                if idx = p0.Index then Some 0 elif idx = p1.Index then Some 1 else None
            | _ -> None
        (match slotOf l, slotOf r with
         | Some a, Some b when a <> b -> Some op
         | _ -> None)
    | _ -> None

/// Does this fold kernel demand REPRODUCIBLE evaluation? True when the
/// callable itself is a `where repro` function, or when it is the
/// eta-expanded wrapper around one (`lambda(a, b) -> f(a, b)` -- the shape a
/// named function takes in kernel position; the wrapper carries no clause of
/// its own, so the demand is read off the callee). A repro demand VETOES the
/// reorder licence below even when `comm` grants it: comm says reordering is
/// mathematically sound, repro says the operation sequence is the contract.
let foldKernelReproVetoed (callable: IRCallable) : bool =
    callable.IsRepro
    || (match callable.Body with
        | IRApp ((IRVar _) as f, _, _) ->
            (match resolveCallable f with
             | Some callee -> callee.IsRepro
             | None -> false)
        | _ -> false)

/// May a fold through `callable` be reordered/reassociated across threads?
/// Answers only the LICENCE question -- whether omp was requested is separate
/// (callable.IsOmpParallel), so the two can be reported independently.
/// `where repro` vetoes unconditionally: this one predicate gates the OpenMP
/// fold paths, every BLADE_FP_REASSOC lane, and the LLVM lane's fast-math
/// flags, so the veto lands on all of them at once.
let foldReorderLicensed (callable: IRCallable) : bool =
    not (foldKernelReproVetoed callable)
    && (callable.IsCommutative
        || not (List.isEmpty callable.CommGroups)
        || (foldKernelBuiltinOp callable).IsSome)

/// An operand's extent along `dim`, as a C++ expression: the LITERAL when the
/// operand's own index record carries one, else the runtime `.extents[dim]`
/// read that every intrinsic emitter used to hardcode.
///
/// This is the rule the gemv/syrk dispatches apply to their `n`, lifted so the
/// intrinsic/IIFE emitters can share it instead of each restating
/// `.extents[0]`. Baking is sound because Phase 4
/// (`IRMono.shapeMonomorphizeModule`) writes a literal into `IRIndexTypeG.Extent`
/// only when EVERY occurrence of the symbolic name was pinned to the SAME
/// literal, and `shapeRewriteType` confines the rewrite to `Extent` -- it never
/// touches a body's `extents(A)` read. Deliberately matches `IRLit (IRLitInt
/// n)` ONLY, not the broader literal-arithmetic evaluation `renderExtentExpr`
/// does: this is a loop bound in a hot emitter, and the narrow rule is the one
/// with an existing precedent.
///
/// The payoff is not the removed load (any compiler hoists an invariant one)
/// but the TRIP COUNT: a literal bound lets GCC unroll and vectorize a short
/// fiber sweep that an opaque `.extents[0]` leaves as a counted loop.
let literalOrRuntimeExtent (ty: IRType) (name: string) (dim: int) : string =
    // Delegates to the IRArrayType core (defined earlier, next to the gram/
    // matmul emitters' uses) so the dense-axis -> record mapping is done
    // identically at every site; parallel implementations of this rule have
    // twice existed and both times disagreed on exactly that mapping.
    match ty with
    | ArrayElem at -> literalOrRuntimeExtentOfArray at name dim
    | _ -> $"{name}.extents[{dim}]"

/// The deterministic K-lane accumulation body, as unindented C++ statements.
/// SHARED by every `fpReassocEnabled ()` site so the emitted shape -- and
/// therefore the numeric answer -- cannot drift between them, and so it stays
/// the same shape Path B's chunked fold already uses (Round C, measured).
///
/// Emits, for the caller's lane count `K` (`foldLaneCount`, or
/// `laneCountForStreams` where the body reads several operand streams) and the
/// half-open range `[lo, hi)`:
///
///   T L0 = elem(lo + 0); ... T L7 = elem(lo + 7);   // seed: no identity needed
///   size_t i = lo + 8;
///   for (; i + 8 <= hi; i += 8) {                   // 8 INDEPENDENT chains
///       combine(L0, elem(i + 0)); ... combine(L7, elem(i + 7));
///   }
///   if (i < hi) { combine(L0, elem(i)); i++; }      // tail: at most K-1 left,
///   ... (through L6) ...                            // lane K-1 can never get one
///   combine(L0, L1); ... combine(L0, L7);           // fixed ASCENDING combine
///
/// and returns that list together with the name of the lane holding the result
/// (`L0`). No pragma of any kind: this is pure instruction-level parallelism,
/// so it is safe to drop inside an already-parallel loop nest (a `#pragma omp
/// parallel for` there would nest teams) and the answer does not depend on any
/// team size. Callers own the `hi - lo < K` short fallback and the disposal of
/// the result lane.
///
/// SEPARATE NAMED LOCALS, not `T lane[K]`: an array kept live by index
/// arithmetic can defeat scalar replacement, leaving every lane update a
/// load-modify-store to the stack -- reintroducing exactly the latency the
/// lanes exist to remove. The fully-unrolled K-1 tail is the price.
///
/// `combine acc rhs` renders one complete fold statement (semicolon included),
/// which is what lets `+=` (prodsum) and `acc = W(acc, x)` (reduce) share this.
///
/// `k` is passed IN rather than read from `foldLaneCount` here because the
/// budget rule (`laneCountForStreams`) makes it a function of the body's
/// operand-stream count; every caller must therefore state the K its emitted
/// order is a function of. Callers whose body reads one stream pass
/// `foldLaneCount` and emit exactly what they emitted before the parameter
/// existed.
let internal fpReassocLaneStmts
        (k: int)
        (elemStr: string)
        (lanePrefix: string)
        (idxName: string)
        (loExpr: string)
        (hiExpr: string)
        (elemAt: string -> string)
        (combine: string -> string -> string)
        : string list * string =
    let lane (l: int) = $"{lanePrefix}{l}"
    // Lane l's seed index. `lo` is the literal 0 at the intrinsic sites, and
    // `0 + 3` would be noise there; everywhere else it is a hoisted `const`.
    let atLo (l: int) = if loExpr = "0" then string l else $"{loExpr} + {l}"
    let stmts =
        [ for l in 0 .. k - 1 -> $"{elemStr} {(lane l)} = {(elemAt (atLo l))};" ]
        @ [ $"size_t {idxName} = {(atLo k)};"
            $$"""for (; {{idxName}} + {{k}} <= {{hiExpr}}; {{idxName}} += {{k}}) {""" ]
        @ [ for l in 0 .. k - 1 ->
              "    " + combine (lane l) (elemAt ($"{idxName} + {l}")) ]
        @ [ "}" ]
        @ [ for l in 0 .. k - 2 ->
              $$"""if ({{idxName}} < {{hiExpr}}) { {{(combine (lane l) (elemAt idxName))}} {{idxName}}++; }""" ]
        @ [ for l in 1 .. k - 1 -> combine (lane 0) (lane l) ]
    (stmts, lane 0)

/// The C++ `reduction(<op>:acc)` operator for a builtin fold body, when OpenMP
/// knows an identity for it. `+` and `*` only: those are the two whose private
/// initializer (0 / 1) is unambiguous for every element type Blade folds, and
/// they are the ones the plan's Path A commits to. `&&`/`||` are licensed by
/// `isCommutativeOp`/`isAssociativeOp` and ARE valid OpenMP reduction
/// identifiers, but no Blade fold reaches these sites with a `bool` element
/// type today, so they are left out rather than shipped unmeasured; everything
/// else licensed goes down the manual chunked path (Path B) or the K-lane form,
/// neither of which needs an identity at all.
///
/// DEFINED HERE, far above Path A's chunked-fold reader, because the
/// BLADE_FP_REASSOC simd form needs it from `simdReducibleElem`'s neighbourhood
/// onwards -- `fpReassocSimdOp` sits with the rest of the knob's machinery, and
/// F# is order-dependent. Same reason `foldLaneCount` and the licence
/// predicates live up here.
let ompReductionOperator (op: IRBinOp) : string option =
    match op with
    | IRAdd -> Some "+"
    | IRMul -> Some "*"
    | _ -> None

/// Is `elemStr` a C++ type OpenMP's builtin `+`/`*` reduction identifiers are
/// defined for, and that a SIMD lane can hold?
///
/// The list is closed on purpose. `reduction(+:x)` on a class type is not a
/// portable construct (whether the builtin identifiers apply to a type with an
/// `operator+` is compiler-dependent, and `declare reduction` would be the
/// portable spelling), and `std::complex<double>` and `std::string` both reach
/// `elemTypeToCpp`. A fold over either keeps the K-lane form, which needs no
/// identity and no vector lane -- it is plain instruction-level parallelism
/// over whatever `combine` the caller passes.
let internal simdReducibleElem (elemStr: string) : bool =
    match elemStr with
    | "double" | "float" | "int32_t" | "int64_t" -> true
    | _ -> false

/// The `omp simd reduction` accumulation body, as unindented C++ statements:
/// the SECOND emission form BLADE_FP_REASSOC can spend its licence on, and a
/// sibling of `fpReassocLaneStmts` rather than a replacement for it.
///
/// Emits, for the half-open range `[lo, hi)` and an accumulator the caller has
/// already DECLARED AND SEEDED:
///
///   BLADE_OMP_SIMD_REDUCTION(+:acc)
///   for (size_t i = lo; i < hi; i++) {
///       acc = acc + elem(i);
///   }
///
/// WHY THIS CAN BEAT THE LANES. Both forms buy the same thing -- independent
/// partial sums that break the serial dependence chain through the fold -- but
/// they ask for it at different levels. The lane form asks for it in SCALARS and
/// relies on the C++ compiler's SLP pass to pack K named locals into vectors;
/// where that pass packs them through a shuffle network the packing costs as
/// much as the chain it removed (measured: the dot shape, where 8 named lanes
/// land at parity with the serial chain through a vunpck/vpermpd permute
/// storm). The simd form asks for it in VECTORS directly: the vectorizer owns
/// the accumulator, so the partials are born in lanes and never marshalled.
///
/// WHAT IT COSTS. The lane count -- and therefore the summation order -- stops
/// being a property of the emitted text and becomes a property of the compiler
/// and its flags (vector width, unroll factor, whether it vectorized at all).
/// That is a trade the knob is allowed to make and the lane form was not: see
/// the contract note at `fpReassocEnabled`, which the policy change to
/// "optimized Release builds are non-repro" is what licenses.
///
/// NO SHORT FALLBACK, no seed lanes, no tail peel, no fixed combine order: the
/// loop IS the serial loop, and every reassociation is the pragma's. That is
/// why this emitter is four lines and its sibling is twenty -- and why a `hi -
/// lo` below the vector width is not a special case here (the vectorizer emits
/// its own scalar remainder).
///
/// `accName` must name a plain local: OpenMP reduction list items may not be
/// struct members or dereferences. Every caller passes either its own IIFE
/// accumulator or `bindingCppName`, both of which are locals.
///
/// `prelude` is emitted at the top of the loop body, before the accumulate.
/// Empty for the `prodsum` IIFE, whose element is just a subscript product; the
/// reduce-over-computation site puts the level's ELEMENT PEELS there, so that
/// its body expression is written straight into the vectorized loop rather than
/// behind a lambda the vectorizer would have to see through. (The lane form has
/// to use a lambda -- it evaluates the body at K different indices and would
/// otherwise re-render it K times -- which is exactly the asymmetry that makes
/// one shared `elemAt` insufficient here.)
let internal fpReassocSimdStmts
        (opStr: string)
        (accName: string)
        (idxName: string)
        (loExpr: string)
        (hiExpr: string)
        (prelude: string list)
        (elemAt: string -> string)
        : string list =
    [ $"BLADE_OMP_SIMD_REDUCTION({opStr}:{accName})"
      $$"""for (size_t {{idxName}} = {{loExpr}}; {{idxName}} < {{hiExpr}}; {{idxName}}++) {""" ]
    @ (prelude |> List.map (fun s -> "    " + s))
    @ [ $"    {accName} = {accName} {opStr} {(elemAt idxName)};"
        "}" ]

/// The reduction operator the simd form would use for a fold through
/// `callable` over `elemStr` elements -- `None` when the simd form is not
/// available and the caller must keep the K-lane form.
///
/// TWO conditions, and they are different in kind. The ELEMENT TYPE condition
/// is portability (see `simdReducibleElem`). The OP condition is that OpenMP's
/// reduction clause names an OPERATOR, not a function: a `comm`-declared user
/// kernel is licensed to be reassociated but its combine is a call
/// `w(acc, x)`, which is not a reduction-statement form -- there is no identity
/// to seed private copies with and no operator to name in the clause. Such a
/// fold keeps the lane form, which needs neither. So the split is:
///
///   builtin `+`/`*` body  -> simd form (this returns Some)
///   comm-declared kernel  -> K-lane form (this returns None)
///
/// and `foldReorderLicensed` -- unchanged -- still gates whether either is
/// reached at all.
let internal fpReassocSimdOp (callable: IRCallable) (elemStr: string) : string option =
    if not (simdReducibleElem elemStr) then None
    else foldKernelBuiltinOp callable |> Option.bind ompReductionOperator

