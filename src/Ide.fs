// Ide.fs: implements `blade ide check --json <file>` (parse + typecheck, no
// codegen), emitting one JSON object on stdout for the VS Code extension
// (see the extension README at _blade_ide for the consumer-side contract):
//
//   { "version": 1,
//     "diagnostics": [ { severity, line, col, endLine, endCol, message } ],
//     "bindings":    [ { name, kind, line, col, type,
//                        doc?,                       // comment block above
//                        params?: [{name,type,doc?,  // functions only
//                                   minRank?}],      // deduced min rank (stage 2)
//                        ret?,                       // functions only
//                        where?,                     // declared conjuncts
//                        deducedComm? } ],           // deduced pin clauses; [] = "None"
//     "kernels":     [ { line, col, endLine, endCol, // lambda kernels, span-keyed
//                        params: [name],
//                        deducedComm: [clause], declaredWhere: [clause],
//                        minRanks: [{param, rank}] } ] }
//
// All positions are 1-based. Bindings cover top-level lets/statics,
// functions (as a signature), function parameters, and function-body
// let/for-in bindings. Doc comments are the contiguous `//` run directly
// above a binding (directives and banner lines filtered out); a doc line
// `name: description` documents that parameter, Ionide-style. Binding
// positions come from a parallel walk of the UNTYPED AST joined by (scope,
// name) in declaration order; compiler-generated declarations are skipped.
//
// calls[]: one entry per BUILTIN call site, with concrete (monomorphized)
// argument/result types rendered in the compiler's `Array<Elem like
// Idx...>` notation, collected by walking the zonked typed tree:
//   "calls": [ { name, line, col, endLine, endCol, args: [..], ret } ]
//
// references[]: one entry per BINDER -- its name token and every use that
// resolves to it -- which is what definition, find-references and rename read:
//   "references": [ { name, kind, def: {line,col,endLine,endCol} | null,
//                     uses: [ {line,col,endLine,endCol} ] } ]
// kind is "function" | "value" | "param" | "local" | "type". Entries are keyed
// internally by the binder's IRId, so two shadowing `x`s are two entries with
// disjoint use lists; a binder with no uses still gets an entry (it is still
// renameable). Uses come from the typed tree (`TExprVar` carries the resolved
// binder id and the identifier's own span), definitions from the parser's name
// tokens. `bindings[]` additionally gained `endLine`/`endCol`, closing the
// DECLARATION span its `line`/`col` already opened.
//
// The same payload also serves `blade ide serve` (IdeServe.fs), which checks
// an UNSAVED buffer instead of a file and adds three fields: top-level "id"
// and "tier" for request correlation, and per-binding "concreteType" when the
// full tier's monomorphization resolved a type the typed AST left abstract.
// One-shot `ide check --json` emits none of them, byte-for-byte as before.

module Blade.Ide

open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open Blade.Ast
open Blade.Types
open Blade.IR
open Blade.IRLoopStructure
open Blade.IRStorage
open Blade.IRLift
open Blade.IRMono
open Blade.IRPrint
open Blade.IRValidate
open Blade.TypedAst

// JSON emission (hand-rolled: tiny payload, zero dependencies)

/// Public because the NDJSON serve loop (IdeServe.fs) builds its own
/// envelope/error lines and must escape them identically -- one escaper, so
/// a control character can never split a response across two lines.
let jsonEscape (s: string) =
    let sb = StringBuilder(s.Length + 8)
    for c in s do
        match c with
        | '"' -> sb.Append "\\\"" |> ignore
        | '\\' -> sb.Append "\\\\" |> ignore
        | '\n' -> sb.Append "\\n" |> ignore
        | '\r' -> sb.Append "\\r" |> ignore
        | '\t' -> sb.Append "\\t" |> ignore
        | c when int c < 32 -> sb.AppendFormat("\\u{0:x4}", int c) |> ignore
        | c -> sb.Append c |> ignore
    sb.ToString()

type private Diag = {
    Severity: string
    Line: int
    Col: int
    EndLine: int
    EndCol: int
    Message: string
    /// BLxxxx diagnostic code; "" = none (the JSON field is omitted).
    Code: string
}

type private ParamInfo = {
    PName: string
    PType: string
    PDoc: string
    /// Deduced minimum rank; Some only when DEDUCED (annotated params show
    /// their rank in the type).
    PMinRank: int option
    /// Pretty-printed DEFAULT value expression for signature help
    /// (`s: Float = 2.0` renders "2.0"); None on required params. The JSON
    /// field is omitted when absent, so the payload shape is unchanged for
    /// existing clients.
    PDefault: string option
}

type private BindingInfo = {
    Name: string
    Kind: string
    Line: int
    Col: int
    /// Closing corner of the SAME span `Line`/`Col` open -- the declaration,
    /// not the name token (changing what `line`/`col` mean would move every
    /// existing hover). Emitted additively; `references[]` is where a client
    /// goes for name-token spans.
    EndLine: int
    EndCol: int
    TypeStr: string
    Doc: string
    Params: ParamInfo list   // non-empty only for functions
    Ret: string option       // Some only for functions
    Where: string list       // where-clause conjuncts, functions only
    /// Stage-3 DEDUCED symmetry as canonical pin-clause strings ("comm(a,
    /// b)"), declared or not -- always emitted for functions ([] = "None").
    DeducedComm: string list
    /// Provenance for a top-level provider READ (`let x = store.vars.v |>
    /// alias.read`): (store binding name, "vars.v" / "dims.v"). None otherwise.
    ProviderRead: (string * string) option
    /// Provenance for a top-level provider WRITE (`let saved = alias.write(
    /// "out.csv", obs)`): the store member the array it persists came from,
    /// chased through `obs`. Its own channel, not `ProviderRead` -- a write
    /// binding reads nothing from the store.
    ProviderWrite: (string * string) option
    /// Full-tier upgrade: the type MONOMORPHIZATION resolved, when it is
    /// strictly more concrete than `TypeStr`. None on the fast tier and on
    /// bindings lowering left unchanged.
    ConcreteType: string option
    /// Is this a top-level value binding (the only kind `IRModule.Bindings`
    /// can name)? Routing only -- never emitted.
    IsTopLevelValue: bool
}

// A single member of a loaded provider store (a `dims` or `vars` field).
type private ProviderMemberInfo = {
    MName: string
    MType: string
}

// A provided named index type, with its extent when statically known.
type private ProviderIndexInfo = {
    IName: string
    IExtent: int64 option
}

// One `let store = alias.load("path")` binding and the structure the
// provider derived (index types plus `dims`/`vars` members). Emitted under
// `providers[]` for hovers on members, the store handle, and the alias.
type private ProviderInfo = {
    Store: string
    Alias: string
    Provider: string
    Path: string
    PLine: int
    PCol: int
    IndexTypes: ProviderIndexInfo list
    Dims: ProviderMemberInfo list
    Vars: ProviderMemberInfo list
}

// One builtin call site: argument and result types, pre-rendered as
// strings in the compiler's concrete notation.
type private CallInfo = {
    CName: string
    CLine: int
    CCol: int
    CEndLine: int
    CEndCol: int
    CArgs: string list
    CRet: string
}

// One fact the checker DEDUCED rather than read off an annotation. A
// top-level `deduced[]` array since it can carry kernel-site facts with no
// named binding. Meaningful fields depend on DKind.
type private DeducedInfo = {
    DKind: string          // "rank" | "comm" | "anticomm" | "packComm"
    DOwner: string         // function name, or "<kernel>" for an inline kernel
    DName: string          // param name ("rank") / pack name ("packComm")
    DLeft: string          // pair members ("comm" / "anticomm")
    DRight: string
    DIndex: int            // param index, or adjacent-pair index
    DRank: int             // "rank" only; 0 otherwise
    DLine: int
    DCol: int
    DEndLine: int
    DEndCol: int
}

// One lambda-kernel site with its deduction snapshot: param names, deduced
// symmetry, declared where-clause conjuncts, and per-param cell ranks.
// Span-keyed: hover/completion resolves through position, not a name.
type private KernelIdeInfo = {
    KLine: int
    KCol: int
    KEndLine: int
    KEndCol: int
    KParamNames: string list
    KDeduced: string list
    KDeclaredW: string list
    KMinRanks: (string * int) list
}

// references[]: one entry per BINDER, with the span of its name token and the
// spans of every use that resolves to it. Keyed internally by the binder's
// IRId, never by name -- which is exactly what makes two shadowing `x`s two
// entries with disjoint use lists instead of one merged blob.

/// A 1-based four-corner span, the shape every `references[]` position takes.
type private RefSpan = {
    RLine: int
    RCol: int
    REndLine: int
    REndCol: int
}

type private RefInfo = {
    RName: string
    /// "function" | "value" | "param" | "local" | "type".
    RKind: string
    /// The NAME TOKEN, not the declaration -- what F2 rewrites and F12 jumps
    /// to. None where no span survived; such an entry is emitted only if it
    /// still has uses to offer.
    RDef: RefSpan option
    RUses: RefSpan list
}

/// Clamp a span to 1-based sanity; noSpan (all zeros) becomes 1:1-1:1.
let private clampSpan (s: Span) =
    let line = max 1 s.StartLine
    let col = max 1 s.StartCol
    let endLine = max line s.EndLine
    let endCol = if s.EndCol >= 1 then s.EndCol else col
    (line, col, endLine, endCol)

/// A `deduced[]` record with every field at its empty default, spanned.
let private emptyDeduced (span: Span) : DeducedInfo =
    let (line, col, endLine, endCol) = clampSpan span
    { DKind = ""; DOwner = ""; DName = ""; DLeft = ""; DRight = ""
      DIndex = 0; DRank = 0
      DLine = line; DCol = col; DEndLine = endLine; DEndCol = endCol }

/// The stage-6a certificate facts, projected into the flat `deduced[]`
/// record. Hoisted out of `ideCheck`'s drain so `deducedJsonForTests` can
/// exercise this mapping without running the ML elaborator.
let private certFactRecords () : DeducedInfo list =
    Blade.ML.Equiv.CertFacts.get ()
    |> List.map (fun (fact, span) ->
        { emptyDeduced span with
            DKind = fact.Discipline
            DOwner = fact.Owner
            DName = fact.Group
            DLeft = String.concat "," fact.Deps })

/// Where one notebook cell's text landed in the assembled session source:
/// a 1-based inclusive line range, plus the absolute line and prefix width
/// of a synthetic wrapper binding when that cell needed one. A cell whose
/// definition was superseded by a later rebind gets an EMPTY range no
/// payload entry can fall inside.
type CellWindow = {
    StartLine: int
    EndLine: int
    WrapLine: int option
    WrapCol: int option
}

/// Request-correlation fields the NDJSON serve protocol prepends to the
/// payload. All None for one-shot `ide check --json`, whose output stays
/// byte-for-byte what it was (the extension's fallback path parses it).
/// `Windows` is set only by `checkCells`, whose caller assembled several
/// notebook cells into the one source this payload describes and needs to
/// know which lines belong to which cell.
type Envelope = {
    Id: int option
    Tier: string option
    Windows: CellWindow list option
}

let noEnvelope : Envelope = { Id = None; Tier = None; Windows = None }

let private renderJson (env: Envelope) (diags: Diag list) (bindings: BindingInfo list) (providers: ProviderInfo list)
                       (deduced: DeducedInfo list) (calls: CallInfo list)
                       (kernels: KernelIdeInfo list) (references: RefInfo list) =
    let sb = StringBuilder()
    sb.Append '{' |> ignore
    match env.Id with
    | Some i -> sb.AppendFormat("\"id\":{0},", i) |> ignore
    | None -> ()
    match env.Tier with
    | Some t -> sb.AppendFormat("\"tier\":\"{0}\",", jsonEscape t) |> ignore
    | None -> ()
    // Cell windows ride ahead of the payload proper: a client rebasing
    // diagnostics wants the map before it starts reading positions. All
    // numbers, so nothing here needs escaping. The wrap pair is absent, not
    // null, for a cell that took no synthetic wrapper.
    match env.Windows with
    | Some ws ->
        sb.Append "\"windows\":[" |> ignore
        ws
        |> List.iteri (fun i w ->
            if i > 0 then sb.Append ',' |> ignore
            sb.AppendFormat("{{\"startLine\":{0},\"endLine\":{1}", w.StartLine, w.EndLine) |> ignore
            match w.WrapLine with
            | Some l -> sb.AppendFormat(",\"wrapLine\":{0}", l) |> ignore
            | None -> ()
            match w.WrapCol with
            | Some c -> sb.AppendFormat(",\"wrapCol\":{0}", c) |> ignore
            | None -> ()
            sb.Append '}' |> ignore)
        sb.Append "]," |> ignore
    | None -> ()
    sb.Append "\"version\":1,\"diagnostics\":[" |> ignore
    diags
    |> List.iteri (fun i d ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"severity\":\"{0}\",\"line\":{1},\"col\":{2},\"endLine\":{3},\"endCol\":{4},\"message\":\"{5}\"",
            d.Severity, d.Line, d.Col, d.EndLine, d.EndCol, jsonEscape d.Message) |> ignore
        if d.Code <> "" then
            sb.AppendFormat(",\"code\":\"{0}\"", jsonEscape d.Code) |> ignore
        sb.Append '}' |> ignore)
    sb.Append "],\"bindings\":[" |> ignore
    bindings
    |> List.iteri (fun i b ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"name\":\"{0}\",\"kind\":\"{1}\",\"line\":{2},\"col\":{3},\"type\":\"{4}\"",
            jsonEscape b.Name, jsonEscape b.Kind, b.Line, b.Col, jsonEscape b.TypeStr) |> ignore
        match b.ConcreteType with
        | Some t -> sb.AppendFormat(",\"concreteType\":\"{0}\"", jsonEscape t) |> ignore
        | None -> ()
        if b.Doc <> "" then
            sb.AppendFormat(",\"doc\":\"{0}\"", jsonEscape b.Doc) |> ignore
        match b.ProviderRead with
        | Some (store, memberPath) ->
            sb.AppendFormat(
                ",\"providerRead\":{{\"store\":\"{0}\",\"member\":\"{1}\"}}",
                jsonEscape store, jsonEscape memberPath) |> ignore
        | None -> ()
        // Same shape, opposite direction, and never both on one binding.
        match b.ProviderWrite with
        | Some (store, memberPath) ->
            sb.AppendFormat(
                ",\"providerWrite\":{{\"store\":\"{0}\",\"member\":\"{1}\"}}",
                jsonEscape store, jsonEscape memberPath) |> ignore
        | None -> ()
        match b.Ret with
        | Some ret ->
            sb.Append ",\"params\":[" |> ignore
            b.Params
            |> List.iteri (fun j p ->
                if j > 0 then sb.Append ',' |> ignore
                sb.AppendFormat("{{\"name\":\"{0}\",\"type\":\"{1}\"", jsonEscape p.PName, jsonEscape p.PType) |> ignore
                if p.PDoc <> "" then sb.AppendFormat(",\"doc\":\"{0}\"", jsonEscape p.PDoc) |> ignore
                match p.PMinRank with
                | Some k -> sb.AppendFormat(",\"minRank\":{0}", k) |> ignore
                | None -> ()
                match p.PDefault with
                | Some d -> sb.AppendFormat(",\"default\":\"{0}\"", jsonEscape d) |> ignore
                | None -> ()
                sb.Append '}' |> ignore)
            sb.AppendFormat("],\"ret\":\"{0}\"", jsonEscape ret) |> ignore
            if not b.Where.IsEmpty then
                sb.Append ",\"where\":[" |> ignore
                b.Where
                |> List.iteri (fun j w ->
                    if j > 0 then sb.Append ',' |> ignore
                    sb.AppendFormat("\"{0}\"", jsonEscape w) |> ignore)
                sb.Append ']' |> ignore
            // Always present on functions: [] means "deduction ran, proved nothing".
            sb.Append ",\"deducedComm\":[" |> ignore
            b.DeducedComm
            |> List.iteri (fun j c ->
                if j > 0 then sb.Append ',' |> ignore
                sb.AppendFormat("\"{0}\"", jsonEscape c) |> ignore)
            sb.Append ']' |> ignore
        | None -> ()
        // Last, so the leading field run stays byte-identical for clients
        // (and tests) that match on it.
        sb.AppendFormat(",\"endLine\":{0},\"endCol\":{1}", b.EndLine, b.EndCol) |> ignore
        sb.Append '}' |> ignore)
    sb.Append "],\"providers\":[" |> ignore
    let appendMembers (label: string) (ms: ProviderMemberInfo list) =
        sb.AppendFormat(",\"{0}\":[", label) |> ignore
        ms
        |> List.iteri (fun j m ->
            if j > 0 then sb.Append ',' |> ignore
            sb.AppendFormat(
                "{{\"name\":\"{0}\",\"type\":\"{1}\"}}", jsonEscape m.MName, jsonEscape m.MType) |> ignore)
        sb.Append ']' |> ignore
    providers
    |> List.iteri (fun i p ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"store\":\"{0}\",\"alias\":\"{1}\",\"provider\":\"{2}\",\"path\":\"{3}\",\"line\":{4},\"col\":{5}",
            jsonEscape p.Store, jsonEscape p.Alias, jsonEscape p.Provider, jsonEscape p.Path, p.PLine, p.PCol) |> ignore
        sb.Append ",\"indexTypes\":[" |> ignore
        p.IndexTypes
        |> List.iteri (fun j ix ->
            if j > 0 then sb.Append ',' |> ignore
            sb.AppendFormat("{{\"name\":\"{0}\"", jsonEscape ix.IName) |> ignore
            match ix.IExtent with
            | Some e -> sb.AppendFormat(",\"extent\":{0}", e) |> ignore
            | None -> ()
            sb.Append '}' |> ignore)
        sb.Append ']' |> ignore
        appendMembers "dims" p.Dims
        appendMembers "vars" p.Vars
        sb.Append '}' |> ignore)
    // Deduced facts: only fields meaningful for the kind are emitted --
    // rank/packComm carry `name`, pair kinds carry `left`/`right`, and
    // certificate kinds need both (else the pair arm would drop `name`).
    sb.Append "],\"deduced\":[" |> ignore
    deduced
    |> List.iteri (fun i d ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"kind\":\"{0}\",\"owner\":\"{1}\"", jsonEscape d.DKind, jsonEscape d.DOwner) |> ignore
        if d.DKind = "rank" || d.DKind = "packComm" then
            sb.AppendFormat(",\"name\":\"{0}\"", jsonEscape d.DName) |> ignore
        elif d.DKind = "equiv" || d.DKind = "galilean" then
            sb.AppendFormat(",\"name\":\"{0}\",\"left\":\"{1}\"",
                            jsonEscape d.DName, jsonEscape d.DLeft) |> ignore
        else
            sb.AppendFormat(",\"left\":\"{0}\",\"right\":\"{1}\"",
                            jsonEscape d.DLeft, jsonEscape d.DRight) |> ignore
        sb.AppendFormat(",\"index\":{0}", d.DIndex) |> ignore
        if d.DKind = "rank" then
            sb.AppendFormat(",\"rank\":{0}", d.DRank) |> ignore
        sb.AppendFormat(",\"line\":{0},\"col\":{1},\"endLine\":{2},\"endCol\":{3}}}",
                        d.DLine, d.DCol, d.DEndLine, d.DEndCol) |> ignore)
    sb.Append "],\"calls\":[" |> ignore
    calls
    |> List.iteri (fun i c ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"name\":\"{0}\",\"line\":{1},\"col\":{2},\"endLine\":{3},\"endCol\":{4},\"ret\":\"{5}\",\"args\":[",
            jsonEscape c.CName, c.CLine, c.CCol, c.CEndLine, c.CEndCol, jsonEscape c.CRet) |> ignore
        c.CArgs
        |> List.iteri (fun j a ->
            if j > 0 then sb.Append ',' |> ignore
            sb.AppendFormat("\"{0}\"", jsonEscape a) |> ignore)
        sb.Append "]}" |> ignore)
    sb.Append "],\"kernels\":[" |> ignore
    let appendStrings (label: string) (ss: string list) =
        sb.AppendFormat(",\"{0}\":[", label) |> ignore
        ss
        |> List.iteri (fun j s ->
            if j > 0 then sb.Append ',' |> ignore
            sb.AppendFormat("\"{0}\"", jsonEscape s) |> ignore)
        sb.Append ']' |> ignore
    kernels
    |> List.iteri (fun i k ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat(
            "{{\"line\":{0},\"col\":{1},\"endLine\":{2},\"endCol\":{3}",
            k.KLine, k.KCol, k.KEndLine, k.KEndCol) |> ignore
        appendStrings "params" k.KParamNames
        appendStrings "deducedComm" k.KDeduced
        appendStrings "declaredWhere" k.KDeclaredW
        sb.Append ",\"minRanks\":[" |> ignore
        k.KMinRanks
        |> List.iteri (fun j (p, r) ->
            if j > 0 then sb.Append ',' |> ignore
            sb.AppendFormat("{{\"param\":\"{0}\",\"rank\":{1}}}", jsonEscape p, r) |> ignore)
        sb.Append "]}" |> ignore)
    // One entry per binder: `def` is the name token (null when none survived),
    // `uses` every resolved reference to THAT binder. Definition, references
    // and rename all read this array and nothing else.
    sb.Append "],\"references\":[" |> ignore
    let appendRefSpan (r: RefSpan) =
        sb.AppendFormat("{{\"line\":{0},\"col\":{1},\"endLine\":{2},\"endCol\":{3}}}",
                        r.RLine, r.RCol, r.REndLine, r.REndCol) |> ignore
    references
    |> List.iteri (fun i r ->
        if i > 0 then sb.Append ',' |> ignore
        sb.AppendFormat("{{\"name\":\"{0}\",\"kind\":\"{1}\",\"def\":",
                        jsonEscape r.RName, jsonEscape r.RKind) |> ignore
        (match r.RDef with
         | Some d -> appendRefSpan d
         | None -> sb.Append "null" |> ignore)
        sb.Append ",\"uses\":[" |> ignore
        r.RUses
        |> List.iteri (fun j u ->
            if j > 0 then sb.Append ',' |> ignore
            appendRefSpan u)
        sb.Append "]}" |> ignore)
    sb.Append "]}" |> ignore
    sb.ToString()

/// Test hook: the `deduced[]` JSON the CertFacts channel ALONE would
/// produce. The certificate producers reset their channel on the way in,
/// so a test cannot observe a staged fact through `ideCheck` end to end.
let deducedJsonForTests () : string =
    renderJson noEnvelope [] [] [] (certFactRecords ()) [] [] []

// Type rendering

/// Collect Id -> nominal-name entries from the index types embedded in a
/// type, so ppIRTypeIn renders `Idx<Lat>` instead of a raw extent (internal
/// structural tags like `__raggedidx` are excluded).
let rec private indexNamesOf (t: IRType) : (IRId * string) list =
    match t with
    | ArrayElem arr ->
        let fromIndices =
            arr.IndexTypes
            |> List.choose (fun idx ->
                match idx.Tag with
                | Some tag when not (tag.StartsWith "__") -> Some (idx.Id, tag)
                // A provider AXIS tag is `__`-prefixed so the type system reads it
                // as synthetic (IcechunkProvider.axisTag says why), but the dim name
                // inside it is exactly the user-facing name this map wants -- without
                // it a checkout array prints as `Idx<24>, Idx<10>, Idx<12>` while the
                // store calls those axes time, lat and lon.
                | Some tag ->
                    Blade.IcechunkProvider.tryAxisTagName tag
                    |> Option.map (fun name -> (idx.Id, name))
                | None -> None)
        fromIndices @ indexNamesOf arr.ElemType
    | IRTTuple ts -> ts |> List.collect indexNamesOf
    | _ -> []

/// Index slots whose EXTENT is an INTERNAL (`__`-prefixed) param -- a
/// compiler-minted name like `__extents_inferred_n`, which `extents(A)` pins on
/// an otherwise-unconstrained array parameter. Nothing user-written produces one
/// and no source can refer to it, so leaking it into a hover or a REPL echo
/// (`Array<Float64 like Idx<__extents_inferred_n>>`) shows an identifier that
/// does not exist. `indexNamesOf` already suppresses `__` TAGS; this is the same
/// rule one level down, on the extent expression a nameless slot falls back to
/// printing. They render as the `_` wildcard `concreteNames` already uses for
/// exactly these slots in `calls[]`, so both surfaces read the same way.
///
/// PLAIN slots only: `ppIndexTypeIn` treats a nominal name on a COMPACT class as
/// the whole class's surface spelling, so naming one `_` would print a bare `_`
/// instead of `SymIdx<2, _>`.
let rec private internalExtentNames (t: IRType) : (IRId * string) list =
    let isInternal (idx: IRIndexType) =
        idx.Symmetry = SymNone
        && (match idx.Extent with
            | IRParam (n, _, _) -> n.StartsWith "__"
            | _ -> false)
    match t with
    | ArrayElem arr ->
        (arr.IndexTypes |> List.choose (fun idx ->
            if isInternal idx then Some (idx.Id, "_") else None))
        @ internalExtentNames arr.ElemType
    | IRTTuple ts -> ts |> List.collect internalExtentNames
    | _ -> []

/// Public: also the REPL's display printer (Cli.fs) -- index-name-aware
/// rendering beats bare ppIRType for any type embedding named index types.
let ppType (t: IRType) : string =
    // Internal extents first so a real nominal name for the same slot wins
    // (Map.ofList keeps the last entry for a duplicate key).
    ppIRTypeIn (internalExtentNames t @ indexNamesOf t |> Map.ofList) t

/// Multi-line function signature: each parameter and the return type on its
/// own line (long array types stay readable).
let private formatFunctionSig (ps: (string * string) list) (ret: string) =
    match ps with
    | [] -> $"() -> {ret}"
    | _ ->
        let paramLines = ps |> List.map (fun (n, t) -> $"    {n}: {t}")
        sprintf "(\n%s\n) -> %s" (String.concat ",\n" paramLines) ret

// Abstract (type-variable) rendering -- shared with the REPL. Post-zonk,
// surviving IRTInfer vars are HM-polymorphic positions; rendering them as
// `T?10000` leaks inference ids into hovers, so they are named from source
// annotations where possible, fresh letters otherwise.

/// Does an unresolved inference variable survive anywhere in the type?
let rec hasInfer (t: IRType) : bool =
    match t with
    | IRTInfer _ -> true
    | IRTTuple ts -> ts |> List.exists hasInfer
    | IRTComputation t | IRTPoly (t, _)
    | IRTUnitAnnotated (t, _) | IRTIdxTagged (t, _) -> hasInfer t
    | IRTDist (_, elem, _) -> hasInfer elem
    | IRTLoop lt ->
        (lt.ArrayTypes |> List.exists hasInfer)
        || (lt.KernelType |> Option.exists hasInfer)
    | IRTArrow (slots, ret, _) ->
        hasInfer ret
        || (slots |> List.exists (function SVal t -> hasInfer t | _ -> false))
    | _ -> false

/// Replace surviving inference variables with named placeholders so the
/// standard printer renders them as abstract type variables.
let rec nameInfers (nameOf: int -> string) (t: IRType) : IRType =
    match t with
    | IRTInfer id -> IRTNamed (nameOf id)
    | IRTTuple ts -> IRTTuple (ts |> List.map (nameInfers nameOf))
    | IRTComputation t -> IRTComputation (nameInfers nameOf t)
    | IRTPoly (t, v) -> IRTPoly (nameInfers nameOf t, v)
    | IRTUnitAnnotated (t, u) -> IRTUnitAnnotated (nameInfers nameOf t, u)
    | IRTIdxTagged (t, r) -> IRTIdxTagged (nameInfers nameOf t, r)
    | IRTDist (o, elem, axes) -> IRTDist (o, nameInfers nameOf elem, axes)
    | IRTLoop lt ->
        IRTLoop { lt with
                    ArrayTypes = lt.ArrayTypes |> List.map (nameInfers nameOf)
                    KernelType = lt.KernelType |> Option.map (nameInfers nameOf) }
    | IRTArrow (slots, ret, ident) ->
        let slot = function SVal t -> SVal (nameInfers nameOf t) | s -> s
        IRTArrow (slots |> List.map slot, nameInfers nameOf ret, ident)
    | _ -> t

/// Best-effort recovery of the SOURCE names of abstract type variables: walk
/// an annotation in parallel with its resolved type, recording (inference
/// id -> declared name) wherever unresolved. `T^k` keeps its arity suffix.
let rec collectVarNames (ann: TypeExpr) (t: IRType) : (int * string) list =
    match ann, t with
    | TyVar (name, arity), IRTInfer id ->
        let disp = match arity with
                   | Some k when k > 0 -> $"{name}^{k}"
                   | _ -> name
        [(id, disp)]
    | TyNamed (name, []), IRTInfer id -> [(id, name)]
    | TyAbstractArray (TyVar (name, _), _, _), IRTInfer id -> [(id, name)]
    | TyTuple anns, IRTTuple ts when anns.Length = ts.Length ->
        List.zip anns ts |> List.collect (fun (a, ty) -> collectVarNames a ty)
    | TyFunc (args, ret), IRTArrow (slots, res, _) ->
        let vals = slots |> List.choose (function SVal ty -> Some ty | _ -> None)
        (if args.Length = vals.Length then
            List.zip args vals |> List.collect (fun (a, ty) -> collectVarNames a ty)
         else [])
        @ collectVarNames ret res
    | TyArray (elem, _), ArrayElem arr -> collectVarNames elem arr.ElemType
    | _ -> []

/// Fresh-letter pool for inference vars no source annotation names.
let private typeVarPool =
    seq { yield! ["T"; "U"; "V"; "W"]
          for i in 1 .. 1000 -> $"T{i}" }

/// A per-signature abstract-type renderer: consistent letters across every
/// type it prints (a function's params + return share one namespace).
/// `seed` pre-names inference ids recovered from source annotations.
let abstractRenderer (seed: (int * string) seq) : IRType -> string =
    let named = Dictionary<int, string>()
    for (id, n) in seed do named.[id] <- n
    let used = HashSet<string>(named.Values)
    let nameOf id =
        match named.TryGetValue id with
        | true, n -> n
        | _ ->
            let n = typeVarPool |> Seq.find (fun c -> not (used.Contains c))
            used.Add n |> ignore
            named.[id] <- n
            n
    fun t -> ppType (if hasInfer t then nameInfers nameOf t else t)

// Concrete call-site instantiations (calls[]): the zonked typed tree
// carries full monomorphized types, so builtin applications are reported
// by a plain walk in the compiler's own notation: `Array<Elem like Idx...>`.

/// Every index slot embedded in a type (indexNamesOf only reports slots
/// with a source-level name).
let rec private allIndicesOf (t: IRType) : IRIndexType list =
    match t with
    | ArrayElem arr -> arr.IndexTypes @ allIndicesOf arr.ElemType
    | IRTTuple ts -> ts |> List.collect allIndicesOf
    | IRTComputation inner | IRTPoly (inner, _) | IRTUnitAnnotated (inner, _) -> allIndicesOf inner
    | IRTGroupKeys (outer, source, _) -> [outer; source]
    | IRTDist (_, elem, axes) -> axes @ allIndicesOf elem
    | IRTArrow (slots, ret, _) ->
        (slots |> List.collect (function SIdx i | SIdxVirt i -> [i] | SVal v -> allIndicesOf v))
        @ allIndicesOf ret
    | _ -> []

/// Index-name map for CONCRETE rendering: a slot keeps its nominal name
/// when it has one, otherwise folds to a literal (else `_`), replacing
/// internal extent params (`__ngroups`, `v12`) with a readable wildcard.
let private concreteNames (ts: IRType list) : Map<IRId, string> =
    let nominal = ts |> List.collect indexNamesOf |> Map.ofList
    ts
    |> List.collect allIndicesOf
    |> List.fold (fun (acc: Map<IRId, string>) idx ->
        if Map.containsKey idx.Id acc then acc
        else
            let text =
                match tryEvalIntIR idx.Extent with
                | Some n -> string n
                | None -> "_"
            Map.add idx.Id text acc) nominal

/// Concrete-type rendering: `Array<Elem like Idx...>` for arrays, curried
/// arrows for function types, structural tuples/Computation. Falls back to
/// the named-index printer for everything else.
let rec private ppConcrete (names: Map<IRId, string>) (t: IRType) : string =
    match t with
    | ArrayElem arr ->
        let indices = arr.IndexTypes |> List.map (ppIndexTypeIn names) |> String.concat ", "
        $"Array<{ppConcrete names arr.ElemType} like {indices}>"
    | FuncElem (paramTys, retTy) ->
        let piece ty =
            match ty with
            | FuncElem _ -> $"({ppConcrete names ty})"
            | _ -> ppConcrete names ty
        String.concat " -> " ((paramTys |> List.map piece) @ [ppConcrete names retTy])
    | IRTTuple ts -> $"""({(ts |> List.map (ppConcrete names) |> String.concat ", ")})"""
    | IRTComputation inner -> $"Computation<{ppConcrete names inner}>"
    // Re-rendered here (not the context-free printer upstream) so the whole
    // tooltip obeys the name/literal/`_` rule.
    | IRTGroupKeys (outer, source, _) ->
        $"GroupKeys<{ppIndexTypeIn names outer}, {ppIndexTypeIn names source}>"
    | IRTDist (order, elem, axes) ->
        $"""Dist<{order}, {(ppConcrete names elem)} like {(axes |> List.map (ppIndexTypeIn names) |> String.concat ", ")}>"""
    | other -> ppIRTypeIn names other

/// Every name `builtinCallOf` below can return, in arm order -- the companion
/// the language-surface dump reports, since a private `match` cannot be
/// enumerated. KEEP IN SYNC with builtinCallOf: adding an arm means adding its
/// name here. The `OpMath` arm is deliberately absent -- it returns whichever
/// intrinsic the node names, and those are already reported as
/// `mathIntrinsics`.
let builtinCallNames : string list =
    [ "hermitian"; "conj"; "method_for"; "object_for"; "pure"; "compute"; "read"
      "guard"; "reynolds"; "zero"; "rank"; "arity"; "extents"; "reduce"; "mask"
      "compound"; "sparse"; "zip"; "stack"; "sort"; "unique"; "intersect"; "union"
      "contains"; "display.emit"; "group_by"; "group_keys"; "transpose"; "decompact"
      "gram"; "matmul"; "eigh"; "solve"; "sequence"; "replicate"; "complex"
      "prodsum"; "fill_random" ]

/// The builtin a typed node is an application of, with its argument nodes
/// in source order -- None for a non-builtin call (PPL/ML surfaces are
/// collected separately by collectFormerCalls). `hermitian(A)` is rewritten
/// by the parser into conj-of-transpose sharing one span, matched as a unit.
let private builtinCallOf (te: TypedExpr) : (string * TypedExpr list) option =
    match te.Kind with
    // Array operands conjugate through TExprArrayConjugate; scalar ones keep
    // the unary op. Match both shapes.
    | TExprArrayConjugate ({ Kind = TExprTranspose (a, 0, 1) } as inner)
    | TExprUnaryOp (OpConj, ({ Kind = TExprTranspose (a, 0, 1) } as inner))
        when inner.Span = te.Span && te.Span.StartLine > 0 -> Some ("hermitian", [a])
    | TExprArrayConjugate a -> Some ("conj", [a])
    | TExprMethodFor info -> Some ("method_for", info.Arrays)
    | TExprObjectFor info -> Some ("object_for", [info.Kernel])
    | TExprPure e -> Some ("pure", [e])
    | TExprCompute e -> Some ("compute", [e])
    | TExprRead e -> Some ("read", [e])
    | TExprGuard (c, b) -> Some ("guard", [c; b])
    | TExprReynolds (k, _) -> Some ("reynolds", [k])
    | TExprZero -> Some ("zero", [])
    | TExprRank e -> Some ("rank", [e])
    | TExprArity _ -> Some ("arity", [])
    | TExprExtents a -> Some ("extents", [a])
    | TExprReduce (a, k, i) -> Some ("reduce", [a; k] @ Option.toList i)
    | TExprMask (a, p) -> Some ("mask", [a; p])
    | TExprCompound (d, m) -> Some ("compound", [d; m])
    | TExprSparse (v, k) -> Some ("sparse", [v; k])
    | TExprZip es -> Some ("zip", es)
    | TExprStack es -> Some ("stack", es)
    | TExprSort (a, k) -> Some ("sort", [a; k])
    | TExprUnique a -> Some ("unique", [a])
    | TExprIntersect (a, b) -> Some ("intersect", [a; b])
    | TExprUnion (a, b) -> Some ("union", [a; b])
    | TExprContains (a, v) -> Some ("contains", [a; v])
    // Both spellings report under the ONE name `display.emit`: `builtinCallNames`
    // above is the checked-in enumeration of everything this function can
    // return (protocol/surface.json is generated from it and a self-test pins
    // the two together), and `emit_id` is the same builtin with one more
    // operand, not a second builtin. The id operand IS reported, so the call
    // record carries both argument types.
    | TExprDisplayEmit (_, _, d, _, idOpt) -> Some ("display.emit", d :: Option.toList idOpt)
    | TExprGroupBy (v, g) -> Some ("group_by", [v; g])
    | TExprGroupKeys ks -> Some ("group_keys", ks)
    | TExprGroupBucket gk -> Some ("group_bucket", [gk])
    | TExprTranspose (a, _, _) -> Some ("transpose", [a])
    | TExprDecompact (a, _) -> Some ("decompact", [a])
    | TExprGram (l, r, _) -> Some ("gram", [l; r])
    | TExprMatmul (l, r) -> Some ("matmul", [l; r])
    | TExprEigh a -> Some ("eigh", [a])
    | TExprSolve (a, b) -> Some ("solve", [a; b])
    | TExprSequence es -> Some ("sequence", es)
    | TExprReplicate (c, b) -> Some ("replicate", [c; b])
    | TExprComplexLit (re, im) -> Some ("complex", [re; im])
    | TExprFma (a, b, c) -> Some ("fma", [a; b; c])
    | TExprProdSum args -> Some ("prodsum", args)
    | TExprFillRandom m -> Some ("fill_random", [m])
    | TExprUnaryOp (OpMath name, a) -> Some (name, [a])
    | TExprUnaryOp (OpCast name, a) -> Some (name, [a])
    | TExprUnaryOp (OpConj, a) -> Some ("conj", [a])
    | _ -> None

/// One call entry from a name, span and already-typed operands.
let private mkCall (name: string) (span: Span) (argTys: IRType list) (retTy: IRType) : CallInfo =
    let (line, col, endLine, endCol) = clampSpan span
    let names = concreteNames (retTy :: argTys)
    { CName = name; CLine = line; CCol = col
      CEndLine = endLine; CEndCol = endCol
      CArgs = argTys |> List.map (ppConcrete names)
      CRet = ppConcrete names retTy }

/// Walk the zonked typed program collecting every builtin call site with a
/// live source span (synthesized nodes carry noSpan and are skipped).
let private collectCalls (tp: TypedProgram) : CallInfo list =
    let acc = ResizeArray<CallInfo>()
    let rec walk (te: TypedExpr) =
        let hit = builtinCallOf te
        (match hit with
         | Some (name, args) when te.Span.StartLine > 0 ->
             acc.Add (mkCall name te.Span (args |> List.map _.Type) te.Type)
         | _ -> ())
        // `hermitian` consumed its own transpose node above; recursing would
        // report that expansion a second time.
        match hit, te.Kind with
        | Some ("hermitian", args), _ -> for a in args do walk a
        | _ -> for c in Blade.TypeCheck.typedExprChildren te do walk c
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b -> walk b.Value
            | TDeclFunction f -> walk f.Body
            | TDeclImpl impl -> for f in impl.Methods do walk f.Body
            | _ -> ()
    // An eta-expanded former and the kernel it wraps can land on one span
    // (both are the same source text); report each name once per position.
    acc
    |> Seq.distinctBy (fun c -> (c.CName, c.CLine, c.CCol, c.CEndLine, c.CEndCol))
    |> List.ofSeq

// Import-gated surfaces (ppl.* / ml.*): rewritten by their elaborators
// before the checker runs, so recovered from the pre-elaboration AST
// joined to the checked types (a former is the entire RHS of a `let`).

/// Binding types by name from the checked program. Module-level bindings
/// win; function parameters and body lets are added underneath so a former
/// called inside a function can still type its operands.
let private moduleBindingTypes (tp: TypedProgram) : Map<string, IRType> =
    let acc = Dictionary<string, IRType>()
    let addLocal (n: string) (t: IRType) = if not (acc.ContainsKey n) then acc.[n] <- t
    let rec localsOf (stmts: TypedStmt list) =
        for s in stmts do
            match s with
            | TStmtLet b ->
                addLocal b.Name b.Type
                for (n, _, t) in b.SubBindings do addLocal n t
            | TStmtForIn (v, _, _, _, body) ->
                addLocal v (IRTScalar ETInt64)
                localsOf body
            | _ -> ()
    let fnLocals (f: TypedFunctionDecl) =
        for p in f.Params do addLocal p.Name p.Type
        match f.Body.Kind with
        | TExprBlock (stmts, _) -> localsOf stmts
        | _ -> ()
    // Module level first, so it takes precedence over any same-named local.
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b ->
                acc.[b.Name] <- b.Type
                for (n, _, t) in b.SubBindings do acc.[n] <- t
            | _ -> ()
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclFunction f -> fnLocals f
            | TDeclImpl impl -> for f in impl.Methods do fnLocals f
            | _ -> ()
    acc |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// A dist binding's NOMINAL type, rebuilt from the PPL registry: dists
/// erase to a tuple of their components, so the raw binding type would
/// show that tuple where the signature promises `Dist<r, Elem like axes>`.
let private distTypeOf (types: Map<string, IRType>) (name: string) : IRType option =
    match Blade.Ppl.Elaborate.IdeDists.tryFind name with
    | Some (order, k1 :: _) ->
        match Map.tryFind k1 types with
        | Some (ArrayElem arr) -> Some (IRTDist (order, arr.ElemType, arr.IndexTypes))
        | _ -> None
    | _ -> None

/// Aliases bound to the import-gated modules whose surfaces `calls[]` covers,
/// as alias -> module name (`import ppl as p` -> "p" -> "ppl").
let private surfaceAliases (prog: Ast.Program) : Map<string, string> =
    let acc = Dictionary<string, string>()
    let consider (qn: string list) (aliasOpt: string option) =
        match List.tryLast qn with
        | Some m when m = "ppl" || m = "ml" -> acc.[defaultArg aliasOpt m] <- m
        | _ -> ()
    for m in prog.Modules do
        for imp in m.Imports do consider imp.Module imp.Alias
        for ld in m.Decls do
            match ld.Value with
            | DeclImport (qn, ImportQualified aliasOpt) -> consider qn aliasOpt
            | _ -> ()
    acc |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// Typed argument lists of the specialized functions the ML elaborator
/// generates, keyed by source span: `ml.linear(SPEC_IN, SPEC_OUT, w, x)`
/// becomes `__ml_N(w, x)`, so inline array-literal operands stay typeable.
let private mlGeneratedArgs (tp: TypedProgram) : Map<(int * int * int * int), IRType list> =
    let acc = Dictionary<(int * int * int * int), IRType list>()
    let rec walk (te: TypedExpr) =
        (match te.Kind with
         | TExprApp ({ Kind = TExprVar (fn, _, _) }, args)
             when fn.StartsWith "__ml" && te.Span.StartLine > 0 ->
             acc.[clampSpan te.Span] <- (args |> List.map _.Type)
         | _ -> ())
        for c in Blade.TypeCheck.typedExprChildren te do walk c
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b -> walk b.Value
            | TDeclFunction f -> walk f.Body
            | TDeclImpl impl -> for f in impl.Methods do walk f.Body
            | _ -> ()
    acc |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// A qualified surface call `alias.op(args)` (or the bare `op(args)` an
/// elaborator pass may already have normalized to), with its operand exprs.
let private surfaceCallOf (aliases: Map<string, string>) (e: Expr) : (string * Expr list) option =
    match e.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, op) }, args)
        when aliases.ContainsKey alias -> Some (op, args)
    | _ -> None

/// Former/op calls in declaration RHS position, typed from the checked
/// program. Only the `let x = alias.op(...)` shape is reported, since that
/// is what makes the result type recoverable from `x`.
let private collectFormerCalls (prog: Ast.Program) (tp: TypedProgram) : CallInfo list =
    let aliases = surfaceAliases prog
    if aliases.IsEmpty then [] else
    let types = moduleBindingTypes tp
    let mlArgs = mlGeneratedArgs tp
    // A dist name reads as its nominal Dist type, not the tuple it erases to.
    let typeOfName (n: string) : IRType option =
        match distTypeOf types n with
        | Some d -> Some d
        | None -> Map.tryFind n types
    let render (t: IRType) = ppConcrete (concreteNames [t]) t
    let argType (a: Expr) : string option =
        match a.Kind with
        | ExprKind.ExprVar n -> typeOfName n |> Option.map render
        | ExprKind.ExprLit (LitInt n) -> Some (string n)
        | ExprKind.ExprLit (LitFloat _) -> Some "Float64"
        | _ -> None
    [ for m in prog.Modules do
        for ld in m.Decls do
            match ld.Value with
            | DeclLet b | DeclStatic b ->
                match b.Pattern.Kind, surfaceCallOf aliases b.Value with
                | PatternKind.PatVar outName, Some (op, args) ->
                    match typeOfName outName with
                    | Some retTy ->
                        // An argument whose type we cannot recover renders `_`
                        // rather than dropping the whole call.
                        let span = if b.Value.Span.StartLine > 0 then b.Value.Span else ld.Span
                        let key = clampSpan span
                        let (line, col, endLine, endCol) = key
                        // The elaborator consumes the leading static arguments
                        // and keeps the runtime ones, so the generated call's
                        // args align to the tail of the surface args.
                        let generated = defaultArg (Map.tryFind key mlArgs) []
                        let offset = args.Length - generated.Length
                        let argAt i (a: Expr) =
                            match argType a with
                            | Some s -> s
                            | None when offset >= 0 && i >= offset ->
                                render generated.[i - offset]
                            | None -> "_"
                        yield { CName = op; CLine = line; CCol = col
                                CEndLine = endLine; CEndCol = endCol
                                CArgs = args |> List.mapi argAt
                                CRet = ppConcrete (concreteNames [retTy]) retTy }
                    | None -> ()
                | _ -> ()
            | _ -> () ]

// Doc comments

let private directiveRe = Regex(@"^(TEST|EXPECT|MODULE|EXPECT_OUTPUT|EXPECT_ERROR)\b", RegexOptions.Compiled)

/// A line that is only banner punctuation (`// ====...`) -- filtered from docs.
let private isBanner (s: string) =
    s.Length > 0 && s |> Seq.forall (fun c -> c = '=' || c = '-' || c = '*' || c = '#')

/// The contiguous `//` comment block directly above 1-based line `line`,
/// stripped of comment markers, directives, and banner lines.
let private docAbove (lines: string[]) (line: int) : string =
    let acc = ResizeArray<string>()
    let mutable i = line - 2   // 0-based index of the line above the binding
    let mutable go = true
    while go && i >= 0 do
        let t = lines.[i].TrimStart()
        if t.StartsWith "//" then
            acc.Add(t.TrimStart('/').Trim())
            i <- i - 1
        else
            go <- false
    let cleaned =
        acc
        |> Seq.rev
        |> Seq.filter (fun l -> not (directiveRe.IsMatch l) && not (isBanner l))
        |> List.ofSeq
    // Drop leading/trailing blank lines the filtering may have exposed.
    let rec trimEnds = function
        | "" :: rest -> trimEnds rest
        | xs -> xs
    cleaned |> trimEnds |> List.rev |> trimEnds |> List.rev |> String.concat "\n"

/// Ionide-style per-parameter doc: a doc-block line of the form
/// `name: description` (optionally bulleted) documents parameter `name`.
let private paramDocIn (doc: string) (pname: string) : string =
    if doc = "" then ""
    else
        let re = Regex(sprintf @"^[\s\-\*]*%s\s*[:\u2014-]\s*(.+)$" (Regex.Escape pname))
        doc.Split('\n')
        |> Array.tryPick (fun l ->
            let m = re.Match l
            if m.Success then Some (m.Groups.[1].Value.Trim()) else None)
        |> Option.defaultValue ""

// Untyped-side span collection: (scopeKey, name, span, kind option) in
// declaration order. scopeKey is "" at module level, the function name
// inside a function body.

/// Every name a binding pattern binds, each with the span of its OWN name
/// token: a PatVar leaf's `Span` is exactly the identifier (the parser builds
/// it with `mkPat (headSpan tokens)`), which is what rename has to rewrite.
let rec private patternNameSpans (p: Pattern) : (string * Span) list =
    match p.Kind with
    | PatternKind.PatVar name -> [(name, p.Span)]
    | PatternKind.PatTuple ps -> ps |> List.collect patternNameSpans
    | PatternKind.PatCons (a, b) -> patternNameSpans a @ patternNameSpans b
    | PatternKind.PatTyped (inner, _) -> patternNameSpans inner
    | PatternKind.PatGuarded (inner, _) -> patternNameSpans inner
    | PatternKind.PatStruct (_, fields) -> fields |> List.collect (snd >> patternNameSpans)
    | PatternKind.PatVariant (_, inner) -> inner |> Option.map patternNameSpans |> Option.defaultValue []
    | PatternKind.PatWildcard | PatternKind.PatLit _ -> []

let private patternNames (p: Pattern) : string list =
    patternNameSpans p |> List.map fst

/// Find a name token INSIDE a source region, as the first whole-word match
/// strictly within the region's bounds. Two jobs:
///
///   * the binders whose AST node has no span slot to carry a name span --
///     `for i in ...` heads (StmtForIn keeps only a string) and `type X = ...`
///     declarations (TypeDecl is a bare DU); both put the name right after a
///     keyword, so the first match in the declaration IS the binding site;
///   * narrowing a reference span that covers more than its identifier -- the
///     parser hands a parenthesized `(A)` the parens' span, and rename would
///     otherwise replace the parentheses too.
///
/// Comment tails are cut first, so a mention in a trailing `//` can never win.
let private locateName (lines: string[]) (region: Span) (name: string) : Span option =
    if name = "" || lines.Length = 0 || region.StartLine < 1 then None
    else
        let re = Regex(@"\b" + Regex.Escape name + @"\b")
        let startLine = region.StartLine
        let endLine = min lines.Length (max startLine region.EndLine)
        let rec scan (ln: int) =
            if ln > endLine || ln > lines.Length then None
            else
                let raw = lines.[ln - 1]
                let uncommented = match raw.IndexOf "//" with
                                  | -1 -> raw
                                  | i -> raw.Substring(0, i)
                // EndCol is EXCLUSIVE throughout the compiler (Lexer.Token), so
                // the region's last character sits at index EndCol - 2.
                let text =
                    if ln = endLine then uncommented.Substring(0, min uncommented.Length (max 0 (region.EndCol - 1)))
                    else uncommented
                let from = if ln = startLine then min (max 0 (region.StartCol - 1)) text.Length else 0
                let m = re.Match(text, from)
                if m.Success then
                    Some { StartLine = ln; StartCol = m.Index + 1
                           EndLine = ln; EndCol = m.Index + m.Length + 1
                           File = region.File }
                else scan (ln + 1)
        scan startLine

/// The names a `type` declaration introduces (a mutual group introduces one
/// per member).
let private typeDeclNames (td: TypeDecl) : string list =
    match td with
    | TyDeclAlias (n, _, _) -> [n]
    | TyDeclSum (n, _, _) -> [n]
    | TyDeclStruct (n, _, _, _, _) -> [n]
    | TyDeclMutualGroup (members, _) -> members |> List.map fst

/// Binding-keyword kind from the surface syntax (TypedBinding.IsMutable is
/// not usable here: module-level bindings come back mutable regardless).
let private bindingKind (b: Binding) =
    match b.Mutability with
    | BindMut -> "let mut"
    // BindConst is internal-only (let static / local function desugar);
    // `const` is not surface syntax.
    | BindConst -> "let static"
    | BindLet -> "let"

let private collectSourceBindings (prog: Ast.Program) =
    // Some kind for let-style bindings (surface keyword is authoritative),
    // None where the typed side names the kind (function / param).
    let acc = ResizeArray<string * string * Span * string option>()
    let rec walkStmts (scope: string) (stmts: Stmt list) (declSpan: Span) =
        for s in stmts do
            let (span, inner) =
                match s with
                | StmtSpanned (inner, sp) -> (sp, unwrapStmt inner)
                | other -> (declSpan, unwrapStmt other)
            match inner with
            | StmtLet b -> for n in patternNames b.Pattern do acc.Add(scope, n, span, Some (bindingKind b))
            | StmtForIn (v, _, body) ->
                acc.Add(scope, v, span, Some "for")
                walkStmts scope body span
            | _ -> ()
    let walkFuncBody (scope: string) (body: Expr) (declSpan: Span) =
        match body.Kind with
        | ExprKind.ExprBlock (stmts, _) -> walkStmts scope stmts declSpan
        | _ -> ()
    let addFunc (f: FunctionDecl) (span: Span) =
        acc.Add("", f.Name, span, None)
        for p in f.Params do acc.Add(f.Name, p.Name, span, None)
        walkFuncBody f.Name f.Body span
    for m in prog.Modules do
        for ld in m.Decls do
            match ld.Value with
            | DeclLet b ->
                for n in patternNames b.Pattern do acc.Add("", n, ld.Span, Some (bindingKind b))
            | DeclStatic b ->
                for n in patternNames b.Pattern do acc.Add("", n, ld.Span, Some "static")
            | DeclFunction f -> addFunc f ld.Span
            | DeclImpl impl -> for f in impl.Methods do addFunc f ld.Span
            | _ -> ()
    acc

// Typed-side collection, in decl order.

/// Render a function's where-clause as displayable conjunct strings: comm
/// groups, parallelization strategies, and open custom conjuncts (indep
/// etc.). TDim specs are internal shape scaffolding and not shown.
let private whereConjuncts (wc: WhereClause option) : string list =
    match wc with
    | None -> []
    | Some w ->
        let comms =
            w.Commutativity
            |> List.map (fun group -> $"""comm({(String.concat ", " group)})""")
        let antis =
            w.Antisymmetry
            |> List.map (fun group -> $"""anticomm({(String.concat ", " group)})""")
        let pars =
            w.Parallel
            |> List.map (function
                | Omp s ->
                    let vars = s.Vars |> List.map (fun (v, n) -> $"{v}: {n}")
                    $"""omp({(String.concat ", " vars)})"""
                | Cuda s -> $"cuda(block: {s.BlockSize})"
                | Mpi -> "mpi")
        let customs =
            w.Custom
            |> List.map (fun (name, args) -> $"""{name}({(String.concat ", " args)})""")
        comms @ antis @ pars @ customs

/// Collapse adjacent-pair parities into canonical pin-clause strings: a
/// maximal run of PInv pairs over params i..j+1 becomes one comm(...)
/// group, a PNeg run one anticomm(...) group (`__`-named runs are dropped).
let private parityClauses (names: string list) (parities: Blade.Deduce.Parity list) : string list =
    let nameArr = List.toArray names
    let parArr = List.toArray parities
    let clauses = ResizeArray<string>()
    let mutable i = 0
    while i < parArr.Length do
        let kw =
            match parArr.[i] with
            | Blade.Deduce.PInv -> Some "comm"
            | Blade.Deduce.PNeg -> Some "anticomm"
            // PConj (Hermitian, f(y,x) = conj(f(x,y))) has no writable
            // clause yet -- it exists to refute comm/anticomm, so it
            // renders like PBottom rather than proposing a pin.
            | Blade.Deduce.PConj | Blade.Deduce.PBottom -> None
        match kw with
        | None -> i <- i + 1
        | Some kw ->
            let mutable j = i
            while j + 1 < parArr.Length && parArr.[j + 1] = parArr.[i] do j <- j + 1
            // Pairs i..j span params i..j+1.
            if j + 1 < nameArr.Length then
                let group = [ for x in i .. j + 1 -> nameArr.[x] ]
                if group |> List.forall (fun n -> not (n.StartsWith "__")) then
                    clauses.Add($"""{kw}({(String.concat ", " group)})""")
            i <- j + 1
    List.ofSeq clauses

type private TypedEntry = {
    Scope: string
    EName: string
    EKind: string
    ETypeStr: string
    EParams: (string * string * int option * string option) list   // name, type, deduced min rank, default text
    ERet: string option
    EWhere: string list
    EDeducedComm: string list
}

/// Compact one-line rendering of a surface DEFAULT expression for signature
/// help. Best-effort: the shapes defaults actually take (literals, names,
/// small arithmetic, calls) render exactly; anything larger elides to "...".
let rec private ppDefaultExpr (e: Expr) : string =
    let binOpToken op =
        match op with
        | OpAdd -> "+" | OpSub -> "-" | OpMul -> "*" | OpDiv -> "/"
        | OpMod -> "%" | OpCaret -> "^"
        | OpEq -> "==" | OpNeq -> "!=" | OpLt -> "<" | OpLe -> "<="
        | OpGt -> ">" | OpGe -> ">=" | OpAnd -> "&&" | OpOr -> "||"
        | _ -> "?"
    match e.Kind with
    | ExprLit (LitInt n) -> string n
    | ExprLit (LitFloat f) ->
        let s = sprintf "%g" f
        if s.Contains "." || s.Contains "e" || s.Contains "E" then s else s + ".0"
    | ExprLit (LitBool b) -> if b then "true" else "false"
    | ExprLit (LitString s) -> $"\"{s}\""
    | ExprVar n -> n
    | ExprUnaryOp (OpNeg, inner) -> "-" + ppDefaultExpr inner
    | ExprBinOp (_, op, l, r) -> $"{ppDefaultExpr l} {binOpToken op} {ppDefaultExpr r}"
    | ExprApp (f, args) -> $"""{(ppDefaultExpr f)}({(args |> List.map ppDefaultExpr |> String.concat ", ")})"""
    | ExprField (b, f) -> $"{ppDefaultExpr b}.{f}"
    | ExprTyped (inner, _) -> ppDefaultExpr inner
    | ExprTuple es -> $"""({(es |> List.map ppDefaultExpr |> String.concat ", ")})"""
    | _ -> "..."

let private collectTypedBindings (srcFuncs: Map<string, FunctionDecl>) (tp: TypedProgram) =
    let acc = ResizeArray<TypedEntry>()
    // Deduction side-channel snapshots (IdeDeductions, recorded during
    // typeCheck): per-function adjacent-pair parities and pack parities.
    // Last write wins on redefinition, like the TypeEnv tables they mirror.
    let dedPairs =
        Blade.TypeCheckIde.IdeDeductions.getPairs ()
        |> List.fold (fun m (n, v) -> Map.add n v m) Map.empty
    let dedPacks =
        Blade.TypeCheckIde.IdeDeductions.getPacks ()
        |> List.fold (fun m (n, v) -> Map.add n v m) Map.empty
    // Each value binding names its own abstract vars: schemes don't share
    // ids across bindings, so per-binding namespaces can't collide.
    let ppVal (t: IRType) = abstractRenderer [] t
    let add scope name kind tyStr =
        acc.Add { Scope = scope; EName = name; EKind = kind; ETypeStr = tyStr
                  EParams = []; ERet = None; EWhere = []; EDeducedComm = [] }
    let rec walkTStmts (scope: string) (stmts: TypedStmt list) =
        for s in stmts do
            match s with
            | TStmtLet b ->
                add scope b.Name "let" (ppVal b.Type)
                for (n, _, t) in b.SubBindings do add scope n "let" (ppVal t)
            | TStmtForIn (v, _, _, _, body) ->
                add scope v "for" (ppType (IRTScalar ETInt64))
                walkTStmts scope body
            | _ -> ()
    let walkFuncBody (scope: string) (body: TypedExpr) =
        match body.Kind with
        | TExprBlock (stmts, _) -> walkTStmts scope stmts
        | _ -> ()
    let addFunc (f: TypedFunctionDecl) =
        // One abstract-var namespace across the whole signature, seeded with
        // the SOURCE type-variable names where the annotations reveal them.
        let seed =
            match Map.tryFind f.Name srcFuncs with
            | Some src when src.Params.Length = f.Params.Length ->
                [ for (p, tp) in List.zip src.Params f.Params do
                    match p.Type with
                    | Some ann -> yield! collectVarNames ann tp.Type
                    | None -> ()
                  match src.ReturnType with
                  | Some ann -> yield! collectVarNames ann f.ReturnType
                  | None -> () ]
            | _ -> []
        let pp = abstractRenderer seed
        // Deduced minimum rank: a param the source left unannotated whose
        // resolved type is an array got that rank from its body uses.
        // Annotated params show their rank in the signature, no minRank.
        let srcAnnotated =
            match Map.tryFind f.Name srcFuncs with
            | Some src when src.Params.Length = f.Params.Length ->
                src.Params |> List.map _.Type.IsSome |> List.toArray
            | _ -> f.Params |> List.map (fun _ -> true) |> List.toArray
        // Surface defaults by param position (for the optional "default"
        // field on params[] -- signature help shows what an omitted arg gets).
        let srcDefaults =
            match Map.tryFind f.Name srcFuncs with
            | Some src when src.Params.Length = f.Params.Length ->
                src.Params |> List.map (fun p -> p.Default |> Option.map ppDefaultExpr) |> List.toArray
            | _ -> f.Params |> List.map (fun _ -> None) |> List.toArray
        let ps =
            f.Params
            |> List.mapi (fun i p ->
                let minRank =
                    if srcAnnotated.[i] then None
                    else
                        match p.Type with
                        | ArrayElem arr when not arr.IndexTypes.IsEmpty ->
                            Some arr.IndexTypes.Length
                        | _ -> None
                (p.Name, pp p.Type, minRank, srcDefaults.[i]))
        let ret = pp f.ReturnType
        let kind = if f.IsStatic then "static function" else "function"
        let deducedComm =
            (match Map.tryFind f.Name dedPairs with
             | Some (names, parities) -> parityClauses names parities
             | None -> [])
            @ (match Map.tryFind f.Name dedPacks with
               | Some (packName, Blade.Deduce.PInv) -> [$"comm({packName})"]
               | _ -> [])
        acc.Add { Scope = ""; EName = f.Name; EKind = kind
                  ETypeStr = formatFunctionSig (ps |> List.map (fun (n, t, _, _) -> (n, t))) ret
                  EParams = ps; ERet = Some ret
                  EWhere = whereConjuncts f.WhereClause
                  EDeducedComm = deducedComm }
        for p in f.Params do add f.Name p.Name "param" (pp p.Type)
        walkFuncBody f.Name f.Body
    // Module-level let types by name, for rebuilding erased dists below.
    let moduleLets = Dictionary<string, IRType>()
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b ->
                add "" b.Name "let" (ppVal b.Type)
                moduleLets.[b.Name] <- b.Type
                for (n, _, t) in b.SubBindings do add "" n "let" (ppVal t)
            | TDeclStatic b ->
                add "" b.Name "static" (ppVal b.Type)
                for (n, _, t) in b.SubBindings do add "" n "static" (ppVal t)
            | TDeclFunction f -> addFunc f
            | TDeclImpl impl -> for f in impl.Methods do addFunc f
            | _ -> ()
    // Erased dists: the flat pushforward formers are register-only, with no
    // decl under the user's name. Rebuild Dist<order, elem like axes> from
    // the first component's inferred type (inverts exactly).
    let named = HashSet<string>(acc |> Seq.filter (fun e -> e.Scope = "") |> Seq.map _.EName)
    for (name, order, comps) in Blade.Ppl.Elaborate.IdeDists.entries () do
        if not (named.Contains name) then
            match comps with
            | k1 :: _ ->
                match moduleLets.TryGetValue k1 with
                | true, ArrayElem arr -> add "" name "let" (ppVal (IRTDist (order, arr.ElemType, arr.IndexTypes)))
                | _ -> ()
            | [] -> ()
    acc

// Type-provider structure: provided members, the store handle, and the
// alias are not ordinary bindings, so this re-derives, per loaded store,
// the index/dims/vars structure plus provider-read provenance.

/// alias -> provider module name for every `import <p> as <alias>` (or bare
/// `import <p>`) whose module is a registered data provider.
let private providerAliases (prog: Ast.Program) : Map<string, string> =
    let acc = Dictionary<string, string>()
    let consider (qn: string list) (aliasOpt: string option) =
        match List.tryLast qn with
        | Some modName when (Blade.ProviderRegistry.tryFind modName).IsSome ->
            acc.[defaultArg aliasOpt modName] <- modName
        | _ -> ()
    for m in prog.Modules do
        for imp in m.Imports do consider imp.Module imp.Alias
        for ld in m.Decls do
            match ld.Value with
            | DeclImport (qn, ImportQualified aliasOpt) -> consider qn aliasOpt
            | _ -> ()
    acc |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq

/// Which direction a provider-verb binding faces. A `write` binding does not
/// read the store member it names -- it PERSISTS an array that came from there
/// -- so the two travel in separate payload fields and must not be conflated.
type private ProvDirection =
    | ProvRead
    | ProvWrite

/// The `store.vars.v` / `store.dims.v` receiver of a provider-verb call (the
/// pipe desugars to `alias.read(store.vars.v)`), recovered from the untyped
/// RHS so a top-level provider-read binding names its source. `read`/`stream`
/// take the view alone; `read_window` adds window bounds and `load_compound`
/// a mask, view first in both. `write` takes a prior named binding, never a
/// view, so its provenance is that binding's own, when one was recorded
/// (`priorOf` -- the caller accumulates in declaration order) -- and comes back
/// tagged `ProvWrite`, because what it names is where the array CAME FROM, not
/// something this binding reads.
let private readOperandProvenance (aliases: Map<string, string>) (priorOf: string -> (string * string) option)
                                  (v: Expr) : (ProvDirection * (string * string)) option =
    let storeMember (operand: Expr) =
        match operand.Kind with
        | ExprKind.ExprField ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar store }, section) }, field)
            when section = "vars" || section = "dims" ->
            Some (store, $"{section}.{field}")
        | _ -> None
    match v.Kind with
    | ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, meth) }, args)
        when aliases.ContainsKey alias ->
        (match meth, args with
         | ("read" | "stream"), [operand]                    // alias.read(v) / alias.stream(v)
         | "read_window", [operand; _; _]                    // alias.read_window(v, lo, hi)
         | "load_compound", [operand; _] ->                  // alias.load_compound(v, mask)
             storeMember operand |> Option.map (fun pr -> ProvRead, pr)
         | "write", [_; { Kind = ExprKind.ExprVar src }] ->  // alias.write("path", v)
             priorOf src |> Option.map (fun pr -> ProvWrite, pr)
         | _ -> None)
    | _ -> None

/// bindingName -> (store, "vars.v"), as two maps: READS (`alias.read` and its
/// siblings) and WRITES (`alias.write`, chased to the source binding it
/// persists).
///
/// The write chase is scoped to ONE MODULE: `alias.write("p", obs)` resolves
/// `obs` against its own module's bindings, so `priorOf`'s accumulator is
/// effectively keyed by (module, name) -- a program-wide name-keyed
/// accumulator would let a `let obs` in module A hand its provenance to an
/// unrelated same-named write in module B.
///
/// The RETURNED maps stay name-keyed: `joinBindings` pairs them against typed
/// binding entries, which carry a scope but no module identity, so
/// cross-module duplicates of a top-level name still collide there (last
/// module wins) -- only the CHASE above is module-scoped.
let private readProvenance (prog: Ast.Program) (aliases: Map<string, string>)
    : Map<string, string * string> * Map<string, string * string> =
    let reads = Dictionary<string, string * string>()
    let writes = Dictionary<string, string * string>()
    if not aliases.IsEmpty then
        for m in prog.Modules do
            let prior = Dictionary<string, string * string>()
            let priorOf name =
                match prior.TryGetValue name with
                | true, pr -> Some pr
                | _ -> None
            for ld in m.Decls do
                match ld.Value with
                | DeclLet b | DeclStatic b ->
                    match b.Pattern.Kind with
                    | PatternKind.PatVar name ->
                        match readOperandProvenance aliases priorOf b.Value with
                        | Some (dir, pr) ->
                            // Both directions feed the chase (a write of a
                            // write still names the array's origin); only the
                            // OUTPUT channel splits.
                            prior.[name] <- pr
                            (match dir with
                             | ProvRead -> reads.[name] <- pr
                             | ProvWrite -> writes.[name] <- pr)
                        | None -> ()
                    | _ -> ()
                | _ -> ()
    (reads |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq,
     writes |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq)

/// Provided structure for every `let store = alias.load("path")`, rendered
/// from the module TypeCheck already stashed in IdeStores -- so this NEVER
/// re-opens the data file. A store whose load didn't type-check is skipped.
let private collectProviderStores (prog: Ast.Program) : ProviderInfo list =
    let aliases = providerAliases prog
    if aliases.IsEmpty then [] else
    let describe store alias provider path (span: Span) (pm: IRModule) : ProviderInfo option =
        try
            let names = indexNameMap pm
            let ppIn t = ppIRTypeIn names t
            let membersOf label =
                pm.Types
                |> List.tryPick (function
                    | IRTDStruct (n, fields)
                        when n = label || n = $"{store}__{label}" -> Some fields
                    | _ -> None)
                |> Option.defaultValue []
                |> List.map (fun (fn, ft) -> { MName = fn; MType = ppIn ft })
            let idxTypes =
                pm.Types
                |> List.choose (function
                    | IRTDIndexType (n, idx) ->
                        let ext =
                            match idx.Extent with
                            | IRLit (IRLitInt v) -> Some v
                            | _ -> None
                        Some { IName = n; IExtent = ext }
                    | _ -> None)
            let (line, col, _, _) = clampSpan span
            Some { Store = store; Alias = alias; Provider = provider; Path = path
                   PLine = line; PCol = col
                   IndexTypes = idxTypes; Dims = membersOf "dims"; Vars = membersOf "vars" }
        with _ -> None
    [ for m in prog.Modules do
        for ld in m.Decls do
            match ld.Value with
            | DeclLet b ->
                match b.Pattern.Kind, b.Value.Kind with
                | PatternKind.PatVar store,
                  ExprKind.ExprApp ({ Kind = ExprKind.ExprField ({ Kind = ExprKind.ExprVar alias }, "load") },
                                    [{ Kind = ExprKind.ExprLit (LitString path) }]) ->
                    match Map.tryFind alias aliases, Blade.ProviderRegistry.IdeStores.tryFind store with
                    | Some provider, Some pm ->
                        match describe store alias provider path ld.Span pm with
                        | Some info -> yield info
                        | None -> ()
                    | _ -> ()
                | _ -> ()
            | _ -> () ]

/// Join typed bindings to source spans by (scope, name), consuming spans in
/// declaration order so shadowed/reused names pair up positionally (typed
/// decls with no source span are dropped; the surface keyword kind wins).
let private joinBindings (prog: Ast.Program) (tp: TypedProgram) (sourceLines: string[]) : BindingInfo list =
    // Source-side function decls by name, for recovering declared
    // type-variable names in signatures.
    let srcFuncs =
        [ for m in prog.Modules do
            for ld in m.Decls do
                match ld.Value with
                | DeclFunction f -> yield (f.Name, f)
                | DeclImpl impl -> for f in impl.Methods do yield (f.Name, f)
                | _ -> () ]
        |> Map.ofList
    let spans = Dictionary<string, Queue<Span * string option>>()
    for (scope, name, span, kindOpt) in collectSourceBindings prog do
        let key = scope + " " + name
        match spans.TryGetValue key with
        | true, q -> q.Enqueue((span, kindOpt))
        | _ ->
            let q = Queue<Span * string option>()
            q.Enqueue((span, kindOpt))
            spans.[key] <- q
    // Memoize doc blocks per source line: params share their function's line
    // (param-level spans don't exist), so the block is fetched repeatedly.
    let docCache = Dictionary<int, string>()
    let docAt line =
        match docCache.TryGetValue line with
        | true, d -> d
        | _ ->
            let d = docAbove sourceLines line
            docCache.[line] <- d
            d
    // Provenance for top-level provider reads (`let x = store.vars.v |>
    // alias.read`) and writes (`let saved = alias.write("out", x)`), attached
    // to the matching module-level binding in their own fields.
    let (provRead, provWrite) = readProvenance prog (providerAliases prog)
    [ for e in collectTypedBindings srcFuncs tp do
        let key = e.Scope + " " + e.EName
        match spans.TryGetValue key with
        | true, q when q.Count > 0 ->
            let (span, srcKind) = q.Dequeue()
            let (line, col, endLine, endCol) = clampSpan span
            let kind = srcKind |> Option.defaultValue e.EKind
            let block = docAt line
            // A parameter's doc is its `name: ...` line in the enclosing
            // function's block; function summaries drop those lines (they
            // travel on params[] instead), everything else gets the whole block.
            let doc =
                if e.EKind = "param" then paramDocIn block e.EName
                elif not e.EParams.IsEmpty && block <> "" then
                    let paramRes =
                        e.EParams
                        |> List.map (fun (n, _, _, _) ->
                            Regex(sprintf @"^[\s\-\*]*%s\s*[:\u2014-]" (Regex.Escape n)))
                    block.Split('\n')
                    |> Array.filter (fun l -> paramRes |> List.forall (fun re -> not (re.IsMatch l)))
                    |> String.concat "\n"
                    |> _.Trim()
                else block
            let ps =
                e.EParams
                |> List.map (fun (n, t, mr, dflt) ->
                    { PName = n; PType = t; PDoc = paramDocIn block n; PMinRank = mr; PDefault = dflt })
            let providerRead = if e.Scope = "" then Map.tryFind e.EName provRead else None
            let providerWrite = if e.Scope = "" then Map.tryFind e.EName provWrite else None
            yield { Name = e.EName; Kind = kind; Line = line; Col = col
                    EndLine = endLine; EndCol = endCol
                    TypeStr = e.ETypeStr; Doc = doc; Params = ps; Ret = e.ERet
                    Where = e.EWhere; DeducedComm = e.EDeducedComm
                    ProviderRead = providerRead
                    ProviderWrite = providerWrite
                    ConcreteType = None
                    // Params and function-body locals carry their function's
                    // scope; functions themselves carry a return type. What
                    // is left is exactly what an IR module binding can name.
                    IsTopLevelValue = (e.Scope = "" && e.ERet.IsNone) }
        | _ -> () ]

// Lambda-kernel deduction sites (IdeDeductions side-channel, span-keyed).

/// One entry per distinct lambda-kernel span. A let-bound lambda applied at
/// several sites records once per instantiation with the same definition
/// span (first wins -- the deduction is per-kernel, not per-site).
let private collectKernels () : KernelIdeInfo list =
    let seen = HashSet<int * int * int * int>()
    [ for k in Blade.TypeCheckIde.IdeDeductions.getKernels () do
        let (line, col, endLine, endCol) = clampSpan k.KSpan
        if seen.Add((line, col, endLine, endCol)) then
            yield { KLine = line; KCol = col; KEndLine = endLine; KEndCol = endCol
                    KParamNames = k.KParams
                    KDeduced = parityClauses k.KParams k.KParities
                    KDeclaredW = k.KDeclared
                    KMinRanks = k.KRanks } ]

// References (definition + use spans)

let private refSpanOf (s: Span) : RefSpan =
    let (line, col, endLine, endCol) = clampSpan s
    { RLine = line; RCol = col; REndLine = endLine; REndCol = endCol }

/// The identifier actually WRITTEN at a name span, falling back to the typed
/// name. Interface-impl methods reach the typed AST mangled (`scale` inside
/// `impl Scalable for Box` becomes `Box__scale`), and a rename driven off that
/// name would paste the mangling into the source. The span always knows better;
/// anything that isn't a plain identifier is refused so a drifted span can
/// never invent a name.
let private nameAt (lines: string[]) (sp: Span) (fallback: string) : string =
    if sp.StartLine < 1 || sp.StartLine > lines.Length || sp.EndLine <> sp.StartLine then fallback
    else
        let line = lines.[sp.StartLine - 1]
        let a = sp.StartCol - 1
        let b = min line.Length (sp.EndCol - 1)
        if a < 0 || b <= a then fallback
        else
            let text = line.Substring(a, b - a)
            let isIdent =
                text.Length > 0
                && (System.Char.IsLetter text.[0] || text.[0] = '_')
                && text |> Seq.forall (fun c -> System.Char.IsLetterOrDigit c || c = '_')
            if isIdent then text else fallback

/// Every binder a reference entry can point at, as (IRId, name, kind, name
/// token). Three sources, each exact for what it covers:
///
///   * functions and parameters -- TypedFunctionDecl/TypedParam now carry the
///     parser's `NameSpan`, so no join is needed at all (this also covers
///     LAMBDA parameters, wherever in the tree the lambda sits);
///   * let-style binders (module lets/statics, function-body lets, their
///     destructured leaves, for-in loop variables) -- the untyped side owns
///     the name spans and the typed side owns the IRIds, joined by (scope,
///     name) queue in declaration order, which is how `bindings[]` has always
///     paired the two walks. Both walks visit the same nodes in the same
///     order, so two shadowing `x`s dequeue their own spans positionally.
///
/// Anything the walks disagree about simply produces no binder, and its uses
/// are dropped downstream -- never a def pointing at the wrong identifier.
let private collectBinders (prog: Ast.Program) (tp: TypedProgram) (lines: string[])
                           : (IRId * string * string * Span) list =
    let acc = ResizeArray<IRId * string * string * Span>()
    let letSpans = Dictionary<string, Queue<Span>>()
    let enqueue (scope: string) (name: string) (span: Span) =
        let key = scope + " " + name
        match letSpans.TryGetValue key with
        | true, q -> q.Enqueue span
        | _ ->
            let q = Queue<Span>()
            q.Enqueue span
            letSpans.[key] <- q
    // --- untyped side: name spans, in declaration order
    let rec srcStmts (scope: string) (stmts: Stmt list) (declSpan: Span) =
        for s in stmts do
            let (span, inner) =
                match s with
                | StmtSpanned (inner, sp) -> (sp, unwrapStmt inner)
                | other -> (declSpan, unwrapStmt other)
            match inner with
            | StmtLet b -> for (n, sp) in patternNameSpans b.Pattern do enqueue scope n sp
            | StmtForIn (v, _, body) ->
                enqueue scope v (defaultArg (locateName lines span v) noSpan)
                srcStmts scope body span
            | _ -> ()
    let srcFunc (f: FunctionDecl) (declSpan: Span) =
        match f.Body.Kind with
        | ExprKind.ExprBlock (stmts, _) -> srcStmts f.Name stmts declSpan
        | _ -> ()
    for m in prog.Modules do
        for ld in m.Decls do
            match ld.Value with
            | DeclLet b | DeclStatic b ->
                for (n, sp) in patternNameSpans b.Pattern do enqueue "" n sp
            | DeclFunction f -> srcFunc f ld.Span
            | DeclImpl impl -> for f in impl.Methods do srcFunc f ld.Span
            | _ -> ()
    // --- typed side: IRIds, in the same order, consuming the queues
    let take (scope: string) (name: string) (kind: string) (id: IRId) =
        match letSpans.TryGetValue (scope + " " + name) with
        | true, q when q.Count > 0 ->
            let sp = q.Dequeue()
            if sp.StartLine > 0 then acc.Add((id, name, kind, sp))
        | _ -> ()
    let rec tyStmts (scope: string) (stmts: TypedStmt list) =
        for s in stmts do
            match s with
            | TStmtLet b ->
                take scope b.Name "local" b.VarId
                for (n, id, _) in b.SubBindings do take scope n "local" id
            | TStmtForIn (v, id, _, _, body) ->
                take scope v "local" id
                tyStmts scope body
            | _ -> ()
    let tyFunc (f: TypedFunctionDecl) =
        if f.NameSpan.StartLine > 0 then
            acc.Add((f.FuncId, nameAt lines f.NameSpan f.Name, "function", f.NameSpan))
        for p in f.Params do
            if p.NameSpan.StartLine > 0 then acc.Add((p.VarId, p.Name, "param", p.NameSpan))
        match f.Body.Kind with
        | TExprBlock (stmts, _) -> tyStmts f.Name stmts
        | _ -> ()
    // Lambda params come from a full-tree sweep rather than the decl walk: a
    // kernel can sit anywhere in an expression. Elaborator-synthesized lambdas
    // carry noSpan params and drop out here.
    let rec lambdaParams (te: TypedExpr) =
        (match te.Kind with
         | TExprLambda info ->
             for p in info.Params do
                 if p.NameSpan.StartLine > 0 then acc.Add((p.VarId, p.Name, "param", p.NameSpan))
         | _ -> ())
        for c in Blade.TypeCheck.typedExprChildren te do lambdaParams c
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b ->
                take "" b.Name "value" b.VarId
                for (n, id, _) in b.SubBindings do take "" n "local" id
            | TDeclFunction f -> tyFunc f
            | TDeclImpl impl -> for f in impl.Methods do tyFunc f
            | _ -> ()
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b -> lambdaParams b.Value
            | TDeclFunction f -> lambdaParams f.Body
            | TDeclImpl impl -> for f in impl.Methods do lambdaParams f.Body
            | _ -> ()
    List.ofSeq acc

/// Every variable USE with a live source span, bucketed by the IRId it
/// resolved to. `TExprVar` carries both the identifier's exact span and the
/// binder's id, so this is a plain walk -- no name matching anywhere.
let private collectVarUses (prog: Ast.Program) (tp: TypedProgram) : Dictionary<IRId, ResizeArray<Span>> =
    // The eight elaborators build their nodes through `Ast.syn`, which stamps
    // the ambient `synthSpan` -- the span of the WHOLE declaration being
    // expanded -- onto every one of them. So a synthesized variable reference
    // surfaces as a "use" covering an entire decl. No real identifier can do
    // that, so a span matching a declaration's exactly is a phantom.
    let declSpans = HashSet<int * int * int * int>()
    for m in prog.Modules do
        for ld in m.Decls do declSpans.Add(clampSpan ld.Span) |> ignore
    let acc = Dictionary<IRId, ResizeArray<Span>>()
    // An eta-expanded wrapper and the node it wraps can report the same
    // reference twice; one entry per (binder, position).
    let seen = HashSet<IRId * (int * int * int * int)>()
    let rec walk (te: TypedExpr) =
        (match te.Kind with
         | TExprVar (name, varId, _) when te.Span.StartLine > 0 && not (name.StartsWith "__") ->
             let key = clampSpan te.Span
             if not (declSpans.Contains key) && seen.Add((varId, key)) then
                 match acc.TryGetValue varId with
                 | true, xs -> xs.Add te.Span
                 | _ ->
                     let xs = ResizeArray<Span>()
                     xs.Add te.Span
                     acc.[varId] <- xs
         | _ -> ())
        for c in Blade.TypeCheck.typedExprChildren te do walk c
    for m in tp.Modules do
        for d in m.Decls do
            match d with
            | TDeclLet b | TDeclStatic b -> walk b.Value
            | TDeclFunction f -> walk f.Body
            | TDeclImpl impl -> for f in impl.Methods do walk f.Body
            | _ -> ()
    acc

/// Join binders to their uses. A use bucket whose IRId matches no binder is
/// DROPPED rather than emitted with `def: null`: every such bucket is either a
/// variant tag (the checker mints a FRESH VarId at each tag use, so they arrive
/// as a crowd of singletons with nothing to rename), a match-case pattern
/// binding (no name span reaches this far yet), or a compiler-generated binder.
/// None of them can answer go-to-definition, and a rename over them would edit
/// text the compiler never agreed was one symbol.
let private collectReferences (prog: Ast.Program) (tp: TypedProgram) (lines: string[]) : RefInfo list =
    let uses = collectVarUses prog tp
    // A use is kept only when the SOURCE TEXT at its span IS the binder's name.
    // Two things make that check earn its keep: the parser gives a
    // parenthesized reference `(A)` the parens' span, which is narrowed here to
    // the identifier; and the checker can hand two binders the SAME IRId (the
    // recursive-array elaborator does, in big files), which would otherwise
    // pull a stranger's references into an entry. Rename rewrites these spans
    // literally, so a span that does not spell the name is worse than absent.
    let verifiedUse (name: string) (sp: Span) : Span option =
        if nameAt lines sp "" = name then Some sp else locateName lines sp name
    let bound =
        collectBinders prog tp lines
        |> List.filter (fun (_, name, _, _) -> not (name.StartsWith "__"))
        |> List.map (fun (id, name, kind, span) ->
            let def = refSpanOf span
            let us =
                match uses.TryGetValue id with
                | true, xs ->
                    xs
                    |> Seq.choose (verifiedUse name)
                    |> Seq.map refSpanOf
                    // The checker synthesizes a subject `ExprVar` at the
                    // pattern's own span for struct/bound guards; a definition
                    // is not a use of itself.
                    |> Seq.filter (fun u -> u <> def)
                    |> Seq.distinct
                    |> List.ofSeq
                | _ -> []
            { RName = name; RKind = kind; RDef = Some def; RUses = us })
        // `ad.grad(f)` CLONES f's declaration, parameters and all, so one
        // source parameter can end up owning two IRIds. The same name at the
        // same source position is one symbol as far as an editor is concerned:
        // merge the use lists rather than offer the user a choice of two.
        |> List.groupBy (fun r -> (r.RName, r.RKind, r.RDef))
        |> List.map (fun (_, group) ->
            { List.head group with
                RUses = group |> List.collect _.RUses |> List.distinct })
    // `type X = ...` names: no IRId exists and no TExprVar ever names one, so
    // these are standalone def-only entries -- still worth emitting, since a
    // type alias is renameable and belongs in the outline.
    let types =
        [ for m in prog.Modules do
            for ld in m.Decls do
                match ld.Value with
                | DeclType td ->
                    for n in typeDeclNames td do
                        if not (n.StartsWith "__") then
                            match locateName lines ld.Span n with
                            | Some sp -> yield { RName = n; RKind = "type"; RDef = Some (refSpanOf sp); RUses = [] }
                            | None -> ()
                | _ -> () ]
    (bound @ types)
    // An entry with neither a definition nor a use says nothing.
    |> List.filter (fun r -> r.RDef.IsSome || not r.RUses.IsEmpty)
    |> List.sortBy (fun r ->
        match r.RDef with
        | Some d -> (d.RLine, d.RCol, r.RName)
        | None -> (System.Int32.MaxValue, 0, r.RName))

// Entry points

/// The FULL-tier seam: parse+typecheck cannot reach monomorphization from
/// here, because this file compiles before Lowering.fs. So the serve loop
/// (IdeServe.fs, which compiles after it) hands the pass in as a function.
/// Ok = top-level value-binding name -> concrete type, ALREADY rendered by
/// this module's printers so spellings match the rest of the payload;
/// Error = (BL code, message) pairs for lowering-stage failures, which are
/// real errors a `blade run` of the same source would hit.
type FullTierUpgrade =
    Ast.Program -> TypedProgram -> IRBuilder -> Result<Map<string, string>, (string * string) list>

/// Attach `concreteType` where monomorphization beat the typed AST. Only
/// top-level value bindings are candidates (an IR module binding names
/// nothing else), and only a genuinely different spelling is emitted -- the
/// client treats the field's presence as "prefer this".
let private applyConcrete (concrete: Map<string, string>) (bindings: BindingInfo list) =
    if concrete.IsEmpty then bindings
    else
        bindings
        |> List.map (fun b ->
            if not b.IsTopLevelValue then b
            else
                match Map.tryFind b.Name concrete with
                | Some t when t <> b.TypeStr -> { b with ConcreteType = Some t }
                | _ -> b)

/// The whole `ide check` pipeline over IN-MEMORY source, returning the JSON
/// payload plus the exit code instead of printing: `ide serve` checks an
/// unsaved buffer, so the source can never be read from disk.
///
/// `env` carries the serve protocol's id/tier (absent for the one-shot
/// command). `upgrade` is Some only for the full tier, and runs only after a
/// CLEAN typecheck -- a file with type errors has nothing to monomorphize.
let ideCheckSourceWith (env: Envelope) (upgrade: FullTierUpgrade option)
                       (filePath: string) (source: string) : string * int =
    let mutable exitCode = 0
    let diags = ResizeArray<Diag>()
    let mutable bindings = []
    let mutable providers = []
    let mutable deduced = []
    let mutable calls = []
    let mutable kernels = []
    let mutable references = []
    match Blade.Parser.parseProgramWithFile (Some filePath) source with
    | Error e ->
        let line = max 1 e.Line
        let col = max 1 e.Col
        let endLine = max line e.EndLine
        let endCol = if e.EndCol >= 1 then e.EndCol else col
        diags.Add { Severity = "error"; Line = line; Col = col; EndLine = endLine; EndCol = endCol
                    Message = e.Message; Code = e.Code }
        exitCode <- 1
    | Ok program ->
        // Provider checkout desugar, on the same terms as the compile lane:
        // `repo.checkout(ref)` becomes the load shape every payload collector
        // below already recognizes (collectProviderStores, readProvenance), so
        // a checkout binding gets its store hover instead of nothing.
        // `typeCheck` re-runs the pass internally (it is idempotent); a
        // wrong-shaped checkout is left alone here and surfaces as the
        // checker's diagnostic, which is where the editor wants it anyway.
        let program = Blade.ProviderDesugar.desugarOrIdentity program
        // File-based imports (`import units.SI`) are resolved here for the
        // same reason `blade check` resolves them: without it the editor
        // squiggles every `Float<newton>` in a file that compiles fine.
        //
        // Only the program handed to the CHECKER grows. Every payload
        // collector below still receives the entry module alone, because each
        // one joins spans against `source` -- this buffer -- and a member
        // file's line 12 is not this file's line 12. The typed program is
        // sliced the same way (checkProgram preserves module order, so the
        // entry is last); the FULL-tier upgrade gets the whole thing, since
        // monomorphization has to see the modules it lowers.
        //
        // With nothing to resolve, `depModules` is empty and every value below
        // is the one this function computed before the module layer existed.
        let resolution =
            match program.Modules with
            | [ m ] -> Blade.ModuleResolve.resolveParsedEntry filePath source m
            | _ -> Blade.ModuleResolve.resolveEntry filePath source
        let depModules =
            match resolution.Errors, resolution.Files with
            | [], (_ :: _ :: _ as files) ->
                let deps = files |> List.take (files.Length - 1)
                match Blade.ModuleResolve.parseResolvedFiles deps with
                | Ok p -> p.Modules
                | Error _ -> []          // reported by the member's own check
            | _ -> []
        for d in resolution.Errors do
            // A resolution failure is spanned in the file that WROTE the bad
            // import. When that is this buffer the span is usable as-is;
            // when it is a member file it is not, so it lands at 1:1 with the
            // file named in the message.
            let sameFile = (d.Span.File = Some filePath) || d.Span.File.IsNone
            let (line, col, endLine, endCol) =
                if sameFile then clampSpan d.Span else (1, 1, 1, 1)
            let message =
                if sameFile then d.Message
                else $"""{d.Message} (in {(defaultArg d.Span.File "?")})"""
            diags.Add { Severity = "error"; Line = line; Col = col
                        EndLine = endLine; EndCol = endCol
                        Message = message; Code = d.Code }
            exitCode <- 1
        let checkedProgram =
            if List.isEmpty depModules then program
            else { program with Modules = depModules @ program.Modules }
        /// The entry module's slice of a typed program checked over the whole
        /// resolved set. Identity when nothing was resolved.
        let entryOnly (tp: TypedProgram) =
            if List.isEmpty depModules then tp
            else match tp.Modules with
                 | [] -> tp
                 | ms -> { tp with Modules = [ List.last ms ] }
        // Fresh provider-module registry (the load site records into it
        // during typeCheck; collectProviderStores reads it), and with it the
        // icechunk axis mint table -- both are per-compilation AsyncLocal
        // side-channels a previous check must not leak into this one.
        Blade.ProviderRegistry.IdeStores.reset ()
        Blade.IcechunkProvider.resetAxisMint ()
        // Suggestions and warnings are NOT error-exclusive: a file with
        // a type error earned every nudge before hitting it. All three
        // channels are AsyncLocal, so Error reads them like Ok does.
        let drainWarningChannels () =
            // Confirm-and-pin suggestions (stage 3/4) arrive twice: as
            // plain strings in typeCheck's Ok payload, and as structured
            // (message, kernel-span) pairs in PinSuggestions -- emit the
            // structured form, BL4010 at the kernel's real span.
            let pinSuggestions = Blade.TypeCheckIde.PinSuggestions.get ()
            // Stage-6a equivariance-certificate suggestions: BL4011 at
            // the DECL span, ghost-rendering `where ml.equiv(G)`.
            let certSuggestions = Blade.ML.Equiv.CertSuggestions.get ()
            // The galilean twin: BL4014, ghost-rendering `where
            // ml.galilean(u, ...)`. Separate channel (different
            // elaborator seam); re-joined here, equiv-first.
            let galCertSuggestions = Blade.ML.Galilean.GalCertSuggestions.get ()
            for (msg, span) in pinSuggestions do
                let (line, col, endLine, endCol) = clampSpan span
                diags.Add { Severity = "warning"; Line = line; Col = col
                            EndLine = endLine; EndCol = endCol
                            Message = msg; Code = "BL4010" }
            for (msg, span) in certSuggestions do
                let (line, col, endLine, endCol) = clampSpan span
                diags.Add { Severity = "warning"; Line = line; Col = col
                            EndLine = endLine; EndCol = endCol
                            Message = msg; Code = "BL4011" }
            for (msg, span) in galCertSuggestions do
                let (line, col, endLine, endCol) = clampSpan span
                diags.Add { Severity = "warning"; Line = line; Col = col
                            EndLine = endLine; EndCol = endCol
                            Message = msg; Code = "BL4014" }
            // The checker's own warnings, coded and spanned. BL4010 is
            // skipped: PinSuggestions above already emitted exactly
            // those (BL4011/BL4014 never ride this channel).
            for d in Blade.TypeCheckIde.WarningLog.get () |> List.distinct do
                if d.Code <> "BL4010" then
                    let (line, col, endLine, endCol) = clampSpan d.Span
                    diags.Add { Severity = "warning"; Line = line; Col = col
                                EndLine = endLine; EndCol = endCol
                                Message = d.Message; Code = d.Code }
        // What the checker PROVED, as distinct from what the source
        // declared. Two producers land in one flat array keyed by
        // `kind`: `TypeCheck.DeducedFacts` (rank/comm/anticomm/packComm)
        // is the checker's own deduction; `ML.Equiv.CertFacts`
        // (equiv/galilean) is the stage-6a certificate inference. The
        // certificate fields map by MEANING: DOwner is the certificate's
        // function, DName the group name, DLeft its dependency closure.
        let drainDeducedFacts () =
            deduced <-
                try
                    let checkerFacts =
                        Blade.TypeCheckIde.DeducedFacts.get ()
                        |> List.map (fun (f, span) ->
                            let empty = emptyDeduced span
                            match f with
                            | Blade.TypeEnv.DeducedRank (owner, param, index, rank) ->
                                { empty with DKind = "rank"; DOwner = owner; DName = param
                                             DIndex = index; DRank = rank }
                            | Blade.TypeEnv.DeducedPairSym (owner, left, right, index, isAnti) ->
                                { empty with DKind = (if isAnti then "anticomm" else "comm")
                                             DOwner = owner; DLeft = left; DRight = right
                                             DIndex = index }
                            | Blade.TypeEnv.DeducedPackComm (owner, pack) ->
                                { empty with DKind = "packComm"; DOwner = owner; DName = pack })
                    checkerFacts @ certFactRecords ()
                with _ -> []
        match Blade.TypeCheck.typeCheck checkedProgram with
        | Error errors ->
            for e in errors do
                // Same member-file rule as the resolution diagnostics above: a
                // span that belongs to another file cannot be squiggled here.
                let foreign =
                    not (List.isEmpty depModules)
                    && (match e.Span.File with Some f -> f <> filePath | None -> false)
                let (line, col, endLine, endCol) =
                    if foreign then (1, 1, 1, 1) else clampSpan e.Span
                let code = (Blade.TypeEnv.diagnosticOfCompileError e).Code
                let msg =
                    let baseMsg = Blade.TypeEnv.formatTypeError e.Error
                    let withCtx =
                        match e.Context with
                        | [] -> baseMsg
                        | ctx -> $"""{baseMsg} ({(String.concat "; " (List.rev ctx))})"""
                    if foreign then
                        $"""{withCtx} (in {(defaultArg e.Span.File "?")})"""
                    else withCtx
                diags.Add { Severity = "error"; Line = line; Col = col
                            EndLine = endLine; EndCol = endCol; Message = msg; Code = code }
            exitCode <- 1
            drainWarningChannels ()
            drainDeducedFacts ()
            // Errors don't have to mean zero hovers: if the checker ran
            // and produced a PARTIAL typed program, surface bindings for
            // the parts that DID check, so errors still get tooltips.
            match Blade.TypeCheck.IdePartial.get () with
            | Some (fullTyped, _) ->
                let typedProg = entryOnly fullTyped
                let sourceLines = source.Replace("\r\n", "\n").Split('\n')
                bindings <- (try joinBindings program typedProg sourceLines with _ -> [])
                providers <- (try collectProviderStores program with _ -> [])
                calls <- (try collectCalls typedProg @ collectFormerCalls program typedProg with _ -> [])
                kernels <- (try collectKernels () with _ -> [])
                references <- (try collectReferences program typedProg sourceLines with _ -> [])
            | None -> ()
        | Ok (fullTyped, builder, _) ->
            drainWarningChannels ()
            drainDeducedFacts ()
            let typedProg = entryOnly fullTyped
            let sourceLines = source.Replace("\r\n", "\n").Split('\n')
            bindings <- joinBindings program typedProg sourceLines
            // Guarded so provider structure can never break the JSON output.
            providers <- (try collectProviderStores program with _ -> [])
            calls <- (try collectCalls typedProg @ collectFormerCalls program typedProg with _ -> [])
            kernels <- (try collectKernels () with _ -> [])
            references <- (try collectReferences program typedProg sourceLines with _ -> [])
            // FULL tier, last: everything above is read off the typed AST, so
            // running monomorphization only after they're collected keeps the
            // fast payload identical whether or not the upgrade runs (or throws).
            match upgrade with
            | None -> ()
            | Some up ->
                let outcome =
                    // Monomorphization lowers, so it needs the WHOLE resolved
                    // program -- not the entry slice the payload collectors got.
                    try up checkedProgram fullTyped builder
                    with ex -> Error [("BL9001", ex.Message)]
                match outcome with
                | Ok concrete -> bindings <- applyConcrete concrete bindings
                | Error failures ->
                    // A file that typechecked but will not lower: these are
                    // errors `blade run` would report, so they belong in the
                    // diagnostics the editor squiggles. No span survives the
                    // IR stages, so they land at 1:1.
                    for (code, msg) in failures do
                        diags.Add { Severity = "error"; Line = 1; Col = 1; EndLine = 1; EndCol = 1
                                    Message = msg; Code = code }
                    exitCode <- 1
    (renderJson env (List.ofSeq diags) bindings providers deduced calls kernels references, exitCode)

/// Fast tier over in-memory source, no envelope: the plain `ide check` payload.
let ideCheckSource (filePath: string) (source: string) : string * int =
    ideCheckSourceWith noEnvelope None filePath source

/// `blade ide check --json <file>`: JSON diagnostics + binding types on
/// stdout. Exit 0 = clean, 1 = errors (the JSON is emitted either way).
let ideCheck (filePath: string) : int =
    let (json, exitCode) =
        if not (File.Exists filePath) then
            let missing =
                { Severity = "error"; Line = 1; Col = 1; EndLine = 1; EndCol = 1
                  Message = $"File not found: {filePath}"; Code = "" }
            (renderJson noEnvelope [missing] [] [] [] [] [] [], 1)
        else
            ideCheckSource filePath (File.ReadAllText filePath)
    printfn "%s" json
    exitCode

// Language surface (`blade ide surface`)
//
// One JSON line naming everything this compiler recognizes: the keyword
// vocabulary with its DU token names, the multi-character operators, the math
// intrinsics, the static-evaluator builtins, the scalar type bases, the
// builtin call names, and the BLxxxx registry with each code's phase band.
// Generated into protocol/surface.json (checked in) so the shared IDE-protocol
// package and its consumers stop hand-copying these lists out of the compiler
// -- drift between such mirrors has already shipped bugs (a builtin with a
// hover but no highlighting).
//
// The dump is a pure function of the binary plus the version string the CLI
// owns: no program is read, no file is touched. Field order is fixed and every
// array is order-stable, so diffing two dumps reads as a changelog.
//
// Deliberately NOT part of renderJson's payload: that one is pinned
// byte-for-byte by the one-shot `ide check --json` test, and the surface has a
// different lifetime (it changes when the compiler changes, not when the
// program does).

/// The `phase` string a diagnostics entry carries: the band `phaseOfCode`
/// derives, rendered for clients that have no F# union to match on.
let private phaseName (p: Blade.Diagnostics.Phase) : string =
    match p with
    | Blade.Diagnostics.PhLex -> "lex"
    | Blade.Diagnostics.PhParse -> "parse"
    | Blade.Diagnostics.PhResolve -> "resolve"
    | Blade.Diagnostics.PhTypes -> "types"
    | Blade.Diagnostics.PhConstraints -> "constraints"
    | Blade.Diagnostics.PhElaborate stage -> "elaborate:" + stage
    | Blade.Diagnostics.PhIRValidate -> "ir"
    | Blade.Diagnostics.PhBackend -> "backend"
    | Blade.Diagnostics.PhRuntime -> "runtime"
    | Blade.Diagnostics.PhInternal -> "internal"

/// `"<name>":["a","b"]` -- the one array shape this dump uses for plain names.
let private appendNameArray (sb: StringBuilder) (name: string) (items: string seq) =
    sb.Append('"').Append(name).Append("\":[") |> ignore
    items
    |> Seq.iteri (fun i s ->
        if i > 0 then sb.Append ',' |> ignore
        sb.Append('"').Append(jsonEscape s).Append('"') |> ignore)
    sb.Append ']' |> ignore

/// The surface line. `id` leads when present -- the serve lane's correlation
/// field, in the position renderJson's envelope puts it -- and is omitted
/// entirely by the one-shot verb. `compilerVersion` arrives as a PARAMETER for
/// `IdeServe.serve`'s reason: Ide.fs must not grow a dependency on Cli.fs.
let renderSurfaceWith (id: int option) (compilerVersion: string) : string =
    // `knownBuiltinNames ()` unions two registries that are filled LAZILY on
    // the check path: StructIdxSpec.install (run by typeCheck) and
    // ML.Statics.install (run by MLElaborate.expand). A dump must not depend on
    // whether this process happened to check a file first -- `blade test
    // surface` compares a live render against the snapshot a ONE-SHOT dump
    // produced, and in the full suite the live render runs after hundreds of
    // corpus checks -- so force both here. Both are idempotent.
    // ProviderStatics.install is deliberately NOT called: it registers a
    // compile-time data READER, not a builtin name.
    Blade.StructIdxSpec.install ()
    Blade.ML.Statics.install ()
    let sb = StringBuilder(16384)
    sb.Append '{' |> ignore
    (match id with
     | Some i -> sb.Append("\"id\":").Append(i).Append(',') |> ignore
     | None -> ())
    sb.Append("\"version\":1,\"compilerVersion\":\"").Append(jsonEscape compilerVersion)
      .Append("\",") |> ignore
    // Keywords in DECLARATION order, each with the DU case name a client can
    // key on. `true`/`True` are two entries sharing one token, deliberately:
    // the surface reports spellings, not tokens.
    sb.Append "\"keywords\":[" |> ignore
    Blade.Lexer.keywordEntries
    |> List.iteri (fun i (word, kw) ->
        if i > 0 then sb.Append ',' |> ignore
        sb.Append("{\"word\":\"").Append(jsonEscape word)
          .Append("\",\"token\":\"").Append(sprintf "%A" kw).Append("\"}") |> ignore)
    sb.Append "]," |> ignore
    // Declaration order, not the length-sorted `operators`: that ordering
    // exists for maximal munch and would read as churn in a snapshot diff.
    appendNameArray sb "operators" Blade.Lexer.operatorEntries
    sb.Append ",\"mathIntrinsics\":{" |> ignore
    appendNameArray sb "unary" Blade.Grad.mathIntrinsics
    sb.Append ',' |> ignore
    appendNameArray sb "binary" Blade.Grad.binaryMathIntrinsics
    sb.Append ',' |> ignore
    appendNameArray sb "ternary" Blade.Grad.ternaryMathIntrinsics
    sb.Append ',' |> ignore
    appendNameArray sb "complex" Blade.Grad.complexMathIntrinsics
    sb.Append "}," |> ignore
    // Sorted (the sets iterate ascending). Includes the internal `__ml_stat_*`
    // sizing names the ML layer registers: this is what the static evaluator
    // will actually accept, and the `__` prefix is the repo's own marker for
    // "internal, not API" -- clients filter it if they want the user surface.
    appendNameArray sb "builtins" (Blade.StaticEval.knownBuiltinNames ())
    sb.Append ',' |> ignore
    appendNameArray sb "scalarTypes" Blade.TypeCheck.builtinScalarNames
    sb.Append ',' |> ignore
    appendNameArray sb "builtinCalls" builtinCallNames
    sb.Append ",\"diagnostics\":[" |> ignore
    Blade.Diagnostics.Codes.registryEntries
    |> List.iteri (fun i (code, title) ->
        if i > 0 then sb.Append ',' |> ignore
        sb.Append("{\"code\":\"").Append(jsonEscape code)
          .Append("\",\"title\":\"").Append(jsonEscape title)
          .Append("\",\"phase\":\"").Append(phaseName (Blade.Diagnostics.Codes.phaseOfCode code))
          .Append("\"}") |> ignore)
    sb.Append "]}" |> ignore
    sb.ToString()

/// The one-shot form: no envelope, exactly what `blade ide surface` prints and
/// what protocol/surface.json is generated from.
let renderSurface (compilerVersion: string) : string = renderSurfaceWith None compilerVersion
