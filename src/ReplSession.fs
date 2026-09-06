// ReplSession.fs: the REPL's eval-once engine, lifted out of `Cli.replLoop` so
// two front ends can drive the SAME session semantics -- the interactive REPL
// (prompts, typed echoes, `[snippet not kept]` on stderr) and `ide serve`'s
// notebook `eval` command (structured results over NDJSON).
//
// A session is a list of raw source snippets plus the top-level name each one
// defines. Every submission builds a CANDIDATE list: a declaration REPLACES
// the earlier definition of the same name in place (so later snippets see the
// rebind), a reassignment and a bare expression each append a hidden wrapper.
// The candidate is written to a temp `session.blade`, lowered ONCE
// (Interp.Repl.lowerSessionDiag) and run under the tree-walking interpreter,
// falling back to a g++ compile+run only where the interpreter falls short.
// A candidate the front end REJECTS is discarded and the session is untouched;
// the session is only replaced once its candidate ran to exit 0. That is the
// whole of "snippet not kept".
//
// The engine NEVER prints. Rejections come back as coded, spanned Diagnostics
// beside the SourceMap that renders them; runs come back as the raw streams
// plus the matched echo LINE, so every caller-visible string -- the prompts,
// the `[snippet not kept]` marker, the type-annotated echo -- stays in the
// front end that owns it. The one exception is a caller-supplied `notice`
// sink, which exists so the interactive REPL can still print "falling back to
// compiled evaluation" BEFORE the multi-second g++ build rather than after it.
//
// Compiles after Interp/Repl.fs (its lowering seam) and before IdeServe.fs
// (its second front end). Cli.fs, far later, installs the g++ lane by hand:
// `compileToExe` sits on top of Build.fs and cannot be reached from here.
module Blade.ReplSession

open System
open System.IO
open System.Text.RegularExpressions

// REPL display: type-annotated echoes. The compiled session prints raw `name
// = value` lines; the REPL joins those with an in-process parse+typecheck of
// the SAME source to display types:
//   - primitives inline:                  a = Int64: 5
//   - other types (arrays, tuples, functions) on the next line, tabbed:
//         v = [1, 2, 3]
//             Array<Int64, Idx<3>>
//   - functions echo their signature; abstract (type-variable) positions
//     render with source names (`T`, `T^2`), inference-bound positions render
//     the concrete type substituted in.
module ReplTypes =
    open Blade.Ast
    open Blade.Types
    open Blade.IR
    open Blade.IRPrint
    open Blade.TypedAst

    /// What the REPL knows about one top-level name.
    type Info =
        | RVal of IRType
        | RFunc of signature: string

    /// Render a function signature: `(Int64, T) -> T`. Concrete positions
    /// print concretely; abstract positions print their source type-variable
    /// names (fresh letters for inference-invented ones); recovery/naming
    /// live in Blade.Ide, shared with `ide check`'s hover types.
    let funcSig (src: FunctionDecl option) (tf: TypedFunctionDecl) : string =
        let seed =
            match src with
            | Some f when f.Params.Length = tf.Params.Length ->
                [ for (p, tp) in List.zip f.Params tf.Params do
                    match p.Type with
                    | Some ann -> yield! Blade.Ide.collectVarNames ann tp.Type
                    | None -> ()
                  match f.ReturnType with
                  | Some ann -> yield! Blade.Ide.collectVarNames ann tf.ReturnType
                  | None -> () ]
            | _ -> []
        let pp = Blade.Ide.abstractRenderer seed
        let ps = tf.Params |> List.map (fun p -> pp p.Type)
        $"""({(String.concat ", " ps)}) -> {(pp tf.ReturnType)}"""

    /// Build the top-level name -> display info map from an ALREADY-lowered
    /// session (the same front-end pass the interpreter runs, so it never
    /// lowers twice). Value bindings prefer the LOWERED types: HM calls
    /// monomorphize during lowering, so the typed AST can still carry T?n
    /// inference vars where the IR is concrete.
    let sessionInfoOf (lowered: Blade.Interp.Repl.LoweredSession) : Map<string, Info> =
        let prog = lowered.Prog
        let tp = lowered.Typed
        let srcFuncs =
            [ for m in prog.Modules do
                for ld in m.Decls do
                    match ld.Value with
                    | DeclFunction f -> yield (f.Name, f)
                    | _ -> () ]
            |> Map.ofList
        let irTypes =
            Map.ofList
                [ for m in lowered.Ir.Modules do
                    for b in m.Bindings do
                        yield (b.Name, b.Type) ]
        let valTy (name: string) (fallback: IRType) =
            match Map.tryFind name irTypes with
            | Some t -> t
            | None -> fallback
        let mutable acc = Map.empty
        for m in tp.Modules do
            for d in m.Decls do
                match d with
                | TDeclLet b | TDeclStatic b ->
                    acc <- Map.add b.Name (RVal (valTy b.Name b.Type)) acc
                    for (n, _, t) in b.SubBindings do
                        acc <- Map.add n (RVal (valTy n t)) acc
                | TDeclFunction f ->
                    acc <- Map.add f.Name
                               (RFunc (funcSig (Map.tryFind f.Name srcFuncs) f)) acc
                | _ -> ()
        acc

    /// Parse + typecheck + lower session source (one pass) and return
    /// top-level name -> display info. Failures yield an empty map (values
    /// still print, just unannotated). Used for the bare-identifier "is this
    /// a session function?" probe; the candidate path reuses the
    /// interpreter's own LoweredSession via sessionInfoOf instead, so it never lowers twice.
    let sessionInfo (source: string) : Map<string, Info> =
        try
            match Blade.Interp.Repl.lowerSession None false source with
            | Error _ -> Map.empty
            | Ok lowered -> sessionInfoOf lowered
        with _ -> Map.empty

    /// Primitive = annotate inline ("Int64: 5"); everything else goes on the
    /// next line, tabbed.
    let rec isPrimitive (t: IRType) : bool =
        match t with
        | IRTScalar _ | IRTNat _ -> true
        | IRTIdxTagged (inner, _) | IRTUnitAnnotated (inner, _) -> isPrimitive inner
        | _ -> false

    let private eqLineRe = Regex(@"^([A-Za-z_][A-Za-z0-9_]*) = (.*)$", RegexOptions.Compiled)

    /// Split a bracketed body at the commas sitting at nesting depth zero, so a
    /// row (`[1, 2]`) or a complex cell (`(1, 0)`) stays ONE part. Depth counts
    /// `[` and `(` alike; commas inside quotes are literal.
    let private splitTopLevelCommas (inner: string) : string list =
        let parts = ResizeArray<string>()
        let cur = System.Text.StringBuilder()
        let mutable depth = 0
        let mutable inQuotes = false
        for c in inner do
            if c = '"' then
                inQuotes <- not inQuotes
                cur.Append(c) |> ignore
            elif inQuotes then cur.Append(c) |> ignore
            else
                match c with
                | '[' | '(' -> depth <- depth + 1; cur.Append(c) |> ignore
                | ']' | ')' -> depth <- depth - 1; cur.Append(c) |> ignore
                | ',' when depth = 0 ->
                    parts.Add(cur.ToString())
                    cur.Clear() |> ignore
                | _ -> cur.Append(c) |> ignore
        if cur.Length > 0 then parts.Add(cur.ToString())
        parts |> List.ofSeq

    /// How many entries the REPL shows per bracket level before eliding.
    let private elideAfter = 5

    /// The REPL's display cap: at EVERY bracket level, show the first
    /// `elideAfter` entries and truncate the rest to `...`. DISPLAY ONLY --
    /// the program's own stdout is untouched (`blade run` prints every cell,
    /// which is what the corpus pins read). Text-level on purpose: works for
    /// any printed shape without re-deriving the value. Public because the
    /// notebook lane caps its `bindings[]` values by the same rule the
    /// interactive echo does -- one display policy, not two.
    let rec elideValue (s: string) : string =
        let t = s.Trim()
        if not (t.Length >= 2 && t.StartsWith "[" && t.EndsWith "]") then t
        else
            let inner = t.Substring(1, t.Length - 2).Trim()
            if inner = "" then "[]"
            else
                let parts = splitTopLevelCommas inner
                let kept = parts |> List.truncate elideAfter |> List.map elideValue
                let shown = if parts.Length > elideAfter then kept @ [ "..." ] else kept
                "[" + String.concat ", " shown + "]"

    /// Bare-integer text -- the shape a whole-valued float prints as under the
    /// byte-pinned C++-parity formatter (`4.0` prints "4", `-0.0` prints "-0").
    let private bareIntRe = Regex(@"^-?[0-9]+$", RegexOptions.Compiled)

    /// Whether a type's printed numeric cells are FLOATING POINT, seen through
    /// unit/index-tag wrappers and into array element types. Complex counts:
    /// its "(re,im)" components are floats. Named (struct) elements do not --
    /// their printed fields can mix widths this text pass can't tell apart.
    let rec private printsAsFloat (t: IRType) : bool =
        match t with
        | IRTScalar (ETFloat64 | ETFloat32 | ETComplex128 | ETComplex64) -> true
        | IRTUnitAnnotated (inner, _) | IRTIdxTagged (inner, _) -> printsAsFloat inner
        | ArrayElem at -> printsAsFloat at.ElemType
        | _ -> false

    /// Give every bare-integer cell in a (possibly bracketed/elided) display
    /// string an F#-style ".0" suffix: `4` -> `4.0`, `[1, 2, 3.5]` ->
    /// `[1.0, 2.0, 3.5]`, `(1,0)` -> `(1.0,0.0)`. Non-integer tokens
    /// (`2.5`, `1e+20`, `inf`, `nan`, `...`) and unrecognized shapes pass
    /// through untouched. Bracket entries rejoin with ", " and paren
    /// components with "," -- exactly the separators the array and complex
    /// emitters use, so the reconstruction is textually faithful.
    let rec private addDecimalPoints (s: string) : string =
        let t = s.Trim()
        if bareIntRe.IsMatch t then t + ".0"
        elif t.Length >= 2 && t.StartsWith "[" && t.EndsWith "]" then
            let inner = t.Substring(1, t.Length - 2).Trim()
            if inner = "" then "[]"
            else "[" + (splitTopLevelCommas inner |> List.map addDecimalPoints |> String.concat ", ") + "]"
        elif t.Length >= 2 && t.StartsWith "(" && t.EndsWith ")" then
            let inner = t.Substring(1, t.Length - 2)
            "(" + (splitTopLevelCommas inner |> List.map addDecimalPoints |> String.concat ",") + ")"
        else t

    /// DISPLAY ONLY: F#-style numeric rendering for the REPL echo and the
    /// notebook `bindings[]`. The underlying run output is the byte-pinned
    /// C++-parity print, where a whole-valued float renders with no decimal
    /// point ("x = 4" for `let x = 4.0`) -- correct for the differential gate,
    /// misleading in a typed echo. A float-typed value re-marks its integer
    /// cells with ".0"; int-typed values are untouched.
    let displayValue (t: IRType) (v: string) : string =
        if printsAsFloat t then addDecimalPoints v else v

    /// Rewrite one raw output line for display. `transient` is the synthetic
    /// binding a bare REPL expression was wrapped in; its name is stripped so the value echoes alone.
    let annotate (info: Map<string, Info>) (transient: string option) (line: string) : string =
        let m = eqLineRe.Match line
        if not m.Success then line
        else
            let name = m.Groups.[1].Value
            let value = elideValue m.Groups.[2].Value
            let isTransient = (transient = Some name)
            match Map.tryFind name info with
            | Some (RVal t) ->
                let value = displayValue t value
                let tyStr = Blade.Ide.abstractRenderer [] t
                if isPrimitive t then
                    if isTransient then $"{tyStr}: {value}"
                    else $"{name} = {tyStr}: {value}"
                else
                    if isTransient then $"{value}\n\t{tyStr}"
                    else $"{name} = {value}\n\t{tyStr}"
            | Some (RFunc _) -> line
            | None -> if isTransient then value else line

// Submission classification. These regexes decide the SHAPE of a submission
// (declaration / reassignment / bare expression) and name the binding it
// echoes. They live here rather than in either front end because both drive
// the same three candidate builders below, and a classification that drifted
// between the REPL and the notebook would be two languages, not one.

/// Top-level name a snippet (re)defines, for rebind replacement.
///
/// `Unit` is capitalised because the KEYWORD is (see Lexer.keywords) -- it is
/// the one declaration keyword that is not lower-case, and both this pattern
/// and `declRe` have to spell it the way the lexer does.
///
/// `rec` is a `let` modifier like `mut` and `static`: without it here a
/// recursive array bound its name as the literal string "rec", which is not a
/// name any rebind can match and not a binding any run output carries -- the
/// notebook echoed `rec` with an empty type and value.
let private bindingNameRe =
    Regex(@"^\s*(?:let\s+(?:mut\s+|static\s+|rec\s+)?|static\s+function\s+|function\s+|type\s+|Unit\s+)([A-Za-z_][A-Za-z0-9_]*)")

/// A destructuring declaration: `let (a, b) = ...`, `let mut (x, _) = ...`,
/// nested parens allowed. Group 1 is the pattern text between the outer parens.
let private destructureNameRe =
    Regex(@"^\s*let\s+(?:mut\s+)?\(((?:[^()]|\([^()]*\))*)\)\s*=")

/// The first line that is neither blank nor a `//` comment: the line a
/// snippet's SHAPE is read from. Every textual probe in this module has to
/// look past a leading comment, because a declaration routinely sits under
/// one -- a `///` doc comment in a file, a prose banner in a notebook cell.
/// `classifyTarget` (below) is the same question asked for classification.
let firstSignificantLine (s: string) =
    s.Replace("\r\n", "\n").Split('\n')
    |> Array.tryFind (fun l ->
        let t = l.TrimStart()
        t <> "" && not (t.StartsWith "//"))
    |> Option.defaultValue ""

/// The name a snippet declares, read from its first significant line.
///
/// Reading it from the RAW snippet was a silent-wrong-answer bug: the regex is
/// anchored `^\s*`, and `\s` spans newlines, so a BLANK-led declaration matched
/// but a COMMENT-led one returned None. `spliceDeclaration` takes None to mean
/// "no name to supersede" and APPENDS -- so re-running a commented declaration
/// left the old binding standing and added the new one after any consumer that
/// had spliced in place, which then read the stale value. Nothing failed; the
/// answer was just wrong.
let bindingName (snippet: string) : string option =
    let line = firstSignificantLine snippet
    let m = bindingNameRe.Match line
    if m.Success then Some m.Groups.[1].Value
    else
        // A destructuring `let (a, b) = ...` declares its LEAVES; its rebind
        // key is the leaf list, `(a,b)`, so re-running the cell supersedes the
        // earlier one in place like any other declaration -- and the key is
        // spelled exactly as TypeCheck names the binding, so the memo
        // invalidation below (`topLevelBindingNames`) drops it. Without this the
        // regex above found no name, the splice APPENDED a second binding of
        // the same leaves, and the interpreter's name-keyed session memo then
        // adopted the FIRST tuple's cached value for the new one -- the rebind
        // was silently a no-op (measured: `let (a, b) = pair(7.0)` echoed the
        // pair(1.5) values). Wildcards are dropped from the key; a nested
        // tuple pattern flattens, which is also how TypeCheck names the
        // binding (`_(a,b)`, see checkDecl).
        let d = destructureNameRe.Match line
        if d.Success then
            let leaves =
                d.Groups.[1].Value.Split([| ','; '('; ')' |], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun x -> x.Trim())
                |> Array.filter (fun x -> x <> "" && x <> "_")
            if leaves.Length = 0 then None else Some ("_(" + String.concat "," leaves + ")")
        else None

/// The names a MULTI-LINE submission declares at top level.
///
/// `bindingName` is anchored `^\s*`, so it happily matches an INDENTED `let` --
/// which is right for a `:paste` block of top-level declarations and wrong for
/// a function body. Running it over every line of
/// `function f(x) = { let n = ...; let m = ... }` harvested `n` and `m` as if
/// the submission had bound them: they rode out to the REPL echo and the
/// notebook's `bindings[]` as scoped names the user never bound, each with an
/// empty type and value (nothing top-level carries that name to read one from),
/// and the LAST of them became the echo target instead of the function.
///
/// Only a line that STARTS at nesting depth 0 can declare a top-level name.
/// Depth counts (), [] and {} outside double-quoted strings and `//` comments --
/// the same best-effort textual stance as the rest of this block (a brace in a
/// comment can still fool it; the cost is a missed echo, never a wrong binding).
let topLevelBindingNames (source: string) : string list =
    let advance (depth: int) (line: string) : int =
        let mutable d = depth
        let mutable inQuotes = false
        let mutable i = 0
        while i < line.Length do
            let c = line.[i]
            if inQuotes then
                if c = '\\' then i <- i + 1
                elif c = '"' then inQuotes <- false
            elif c = '"' then inQuotes <- true
            elif c = '/' && i + 1 < line.Length && line.[i + 1] = '/' then i <- line.Length
            elif c = '(' || c = '[' || c = '{' then d <- d + 1
            elif c = ')' || c = ']' || c = '}' then d <- d - 1
            i <- i + 1
        d
    let acc = ResizeArray<string>()
    let mutable depth = 0
    for line in source.Replace("\r\n", "\n").Split('\n') do
        if depth <= 0 then
            match bindingName line with
            | Some n ->
                acc.Add n
                // A destructuring line binds its LEAVES too, each a memo key
                // of its own (the interpreter publishes `a` and `b` beside
                // the parent `_(a,b)`); a rebind must drop all three.
                if n.StartsWith "_(" then
                    for leaf in n.Substring(2, n.Length - 3).Split(',') do acc.Add leaf
            | None -> ()
        depth <- advance depth line
    List.ofSeq acc

/// The generated main prints a "<name> completed in Xs" timing line whose
/// value changes every run -- exclude it from the output diff.
let isTimingLine (l: string) =
    Regex.IsMatch(l, @"completed in [0-9.eE+~-]+m?s\s*$")

/// A snippet is a declaration iff it opens with a declaration keyword;
/// anything else is a bare expression to evaluate and echo.
///
/// The alternation is CASE-SENSITIVE and must list each keyword exactly as
/// Lexer.keywords spells it: `Unit` is capitalised, and while it was spelled
/// `unit` here it never matched, so a `Unit d = ...` cell fell through to the
/// bare-expression lane and was wrapped in `let __cellN = `.
let declRe =
    Regex(@"^\s*(let|static|function|type|struct|interface|impl|Unit|import|from|module)\b")

let identRe = Regex(@"^[A-Za-z_][A-Za-z0-9_]*$")

/// Drop `//`-to-end-of-line and `/* ... */` comments, keeping everything else
/// (including line structure) so the result still lines up statement for
/// statement. Quote-aware, so a `//` inside a string literal survives.
let private stripComments (s: string) : string =
    let sb = System.Text.StringBuilder()
    let mutable i = 0
    let mutable inQuotes = false
    let mutable inBlock = false
    while i < s.Length do
        let c = s.[i]
        if inBlock then
            if c = '*' && i + 1 < s.Length && s.[i + 1] = '/' then
                inBlock <- false
                i <- i + 1
            elif c = '\n' then sb.Append c |> ignore
        elif inQuotes then
            sb.Append c |> ignore
            if c = '\\' && i + 1 < s.Length then
                sb.Append s.[i + 1] |> ignore
                i <- i + 1
            elif c = '"' then inQuotes <- false
        elif c = '"' then
            inQuotes <- true
            sb.Append c |> ignore
        elif c = '/' && i + 1 < s.Length && s.[i + 1] = '/' then
            while i < s.Length && s.[i] <> '\n' do i <- i + 1
            if i < s.Length then sb.Append '\n' |> ignore
        elif c = '/' && i + 1 < s.Length && s.[i + 1] = '*' then
            inBlock <- true
            i <- i + 1
        else sb.Append c |> ignore
        i <- i + 1
    sb.ToString()

/// The single identifier a submission (or one statement of one) consists of,
/// once comments and whitespace are set aside.
///
/// It matters because a bare identifier naming a session FUNCTION is echoed
/// from the declaration -- its name and the checker's rendering of its
/// signature, `(T^1, T^1) -> T^1` -- while everything else is wrapped in a
/// transient binding and reported anonymously. Matching `identRe` against the
/// RAW text answered "no" for `covariance // : Array<...>`, which is exactly
/// how the notebook writes it (quickstart-1 section 10), so the same function
/// rendered two different ways depending on whether a comment trailed it: the
/// commented spelling fell through to the wrapper and reported the WRAPPER's
/// raw IR type under no name at all (`{"name":"", "type":"Arrow<T, T -> T>"}`).
let bareIdentifier (s: string) : string option =
    let t = (stripComments s).Trim()
    if identRe.IsMatch t then Some t else None

/// Prepend `prefix` to the first SIGNIFICANT line of `text` -- the same line
/// `classifyTarget` classified on -- and report which row took it.
///
/// The row matters twice: a hidden wrapper on a comment line does not parse
/// (`let it = // note` swallows the expression on the line below, BL1999), and
/// the column shift a diagnostic needs to undo applies to that row alone.
/// Blank or comment-only text has no significant line and takes the prefix on
/// row 0, which is what the callers' own "nothing to wrap" guards expect.
let wrapAtSignificantLine (prefix: string) (text: string) : string * int =
    let lines = text.Replace("\r\n", "\n").Split('\n')
    let row =
        lines
        |> Array.tryFindIndex (fun l ->
            let t = l.TrimStart()
            t <> "" && not (t.StartsWith "//"))
        |> Option.defaultValue 0
    let wrapped = Array.copy lines
    wrapped.[row] <- prefix + wrapped.[row]
    (String.concat "\n" wrapped, row)

/// A reassignment `x = e` (or `x(i) = e`, `x.f = e`, `x += e`, etc.): an
/// lvalue followed by an assignment operator. `=(?!=)` matches `=` but not
/// `==`, so `b == 1` stays a bare expression. Group 1 (leading identifier)
/// is the ROOT variable to echo. Checked after declRe so `let ...` stays a declaration.
///
/// `(...)` is in the suffix set because `()` is how Blade subscripts an array
/// (`[]` is tuple/pack access): without it `arr(0) = 9.0` missed the
/// reassignment lane, was wrapped as a bare expression, and its write was
/// dropped from the session along with the transient wrapper. The trailing
/// `\s*=` is what keeps the addition tight -- a call with no assignment after
/// it (`f(x)`) still fails to match and stays an expression.
let assignRe =
    Regex(@"^\s*([A-Za-z_][A-Za-z0-9_]*)(?:\.[A-Za-z_][A-Za-z0-9_]*|\[[^\]]*\]|\([^)]*\))*\s*(?:\+=|-=|\*=|/=|=(?!=))")

/// A raw run-output line is `name = value`; grab the leading name so we can
/// single out just the one binding we mean to echo.
let outNameRe = Regex(@"^([A-Za-z_][A-Za-z0-9_]*) = ", RegexOptions.Compiled)

/// Every identifier-shaped token in a snippet -- the best-effort dependency
/// probe rebind placement uses (same textual stance as bindingName/declRe: a
/// name inside a comment or string can fool it; a false positive only ever
/// moves a rebind later than strictly necessary, never changes which names
/// it sees).
let private identTokensRe = Regex(@"[A-Za-z_][A-Za-z0-9_]*", RegexOptions.Compiled)

let private referencedNames (snippet: string) : Set<string> =
    identTokensRe.Matches snippet |> Seq.map _.Value |> Set.ofSeq

/// Classification looks at the first non-comment, non-blank line so a
/// doc-commented declaration isn't mistaken for a bare expression -- the same
/// line `bindingName` reads the declared name from, so the two can never
/// disagree about which line a snippet's shape lives on.
let classifyTarget (s: string) = firstSignificantLine s

// Splitting a submission into its top-level statements.
//
// `classifyTarget` above reads the FIRST significant line and gives the whole
// submission one shape. That is right for the interactive REPL, where a
// submission is a line (or a `:paste` block of declarations), and wrong for a
// notebook cell, where prose-driven cells routinely mix the two:
//
//     let t = b, c
//     t[0] + t[1]
//
// Classified as a declaration, that cell passes through whole and a
// declarations-only top-level grammar rejects its second line (BL1999
// "Expected declaration"); classified as an expression it would be wrapped and
// its first line would be the thing that failed. Neither shape fits, because
// the cell has TWO statements -- and the split is what lets each of them be a
// binding this cell can name, echo and later supersede on its own, which stays
// true however permissive the top-level grammar becomes. So the notebook lane
// splits first and classifies each piece.
//
// The split is the LEXER's, not a regex's: `tokenizeWithNewlines` already
// answers "is this newline a statement terminator" -- it drops newlines inside
// (), [] and {} and keeps the ones at depth 0 -- so brace, paren, string and
// comment nesting are handled by the same code the parser trusts. What the
// token filter cannot know is the handful of places the PARSER goes on to skip
// a depth-0 newline anyway: after a trailing `=`, `with` or `->`, and before a
// continuation line opening with `|`, `where`, `==`, `then`, `else`. Those are
// the two predicates below, and both are deliberately biased AGAINST splitting:
// merging two statements reproduces today's behaviour (the parse error the user
// already sees), while splitting one statement in half would invent a new one.
module private Segments =
    open Blade.Lexer

    /// Can this token be the LAST one of a top-level statement? A trailing
    /// operator, `=`, comma, `with` or `->` means the parser is mid-production
    /// and the newline after it is not a terminator.
    let canEnd (k: TokenKind) =
        match k with
        | TokInt _ | TokFloat _ | TokString _ | TokChar _ | TokBool _ | TokIdent _ -> true
        | TokRParen | TokRBracket | TokRBrace | TokUnderscore -> true
        // `>` closes a type application (`Idx<4>`), and a nested one closes
        // with the `>>` the lexer produces for `Idx<Array<...>>`.
        | TokOp op when op.Length > 0 && op |> Seq.forall (fun c -> c = '>') -> true
        | TokKeyword (KwTrue | KwFalse | KwCompute | KwZero | KwVoid) -> true
        | _ -> false

    /// Can this token OPEN a top-level statement? Anything that only ever
    /// continues an expression -- an infix operator, a `|` match arm, a `where`
    /// clause on its own line -- says the previous line has not finished.
    let canStart (k: TokenKind) =
        match k with
        | TokOp _ | TokNamedInfix _ -> false
        | TokComma | TokSemi | TokColon | TokColonColon | TokDot | TokDotDot -> false
        | TokPipe | TokAt | TokHash | TokQuestion -> false
        | TokRParen | TokRBracket | TokRBrace -> false
        | TokNewline | TokEOF | TokError _ -> false
        | TokKeyword (KwWhere | KwWith | KwThen | KwElse | KwAnd | KwIn | KwAs | KwLike) -> false
        | TokKeyword (KwRec | KwMut | KwComm | KwAntisymm | KwOmp | KwCuda | KwMpi | KwReynolds) -> false
        | _ -> true

    /// Does this text carry any CODE, or is it blank/comments only? Token-level
    /// so a `/* ... */` block counts as empty exactly as a `//` line does.
    let hasCode (text: string) =
        try
            Blade.Lexer.tokenize text
            |> List.exists (fun t ->
                match t.Kind with
                | TokNewline | TokEOF | TokError _ -> false
                | _ -> true)
        with _ -> text.Trim() <> ""

/// Split a submission into its top-level statements: `(1-based start line
/// within `source`, text)` per statement, in order, with blank lines trimmed
/// off each end and blank/comment-only pieces dropped. A leading `//` comment
/// stays attached to the statement BELOW it, the way a doc comment reads.
///
/// A lexer failure yields the whole submission as one statement, which is
/// exactly what the caller did before this existed: an unlexable cell has to
/// reach the compiler and earn its own diagnostic, not be mangled here.
let splitTopLevelStatements (source: string) : (int * string) list =
    let src = source.Replace("\r\n", "\n")
    let lines = src.Split('\n')
    let breaks =
        try
            let toks = Blade.Lexer.tokenizeWithNewlines src |> Array.ofList
            let acc = ResizeArray<int>()
            let mutable prev : Blade.Lexer.TokenKind option = None
            for i in 0 .. toks.Length - 1 do
                match toks.[i].Kind with
                | Blade.Lexer.TokNewline ->
                    let next = if i + 1 < toks.Length then Some toks.[i + 1].Kind else None
                    match prev, next with
                    | Some p, Some n when Segments.canEnd p && Segments.canStart n ->
                        // The newline token sits ON the line it terminates.
                        acc.Add toks.[i].Line
                    | _ -> ()
                | Blade.Lexer.TokEOF -> ()
                | k -> prev <- Some k
            Set.ofSeq acc
        with _ -> Set.empty
    let raw = ResizeArray<int * string>()
    let cur = ResizeArray<string>()
    let mutable startLine = 1
    for i in 0 .. lines.Length - 1 do
        cur.Add lines.[i]
        if Set.contains (i + 1) breaks then
            raw.Add (startLine, String.concat "\n" cur)
            cur.Clear()
            startLine <- i + 2
    if cur.Count > 0 then raw.Add (startLine, String.concat "\n" cur)
    [ for (sl, text) in raw do
        let ls = text.Split('\n')
        match ls |> Array.tryFindIndex (fun l -> l.Trim() <> "") with
        | None -> ()
        | Some first ->
            let last = ls |> Array.findIndexBack (fun l -> l.Trim() <> "")
            let body = ls.[first .. last] |> String.concat "\n"
            if Segments.hasCode body then yield (sl + first, body) ]

// The evaluation lanes and their results.

/// Which evaluator produced a run's output.
type Lane =
    /// The tree-walking interpreter -- the common path, <100 ms.
    | LaneInterp
    /// A g++ compile+run of this ONE input, entered only when the interpreter
    /// cannot evaluate some node yet (125) or hit its own bug (70).
    | LaneGpp

/// Everything one candidate evaluation produced, with nothing rendered.
type CandidateRun =
    { ExitCode: int
      Lane: Lane
      /// Wall time across lowering AND evaluation -- the number a notebook cell
      /// badge means by "how long did this take".
      ElapsedMs: int
      Stdout: string
      Stderr: string
      /// stdout split on newlines with the generated main's timing line dropped.
      Lines: string[]
      /// The RAW `name = value` line for the echo target, if the run printed
      /// one. Annotation is the caller's (ReplTypes.annotate).
      Echo: string option
      Info: Map<string, ReplTypes.Info>
      /// Typecheck warnings, drained structurally -- INTERP lane only. On the
      /// g++ lane the compile driver re-runs the front end and prints them
      /// itself, exactly as it always has, so surfacing them here too would
      /// double them.
      Warnings: Blade.Diagnostics.Diagnostic list }

/// The verdict on one candidate. `CandidateRan` covers a NON-ZERO exit too: a
/// runtime panic is a real answer, and whether such a candidate is kept is the
/// caller's call, not the engine's.
type CandidateOutcome =
    /// Front-end / validate rejection, structured plus the SourceMap that
    /// renders it rustc-style.
    | CandidateRejected of Blade.Diagnostics.Diagnostic list * Blade.Diagnostics.SourceMap
    /// The g++ lane could not produce a result: the message is already
    /// composed exactly as the REPL has always printed it.
    | CandidateFailed of string
    | CandidateRan of CandidateRun

// The structured result the notebook lane speaks.

/// One displayed value, with its concrete type and its ELIDED display value.
/// The notebook lane reports at most ONE entry: the cell's final statement,
/// when that statement is a bare expression -- under `Name = ""` (the
/// transient wrapper's name, stripped exactly as the interactive echo strips
/// it), or under the declaration's own name when the expression is a bare
/// identifier naming a session function (the signature echo). Declarations
/// and reassignments display nothing.
type Binding =
    { Name: string
      Type: string
      Value: string }

/// One top-level statement of a MIXED cell, once placed in a candidate: where
/// it landed, what hidden wrapper (if any) it carries, which line of the
/// submission it came from, the bindings it contributes to the cell's result,
/// and whether it survives the commit.
type MixedSlot =
    { Index: int
      Prefix: int
      PrefixRow: int
      SubLine: int
      /// (name to REPORT, name the session actually bound it under). They
      /// differ only for a bare expression, whose transient wrapper reports
      /// as "" exactly as the single-expression lane's does.
      Names: (string * string) list
      /// The identifier this statement consists of, when it is nothing but
      /// one (`bareIdentifier`). Carried so the report can prefer the
      /// DECLARATION's binding when that identifier turns out to name a
      /// function: only the run's own type map knows whether it does, and
      /// that map does not exist until after the run. `None` for every other
      /// statement shape.
      NamedIdent: string option
      /// A transient expression wrapper: run and echoed, never committed.
      Transient: bool }

/// A diagnostic in CELL-LOCAL 1-based coordinates (see `EvalResult`).
type EvalDiagnostic =
    { Severity: string
      Line: int
      Col: int
      EndLine: int
      EndCol: int
      Message: string
      /// BLxxxx code; "" = none.
      Code: string }

/// One submission's outcome, in the coordinate space of the SUBMISSION rather
/// than of the assembled session file.
type EvalResult =
    { /// The submission was ACCEPTED (parsed, typechecked, lowered and ran to
      /// exit 0). False is the wire form of "[snippet not kept]": the session
      /// is unchanged. Note that a bare expression is accepted without joining
      /// the session -- acceptance and membership are different questions.
      Kept: bool
      ExitCode: int
      Lane: Lane
      ElapsedMs: int
      /// What the PROGRAM wrote: the run's stdout minus the generated main's
      /// timing line and minus every `name = value` binding echo. The session
      /// re-prints all of its bindings on every input, so leaving them here
      /// would make each cell replay the whole notebook; the ones that matter
      /// are in `Bindings`.
      Stdout: string
      Stderr: string
      Bindings: Binding list
      Diagnostics: EvalDiagnostic list
      /// Display frames this submission produced, in emission order: the raw
      /// JSON text of each, exactly as it travelled on stdout
      /// (Blade.Display.Frame). `ide serve` writes them as the eval response's
      /// `display` array (docs/display-frames.md section 2); they are LIFTED OUT of
      /// `Stdout` above, so a client showing both never sees a frame twice.
      ///
      /// A session re-runs every accumulated snippet on each submission, so an
      /// earlier cell's frames reappear here. That is deliberate and is the
      /// contract in the spec's section 10: their generated `meta.id`s are stable
      /// across re-runs, so the panel merges equal ids into the plot it already
      /// has, and the notebook lane skips an already-seen id for cell outputs.
      Display: string list }

/// The compiled fallback lane: session source path -> working directory ->
/// (exit code, stdout, stderr), or an already-composed failure message.
/// Injected because `compileToExe` lives in Cli.fs on top of Build.fs, both of
/// which compile long after this file.
type CompiledLane = string -> string -> Result<int * string * string, string>

let mutable private compiledLane : CompiledLane option = None

/// Cli.fs calls this once at dispatch time. Until it does, a submission the
/// interpreter cannot evaluate reports the lane as unavailable rather than
/// pretending the snippet failed.
let installCompiledLane (f: CompiledLane) = compiledLane <- Some f

// Diagnostic remapping: session-file coordinates -> submission coordinates.

/// Where one piece of a submission sits inside the assembled session file --
/// everything the remap needs. `Prefix` is the characters a hidden wrapper
/// prepended (`let it = `), `PrefixRow` the row WITHIN the snippet that carries
/// them (0 except when a leading comment pushed the wrapper down), and
/// `SubLine` the 1-based line of the SUBMISSION this snippet's text starts on
/// -- 1 for a whole-submission placement whose `Trim()` ate nothing, and the
/// statement's own line for each piece of a split (mixed) cell.
type private Placement =
    { Index: int
      Prefix: int
      PrefixRow: int
      SubLine: int }

let private lineCount (s: string) =
    (s.Replace("\r\n", "\n").Split('\n')).Length

/// Snippets are assembled with `String.concat "\n\n"`, so each one costs its
/// own line count plus the single blank line separating it from the next.
let private startLineOf (candidate: ResizeArray<string>) (idx: int) =
    let mutable line = 1
    for j in 0 .. idx - 1 do
        line <- line + lineCount candidate.[j] + 1
    line

let private severityText (s: Blade.Diagnostics.Severity) =
    match s with
    | Blade.Diagnostics.SevError -> "error"
    | Blade.Diagnostics.SevWarning -> "warning"
    | Blade.Diagnostics.SevNote -> "note"

/// Rebase one session-file diagnostic onto the submission.
///
/// A submission owns one placement per top-level statement it contributed (one
/// for all of it, until a mixed cell split it), and a diagnostic belongs to
/// whichever of them its line falls inside.
///
/// A rebind splices MID-session, so the compiler can perfectly well report
/// against a LATER snippet that the rebind just broke -- a position with no
/// meaning in this cell at all. Those clamp to 1:1 and say so in the message,
/// which is honest and keeps the client from squiggling an unrelated line. A
/// diagnostic with no span (a provider load that failed at lowering time)
/// clamps to 1:1 as well, but earns no prefix: it is not elsewhere, it is
/// nowhere.
let private remapDiagnostic (candidate: ResizeArray<string>) (ps: Placement list)
                            (d: Blade.Diagnostics.Diagnostic) : EvalDiagnostic =
    let sev = severityText d.Severity
    let sl = d.Span.StartLine
    let owner =
        if sl <= 0 then None
        else
            ps |> List.tryFind (fun p ->
                p.Index >= 0 && p.Index < candidate.Count &&
                let start = startLineOf candidate p.Index
                sl >= start && sl <= start + lineCount candidate.[p.Index] - 1)
    match owner with
    | None when sl <= 0 ->
        { Severity = sev; Line = 1; Col = 1; EndLine = 1; EndCol = 1
          Message = d.Message; Code = d.Code }
    | None ->
        { Severity = sev; Line = 1; Col = 1; EndLine = 1; EndCol = 1
          Message = "elsewhere in session: " + d.Message; Code = d.Code }
    | Some p ->
        let start = startLineOf candidate p.Index
        // The wrapper's prefix sits on exactly one line of the snippet.
        let prefixLine = start + p.PrefixRow
        let mapCol (l: int) (c: int) = if l = prefixLine then max 1 (c - p.Prefix) else max 1 c
        let shift = start - p.SubLine
        let line = max 1 (sl - shift)
        let col = mapCol sl d.Span.StartCol
        let endLine = max line (d.Span.EndLine - shift)
        let endCol = if d.Span.EndCol >= 1 then mapCol d.Span.EndLine d.Span.EndCol else col
        { Severity = sev; Line = line; Col = col
          EndLine = endLine; EndCol = max 1 endCol
          Message = d.Message; Code = d.Code }

/// A failure with nothing to point at still has to SAY something. A front-end
/// rejection arrives as spanned diagnostics, but a runtime guard panic and a
/// toolchain failure arrive on stderr with no position at all -- and a client
/// that builds its error card from `Diagnostics.[0]` would show "rejected"
/// with no reason. Those become ONE 1:1 error diagnostic carrying the message
/// verbatim; the text stays on `Stderr` as well, for a client that shows both.
let private ensureFailureDiagnostic (r: EvalResult) : EvalResult =
    if r.Kept || r.Stderr.Trim() = "" then r
    elif r.Diagnostics |> List.exists (fun d -> d.Severity = "error") then r
    else
        { r with
            Diagnostics =
                { Severity = "error"; Line = 1; Col = 1; EndLine = 1; EndCol = 1
                  Message = r.Stderr.Trim(); Code = "" } :: r.Diagnostics }

// The engine.

/// Splice a DECLARATION snippet into a candidate list: rebinding replaces the
/// earlier definition IN PLACE so later snippets referencing the name see the
/// update (duplicate lets are a C++ redeclaration error) -- except when the new
/// text references a name bound LATER in the session, where the rebind moves
/// just past its last dependency instead.
///
/// A rebind may reference a binding added AFTER it (split one cell into two,
/// then rebind the first: `let pairs = xloop <@> ...` where `xloop` joined the
/// session after `pairs` did). In place, that reference sits above its
/// definition in the flat session file and the candidate is rejected as
/// unbound -- so place the new text just after the LAST later snippet it
/// references. Only safe when no snippet in between references THIS name; if
/// one does, the dependency is genuinely circular in a flat file and in-place
/// (with its honest unbound/type error) stands.
///
/// Returns the index the snippet landed at AND how the operation moved the
/// indices that were already valid: a mixed cell places several statements one
/// after another and each splice can shift the ones before it.
let private spliceDeclaration (candidate: ResizeArray<string>) (text: string) : int * (int -> int) =
    match bindingName text with
    | Some name ->
        let idx = candidate.FindIndex(fun s -> bindingName s = Some name)
        if idx >= 0 then
            let refs = referencedNames text
            let lastDep =
                [ idx + 1 .. candidate.Count - 1 ]
                |> List.filter (fun j ->
                    match bindingName candidate.[j] with
                    | Some n -> Set.contains n refs
                    | None -> false)
                |> List.tryLast
            match lastDep with
            | Some j when [ idx + 1 .. j ] |> List.forall (fun k ->
                              not (Set.contains name (referencedNames candidate.[k]))) ->
                candidate.RemoveAt idx
                // The removal shifted everything after `idx` down one, so
                // inserting AT `j` lands immediately after the dependency.
                candidate.Insert(j, text)
                let fix k = if k = idx then j elif k > idx && k <= j then k - 1 else k
                (j, fix)
            | _ ->
                candidate.[idx] <- text
                (idx, id)
        else
            candidate.Add text
            (candidate.Count - 1, id)
    | None ->
        candidate.Add text
        (candidate.Count - 1, id)

/// Does this session declare NOTHING that can write through a binding it was
/// handed? A `mut` parameter is the only such thing in Blade: it is array-only
/// and its element writes alias the caller's storage, so a call can change a
/// value that was bound earlier. Everything else either rebinds a name (which
/// the snippet list already shows) or writes through a reassignment statement
/// (which `assignRe` finds in the snippet text).
///
/// With no `mut` parameter anywhere, a snippet's effect on OTHER bindings is
/// confined to what its own text says, which is what lets the session cache
/// keep a proper prefix of an earlier run (see the prefix test in
/// `EvalCandidate`). Top-level `function` declarations are the ONLY place a
/// `mut` parameter can exist: lambda and nested-function parameters go through
/// `parseLambdaParam`, whose grammar starts at the identifier and admits no
/// modifier -- so checking `DeclFunction` params is complete, not shallow.
let mutationFreeSession (prog: Blade.Ast.Program) : bool =
    prog.Modules
    |> List.forall (fun m ->
        m.Decls
        |> List.forall (fun d ->
            match d.Value with
            | Blade.Ast.DeclFunction f ->
                f.Params |> List.forall (fun p -> p.Mutability <> Blade.Ast.Mutable)
            | _ -> true))

/// One accumulating REPL session: the snippet list, the temp directory its
/// assembled program is written to, and eval-once. Independent instances share
/// nothing, which is what lets `ide serve` hold one per notebook.
type ReplSession(runCwd: string) =
    let sessionDir =
        Path.Combine(Path.GetTempPath(), "blade-repl-" + Guid.NewGuid().ToString("N").Substring(0, 8))
    do Directory.CreateDirectory sessionDir |> ignore
    let srcPath = Path.Combine(sessionDir, "session.blade")
    let snippets = ResizeArray<string>()

    /// The snippet list of the last CLEANLY-INTERPRETED candidate, paired with
    /// the values that run left behind. This is the whole of the session cache:
    /// the next candidate may adopt those values only if its own snippet list
    /// STARTS WITH exactly this one (the prefix test in `EvalCandidate`), which
    /// is what makes the cached values the true end state of the new
    /// candidate's prefix.
    ///
    /// Keyed on the CANDIDATE that ran, which is the only list these values
    /// describe. A bare-expression or mixed cell drops its transient wrappers
    /// from the commit, so the NEXT candidate is not an extension of this list
    /// and recomputes -- one cold cell per such cell. That is deliberate: the
    /// alternative, trusting a prefix of the cached run, would mean adopting
    /// values that the dropped snippets could have mutated in place after the
    /// fact, and there is nothing here that could tell whether they did.
    let mutable memoSnippets : string[] = [||]
    let mutable memo = Blade.Interp.Run.emptyMemo

    /// Where a COMPILED session executable runs. The interpreter lane ignores
    /// it; the g++ lane needs it so relative data paths resolve where the user
    /// is, not in the session temp dir.
    member val RunCwd = runCwd with get, set

    member _.SessionDir = sessionDir
    member _.SourcePath = srcPath

    /// The accumulated snippets, in session order. Read-only by convention:
    /// the front ends use it for `:show` and the engine replaces it wholesale
    /// through `Commit`.
    member _.Snippets = snippets

    member _.Reset() =
        snippets.Clear()
        // The cache describes a session that no longer exists.
        memoSnippets <- [||]
        memo <- Blade.Interp.Run.emptyMemo

    /// Adopt a candidate as the new session. Only ever called for a candidate
    /// that ran to exit 0.
    member _.Commit(candidate: ResizeArray<string>) =
        snippets.Clear()
        snippets.AddRange candidate

    member _.Cleanup() =
        try Directory.Delete(sessionDir, true) with _ -> ()

    /// The candidate a DECLARATION produces (see `spliceDeclaration` for the
    /// rebind rule). Returns the list and the index the snippet landed at.
    member _.DeclarationCandidate(trimmed: string) : ResizeArray<string> * int =
        let candidate = ResizeArray(snippets)
        let (idx, _) = spliceDeclaration candidate trimmed
        (candidate, idx)

    /// The candidate a MIXED cell produces: one snippet per TOP-LEVEL
    /// STATEMENT, placed in cell order.
    ///
    /// Splitting is what makes the rebind granularity right, not just what
    /// makes the cell parse. A cell holding `let a = 1` through `let tail =
    /// ...` is not one binding called `a`: a later cell's `let mut a = 2`
    /// would otherwise supersede the WHOLE cell (`bindingName` reads a
    /// snippet's first name) and take `b`, `c` and the rest down with it.
    ///
    /// Each statement is classified exactly as a whole submission would be:
    /// declarations splice by name, reassignments and bare expressions ride a
    /// hidden binding. The wrapper is what makes them parse under a grammar
    /// that admits only declarations at top level, but it is NOT only that:
    /// the name is the handle this engine reads the value back under (run
    /// output is `name = value` lines) and the handle a rebind matches on. A
    /// grammar that auto-names a bare expression names it per MODULE, counting
    /// across the whole assembled session, which is not a name a cell can
    /// predict -- so the wrapper stays either way.
    /// The expression wrappers are TRANSIENT -- run and echoed, dropped from the commit --
    /// which is the single-expression lane's rule and is also what keeps
    /// re-running a cell from stacking a second copy of its expressions.
    /// Statements still run in cell order, so a call-for-effect earlier in the
    /// cell is visible to a value read later in the same cell.
    member _.MixedCandidate(segments: (int * string) list) : ResizeArray<string> * MixedSlot list =
        let candidate = ResizeArray(snippets)
        let slots = ResizeArray<MixedSlot>()
        let freshName (stem: string) =
            let inUse = candidate |> Seq.choose bindingName |> Set.ofSeq
            Seq.initInfinite (fun i -> if i = 0 then stem else $"{stem}{i}")
            |> Seq.find (fun n -> not (Set.contains n inUse))
        for (subLine, text) in segments do
            let head = classifyTarget text
            if declRe.IsMatch head then
                let (idx, fix) = spliceDeclaration candidate text
                // A splice that MOVED a snippet renumbers the statements this
                // cell already placed; without this their diagnostics would
                // rebase against someone else's text.
                for k in 0 .. slots.Count - 1 do
                    slots.[k] <- { slots.[k] with Index = fix slots.[k].Index }
                slots.Add { Index = idx; Prefix = 0; PrefixRow = 0; SubLine = subLine
                            Names = topLevelBindingNames text |> List.map (fun n -> (n, n))
                            NamedIdent = None
                            Transient = false }
            else
                // The wrapper goes on the first SIGNIFICANT line -- the same
                // line `classifyTarget` classified on -- so a statement that
                // opens with a comment still gets a parseable binding.
                let isAssign = assignRe.IsMatch head
                let hidden = freshName (if isAssign then "__assign" else "it")
                let prefix = $"let {hidden} = "
                let (wrapped, row) = wrapAtSignificantLine prefix text
                candidate.Add wrapped
                slots.Add
                    { Index = candidate.Count - 1; Prefix = prefix.Length; PrefixRow = row
                      SubLine = subLine
                      Names =
                        // A reassignment echoes the variable it mutated; a bare
                        // expression echoes its value under the stripped name.
                        (if isAssign then
                            let root = (assignRe.Match head).Groups.[1].Value
                            [ (root, root) ]
                         else [ ("", hidden) ])
                      NamedIdent = (if isAssign then None else bareIdentifier text)
                      Transient = not isAssign }
        (candidate, List.ofSeq slots)

    /// The candidate a REASSIGNMENT produces. Wrapping it in a hidden binding
    /// is what carries it under a declarations-only top-level grammar, and the
    /// wrapper's value IS the ExprAssign, which mutates the target's existing cell.
    /// Unlike bare expressions the wrapper is KEPT so the mutation persists;
    /// successive assignments append under fresh __assignN names, so
    /// `b = b + 1` twice accumulates 1->2->3. Returns the list, the index, the
    /// hidden name, and the ROW of the submission the wrapper landed on (see
    /// `wrapAtSignificantLine`: a comment above the statement pushes it down).
    member _.AssignmentCandidate(trimmed: string) : ResizeArray<string> * int * string * int =
        let candidate = ResizeArray(snippets)
        let hidden =
            let inUse = candidate |> Seq.choose bindingName |> Set.ofSeq
            Seq.initInfinite (fun i -> if i = 0 then "__assign" else $"__assign{i}")
            |> Seq.find (fun n -> not (Set.contains n inUse))
        let (text, row) = wrapAtSignificantLine $"let {hidden} = " trimmed
        candidate.Add text
        (candidate, candidate.Count - 1, hidden, row)

    /// The candidate a BARE EXPRESSION produces: `blade run` semantics only
    /// print top-level BINDINGS, so the expression is wrapped in a transient
    /// one that is run, echoed, and NOT kept -- re-entering the same expression
    /// echoes again rather than diffing to silence. Returns the list, the
    /// index, the transient name, and the ROW of the submission the wrapper
    /// landed on -- `// note` above the expression pushes it down, and putting
    /// it on row 0 regardless made `let it = // note` swallow the line below
    /// (BL1999 "Unexpected token: end of line") for a cell that reads fine.
    member _.ExpressionCandidate(trimmed: string) : ResizeArray<string> * int * string * int =
        let transient =
            let inUse = snippets |> Seq.choose bindingName |> Set.ofSeq
            Seq.initInfinite (fun i -> if i = 0 then "it" else $"it{i}")
            |> Seq.find (fun n -> not (Set.contains n inUse))
        let candidate = ResizeArray(snippets)
        let (text, row) = wrapAtSignificantLine $"let {transient} = " trimmed
        candidate.Add text
        (candidate, candidate.Count - 1, transient, row)

    /// Evaluate one candidate. INTERP-FIRST: the candidate lowers ONCE
    /// (shared with the type-annotation map), then runs under the tree-walking
    /// interpreter. On a supported exit its output is authoritative and no g++
    /// is invoked -- a typical turn drops from ~1-5s to <100ms. If the
    /// interpreter can't yet evaluate some node (125) or hits its own bug (70)
    /// it falls back to a g++ compile+run for this one input.
    ///
    /// `target` is the binding whose output line to hand back as the echo;
    /// `notice` receives the one-line fallback announcement BEFORE the g++
    /// build starts, so an interactive caller can show it while waiting.
    /// `printOnly`: the top-level names whose `name = value` output lines this
    /// caller will actually read (Some) -- the run formats only those, which on
    /// a data-heavy session is most of the eval -- or None for the full
    /// compiled-parity print. Must be a SUPERSET of every name the caller
    /// resolves against the output; the g++ fallback lane ignores it (that
    /// lane always prints everything, a harmless superset).
    member this.EvalCandidate(candidate: ResizeArray<string>, target: string option,
                              printOnly: Set<string> option,
                              notice: string -> unit) : CandidateOutcome =
        let src = String.concat "\n\n" candidate + "\n"
        File.WriteAllText(srcPath, src)
        // THE PREFIX TEST. The cached values may be adopted only when every
        // snippet that produced them is still present, unchanged, at the same
        // position -- then they are exactly the state this candidate's prefix
        // would have reached, and the snippets that remain are the only ones
        // with work to do. Any edit, reorder, or rebind lands a differing
        // snippet inside the cached range and drops the whole memo, which is
        // the conservative answer and the correct one: a redefinition splices
        // in place, so it is visible here as a changed element.
        let memoIn =
            let shared =
                let n = min memoSnippets.Length candidate.Count
                let mutable k = 0
                while k < n && memoSnippets.[k] = candidate.[k] do k <- k + 1
                k
            if shared = memoSnippets.Length then
                // The cached run is a whole prefix of this one: adopt it all.
                memo
            elif shared = 0 then Blade.Interp.Run.emptyMemo
            else
                // The cached run has snippets this candidate does not. In the
                // common case they are the TRANSIENT WRAPPERS a bare-expression
                // or mixed cell runs and never commits -- every `plot.*` cell is
                // one -- and without this arm each plot would cost the whole
                // session again, which is most of a plotting notebook.
                //
                // Two things must hold to keep the first `shared` values. Their
                // own bindings must go, so the dropped snippets have to NAME
                // themselves; and, because they ran AFTER the ones being kept,
                // they must not have been able to change those. `MutationFree`
                // rules out writes through a `let mut` array and the `assignRe`
                // scan rules out a reassignment; anything left unproven drops
                // the memo rather than guessing.
                let dropped = [ for i in shared .. memoSnippets.Length - 1 -> memoSnippets.[i] ]
                let names = dropped |> List.map topLevelBindingNames
                let allNamed = names |> List.forall (List.isEmpty >> not)
                let reassigns = dropped |> List.exists (fun s -> assignRe.IsMatch(s.Trim()))
                if memo.MutationFree && allNamed && not reassigns then
                    let gone = names |> List.concat |> Set.ofList
                    // Frames travel with their value: a name whose value is
                    // dropped must not keep a cached picture behind, or the
                    // map outlives every session that could replay from it.
                    { memo with
                        Values = memo.Values |> Map.filter (fun nm _ -> not (Set.contains nm gone))
                        Frames = memo.Frames |> Map.filter (fun nm _ -> not (Set.contains nm gone)) }
                else Blade.Interp.Run.emptyMemo
        let watch = System.Diagnostics.Stopwatch.StartNew()
        match Blade.Interp.Repl.lowerSessionDiag (Some srcPath) src with
        | Error (ds, sm) ->
            watch.Stop()
            CandidateRejected (ds, sm)
        | Ok lowered ->
            let info = ReplTypes.sessionInfoOf lowered
            // Drained HERE, where `lower` would have printed: the channels are
            // AsyncLocal and reset per typeCheck, and nothing between this
            // point and the run touches them.
            let warnings = Blade.Lowering.typeCheckWarningDiagnostics false
            let finish (lane: Lane) (code: int) (stdout: string) (stderr: string) =
                watch.Stop()
                let lines =
                    stdout.Replace("\r\n", "\n").Split('\n')
                    |> Array.filter (fun l -> not (isTimingLine l))
                let echo =
                    target
                    |> Option.bind (fun tgt ->
                        lines
                        |> Array.tryFind (fun l ->
                            let m = outNameRe.Match l
                            m.Success && m.Groups.[1].Value = tgt))
                CandidateRan
                    { ExitCode = code; Lane = lane; ElapsedMs = int watch.ElapsedMilliseconds
                      Stdout = stdout; Stderr = stderr; Lines = lines; Echo = echo
                      Info = info
                      Warnings = (if lane = LaneInterp then warnings else []) }
            match Blade.Interp.Repl.evalSessionMemo lowered "session" memoIn printOnly with
            | (Blade.Interp.Repl.InterpDone r, memoOut) ->
                // Interpreter is authoritative (exit 0 or guard panic 1).
                // Publish the memo only for a CLEAN run: a guard panic (exit 1)
                // stopped partway, so the bindings after it never ran and the
                // values it did produce are not a whole prefix's end state.
                if r.ExitCode = Blade.Interp.Run.ExitOk then
                    memoSnippets <- candidate.ToArray()
                    memo <- { memoOut with MutationFree = mutationFreeSession lowered.Prog }
                else
                    memoSnippets <- [||]
                    memo <- Blade.Interp.Run.emptyMemo
                finish LaneInterp r.ExitCode r.Stdout r.Stderr
            | (Blade.Interp.Repl.InterpFellShort _, _) ->
                // The g++ lane runs its own process; nothing it computes is
                // visible here, so the cache cannot describe the session state
                // after it and must be dropped.
                memoSnippets <- [||]
                memo <- Blade.Interp.Run.emptyMemo
                notice "-- falling back to compiled evaluation for this input --"
                match compiledLane with
                | None ->
                    watch.Stop()
                    CandidateFailed "compiled evaluation is unavailable in this process"
                | Some run ->
                    match run srcPath this.RunCwd with
                    | Error msg ->
                        watch.Stop()
                        CandidateFailed msg
                    | Ok (code, stdout, stderr) -> finish LaneGpp code stdout stderr

    /// Evaluate one submission with full REPL semantics and hand back
    /// structured data -- the notebook's entry point. Classification,
    /// splicing, evaluation, commit and diagnostic rebasing all happen here;
    /// nothing is printed and nothing is rendered.
    member this.EvalOnce(source: string) : EvalResult =
        let trimmed = source.Trim()
        let blank =
            { Kept = true; ExitCode = 0; Lane = LaneInterp; ElapsedMs = 0
              Stdout = ""; Stderr = ""; Bindings = []; Diagnostics = []; Display = [] }
        if trimmed = "" then blank else
        // `Trim()` may have eaten leading blank lines; the client's cell
        // coordinates still count them.
        let leadPad =
            let cut = source.Length - source.TrimStart().Length
            source.Substring(0, cut).Replace("\r\n", "\n") |> Seq.filter (fun c -> c = '\n') |> Seq.length
        let head = classifyTarget trimmed
        /// Run a candidate and project it onto the submission. `wanted` pairs
        /// the name to REPORT with the name the session actually bound it
        /// under (they differ only for a bare expression, whose transient
        /// wrapper reports as ""); it is a FUNCTION of the run's type map
        /// because one of those choices -- whether a bare identifier echoes
        /// from the declaration or from its transient wrapper -- turns on
        /// whether the identifier names a function, which only the map knows.
        /// `commit` carries the list to ADOPT, which is the candidate itself
        /// for the declaration and reassignment lanes, the candidate minus its
        /// transient wrappers for a mixed cell, and None for the
        /// bare-expression lane, which never joins the session.
        let evalWith (candidate: ResizeArray<string>) (placements: Placement list)
                     (target: string option)
                     (reads: string list)
                     (wanted: Map<string, ReplTypes.Info> -> (string * string) list)
                     (commit: ResizeArray<string> option) =
            // `reads` is the static superset of session names this lane can
            // resolve against the run's output lines; the target rides along.
            let printOnly = Some (Set.ofList (reads @ Option.toList target))
            ensureFailureDiagnostic <|
            match this.EvalCandidate(candidate, target, printOnly, ignore) with
            | CandidateRejected (ds, _) ->
                { Kept = false; ExitCode = 1; Lane = LaneInterp; ElapsedMs = 0
                  Stdout = ""; Stderr = ""; Bindings = []
                  Diagnostics = ds |> List.map (remapDiagnostic candidate placements)
                  Display = [] }
            | CandidateFailed msg ->
                { Kept = false; ExitCode = 1; Lane = LaneGpp; ElapsedMs = 0
                  Stdout = ""; Stderr = msg; Bindings = []; Diagnostics = []; Display = [] }
            | CandidateRan r ->
                let valueOf (name: string) =
                    r.Lines
                    |> Array.tryPick (fun l ->
                        let m = outNameRe.Match l
                        if m.Success && m.Groups.[1].Value = name then
                            Some (ReplTypes.elideValue (l.Substring m.Length))
                        else None)
                    |> Option.map (fun v ->
                        match Map.tryFind name r.Info with
                        | Some (ReplTypes.RVal t) -> ReplTypes.displayValue t v
                        | _ -> v)
                    |> Option.defaultValue ""
                let typeOf (name: string) =
                    match Map.tryFind name r.Info with
                    | Some (ReplTypes.RVal t) -> Blade.Ide.abstractRenderer [] t
                    | Some (ReplTypes.RFunc s) -> s
                    | None -> ""
                let kept = (r.ExitCode = 0)
                match commit with
                | Some adopt when kept -> this.Commit adopt
                | _ -> ()
                // Display frames leave the text stream here, once, for every
                // lane: both the interpreter and the compiled binary put them
                // on stdout (that is what makes the REPL channel work at all),
                // so the serve channel is a pure line filter rather than a
                // second emission path.
                let frames = r.Lines |> Array.choose (fun l ->
                    if l.StartsWith Blade.Display.Frame.Sentinel
                    then Some (l.Substring Blade.Display.Frame.Sentinel.Length)
                    else None)
                { Kept = kept
                  Display = List.ofArray frames
                  ExitCode = r.ExitCode
                  Lane = r.Lane
                  ElapsedMs = r.ElapsedMs
                  Stdout =
                    let userOut =
                        r.Lines
                        |> Array.filter (fun l ->
                            not (outNameRe.IsMatch l) && not (l.StartsWith Blade.Display.Frame.Sentinel))
                    let joined = String.concat "\n" userOut
                    if joined.Trim() = "" then "" else joined
                  Stderr = r.Stderr.Trim()
                  Bindings =
                    if kept then
                        wanted r.Info |> List.map (fun (report, bound) ->
                            { Name = report; Type = typeOf bound; Value = valueOf bound })
                    else []
                  Diagnostics = r.Warnings |> List.map (remapDiagnostic candidate placements) }
        // How many TOP-LEVEL STATEMENTS did this submission bring? One is the
        // REPL's world and keeps its three lanes exactly as they were -- including
        // the bare-identifier function probe and the `it`/`__assign` naming a
        // front end may already be showing. Two or more is a notebook cell that
        // mixes shapes, and no single classification fits it.
        let segments = splitTopLevelStatements source
        // Comments only: nothing to run, and nothing to reject either.
        if List.isEmpty segments then blank
        elif not (List.isEmpty (List.tail segments)) then
            let (candidate, slots) = this.MixedCandidate segments
            let committed =
                let drop = slots |> List.filter _.Transient |> List.map _.Index |> Set.ofList
                let out = ResizeArray<string>()
                for i in 0 .. candidate.Count - 1 do
                    if not (Set.contains i drop) then out.Add candidate.[i]
                out
            let placements =
                slots |> List.map (fun s ->
                    { Index = s.Index; Prefix = s.Prefix; PrefixRow = s.PrefixRow; SubLine = s.SubLine })
            // A cell DISPLAYS only its "return value": the final statement,
            // and only when that statement is a bare expression. Declarations
            // and reassignments run silently -- their work shows through later
            // reads, not through an echo -- and a mid-cell expression still
            // runs where it was written (its effects and display frames land)
            // but does not echo either. This is the notebook's display rule
            // alone: the interactive REPL front end drives the candidate API
            // directly and keeps its per-declaration echoes.
            //
            // A final statement that is nothing but an identifier NAMING A
            // FUNCTION echoes from the declaration -- its own name and the
            // checker's rendering of the signature -- exactly as a
            // one-statement cell holding the same text does. Without this the
            // wrapper answered instead, anonymously and in raw IR
            // (`Arrow<T, T -> T>` where the declaring cell had rendered
            // `(T^1, T^1) -> T^1`). An identifier naming a VALUE keeps the
            // anonymous echo: it has a printed value to show, which is the
            // thing being asked for.
            let lastExpr = slots |> List.tryLast |> Option.filter _.Transient
            let report (info: Map<string, ReplTypes.Info>) =
                match lastExpr with
                | Some s ->
                    (match s.NamedIdent with
                     | Some n when (match Map.tryFind n info with
                                    | Some (ReplTypes.RFunc _) -> true
                                    | _ -> false) -> [ (n, n) ]
                     | _ -> s.Names)
                | None -> []
            let reads =
                match lastExpr with
                | Some s -> (s.Names |> List.map snd) @ Option.toList s.NamedIdent
                | None -> []
            evalWith candidate placements None reads report (Some committed)
        elif declRe.IsMatch head then
            let (candidate, idx) = this.DeclarationCandidate trimmed
            // Declarations are silent (see the mixed lane's display rule): the
            // cell shows its trailing expression or nothing. With no name to
            // read back, printOnly is empty and the run formats no values --
            // which is what keeps a declaration-heavy cell cheap. The
            // declaration still EVALUATES exactly as before (effects and
            // `|> compute` are program semantics, not display).
            evalWith candidate [ { Index = idx; Prefix = 0; PrefixRow = 0; SubLine = leadPad + 1 } ]
                     None [] (fun _ -> []) (Some candidate)
        elif assignRe.IsMatch head then
            let (candidate, idx, hidden, row) = this.AssignmentCandidate trimmed
            // A reassignment is a statement, not a value ask: silent, like a
            // declaration. The mutation itself persists (the wrapper commits).
            evalWith candidate
                     [ { Index = idx; Prefix = $"let {hidden} = ".Length
                         PrefixRow = row; SubLine = leadPad + 1 } ]
                     None [] (fun _ -> []) (Some candidate)
        else
            // A bare identifier naming a session FUNCTION can't be let-bound
            // just to echo it; its signature comes straight from the
            // typechecker and nothing runs. `bareIdentifier` rather than
            // `identRe` over the raw text, so the notebook's own spelling --
            // the name with a comment trailing it -- takes this path too.
            let ident = bareIdentifier trimmed
            let asFuncSig =
                ident
                |> Option.bind (fun n ->
                    match Map.tryFind n (ReplTypes.sessionInfo (String.concat "\n\n" snippets + "\n")) with
                    | Some (ReplTypes.RFunc s) -> Some (n, s)
                    | _ -> None)
            match asFuncSig with
            | Some (n, s) ->
                { blank with Bindings = [ { Name = n; Type = s; Value = "" } ] }
            | None ->
                let (candidate, idx, transient, row) = this.ExpressionCandidate trimmed
                // The probe above lowers the session WITHOUT the wrapper and
                // can fail where the candidate itself lowers fine (an import
                // the bare-session lowering rejects, say). The run's own type
                // map is the reliable authority: an identifier that turns out
                // to name a function still echoes from the declaration, not
                // the wrapper -- the same rule the mixed lane applies.
                evalWith candidate
                         [ { Index = idx; Prefix = $"let {transient} = ".Length
                             PrefixRow = row; SubLine = leadPad + 1 } ]
                         (Some transient) [ transient ]
                         (fun info ->
                             match ident with
                             | Some n when (match Map.tryFind n info with
                                            | Some (ReplTypes.RFunc _) -> true
                                            | _ -> false) -> [ (n, n) ]
                             | _ -> [ ("", transient) ])
                         None

// Whole-notebook assembly: many cells in, one source out.
//
// `EvalOnce` above feeds the compiler one cell at a time and lets the session
// accumulate. Typechecking a notebook AS THE USER TYPES cannot afford that --
// it wants one source built from every code cell and one payload back -- but
// every cell has to land exactly where an eval would have put it, or the
// squiggles disagree with the kernel that will later run the same cells. So
// the assembly below reuses the very rules the eval path uses: `classifyTarget`
// and `declRe`/`assignRe` to decide a cell's shape, `bindingName` to find the
// definition a rebind supersedes, and `DeclarationCandidate`'s dependency-aware
// placement to decide where the rebind goes. This is the ONLY implementation of
// those rules on the notebook path -- the extension used to keep its own copy of
// them, which is exactly how the two drifted apart.
//
// Nothing here is committed anywhere: no ReplSession, no temp file, no snippet
// list. The only outputs are the assembled text and one window per cell.

/// One cell's contribution to the assembled source: the cell it came from, the
/// text it contributes (empty for a definition a later rebind superseded), and,
/// for every synthetic wrapper prepended, the line index WITHIN this text that
/// carries it plus the prefix's character count. A cell mixing declarations
/// with bare expressions takes ONE wrapper per expression, hence a list.
type private CellSlot =
    { Cell: int
      Text: string
      Wrap: (int * int) list }

/// Wrap every bare-expression STATEMENT of one cell in its own synthetic
/// binding, leaving declarations alone, and report the wrappers.
///
/// Under a grammar that admits only declarations at top level an unwrapped
/// expression does not parse -- and a cell that mixes the two used to poison
/// the check for EVERY other cell, because one parse error is the whole
/// assembled source's answer. Splitting the cell first (`splitTopLevelStatements`)
/// is what lets each statement take the treatment it needs.
///
/// The wrapper is worth keeping even where the grammar stops requiring it: the
/// client filters payload entries by the wrapper's NAME, and a grammar that
/// auto-names a bare expression numbers it per module, across the whole
/// assembled notebook -- a name no single cell can predict, and one the
/// synthetic-name filter would have to re-derive.
///
/// The single-expression cell keeps its historical name `__cellK` -- the client
/// filters synthetic bindings by that shape -- and only a cell with several
/// wrappers numbers them `__cellK_j`.
let private wrapCellExpressions (k: int) (src: string) : string * (int * int) list =
    let lines = src.Split('\n')
    let segments = splitTopLevelStatements src
    let exprRows =
        [ for (subLine, text) in segments do
            if not (declRe.IsMatch (classifyTarget text)) then
                // The wrapper goes on the first SIGNIFICANT line of the
                // statement -- the same line `classifyTarget` classified on --
                // so a statement opening with a comment still gets a parseable
                // binding. `subLine` is 1-based within the cell.
                let rel =
                    text.Split('\n')
                    |> Array.tryFindIndex (fun l ->
                        let t = l.TrimStart()
                        t <> "" && not (t.StartsWith "//"))
                    |> Option.defaultValue 0
                yield subLine - 1 + rel ]
    if List.isEmpty exprRows then (src, [])
    else
        let wrapped = Array.copy lines
        let wraps =
            exprRows
            |> List.mapi (fun j row ->
                let prefix =
                    if exprRows.Length = 1 then $"let __cell{k} = "
                    else $"let __cell{k}_{j} = "
                wrapped.[row] <- prefix + wrapped.[row]
                (row, prefix.Length))
        (String.concat "\n" wrapped, wraps)

/// Assemble ordered CODE cell sources (the client filters markdown out) into
/// one session source plus one `Blade.Ide.CellWindow` per cell, in input order,
/// naming the 1-based inclusive line range each cell's text occupies. Stateless
/// by construction: call it with the same cells and you get the same answer.
let assembleCells (cells: string list) : string * Blade.Ide.CellWindow list =
    let slots = ResizeArray<CellSlot>()
    cells
    |> List.iteri (fun k raw ->
        // Line counting downstream assumes \n, and the client's cell text may
        // still carry the editor's CRLFs.
        let raw = raw.Replace("\r\n", "\n")
        // Wrapping happens FIRST and for every cell shape: a declaration-first
        // cell can still carry bare expressions after its declarations, and
        // those need the same synthetic bindings an expression-only cell gets.
        let (src, wraps) = wrapCellExpressions k raw
        if declRe.IsMatch (classifyTarget src) then
            match bindingName src with
            // A `// doc` line ahead of the keyword defeats bindingName exactly
            // as it does in the eval path: unnamed means unmatchable, so the
            // cell simply appends.
            | None -> slots.Add { Cell = k; Text = src; Wrap = wraps }
            | Some name ->
                let idx = slots.FindIndex(fun s -> bindingName s.Text = Some name)
                if idx < 0 then slots.Add { Cell = k; Text = src; Wrap = wraps }
                else
                    // A rebind, placed by DeclarationCandidate's rule: in place,
                    // unless the new text leans on a name bound LATER, in which
                    // case it moves just past its last dependency -- and only
                    // when nothing in between still references THIS name, where
                    // the dependency is circular in a flat file and in place
                    // (with its honest error) stands.
                    let refs = referencedNames src
                    let lastDep =
                        [ idx + 1 .. slots.Count - 1 ]
                        |> List.filter (fun j ->
                            match bindingName slots.[j].Text with
                            | Some n -> Set.contains n refs
                            | None -> false)
                        |> List.tryLast
                    let displaced = slots.[idx].Cell
                    let at =
                        match lastDep with
                        | Some j when [ idx + 1 .. j ] |> List.forall (fun m ->
                                          not (Set.contains name (referencedNames slots.[m].Text))) ->
                            slots.RemoveAt idx
                            // The removal shifted everything after `idx` down
                            // one, so inserting AT `j` lands immediately after
                            // the dependency.
                            slots.Insert(j, { Cell = k; Text = src; Wrap = wraps })
                            j
                        | _ ->
                            slots.[idx] <- { Cell = k; Text = src; Wrap = wraps }
                            idx
                    // The superseded cell still owes the client a window, and
                    // windows may not overlap: an empty slot directly above the
                    // winner gives it one blank line of its own, which no
                    // payload position can land inside.
                    slots.Insert(at, { Cell = displaced; Text = ""; Wrap = [] })
        else
            // Nothing declaration-shaped opens this cell, so it appends: a
            // reassignment, a bare expression, or several of either -- all of
            // them already wrapped above. Blank or comments only lands here too
            // and wraps nothing, which is right: an unwrapped comment parses.
            //
            // The wrappers are named per cell, so they are unique by
            // construction and need none of the in-use scan `it` and `__assign`
            // require: those are committed to a live session, and this assembly
            // commits nothing.
            slots.Add { Cell = k; Text = src; Wrap = wraps })
    let source = String.concat "\n" (slots |> Seq.map _.Text) + "\n"
    // One walk converts each slot into the absolute range it occupies: the join
    // contributes exactly one newline between neighbours, so the next slot opens
    // on the line after this one closes.
    let byCell = System.Collections.Generic.Dictionary<int, Blade.Ide.CellWindow>()
    let mutable line = 1
    for s in slots do
        // Copied out of the mutable cursor: the wrapper's absolute line is
        // computed inside a closure, which cannot capture a mutable local.
        let startLine = line
        let endLine = startLine + lineCount s.Text - 1
        // `CellWindow` carries ONE wrap pair, so a cell with several wrapped
        // expressions reports its first. The client uses the pair to shift
        // columns back on the wrapped line; a later wrapped line in the same
        // cell therefore reads its columns un-shifted, which is a cosmetic
        // offset -- and strictly better than the alternative this replaced,
        // where the cell did not parse at all and the whole notebook's payload
        // was one BL1999.
        let w : Blade.Ide.CellWindow =
            { StartLine = startLine
              EndLine = endLine
              WrapLine = s.Wrap |> List.tryHead |> Option.map (fun (li, _) -> startLine + li)
              WrapCol = s.Wrap |> List.tryHead |> Option.map snd }
        byCell.[s.Cell] <- w
        line <- endLine + 1
    // Back into CELL order, one window each: every cell owns exactly one slot by
    // construction (a superseded one keeps its empty placeholder), so the lookup
    // cannot miss -- and if it ever did, a 1:1 window is a better answer on the
    // wire than an exception.
    let unplaced : Blade.Ide.CellWindow =
        { StartLine = 1; EndLine = 1; WrapLine = None; WrapCol = None }
    let windows =
        cells
        |> List.mapi (fun k _ ->
            match byCell.TryGetValue k with
            | true, w -> w
            | _ -> unplaced)
    (source, windows)
