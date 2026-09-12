// Parser infrastructure: ParseResult/ParseState, token helpers, span/expr
// builders, the operator active patterns, combinators, and the two forward-
// reference cells (parseExprRef/parseBodyRef) the later parser files wire up
// in Parser.fs -- the seam that keeps the type grammar out of the expression
// rec-chain. Also the shared atoms (literals, rank exprs) both grammars use.
module Blade.ParserCore

open Blade.Ast
open Blade.Lexer

type ParseError = {
    Message: string
    Line: int
    Col: int
    // Exclusive end of the offending span (defaults to a point at Line/Col for
    // legacy call sites). Stable BLxxxx code for classification: BL1001
    // expected-token, BL1002 unexpected-EOF, BL1999 generic parse error.
    EndLine: int
    EndCol: int
    Code: string
}

type ParseResult<'T> = Result<'T * Token list, ParseError>

// Parser-wide mutable state (per-parse)
//
// PER-THREAD, not per-process. These fields describe ONE token stream, and
// several parses can be genuinely in flight at once: the corpus harness maps
// tests with `Array.Parallel.mapi`, and under `BLADE_TEST_FSHARP_PARALLEL=1`
// (tests/Runner.fs) the front end runs without the pipeline lock. Sharing them
// was an observed crash, not a hypothesis -- `setEofFrom` publishes the token
// COUNT and the two tables it describes as separate writes, so a concurrent
// `consumedEnd` on another parse could clear the bounds guard against the new
// count and then index the old, shorter `LastMeaning`:
//
//   System.IndexOutOfRangeException: Index was outside the bounds of the array.
//      at Blade.Parser.consumedEnd ... at Blade.Parser.rangeSpan ...
//
// which `Array.Parallel.mapi` wrapped in an AggregateException and Cli.dispatch
// rendered as `error[BL9001]: internal compiler error: One or more errors
// occurred. (Index was outside the bounds of the array.)`, killing the whole
// category run. Reproduced on `blade test loops`, ~1 run in 6. `Span.File`
// (the `File` field) had the same shape with a quieter symptom: one parse could
// stamp another's spans with its filename.
//
// Per-THREAD is exactly per-parse here: a parse runs synchronously from entry
// to return (no async, no nested parallelism inside the parser), and each
// harness test gets its own thread via Runtime.runOnLargeStack.
//
// `[<ThreadStatic>]` rather than the `AsyncLocal` the rest of the compiler uses
// for per-flow channels: those are touched a handful of times per compile,
// while this state is read at EVERY AST node (`rangeSpan` -> `consumedEnd`).
// A thread-static field read is a couple of instructions; an AsyncLocal read is
// a lookup in the execution context's value map, which would hand back the very
// win these O(1) tables exist to deliver (docs/plan-compile-speed.md Stage 3).
[<AllowNullLiteral>]
type internal ParseState() =
    /// The source file currently being parsed. Set by parseMultiSource /
    /// parseProgramWithFile before parsing each file and reset afterwards;
    /// every span the parser constructs stamps this into Span.File.
    member val File : string option = None with get, set
    /// End position (line, col) of the input, used to report EOF errors at the
    /// end of the last token instead of 0:0. Refreshed per file when tokenizing.
    member val LastEnd : int * int = (0, 0) with get, set
    /// Span tables for the token stream currently being parsed, rebuilt per
    /// file by `setEofFrom`. Indexed by `Token.Index` (the LEXER's numbering,
    /// which still counts the newlines `tokenizeWithNewlines` dropped -- absent
    /// tokens simply never appear as "meaningful").
    ///
    ///   EndAt.[i]        end position (line, col) of the token with index i
    ///   LastMeaning.[i]  index of the last non-terminator token with index < i,
    ///                    or -1; length is IndexCount + 1, so entry
    ///                    [IndexCount] answers "everything was consumed".
    ///
    /// They exist to make `consumedEnd` O(1): computing it from list lengths
    /// cost O(tokens remaining) at every AST node, i.e. O(n^2) over a file.
    member val EndAt : struct (int * int)[] = [||] with get, set
    member val LastMeaning : int[] = [||] with get, set
    member val IndexCount : int = 0 with get, set
    /// Bare top-level expression statements are desugared to a let over a
    /// synthesized name (see `parseDecl`); this numbers them so each name is
    /// unique AND predictable enough to pin with `// EXPECT: __expr1 = ...`.
    /// Reset by `setEofFrom`, which every module/file entry point calls exactly
    /// once before consuming a token, so numbering restarts per module rather
    /// than accumulating across the files of a multi-source program. Lives here
    /// rather than in a module-level `let mutable` for the same reason as the
    /// rest of this state: concurrent parses would otherwise interleave their
    /// numbering and hand two files the same `__exprN`.
    member val TopExprCounter : int = 0 with get, set

/// This thread's `ParseState`, created on first touch. A thread that never
/// parses never allocates one.
[<AbstractClass; Sealed>]
type internal PS =
    [<System.ThreadStatic; DefaultValue>]
    static val mutable private cur : ParseState
    static member Cur : ParseState =
        if isNull PS.cur then PS.cur <- ParseState()
        PS.cur

let internal setEofFrom (tokens: Token list) =
    let st = PS.Cur
    st.TopExprCounter <- 0
    match List.tryLast tokens with
    | Some t -> st.LastEnd <- (t.EndLine, t.EndCol)
    | None -> st.LastEnd <- (0, 0)
    // Rebuild the span tables for exactly the list the parser will consume.
    let n = match List.tryLast tokens with Some t -> t.Index + 1 | None -> 0
    st.IndexCount <- n
    let ends : struct (int * int)[] = Array.zeroCreate n
    let meaningful : bool[] = Array.zeroCreate n
    for t in tokens do
        if t.Index >= 0 && t.Index < n then
            ends.[t.Index] <- struct (t.EndLine, t.EndCol)
            meaningful.[t.Index] <- (match t.Kind with TokNewline | TokSemi -> false | _ -> true)
    let last : int[] = Array.zeroCreate (n + 1)
    last.[0] <- -1
    for i in 0 .. n - 1 do
        last.[i + 1] <- (if meaningful.[i] then i else last.[i])
    st.EndAt <- ends
    st.LastMeaning <- last

// Basic Combinators

let success value remaining : ParseResult<'T> = Ok (value, remaining)

/// Generic parse error (BL1999), end defaults to a point at line:col.
let error msg line col : ParseResult<'T> =
    Error { Message = msg; Line = line; Col = col; EndLine = line; EndCol = col; Code = "BL1999" }

/// Coded parse error with a caller-supplied BLxxxx code; point end at line:col.
let errorC code msg line col : ParseResult<'T> =
    Error { Message = msg; Line = line; Col = col; EndLine = line; EndCol = col; Code = code }

/// Coded parse error with an explicit end span.
let errorFull code msg line col endLine endCol : ParseResult<'T> =
    Error { Message = msg; Line = line; Col = col; EndLine = endLine; EndCol = endCol; Code = code }

/// ParseError -> unified Diagnostic. The file is supplied by the parse
/// entry point (the error record does not carry it).
let diagnosticOfParseError (file: string option) (e: ParseError) : Blade.Diagnostics.Diagnostic =
    let span : Span =
        { StartLine = e.Line; StartCol = e.Col
          EndLine = e.EndLine; EndCol = e.EndCol; File = file }
    Blade.Diagnostics.mkError e.Code Blade.Diagnostics.PhParse span e.Message

/// Unexpected-EOF error (BL1002) reported at the end of the last token.
let errorEof msg : ParseResult<'T> =
    let (l, c) = PS.Cur.LastEnd
    Error { Message = msg; Line = l; Col = c; EndLine = l; EndCol = c; Code = "BL1002" }

/// Human-readable rendering of a token kind for error messages, e.g.
///   keyword 'let'   '('   identifier 'foo'   integer literal 42   end of file
/// Avoids leaking raw DU constructor names (TokLParen, TokKeyword, etc).
let private keywordText =
    // Reverse of Lexer.keywords (a Map string->Keyword): canonical spelling per keyword.
    keywords |> Map.toList |> List.map (fun (s, k) -> (k, s)) |> Map.ofList
let describeToken (kind: TokenKind) : string =
    match kind with
    | TokInt v -> $"integer literal {v}"
    | TokFloat v -> sprintf "float literal %g" v
    | TokString s -> $"string literal \"{s}\""
    | TokChar c -> $"character literal '{c}'"
    | TokBool b -> sprintf "boolean literal %b" b
    | TokIdent n -> $"identifier '{n}'"
    | TokKeyword kw ->
        match Map.tryFind kw keywordText with
        | Some s -> $"keyword '{s}'"
        | None -> sprintf "keyword %A" kw
    | TokOp s -> $"'{s}'"
    | TokNamedInfix s -> $"operator ':{s}:'"
    | TokLParen -> "'('"
    | TokRParen -> "')'"
    | TokLBracket -> "'['"
    | TokRBracket -> "']'"
    | TokLBrace -> "'{'"
    | TokRBrace -> "'}'"
    | TokComma -> "','"
    | TokSemi -> "';'"
    | TokColon -> "':'"
    | TokColonColon -> "'::'"
    | TokDot -> "'.'"
    | TokDotDot -> "'..'"
    | TokPipe -> "'|'"
    | TokUnderscore -> "'_'"
    | TokAt -> "'@'"
    | TokHash -> "'#'"
    | TokQuestion -> "'?'"
    | TokNewline -> "end of line"
    | TokEOF -> "end of file"
    | TokError s -> $"invalid token ({s})"

let currentPos (tokens: Token list) =
    match tokens with
    | t :: _ -> t.Line, t.Col
    | [] -> PS.Cur.LastEnd

/// End position (line, col) of the last meaningful (non-terminator) token
/// consumed advancing from `before` to `after` (`after` must be a suffix of
/// `before`). Newline/semi terminators are excluded so the span stops at the
/// statement's real end rather than overshooting to the next token's start.
/// Falls back to the given start position when nothing was consumed.
///
/// O(1): `before` and `after` are suffixes of one token stream, so the tokens
/// consumed are exactly the index range [before.Head.Index, after.Head.Index)
/// and the answer is a lookup in the tables `setEofFrom` built. The slow
/// list-walking form is kept as a fallback for a token list the tables do not
/// describe (indices out of range -- a caller that parsed without going
/// through an entry point), so the result is identical either way.
let private consumedEndSlow (before: Token list) (after: Token list) (fallbackLine: int) (fallbackCol: int) : int * int =
    let n = List.length before - List.length after
    if n <= 0 then (fallbackLine, fallbackCol)
    else
        let meaningful =
            before
            |> List.truncate n
            |> List.filter (fun t -> match t.Kind with TokNewline | TokSemi -> false | _ -> true)
        match List.tryLast meaningful with
        | Some t -> (t.EndLine, t.EndCol)
        | None -> (fallbackLine, fallbackCol)

let consumedEnd (before: Token list) (after: Token list) (fallbackLine: int) (fallbackCol: int) : int * int =
    match before with
    | [] -> (fallbackLine, fallbackCol)   // nothing to consume: n <= 0
    | b :: _ ->
        let st = PS.Cur
        let bi = b.Index
        // Everything consumed (`after` empty) reads as "one past the last token".
        let ai = match after with a :: _ -> a.Index | [] -> st.IndexCount
        if bi < 0 || bi >= st.IndexCount || ai < 0 || ai > st.IndexCount then
            consumedEndSlow before after fallbackLine fallbackCol
        elif ai <= bi then (fallbackLine, fallbackCol)
        else
            let j = st.LastMeaning.[ai]
            if j >= bi then
                let struct (l, c) = st.EndAt.[j]
                (l, c)
            else (fallbackLine, fallbackCol)

/// Build a Span from a single token, stamped with the current file.
let spanOfToken (t: Token) : Span =
    { StartLine = t.Line; StartCol = t.Col; EndLine = t.EndLine; EndCol = t.EndCol; File = PS.Cur.File }

/// Span of the head token of `tokens` (single-token productions: ExprVar,
/// ExprLit, PatVar, keyword atoms). noSpan when the list is empty (unreachable
/// for the call sites, which already matched a `Some` head).
let internal headSpan (tokens: Token list) : Span =
    match tokens with
    | t :: _ -> spanOfToken t
    | [] -> noSpan

/// Real span for a multi-token production: from the first token of `startToks`
/// to the last meaningful token consumed reaching `remaining`. The natural
/// range for delimited/keyword-led forms (calls, blocks, formers).
let internal rangeSpan (startToks: Token list) (remaining: Token list) : Span =
    let sL, sC = currentPos startToks
    let eL, eC = consumedEnd startToks remaining sL sC
    { StartLine = sL; StartCol = sC; EndLine = eL; EndCol = eC; File = PS.Cur.File }

/// Build an Expr whose span covers the production from `startToks` to `remaining`.
let internal mkE (startToks: Token list) (remaining: Token list) (kind: ExprKind) : Expr =
    mkExpr (rangeSpan startToks remaining) kind

/// Build a Pattern whose span covers the production from `startToks` to `remaining`.
let internal mkP (startToks: Token list) (remaining: Token list) (kind: PatternKind) : Pattern =
    mkPat (rangeSpan startToks remaining) kind

/// Expected-token error: `expected` is the wanted kind, `tokens` the stream
/// (head = actual token, empty = EOF). Humanized message + BL1001, except
/// "got end of file" which is classified BL1002.
let expectedError (expected: TokenKind) (tokens: Token list) : ParseResult<'T> =
    let msg actual = $"Expected {describeToken expected} but got {describeToken actual}"
    match tokens with
    | t :: _ when t.Kind = TokEOF -> errorFull "BL1002" (msg TokEOF) t.Line t.Col t.EndLine t.EndCol
    | t :: _ -> errorFull "BL1001" (msg t.Kind) t.Line t.Col t.EndLine t.EndCol
    | [] ->
        let (l, c) = PS.Cur.LastEnd
        errorFull "BL1002" (msg TokEOF) l c l c

let peek (tokens: Token list) =
    match tokens with
    | t :: _ -> Some t.Kind
    | [] -> None

/// Peek, skipping any leading newlines
let advance (tokens: Token list) =
    match tokens with
    | _ :: rest -> rest
    | [] -> []

/// Skip all leading newlines
let rec skipNL (tokens: Token list) =
    match tokens with
    | t :: rest when t.Kind = TokNewline -> skipNL rest
    | _ -> tokens

/// Is this token an infix combinator operator that can never start a statement?
/// Used for implicit line continuation: if a line starts with one of these,
/// it's a continuation of the previous expression.
let isCombinatorOp (kind: TokenKind) : bool =
    match kind with
    | TokOp "|>" | TokOp "|@>"
    | TokOp "<@>" | TokOp "<$>"
    | TokOp "<&>" | TokOp "<&!>"
    | TokOp "<|>" | TokOp "<|:>"
    | TokOp ">>=" | TokOp ">>@" | TokOp "@>>" | TokOp ">>"
    | TokOp "<*>" -> true
    | _ -> false

/// Consume `.segment` runs after a leading identifier in TYPE position,
/// joining them into one dotted name. The only qualified type paths today are
/// the type-provider axis types (`store.index.y`), resolved through the
/// ordinary TypeDefs lookup under that joined key (the same trick StaticEval
/// uses to key qualified statics, "Module.name"). `..` lexes as its own
/// TokDotDot, so a range can never be swallowed here.
let rec parseDottedTypeName (name: string) (tokens: Token list) : string * Token list =
    match peek tokens with
    | Some TokDot ->
        let afterDot = advance tokens
        match peek afterDot with
        | Some (TokIdent seg) -> parseDottedTypeName (name + "." + seg) (advance afterDot)
        | _ -> (name, tokens)
    | _ -> (name, tokens)

let peekContinuation (tokens: Token list) : TokenKind option * Token list =
    let skipped = skipNL tokens
    match skipped with
    | t :: _ when isCombinatorOp t.Kind -> (Some t.Kind, skipped)
    | _ -> (peek tokens, tokens)

let expect kind (tokens: Token list) : ParseResult<Token> =
    match tokens with
    | t :: rest when t.Kind = kind -> Ok (t, rest)
    | _ -> expectedError kind tokens

let expectIdent (tokens: Token list) : ParseResult<string> =
    match tokens with
    | t :: rest ->
        match t.Kind with
        | TokIdent name -> Ok (name, rest)
        | TokEOF -> errorFull "BL1002" $"Expected identifier but got {describeToken TokEOF}" t.Line t.Col t.EndLine t.EndCol
        | _ -> errorFull "BL1001" $"Expected identifier but got {describeToken t.Kind}" t.Line t.Col t.EndLine t.EndCol
    | [] -> errorEof "Expected identifier but got end of file"

/// Expect a closing > for type parameters. Handles >> by splitting: consume
/// one > and leave one > (the standard Rust/Java7+/C# resolution of the
/// >> shift-vs-two-type-closes ambiguity).
let expectGt (tokens: Token list) : ParseResult<unit> =
    match tokens with
    | t :: rest when t.Kind = TokOp ">" ->
        Ok ((), rest)
    | t :: rest when t.Kind = TokOp ">>" ->
        // Split >>: consume first >, leave second > with adjusted position
        let remainingGt = { t with Kind = TokOp ">"; Col = t.Col + 1; Length = 1 }
        Ok ((), remainingGt :: rest)
    | t :: _ when t.Kind = TokEOF -> errorFull "BL1002" $"Expected '>' but got {describeToken TokEOF}" t.Line t.Col t.EndLine t.EndCol
    | t :: _ -> errorFull "BL1001" $"Expected '>' but got {describeToken t.Kind}" t.Line t.Col t.EndLine t.EndCol
    | [] -> errorEof "Expected '>' but got end of file"

// Bind operator for chaining parsers
let (>>=) (result: ParseResult<'a>) (f: 'a -> Token list -> ParseResult<'b>) : ParseResult<'b> =
    match result with
    | Ok (v, rest) -> f v rest
    | Error e -> Error e

// Forward reference for body parser (inline or block)
let parseBodyRef : (Token list -> ParseResult<Expr>) ref = ref (fun _ -> Error { Message = "Not initialized"; Line = 0; Col = 0; EndLine = 0; EndCol = 0; Code = "BL1999" })
let parseBody tokens = !parseBodyRef tokens

// Active Patterns for Token Classification (sorted by precedence). The
// (mode, op) pair returned by most of these is (Elementwise | Outer, BinOp).
let (|LiteralTok|_|) = function
    | TokInt v -> Some (LitInt v)
    | TokFloat v -> Some (LitFloat v)
    | TokBool v -> Some (LitBool v)
    | TokString v -> Some (LitString v)
    | TokChar v -> Some (LitChar v)
    | _ -> None

let (|PipelineOp|_|) = function
    | TokOp "|>" -> true
    | _ -> false

let (|ChoiceOp|_|) = function
    | TokOp "<|>" -> Some OpChoice
    | TokOp "<|:>" -> Some OpFallback
    | _ -> None

let (|ParallelOp|_|) = function
    | TokOp "<&>" -> Some OpParallel
    | TokOp "<&!>" -> Some OpFusion
    | _ -> None

let (|BindOp|_|) = function
    | TokOp ">>=" -> Some OpBind
    | TokOp ">>@" -> Some OpComposeObj
    | TokOp "@>>" -> Some OpComposeMeth
    | _ -> None

let (|ApplyOp|_|) = function
    | TokOp "<@>" -> Some OpApply
    | TokOp "<$>" -> Some OpFunctor
    | _ -> None

let (|ArrayProductOp|_|) = function
    | TokOp "<*>" -> Some OpArrayProd
    | _ -> None

let (|OrOp|_|) = function
    | TokOp "||" -> Some (Elementwise, OpOr)
    | TokOp "[||]" -> Some (Outer, OpOr)
    | _ -> None

let (|AndOp|_|) = function
    | TokOp "&&" -> Some (Elementwise, OpAnd)
    | TokOp "[&&]" -> Some (Outer, OpAnd)
    | _ -> None

let (|EqualityOp|_|) = function
    | TokOp "==" -> Some (Elementwise, OpEq)
    | TokOp "!=" -> Some (Elementwise, OpNeq)
    | TokOp "[==]" -> Some (Outer, OpEq)
    | TokOp "[!=]" -> Some (Outer, OpNeq)
    | _ -> None

let (|ComparisonOp|_|) = function
    | TokOp "<" -> Some (Elementwise, OpLt)
    | TokOp "<=" -> Some (Elementwise, OpLe)
    | TokOp ">" -> Some (Elementwise, OpGt)
    | TokOp ">=" -> Some (Elementwise, OpGe)
    | TokOp "[<]" -> Some (Outer, OpLt)
    | TokOp "[<=]" -> Some (Outer, OpLe)
    | TokOp "[>]" -> Some (Outer, OpGt)
    | TokOp "[>=]" -> Some (Outer, OpGe)
    | _ -> None

let (|AdditiveOp|_|) = function
    | TokOp "+" -> Some (Elementwise, OpAdd)
    | TokOp "-" -> Some (Elementwise, OpSub)
    | TokOp "[+]" -> Some (Outer, OpAdd)
    | TokOp "[-]" -> Some (Outer, OpSub)
    | _ -> None

let (|MultiplicativeOp|_|) = function
    | TokOp "*" -> Some (Elementwise, OpMul)
    | TokOp "/" -> Some (Elementwise, OpDiv)
    | TokOp "%" -> Some (Elementwise, OpMod)
    | TokOp "[*]" -> Some (Outer, OpMul)
    | TokOp "[/]" -> Some (Outer, OpDiv)
    | TokOp "[%]" -> Some (Outer, OpMod)
    | _ -> None

let (|PowerOp|_|) = function
    | TokOp "^" -> Some (Elementwise, OpCaret)
    | TokOp "[^]" -> Some (Outer, OpCaret)
    | _ -> None

let (|UnaryOp|_|) = function
    | TokOp "-" -> Some OpNeg
    | TokOp "!" -> Some OpNot
    | _ -> None

// Helper Combinators

let optional parser (tokens: Token list) =
    match parser tokens with
    | Ok (v, rest) -> Ok (Some v, rest)
    | Error _ -> Ok (None, tokens)

let many parser (tokens: Token list) =
    let rec loop acc toks =
        match parser toks with
        | Ok (v, rest) -> loop (v :: acc) rest
        | Error _ -> Ok (List.rev acc, toks)
    loop [] tokens

/// Errors that `sepBy` (and the let-annotation slot) must PROPAGATE rather
/// than treat as "the list ends here". A code qualifies only if it can be
/// raised exclusively after the parse has committed -- BL1004 fires only once
/// the two-token lookahead `Tuple <` has matched in TYPE position, so there
/// is no legitimate backtrack it could be cutting off. Without this the
/// precise diagnostic is swallowed and the caller re-reports far away
/// ("Expected ')' but got identifier 't'" at the parameter NAME).
/// BL1003 qualifies for the same reason: every site that raises it is a
/// REMOVED/RELOCATED-CONSTRUCT steer (the imperative `for`, `if` on a
/// recursive-array arm, `while` on an ordinary one), fired only after the
/// distinctive token sequence has already been consumed. There is no
/// production that could still succeed from there, so a swallowed BL1003 is
/// always a curated message traded for a generic one.
let isHardParseError (e: ParseError) : bool = e.Code = "BL1004" || e.Code = "BL1003"

let sepBy parser sep (tokens: Token list) =
    match parser tokens with
    | Error e when isHardParseError e -> Error e
    | Error _ -> Ok ([], tokens)
    | Ok (first, rest) ->
        let rec loop acc toks =
            match expect sep toks with
            | Error _ -> Ok (List.rev acc, toks)
            | Ok (_, afterSep) ->
                match parser afterSep with
                | Ok (v, rest') -> loop (v :: acc) rest'
                | Error e when isHardParseError e -> Error e
                | Error _ -> Ok (List.rev acc, toks)
        loop [first] rest

// Forward Reference for Expression Parser

// The only forward reference we need - expressions can be nested
let parseExprRef : (Token list -> ParseResult<Expr>) ref = 
    ref (fun _ -> failwith "parseExpr not initialized")

let parseExpr tokens = !parseExprRef tokens

// Simple expression parser for type arguments (stops at > and ,); multiplicative binds tighter than additive.

/// For-in range header context. A `for x in RANGE { body }` statement's range
/// expression is always followed by the loop-body brace, so a bare identifier
/// meeting `{` there is the start of the body, not a struct literal -- without
/// this, `for k in 0..n {` misparses as struct construction `n { ... }`. The
/// flag suppresses struct-literal parsing at the top nesting level of the
/// range expression only; delimited sub-expressions (parens, call arguments,
/// index brackets) restore it, so `for p in f(Point { x = 1 })` stays
/// expressible. AsyncLocal mirrors the compiler's other context cells
/// (parse-parallel safety).
let internal noStructLiteralCtx = new System.Threading.AsyncLocal<bool>()

/// Run a sub-parse with struct literals re-enabled (delimited context).
let internal allowingStructLiterals (parse: unit -> ParseResult<'a>) : ParseResult<'a> =
    let saved = noStructLiteralCtx.Value
    noStructLiteralCtx.Value <- false
    let r = parse ()
    noStructLiteralCtx.Value <- saved
    r

/// Run a sub-parse with struct literals suppressed (for-in range header).
let internal suppressingStructLiterals (parse: unit -> ParseResult<'a>) : ParseResult<'a> =
    let saved = noStructLiteralCtx.Value
    noStructLiteralCtx.Value <- true
    let r = parse ()
    noStructLiteralCtx.Value <- saved
    r

let rec parseSimpleExpr (tokens: Token list) : ParseResult<Expr> =
    parseSimpleAdditive tokens

and parseSimpleAdditive (tokens: Token list) : ParseResult<Expr> =
    parseSimpleMultiplicative tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "+") ->
            advance toks |> parseSimpleMultiplicative >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpAdd, acc, right))) remaining
        | Some (TokOp "-") ->
            advance toks |> parseSimpleMultiplicative >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpSub, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseSimpleMultiplicative (tokens: Token list) : ParseResult<Expr> =
    parseSimplePrimary tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "*") ->
            advance toks |> parseSimplePrimary >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpMul, acc, right))) remaining
        | Some (TokOp "/") ->
            advance toks |> parseSimplePrimary >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpDiv, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseSimplePrimary (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some (TokOp "-") ->
        // Unary minus on a simple operand: needed for signed static payloads
        // such as halo<I, [-1, 0, 1]> (and negative EnumIdx keys). Binds
        // tighter than the additive/multiplicative loops, matching normal
        // unary-minus precedence; folds via StaticEval's OpNeg arm.
        advance tokens |> parseSimplePrimary >>= fun operand remaining ->
        success (mkExpr (mergeSpan (headSpan tokens) operand.Span) (ExprUnaryOp (OpNeg, operand))) remaining
    | Some (LiteralTok lit) -> success (mkExpr (headSpan tokens) (ExprLit lit)) (advance tokens)
    | Some (TokIdent name) -> success (mkExpr (headSpan tokens) (ExprVar name)) (advance tokens)
    | Some (TokKeyword KwArity) ->
        // `arity(p)` in a static payload position, e.g. `Idx<arity(args)>`:
        // the poly-pack former's extent (formalism 7.4). Resolves to a
        // literal at pack monomorphization (Ir.specializeFunction's IRArity rewrite).
        let afterArity = advance tokens
        expect TokLParen afterArity >>= fun _ afterLParen ->
        (match peek afterLParen with
         | Some (TokIdent packName) -> success packName (advance afterLParen)
         | _ ->
            let line, col = currentPos afterLParen
            error "Expected a pack parameter name in arity(...)" line col)
        >>= fun packName afterName ->
        expect TokRParen afterName >>= fun _ remaining ->
        success (mkE tokens remaining (ExprArity packName)) remaining
    | Some TokLParen ->
        // Parenthesized simple expression OR tuple of simple expressions --
        // the tuple form serves static index-type arguments whose payload is
        // tuple-structured (e.g. IrrepsIdx<[(0, 0, 2), (1, 1, 2)]>).
        advance tokens |> sepBy parseSimpleExpr TokComma >>= fun exprs afterExprs ->
        expect TokRParen afterExprs >>= fun _ remaining ->
        match exprs with
        | [] ->
            let line, col = currentPos tokens
            error "Expected simple expression inside parentheses" line col
        | [single] -> success single remaining
        | many -> success (mkE tokens remaining (ExprTuple many)) remaining
    | Some TokLBracket ->
        // Array literal inside simple expression context (e.g., EnumIdx<[1, 2, 3]>)
        advance tokens |> sepBy parseSimpleExpr TokComma >>= fun elems afterElems ->
        expect TokRBracket afterElems >>= fun _ remaining ->
        success (mkE tokens remaining (ExprArrayLit elems)) remaining
    | Some kind ->
        let line, col = currentPos tokens
        error $"Expected simple expression but got {describeToken kind}" line col
    | None ->
        errorEof "Expected expression but got end of file"

// Literal Parsing

let parseLiteral (tokens: Token list) : ParseResult<Literal> =
    match tokens with
    | t :: rest ->
        match t.Kind with
        | TokInt v -> success (LitInt v) rest
        | TokFloat v -> success (LitFloat v) rest
        | TokBool v -> success (LitBool v) rest
        | TokString v -> success (LitString v) rest
        | TokChar v -> success (LitChar v) rest
        | _ -> error "Expected literal" t.Line t.Col
    | [] -> errorEof "Expected literal but got end of file"

// Type Expression Parsing

/// Parse a rank expression (for T^r syntax)
/// Can be: integer literal, arity keyword, or simple identifier
let parseRankExpr (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some (TokInt n) ->
        success (mkExpr (headSpan tokens) (ExprLit (LitInt n))) (advance tokens)
    | Some (TokKeyword KwArity) ->
        let afterArity = advance tokens
        match peek afterArity with
        | Some TokLParen ->
            advance afterArity |> fun afterLParen ->
            match peek afterLParen with
            | Some (TokIdent paramName) ->
                advance afterLParen |> expect TokRParen >>= fun _ remaining ->
                success (mkE tokens remaining (ExprArity paramName)) remaining
            | _ ->
                let line, col = currentPos afterLParen
                error "Expected parameter name in arity()" line col
        | _ ->
            let line, col = currentPos afterArity
            error "arity requires parameter name: arity(paramName)" line col
    | Some (TokIdent name) ->
        success (mkExpr (headSpan tokens) (ExprVar name)) (advance tokens)
    | Some t ->
        let line, col = currentPos tokens
        error $"Expected rank expression (integer, arity, or identifier), got {describeToken t}" line col
    | None ->
        errorEof "Expected rank expression but got end of file"

/// Two-token lookahead for the tag wildcard `Base<_>`: TokUnderscore
/// immediately followed by `>` or `>>` (expectGt splits the latter). `_`
/// means "any tag" only when it is the sole type argument (same discipline
/// as `RaggedIdx<_>` below), so `_ + 1`, `_, x` etc. go to the ordinary
/// parsers. `tokens` is positioned just after the opening `<`.
let isTagWildcardArg (tokens: Token list) : bool =
    match tokens with
    | t1 :: t2 :: _ when
        t1.Kind = TokUnderscore &&
        (t2.Kind = TokOp ">" || t2.Kind = TokOp ">>") -> true
    | _ -> false

/// One argument of a type application `Base<...>`: an ordinary positional
/// type argument (a unit name, an index-type name, a type), or a named bound
/// argument `min=e` / `max=e` (formalism 2.4's bounded primitives).
type TypeArg =
    | TAPositional of TypeExpr
    | TANamed of name: string * value: Expr

/// Two-token lookahead for a named type argument: an identifier immediately
/// followed by `=`. At HEAD this shape is a hard parse error (the identifier
/// parses as a type name and the `=` then fails the `,`-or-`>` expectation),
/// so recognizing it is a strict widening of the grammar.
let isNamedTypeArg (tokens: Token list) : bool =
    match tokens with
    | t1 :: t2 :: _ ->
        t1.Kind.IsTokIdent && t2.Kind = TokOp "="
    | _ -> false

// Unit-expression grammar (shared by Unit-declaration right-hand sides and
// compound type-argument annotations like `Float<meter/second^2>`). Defined
// HERE, ahead of the parseTypeExpr group, because parseTypeArg routes into
// it; parseUnitDecl (further down) calls it too. A unit expression never
// contains `,` or `>`, so a sub-parse inside a type-argument list always
// stops cleanly before the enclosing constructor's delimiters.

/// Recover the EXACT rational a decimal literal denotes, from its shortest
/// round-trip spelling rather than from the binary double it parsed to:
/// `0.0254` is 254/10000, not the dyadic fraction that literal lands on.
/// "R" round-trips, so the digits recovered are exactly the ones that could
/// have been written. None for a non-finite or zero factor (caller rejects).
let rationalOfFloatLiteral (v: float) : (bigint * bigint) option =
    if System.Double.IsNaN v || System.Double.IsInfinity v || v = 0.0 then None
    else
        let s = v.ToString ("R", System.Globalization.CultureInfo.InvariantCulture)
        // Split off an exponent suffix first ("1.5E-05"), then the point.
        let mantissa, exp10 =
            match s.IndexOfAny [| 'e'; 'E' |] with
            | -1 -> s, 0
            | i -> s.Substring (0, i), int (s.Substring (i + 1))
        let neg = mantissa.StartsWith "-"
        let mantissa = if neg then mantissa.Substring 1 else mantissa
        let intPart, fracPart =
            match mantissa.IndexOf '.' with
            | -1 -> mantissa, ""
            | i -> mantissa.Substring (0, i), mantissa.Substring (i + 1)
        match System.Numerics.BigInteger.TryParse (intPart + fracPart) with
        | false, _ -> None
        | true, digits ->
            let num = if neg then -digits else digits
            // Net power of ten: fraction digits push down, the exponent
            // suffix pushes either way.
            let p = exp10 - fracPart.Length
            if p >= 0 then Some (num * System.Numerics.BigInteger.Pow (bigint 10, p), bigint 1)
            else Some (num, System.Numerics.BigInteger.Pow (bigint 10, -p))

