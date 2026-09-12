// Blade-DSL Lexer
// Tokenizes Blade source code

module Blade.Lexer

open System
open System.Text

// Tokens

type TokenKind =
    // Literals
    | TokInt of int64
    | TokFloat of float
    | TokString of string
    | TokChar of char
    | TokBool of bool
    // Identifiers and keywords
    | TokIdent of string
    | TokKeyword of Keyword
    // Operators
    | TokOp of string
    | TokNamedInfix of string  // :name: syntax for custom infix operators
    // Punctuation
    | TokLParen        // (
    | TokRParen        // )
    | TokLBracket      // [
    | TokRBracket      // ]
    | TokLBrace        // {
    | TokRBrace        // }
    | TokComma         // ,
    | TokSemi          // ;
    | TokColon         // :
    | TokColonColon    // ::
    | TokDot           // .
    | TokDotDot        // ..

    | TokPipe          // |
    | TokUnderscore    // _
    | TokAt            // @
    | TokHash          // #
    | TokQuestion      // ?
    // Special
    | TokNewline
    | TokEOF
    | TokError of string

and Keyword =
    | KwLet
    | KwRec
    | KwMut
    | KwStatic
    | KwFunction
    | KwLambda
    | KwType
    | KwStruct
    | KwInterface
    | KwImpl
    | KwModule
    | KwFor
    | KwIf
    | KwThen
    | KwElse
    | KwMatch
    | KwWith
    | KwWhere
    | KwAnd
    | KwComm
    | KwAntisymm
    | KwOmp
    | KwCuda
    | KwMpi
    | KwReynolds
    | KwTrue
    | KwFalse
    | KwIn
    | KwImport
    | KwFrom
    | KwAs
    | KwVoid
    | KwUnit
    | KwArray
    | KwIdx
    | KwSymIdx
    | KwAntisymIdx
    | KwHermitianIdx
    | KwCompoundIdx
    | KwChunked
    | KwSparseIdx
    | KwOrbIdx
    | KwEnumIdx
    | KwDepIdx
    | KwRaggedIdx
    | KwIrrepsIdx
    | KwPgIrrepsIdx
    | KwTreeIdx
    | KwLeafIdx
    | KwNodeIdx
    | KwMethodFor
    | KwObjectFor
    | KwRange
    | KwReverse
    | KwHalo
    | KwTranspose
    | KwHermitian
    | KwGram
    | KwGramApply
    | KwDecompact
    | KwPure
    | KwCompute
    // (`read` is NOT a keyword: provider reads are module-qualified
    // `alias.read(...)`, so `read` must lex as a plain identifier/field.)
    | KwGuard
    | KwSequence
    | KwReplicate
    | KwZip
    | KwStack
    | KwJoin
    | KwArity
    | KwNth
    | KwZero
    | KwRank
    | KwMask
    | KwCompound
    | KwSparse
    | KwIntersect
    | KwUnion
    | KwUnique
    | KwContains
    | KwGroupBy
    | KwGroupKeys
    | KwGroupBucket
    | KwSort
    | KwReduce
    | KwConj
    | KwExtents
    | KwLike
    | KwPoly

type Token = {
    Kind: TokenKind
    Line: int
    Col: int
    Length: int
    // Exclusive end position: the source position immediately AFTER the lexeme
    // (i.e. where the next token's scan begins, before whitespace). For a
    // single-line lexeme EndLine = Line and EndCol = Col + Length; for a
    // multi-line lexeme (e.g. a string literal spanning lines) they track the
    // post-lexeme cursor. Length is retained unchanged for existing consumers.
    EndLine: int
    EndCol: int
    /// Position of this token in the stream the lexer produced, counting from
    /// 0 and INCLUDING the newline tokens a later filter may drop. Monotone
    /// along any sublist the parser holds, which is what lets the parser turn
    /// "what did this production consume" into an array lookup instead of a
    /// list-length subtraction (see Parser.consumedEnd).
    Index: int
}

// Keyword Map

/// Declaration-order source of truth for the keyword vocabulary. Split out of
/// the Map below so the language-surface dump (`blade ide surface`, Ide.fs)
/// can report each word with its DU token name; the lexer itself only ever
/// wants the Map.
let keywordEntries : (string * Keyword) list =
    [ "let", KwLet
      "rec", KwRec
      "mut", KwMut
      "static", KwStatic
      "function", KwFunction
      "lambda", KwLambda
      "type", KwType
      "struct", KwStruct
      "interface", KwInterface
      "impl", KwImpl
      "module", KwModule
      "for", KwFor
      "if", KwIf
      "then", KwThen
      "else", KwElse
      "match", KwMatch
      "with", KwWith
      "where", KwWhere
      "and", KwAnd
      "comm", KwComm
      // `anticomm` is a where-clause conjunct keyword (the anticommutativity
      // pin, signed sibling of `comm`). Named after the property it pins,
      // f(b, a) = -f(a, b), so it cannot collide with the `AntisymIdx` type
      // keyword or the `Antisymmetric` reynolds variant identifier, both of
      // which speak the index-storage sense of "antisymmetric".
      "anticomm", KwAntisymm
      "omp", KwOmp
      "cuda", KwCuda
      "mpi", KwMpi
      "reynolds", KwReynolds
      "true", KwTrue
      "false", KwFalse
      "True", KwTrue
      "False", KwFalse
      "in", KwIn
      "import", KwImport
      "from", KwFrom
      "as", KwAs
      "Void", KwVoid
      "Unit", KwUnit
      "Array", KwArray
      "Idx", KwIdx
      "SymIdx", KwSymIdx
      "AntisymIdx", KwAntisymIdx
      "HermitianIdx", KwHermitianIdx
      "CompoundIdx", KwCompoundIdx
      "Chunked", KwChunked
      "SparseIdx", KwSparseIdx
      "OrbIdx", KwOrbIdx
      "EnumIdx", KwEnumIdx
      "DepIdx", KwDepIdx
      "RaggedIdx", KwRaggedIdx
      "IrrepsIdx", KwIrrepsIdx
      "PgIrrepsIdx", KwPgIrrepsIdx
      "TreeIdx", KwTreeIdx
      "LeafIdx", KwLeafIdx
      "NodeIdx", KwNodeIdx
      "method_for", KwMethodFor
      "object_for", KwObjectFor
      "range", KwRange
      "reverse", KwReverse
      "halo", KwHalo
      "transpose", KwTranspose
      "hermitian", KwHermitian
      "gram", KwGram
      "gram_apply", KwGramApply
      "decompact", KwDecompact
      "pure", KwPure
      "compute", KwCompute
      "guard", KwGuard
      "sequence", KwSequence
      "replicate", KwReplicate
      "zip", KwZip
      "stack", KwStack
      "join", KwJoin
      "arity", KwArity
      "nth", KwNth
      "zero", KwZero
      "rank", KwRank
      "mask", KwMask
      "compound", KwCompound
      "sparse", KwSparse
      "intersect", KwIntersect
      "union", KwUnion
      "unique", KwUnique
      "contains", KwContains
      "group_by", KwGroupBy
      "group_keys", KwGroupKeys
      "group_bucket", KwGroupBucket
      "sort", KwSort
      "reduce", KwReduce
      "conj", KwConj
      "extents", KwExtents
      "like", KwLike
      "Poly", KwPoly ]

let keywords = keywordEntries |> Map.ofList

// Multi-character Operators

/// Declaration-order source of truth for the operator vocabulary (the surface
/// dump reads this one). `operators` below is the same set re-ordered for
/// maximal munch, which is a SCANNING concern, not a vocabulary one.
let operatorEntries : string list =
    [ "<@>"; ">>="; "<&>"; "<&!>"; "<*>"; "<$>"; "<|>"; "<|:>"
      ">>@"; "@>>"; "|@>"; "::"; "->"; "=>"; ".."; "=="
      "!="; "<="; ">="; "&&"; "||"; ">>"
      "+="; "-="; "*="; "/="
      "|>"; "<-"
      "+"; "-"; "*"; "/"; "%"; "="; "<"; ">"; "!"; "^" ]

let operators =
    operatorEntries
    |> List.sortByDescending String.length  // Match longer operators first

// Lexer State

type LexerState = {
    Source: string
    mutable Pos: int
    mutable Line: int
    mutable Col: int
    // Append-only accumulator. A `Token list` here made `emit` an O(n) list
    // append per token, i.e. O(n^2) over a file; nothing reads it before
    // `tokenize` converts it to a list once, so a ResizeArray is a pure win.
    Tokens: ResizeArray<Token>
}

let createLexer source = {
    Source = source
    Pos = 0
    Line = 1
    Col = 1
    Tokens = ResizeArray<Token>()
}

// Character Utilities

let isDigit c = c >= '0' && c <= '9'
let isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let isAlphaNum c = isDigit c || isAlpha c
let isIdentStart c = isAlpha c || c = '_'
let isIdentChar c = isAlphaNum c || c = '_'
let isWhitespace c = c = ' ' || c = '\t' || c = '\r'

let peek (state: LexerState) =
    if state.Pos < state.Source.Length then
        Some state.Source.[state.Pos]
    else
        None

let peekN (state: LexerState) n =
    if state.Pos + n < state.Source.Length then
        Some state.Source.[state.Pos + n]
    else
        None

let peekStr (state: LexerState) len =
    if state.Pos + len <= state.Source.Length then
        state.Source.Substring(state.Pos, len)
    else
        ""

let advance (state: LexerState) =
    if state.Pos < state.Source.Length then
        let c = state.Source.[state.Pos]
        state.Pos <- state.Pos + 1
        if c = '\n' then
            state.Line <- state.Line + 1
            state.Col <- 1
        else
            state.Col <- state.Col + 1
        Some c
    else
        None

let emit (state: LexerState) startLine startCol kind =
    let len = state.Col - startCol
    // state.Line/state.Col sit immediately after the just-consumed lexeme, so
    // they are the natural exclusive end -- correct even for multi-line lexemes.
    // For a zero-width token (EOF), End = Start.
    let tok = { Kind = kind; Line = startLine; Col = startCol; Length = max 1 len
                EndLine = state.Line; EndCol = state.Col; Index = state.Tokens.Count }
    state.Tokens.Add tok

// Token Scanners

let skipWhitespace (state: LexerState) =
    while (match peek state with Some c -> isWhitespace c | None -> false) do
        advance state |> ignore

let skipLineComment (state: LexerState) =
    advance state |> ignore
    advance state |> ignore
    while (match peek state with Some c -> c <> '\n' | None -> false) do
        advance state |> ignore

let skipBlockComment (state: LexerState) =
    advance state |> ignore
    advance state |> ignore
    let mutable depth = 1
    while depth > 0 do
        match peek state, peekN state 1 with
        | Some '/', Some '*' ->
            advance state |> ignore
            advance state |> ignore
            depth <- depth + 1
        | Some '*', Some '/' ->
            advance state |> ignore
            advance state |> ignore
            depth <- depth - 1
        | Some _, _ ->
            advance state |> ignore
        | None, _ ->
            depth <- 0  // EOF, stop

let scanNumber (state: LexerState) =
    let startLine = state.Line
    let startCol = state.Col
    let sb = StringBuilder()
    let mutable isFloat = false
    
    // Integer part
    while (match peek state with Some c -> isDigit c | None -> false) do
        sb.Append(advance state |> Option.get) |> ignore
    
    // Decimal part
    match peek state, peekN state 1 with
    | Some '.', Some c when isDigit c ->
        isFloat <- true
        sb.Append(advance state |> Option.get) |> ignore  // .
        while (match peek state with Some c -> isDigit c | None -> false) do
            sb.Append(advance state |> Option.get) |> ignore
    | _ -> ()
    
    // Exponent
    match peek state with
    | Some 'e' | Some 'E' ->
        isFloat <- true
        sb.Append(advance state |> Option.get) |> ignore
        match peek state with
        | Some '+' | Some '-' ->
            sb.Append(advance state |> Option.get) |> ignore
        | _ -> ()
        while (match peek state with Some c -> isDigit c | None -> false) do
            sb.Append(advance state |> Option.get) |> ignore
    | _ -> ()
    
    let text = sb.ToString()
    let kind =
        if isFloat then
            match Double.TryParse(text) with
            | true, v -> TokFloat v
            | false, _ -> TokError $"Invalid float: {text}"
        else
            match Int64.TryParse(text) with
            | true, v -> TokInt v
            | false, _ -> TokError $"Invalid integer: {text}"
    
    emit state startLine startCol kind

let scanString (state: LexerState) =
    let startLine = state.Line
    let startCol = state.Col
    let sb = StringBuilder()
    
    advance state |> ignore
    
    let mutable escaped = false
    let mutable closed = false
    
    while not closed do
        match peek state with
        | None ->
            emit state startLine startCol (TokError "Unterminated string")
            closed <- true
        | Some '\\' when not escaped ->
            escaped <- true
            advance state |> ignore
        | Some '"' when not escaped ->
            advance state |> ignore
            emit state startLine startCol (TokString (sb.ToString()))
            closed <- true
        | Some c ->
            if escaped then
                let ec = 
                    match c with
                    | 'n' -> '\n'
                    | 't' -> '\t'
                    | 'r' -> '\r'
                    | '\\' -> '\\'
                    | '"' -> '"'
                    | _ -> c
                sb.Append(ec) |> ignore
                escaped <- false
            else
                sb.Append(c) |> ignore
            advance state |> ignore

let scanChar (state: LexerState) =
    let startLine = state.Line
    let startCol = state.Col
    
    advance state |> ignore
    
    let c =
        match peek state with
        | Some '\\' ->
            advance state |> ignore
            match peek state with
            | Some 'n' -> advance state |> ignore; '\n'
            | Some 't' -> advance state |> ignore; '\t'
            | Some 'r' -> advance state |> ignore; '\r'
            | Some '\\' -> advance state |> ignore; '\\'
            | Some '\'' -> advance state |> ignore; '\''
            | Some c -> advance state |> ignore; c
            | None -> '\000'
        | Some c ->
            advance state |> ignore
            c
        | None -> '\000'
    
    match peek state with
    | Some '\'' ->
        advance state |> ignore
        emit state startLine startCol (TokChar c)
    | _ ->
        emit state startLine startCol (TokError "Unterminated character literal")

let scanIdentOrKeyword (state: LexerState) =
    let startLine = state.Line
    let startCol = state.Col
    let sb = StringBuilder()
    
    while (match peek state with Some c -> isIdentChar c | None -> false) do
        sb.Append(advance state |> Option.get) |> ignore
    
    let text = sb.ToString()
    let kind =
        match text with
        // A lone `_` is the wildcard token, not an identifier. The scanToken
        // dispatch reaches this function for `_` (isIdentStart includes '_'),
        // shadowing the dedicated TokUnderscore branch; recover it here so the
        // wildcard consumers (patterns, RaggedIdx<_>, compound-index coordinates)
        // receive TokUnderscore. `_` is never a valid Blade identifier or keyword.
        | "_" -> TokUnderscore
        | _ ->
            match Map.tryFind text keywords with
            | Some kw -> 
                match kw with
                | KwTrue -> TokBool true
                | KwFalse -> TokBool false
                | _ -> TokKeyword kw
            | None -> TokIdent text
    
    emit state startLine startCol kind

let tryOperator (state: LexerState) =
    // Try to match longest operator first
    operators
    |> List.tryFind (fun op ->
        let s = peekStr state op.Length
        s = op)

let scanOperator (state: LexerState) =
    let startLine = state.Line
    let startCol = state.Col
    
    match tryOperator state with
    | Some op ->
        for _ in 1..op.Length do
            advance state |> ignore
        emit state startLine startCol (TokOp op)
    | None ->
        // Single character operator
        let c = advance state |> Option.get
        emit state startLine startCol (TokOp (string c))

// Main Lexer

let scanToken (state: LexerState) =
    skipWhitespace state
    
    let startLine = state.Line
    let startCol = state.Col
    
    match peek state with
    | None ->
        emit state startLine startCol TokEOF
        false
    
    | Some '\n' ->
        advance state |> ignore
        emit state startLine startCol TokNewline
        true
    
    | Some '/' ->
        match peekN state 1 with
        | Some '/' ->
            skipLineComment state
            true
        | Some '*' ->
            skipBlockComment state
            true
        | _ ->
            scanOperator state
            true
    
    | Some '"' ->
        scanString state
        true
    
    | Some '\'' ->
        scanChar state
        true
    
    | Some c when isDigit c ->
        scanNumber state
        true
    
    | Some c when isIdentStart c ->
        scanIdentOrKeyword state
        true
    
    | Some '(' ->
        advance state |> ignore
        emit state startLine startCol TokLParen
        true
    
    | Some ')' ->
        advance state |> ignore
        emit state startLine startCol TokRParen
        true
    
    | Some '[' ->
        // Check for bracketed operators: [op] for outer product mode
        // Supported: arithmetic (+,-,*,/,%,^), comparison (==,!=,<,>,<=,>=), logical (&&,||)
        let tryBracketedOp () =
            // Try two-char ops first
            match peekN state 1, peekN state 2, peekN state 3 with
            | Some '=', Some '=', Some ']' -> Some "[==]"  // equality
            | Some '!', Some '=', Some ']' -> Some "[!=]"  // not equal
            | Some '<', Some '=', Some ']' -> Some "[<=]"  // less equal
            | Some '>', Some '=', Some ']' -> Some "[>=]"  // greater equal
            | Some '&', Some '&', Some ']' -> Some "[&&]"  // logical and
            | Some '|', Some '|', Some ']' -> Some "[||]"  // logical or
            | _ ->
                // Try single-char ops
                match peekN state 1, peekN state 2 with
                | Some '+', Some ']' -> Some "[+]"
                | Some '-', Some ']' -> Some "[-]"
                | Some '*', Some ']' -> Some "[*]"
                | Some '/', Some ']' -> Some "[/]"
                | Some '%', Some ']' -> Some "[%]"
                | Some '^', Some ']' -> Some "[^]"
                | Some '<', Some ']' -> Some "[<]"
                | Some '>', Some ']' -> Some "[>]"
                | _ -> None
        
        match tryBracketedOp () with
        | Some opStr ->
            for _ in 1..opStr.Length do
                advance state |> ignore
            emit state startLine startCol (TokOp opStr)
        | None ->
            advance state |> ignore
            emit state startLine startCol TokLBracket
        true
    
    | Some ']' ->
        advance state |> ignore
        emit state startLine startCol TokRBracket
        true
    
    | Some '{' ->
        advance state |> ignore
        emit state startLine startCol TokLBrace
        true
    
    | Some '}' ->
        advance state |> ignore
        emit state startLine startCol TokRBrace
        true
    
    | Some ',' ->
        advance state |> ignore
        emit state startLine startCol TokComma
        true
    
    | Some ';' ->
        advance state |> ignore
        emit state startLine startCol TokSemi
        true
    
    | Some ':' ->
        match peekN state 1 with
        | Some ':' ->
            advance state |> ignore
            advance state |> ignore
            emit state startLine startCol TokColonColon
        | Some c when Char.IsLetter(c) || c = '_' ->
            // Potential named infix: :name:
            advance state |> ignore  // consume first ':'
            let nameStart = state.Pos
            let colAfterColon = state.Col  // identifiers contain no newlines, so Line is unchanged
            while state.Pos < state.Source.Length &&
                  (let ch = state.Source.[state.Pos] in Char.IsLetterOrDigit(ch) || ch = '_') do
                advance state |> ignore
            let name = state.Source.Substring(nameStart, state.Pos - nameStart)
            match peek state with
            | Some ':' ->
                advance state |> ignore  // consume closing ':'
                emit state startLine startCol (TokNamedInfix name)
            | _ ->
                // Not a named infix (e.g. a type annotation `:Int64`). Backtrack:
                // emit a plain colon and rewind so the identifier is re-lexed.
                ignore name
                state.Pos <- nameStart
                state.Col <- colAfterColon
                emit state startLine startCol TokColon
        | _ ->
            advance state |> ignore
            emit state startLine startCol TokColon
        true
    
    | Some '.' ->
        match peekN state 1 with
        | Some '.' ->
            advance state |> ignore
            advance state |> ignore
            emit state startLine startCol TokDotDot
        | _ ->
            advance state |> ignore
            emit state startLine startCol TokDot
        true
    
    | Some '|' ->
        match peekN state 1 with
        | Some '@' when peekN state 2 = Some '>' ->
            advance state |> ignore  // |
            advance state |> ignore  // @
            advance state |> ignore  // >
            emit state startLine startCol (TokOp "|@>")
        | Some '>' ->
            advance state |> ignore
            advance state |> ignore
            emit state startLine startCol (TokOp "|>")
        | Some '|' ->
            advance state |> ignore
            advance state |> ignore
            emit state startLine startCol (TokOp "||")
        | _ ->
            advance state |> ignore
            emit state startLine startCol TokPipe
        true
    
    | Some '_' ->
        match peekN state 1 with
        | Some c when isIdentChar c ->
            scanIdentOrKeyword state
        | _ ->
            advance state |> ignore
            emit state startLine startCol TokUnderscore
        true
    
    | Some '@' ->
        // Check for @>> operator before treating @ as standalone
        match peekN state 1, peekN state 2 with
        | Some '>', Some '>' ->
            advance state |> ignore  // @
            advance state |> ignore  // >
            advance state |> ignore  // >
            emit state startLine startCol (TokOp "@>>")
        | _ ->
            advance state |> ignore
            emit state startLine startCol TokAt
        true
    
    | Some '#' ->
        advance state |> ignore
        emit state startLine startCol TokHash
        true
    
    | Some '?' ->
        advance state |> ignore
        emit state startLine startCol TokQuestion
        true
    
    | Some _ ->
        scanOperator state
        true

let tokenize source =
    let state = createLexer source
    while scanToken state do ()
    List.ofSeq state.Tokens

/// Filter newlines based on delimiter depth: newlines inside (), [], {} are
/// removed (treated as whitespace); newlines at depth 0 are kept (statement
/// terminators); consecutive newlines collapse to one; leading/trailing
/// newlines around delimiters are removed.
let tokenizeWithNewlines source =
    let tokens = tokenize source
    let mutable depth = 0
    let mutable lastWasNewline = false
    let mutable lastWasOpen = false  // after (, [, {
    
    tokens |> List.filter (fun t ->
        match t.Kind with
        | TokLParen | TokLBracket | TokLBrace ->
            depth <- depth + 1
            lastWasNewline <- false
            lastWasOpen <- true
            true
        | TokRParen | TokRBracket | TokRBrace ->
            depth <- max 0 (depth - 1)
            lastWasNewline <- false
            lastWasOpen <- false
            true
        | TokNewline ->
            if depth > 0 then
                // Inside delimiters - skip newline
                false
            elif lastWasNewline || lastWasOpen then
                // Collapse consecutive newlines, skip after open delimiter
                false
            else
                lastWasNewline <- true
                lastWasOpen <- false
                true
        | TokEOF ->
            // Don't update flags for EOF
            true
        | _ ->
            lastWasNewline <- false
            lastWasOpen <- false
            true)

// Filter out all newlines for simpler parsing.
let tokenizeFiltered source =
    tokenize source
    |> List.filter (fun t -> 
        match t.Kind with 
        | TokNewline -> false 
        | _ -> true)
