// The expression/statement grammar: one rec-chain from parseExprImpl down to
// parseLetStmt (the precedence ladder, primaries, lambda/let/if/match, guard
// sub-ladder, method_for/object_for, blocks, nested functions, rec-array
// bindings). Atomic by mutual recursion -- do not split further.
module Blade.ParserGrammar

open Blade.Ast
open Blade.Lexer
open Blade.ParserCore
open Blade.ParserTypes
open Blade.ParserPatterns

let rec parseExprImpl (tokens: Token list) : ParseResult<Expr> =
    parseAssignment tokens

/// Parse a body expression - either a block {...} or an inline expression
/// Inline expressions stop at newline (consumed) or other terminators
and parseInlineOrBlock (tokens: Token list) : ParseResult<Expr> =
    let tokens = skipNL tokens
    match peek tokens with
    | Some TokLBrace ->
        parseBlock (advance tokens)
    // A braceless body that OPENS with a binding statement. A `let` is not a
    // value, so such a body cannot be one statement long -- it is a let-chain
    // followed by the single expression that is its value. That is most of the
    // termination rule; `parseBracelessBinders` adds the layout guard for the
    // rest. The result is ExprBlock, i.e. literally the braced form's AST, so
    //     | p -> let x = e
    //            f(x)
    // and `| p -> { let x = e; f(x) }` are the same program by construction.
    // Gated on the `let`/`function` opener so every braceless body that parses
    // today keeps its exact historical path.
    | Some (TokKeyword KwLet) | Some (TokKeyword KwFunction) ->
        (match parseBracelessBinders tokens with
         | Some result -> result
         // No value expression belongs to the chain (the arm ended, or the
         // next statement is outdented past this body). Nothing NEW is
         // expressible there, so re-run the historical parse and let it
         // produce the historical AST and diagnostic verbatim.
         | None -> parseInlineExpr tokens)
    | _ ->
        parseInlineExpr tokens

/// The historical braceless body: one expression, with a trailing newline
/// consumed if present. Factored out so the multi-statement path can fall back
/// to it byte-for-byte.
and parseInlineExpr (tokens: Token list) : ParseResult<Expr> =
    parseExprImpl tokens >>= fun expr remaining ->
    let remaining =
        match peek remaining with
        | Some TokNewline -> advance remaining
        | _ -> remaining
    success expr remaining

/// Parse a braceless `let`-chain body: binding statements followed by the one
/// expression that is the body's value, assembled into the same ExprBlock the
/// braced form builds.
///
/// `None` means "this is not that shape after all" -- the caller falls back to
/// the historical single-expression parse. A genuine parse error INSIDE a
/// binder is `Some (Error _)` and propagates, so a typo in a let is still
/// reported at the typo.
///
/// Termination has two rules, and it needs both:
///
///   1. Structural -- keep going while the last statement read was a binding,
///      stop at the first statement that is an expression (that expression is
///      the value). A `let` cannot end a body, so this is forced.
///   2. Layout -- a continuation statement must start at a column no less than
///      the body's first statement. Rule 1 alone is not enough at top level,
///      where a match arm or function body is followed by more DECLARATIONS:
///          let z = match x with
///          | 0 -> let a = 1
///          let w = 2
///          w
///      Without the column guard the arm would swallow `let w = 2` as a binder
///      and `w` as its value, stealing a top-level declaration. The guard is
///      the only place this parser consults indentation, and it only ever
///      makes the body SHORTER, never reinterprets what it already took.
and parseBracelessBinders (tokens: Token list) : ParseResult<Expr> option =
    let (_, baseCol) = currentPos tokens
    // The chain ran out of statements without reaching a value. With at least
    // one binder consumed, that is not a fallback case -- it is the user's
    // mistake, and it has exactly two shapes: they forgot the result, or they
    // outdented it past `baseCol` so rule 2 fenced it off. Diagnosing it here
    // is what keeps it from reaching codegen as a void-valued match arm.
    // Reported at the body's first token, which is the `let` that cannot be a
    // value. Generic BL1999 -- no new diagnostic code to register or pin.
    let noValue () : ParseResult<Expr> option =
        let (l, c) = currentPos tokens
        Some (error ("A braceless body cannot consist only of bindings -- a `let` is not a value. "
                     + "Add the result expression on its own line, indented to at least the column of "
                     + "this `let`, or wrap the body in braces: `{ let x = ...; expr }`.") l c)
    let rec loop (stmts: Stmt list) (toks: Token list) : ParseResult<Expr> option =
        let toks = skipNL toks
        let (sLine, sCol) = currentPos toks
        let spanned (remaining: Token list) (stmt: Stmt) =
            let (eLine, eCol) = consumedEnd toks remaining sLine sCol
            StmtSpanned (stmt, { StartLine = sLine; StartCol = sCol
                                 EndLine = eLine; EndCol = eCol; File = PS.Cur.File })
        // Rule 2. Checked before dispatch, so it fences the value expression
        // exactly as it fences a binder.
        if sCol < baseCol then (if List.isEmpty stmts then None else noValue ())
        else
        match peek toks with
        | Some (TokKeyword KwLet) ->
            (match parseLetStmt (advance toks) with
             | Ok (stmt, rest) ->
                 let rest = skipTerminator rest
                 loop (spanned rest stmt :: stmts) rest
             | Error e -> Some (Error e))
        | Some (TokKeyword KwFunction) ->
            (match parseNestedFunction (advance toks) with
             | Ok (stmt, rest) ->
                 let rest = skipTerminator rest
                 loop (spanned rest stmt :: stmts) rest
             | Error e -> Some (Error e))
        // Closers: the enclosing arm / block / call ended before any value
        // expression appeared.
        | Some TokPipe | Some TokRBrace | Some TokRParen | Some TokRBracket
        | Some TokComma | Some TokEOF | None ->
            if List.isEmpty stmts then None else noValue ()
        | Some _ ->
            (match parseInlineExpr toks with
             | Ok (value, rest) ->
                 Some (success (mkExpr (rangeSpan tokens rest)
                                       (ExprBlock (List.rev stmts, Some value))) rest)
             | Error e -> Some (Error e))
    loop [] tokens

and parseAssignment (tokens: Token list) : ParseResult<Expr> =
    parseTyped tokens >>= fun left rest ->
    match peek rest with
    | Some (TokOp "=") ->
        advance rest |> parseAssignment >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprAssign (left, right))) remaining
    // Compound assignment: desugar x += y to x = x + y
    | Some (TokOp "+=") ->
        advance rest |> parseAssignment >>= fun right remaining ->
        let sp = mergeSpan left.Span right.Span
        success (mkExpr sp (ExprAssign (left, mkExpr sp (ExprBinOp (Elementwise, OpAdd, left, right))))) remaining
    | Some (TokOp "-=") ->
        advance rest |> parseAssignment >>= fun right remaining ->
        let sp = mergeSpan left.Span right.Span
        success (mkExpr sp (ExprAssign (left, mkExpr sp (ExprBinOp (Elementwise, OpSub, left, right))))) remaining
    | Some (TokOp "*=") ->
        advance rest |> parseAssignment >>= fun right remaining ->
        let sp = mergeSpan left.Span right.Span
        success (mkExpr sp (ExprAssign (left, mkExpr sp (ExprBinOp (Elementwise, OpMul, left, right))))) remaining
    | Some (TokOp "/=") ->
        advance rest |> parseAssignment >>= fun right remaining ->
        let sp = mergeSpan left.Span right.Span
        success (mkExpr sp (ExprAssign (left, mkExpr sp (ExprBinOp (Elementwise, OpDiv, left, right))))) remaining
    | _ -> success left rest

/// Postfix type annotation `expr : Type`, sitting between parseAssignment and
/// parseNamedInfix in the precedence chain: binds tighter than `=` (so
/// `x = e: T` parses as `x = (e: T)`) but looser than every operator below it
/// (so `a + b : Int` parses as `(a + b) : Int`). Motivating case is width
/// adoption on constructor calls, e.g. `complex(re, im) : Complex64`, but the
/// form is general; TypeCheck applies it via bidirectional checkExpr, falling
/// back to inferExpr + unify.
and parseTyped (tokens: Token list) : ParseResult<Expr> =
    parseNamedInfix tokens >>= fun expr rest ->
    match peek rest with
    | Some TokColon ->
        advance rest |> parseTypeExpr >>= fun ty remaining ->
        // Span runs from the base expression through the annotated type.
        success (mkExpr (mergeSpan expr.Span (rangeSpan rest remaining)) (ExprTyped (expr, ty))) remaining
    | _ -> success expr rest

/// Named infix operators: a :name: b -> name(a, b)
/// Lowest precedence, left-associative
and parseNamedInfix (tokens: Token list) : ParseResult<Expr> =
    parsePipeline tokens >>= fun left rest ->
    let rec loop (acc: Expr) toks =
        match peek toks with
        | Some (TokNamedInfix name) ->
            advance toks |> parsePipeline >>= fun right remaining ->
            // Desugar :name: to function application: name(a, b)
            let fn = mkExpr (headSpan toks) (ExprVar name)
            let call = mkExpr (mergeSpan acc.Span right.Span) (ExprApp (fn, [acc; right]))
            loop call remaining
        | _ -> success acc toks
    loop left rest

and parsePipeline (tokens: Token list) : ParseResult<Expr> =
    parseChoice tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (TokOp "|>") ->
            advance toks' |> parseChoice >>= fun right remaining ->
            match right.Kind with
            | ExprKind.ExprVar "compute" -> loop (mkExpr (mergeSpan acc.Span right.Span) (ExprCompute acc)) remaining
            // Provider reads are module-qualified (`x |> alias.read`), so they
            // take the generic pipe-application desugar below; the checker's
            // provider-read arm rewrites the application to TExprRead.
            | _ -> loop (mkExpr (mergeSpan acc.Span right.Span) (ExprApp (right, [acc]))) remaining
        | Some (TokOp "|@>") ->
            // Pipe-apply: a |@> b  desugars to  b <@> a
            advance toks' |> parseChoice >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpApply, right, acc))) remaining
        | _ -> success acc toks
    loop left rest

and parseChoice (tokens: Token list) : ParseResult<Expr> =
    parseParallel tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (ChoiceOp op) ->
            advance toks' |> parseParallel >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseParallel (tokens: Token list) : ParseResult<Expr> =
    parseBind tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (ParallelOp op) ->
            advance toks' |> parseBind >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseBind (tokens: Token list) : ParseResult<Expr> =
    parseApply tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (BindOp op) ->
            advance toks' |> parseApply >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, op, acc, right))) remaining
        // >> is now a single token from the lexer
        | Some (TokOp ">>") ->
            advance toks' |> parseApply >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpCompose, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseApply (tokens: Token list) : ParseResult<Expr> =
    parseArrayProduct tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (ApplyOp op) ->
            advance toks' |> parseArrayProduct >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseArrayProduct (tokens: Token list) : ParseResult<Expr> =
    parseOr tokens >>= fun left rest ->
    let rec loop acc toks =
        let (peeked, toks') = peekContinuation toks
        match peeked with
        | Some (ArrayProductOp op) ->
            advance toks' |> parseOr >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseOr (tokens: Token list) : ParseResult<Expr> =
    parseAnd tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (OrOp (mode, op)) ->
            advance toks |> parseAnd >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (mode, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseAnd (tokens: Token list) : ParseResult<Expr> =
    parseEquality tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (AndOp (mode, op)) ->
            advance toks |> parseEquality >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (mode, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseEquality (tokens: Token list) : ParseResult<Expr> =
    parseComparison tokens >>= fun left rest ->
    match peek rest with
    | Some (EqualityOp (mode, op)) ->
        advance rest |> parseComparison >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (mode, op, left, right))) remaining
    | _ -> success left rest

and parseComparison (tokens: Token list) : ParseResult<Expr> =
    parseCons tokens >>= fun left rest ->
    match peek rest with
    | Some (ComparisonOp (mode, op)) ->
        advance rest |> parseCons >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (mode, op, left, right))) remaining
    | _ -> success left rest

and parseCons (tokens: Token list) : ParseResult<Expr> =
    parseDotDot tokens >>= fun left rest ->
    match peek rest with
    | Some TokColonColon ->
        advance rest |> parseDotDot >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpCons, left, right))) remaining
    | _ -> success left rest

and parseDotDot (tokens: Token list) : ParseResult<Expr> =
    parseAdditive tokens >>= fun left rest ->
    match peek rest with
    | Some TokDotDot ->
        advance rest |> parseAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprDotDot (left, right))) remaining
    | _ -> success left rest

and parseAdditive (tokens: Token list) : ParseResult<Expr> =
    parseMultiplicative tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (AdditiveOp (mode, op)) ->
            advance toks |> parseMultiplicative >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (mode, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseMultiplicative (tokens: Token list) : ParseResult<Expr> =
    parsePower tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (MultiplicativeOp (mode, op)) ->
            advance toks |> parsePower >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (mode, op, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parsePower (tokens: Token list) : ParseResult<Expr> =
    parseUnary tokens >>= fun left rest ->
    match peek rest with
    | Some (PowerOp (mode, op)) ->
        advance rest |> parsePower >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (mode, op, left, right))) remaining
    | _ -> success left rest

and parseUnary (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some (UnaryOp op) ->
        advance tokens |> parseUnary >>= fun expr remaining ->
        success (mkExpr (mergeSpan (headSpan tokens) expr.Span) (ExprUnaryOp (op, expr))) remaining
    | _ -> parsePostfix tokens

/// Parse struct construction: Name { field1 = val1, field2 = val2 }
and parseStructExpr (name: string) (tokens: Token list) : ParseResult<Expr> =
    expect TokLBrace tokens >>= fun _ afterBrace ->
    
    let rec parseFieldInits toks =
        let toks = skipNL toks
        match peek toks with
        | Some TokRBrace -> success ([], None) (advance toks)
        | Some TokDotDot ->
            // Functional update: `..base` copies the remaining fields from
            // base. Must be the LAST entry (nothing may follow it).
            parseExprImpl (advance toks) >>= fun baseExpr afterBase ->
            let afterBase = skipNL afterBase
            (match peek afterBase with
             | Some TokRBrace -> success ([], Some baseExpr) (advance afterBase)
             | _ ->
                 let line, col = currentPos afterBase
                 error "'..base' must be the last entry in a struct literal" line col)
        | Some (TokIdent fieldName) ->
            let afterFieldName = advance toks
            match peek afterFieldName with
            | Some (TokOp "=") ->
                parseExprImpl (advance afterFieldName) >>= fun value afterValue ->
                let afterValue = skipNL afterValue
                match peek afterValue with
                | Some TokComma ->
                    parseFieldInits (advance afterValue) >>= fun (rest, spread) remaining ->
                    success ((fieldName, value) :: rest, spread) remaining
                | Some TokRBrace ->
                    success ([(fieldName, value)], None) (advance afterValue)
                | _ ->
                    let line, col = currentPos afterValue
                    error "Expected ',' or '}' in struct expression" line col
            | Some TokComma ->
                // shorthand: field (same as field = field)
                parseFieldInits (advance afterFieldName) >>= fun (rest, spread) remaining ->
                success ((fieldName, mkExpr (headSpan toks) (ExprVar fieldName)) :: rest, spread) remaining
            | Some TokRBrace ->
                success ([(fieldName, mkExpr (headSpan toks) (ExprVar fieldName))], None) (advance afterFieldName)
            | _ ->
                let line, col = currentPos afterFieldName
                error "Expected '=' or ',' in struct field" line col
        | _ ->
            let line, col = currentPos toks
            error "Expected field name, '..base', or '}'" line col

    parseFieldInits afterBrace >>= fun (fields, spread) remaining ->
    // Span covers the `{ ... }` body (the type name is consumed by the caller
    // before this function is entered, so it is not part of this range).
    success (mkE tokens remaining (ExprStruct (name, fields, spread))) remaining

and parsePostfix (tokens: Token list) : ParseResult<Expr> =
    parsePrimary tokens >>= fun left rest ->
    // A postfix `(` or `[` extends the previous expression only when it opens
    // on the line where that expression ends. The lexer strips newline
    // tokens inside delimiters (tokenizeWithNewlines), so inside a block
    // `let b = v` followed by a final tuple `(a, b)` on the next line would
    // otherwise parse as the call `v(a, b)`, reported as a baffling
    // unbound-variable error at the let. Line numbers survive the stripping:
    // compare the opener's line against the token before it (the same-line
    // rule Kotlin and Swift use). `.field` chains stay line-insensitive.
    let opensOnLaterLine (opener: Token) =
        let rec lastLineBefore last toks =
            match toks with
            | (t: Token) :: rest when t.Line < opener.Line
                                      || (t.Line = opener.Line && t.Col < opener.Col) ->
                lastLineBefore t.Line rest
            | _ -> last
        lastLineBefore opener.Line tokens < opener.Line
    let rec loop acc toks =
        match peek toks with
        | Some TokLParen when not (opensOnLaterLine (List.head toks)) ->
            // Function call (delimited: struct literals re-enabled)
            allowingStructLiterals (fun () -> advance toks |> sepBy parseExprImpl TokComma) >>= fun args afterArgs ->
            expect TokRParen afterArgs >>= fun _ remaining ->
            loop (mkExpr (mergeSpan acc.Span (rangeSpan toks remaining)) (ExprApp (acc, args))) remaining
        | Some TokLBracket when not (opensOnLaterLine (List.head toks)) ->
            // Poly-tuple indexing: args[k] (delimited: struct literals re-enabled)
            allowingStructLiterals (fun () -> advance toks |> parseExprImpl) >>= fun index afterIndex ->
            expect TokRBracket afterIndex >>= fun _ remaining ->
            loop (mkExpr (mergeSpan acc.Span (rangeSpan toks remaining)) (ExprTupleIndex (acc, index))) remaining
        | Some TokDot ->
            advance toks |> expectIdent >>= fun field remaining ->
            loop (mkExpr (mergeSpan acc.Span (rangeSpan toks remaining)) (ExprField (acc, field))) remaining
        | _ -> success acc toks
    loop left rest

and parsePrimary (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some (LiteralTok lit) -> success (mkExpr (headSpan tokens) (ExprLit lit)) (advance tokens)

    // Wildcard hole `_` (e.g. a free axis in a compound index B((a, _, c))):
    // a general token whose meaning comes from the consuming context;
    // unconsumed uses are rejected in typecheck.
    | Some TokUnderscore -> success (mkExpr (headSpan tokens) ExprWildcard) (advance tokens)

    | Some (TokIdent name) ->
        let afterName = advance tokens
        match peek afterName with
        | Some TokLBrace when not noStructLiteralCtx.Value ->
            // Struct construction: Name { field1 = val1, ... }, suppressed in
            // for-in range headers (see noStructLiteralCtx). Widen the span
            // to include the leading type name.
            parseStructExpr name afterName >>= fun e remaining ->
            success { e with Span = rangeSpan tokens remaining } remaining
        | _ ->
            success (mkExpr (headSpan tokens) (ExprVar name)) afterName

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
    | Some (TokKeyword KwNth) -> success (mkExpr (headSpan tokens) ExprNth) (advance tokens)
    | Some (TokKeyword KwZero) -> success (mkExpr (headSpan tokens) ExprZero) (advance tokens)

    | Some (TokKeyword KwRank) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun expr afterExpr ->
        expect TokRParen afterExpr >>= fun _ remaining ->
        success (mkE tokens remaining (ExprRank expr)) remaining

    | Some (TokKeyword KwCompute) ->
        success (mkExpr (headSpan tokens) (ExprVar "compute")) (advance tokens)

    | Some (TokKeyword KwLambda) ->
        // Widen the span to include the leading `lambda` keyword.
        parseLambda (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some (TokKeyword KwLet) ->
        parseLet (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some (TokKeyword KwIf) ->
        parseIf (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some (TokKeyword KwMatch) ->
        parseMatch (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some (TokKeyword KwMethodFor) ->
        parseMethodFor (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    // for (A, B) in virtualArray: co-iteration construct
    | Some (TokKeyword KwFor) ->
        parseForConstruct (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    // static method_for / static object_for / static for: the wrapped
    // former's argument list elaborates at compile time (Unfold eliminates
    // ExprStatic before typechecking). Only valid immediately before a former.
    | Some (TokKeyword KwStatic) ->
        let afterStatic = advance tokens
        match peek afterStatic with
        | Some (TokKeyword KwMethodFor) ->
            parseMethodFor (advance afterStatic) >>= fun former remaining ->
            success (mkE tokens remaining (ExprStatic former)) remaining
        | Some (TokKeyword KwObjectFor) ->
            parseObjectFor (advance afterStatic) >>= fun former remaining ->
            success (mkE tokens remaining (ExprStatic former)) remaining
        | Some (TokKeyword KwFor) ->
            parseForConstruct (advance afterStatic) >>= fun former remaining ->
            success (mkE tokens remaining (ExprStatic former)) remaining
        | _ ->
            let (line, col) = currentPos afterStatic
            error "Expected 'method_for', 'object_for', or 'for' after 'static' in expression position" line col

    | Some (TokKeyword KwObjectFor) ->
        parseObjectFor (advance tokens) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some (TokKeyword KwZip) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        sepBy parseExprImpl TokComma afterLParen >>= fun exprs afterExprs ->
        expect TokRParen afterExprs >>= fun _ remaining ->
        success (mkE tokens remaining (ExprZip exprs)) remaining

    | Some (TokKeyword KwStack) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        sepBy parseExprImpl TokComma afterLParen >>= fun exprs afterExprs ->
        expect TokRParen afterExprs >>= fun _ remaining ->
        success (mkE tokens remaining (ExprStack exprs)) remaining

    // join(A, B, ..., d): concatenate along dimension d. d is the LAST
    // argument and must be an integer literal (compile-time, like transpose's axis pair).
    | Some (TokKeyword KwJoin) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        sepBy parseExprImpl TokComma afterLParen >>= fun exprs afterExprs ->
        expect TokRParen afterExprs >>= fun _ remaining ->
        let line, col = currentPos afterExprs
        (match List.rev exprs with
         | last :: revArrays when revArrays.Length >= 2 ->
            (match last.Kind with
             | ExprKind.ExprLit (LitInt d) ->
                success (mkE tokens remaining (ExprJoin (List.rev revArrays, int d))) remaining
             | _ ->
                error "join expects an integer dimension as its last argument: join(A, B, d)" line col)
         | _ ->
            error "join expects at least two arrays and a dimension: join(A, B, d)" line col)

    | Some (TokKeyword KwSequence) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        sepBy parseExprImpl TokComma afterLParen >>= fun exprs afterExprs ->
        expect TokRParen afterExprs >>= fun _ remaining ->
        success (mkE tokens remaining (ExprSequence exprs)) remaining

    | Some (TokKeyword KwReplicate) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun count afterCount ->
        expect TokComma afterCount >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun body afterBody ->
        expect TokRParen afterBody >>= fun _ remaining ->
        success (mkE tokens remaining (ExprReplicate (count, body))) remaining

    | Some (TokKeyword KwGuard) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun cond afterCond ->
        expect TokComma afterCond >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun body afterBody ->
        expect TokRParen afterBody >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGuard (cond, body))) remaining

    | Some (TokKeyword KwMask) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun arr afterArr ->
        expect TokComma afterArr >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun pred afterPred ->
        expect TokRParen afterPred >>= fun _ remaining ->
        success (mkE tokens remaining (ExprMask (arr, pred))) remaining

    | Some (TokKeyword KwCompound) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun dense afterDense ->
        expect TokComma afterDense >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun mask afterMask ->
        expect TokRParen afterMask >>= fun _ remaining ->
        success (mkE tokens remaining (ExprCompound (dense, mask))) remaining

    // sparse(values, keys): bundles rank-1 values (in key order) with an
    // explicit key list into a SparseIdx-typed array (formalism 3.5).
    | Some (TokKeyword KwSparse) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun values afterValues ->
        expect TokComma afterValues >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun keys afterKeys ->
        expect TokRParen afterKeys >>= fun _ remaining ->
        success (mkE tokens remaining (ExprSparse (values, keys))) remaining

    | Some (TokKeyword KwIntersect) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun a afterA ->
        expect TokComma afterA >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun b afterB ->
        expect TokRParen afterB >>= fun _ remaining ->
        success (mkE tokens remaining (ExprIntersect (a, b))) remaining

    | Some (TokKeyword KwUnion) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun a afterA ->
        expect TokComma afterA >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun b afterB ->
        expect TokRParen afterB >>= fun _ remaining ->
        success (mkE tokens remaining (ExprUnion (a, b))) remaining

    | Some (TokKeyword KwUnique) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun arr afterArr ->
        expect TokRParen afterArr >>= fun _ remaining ->
        success (mkE tokens remaining (ExprUnique arr)) remaining

    | Some (TokKeyword KwContains) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun arr afterArr ->
        expect TokComma afterArr >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun value afterValue ->
        expect TokRParen afterValue >>= fun _ remaining ->
        success (mkE tokens remaining (ExprContains (arr, value))) remaining

    | Some (TokKeyword KwGroupBy) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun vals afterVals ->
        expect TokComma afterVals >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun keys afterKeys ->
        expect TokRParen afterKeys >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGroupBy (vals, keys))) remaining
    
    // group_keys(keys1, keys2, ...): one key for ordinary grouping; multiple
    // keys triggers compound (tuple-keyed) grouping.
    | Some (TokKeyword KwGroupKeys) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        sepBy parseExprImpl TokComma afterLParen >>= fun keys afterKeys ->
        expect TokRParen afterKeys >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGroupKeys keys)) remaining

    // group_bucket(gk): expose the grouping's row -> bucket map as data. One
    // argument; the "must be a bare name" rule is a TYPECHECK refusal, not a
    // parse one, so the diagnostic can explain why (gk is not a value).
    | Some (TokKeyword KwGroupBucket) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun gk afterGk ->
        expect TokRParen afterGk >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGroupBucket gk)) remaining

    | Some (TokKeyword KwSort) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        expect TokComma afterArr >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun key afterKey ->
        expect TokRParen afterKey >>= fun _ remaining ->
        success (mkE tokens remaining (ExprSort (array, key))) remaining

    // transpose(A, [d1, d2]): swaps exactly two axes; the axis list must be
    // exactly two integer literals (no general permutation). Semantic checks
    // (d1 != d2, in range, both axes arity-1 SymNone) happen in TypeCheck.
    | Some (TokKeyword KwTranspose) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        expect TokComma afterArr >>= fun _ afterComma ->
        expect TokLBracket afterComma >>= fun _ afterLBrack ->
        (match peek afterLBrack with
         | Some (TokInt d1) ->
            let afterD1 = advance afterLBrack
            expect TokComma afterD1 >>= fun _ afterComma2 ->
            (match peek afterComma2 with
             | Some (TokInt d2) ->
                let afterD2 = advance afterComma2
                expect TokRBracket afterD2 >>= fun _ afterRBrack ->
                expect TokRParen afterRBrack >>= fun _ remaining ->
                success (mkE tokens remaining (ExprTranspose (array, int d1, int d2))) remaining
             | _ ->
                let line, col = currentPos afterComma2
                error "transpose expects exactly two integer axis indices: transpose(A, [d1, d2])" line col)
         | _ ->
            let line, col = currentPos afterLBrack
            error "transpose expects exactly two integer axis indices: transpose(A, [d1, d2])" line col)
    
    // hermitian(A): the conjugate-transpose A^H of a rank-2 array; desugars to
    // conj(transpose(A, [0, 1])). Result is a plain dense array (the operation
    // A^H), not a SymHermitian-typed matrix -- that comes from `gram`.
    | Some (TokKeyword KwHermitian) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        expect TokRParen afterArr >>= fun _ remaining ->
        let sp = rangeSpan tokens remaining
        success (mkExpr sp (ExprUnaryOp (OpConj, mkExpr sp (ExprTranspose (array, 0, 1))))) remaining

    // gram(A, B) = A * B^H: result[i][j] = sum_k A[i][k] * conj(B[j][k]).
    // A is m x n, B is p x n (shared contracted dim n), result is m x p. When A
    // and B are the same variable the result is symmetric/Hermitian, computed
    // via the triangular upper-half scatter; otherwise a general dense array.
    | Some (TokKeyword KwGram) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun left afterLeft ->
        expect TokComma afterLeft >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun right afterRight ->
        expect TokRParen afterRight >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGram (left, right))) remaining

    // gram_apply(A, B, x) = A * (B^H * x): the ACTION of gram(A, B) on the
    // vector x without forming the m x p matrix. A is m x n, B is p x n, x
    // has p cells, the result has m cells (docs/plans/structural/05, 3.2).
    | Some (TokKeyword KwGramApply) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun left afterLeft ->
        expect TokComma afterLeft >>= fun _ afterComma ->
        parseExprImpl afterComma >>= fun right afterRight ->
        expect TokComma afterRight >>= fun _ afterComma2 ->
        parseExprImpl afterComma2 >>= fun vec afterVec ->
        expect TokRParen afterVec >>= fun _ remaining ->
        success (mkE tokens remaining (ExprGramApply (left, right, vec))) remaining

    | Some (TokKeyword KwDecompact) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        expect TokComma afterArr >>= fun _ afterComma ->
        (match peek afterComma with
         | Some (TokInt d) ->
            let afterD = advance afterComma
            expect TokRParen afterD >>= fun _ remaining ->
            success (mkE tokens remaining (ExprDecompact (array, int d))) remaining
         | _ ->
            let line, col = currentPos afterComma
            error "decompact expects a single integer dimension index: decompact(A, d)" line col)
    
    // reduce(array, op[, init][, axes = n]): folds the innermost `n` axes by a
    // binary kernel, n = 1 by default (default kernel (+) if omitted; accepts
    // operator sections like (+)). The optional init is each folded group's
    // initial accumulator (init (+) a0 (+) a1 ...); an empty group reduces to
    // init, and without init empty inputs are rejected.
    //
    // `axes` is a NAMED final argument, not a fourth positional one: the third
    // POSITIONAL slot is already the seed, so a bare `reduce(A, op, 2)` would be
    // ambiguous between "seed 2" and "fold 2 axes". Recognized by the same
    // two-token lookahead `isNamedTypeArg` uses for `min=`/`max=` in type
    // arguments -- an identifier immediately followed by `=`, a shape that is a
    // hard parse error in an expression argument today, so this is a strict
    // widening. Special-cased to `reduce`; ordinary calls have no named slots.
    | Some (TokKeyword KwReduce) ->
        let isAxesArg (toks: Token list) =
            match toks with
            | t1 :: t2 :: _ ->
                (match t1.Kind with TokIdent "axes" -> true | _ -> false) && t2.Kind = TokOp "="
            | _ -> false
        // `axes = n` then the closing paren. Only ever called with `isAxesArg`
        // true, so the two leading tokens are the name and the `=`.
        let parseAxesTail (array: Expr) (op: Expr) (initE: Expr option) (toks: Token list) =
            parseExprImpl (advance (advance toks)) >>= fun axesE afterAxes ->
            expect TokRParen afterAxes >>= fun _ remaining ->
            success (mkE tokens remaining (ExprReduce (array, op, initE, Some axesE))) remaining
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        match peek afterArr with
        | Some TokRParen ->
            // 1-arg form: reduce(arr) = reduce(arr, (+))
            expect TokRParen afterArr >>= fun _ remaining ->
            let sp = rangeSpan tokens remaining
            success (mkExpr sp (ExprReduce (array, mkExpr sp (ExprSection OpAdd), None, None))) remaining
        | _ ->
            expect TokComma afterArr >>= fun _ afterComma ->
            // `reduce(A, axes = n)`: the kernel-omitted sugar, carrying an axis
            // count. Same defaulted (+) the 1-arg form supplies.
            if isAxesArg afterComma then
                parseAxesTail array (mkExpr (rangeSpan tokens afterComma) (ExprSection OpAdd)) None afterComma
            else
            parseExprImpl afterComma >>= fun op afterOp ->
            match peek afterOp with
            | Some TokRParen ->
                expect TokRParen afterOp >>= fun _ remaining ->
                success (mkE tokens remaining (ExprReduce (array, op, None, None))) remaining
            | _ ->
                expect TokComma afterOp >>= fun _ afterComma2 ->
                if isAxesArg afterComma2 then
                    parseAxesTail array op None afterComma2
                else
                parseExprImpl afterComma2 >>= fun initE afterInit ->
                match peek afterInit with
                | Some TokRParen ->
                    expect TokRParen afterInit >>= fun _ remaining ->
                    success (mkE tokens remaining (ExprReduce (array, op, Some initE, None))) remaining
                | _ ->
                    expect TokComma afterInit >>= fun _ afterComma3 ->
                    if isAxesArg afterComma3 then
                        parseAxesTail array op (Some initE) afterComma3
                    else
                        let line, col = currentPos afterComma3
                        error "reduce takes at most three positional arguments (array, kernel, init); \
the axis count is the named final argument `axes = n`" line col

    // conj(x): complex conjugate (identity on real). Lowers to ExprUnaryOp(OpConj, _).
    | Some (TokKeyword KwConj) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun arg afterArg ->
        expect TokRParen afterArg >>= fun _ remaining ->
        success (mkE tokens remaining (ExprUnaryOp (OpConj, arg))) remaining

    | Some (TokKeyword KwExtents) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun array afterArr ->
        expect TokRParen afterArr >>= fun _ remaining ->
        success (mkE tokens remaining (ExprExtents array)) remaining

    | Some (TokKeyword KwPure) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun expr afterExpr ->
        expect TokRParen afterExpr >>= fun _ remaining ->
        success (mkE tokens remaining (ExprPure expr)) remaining

    // reynolds(kernel) or reynolds(kernel, Antisymmetric)
    | Some (TokKeyword KwReynolds) ->
        advance tokens |> expect TokLParen >>= fun _ afterLParen ->
        parseExprImpl afterLParen >>= fun kernel afterKernel ->
        let isAntisym, afterSpec =
            match peek afterKernel with
            | Some TokComma ->
                match peek (advance afterKernel) with
                | Some (TokIdent "Antisymmetric") -> true, advance (advance afterKernel)
                | Some (TokIdent "Symmetric") -> false, advance (advance afterKernel)
                | _ -> false, afterKernel
            | _ -> false, afterKernel
        expect TokRParen afterSpec >>= fun _ remaining ->
        success (mkE tokens remaining (ExprReynolds (kernel, isAntisym))) remaining
    
    // range<T1, ..., Tn>: one virtual array spanning all listed index types,
    // uncurried into nested loop levels in IR.
    | Some (TokKeyword KwRange) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        afterLt |> sepBy parseTypeExpr TokComma >>= fun tys afterTys ->
        expectGt afterTys >>= fun _ remaining ->
        success (mkE tokens remaining (ExprRange tys)) remaining

    | Some (TokKeyword KwReverse) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun ty afterTy ->
        expectGt afterTy >>= fun _ remaining ->
        success (mkE tokens remaining (ExprReverse ty)) remaining

    // halo<Inner, [offsets]>: stencil traversal transformer. At each ordinal
    // position of the inner index type, the static signed offsets select the
    // neighborhood (center = 0, sign = direction). Composes inside range<...>
    // for n-D. Offsets parse via parseSimpleExpr, like other static payloads.
    | Some (TokKeyword KwHalo) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun inner afterInner ->
        expect TokComma afterInner >>= fun _ afterComma ->
        parseSimpleExpr afterComma >>= fun offsets afterOffsets ->
        expectGt afterOffsets >>= fun _ remaining ->
        success (mkE tokens remaining (ExprHalo (inner, offsets))) remaining

    // Parenthesized expr or tuple (delimited: struct literals re-enabled --
    // the for-in range-header suppression applies only at the top nesting
    // level). Span widens to cover both parens; child spans are untouched.
    | Some TokLParen ->
        allowingStructLiterals (fun () -> advance tokens |> parseParenExpr) >>= fun e remaining ->
        success { e with Span = rangeSpan tokens remaining } remaining

    | Some TokLBracket ->
        allowingStructLiterals (fun () ->
            advance tokens |> sepBy parseExprImpl TokComma >>= fun elems afterElems ->
            expect TokRBracket afterElems >>= fun _ remaining ->
            success (mkE tokens remaining (ExprArrayLit elems)) remaining)

    | Some TokLBrace ->
        parseBlock (advance tokens)

    | Some kind ->
        let line, col = currentPos tokens
        error $"Unexpected token: {describeToken kind}" line col

    | None ->
        errorEof "Unexpected end of input"

// Compound Expression Parsers

and parseLambda (tokens: Token list) : ParseResult<Expr> =
    expect TokLParen tokens >>= fun _ afterLParen ->
    sepBy parseLambdaParam TokComma afterLParen >>= fun parms afterParms ->
    expect TokRParen afterParms >>= fun _ afterRParen ->
    
    // Optional where clause (parallel to function declarations). A parse
    // error inside it must propagate as a genuine error, not be swallowed
    // to None -- that would produce a misleading "expected ->" at `where`
    // and hide the real cause (e.g. mutual-exclusion violations).
    let whereResult : ParseResult<WhereClause option> =
        match peek afterRParen with
        | Some (TokKeyword KwWhere) ->
            match parseWhereClause (advance afterRParen) with
            | Ok (w, rest) -> Ok (Some w, rest)
            | Error e -> Error e
        | _ -> Ok (None, afterRParen)
    whereResult >>= fun whereClause afterWhere ->
    expect (TokOp "->") afterWhere >>= fun _ afterArrow ->
    let afterArrow = skipNL afterArrow
    match peek afterArrow with
    | Some TokLBrace ->
        parseBlock (advance afterArrow) >>= fun body remaining ->
        success (mkE tokens remaining (ExprLambda (parms, whereClause, body))) remaining
    | _ ->
        // Inline body parses at Apply precedence so |> isn't consumed: this
        // means `lambda(x) -> a <@> b |> compute` parses as
        // `(lambda(x) -> a <@> b) |> compute`.
        parseApply afterArrow >>= fun body remaining ->
        success (mkE tokens remaining (ExprLambda (parms, whereClause, body))) remaining

and parseLambdaParam (tokens: Token list) : ParseResult<LambdaParam> =
    // The head token IS the name, so its span is kept for navigation before
    // the optional annotation is consumed.
    let nameSpan = headSpan tokens
    expectIdent tokens >>= fun name afterName ->
    // Optional default value: `name = expr` / `name: Type = expr`. The
    // expression parser stops at `,` and `)`, so the param list's own
    // delimiters are unaffected.
    let withDefault ty afterTy =
        match peek afterTy with
        | Some (TokOp "=") ->
            parseExprImpl (advance afterTy) >>= fun dflt remaining ->
            success { Name = name; Type = ty; Default = Some dflt; NameSpan = nameSpan } remaining
        | _ ->
            success { Name = name; Type = ty; Default = None; NameSpan = nameSpan } afterTy
    match peek afterName with
    | Some TokColon ->
        advance afterName |> parseTypeExpr >>= fun ty afterTy ->
        withDefault (Some ty) afterTy
    | _ ->
        withDefault None afterName

/// The right-hand side of a `let`, with the BARE-COMMA tuple construction of
/// docs/plan-tuples-vs-arg-packs.md 6b: `let t = b, c` builds the 2-tuple
/// `(b, c)`, `let t = a, b, c` the 3-tuple, and so on for any width >= 2.
/// This is the CONSTRUCTION half of plan issue #10 only -- the binder side
/// (`let a, b = t`) stays deferred, and the parenthesized `let (a, b) = t`
/// destructure is untouched.
///
/// Each component parses at full expression precedence and stops at the
/// comma (a comma is a separator, never an operator), so `let t = f(x), g(y)`
/// is a 2-tuple of two CALLS, not `f(x, g(y))`. The node produced is exactly
/// the `ExprTuple` a parenthesized literal produces, so the two spellings are
/// the same program from the checker down.
///
/// Shared by all three let-parse sites (`parseLet`, `parseLetStmt`,
/// `parseTopLevelLet`) so they cannot drift.
/// The optional `: T` annotation slot of a `let`, shared by all three let
/// sites. Historically each site inlined `match parseTypeExpr ... with Error
/// _ -> None, afterPat` -- a type-parse failure is SWALLOWED and the site
/// re-reports at the `=`, which turns a precise annotation diagnostic into a
/// misleading "Expected '=' but got ':'" pointing at the colon.
///
/// `isHardParseError` codes are propagated instead of swallowed -- the same
/// rule `sepBy` applies, for the same reason: a BL1004 can only be raised
/// once the slot is unambiguously an annotation, so the fallback's tolerance
/// for non-annotation colons is unchanged.
and parseLetAnnotation (afterPat: Token list) : ParseResult<TypeExpr option> =
    match peek afterPat with
    | Some TokColon ->
        match parseTypeExpr (advance afterPat) with
        | Ok (t, rest) -> success (Some t) rest
        | Error e when isHardParseError e -> Error e
        | Error _ -> success None afterPat
    | _ -> success None afterPat

/// The LHS of a `let`, in BOTH spellings: a single pattern, or a BARE COMMA
/// LIST (`let a, b = t`). The list desugars to exactly what the parenthesized
/// `let (a, b) = t` produces -- one `PatTuple` over the same leaves -- so
/// there is no second destructuring mechanism to keep in step
/// (docs/plan-array-expression-fixes.md #10, the deferred half; construction
/// `let t = b, c` is `parseLetRhs`'s mirror image, landed the same day).
///
/// UNAMBIGUOUS against RHS construction, which is the reason the two halves
/// can coexist: the LHS list is bounded by the `:` or `=` that must follow a
/// let pattern, and the RHS list only begins after that `=`. So
/// `let a, b = c, d` is "destructure the pair built from c and d", and no
/// existing program changes meaning -- a comma could not previously follow a
/// let pattern at all (it was `BL1001: Expected '=' but got ','`). The only
/// behavioural difference on ill-formed input is which token the missing-`=`
/// error points at.
///
/// Shared by all three let-parse sites (`parseLet`, `parseLetStmt`,
/// `parseTopLevelLet`) so they cannot drift.
and parseLetPattern (tokens: Token list) : ParseResult<Pattern> =
    parsePattern tokens >>= fun first afterFirst ->
    match peek afterFirst with
    | Some TokComma ->
        let rec rest (acc: Pattern list) (toks: Token list) : ParseResult<Pattern list> =
            parsePattern toks >>= fun p afterP ->
            match peek afterP with
            | Some TokComma -> rest (p :: acc) (advance afterP)
            | _ -> success (List.rev (p :: acc)) afterP
        rest [] (advance afterFirst) >>= fun tail afterTail ->
        success (mkP tokens afterTail (PatTuple (first :: tail))) afterTail
    | _ -> success first afterFirst

and parseLetRhs (tokens: Token list) : ParseResult<Expr> =
    parseExprImpl tokens >>= fun first afterFirst ->
    match peek afterFirst with
    | Some TokComma ->
        advance afterFirst |> sepBy parseExprImpl TokComma >>= fun rest afterRest ->
        success (mkExpr (rangeSpan tokens afterRest) (ExprTuple (first :: rest))) afterRest
    | _ -> success first afterFirst

and parseLet (tokens: Token list) : ParseResult<Expr> =
    // let [mut] pattern [: type] = value. Blade has no ML-style
    // "let x = v in body" -- `in` is only used for virtual arrays in for-loops.
    // There is NO `const` in the surface language: BindConst exists only as
    // the internal immutability marker minted by `let static` and the local
    // `function` desugar.
    let mutability, afterMut =
        match peek tokens with
        | Some (TokKeyword KwMut) -> BindMut, advance tokens
        | _ -> BindLet, tokens
    
    parseLetPattern afterMut >>= fun pat afterPat ->
    parseLetAnnotation afterPat >>= fun ty afterTy ->

    expect (TokOp "=") afterTy >>= fun _ afterEq ->

    let afterEq = skipNL afterEq
    match peek afterEq with
    | Some TokLBrace ->
        parseBlock (advance afterEq) >>= fun value afterValue ->
        let binding = { Mutability = mutability; Pattern = pat; Type = ty; Value = value }
        // Blade has no `let ... in`; the body is a synthesized unit placeholder
        // spanning the same source region as the let.
        let sp = rangeSpan tokens afterValue
        success (mkExpr sp (ExprLet (binding, mkExpr sp (ExprLit LitUnit)))) afterValue
    | _ ->
        parseLetRhs afterEq >>= fun value afterValue ->
        let binding = { Mutability = mutability; Pattern = pat; Type = ty; Value = value }
        let sp = rangeSpan tokens afterValue
        success (mkExpr sp (ExprLet (binding, mkExpr sp (ExprLit LitUnit)))) afterValue

and parseIf (tokens: Token list) : ParseResult<Expr> =
    parseExprImpl tokens >>= fun cond afterCond ->
    expect (TokKeyword KwThen) afterCond >>= fun _ afterThen ->
    parseExprImpl afterThen >>= fun thenBr afterThenBr ->
    expect (TokKeyword KwElse) afterThenBr >>= fun _ afterElse ->
    parseExprImpl afterElse >>= fun elseBr remaining ->
    success (mkE tokens remaining (ExprIf (cond, thenBr, elseBr))) remaining

and parseMatch (tokens: Token list) : ParseResult<Expr> =
    parseExprImpl tokens >>= fun scrutinee afterScrutinee ->
    expect (TokKeyword KwWith) afterScrutinee >>= fun _ afterWith ->
    manyMatchCases (skipNL afterWith) >>= fun cases remaining ->
    success (mkE tokens remaining (ExprMatch (scrutinee, cases))) remaining

/// `many parseMatchCase`, except that an `isHardParseError` code PROPAGATES
/// instead of ending the arm list. Plain `many` treats every failure as "the
/// list stops here", so a curated steer raised inside an arm (the `while`-on-
/// an-ordinary-arm message, the removed-`for` steer in an arm body) was
/// discarded and the caller re-reported the far-away, useless "Expected
/// declaration but got '|'" at the NEXT arm -- which is why every
/// pattern-level parse failure in a match looked identical. Same rule and
/// same justification `sepBy` and `parseLetAnnotation` already apply: a hard
/// code is raised only after the parse has committed, so propagating it cuts
/// off no legitimate backtrack.
and manyMatchCases (tokens: Token list) : ParseResult<MatchCase list> =
    let rec loop acc toks =
        match parseMatchCase toks with
        | Ok (v, rest) -> loop (v :: acc) rest
        | Error e when isHardParseError e -> Error e
        | Error _ -> Ok (List.rev acc, toks)
    loop [] tokens

// Guard-parse errors propagate rather than being swallowed.
and parseMatchCase (tokens: Token list) : ParseResult<MatchCase> =
    let tokens = skipNL tokens
    match peek tokens with
    | Some TokPipe ->
        advance tokens |> parsePattern >>= fun pat afterPat ->
        match peek afterPat with
        | Some (TokKeyword KwIf) ->
            advance afterPat |> parseGuardExpr >>= fun guard afterGuard ->
            expect (TokOp "->") afterGuard >>= fun _ afterArrow ->
            // parseBody: inline expressions stop at newline; multi-line
            // bodies (e.g. nested match) require braces.
            parseBody afterArrow >>= fun body remaining ->
            success { Pattern = pat; Guard = Some guard; Body = body } remaining
        // `while` belongs to the RECURSIVE-ARRAY arm and only there: it
        // declares when the induction stops (and freezes), which is a
        // statement about the recursion, not about this cell. An ordinary
        // match arm has nothing to stop -- its guard falls through to the
        // next arm -- so the two are not interchangeable in either
        // direction, and `parseConsArm` steers the mirror case (`if` on a
        // rec-array arm) back here. Without this the misuse died as a bare
        // "Expected '->'".
        | Some (TokIdent "while") ->
            let line, col = currentPos afterPat
            errorC "BL1003"
                "`while` guards a RECURSIVE-ARRAY arm (`let rec x = match x with | prefix :: n while <cond> -> ...`), where it declares when the induction stops and the array freezes. An ordinary match arm has no induction to stop: its guard is `if`, and a failing `if` falls through to the next arm."
                line col
        | _ ->
            expect (TokOp "->") afterPat >>= fun _ afterArrow ->
            parseBody afterArrow >>= fun body remaining ->
            success { Pattern = pat; Guard = None; Body = body } remaining
    | _ ->
        let line, col = currentPos tokens
        error "Expected '|' to start match case" line col

// Restricted expression parser for match guards: stops before -> (doesn't consume it).
and parseGuardExpr (tokens: Token list) : ParseResult<Expr> =
    parseGuardOr tokens

and parseGuardOr (tokens: Token list) : ParseResult<Expr> =
    parseGuardAnd tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "||") ->
            advance toks |> parseGuardAnd >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpOr, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseGuardAnd (tokens: Token list) : ParseResult<Expr> =
    parseGuardComparison tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "&&") ->
            advance toks |> parseGuardComparison >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpAnd, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseGuardComparison (tokens: Token list) : ParseResult<Expr> =
    parseGuardAdditive tokens >>= fun left rest ->
    match peek rest with
    | Some (TokOp "==") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpEq, left, right))) remaining
    | Some (TokOp "!=") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpNeq, left, right))) remaining
    | Some (TokOp "<") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpLt, left, right))) remaining
    | Some (TokOp "<=") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpLe, left, right))) remaining
    | Some (TokOp ">") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpGt, left, right))) remaining
    | Some (TokOp ">=") ->
        advance rest |> parseGuardAdditive >>= fun right remaining ->
        success (mkExpr (mergeSpan left.Span right.Span) (ExprBinOp (Elementwise, OpGe, left, right))) remaining
    | _ -> success left rest

and parseGuardAdditive (tokens: Token list) : ParseResult<Expr> =
    parseGuardMultiplicative tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "+") ->
            advance toks |> parseGuardMultiplicative >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpAdd, acc, right))) remaining
        | Some (TokOp "-") ->
            advance toks |> parseGuardMultiplicative >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpSub, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseGuardMultiplicative (tokens: Token list) : ParseResult<Expr> =
    parseGuardPrimary tokens >>= fun left rest ->
    let rec loop acc toks =
        match peek toks with
        | Some (TokOp "*") ->
            advance toks |> parseGuardPrimary >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpMul, acc, right))) remaining
        | Some (TokOp "/") ->
            advance toks |> parseGuardPrimary >>= fun right remaining ->
            loop (mkExpr (mergeSpan acc.Span right.Span) (ExprBinOp (Elementwise, OpDiv, acc, right))) remaining
        | _ -> success acc toks
    loop left rest

and parseGuardPrimary (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some (LiteralTok lit) -> success (mkExpr (headSpan tokens) (ExprLit lit)) (advance tokens)
    | Some (TokIdent name) ->
        let afterName = advance tokens
        match peek afterName with
        | Some TokLParen ->
            advance afterName |> sepBy parseGuardExpr TokComma >>= fun args afterArgs ->
            expect TokRParen afterArgs >>= fun _ remaining ->
            let fn = mkExpr (headSpan tokens) (ExprVar name)
            success (mkE tokens remaining (ExprApp (fn, args))) remaining
        | _ -> success (mkExpr (headSpan tokens) (ExprVar name)) afterName
    | Some TokLParen ->
        advance tokens |> parseGuardExpr >>= fun expr afterExpr ->
        expect TokRParen afterExpr >>= fun _ remaining ->
        success expr remaining
    | Some (TokOp "-") ->
        advance tokens |> parseGuardPrimary >>= fun expr remaining ->
        success (mkExpr (mergeSpan (headSpan tokens) expr.Span) (ExprUnaryOp (OpNeg, expr))) remaining
    | Some (TokOp "!") ->
        advance tokens |> parseGuardPrimary >>= fun expr remaining ->
        success (mkExpr (mergeSpan (headSpan tokens) expr.Span) (ExprUnaryOp (OpNot, expr))) remaining
    | Some kind ->
        let line, col = currentPos tokens
        error $"Unexpected token in guard: {describeToken kind}" line col
    | None ->
        errorEof "Expected guard expression but got end of file"

and parseMethodFor (tokens: Token list) : ParseResult<Expr> =
    expect TokLParen tokens >>= fun _ afterLParen ->
    sepBy parseExprImpl TokComma afterLParen >>= fun arrays afterArrays ->
    expect TokRParen afterArrays >>= fun _ remaining ->
    success (mkE tokens remaining (ExprMethodFor arrays)) remaining

/// The body of a `for` expression (tokens start after the `for` keyword):
/// `for (A, B) [in virt]` -> ForArrays; `for lambda(...)` -> ForKernel.
/// Shared by the plain and `static`-marked spellings.
and parseForConstruct (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some TokLParen ->
        advance tokens |> sepBy parseExprImpl TokComma >>= fun arrays afterArrays ->
        expect TokRParen afterArrays >>= fun _ afterRParen ->
        match peek afterRParen with
        | Some (TokKeyword KwIn) ->
            // Virtual array expression parses at arrayProduct level (stops before <@>).
            parseArrayProduct (advance afterRParen) >>= fun inExpr afterIn ->
            success (mkE tokens afterIn (ExprFor (ForArrays (arrays, Some inExpr), [], None))) afterIn
        | _ ->
            // No in-clause: equivalent to method_for(A, B)
            success (mkE tokens afterRParen (ExprFor (ForArrays (arrays, None), [], None))) afterRParen
    | _ ->
        parseExprImpl tokens >>= fun kernel remaining ->
        success (mkE tokens remaining (ExprFor (ForKernel kernel, [], None))) remaining

and parseObjectFor (tokens: Token list) : ParseResult<Expr> =
    expect TokLParen tokens >>= fun _ afterLParen ->
    // Combinator section: object_for(<&>), object_for(<&!>), object_for(<*>), etc.
    match peek afterLParen with
    | Some (TokOp op) ->
        let afterOp = advance afterLParen
        match peek afterOp with
        | Some TokRParen ->
            let binOp = combinatorOrScalarSection op
            match binOp with
            | Some b ->
                let remaining = advance afterOp
                let sp = rangeSpan tokens remaining
                success (mkExpr sp (ExprObjectFor (mkExpr sp (ExprSection b)))) remaining
            | None ->
                let line, col = currentPos afterLParen
                error $"Unknown operator in object_for: {op}" line col
        | _ ->
            parseExprImpl afterLParen >>= fun kernel afterKernel ->
            expect TokRParen afterKernel >>= fun _ remaining ->
            success (mkE tokens remaining (ExprObjectFor kernel)) remaining
    | _ ->
        parseExprImpl afterLParen >>= fun kernel afterKernel ->
        expect TokRParen afterKernel >>= fun _ remaining ->
        success (mkE tokens remaining (ExprObjectFor kernel)) remaining

and parseParenExpr (tokens: Token list) : ParseResult<Expr> =
    match peek tokens with
    | Some TokRParen ->
        let remaining = advance tokens
        success (mkE tokens remaining (ExprTuple [])) remaining
    // Operator section: (+), (*), (<&!>), etc.
    | Some (TokOp op) ->
        let afterOp = advance tokens
        match peek afterOp with
        | Some TokRParen ->
            match combinatorOrScalarSection op with
            | Some binOp ->
                let remaining = advance afterOp
                success (mkE tokens remaining (ExprSection binOp)) remaining
            | None ->
                let line, col = currentPos tokens
                error $"Unknown operator in section: {op}" line col
        | _ ->
            parseExprImpl tokens >>= fun first afterFirst ->
            match peek afterFirst with
            | Some TokRParen ->
                success first (advance afterFirst)
            | Some TokComma ->
                advance afterFirst |> sepBy parseExprImpl TokComma >>= fun rest afterRest ->
                expect TokRParen afterRest >>= fun _ remaining ->
                success (mkE tokens remaining (ExprTuple (first :: rest))) remaining
            | _ ->
                let line, col = currentPos afterFirst
                errorC "BL1001" "Expected ')' or ',' in parenthesized expression" line col
    | _ ->
        parseExprImpl tokens >>= fun first afterFirst ->
        match peek afterFirst with
        | Some TokRParen ->
            success first (advance afterFirst)
        | Some TokComma ->
            advance afterFirst |> sepBy parseExprImpl TokComma >>= fun rest afterRest ->
            expect TokRParen afterRest >>= fun _ remaining ->
            success (mkE tokens remaining (ExprTuple (first :: rest))) remaining
        | _ ->
            let line, col = currentPos afterFirst
            errorC "BL1001" "Expected ')' or ',' in parenthesized expression" line col

/// Convert operator string to BinOp
/// The section table shared by `(op)` and `object_for(op)`: the COMBINATOR
/// operators first, then the scalar ops. One table, so the two spellings can
/// never disagree about which operators have a section -- which is what let
/// `object_for(<&!>)` parse while `(<&!>)` did not (BL1999 "Unknown operator
/// in section"), even though the reduction-join forms need both.
and combinatorOrScalarSection (op: string) : BinOp option =
    match op with
    | "<&>" -> Some OpParallel
    | "<&!>" -> Some OpFusion
    | "<*>" -> Some OpArrayProd
    | "<@>" -> Some OpApply
    | "<$>" -> Some OpFunctor
    | "<|>" -> Some OpChoice
    | "<|:>" -> Some OpFallback
    | ">>=" -> Some OpBind
    | ">>@" -> Some OpComposeObj
    | "@>>" -> Some OpComposeMeth
    | _ -> stringToBinOp op  // scalar ops (+, *, etc.)

and stringToBinOp (op: string) : BinOp option =
    match op with
    | "+" -> Some OpAdd
    | "-" -> Some OpSub
    | "*" -> Some OpMul
    | "/" -> Some OpDiv
    | "%" -> Some OpMod
    | "^" -> Some OpCaret
    | "==" -> Some OpEq
    | "!=" -> Some OpNeq
    | "<" -> Some OpLt
    | "<=" -> Some OpLe
    | ">" -> Some OpGt
    | ">=" -> Some OpGe
    | "&&" -> Some OpAnd
    | "||" -> Some OpOr
    | _ -> None

and parseBlock (tokens: Token list) : ParseResult<Expr> =
    let rec loop stmts toks =
        let toks = skipNL toks
        // Statement span (audit 3.4): start = first token; end filled in by
        // `spanned` below using the last meaningful token consumed (not the
        // next token's start, which would overshoot across whitespace/comments).
        let sLine, sCol = currentPos toks
        let spanned (remaining: Token list) (stmt: Stmt) =
            let eLine, eCol = consumedEnd toks remaining sLine sCol
            StmtSpanned (stmt, { StartLine = sLine; StartCol = sCol; EndLine = eLine; EndCol = eCol; File = PS.Cur.File })
        match peek toks with
        | Some TokRBrace ->
            // Last expression (if any) is the block's return value. stmts is
            // in reverse order (most recent first), so head = last statement.
            let (statements, finalExpr) =
                match stmts with
                | StmtSpanned (StmtExpr e, _) :: rest
                | StmtExpr e :: rest -> List.rev rest, Some e
                | all -> List.rev all, None
            // Span runs from the first statement token to the closing brace
            // (`tokens` is positioned just after the opening `{`).
            success (mkE tokens (advance toks) (ExprBlock (statements, finalExpr))) (advance toks)
        | Some TokSemi ->
            loop stmts (advance toks)
        | Some (TokKeyword KwLet) ->
            advance toks |> parseLetStmt >>= fun stmt remaining ->
            let remaining = skipTerminator remaining
            loop (spanned remaining stmt :: stmts) remaining
        | Some (TokKeyword KwFunction) ->
            // Nested function declaration: parsed as a let binding of a lambda.
            advance toks |> parseNestedFunction >>= fun stmt remaining ->
            let remaining = skipTerminator remaining
            loop (spanned remaining stmt :: stmts) remaining
        | Some (TokKeyword KwFor) ->
            // Imperative `for IDENT in a..b { }` is removed from the surface
            // language: iteration is loop objects (parallel) or recursive
            // arrays (sequential). This shell stays only to give the shape a
            // steering diagnostic instead of a confusing misparse at `in`.
            // (Internal generators still construct StmtForIn directly.)
            let afterFor = advance toks
            match peek afterFor with
            | Some (TokIdent _) ->
                let afterIdent = advance afterFor
                match peek afterIdent with
                | Some (TokKeyword KwIn) ->
                    let line, col = currentPos toks
                    errorC "BL1003" forInRemovedMsg line col
                | _ ->
                    // Loop-object `for` expression: the surviving form.
                    parseExprImpl toks >>= fun expr remaining ->
                    let remaining = skipTerminator remaining
                    loop (spanned remaining (StmtExpr expr) :: stmts) remaining
            | _ ->
                parseExprImpl toks >>= fun expr remaining ->
                let remaining = skipTerminator remaining
                loop (spanned remaining (StmtExpr expr) :: stmts) remaining
        | Some _ ->
            parseExprImpl toks >>= fun expr remaining ->
            let remaining = skipTerminator remaining
            loop (spanned remaining (StmtExpr expr) :: stmts) remaining
        | None ->
            errorEof "Unexpected end of input in block"
    loop [] tokens

and skipTerminator toks =
    match peek toks with
    | Some TokNewline -> advance toks
    | Some TokSemi -> advance toks
    | _ -> toks

and parseNestedFunction (tokens: Token list) : ParseResult<Stmt> =
    // `function name(params) where ... -> Type = body` desugars to
    // `let name = lambda(params) where ... -> Type -> body`.
    expectIdent tokens >>= fun name afterName ->
    expect TokLParen afterName >>= fun _ afterLParen ->
    sepBy parseLambdaParam TokComma afterLParen >>= fun parms afterParms ->
    expect TokRParen afterParms >>= fun _ afterRParen ->

    let afterRParen = skipNL afterRParen

    // A parse error inside an optional where clause must propagate as a
    // genuine error (mirrors parseLambda), not be swallowed to None -- that
    // would fail later with a misleading "expected =" at `where`.
    let whereResult : ParseResult<WhereClause option> =
        match peek afterRParen with
        | Some (TokKeyword KwWhere) ->
            match parseWhereClause (advance afterRParen) with
            | Ok (w, rest) -> Ok (Some w, skipNL rest)
            | Error e -> Error e
        | _ -> Ok (None, afterRParen)
    whereResult >>= fun whereClause afterWhere ->

    // A swallowed type-parse failure re-reports at the `=`; `isHardParseError`
    // codes are precise enough that losing them would be strictly worse.
    (match peek afterWhere with
     | Some (TokOp "->") ->
         (match parseTypeExpr (advance afterWhere) with
          | Ok (t, rest) -> success (Some t) (skipNL rest)
          | Error e when isHardParseError e -> Error e
          | Error _ -> success None afterWhere)
     | _ -> success None afterWhere) >>= fun retType afterRet ->

    expect (TokOp "=") afterRet >>= fun _ afterEq ->
    parseInlineOrBlock afterEq >>= fun body remaining ->

    let lambda = mkExpr (rangeSpan tokens remaining) (ExprLambda (parms, whereClause, body))
    let binding = {
        Pattern = mkPat (headSpan tokens) (PatVar name)
        Type = retType
        Value = lambda
        Mutability = BindConst
    }
    success (StmtLet binding) remaining

/// Parse the binding tail of `let rec NAME : TYPE = match NAME with ...`
/// (tokens start at NAME). Shared by the block-level and top-level let
/// paths. The arm shapes are validated HERE -- productivity is syntactic:
///   | zero        -> zero              required base (extent 0)
///   | zero :: n   -> zero :: SEED      optional seed (extent 1)
///   | prefix :: n -> prefix :: SLICE   required inductive arm
/// The snoc `::` exists ONLY inside these arm bodies (no general array-cons
/// expression operator), so the inductive arm literally cannot produce more
/// or less than one new slice.
and parseRecArrayBinding (tokens: Token list) : ParseResult<Binding> =
    let errHere toks msg =
        let line, col = currentPos toks
        errorC "BL1003" msg line col
    expectIdent tokens >>= fun name afterName ->
    // Type annotation is REQUIRED: a self-referential definition cannot
    // infer its own type from its body (mirrors recursive functions
    // declaring return types).
    match peek afterName with
    | Some TokColon ->
        parseTypeExpr (advance afterName) >>= fun ty afterTy ->
        expect (TokOp "=") afterTy >>= fun _ afterEq ->
        let afterEq = skipNL afterEq
        match peek afterEq with
        | Some (TokKeyword KwMatch) ->
            let afterMatch = advance afterEq
            (match peek afterMatch with
             | Some (TokIdent scrut) when scrut = name -> success () (advance afterMatch)
             | _ -> errHere afterMatch $"recursive array '{name}': the match scrutinee must be the array being defined (`match {name} with`)")
            >>= fun () afterScrut ->
            expect (TokKeyword KwWith) afterScrut >>= fun _ afterWith ->
            // --- arm 1 (required): | zero -> zero
            let afterWith = skipNL afterWith
            expect TokPipe afterWith >>= fun _ a1 ->
            (match peek a1 with
             | Some (TokKeyword KwZero) -> success () (advance a1)
             | _ -> errHere a1 $"recursive array '{name}': the first arm must be the base case `| zero -> zero` (extent 0 is the empty array)")
            >>= fun () a2 ->
            expect (TokOp "->") a2 >>= fun _ a3 ->
            (match peek a3 with
             | Some (TokKeyword KwZero) -> success () (advance a3)
             | _ -> errHere a3 $"recursive array '{name}': the base arm's body must be `zero`")
            >>= fun () afterBase ->
            // --- arm 2 (optional seed) / arm 3 (required inductive):
            //     | zero :: n -> zero :: SEED
            //     | prefix :: n -> prefix :: SLICE
            let parseConsArm (toks: Token list) : ParseResult<bool * Ident * Ident * Expr * Expr option> =
                // returns (isSeedArm, prefixOrZeroName, stepVar, sliceExpr, guard)
                expect TokPipe (skipNL toks) >>= fun _ t1 ->
                let isSeed, pfxName, t2res =
                    match peek t1 with
                    | Some (TokKeyword KwZero) -> true, "", Ok (advance t1)
                    | Some (TokIdent p) -> false, p, Ok (advance t1)
                    | _ -> false, "", Error ()
                match t2res with
                | Error () -> errHere t1 $"recursive array '{name}': expected `zero :: n` (seed arm) or `prefix :: n` (inductive arm) pattern"
                | Ok t2 ->
                expect TokColonColon t2 >>= fun _ t3 ->
                expectIdent t3 >>= fun stepVar t4 ->
                // Optional `while GUARD` between the step var and `->`. A bare
                // TokIdent, NOT a keyword (the `repro` where-conjunct
                // precedent) -- a program binding `while` elsewhere still
                // parses. Only the inductive arm may carry it: the seed slice
                // is unconditional by construction.
                (match peek t4 with
                 | Some (TokIdent "while") when not isSeed ->
                     advance t4 |> parseGuardExpr >>= fun g afterG ->
                     success (Some g) afterG
                 | Some (TokIdent "while") ->
                     errHere t4 $"recursive array '{name}': a `while` guard is only legal on the inductive arm (`| prefix :: n while <cond> -> ...`), not on the seed arm"
                 | Some (TokKeyword KwIf) ->
                     errHere t4 $"recursive array '{name}': a recursive-array arm takes a `while` guard, not `if` -- `| prefix :: n while <cond> -> ...` (defined until the guard goes false, frozen after)"
                 | _ -> success None t4)
                >>= fun guard t4 ->
                expect (TokOp "->") t4 >>= fun _ t5 ->
                // Body must open with the SAME constructor head: `zero ::` /
                // `prefix ::` -- this is the productivity check.
                let headOk, t6res =
                    match peek t5, isSeed with
                    | Some (TokKeyword KwZero), true -> true, Ok (advance t5)
                    | Some (TokIdent p), false when p = pfxName -> true, Ok (advance t5)
                    | _ -> false, Error ()
                match t6res with
                | Error () ->
                    let expected = if isSeed then "zero :: <seed slice>" else $"{pfxName} :: <slice expr>"
                    errHere t5 $"recursive array '{name}': the arm body must produce exactly one new slice -- `{expected}`"
                | Ok t6 ->
                let _ = headOk
                expect TokColonColon t6 >>= fun _ t7 ->
                suppressingStructLiterals (fun () -> parseExprImpl t7) >>= fun slice t8 ->
                success (isSeed, pfxName, stepVar, slice, guard) t8
            parseConsArm afterBase >>= fun (isSeed1, pfx1, step1, slice1, guard1) afterArm2 ->
            if isSeed1 then
                // seed arm present; inductive arm must follow
                parseConsArm afterArm2 >>= fun (isSeed2, pfx2, step2, slice2, guard2) afterArm3 ->
                if isSeed2 then
                    errHere afterArm2 $"recursive array '{name}': only one seed arm (`zero :: n`) is allowed; expected the inductive arm `prefix :: n -> prefix :: <slice>`"
                else
                    let sp = rangeSpan tokens afterArm3
                    success {
                        Mutability = BindLet
                        // The PATTERN is the name token, not the whole `let rec
                        // ... match ... with` block the value spans -- otherwise
                        // rename would rewrite the entire declaration.
                        Pattern = mkPat (headSpan tokens) (PatVar name)
                        Type = Some ty
                        Value = mkExpr sp (ExprRecArray {
                            Name = name; SeedArm = Some (step1, slice1)
                            PrefixVar = pfx2; StepVar = step2; SliceExpr = slice2
                            Guard = guard2 })
                    } afterArm3
            else
                let sp = rangeSpan tokens afterArm2
                success {
                    Mutability = BindLet
                    Pattern = mkPat (headSpan tokens) (PatVar name)
                    Type = Some ty
                    Value = mkExpr sp (ExprRecArray {
                        Name = name; SeedArm = None
                        PrefixVar = pfx1; StepVar = step1; SliceExpr = slice1
                        Guard = guard1 })
                } afterArm2
        | _ ->
            errHere afterEq $"recursive array '{name}': the body must be `match {name} with | zero -> zero | prefix :: n -> prefix :: <slice>`"
    | _ ->
        errHere afterName $"recursive array '{name}' requires an explicit type annotation (`let rec {name}: Array<T like Step, ...> = ...`) -- a self-referential definition cannot infer its own type"

and parseLetStmt (tokens: Token list) : ParseResult<Stmt> =
    match peek tokens with
    | Some (TokKeyword KwRec) ->
        parseRecArrayBinding (advance tokens) >>= fun binding remaining ->
        success (StmtLet binding) remaining
    | _ ->
    let mutability, afterMut =
        match peek tokens with
        | Some (TokKeyword KwMut) -> BindMut, advance tokens
        | _ -> BindLet, tokens

    parseLetPattern afterMut >>= fun pat afterPat ->
    parseLetAnnotation afterPat >>= fun ty afterTy ->

    expect (TokOp "=") afterTy >>= fun _ afterEq ->
    parseLetRhs afterEq >>= fun value remaining ->

    success (StmtLet {
        Mutability = mutability
        Pattern = pat
        Type = ty
        Value = value
    }) remaining

