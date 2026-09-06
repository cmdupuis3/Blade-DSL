// The unit-expression and type-expression grammars (both rec-chains),
// including index types. Calls back into the expression grammar only through
// ParserCore's parseExprRef cell, which is what lets this file precede it.
module Blade.ParserTypes

open Blade.Ast
open Blade.Lexer
open Blade.ParserCore

/// Parse a unit expression: meters / seconds, kg * velocity, meters^2
let rec parseUnitExpr (tokens: Token list) : ParseResult<UnitExpr> =
    parseUnitTerm tokens >>= fun left rest ->
    parseUnitExprTail left rest

and parseUnitExprTail (left: UnitExpr) (tokens: Token list) : ParseResult<UnitExpr> =
    match peek tokens with
    | Some (TokOp "*") ->
        parseUnitTerm (advance tokens) >>= fun right rest ->
        parseUnitExprTail (UnitMul (left, right)) rest
    | Some (TokOp "/") ->
        parseUnitTerm (advance tokens) >>= fun right rest ->
        parseUnitExprTail (UnitDiv (left, right)) rest
    | _ -> success left tokens

and parseUnitTerm (tokens: Token list) : ParseResult<UnitExpr> =
    parseUnitAtom tokens >>= fun atom rest ->
    match peek rest with
    | Some (TokOp "^") ->
        let afterCaret = advance rest
        match peek afterCaret with
        | Some (TokInt n) -> success (UnitPow (atom, int n)) (advance afterCaret)
        // Negative exponent (`seconds^-1`, `meters^-2`): the lexer emits the
        // minus as its own operator token, so glue it back on here --
        // reciprocal units (frequencies, decay coefficients) are too common
        // to force through the a/b spelling.
        | Some (TokOp "-") ->
            (match peek (advance afterCaret) with
             | Some (TokInt n) -> success (UnitPow (atom, -(int n))) (advance (advance afterCaret))
             | _ ->
                 let line, col = currentPos (advance afterCaret)
                 error "Expected integer exponent after '^' in unit expression" line col)
        | _ ->
            let line, col = currentPos afterCaret
            error "Expected integer exponent after '^' in unit expression" line col
    | _ -> success atom rest

and parseUnitAtom (tokens: Token list) : ParseResult<UnitExpr> =
    match peek tokens with
    | Some (TokIdent name) -> success (UnitNamed name) (advance tokens)
    // The unity literal `1`: empty dims. Enables the dimensionless-quantity
    // form (`Unit levels: 1`) and reciprocal aliases (`Unit hz = 1 / seconds`).
    | Some (TokInt 1L) -> success UnitOne (advance tokens)
    // A MAGNITUDE factor (`Unit day = 86400 * second`). Dimensionless, so it
    // composes through the same mul/div/pow arms as any other atom; what it
    // contributes is the scale, not a dim.
    | Some (TokInt 0L) ->
        let line, col = currentPos tokens
        error "A unit scale factor cannot be zero (a zero-magnitude unit has no inverse)" line col
    | Some (TokInt n) -> success (UnitScaleLit (bigint n, bigint 1)) (advance tokens)
    | Some (TokFloat v) ->
        (match rationalOfFloatLiteral v with
         | Some (num, den) -> success (UnitScaleLit (num, den)) (advance tokens)
         | None ->
             let line, col = currentPos tokens
             error (sprintf "'%g' is not usable as a unit scale factor (must be finite and non-zero)" v) line col)
    | Some TokLParen ->
        parseUnitExpr (advance tokens) >>= fun expr afterExpr ->
        expect TokRParen afterExpr >>= fun _ remaining ->
        success expr remaining
    | _ ->
        let line, col = currentPos tokens
        error "Expected unit name, '1', or '(' in unit expression" line col

/// Lookahead for a COMPOUND unit expression in type-argument position.
/// Deliberately conservative -- it claims only shapes the type grammar
/// cannot already mean, so every existing type-argument spelling parses
/// exactly as before:
///   - a LONE unit name stays TyNamed (`Float<meter>`, `Float<speed>`);
///   - `name^INT` stays TyVar (`T^2`); a unit name there is disambiguated
///     at LOWERING (units-first policy), not in the grammar;
///   - claimed here: `name * ...` / `name / ...`, `name^-INT` (negative
///     exponents never parsed before), `name^INT` followed by `*`/`/`,
///     a leading unity `1`, a leading MAGNITUDE followed by `*`/`/`
///     (`86400 * second`, `2 * pi / day`, `0.5 * meter`), and a
///     parenthesized group opening with `(name *|/|^ ...` (a tuple type
///     never continues a name that way).
///
/// The magnitude arms exist so an annotation can spell the same thing a
/// `Unit` RHS can. Without them a scale factor parses only when it is not
/// the FIRST token (`Float<day / 2>` worked, `Float<2 * pi / day>` did not),
/// which is precisely the position a conversion target gets written in. No
/// type argument in the language starts with a numeric literal followed by
/// `*` or `/`, so nothing else can mean this.
let isUnitExprArg (tokens: Token list) : bool =
    match tokens |> List.truncate 4 |> List.map (_.Kind) with
    | TokInt 1L :: _ -> true
    | TokInt _ :: TokOp "*" :: _ -> true
    | TokInt _ :: TokOp "/" :: _ -> true
    | TokFloat _ :: TokOp "*" :: _ -> true
    | TokFloat _ :: TokOp "/" :: _ -> true
    | TokIdent _ :: TokOp "*" :: _ -> true
    | TokIdent _ :: TokOp "/" :: _ -> true
    | TokIdent _ :: TokOp "^" :: TokOp "-" :: _ -> true
    | TokIdent _ :: TokOp "^" :: TokInt _ :: TokOp "*" :: _ -> true
    | TokIdent _ :: TokOp "^" :: TokInt _ :: TokOp "/" :: _ -> true
    | TokLParen :: TokIdent _ :: TokOp "*" :: _ -> true
    | TokLParen :: TokIdent _ :: TokOp "/" :: _ -> true
    | TokLParen :: TokIdent _ :: TokOp "^" :: _ -> true
    | _ -> false

let rec parseTypeExpr (tokens: Token list) : ParseResult<TypeExpr> =
    parseTypeAtom tokens >>= fun first rest ->
    match peek rest with
    | Some (TokOp "->") ->
        advance rest |> parseTypeExpr >>= fun ret remaining ->
        success (TyFunc ([first], ret)) remaining
    | _ -> success first rest

and parseTypeAtom (tokens: Token list) : ParseResult<TypeExpr> =
    match peek tokens with
    | Some (TokKeyword KwArray) ->
        // Array<T like I1, I2>
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun elemType afterElem ->
        match peek afterElem with
        | Some (TokKeyword KwLike) ->
            // After 'like', only expect index types (Idx or SymIdx)
            advance afterElem |> sepBy parseIndexType TokComma >>= fun indexTypes afterIndices ->
            expectGt afterIndices >>= fun _ remaining ->
            success (TyArray (elemType, indexTypes)) remaining
        | _ ->
            expectGt afterElem >>= fun _ remaining ->
            success (TyArray (elemType, [])) remaining
    
    | Some (TokKeyword KwPoly) ->
        // Poly<T^r> - arity-polymorphic pack type
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun innerType afterInner ->
        expectGt afterInner >>= fun _ remaining ->
        success (TyPoly innerType) remaining
    
    | Some (TokKeyword KwIdx) ->
        parseIndexType tokens
    
    | Some (TokKeyword KwSymIdx) ->
        parseIndexType tokens
    
    | Some (TokKeyword KwAntisymIdx) ->
        parseIndexType tokens
    
    | Some (TokKeyword KwHermitianIdx) ->
        parseIndexType tokens
    
    | Some (TokKeyword KwCompoundIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwChunked) ->
        parseIndexType tokens

    | Some (TokKeyword KwSparseIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwOrbIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwEnumIdx) ->
        parseIndexType tokens
    
    | Some (TokKeyword KwDepIdx) ->
        // DepIdx in standalone position (alias body, fn return, etc). Inside
        // `Array<T like ...>` this is reached via sepBy parseIndexType
        // directly; this dispatch makes the type writable everywhere.
        parseIndexType tokens

    | Some (TokKeyword KwRaggedIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwIrrepsIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwPgIrrepsIdx) ->
        parseIndexType tokens

    | Some (TokKeyword KwHalo) ->
        // halo<Inner, [offsets]> in TYPE position: a range<> slot only.
        // Deliberately not in parseIndexType -- `Array<T like halo<..>>` must
        // stay illegal (a halo is a traversal transformer, not storage).
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun inner afterInner ->
        expect TokComma afterInner >>= fun _ afterComma ->
        parseSimpleExpr afterComma >>= fun offsets afterOffsets ->
        expectGt afterOffsets >>= fun _ remaining ->
        success (TyHalo (inner, offsets)) remaining

    | Some (TokKeyword KwVoid) ->
        success (TyNamed ("Void", [])) (advance tokens)

    | Some TokLParen ->
        advance tokens |> sepBy parseTypeExpr TokComma >>= fun types afterTypes ->
        expect TokRParen afterTypes >>= fun _ remaining ->
        match types with
        | [single] -> success single remaining
        | _ -> success (TyTuple types) remaining

    // `Tuple<-2>`: the lexer glues `<-` into ONE token, so a negative width
    // never reaches the arm below. Caught here so it gets the width
    // diagnostic rather than falling through to `TyNamed "Tuple"` and
    // failing at the `=` with an unrelated message.
    | Some (TokIdent "Tuple") when (match peek (advance tokens) with Some (TokOp "<-") -> true | _ -> false) ->
        let (line, col) = currentPos (advance tokens)
        errorC "BL1004" "A tuple width cannot be negative: `Tuple<N>` requires an integer literal N >= 2" line col

    | Some (TokIdent "Tuple") when (match peek (advance tokens) with Some (TokOp "<") -> true | _ -> false) ->
        // `Tuple<...>` has TWO spellings, disambiguated by the ARGUMENT LIST:
        //
        //   * a SINGLE INTEGER LITERAL -- `Tuple<2>` -- is the WIDTH-ONLY
        //     annotation (Design C, docs/plan-tuples-vs-arg-packs.md 6b): the
        //     width is written, the element types are inferred.
        //   * ANY other argument list is a COMPONENT-TYPE LIST of width
        //     k >= 2 -- `Tuple<U^1, T<time>^1>` -- and produces exactly the
        //     node a written `(T1, ..., Tk)` produces. `Tuple<A, B>` and
        //     `(A, B)` are therefore THE SAME TYPE, not two types that agree:
        //     unify's equal-length rule, printing, projection, the width
        //     schema (`declaredTupleWidth` already counts a written `TyTuple`)
        //     and codegen are all untouched by this spelling existing.
        //
        // Why both: `TyTupleWidth` lowers its element slots to FRESH
        // inference variables and nothing at a direct call instantiates them
        // (plan 9, `tuples/002`'s note), so a width-only tuple of ARRAYS
        // defaults to `double` and dies in g++. With the components WRITTEN
        // the slots are concrete, which is the whole point of this spelling.
        //
        // The two cannot be MIXED (`Tuple<3, Float64>`): a width is not a type
        // and a type is not a width, so a list containing both is refused
        // outright rather than silently given one of the two readings.
        //
        // Reserved-name story: this follows `Dist`'s SOFT-keyword precedent
        // (the arm just above) rather than `Array`'s hard lexer keyword. The
        // builtin wins in TYPE position whenever the two-token lookahead
        // `Tuple <` matches, so a user `type Tuple<...>` can never shadow it
        // there; a bare `Tuple` with no `<`, and every VALUE named `Tuple`,
        // still falls through to the ordinary TyNamed / identifier paths.
        // Making it a lexer keyword would have broken those.
        let (ltLine, ltCol) = currentPos (advance tokens)
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        // The width reading needs the integer to be the WHOLE list, so the
        // closing `>` (or the `>>` expectGt splits) has to follow immediately.
        let isWidthForm =
            match peek afterLt, peek (advance afterLt) with
            | Some (TokInt _), Some (TokOp ">") | Some (TokInt _), Some (TokOp ">>") -> true
            | _ -> false
        (match peek afterLt with
         | Some (TokInt n) when isWidthForm ->
             // Width must be a positive integer LITERAL >= 2. A 1-tuple does
             // not exist in Blade (`(e)` is grouping, formalism.md:1275) and
             // the 0-tuple has no annotation spelling, so both are refused
             // here rather than lowered into a degenerate IRTTuple. A
             // symbolic width would make pack widths inference-dependent,
             // which 6b rules out explicitly.
             if n < 2L then
                 let (line, col) = currentPos afterLt
                 errorC "BL1004" $"Tuple<{n}> is not a tuple width: `Tuple<N>` requires an integer literal N >= 2 (there is no 1-tuple -- `(e)` is grouping -- and no 0-tuple annotation)" line col
             else
                 expectGt (advance afterLt) >>= fun _ remaining ->
                 success (TyTupleWidth (int n)) remaining
         | Some (TokOp ">") | Some (TokOp ">>") ->
             errorC "BL1004" "`Tuple<>` is empty: write an integer literal width (`Tuple<2>`, element types inferred) or a list of at least two component types (`Tuple<Float64, Float64>`)" ltLine ltCol
         | _ ->
             // COMPONENT-TYPE LIST. Hand-rolled instead of `sepBy` so that an
             // integer in ANY position -- `Tuple<3, Float64>` and
             // `Tuple<Float64, 3>` alike -- gets the mixture diagnostic rather
             // than `parseTypeExpr`'s generic "unexpected token in type".
             let rec parseComponents (acc: TypeExpr list) (toks: Token list) : ParseResult<TypeExpr list> =
                 match peek toks with
                 | Some (TokInt _) ->
                     let (line, col) = currentPos toks
                     errorC "BL1004" "`Tuple<...>` is either a single integer WIDTH (`Tuple<2>`, element types inferred) or a list of component TYPES (`Tuple<Float64, Float64>`) -- the two spellings cannot be mixed" line col
                 | _ ->
                     parseTypeExpr toks >>= fun ty afterTy ->
                     match peek afterTy with
                     | Some TokComma -> parseComponents (ty :: acc) (advance afterTy)
                     | _ -> success (List.rev (ty :: acc)) afterTy
             parseComponents [] afterLt >>= fun comps afterComps ->
             expectGt afterComps >>= fun _ remaining ->
             match comps with
             | [_] ->
                 // Same rule as `Tuple<1>`, one spelling up: there is no
                 // 1-tuple, and `Tuple<T>` reads like one.
                 errorC "BL1004" "`Tuple<T>` is not a tuple type: a component-typed `Tuple<...>` needs at least TWO component types (there is no 1-tuple -- `(e)` is grouping). For the width-only spelling write an integer literal, e.g. `Tuple<2>`." ltLine ltCol
             | _ ->
                 // The SAME node the written `(T1, ..., Tk)` type produces.
                 success (TyTuple comps) remaining)

    | Some (TokIdent "Dist") when (match peek (advance tokens) with Some (TokOp "<") -> true | _ -> false) ->
        // Dist<order, Elem like I1, ..., Ik>: the typed dist tower
        // (ppl/NOTES.md). Order leads because it's an expression (any
        // statically-evaluable int, per the replicate-count contract), which
        // keeps the parse unambiguous; the rest is Array's `Elem like
        // indices` syntax. A bare `Dist` without `<` falls to TyNamed below.
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun orderExpr afterOrder ->
        expect TokComma afterOrder >>= fun _ afterComma ->
        parseTypeExpr afterComma >>= fun elemType afterElem ->
        (match peek afterElem with
         | Some (TokKeyword KwLike) ->
             advance afterElem |> sepBy parseIndexType TokComma >>= fun axes afterAxes ->
             expectGt afterAxes >>= fun _ remaining ->
             success (TyDist (orderExpr, elemType, axes)) remaining
         | _ ->
             expectGt afterElem >>= fun _ remaining ->
             success (TyDist (orderExpr, elemType, [])) remaining)

    | Some (TokIdent name0) ->
        // A dotted run is a qualified type path (`store.index.y`); a bare
        // name is unaffected.
        let (name, afterName) = parseDottedTypeName name0 (advance tokens)
        match peek afterName with
        | Some (TokOp "^") ->
            // Caret marks a type variable: T^0 = scalar, T^2 = rank-2 array,
            // T^r = variable-rank.
            advance afterName |> parseRankExpr >>= fun rankExpr remaining ->
            match rankExpr.Kind with
            | ExprKind.ExprLit (LitInt n) ->
                success (TyVar (name, Some (int n))) remaining
            | _ ->
                success (TyAbstractArray (TyVar (name, None), rankExpr, None)) remaining
        | Some (TokOp "<") ->
            let afterLt = advance afterName
            if isTagWildcardArg afterLt then
                // Tag wildcard: `Nat<_>`, `Int64<_>`, `Float64<_>`. Legality
                // per base type and per position is settled at lowering.
                expectGt (advance afterLt) >>= fun _ remaining ->
                success (TyNamed (name, [TyWildcard])) remaining
            else
            // Parameterized type: Array<T>, MyStruct<Int>, Float<velocity>,
            // or the bounded primitives Float<min=0, max=1> /
            // Float<velocity, min=0, max=1>.
            afterLt |> sepBy parseTypeArg TokComma >>= fun args afterArgs ->
            expectGt afterArgs >>= fun _ remaining ->
            let line, col = currentPos afterLt
            buildTypeApp name args line col remaining >>= fun applied afterApplied ->
            // A TRAILING caret after a type application is the abstract-array
            // spelling with a unit-carrying element: `T<day>^1` is a rank-1
            // abstract array of `T` annotated `day`, `T<day>^0` the scalar.
            // The caret is what marks the type-VARIABLE reading of the head;
            // without it `T<x>` keeps its ordinary named-type meaning, and a
            // real base name under a caret (`Float<day>^1`) stays concrete.
            // Lowering (TypeCheck.lowerTypeExpr, TyAbstractArray) decides
            // which of the two the head is.
            match peek afterApplied with
            | Some (TokOp "^") ->
                advance afterApplied |> parseRankExpr >>= fun rankExpr afterRank ->
                success (TyAbstractArray (applied, rankExpr, None)) afterRank
            | _ -> success applied afterApplied
        | _ ->
            success (TyNamed (name, [])) afterName

    | Some kind ->
        let line, col = currentPos tokens
        error $"Unexpected token in type: {describeToken kind}" line col
    
    | None ->
        errorEof "Expected type but got end of file"

/// One argument of a type application. A named argument (`min=e`) is
/// recognized by two-token lookahead and its value parses with the same
/// static-payload grammar `parseSimpleExpr` that `Idx<n>` uses, so literals
/// (including negative ones), `let static` names, and + - * / arithmetic are
/// all admissible and stop cleanly before `,`/`>`. Anything else is an
/// ordinary positional type.
and parseTypeArg (tokens: Token list) : ParseResult<TypeArg> =
    if isNamedTypeArg tokens then
        let argName = match (List.head tokens).Kind with TokIdent n -> n | _ -> ""
        parseSimpleExpr (advance (advance tokens)) >>= fun value remaining ->
        success (TANamed (argName, value)) remaining
    elif isUnitExprArg tokens then
        // COMPOUND unit annotation (`meter/second`, `second^-1`, `1`,
        // `(meter*second)^2`): the full Unit-declaration grammar, resolved
        // through env.Units at lowering. The sub-parse stops at anything
        // that is not `* / ^`, so it can never eat the `,` or closing `>`
        // of the enclosing constructor. Lone names and `name^INT` are NOT
        // claimed (see isUnitExprArg), so existing spellings are untouched.
        parseUnitExpr tokens >>= fun ue remaining ->
        success (TAPositional (TyUnitExpr ue)) remaining
    else
        parseTypeExpr tokens >>= fun ty remaining ->
        success (TAPositional ty) remaining

/// Assemble a parsed type application. With no named arguments this is just
/// `TyNamed (name, args)`. Named `min=`/`max=` arguments (formalism 2.4) wrap
/// that node in TyBounded; they must follow every positional argument, may
/// not repeat, and no other argument name exists.
and buildTypeApp (name: string) (args: TypeArg list) (line: int) (col: int) (remaining: Token list) : ParseResult<TypeExpr> =
    let positionals = args |> List.choose (function TAPositional t -> Some t | _ -> None)
    let named = args |> List.choose (function TANamed (n, v) -> Some (n, v) | _ -> None)
    if named.IsEmpty then
        success (TyNamed (name, positionals)) remaining
    else
        // Positional-after-named is rejected: the unit/tag argument is what
        // the base type is ABOUT, so it leads.
        let orderOk =
            args
            |> List.fold (fun (sawNamed, ok) a ->
                match a with
                | TANamed _ -> (true, ok)
                | TAPositional _ -> (sawNamed, ok && not sawNamed)) (false, true)
            |> snd
        let badName = named |> List.tryPick (fun (n, _) -> if n = "min" || n = "max" then None else Some n)
        let dup =
            named |> List.map fst |> List.countBy id
            |> List.tryPick (fun (n, c) -> if c > 1 then Some n else None)
        if not orderOk then
            error $"In `{name}<...>`: a positional type argument may not follow a named bound. Write the unit or tag first: `{name}<Unit, min=..., max=...>`" line col
        elif badName.IsSome then
            error $"Unknown named type argument '{badName.Value}=' in `{name}<...>`: only `min=` and `max=` exist" line col
        elif dup.IsSome then
            error $"In `{name}<...>`: `{dup.Value}=` given more than once" line col
        else
            let pick k = named |> List.tryPick (fun (n, v) -> if n = k then Some v else None)
            success (TyBounded (TyNamed (name, positionals), pick "min", pick "max")) remaining

/// The second argument of `SymIdx<k, _>` / `AntisymIdx<k, _>` (seam S1 of the
/// transforms-as-types plan 2.7). The slot is normally `parseSimpleExpr` (an
/// int expression, so the base space is an anonymous dense extent), but also
/// admits an index TYPE for the two forms meaningful as a symmetric-power base:
///   - `IrrepsIdx<spec>`: `SymIdx<k, IrrepsIdx<s>>` is Sym^k of an irreps
///     space, keeping the spec as type identity so two specs of equal
///     total_dim stay distinct (6.3).
///   - `Idx<n>`: the trivial base, lowering to exactly what legacy
///     `SymIdx<k, n>` produces.
/// Anything else -- in particular a bare NAME -- stays on the legacy int
/// reading permanently HERE: `SymIdx<2, N>` parses as "extent N", so existing
/// programs cannot change meaning at the grammar.
///
/// A named alias base (`type S = Idx<n>` then `SymIdx<2, S>`) is resolved one
/// layer down instead, by `TypeCheck.symPowerAliasBase`, and only as a
/// FALLBACK once the extent reading has provably failed -- the name resolves
/// to no value and does name an index type. Keeping that decision out of the
/// parser is what makes the precedence unambiguous: a `let static n` base is
/// a value and never reaches the alias path. Before it existed the alias
/// spelling had no working reading at all, lowering to a symbolic extent that
/// codegen turned into an undeclared `__range<i>` (corpus index-types/239).
and parseSymIdxBase (tokens: Token list) : ParseResult<SymIdxBase> =
    match peek tokens with
    | Some (TokKeyword KwIrrepsIdx) | Some (TokKeyword KwIdx) ->
        parseIndexType tokens >>= fun ty afterTy -> success (SymBaseIndex ty) afterTy
    | _ ->
        parseSimpleExpr tokens >>= fun extent afterExtent -> success (SymBaseExtent extent) afterExtent

/// One level of an `OrbIdx` class: `( INT , + | - )`. A dedicated sub-parser,
/// deliberately not routed through `parseSimpleExpr`: there are no tuple or
/// sign literals in the expression grammar (`Literal` is
/// int/float/bool/string/char/unit, Ast.fs), so `(2,+)` has no expression
/// parse, and inventing one would widen the whole grammar for one type
/// argument. `+`/`-` already lex as `TokOp`, so no lexer change is needed.
///
/// The rank must be a bare non-negative INT LITERAL -- no `let static` name,
/// no arithmetic -- because the level list is part of the type's IDENTITY
/// (two classes with the same total rank but different level lists are
/// different types with different cardinality), and identity depending on
/// static evaluation would have to be compared after folding. Rank 0 is
/// refused: `S_0` is not a group this class can act with, matching OrbRank's
/// `validateLevels`.
and parseOrbLevel (tokens: Token list) : ParseResult<int * bool> =
    expect TokLParen tokens >>= fun _ afterLP ->
    match peek afterLP with
    | Some (TokInt r) ->
        let afterRank = advance afterLP
        if r < 1L then
            let line, col = currentPos afterLP
            error $"OrbIdx level ({r}, ...): a level rank must be >= 1 (S_0 is not a symmetric group; a rank-1 level is the trivial group and is normalized away)" line col
        elif r > 4096L then
            // A per-level sanity cap, not the real bound: the real bound is the
            // class's raw axis count (product of level ranks), checked at
            // lowering where the whole list is in hand, and int64 cell-count
            // overflow bites long before either (7.2). This only catches a
            // typo like `(4000000000,+)` before it reaches an int multiply.
            let line, col = currentPos afterLP
            error $"OrbIdx level ({r}, ...): a level rank above 4096 is almost certainly a typo. The binding constraint on an OrbIdx class is int64 overflow in its cell count, which is reached at far smaller ranks (docs/plan-orbit-index-types.md section 7.2)." line col
        else
        expect TokComma afterRank >>= fun _ afterComma ->
        match peek afterComma with
        | Some (TokOp "+") | Some (TokOp "-") ->
            let isPlus = (peek afterComma = Some (TokOp "+"))
            let afterSign = advance afterComma
            expect TokRParen afterSign >>= fun _ remaining ->
            success (int r, isPlus) remaining
        | _ ->
            let line, col = currentPos afterComma
            error "OrbIdx level: expected a character sign '+' or '-' after the rank, as in (2,+) or (2,-)" line col
    | _ ->
        let line, col = currentPos afterLP
        error "OrbIdx level: expected an integer rank literal, as in (2,+)" line col

/// The bracketed level list `[(r1,s1), ..., (rd,sd)]`. Empty (`[]`) is legal
/// and denotes the trivial class (docs/plan-orbit-index-types.md 3: `Idx<n>`
/// is `OrbIdx<[],n>` in normal form).
and parseOrbLevels (tokens: Token list) : ParseResult<(int * bool) list> =
    expect TokLBracket tokens >>= fun _ afterLB ->
    let rec loop (acc: (int * bool) list) (toks: Token list) : ParseResult<(int * bool) list> =
        match peek toks with
        | Some TokRBracket -> success (List.rev acc) (advance toks)
        | _ ->
            parseOrbLevel toks >>= fun lvl afterLvl ->
            match peek afterLvl with
            | Some TokComma -> loop (lvl :: acc) (advance afterLvl)
            | Some TokRBracket -> success (List.rev (lvl :: acc)) (advance afterLvl)
            | _ ->
                let line, col = currentPos afterLvl
                error "OrbIdx level list: expected ',' or ']' after a (rank, sign) level" line col
    loop [] afterLB

// Index types (Idx<extent>, SymIdx<arity, extent>, ...) are self-contained with their own < > brackets.
and parseIndexType (tokens: Token list) : ParseResult<TypeExpr> =
    match peek tokens with
    | Some (TokKeyword KwIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun extent afterExtent ->
        expectGt afterExtent >>= fun _ remaining ->
        success (TyIdx extent) remaining
    
    | Some (TokKeyword KwSymIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseLiteral afterLt >>= fun rankLit afterRank ->
        expect TokComma afterRank >>= fun _ afterComma ->
        parseSymIdxBase afterComma >>= fun baseIdx afterBase ->
        expectGt afterBase >>= fun _ remaining ->
        let rank = match rankLit with LitInt n -> int n | _ -> 2
        success (TySymIdx (rank, baseIdx)) remaining

    | Some (TokKeyword KwAntisymIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseLiteral afterLt >>= fun rankLit afterRank ->
        expect TokComma afterRank >>= fun _ afterComma ->
        parseSymIdxBase afterComma >>= fun baseIdx afterBase ->
        expectGt afterBase >>= fun _ remaining ->
        let rank = match rankLit with LitInt n -> int n | _ -> 2
        success (TyAntisymIdx (rank, baseIdx)) remaining

    // OrbIdx<[(r1,s1), ..., (rd,sd)], n>: the flat iterated-wreath class
    // (docs/plan-orbit-index-types.md 2). Levels are outermost-last. The
    // extent slot reuses `parseSymIdxBase` verbatim, so it accepts exactly what
    // `SymIdx<k, _>`'s second argument accepts and the two forms cannot drift.
    | Some (TokKeyword KwOrbIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseOrbLevels afterLt >>= fun levels afterLevels ->
        expect TokComma afterLevels >>= fun _ afterComma ->
        parseSymIdxBase afterComma >>= fun baseIdx afterBase ->
        expectGt afterBase >>= fun _ remaining ->
        success (TyOrbIdx (levels, baseIdx)) remaining

    | Some (TokKeyword KwHermitianIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun extent afterExtent ->
        expectGt afterExtent >>= fun _ remaining ->
        success (TyHermitianIdx extent) remaining

    // Chunked<I, spec>: I segmented (docs/plans/structural/07 §2.1). The
    // inner is any type expression naming an axis; the spec is a simple
    // expression -- a literal edge, the identifier `store`, or a list of
    // `[store, chunking]` pairs -- judged at type registration.
    | Some (TokKeyword KwChunked) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseTypeExpr afterLt >>= fun inner afterInner ->
        expect TokComma afterInner >>= fun _ afterComma ->
        parseSimpleExpr afterComma >>= fun spec afterSpec ->
        expectGt afterSpec >>= fun _ remaining ->
        success (TyChunked (inner, spec)) remaining

    // CompoundIdx<mask>: masked product space (formalism 4.5). Mask is a
    // runtime array expression whose rank determines the compound's arity,
    // recovered from the mask's type at lowering.
    | Some (TokKeyword KwCompoundIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun mask afterMask ->
        expectGt afterMask >>= fun _ remaining ->
        success (TyCompoundIdx mask) remaining

    // SparseIdx<keys>: explicit valid-tuple enumeration (formalism 3.5). Keys
    // is a rank-1 array of Nat tuples (`let static` or runtime); arity is
    // implicit from the tuple arity, recovered at lowering like CompoundIdx.
    | Some (TokKeyword KwSparseIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun keys afterKeys ->
        expectGt afterKeys >>= fun _ remaining ->
        success (TySparseIdx keys) remaining

    | Some (TokKeyword KwEnumIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun values afterValues ->
        expectGt afterValues >>= fun _ remaining ->
        success (TyEnumIdx values) remaining

    // DepIdx<outer, lambda(i) -> body> | DepIdx<outer, func>
    // Both forms produce TyDepIdx(outer, param, body); the eta-reduced form
    // is desugared to a lambda whose body is `func(<param>)`.
    | Some (TokKeyword KwDepIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseIndexType afterLt >>= fun outer afterOuter ->
        expect TokComma afterOuter >>= fun _ afterComma ->
        // Body is either `lambda(i) -> Idx<...>` or a bare function name.
        match peek afterComma with
        | Some (TokKeyword KwLambda) ->
            // `lambda(name) -> idxBody`; body is an index type, not a general type.
            let afterLambda = advance afterComma
            expect TokLParen afterLambda >>= fun _ afterLParen ->
            match peek afterLParen with
            | Some (TokIdent paramName) ->
                let afterName = advance afterLParen
                expect TokRParen afterName >>= fun _ afterRParen ->
                expect (TokOp "->") afterRParen >>= fun _ afterArrow ->
                parseIndexType afterArrow >>= fun bodyTy afterBody ->
                expectGt afterBody >>= fun _ remaining ->
                success (TyDepIdx (outer, paramName, bodyTy)) remaining
            | _ ->
                let line, col = currentPos afterLParen
                error "DepIdx lambda: expected single parameter name" line col
        | Some (TokIdent funcName) ->
            // Eta-reduced form `DepIdx<outer, func>` desugars to
            // `DepIdx<outer, lambda(__d_i) -> func(__d_i)>` with a fresh
            // parameter name to avoid collisions. The synthesized body is a
            // TypeExpr (TyNamed(func, [paramRef])) even though `func(i)` is
            // conceptually an index type; lowering resolves it through the
            // type-def lookup path.
            let afterName = advance afterComma
            expectGt afterName >>= fun _ remaining ->
            let paramName = "__d_i"
            let bodyTy = TyNamed (funcName, [TyNamed (paramName, [])])
            success (TyDepIdx (outer, paramName, bodyTy)) remaining
        | _ ->
            let line, col = currentPos afterComma
            error "DepIdx: expected lambda or function name as second argument" line col

    // RaggedIdx<lengths>: externally parameterized via a lengths array.
    // RaggedIdx<_>: opaque-extent variant, used in kernel-parameter types
    // (`lambda(g: Array<T like RaggedIdx<_>>) -> ...`) where the extent is
    // supplied by the peel context, not declared up front. `_` means "opaque
    // extent" only when it's the sole argument (immediately followed by `>`
    // or `>>`, which expectGt splits) -- any other position (`_ + 1`, `_, x`,
    // etc.) is left for the lengths-expr parser, so the wildcard can't
    // accidentally swallow a piece of a real expression.
    | Some (TokKeyword KwRaggedIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        let isOpaqueWildcard =
            match afterLt with
            | t1 :: t2 :: _ when
                t1.Kind = TokUnderscore &&
                (t2.Kind = TokOp ">" || t2.Kind = TokOp ">>") -> true
            | _ -> false
        if isOpaqueWildcard then
            let afterUnderscore = advance afterLt
            expectGt afterUnderscore >>= fun _ remaining ->
            success TyRaggedIdxOpaque remaining
        else
            parseSimpleExpr afterLt >>= fun lengthsExpr afterLengths ->
            expectGt afterLengths >>= fun _ remaining ->
            success (TyRaggedIdx lengthsExpr) remaining

    // IrrepsIdx<spec>: spec is a static-expression argument, a `let static`
    // name or an inline array-of-triples literal ([(l, parity, mult), ...]),
    // resolved at typecheck via StaticEval. Call syntax (IrrepsIdx<sh_spec(2)>)
    // is not part of the simple-expression grammar -- bind a `let static` first.
    | Some (TokKeyword KwIrrepsIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        parseSimpleExpr afterLt >>= fun specExpr afterSpec ->
        expectGt afterSpec >>= fun _ remaining ->
        success (TyIrrepsIdx specExpr) remaining

    // PgIrrepsIdx<GROUP, spec>: the point-group block-spec member (transforms-
    // as-types plan 3.6). GROUP is a bare identifier, not an expression --
    // point-group names are frozen registry data ({C4, D4}), so the slot is a
    // name the way `Idx<n>`'s slot is a number. The registry lookup (with the
    // unknown-group diagnostic) and spec decode happen at lowering. `spec` is
    // the same static-expression argument IrrepsIdx takes.
    | Some (TokKeyword KwPgIrrepsIdx) ->
        advance tokens |> expect (TokOp "<") >>= fun _ afterLt ->
        (match peek afterLt with
         | Some (TokIdent groupName) ->
             let afterGroup = advance afterLt
             expect TokComma afterGroup >>= fun _ afterComma ->
             parseSimpleExpr afterComma >>= fun specExpr afterSpec ->
             expectGt afterSpec >>= fun _ remaining ->
             success (TyPgIrrepsIdx (groupName, specExpr)) remaining
         | _ ->
             let line, col = currentPos afterLt
             error "PgIrrepsIdx: expected a point-group NAME as the first argument -- PgIrrepsIdx<C4, SPEC>" line col)

    | Some (TokIdent name0) ->
        // Named index type alias (e.g. type RegionIdx = Idx<3>; ...like RegionIdx),
        // or a qualified provider axis (`like store.index.y`). Resolved at
        // typecheck via lowerIndexType / TyNamed lookup against TDIIndexType
        // or TDIEnumIdx.
        let (name, afterName) = parseDottedTypeName name0 (advance tokens)
        match peek afterName with
        | Some (TokOp "<") ->
            let afterLt = advance afterName
            if isTagWildcardArg afterLt then
                // A wildcard in a storage-index slot is illegal, but parsing it
                // lets lowerIndexType report the position error with a real span
                // instead of a bare "unexpected token" from the token stream.
                expectGt (advance afterLt) >>= fun _ remaining ->
                success (TyNamed (name, [TyWildcard])) remaining
            else
            afterLt |> sepBy parseTypeExpr TokComma >>= fun args afterArgs ->
            expectGt afterArgs >>= fun _ remaining ->
            success (TyNamed (name, args)) remaining
        | _ ->
            success (TyNamed (name, [])) afterName
    
    | Some kind ->
        let line, col = currentPos tokens
        error $"Expected index type (Idx, SymIdx, AntisymIdx, HermitianIdx, EnumIdx, DepIdx, RaggedIdx, IrrepsIdx, PgIrrepsIdx, or a named index type alias) but got {describeToken kind}" line col
    
    | None ->
        errorEof "Expected index type but got end of file"

