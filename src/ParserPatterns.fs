// Pattern grammar, ident lists, and where-clause parsing (comm/anticomm/omp/
// cuda/tdim/reynolds and friends), plus the removed-for-in detector shared
// with the expression grammar.
module Blade.ParserPatterns

open Blade.Ast
open Blade.Lexer
open Blade.ParserCore
open Blade.ParserTypes

// Pattern Parsing

let rec parsePattern (tokens: Token list) : ParseResult<Pattern> =
    parseAtomicPattern tokens >>= fun left rest ->
    match peek rest with
    | Some TokColonColon ->
        advance rest |> parsePattern >>= fun right remaining ->
        success (mkPat (mergeSpan left.Span right.Span) (PatCons (left, right))) remaining
    | _ -> success left rest

and parseAtomicPattern (tokens: Token list) : ParseResult<Pattern> =
    match peek tokens with
    | Some TokUnderscore ->
        success (mkPat (headSpan tokens) PatWildcard) (advance tokens)

    | Some (TokIdent name) ->
        let afterName = advance tokens
        match peek afterName with
        | Some TokLBrace ->
            // Struct pattern: Name { field1, field2: pat }
            parseStructPattern name afterName
        | Some TokLParen ->
            // Variant pattern with data: Some(x)
            advance afterName |> parsePattern >>= fun inner afterInner ->
            expect TokRParen afterInner >>= fun _ remaining ->
            success (mkP tokens remaining (PatVariant (name, Some inner))) remaining
        | _ ->
            // Could be a variant without data (like None) or a variable;
            // treated as a variable here, with variant detection at typecheck.
            success (mkPat (headSpan tokens) (PatVar name)) afterName

    | Some (TokInt v) ->
        success (mkPat (headSpan tokens) (PatLit (LitInt v))) (advance tokens)

    | Some (TokBool v) ->
        success (mkPat (headSpan tokens) (PatLit (LitBool v))) (advance tokens)

    | Some (TokString v) ->
        success (mkPat (headSpan tokens) (PatLit (LitString v))) (advance tokens)

    | Some TokLParen ->
        advance tokens |> sepBy parsePattern TokComma >>= fun pats afterPats ->
        expect TokRParen afterPats >>= fun _ remaining ->
        match pats with
        | [] -> success (mkP tokens remaining (PatLit LitUnit)) remaining
        | [single] -> success single remaining
        | _ -> success (mkP tokens remaining (PatTuple pats)) remaining
    
    | Some kind ->
        let line, col = currentPos tokens
        error $"Unexpected token in pattern: {describeToken kind}" line col
    
    | None ->
        errorEof "Expected pattern but got end of file"

/// Parse struct pattern: Name { field1, field2: pat, ... }
and parseStructPattern (name: string) (tokens: Token list) : ParseResult<Pattern> =
    expect TokLBrace tokens >>= fun _ afterBrace ->
    
    let rec parseFieldPats toks =
        let toks = skipNL toks
        match peek toks with
        | Some TokRBrace -> success [] (advance toks)
        | Some (TokIdent fieldName) ->
            let afterFieldName = advance toks
            match peek afterFieldName with
            | Some TokColon ->
                parsePattern (advance afterFieldName) >>= fun pat afterPat ->
                let afterPat = skipNL afterPat
                match peek afterPat with
                | Some TokComma ->
                    parseFieldPats (advance afterPat) >>= fun rest remaining ->
                    success ((fieldName, pat) :: rest) remaining
                | Some TokRBrace ->
                    success [(fieldName, pat)] (advance afterPat)
                | _ ->
                    let line, col = currentPos afterPat
                    error "Expected ',' or '}' in struct pattern" line col
            | Some TokComma ->
                // shorthand: field (binds to variable of same name)
                parseFieldPats (advance afterFieldName) >>= fun rest remaining ->
                success ((fieldName, mkPat (headSpan toks) (PatVar fieldName)) :: rest) remaining
            | Some TokRBrace ->
                success [(fieldName, mkPat (headSpan toks) (PatVar fieldName))] (advance afterFieldName)
            | _ ->
                let line, col = currentPos afterFieldName
                error "Expected ':' or ',' in struct pattern" line col
        | _ ->
            let line, col = currentPos toks
            error "Expected field name or '}' in struct pattern" line col
    
    parseFieldPats afterBrace >>= fun fields remaining ->
    // Span covers the `{ ... }` body (the type name is consumed by the caller
    // before this function is entered, so it is not part of this range).
    success (mkP tokens remaining (PatStruct (name, fields))) remaining

// Expression Parsing - Precedence Climbing

// Precedence levels (lowest to highest):
// 1. Assignment =
// 2. Pipeline |>
// 3. Choice <|>
// 4. Parallel <&>
// 5. Bind >>=
// 6. Apply <@>
// 7. Array product <*>
// 8. Or ||
// 9. And &&
// 10. Equality == !=
// 11. Comparison < <= > >=
// 12. Cons ::
// 13. Additive + -
// 14. Multiplicative * / %
// 15. Power **
// 16. Unary - !
// 17. Postfix (call, index, field)
// 18. Primary (literals, variables, etc.)

let parseIdentList (tokens: Token list) : ParseResult<string list> =
    let rec loop acc toks =
        match toks with
        | t :: rest when t.Kind.IsTokIdent ->
            let name = match t.Kind with TokIdent n -> n | _ -> ""
            match rest with
            | t2 :: rest2 when t2.Kind = TokComma -> loop (name :: acc) rest2
            | _ -> Ok (List.rev (name :: acc), rest)
        | _ -> 
            if List.isEmpty acc then
                let line, col = currentPos toks
                errorC "BL1001" "Expected identifier" line col
            else
                Ok (List.rev acc, toks)
    loop [] tokens

/// Arguments of an open where-conjunct (`<name>(...)` / `<alias>.<name>(...)`,
/// the Blade.Constraints registry surface): identifiers (the comm/indep/
/// galilean shape, where an argument names a parameter) or int literals,
/// rendered into the same `string list` the registry carries. The literal
/// form is what a group-parameter conjunct needs: `where ml.perm_equiv(4)`
/// names the node-axis extent of the S_n certificate, not a parameter
/// (MLPerm.fs parses the string back, and also accepts a `let static` name).
/// Widening the token set here cannot loosen comm/antisymm groups, which
/// keep the ident-only `parseIdentList`.
let parseConjunctArgList (tokens: Token list) : ParseResult<string list> =
    let rec loop acc toks =
        let taken =
            match toks with
            | t :: rest ->
                match t.Kind with
                | TokIdent n -> Some (n, rest)
                | TokInt n -> Some (string n, rest)
                | _ -> None
            | [] -> None
        match taken with
        | Some (text, rest) ->
            match rest with
            | t2 :: rest2 when t2.Kind = TokComma -> loop (text :: acc) rest2
            | _ -> Ok (List.rev (text :: acc), rest)
        | None ->
            if List.isEmpty acc then
                let line, col = currentPos toks
                errorC "BL1001" "Expected an identifier or an integer literal" line col
            else
                Ok (List.rev acc, toks)
    loop [] tokens

// Where clause parsing (used by both function declarations and lambdas)
/// Parse the body of omp(...): comma-separated `ident: int` pairs, e.g.
/// omp(a: 2, b: 1) => [("a",2); ("b",1)]. The parenthesised form is optional --
/// bare `omp` yields `Omp { Vars = [] }`, read downstream as "auto": every
/// consumer treats an empty depth list as the outermost-level license
/// (IRStorage.buildLoopNestCodeGen's `licenseUnresolved` fallback), and the
/// fold-kernel gate (docs/plan-cpp-perf-exploitation.md) has no per-argument
/// depth to name at all -- a reduce walks one axis, so the only question is
/// whether it may be reordered.
let rec private parseOmpArgs (acc: (string * int) list) (tokens: Token list) : ParseResult<(string * int) list> =
    expectIdent tokens >>= fun name afterName ->
    expect TokColon afterName >>= fun _ afterColon ->
    match afterColon with
    | t :: rest ->
        match t.Kind with
        | TokInt n ->
            let acc' = (name, int n) :: acc
            match rest with
            | t2 :: rest2 when t2.Kind = TokComma -> parseOmpArgs acc' rest2
            | _ -> Ok (List.rev acc', rest)
        | _ -> error $"Expected integer in omp(...) but got {describeToken t.Kind}" t.Line t.Col
    | [] -> errorEof "Expected integer in omp(...) but got end of file"

let parseWhereClause (tokens: Token list) : ParseResult<WhereClause> =
    // `par` accumulates parallelization strategies as a LIST (see
    // WhereClause.Parallel), outer backend first. Mixed parallelism allows at
    // most two: `mpi, omp(...)` (ranks outer, threads within each rank's
    // slab) and `mpi, cuda(...)` (ranks outer, device kernels per rank). This
    // order table is closed and purely syntactic, so illegal pairs reject
    // here with steering; shape-dependent checks (device eligibility, fold
    // reassociation) stay in codegen.
    let rec loop comms (antis: string list list) (par: ParallelStrategy list) custom (repro: bool) toks =
        let isOmp (s: ParallelStrategy) = s.IsOmp
        let isCuda (s: ParallelStrategy) = s.IsCuda
        let isMpi (s: ParallelStrategy) = s.IsMpi
        let rejectPair (line: int) (col: int) (incoming: string) =
            if List.length par >= 2 then
                error "At most two parallelization strategies per where-clause (outer, inner)" line col
            elif par |> List.exists isCuda then
                error "CUDA owns the whole device leaf -- nothing nests inside a device kernel (write the cuda backend LAST: `where mpi, cuda(...)`)" line col
            else
                match incoming with
                | "omp" when par |> List.exists isOmp ->
                    error "omp requested twice -- one omp(...) clause lists all parallel dims" line col
                | "mpi" when par |> List.exists isMpi ->
                    error "mpi requested twice" line col
                | "cuda" -> // par starts with Omp here (cuda/dup handled above)
                    error "A single kernel cannot be both OpenMP-host and CUDA-device -- use `where cuda(...)` for the device kernel (omp-driven sections over independent cuda leaves are future work)" line col
                | "mpi" -> // par starts with Omp: the rejected OpenMP-outer/MPI-inner order
                    error "OpenMP-outer / MPI-inner is not supported: MPI ranks are processes fixed at launch and cannot nest inside host threads -- did you mean `where mpi, omp(...)` (ranks outer, threads within each rank)?" line col
                | _ ->
                    error "Unsupported parallelization strategy combination" line col
        match peek toks with
        | Some (TokKeyword KwComm) ->
            advance toks |> expect TokLParen >>= fun _ afterLParen ->
            parseIdentList afterLParen >>= fun names afterNames ->
            expect TokRParen afterNames >>= fun _ remaining ->
            loop (names :: comms) antis par custom repro remaining
        | Some (TokKeyword KwAntisymm) ->
            // `anticomm(a, b, ...)`: the signed sibling of `comm`. Needs at
            // least two parameters -- an anticommutativity relation is about
            // an exchange, so a one-name group names no swap at all.
            let line, col = currentPos toks
            advance toks |> expect TokLParen >>= fun _ afterLParen ->
            parseIdentList afterLParen >>= fun names afterNames ->
            expect TokRParen afterNames >>= fun _ remaining ->
            if List.length names < 2 then
                error "anticomm(...) needs at least two parameters: anticommutativity is a relation between two exchanged arguments (f(b, a) = -f(a, b))" line col
            else
                loop comms (names :: antis) par custom repro remaining
        | Some (TokKeyword KwOmp) ->
            // Legal as: sole strategy, or the INNER of `mpi, omp(...)`.
            if not (par = [] || par = [Mpi]) then
                let line, col = currentPos toks
                rejectPair line col "omp"
            else
                // `omp(a: n, ...)`  OR  bare `omp` (auto). Same optional-paren
                // shape the `cuda` arm below already uses.
                let afterOmp = advance toks
                match peek afterOmp with
                | Some TokLParen ->
                    expect TokLParen afterOmp >>= fun _ afterLParen ->
                    parseOmpArgs [] afterLParen >>= fun pairs afterArgs ->
                    expect TokRParen afterArgs >>= fun _ remaining ->
                    loop comms antis (par @ [Omp { Vars = pairs }]) custom repro remaining
                | _ ->
                    loop comms antis (par @ [Omp { Vars = [] }]) custom repro afterOmp
        | Some (TokKeyword KwMpi) ->
            // Legal only as the sole (and hence OUTER) strategy.
            if not (List.isEmpty par) then
                let line, col = currentPos toks
                rejectPair line col "mpi"
            else
                // bare `mpi`: rank count is supplied at launch (mpiexec -n N)
                loop comms antis (par @ [Mpi]) custom repro (advance toks)
        | Some (TokKeyword KwCuda) ->
            // Legal as: sole strategy, or the INNER of `mpi, cuda(...)`.
            if not (par = [] || par = [Mpi]) then
                let line, col = currentPos toks
                rejectPair line col "cuda"
            else
                // cuda  OR  cuda(block: N)
                let afterCuda = advance toks
                match peek afterCuda with
                | Some TokLParen ->
                    expect TokLParen afterCuda >>= fun _ afterLParen ->
                    expectIdent afterLParen >>= fun key afterKey ->
                    if key <> "block" then
                        let line, col = currentPos afterLParen
                        error $"Expected 'block' in cuda(...) but got '{key}'" line col
                    else
                        expect TokColon afterKey >>= fun _ afterColon ->
                        match afterColon with
                        | t :: rest ->
                            match t.Kind with
                            | TokInt n ->
                                expect TokRParen rest >>= fun _ remaining ->
                                loop comms antis (par @ [Cuda { BlockSize = int n }]) custom repro remaining
                            | _ -> error $"Expected integer block size but got {describeToken t.Kind}" t.Line t.Col
                        | [] -> errorEof "Expected integer block size but got end of file"
                | _ ->
                    // bare `cuda` => default block size
                    loop comms antis (par @ [Cuda { BlockSize = 256 }]) custom repro afterCuda
        | Some (TokIdent "repro") when (match peek (advance toks) with Some TokLParen -> false | _ -> true) ->
            // Bare `repro`: reproducible-evaluation demand. Idempotent (a
            // second spelling changes nothing); its one illegal partner --
            // cuda -- is checked at the end of the clause so the two may
            // appear in either order. `repro(` falls through to the open
            // conjunct arm and errors there with the registry vocabulary.
            loop comms antis par custom true (advance toks)
        | Some (TokIdent alias) when (match peek (advance toks) with Some TokDot -> true | _ -> false) ->
            // Module-qualified constraint conjunct: `<alias>.<name>(<idents>)`
            // (e.g. `p.indep(a, b)` with `import ppl as p`). Stored as DATA
            // with the dotted name ("p.indep", args); the owning module's
            // elaboration stage normalizes the alias, and the CHECKER
            // dispatches through the Blade.Constraints registry.
            advance (advance toks) |> expectIdent >>= fun name afterName ->
            expect TokLParen afterName >>= fun _ afterLParen ->
            parseConjunctArgList afterLParen >>= fun args afterArgs ->
            expect TokRParen afterArgs >>= fun _ remaining ->
            loop comms antis par (custom @ [(alias + "." + name, args)]) repro remaining
        | Some (TokIdent name) when (match peek (advance toks) with Some TokLParen -> true | _ -> false) ->
            // Open constraint conjunct: `<name>(<idents>)` for any identifier
            // the grammar doesn't own. Parsed as data (name, args) and
            // dispatched by the checker through the Blade.Constraints
            // registry; an unregistered name errors there with the
            // registered vocabulary, not here. (Module-owned keywords like
            // PPL's `indep` are qualified -- see the dotted arm above.)
            advance toks |> expect TokLParen >>= fun _ afterLParen ->
            parseConjunctArgList afterLParen >>= fun args afterArgs ->
            expect TokRParen afterArgs >>= fun _ remaining ->
            loop comms antis par (custom @ [(name, args)]) repro remaining
        | Some TokComma ->
            loop comms antis par custom repro (advance toks)
        | _ ->
            // A parameter cannot be declared both commutative and
            // antisymmetric (nor antisymmetric twice): the two clauses would
            // fuse into one axis group whose storage must be simultaneously
            // inclusive and strict, so iteration and layout would disagree.
            // Checked syntactically, on names, before either list is resolved
            // to indices. (Overlapping comm groups stay legal.)
            let antiNames = antis |> List.collect id
            let commNames = comms |> List.collect id
            let dupInAnti =
                antiNames |> List.countBy id |> List.tryPick (fun (n, c) -> if c > 1 then Some n else None)
            let bothWays = antiNames |> List.tryFind (fun n -> List.contains n commNames)
            let line, col = currentPos toks
            match dupInAnti, bothWays with
            | Some n, _ ->
                error $"'{n}' appears in two anticomm(...) groups: an argument belongs to at most one anticommutativity relation (the groups would fuse into one axis with two contradictory layouts)" line col
            | _, Some n ->
                error $"'{n}' is declared both comm(...) and anticomm(...): one exchange cannot be both commutative (inclusive triangle, diagonal stored) and anticommutative (strict triangle, diagonal zero)" line col
            | None, None ->
            // repro x cuda: a device kernel's contraction and rounding are the
            // GPU's, not the interpreter's -- the reproducibility demand cannot
            // be discharged there, so the pair refuses rather than silently
            // meaning less than it says. Checked here (order-independent),
            // like the comm/anticomm overlap above.
            if repro && (par |> List.exists (fun s -> s.IsCuda)) then
                error "`repro` cannot be combined with `cuda`: a device kernel's contraction and rounding are the GPU's own, so the reproducibility demand (the interpreter's operation sequence) cannot be discharged there" line col
            else
            success {
                Commutativity = List.rev comms
                Antisymmetry = List.rev antis
                Parallel = par
                Repro = repro
                TDims = []
                Custom = custom
            } toks
    loop [] [] [] [] false tokens

/// The BL1003 steer for the removed imperative `for`. Shared verbatim by the
/// two positions the shape can appear in -- a block statement (parseBlock) and
/// a top-level statement (parseDecl) -- so the wording cannot drift between
/// them and corpus `ERROR-CONTAINS` pins match either site.
let forInRemovedMsg =
    "The imperative `for x in a..b { ... }` statement has been removed. Re-express sequential recurrences as a recursive array (`let rec q: Array<T like Step, ...> = match q with | zero -> zero | prefix :: n -> prefix :: <slice>`), folds as `reduce(...)`, and parallel maps as `method_for(range<...>) <@> lambda(...)`. See formalism 7.5."

/// Does the token stream open with the REMOVED imperative `for IDENT in ...`
/// (as opposed to the surviving loop-object `for (A, B) in virtualArray`)?
/// Both parseBlock and parseDecl gate their BL1003 steer on this.
let internal isImperativeForIn (tokens: Token list) : bool =
    match peek tokens with
    | Some (TokKeyword KwFor) ->
        match peek (advance tokens) with
        | Some (TokIdent _) ->
            (match peek (advance (advance tokens)) with
             | Some (TokKeyword KwIn) -> true
             | _ -> false)
        | _ -> false
    | _ -> false

