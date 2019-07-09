
open System.IO
open System.Text.RegularExpressions

#load "parser.fs"

/// Union of all the relevant token types
type Token = 
    | NewLine
    | WhiteSpace
    | Symbol of char
    | Str of string
    | Int of int
    | Other of string

/// Tokenizer helper pattern for regexes
let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

/// Convert string to a token based on a regex pattern
let toToken = function
    | Match @"^\n|^\r"               s -> s, Token.NewLine
    | Match @"^\s+"                  s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>|^;|^:|^~|^\*|^="    s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z][a-zA-Z0-9]*" s -> s, Token.Str s
    | Match @"^\d+"                  s -> s, Token.Int (int s)
    | Match @"."                     s -> s, Token.Other (string s)
    | _ -> failwith "Invalid Token"

/// Convert string into a list of parsable tokens
let tokenize (s: string) =
    // Convert substrings to tokens, starting at position 'index' in the string. 
    let rec tokenize' index (s: string) =
        if index = s.Length then []
        else
            let text, token = toToken (s.Substring index)
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    // strip out the whitespace tokens; note that since new line characters are distinct, they will remain.
    |> List.choose (function Token.WhiteSpace -> None | t -> Some t)

/// Pragma clause type; a tuple of the clause name and a list of arguments
type Clause = string * Token list

/// Pragma scope type; contains an unparsed list of tokens visible to the pragma
type BlockScope =
    | Nested of BlockScope list
    | Block of Token list
    | Line of Token list

/// Pragma type; consists of a directive, a list of clauses, and a scope
type Pragma = Clause * Clause list * BlockScope

/// Pattern for pragma scopes; basically dumps all the tokens inside the pragma scope into a buffer, and returns the tail separately
let rec (|ScopePattern|_|) = function
    | Token.Symbol '{' :: tail ->
        /// Make a list of tokens inside this scope by counting the number of braces (terrible, I know)
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol '}' :: Token.NewLine :: tail' -> 
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: Token.NewLine :: h', t'
                else
                    [], tail'
            | Token.Symbol '}' :: tail' -> 
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: h', t'
                else
                    [], tail'
            | Token.Symbol '{' :: tail' -> 
                let h', t' = toScope tail' (ctr+1)
                Token.Symbol '{' :: h', t'
            | head' :: tail' -> 
                let h', t' = toScope tail' ctr
                head' :: h', t'
            | _ -> failwith "asdfsdf"
        let scope, t = toScope tail 0
        Some (BlockScope.Block (scope), (t: Token list))
    | head :: tail -> 
        match tail with
        | ScopePattern t -> Some t
        | _ -> None
    | _ -> None

/// Parse code for all the scopes and return a list of them.
let rec parseScopes (s: Token list) =
    let rec parse' = function
    | ScopePattern (v,t) -> v :: (parse' t)
    | head :: tail -> parse' tail
    | [] -> []
    BlockScope.Nested (parse' s)

//match tokenize "asd(8) {fjfj jfjf }\n" with | ScopePattern(s) -> Some(s) |_->None;;
//let a = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopePattern v -> Some(v) |_-> None ;;
//let b = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopesPattern v -> Some(v) |_-> None ;;

/// Pattern for pragmas applied to individual lines; basically dumps all the tokens inside the pragma scope into a buffer, and returns the tail separately
let rec (|LinePattern|_|) = function
    | head :: Token.Symbol ';' :: Token.NewLine :: tail | head :: Token.Symbol ';' :: tail ->
        Some (head :: [Token.Symbol ';'], tail)
    | head :: tail ->
        let h, t = 
            match tail with
            | LinePattern t -> t
            | _ -> [], []
        Some (head :: h, t)
    | _ -> None

//match tokenize "asd(8);\n fjfjfj" with | LinePattern(s) -> Some(s) |_->None;;
//match tokenize "asd(8);\n asdf;\n fjfjfjfj 9;\n" with | LinesPattern(s) -> Some(s) |_->None;;

/// Make a list of clause arguments and return the tail
let rec toElements s = 
    match s with
    | head :: Token.Symbol ',' :: tail -> 
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []

/// Pattern for individual pragma clauses
let rec (|ClausePattern|_|) = function
    | Token.NewLine :: tail -> None
    | Token.Str head :: Token.Symbol '(' :: tail -> 
        let elements, t = toElements tail
        if elements.Length = 1 then
            Some (Clause (head, elements), t)
        else
            Some (Clause (head, elements), t)
    | Token.Str head :: tail -> Some (Clause (head, []), tail)
    | _  -> None
/// Pattern for all pragma clauses
let (|ClausesPattern|_|) = function
    | ClausePattern (head, tail) ->
        let rec aux head' = function
            | ClausePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some (aux [head] tail)
    | _ -> None
/// Pattern for pragmas
let (|PragmaPattern|_|) = function
    | Token.Symbol '#' :: Token.Str "pragma" :: Token.Str "edgi" :: ClausesPattern (cl, Token.NewLine :: tail) ->
        match tail with
        | ScopePattern (h, t) -> h, t
        | LinePattern (h, t) -> BlockScope.Line h, t
        | _ -> failwith "edgi pragma must occur before a statement or a block."
        ||> fun h -> fun t ->
            Some (Pragma (cl.Head, cl.Tail, h), t)
    | _ -> None


type Statement =
    | MethodLoopInit of Token * Token list
    | ObjectLoopInit of Token * Token
    | MethodLoopCall of Token * Token
    | ObjectLoopCall of Token * Token list
    | NestedLoopCall of Token * Token * Token list
    | Pipe of Token * Token list
    | Cat  of Token * Token list


let (|StatementPattern|_|) = function
    | Token.Str loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
        let elements, t = toElements tail
        Some (Statement.MethodLoopInit (Token.Str loop, elements), tail)
    | Token.Str loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: func :: Token.Symbol ')' :: tail ->
        Some (Statement.ObjectLoopInit (Token.Str loop, func), tail)


/// Parse code for all the pragmas and return a list of them.
let parsePragmas (s: Token list) =
    let rec parse' = function
    | PragmaPattern (v,[]) -> [v]
    | PragmaPattern (v, t) -> v :: (parse' t)
    | head :: tail -> parse' tail
    | [] -> []
    parse' s

type NestedArray = 
    { arrName: string
      arrType: string
      arrRank: int 
      arrSym:  int list list}

type NestedNetCDFArray = 
    { arrName:  string
      arrType:  string
      arrRank:  int
      arrSym:   int list list
      fileName: string
      varName:  string }


type NestedClosure(iarraysIn: NestedArray list, symGroupsIn: int list list, comGroupsIn: int list, oarraysIn: NestedArray list, functionIn: BlockScope) =
    member this.Iarrays = iarraysIn
    member this.SymGroups = symGroupsIn
    member this.ComGroups = comGroupsIn
    member this.Oarrays = oarraysIn
    member this.Function = functionIn

let (|ArraySymmetryPattern|_|) = function
    | "symmetry", (vals: Token list) -> Some (vals)
    | _ -> failwith "Invalid array clause."

let getSymmetry (rank: int) (symGroups: int list) = 
    if symGroups.IsEmpty then 
        List.init rank id
    else
        if symGroups.Length = rank then
            symGroups
        else
            failwith "Symmetry vector length did not match the rank of the array."

let (|ArrayPattern|_|) (symGroups: int list) = function
    | BlockScope.Line (Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Str name :: Token.Symbol ';' :: tail) ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]}, tail )
    | BlockScope.Line (Token.Str "promote" :: Token.Symbol '<' :: Token.Str valtype :: Token.Symbol ',' :: Token.Int rank :: Token.Symbol '>' :: Token.Symbol ':' :: Token.Symbol ':' :: Token.Str "type" :: Token.Str name :: Token.Symbol ';' :: tail) ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]}, tail )
    | _ -> None

let (|NetCDFArrayPattern|_|) (symGroups: int list) = function
    | BlockScope.Line (Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Symbol '<' :: Token.Str fname :: Token.Symbol ',' :: Token.Str vname :: Token.Symbol '>' :: Token.Str name :: Token.Symbol ';' :: tail) ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]; fileName = fname; varName = vname}, tail )
    | _ -> None


let getArray (clauses: Clause list) (block: BlockScope) =
    let hasSym = clauses |> List.exists (fun x ->
                                             match x with
                                             | ArraySymmetryPattern s -> true
                                             | _ -> false
                                        )
    let a = if hasSym then
                clauses |> List.pick (fun x ->
                                          match x with
                                          | ArraySymmetryPattern s -> Some (s |> List.map (string >> (fun x -> x.Substring 4) >> int)) // megahack! try to fix for proper Token.Int -> int conversion
                                          | _ -> None
                                     )
            else []
    match block with
    | ArrayPattern a s -> Some(fst s)
    | _ -> failwith "Array pragma applied to invalid array declaration."


let (|FunctionArityPattern|_|) = function
    | "arity", [Token.Str "any"] -> None
    | "arity", [Token.Int arity] -> Some (arity)
    | _ -> failwith "Invalid or missing arity clause."

let (|FunctionInputPattern|_|) = function
    | "input", (value: Token list) -> Some (value |> tokenToStr)
    | _ -> failwith "Invalid or missing input clause."

let (|FunctionOutputPattern|_|) = function
    | "output", [Token.Str value] -> Some (value)
    | _ -> failwith "Invalid or missing output clause."

let (|FunctionIRankPattern|_|) (arity: int) = function
    | "irank", [Token.Int value] -> Some (List.init arity id)
    | "iranks", (values: Token list) -> Some (values |> tokenToInt)
    | _ -> failwith "Invalid or missing irank / iranks clause."

let (|FunctionOrankPattern|_|) = function
    | "orank", [Token.Int value] -> Some (value)
    | _ -> failwith "Invalid or missing orank clause clause."

let (|FunctionCommutativityPattern|_|) = function
    | "commutativity", (vals: Token list) -> Some (vals |> tokenToInt)
    | _ -> failwith "Invalid array clause."

let getFunction (name: string) (clauses: Clause list) (block: BlockScope) =

    let arity  = clauses |> List.pick (fun x -> match x with | FunctionArityPattern s       -> Some(s) | _ -> None)
    let input  = clauses |> List.pick (fun x -> match x with | FunctionInputPattern s       -> Some(s) | _ -> None)
    let output = clauses |> List.pick (fun x -> match x with | FunctionOutputPattern s      -> Some(s) | _ -> None)
    let iranks = clauses |> List.pick (fun x -> match x with | FunctionIRankPattern arity s -> Some(s) | _ -> None)
    let orank  = clauses |> List.pick (fun x -> match x with | FunctionOrankPattern s       -> Some(s) | _ -> None)

    let hasCom = clauses |> List.exists (fun x -> match x with | FunctionCommutativityPattern s -> true | _ -> false)
    let com = if hasCom then
                  clauses |> List.pick (fun x -> match x with | FunctionCommutativityPattern s -> Some (s) | _ -> None)
              else []

    {funcName = name; funcArity = arity; funcIType = ; funcIRank = iranks; funcOType = ; funcORank = orank; funcCom = com; funcBlock = block}

type NestedFunction =
    { funcName:  string
      funcArity: int
      funcIType: string
      funcIRank: int list
      funcOType: string
      funcORank: int
      funcCom:   int list
      funcBlock: Token list }

type NestedClosure(iarraysIn: NestedArray list, symGroupsIn: int list list, comGroupsIn: int list, oarraysIn: NestedArray list, functionIn: BlockScope) =
    member this.Iarrays = iarraysIn
    member this.SymGroups = symGroupsIn
    member this.ComGroups = comGroupsIn
    member this.Oarrays = oarraysIn
    member this.Function = functionIn

type PragmaObj =
    | Array of NestedArray
    | Function of NestedFunction

let sortPragmas (s: Pragma list) =
    let alist = List.filter (fun x ->
                                 let directive, clauses, scope = x
                                 fst directive = "array"
                            )
    let flist = List.filter (fun x ->
                                 let directive, clauses, scope = x
                                 fst directive = "function"
                            )
    alist s, flist s

(*
let parse (s: Token list) = 
    let alist, flist = s |> (parsePragmas >> sortPragmas)
    let arrays = List.init alist.Length (fun i ->
                                             let directive, clauses, scope = alist.[i]
                                             getArray clauses scope
                                        )
    let funcs = List.init flist.Length (fun i ->
                                            let directive, clauses, scope = flist.[i]
                                            getFunction (snd directive) clauses scope
                                       )
*)


let code = """

#include "things.hpp"
#include "stuff.hpp"
#pragma edgi function(product) arity(any) input(iarrays) irank(0) output(oarray) orank(0)
{
    oarray = 1;
    for(int i = 0; i < arity; i++){
        oarray *= iarrays[i];
    }
}
#pragma edgi function(product3) arity(3) input(iarrays) irank(0) output(oarray) orank(0)
{
    oarray = 1;
    for(int i = 0; i < arity; i++){
        oarray *= iarrays[i];
    }
}
#pragma edgi function(sumThenMultiply) input(iarray1, iarray2, iarray3) iranks(1, 1, 0) commutativity(1, 1, 3) output(oarray) orank(0)
auto sumThenMultiply = function(iarray1, iarray2, iarray3, oarray){
    // assume iarray1 and iarray2 last extents are same
    for(int i = 0; i < iarray1.current_extent(); i++){
        oarray += iarray1[i] + iarray2[i];
    }

    oarray *= iarray3;
    //return oarray;
}
#pragma edgi function(add10) input(iarray) iranks(0) output(oarray) orank(0)
{
    oarray = iarray + 10;
    //return oarray;
}
int main(){

    #pragma edgi array symmetry(1, 2, 2, 3)
    promote<float, 4>::type array1;

    #pragma edgi array
    promote<float, 3>::type array3;

    auto oloop = object_for(sumThenMultiply);
    auto mloop = method_for(array1, array1, array3);

    auto ooarray = oloop(array1, array1, array3);
    auto moarray = mloop(sumThenMultiply);

    auto noarray = nested_for(sumThenMultiply, array1, array1, array3);

    auto newfunc = pipe(sumThenMultiply, add10);

    return 0;
}"""

