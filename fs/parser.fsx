
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
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s -> s, Token.Str s
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
    |> List.choose (function Token.WhiteSpace -> None | tokens -> Some tokens)

/// Megahack to convert tokens to ints. try to fix for proper Token.Int -> int conversion
let tokenToInt (tokens: Token list) = 
    tokens |> List.map (string >> (fun x -> x.Substring 4) >> int)

/// Megahack to convert tokens to strings. try to fix for proper Token.Str -> string conversion
let tokenToStr (tokens: Token list) = 
    let unquote (tokens': string) = tokens'.Substring(1, tokens'.Length-2)
    tokens |> List.map (string >> (fun x -> x.Substring 4) >> unquote)

/// Pragma clause type; a tuple of the clause name and a list of arguments
type Clause = string * Token list

/// Pragma type; consists of a directive, a list of clauses, and a scope
type Pragma = Clause * Clause list * Token list

/// Delete the "return" line of a function (needed for function expansion)
let rec deleteReturnLine = function
    | Token.Str "return" :: tail ->
        let rec aux = function
            | Token.Symbol ';' :: tail' -> Some tail'
            | head' :: tail'-> aux tail'
            | _ -> None
        aux tail
    | head :: tail -> Some (head :: tail)
    | _ -> None

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
        Some (scope, (t: Token list))
    | head :: tail -> 
        match tail with
        | ScopePattern t -> Some t
        | _ -> None
    | _ -> None

// Pattern for all code after a certain code block where the code block is in scope
let rec (|PostScopePattern|_|) = function
    | Token.Symbol '{' :: tail ->
        /// Make a list of tokens inside this scope by counting the number of braces (terrible, I know)
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol '}' :: Token.NewLine :: tail' -> 
                if ctr > -1 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: Token.NewLine :: h', t'
                else
                    [], tail'
            | Token.Symbol '}' :: tail' -> 
                if ctr > -1 then
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
        Some (Token.Symbol '{' :: scope, (t: Token list))
    | head :: tail -> 
        match tail with
        | PostScopePattern t -> 
            match t with
            | [], [] -> None
            | _ -> Some (head :: (fst t), snd t)
        | _ -> None
    | _ -> None

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
let rec toElements = function
    | head :: Token.Symbol ',' :: tail -> 
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []

/// Pattern for individual pragma clauses
let rec (|ClausePattern|_|) = function
    | Token.NewLine :: tail -> None
    | Token.Str head :: Token.Symbol '(' :: tail -> 
        let elements, tokens = toElements tail
        if elements.Length = 1 then
            Some (Clause (head, elements), tokens)
        else
            Some (Clause (head, elements), tokens)
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
        | LinePattern (h, t) ->  h, t
        | _ -> failwith "edgi pragma must occur before a statement or a block."
        ||> fun h -> fun t ->
            Some (Pragma (cl.Head, cl.Tail, h), t)
    | _ -> None

/// Parse code for all the pragmas and return a list of them.
let parsePragmas (tokens: Token list) =
    let rec parse' = function
    | PragmaPattern (pragma,[]) -> [pragma]
    | PragmaPattern (pragma, tokens') -> pragma :: (parse' tokens')
    | head :: tail -> parse' tail
    | [] -> []
    parse' tokens

type NestedArray = 
    { arrName: string
      arrType: string
      arrRank: int 
      arrSym:  int list list }

type NestedNetCDFArray = 
    { arrName:  string
      arrType:  string
      arrRank:  int
      arrSym:   int list list
      fileName: string
      varName:  string }

let getSymmetry (rank: int) (symGroups: int list) = 
    if symGroups.IsEmpty then 
        List.init rank id
    else
        if symGroups.Length = rank then
            symGroups
        else
            failwith "Symmetry vector length did not match the rank of the array."

let (|ArrayPattern|_|) (symGroups: int list) = function
    | Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]}, tail )
    | Token.Str "promote" :: Token.Symbol '<' :: Token.Str valtype :: Token.Symbol ',' :: Token.Int rank :: Token.Symbol '>' :: Token.Symbol ':' :: Token.Symbol ':' :: Token.Str "type" :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]}, tail )
    | _ -> None
(*
let (|NetCDFArrayPattern|_|) (symGroups: int list) = function
    | Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Symbol '<' :: Token.Str fname :: Token.Symbol ',' :: Token.Str vname :: Token.Symbol '>' :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = [getSymmetry rank symGroups]; fileName = fname; varName = vname}, tail )
    | _ -> None
*)

//[Str "promote"; Symbol '<'; Str "float"; Symbol ','; Int 4; Symbol '>'; Token.Symbol ':'; Token.Symbol ':'; Token.Str "type"; Str "array1"; Token.Symbol ';'] |> function | ArrayPattern(s) -> Some(s) | _ -> failwith "Array pragma specified on a line that did not declare an array.";;

let getArray (clauses: Clause list) (block: Token list) =
    let hasSym = clauses |> List.exists (fst >> (function | "symmetry" -> true | _ -> false))
    let sym = if hasSym then
                  (clauses |> List.find (fst >> (function | "symmetry" -> true | _ -> false))) |> (snd >> tokenToInt)
              else []
    match block with
    | ArrayPattern sym s -> fst s
    | _ -> failwith "Array pragma applied to invalid array declaration."

type NestedFunction =
    { funcName:  string
      funcArity: int
      funcINames: string list
      funcIRank: int list
      funcOName: string
      funcORank: int
      funcCom:   int list
      funcBlock: Token list }

let getFunction (name: string) (clauses: Clause list) (block: Token list) =

    let arity  = (clauses |> List.find (fst >> (function | "arity" -> true | _ -> false))) |> (snd >> tokenToInt >> List.head)
    let input  = (clauses |> List.find (fst >> (function | "input" -> true | _ -> false))) |> (snd >> tokenToStr)
    let output = (clauses |> List.find (fst >> (function | "output" -> true | _ -> false))) |> (snd >> tokenToStr >> List.head)
    let iranks = (clauses |> List.find (fst >> (function | "iranks" -> true | _ -> false))) |> (snd >> tokenToInt)
    let orank  = (clauses |> List.find (fst >> (function | "orank" -> true | _ -> false))) |> (snd >> tokenToInt >> List.head)

    let hasCom = clauses |> List.exists (fst >> (function | "commutativity" -> true | _ -> false))
    let com = if hasCom then
                  (clauses |> List.find (fst >> (function | "commutativity" -> true | _ -> false))) |> (snd >> tokenToInt)
              else List.init arity id

    { funcName = name; 
      funcArity = arity;
      funcINames = input; 
      funcIRank = iranks; 
      funcOName = output; 
      funcORank = orank; 
      funcCom = com; 
      funcBlock = block |> deleteReturnLine |> Option.get }

type NestedClosure(iarraysIn: NestedArray list, oarrayIn: NestedArray, functionIn: NestedFunction) =
    member this.Iarrays = iarraysIn
    member this.Oarray = oarrayIn
    member this.Function = functionIn

type PragmaObj =
    | Array of NestedArray
    | Function of NestedFunction

let sortPragmas (pragmas: Pragma list) =
    let filter = fun (s: string) ->
        List.filter (fun x ->
                        let directive, clauses, scope = x
                        fst directive = s
                    )
    filter "array" pragmas, filter "function" pragmas

(*
type Statement =
    | MethodLoopInit of Token * Token list
    | ObjectLoopInit of Token * Token
    | MethodLoopCall of Token * Token
    | ObjectLoopCall of Token * Token list
    //| NestedLoopCall of Token * Token * Token list
    | Pipe of Token * Token list
    | Cat  of Token * Token list
*)

let queryArray (name: string) (arrays: NestedArray list) =
    List.pick (fun (i: NestedArray) -> if i.arrName = name then Some i else None) arrays

let queryFunction (name: string) (funcs: NestedFunction list) =
    List.pick (fun (i: NestedFunction) -> if i.funcName = name then Some i else None) funcs

let (|MethodLoopPattern|_|) (arrays: NestedArray list) (functions: NestedFunction list) = function
    | Token.Str loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
        let elements, tokens = toElements tail
        let iarrays = (tokenToStr elements) |> List.map (fun x -> queryArray x arrays)
        let rec findCalls tokens' = 
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: loop :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail' -> (func, oarray) :: findCalls tail'
            | head' :: tail' -> findCalls tail'
            | [] -> []
        let oarrays, funcs = 
            match tokens with
            | PostScopePattern (tokens',_) -> findCalls (tokens')
            | _ -> []
            |> fun x -> List.map (fst >> fun y -> queryArray y arrays) x, List.map (snd >> fun y -> queryFunction y functions) x
        Some (List.init funcs.Length (fun i -> NestedClosure(iarrays, oarrays.[i], funcs.[i])))
    | _ -> None

let (|ObjectLoopPattern|_|) (arrays: NestedArray list) (functions: NestedFunction list) = function
    | Token.Str loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: func :: Token.Symbol ')' :: tail ->
        let func' = ((tokenToStr [func]) |> List.map (fun x -> queryFunction x functions)).Head
        let rec findCalls tokens' = 
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: loop :: Token.Symbol '(' :: tail' ->
                let iarrays, tokens = toElements tail
                (tokenToStr iarrays, oarray) :: findCalls tail'
            | head' :: tail' -> findCalls tail'
            | [] -> []
        let iarrays, oarrays = 
            match tail with
            | PostScopePattern (tokens',_) -> findCalls (tokens')
            | _ -> []
            |> fun x -> List.map (fst >> List.map (fun y -> queryArray y arrays)) x, List.map (snd >> fun y -> queryArray y arrays) x
        Some (List.init oarrays.Length (fun i -> NestedClosure(iarrays.[i], oarrays.[i], func')))
    | _ -> None

//let a = match (tokenize "l = method_for(dsds);\n l(jjrjr);\n") with | MethodLoopPattern alist flist v -> Some(v) |_-> None ;;
//let a = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopePattern v -> Some(v) |_-> None ;;

let parse (tokens: Token list) =
    let alist, flist = tokens |> (parsePragmas >> sortPragmas)
    let arrays = List.init alist.Length (fun i ->
                                             let directive, clauses, scope = alist.[i]
                                             getArray clauses scope
                                        )
    let funcs = List.init flist.Length (fun i ->
                                            let directive, clauses, scope = flist.[i]
                                            let name = ([(snd directive).Head] |> tokenToStr).Head
                                            getFunction name clauses scope
                                       )
    []

let code = """

#include "things.hpp"
#include "stuff.hpp"
#pragma edgi function(sumThenMultiply) arity(3) input(iarray1, iarray2, iarray3) iranks(1, 1, 0) commutativity(1, 1, 3) output(oarray) orank(0)
void sumThenMultiply(nested_array_t<float, 1> oarray, nested_array_t<float,1> iarray1, nested_array_t<float,1> iarray2, nested_array_t<float,0> iarray3) {
    // assume iarray1 and iarray2 last extents are same
    for(int i = 0; i < iarray1.current_extent(); i++){
        oarray += iarray1[i] + iarray2[i];
    }

    oarray *= iarray3;
    return oarray;
}
#pragma edgi function(add10) arity(1) input(iarray) iranks(0) output(oarray) orank(0)
void sumThenMultiply(nested_array_t<float, 0> oarray, nested_array_t<float, 0> iarray1) {
    oarray = iarray + 10;
    return oarray;
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
