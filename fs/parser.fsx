
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
    |> List.choose (function Token.WhiteSpace -> None | t -> Some t)

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
    | head :: tail ->
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
        Some (head :: scope, (t: Token list))
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

/// Make a list of arguments from a comma-separated list and return the tail
let rec toElements s = 
    match s with
    | head :: Token.Symbol ',' :: tail -> 
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []

type NestedArray = 
    { arrName: string
      arrType: string
      arrRank: int 
      arrSym:  int list }

type NestedFunction =
    { funcName:  string
      funcArity: int
      funcINames: string list
      funcIRank: int list
      funcOName: string
      funcORank: int
      funcCom:   int list
      funcBlock: Token list }

type MethodLoop (nameIn: string, initIn: string list, callIn: (string * string) list)  =
    let mutable (iarrays: NestedArray list) = []
    let mutable (oarrays: NestedArray list) = []
    let mutable (func: NestedFunction list) = []
    member this.Name = nameIn
    member this.Init = initIn // iarrays
    member this.Call = callIn // oarray and func

    member this.PushIarray (v: NestedArray) = iarrays <- List.append iarrays [v]
    member this.PushOarray (v: NestedArray) = oarrays <- List.append oarrays [v]
    member this.PushFunc (v: NestedFunction) = func <- List.append func [v]

type ObjectLoop (nameIn: string, initIn: string, callIn: ((string list) * string) list) =
    let mutable (iarrays: NestedArray list list) = []
    let mutable (oarrays: NestedArray list) = []
    let mutable (func: NestedFunction list) = []
    member this.Name = nameIn
    member this.Init = initIn // func
    member this.Call = callIn // iarrays and oarray

    member this.PushIarrays (v: NestedArray list) = iarrays <- List.append iarrays [v]
    member this.PushOarray (v: NestedArray) = oarrays <- List.append oarrays [v]
    member this.SetFunc (v: NestedFunction) = func <- [v]
    member this.GetFunc (v: NestedFunction) = func.Head

/// Pattern for method_for loops and all their calls
let rec (|MethodLoopPattern|_|) = function
    | loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
        let elements, tokens = toElements tail
        let iarrays = tokenToStr elements
        let rec findCalls tokens' =
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail' when lname = loop -> (func, oarray) :: findCalls tail'
            | head' :: tail' -> findCalls tail'
            | [] -> []
        let calls = 
            match tokens with
            | PostScopePattern tokens' -> findCalls (fst tokens')
            | _ -> []
        Some (MethodLoop((tokenToStr [loop]).Head, iarrays, calls), tokens)
(*
    | head :: tail ->
        match tail with
        | MethodLoopPattern t -> Some (t)
        | _ -> None
*)
    | _ -> None

/// Scan code for all the method_for loops and return a list of them.
let scanMethodLoops (tokens: Token list) =
    let rec scan = function
    | MethodLoopPattern (loop,[]) -> [loop]
    | MethodLoopPattern (loop, tail) -> loop :: (scan tail)
    | head :: tail -> scan tail
    | [] -> []
    scan tokens

//let a = match tokenize code with | MethodLoopPattern(s) -> Some(s) | _ -> None;;
//let mystr = "auto mloop = method_for(array1, array1, array3); auto ooarray = oloop(array1, array1, array3); auto moarray = mloop(sumThenMultiply);"
//let b = match tokenize mystr with | MethodLoopPattern(s) -> Some(s) | _ -> None;;

/// Pattern for object_for loops and all their calls
let rec (|ObjectLoopPattern|_|) = function
    | loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail ->
        let rec findCalls tokens' = 
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: tail' when lname = loop ->
                let iarrays, t = toElements tail'
                (tokenToStr iarrays, oarray) :: findCalls tail'
            | head' :: tail' -> findCalls tail'
            | [] -> []
        let calls = 
            match tail with
            | PostScopePattern t' -> findCalls (fst t')
            | _ -> []
        Some (ObjectLoop((tokenToStr [loop]).Head, string func, calls), tail)
(*
    | head :: tail ->
        match tail with
        | ObjectLoopPattern t -> Some (t)
        | _ -> None
*)
    | _ -> None
    
/// Scan code for all the method_for loops and return a list of them.
let scanObjectLoops (tokens: Token list) =
    let rec scan = function
    | ObjectLoopPattern (loop,[]) -> [loop]
    | ObjectLoopPattern (loop, tail) -> loop :: (scan tail)
    | head :: tail -> scan tail
    | [] -> []
    scan tokens


//let a = match tokenize code with | ObjectLoopPattern(s) -> Some(s) | _ -> None;;
//let mystr = "auto oloop = object_for(sumThenMultiply); auto mloop = method_for(array1, array1, array3); auto ooarray = oloop(array1, array1, array3);"
//let b = match tokenize mystr with | ObjectLoopPattern(s) -> Some(s) | _ -> None;;

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
        | LinePattern (h, t) ->  h, t
        | _ -> failwith "edgi pragma must occur before a statement or a block."
        ||> fun h -> fun t ->
            Some (Pragma (cl.Head, cl.Tail, h), t)
    | _ -> None

/// Scan code for all the pragmas and return a list of them.
let scanPragmas (tokens: Token list) =
    let rec scan = function
    | PragmaPattern (pragma,[]) -> [pragma]
    | PragmaPattern (pragma, tail) -> pragma :: (scan tail)
    | head :: tail -> scan tail
    | [] -> []
    scan tokens

let (|ArraySymmetryPattern|_|) = function
    | "symmetry", (vals: Token list) -> Some (vals |> tokenToInt)
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
    | Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = getSymmetry rank symGroups}, tail )
    | Token.Str "promote" :: Token.Symbol '<' :: Token.Str valtype :: Token.Symbol ',' :: Token.Int rank :: Token.Symbol '>' :: Token.Symbol ':' :: Token.Symbol ':' :: Token.Str "type" :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = getSymmetry rank symGroups}, tail )
    | _ -> None

let getArray (clauses: Clause list) (block: Token list) =
    let hasSym = clauses |> List.exists (function | ArraySymmetryPattern s -> true | _ -> false)
    let sym = if hasSym then
                  clauses |> List.pick (function | ArraySymmetryPattern s -> Some (s) | _ -> None)
              else []
    match block with
    | ArrayPattern sym s -> fst s
    | _ -> failwith "Array pragma applied to invalid array declaration."

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

let sortPragmas (pragmas: Pragma list) =
    let bin (s: string) = 
        List.filter (fun x ->
                        let directive, clauses, scope = x
                        fst directive = s
                    ) pragmas
    bin "array", bin "function"

/// Message-passing function for sending info about array pragmas to nested loop objects
let sendArraysToLoops (arrays: NestedArray list) (mloops: MethodLoop list) (oloops: ObjectLoop list) = 
    for i in 0..mloops.Length do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..arrays.Length do
            for k in 0..mloops.[i].Init.Length do
                if mloops.[i].Init.[k] = arrays.[j].arrName then
                    mloops.[i].PushIarray arrays.[j]
            for k in 0..mloops.[i].Call.Length do
                if fst mloops.[i].Call.[k] = arrays.[j].arrName then
                    mloops.[i].PushOarray arrays.[j]
    for i in 0..oloops.Length do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..arrays.Length do
            for k in 0..oloops.[i].Call.Length do
                let mutable itemp = []
                for l in 0..(fst oloops.[i].Call.[k]).Length do
                    if (fst oloops.[i].Call.[k]).[l] = arrays.[j].arrName then
                        itemp <- List.append itemp [arrays.[j]]
                oloops.[i].PushIarrays itemp
                if snd oloops.[i].Call.[k] = arrays.[j].arrName then
                    oloops.[i].PushOarray arrays.[j]


/// Message-passing function for sending info about function pragmas to nested loop objects
let sendFunctionsToLoops (funcs: NestedFunction list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
    for i in 0..mloops.Length do
        // search for function names in funcs list and copy info to loop objects (results must be in order!)
        for j in 0..funcs.Length do
            for k in 0..mloops.[i].Init.Length do
                if mloops.[i].Init.[k] = funcs.[j].funcName then
                    mloops.[i].PushFunc funcs.[j]
            for k in 0..mloops.[i].Call.Length do
                if fst mloops.[i].Call.[k] = funcs.[j].funcName then
                    mloops.[i].PushFunc funcs.[j]


let lex (tokens: Token list) = 
    let mloops = scanMethodLoops tokens
    let oloops = scanObjectLoops tokens

    let alist, flist = tokens |> (scanPragmas >> sortPragmas)
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
