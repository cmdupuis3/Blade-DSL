
open System.IO
open System.Text.RegularExpressions

//#r "FParsec"
//open FParsec

#load "parser.fs"

type Token = 
    | NewLine
    | WhiteSpace
    | Symbol of char
    | Str of string
    | Int of int
    | Other of string

let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

let toToken = function
    | Match @"^\n|^\r"               s -> s, Token.NewLine
    | Match @"^\s+"                  s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>|^;"    s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z][a-zA-Z0-9]*" s -> s, Token.Str s
    | Match @"^\d+"                  s -> s, Token.Int (int s)
    | Match @"."                 s -> s, Token.Other (string s)
    | _ -> failwith "Invalid Token"


let tokenize (s: string) =
    let rec tokenize' index (s: string) =
        if index = s.Length then []
        else
            let text, token = toToken (s.Substring index)
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    |> List.choose (function Token.WhiteSpace -> None | t -> Some t)

type Clause =
    | Null of string
    | Single of string * Token
    | List of string * Token list

type Type = string

type Identifier = string

type Declaration = Type * Identifier

type Expression =
    | MethodLoopInit of Identifier list
    | ObjectLoopInit of Identifier
    | MethodLoopCall of Identifier
    | ObjectLoopCall of Identifier list
    | NestedLoopCall of Identifier * Identifier list
    | Pipe of Identifier list
    | Cat  of Identifier list

type Assignment = 
    | Assign of Identifier * Expression
    | Construct of Declaration * Expression

type Block = Token list

type PragmaScope =
    | Block of Token list
    | Line of Token list

type Pragma = Clause * Clause list * PragmaScope


let rec (|ScopePattern|_|) = function
    | Token.Symbol '{' :: tail -> 
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
        Some (Block (scope), (t: Token list))
    | head :: tail -> 
        match tail with
        | ScopePattern t -> Some t
        | _ -> None
    | _ -> None
let (|ScopesPattern|_|) = function
    | ScopePattern (head, tail) ->
        let rec aux head' = function
            | ScopePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some (aux [head] tail)
    | _ -> None

//match tokenize "asd(8) {fjfj jfjf }\n" with | ScopePattern(s) -> Some(s) |_->None;;
//let a = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopePattern v -> Some(v) |_-> None ;;
//let b = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopesPattern v -> Some(v) |_-> None ;;

let rec (|LinePattern|_|) = function
    | head :: Token.Symbol ';' :: Token.NewLine :: tail | head :: Token.Symbol ';' :: tail ->
        Some ([head], tail)
    | head :: tail ->
        let h, t = 
            match tail with
            | LinePattern t -> t
            | _ -> [], []
        Some (head :: h, t)
    | _ -> None
let (|LinesPattern|_|) = function
    | LinePattern (head, tail) ->
        let rec aux head' = function
            | LinePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some (aux [head] tail)
    | _ -> None

//match tokenize "asd(8);\n fjfjfj" with | LinePattern(s) -> Some(s) |_->None;;
//match tokenize "asd(8);\n asdf;\n fjfjfjfj 9;\n" with | LinesPattern(s) -> Some(s) |_->None;;


let rec toElements s = 
    match s with
    | head :: Token.Symbol ',' :: tail -> 
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []

let rec (|ClausePattern|_|) = function
    | Token.NewLine :: tail -> None
    | Token.Str head :: Token.Symbol '(' :: tail -> 
        let elements, t = toElements tail
        if elements.Length = 1 then
            Some (Clause.Single (head, elements.Head), t)
        else
            Some (Clause.List (head, elements), t)
    | Token.Str head :: tail -> Some (Clause.Null head, tail)
    | _  -> None
let (|ClausesPattern|_|) = function
    | ClausePattern (head, tail) ->
        let rec aux head' = function
            | ClausePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some (aux [head] tail)
    | _ -> None
let (|PragmaPattern|_|) = function
    | Token.Symbol '#' :: Token.Str "pragma" :: Token.Str "edgi" :: ClausesPattern (cl, Token.NewLine :: tail) ->
        match tail with
        | ScopePattern (h, t) -> h, t
        | LinePattern (h, t) -> PragmaScope.Line h, t
        | _ -> failwith "fjfjfjpaoixc"
        ||> fun h -> fun t ->
            Some (Pragma (cl.Head, cl.Tail, h), t)
    | _ -> None


let parse (s: string) =
    let rec parse' = function
    //| ScopesPattern v -> v
    | PragmaPattern (v,[]) -> [v]
    | PragmaPattern (v, t) -> v :: (parse' t)
    | head :: tail -> parse' tail
    | [] -> []
    | _ -> failwith "Failed to parse"
    parse' (tokenize s)



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
{
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
    promote<float, 4> array1;

    #pragma edgi array
    promote<float, 3> array3;
    
    auto oloop = object_for(sumThenMultiply);
    auto mloop = method_for(array1, array1, array3);

    auto ooarray = oloop(array1, array1, array3);
    auto moarray = mloop(sumThenMultiply);

    auto noarray = nested_for(sumThenMultiply, array1, array1, array3);

    auto newfunc = pipe(sumThenMultiply, add10);


    return 0;
}

"""
