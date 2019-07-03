

open System.IO
open System.Text.RegularExpressions

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
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>"    s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z][a-zA-Z0-9]*" s -> s, Token.Str s
    | Match @"^\d+"                  s -> s, Token.Int (int s)
    | Match @".*"                    s -> s, Token.Other (string s)
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

type Scope = string
type Pragma = Clause * Clause list

type ScopeTree =
  | Leaf of Token list
  | Node of ScopeTree // First type is contains all tokens at this scope visible to any internal scopes. Second type is the list of all scopes where the the first is visible.

type Syntax =
    | Block of Token list
    | Pragma of Clause * Clause list
//    | Declaration of Declaration
//    | Assignment of Assignment
    | Null


let rec toElements s = 
    match s with
    | head :: Token.Symbol ',' :: tail -> 
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []

let rec (|ClausePattern|_|) = function
    | Token.Str head :: Token.Symbol '(' :: tail -> 
        let elements, t = toElements tail
        let last = ClausePattern t
        if elements.Length = 1 then
            Some (Clause.Single (head, elements.Head), t)
        else
            Some (Clause.List (head, elements), t)
    | Token.Str head :: tail -> Some (Clause.Null head, tail)
    | _  -> None
and (|ClausesPattern|_|) = function
    | ClausePattern (head, tail) ->
        let rec aux head' = function
            | ClausePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some(aux [head] tail)
    | _ -> None
and (|PragmaPattern|_|) = function
    | Token.Symbol '#' :: Token.Str "pragma" :: Token.Str "edgi" :: ClausesPattern (cl, Token.NewLine :: tail) ->
        Some (Pragma (cl.Head, cl.Tail))
    | _ -> None


let rec (|ScopePattern|_|) = function
    | Token.Symbol '{' :: tail -> 
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol '}' :: tail' -> 
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: h', t'
                else
                    [], []
            | Token.Symbol '{' :: tail' -> 
                let h', t' = toScope tail' (ctr+1)
                Token.Symbol '{' :: h', t'
            | head' :: tail' -> 
                let h', t' = toScope tail' ctr
                head' :: h', t'
            | _ -> failwith "asdfsdf"
        let scope, t = toScope tail 0
        Some (Syntax.Block (scope), (t: Token list))
    | head :: tail -> 
        match tail with
        | ScopePattern(t) -> Some(t)
        | _ -> None
    | _ -> None



let parse s =
    tokenize s |> function 
    //| ClausesPattern v -> v
    | PragmaPattern v -> v
    | _ -> failwith "Failed to parse"



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
