

open System.IO
open System.Text.RegularExpressions

#load "parser.fs"

type Token = 
    | WhiteSpace
    | Symbol of char
    | Str of string
    | Int of int

let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

let toToken = function
    | Match @"^\s+"                  s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^,"    s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z][a-zA-Z0-9]*" s -> s, Token.Str s
    | Match @"^\d+"                  s -> s, Token.Int (int s)
    | _ -> failwith "Invalid Token"


let tokenize (s: string) =
    let rec tokenize' index (s: string) =
        if index = s.Length then []
        else
            let text, token = toToken (s.Substring index)
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    |> List.choose (function Token.WhiteSpace -> None | t -> Some t)


type Syntax =
    | Block of string
    | Pragma of InitClause * Clause list
    | Declaration of Declaration
    | Assignment of Assignment
    | Null
and Type = string
and Identifier = string
and Declaration = Type * Identifier
and Clause =
    | NullClause of string
    | StrClause of string * string
    | IntClause of string * int
    | StrListClause of string * string list
    | IntListClause of string * int list
and InitClause =
    | NullClause of string
    | StrClause of string * string
and Assignment = 
    | Assign of Identifier * Expression
    | Construct of Declaration * Expression
and Expression =
    | MethodLoopInit of string * Identifier list
    | ObjectLoopInit of string * Identifier
    | MethodLoopCall of string * Identifier
    | ObjectLoopCall of string * Identifier list
    | NestedLoopCall of string * Identifier * Identifier list
    | Pipe of string * Identifier list
    | Cat  of string * Identifier list






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
