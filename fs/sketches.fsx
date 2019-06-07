// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "sketches.fs"
open iterators

// Define your library scripting code here

let loopText itype iname (imin: int) (imax: int) =
    String.concat "" ["for("; itype; " "; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]
        
type Counter() =
    let mutable num = 0
    member this.This = 
        String.concat "" ["__i"; (string num)]
    member this.Next = 
        num <- num + 1
        String.concat "" ["__i"; (string num)]

let counter = Counter()
let loop (imin: int) (imax: int) =
    loopText "int" counter.Next imin imax

let index i arrayName =
    String.concat "" [arrayName; "("; (string i); ")"]

let indexToTemp i arrayName =
    String.concat "" [arrayName; counter.This; "("; (string i); ") = "; (index i arrayName); ";"]

let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["\n}"]


let rec loopNestRec (extents: int list) (inner: string list) =
    match extents with
    | [] -> inner
    | head::tail -> (
        let braced = brace (loop 0 head)
        List.append (List.append [braced.[0]] (tab (loopNestRec tail inner))) [braced.[1]]
    )

            




let myfunc = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]

let a = loop 0 10
let b = loop 0 20


let iarray = "iarray"
let oarray = "oarray"
