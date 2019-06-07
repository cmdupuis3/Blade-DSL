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

let loop (imin: int) (imax: int) (ctr: Counter) =
    loopText "int" ctr.This imin imax

let index i arrayName =
    String.concat "" [arrayName; "("; (string i); ")"]

let indexToTemp i arrayName (ctr: Counter) =
    String.concat "" ["auto "; arrayName; string (ctr.Next); " = "; (index i arrayName); ";"]

let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["\n}"]


let rec loopNestRec (arrayName: string) (extents: int list) (inner: string list) (ctr: Counter) =
    match extents with
    | [] -> inner
    | head::tail -> (
        let braced = brace (loop 0 head ctr)
        List.concat [ [braced.[0]];
                      tab [(indexToTemp ctr.This arrayName ctr)];
                      tab (loopNestRec arrayName tail inner ctr);
                      [braced.[1]] ]
    )


let c = Counter()



let myfunc = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]

let a = loop 0 10
let b = loop 0 20


let iarray = "iarray"
let oarray = "oarray"
