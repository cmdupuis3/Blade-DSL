// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Core.Option

#load "sketches.fs"
open iterators

// Define your library scripting code here

        
type Counter() =
    let mutable num = 0
    member this.Last = num-1
    member this.This = num
    member this.Next = num <- num + 1; num
    member this.LastSuffix = 
        String.concat "" ["__i"; (string (num - 1))]
    member this.ThisSuffix = 
        String.concat "" ["__i"; (string num)]
    member this.NextSuffix = 
        num <- num + 1
        String.concat "" ["__i"; (string num)]

let loopText itype iname (imin: int) (imax: int) =
    String.concat "" ["for("; itype; " "; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]

let loop (imin: int) (imax: int) (ctr: int) =
    loopText "int" (String.concat "" ["__i"; (string ctr)]) imin imax

let index i arrayName =
    String.concat "" [arrayName; "(__i"; i; ")"]

let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["\n}"]


let rec unaryLoop (arrayName: string) (extents: int list) (inner: string list) (depth: int) =
    match extents with
    | [] -> (inner, String.concat "" [arrayName; "__i"; string (depth-1)], depth-1)
    | head::tail ->
        let nextLoop, lastArrayName, lastDepth = unaryLoop arrayName tail inner (depth+1)
        let braced = brace (loop 0 head depth)
        if depth = 0 then 
            List.concat [ [braced.[0]];
                          tab [String.concat "" ["auto "; arrayName; "__i"; string depth; " = "; index (string depth) arrayName; ";\n"]]
                          tab (nextLoop);
                          [braced.[1]] ], lastArrayName, lastDepth
        else
            List.concat [ [braced.[0]];
                          tab [String.concat "" ["auto "; arrayName; "__i"; string depth; " = "; (index (string depth) (String.concat "" [arrayName; "__i"; string (depth-1)])); ";\n"]]
                          tab (nextLoop);
                          [braced.[1]] ], lastArrayName, lastDepth



let rec naryLoop (arrayNames: string list) (extents: int list list) (inner: string list) (arg: int) =
    assert (List.length arrayNames = List.length extents)
    match arrayNames with
    | [] -> failwith "Empty array names list." // Should be impossible for recursive calls; N-ary nested_for should terminate in loopNestRec
    | [head] -> 
        let lastLoop, lastArrayName, lastDepth = unaryLoop head extents.[0] inner 0
        lastLoop, [lastArrayName], lastDepth
    | head::tail ->
        let nextLoop, nextArrayName, nextDepth = naryLoop tail extents.[1..] inner (arg+1)
        let thisLoop, thisArrayName, thisDepth = unaryLoop head extents.[0] (nextLoop) (nextDepth+1)
        thisLoop, thisArrayName::nextArrayName, (thisDepth+1)





let c = Counter()
let d = Counter()
let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray2"]
let iextents = [ [2;3;4]; [5;6;7] ]
let oarray = "oarray"

let p, q, r = unaryLoop iarrays.[0] iextents.[0] (newln inner) 0
let x, y, z = naryLoop iarrays iextents (newln inner) 0
