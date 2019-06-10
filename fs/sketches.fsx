// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Core.Option

#load "sketches.fs"
open iterators

// Define your library scripting code here


let loopText iname (imin: int) (imax: int) =
    String.concat "" ["for("; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]

let loopLine (imin: int) (imax: int) (ctr: int) =
    loopText (String.concat "" ["__i"; (string ctr)]) imin imax

let index (i: int) arrayName =
    String.concat "" [arrayName; "(__i"; string i; ")"]

let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" [y; "\n"])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["}\n"]

let ompLine (i: int) =
    String.concat "" ["#pragma omp parallel for private(__i"; string i; ")"]

let declLine (i: int) itype =
    String.concat "" [itype; " __i"; string i; ";"]


let rec unaryLoop (arrayName: string) (extents: int list) (inner: string list) (depth: int) (counter: int) (ompLevels: int) =
    match extents with
    | [] -> (newln inner, String.concat "" [arrayName; "__i"; string (counter-1)], depth-1)
    | head::tail ->
        let nextLoop, lastArrayName, lastDepth = unaryLoop arrayName tail inner (depth+1) (counter+1) (ompLevels-1)
        let braced = brace (loopLine 0 head counter)
        let ompline = if ompLevels > 0 then [ompLine counter] else []

        if depth = 0 then 
            List.concat [ newln [declLine counter "int"];
                          newln ompline;
                          [braced.[0]];
                          tab [String.concat "" ["auto "; arrayName; "__i"; string counter; " = "; index counter arrayName; ";\n"]]
                          tab (nextLoop);
                          [braced.[1]] ],
            lastArrayName,
            lastDepth
        else
            List.concat [ newln [declLine counter "int"];
                          newln ompline;
                          [braced.[0]];
                          tab [String.concat "" ["auto "; arrayName; "__i"; string counter; " = "; (index counter (String.concat "" [arrayName; "__i"; string (counter-1)])); ";\n"]]
                          tab (nextLoop);
                          [braced.[1]] ],
            lastArrayName,
            lastDepth


let rec naryLoop (arrayNames: string list) (extents: int list list) (inner: string list) (arg: int) (ompLevels: int list) =
    assert (List.length arrayNames = List.length extents)
    match arrayNames with
    | [] -> failwith "Empty array names list." // Should be impossible for recursive calls; N-ary nested_for should terminate in loopNestRec
    | [head] -> 
        let lastLoop, lastArrayName, lastDepth = unaryLoop head extents.[0] inner 0 0 ompLevels.[0]
        lastLoop, [lastArrayName], lastDepth
    | head::tail ->
        let nextLoop, nextArrayName, nextDepth = naryLoop tail extents.[1..] inner (arg+1) ompLevels.[1..]
        let thisLoop, thisArrayName, thisDepth = unaryLoop head extents.[0] (nextLoop) 0 (nextDepth+1) ompLevels.[0]
        thisLoop, thisArrayName::nextArrayName, (thisDepth+1)



let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray2"]
let iextents = [ [2;3;4]; [5;6;7] ]
let oarray = "oarray"

let p, q, r = unaryLoop iarrays.[0] iextents.[0] inner 0 3 2
let x, y, z = naryLoop iarrays iextents inner 0 [2;1]
