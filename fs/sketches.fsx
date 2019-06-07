// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open FSharp.Core.Option

#load "sketches.fs"
open iterators

// Define your library scripting code here

let loopText itype iname (imin: int) (imax: int) =
    String.concat "" ["for("; itype; " "; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]
        
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

let loop (imin: int) (imax: int) (ctr: int) =
    loopText "int" (String.concat "" ["__i"; (string ctr)]) imin imax

let index i arrayName =
    String.concat "" [arrayName; "(__i"; i; ")"]

let indexToTemp arrayName (ctr: Counter) =
    if ctr.This = 0 then
        String.concat "" ["auto "; arrayName; ctr.NextSuffix; " = "; (index ctr.LastSuffix arrayName); ";\n"]
    else
        String.concat "" ["auto "; arrayName; ctr.NextSuffix; " = "; (index ctr.LastSuffix (String.concat "" [arrayName; ctr.ThisSuffix])); ";\n"]


let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["\n}"]




type ArrayIndexer(arrayName: string, extents: int list) =
    let name = arrayName
    let rank = List.length extents
    let extents = extents
    let counter = Counter()
    let mutable names = [arrayName]
    let mutable code = []

    member this.Name = name
    member this.Rank = rank
    member this.Extents = extents
    
    member this.SetNextName(nextName) =
        counter.Next |> ignore
        names <- List.append names [nextName]
    member this.LastName = List.last names



let rec unaryLoop (arrayName: string) (extents: int list) (inner: string list) (indCtr: Counter) (depth: int)=

    match extents with
    | [] -> (inner, String.concat "" [arrayName; "__i"; string depth])
    | head::tail -> (
        let nextLoop, lastArrayName = unaryLoop arrayName tail inner indCtr (depth+1)
        let braced = brace (loop 0 head depth)
        if depth = 0 then 
            (List.concat [ [braced.[0]];
                           [String.concat "" ["auto "; arrayName; "__i"; string depth; " = "; index (string depth) arrayName; ";\n"]]
                           tab (nextLoop);
                           [braced.[1]] ], lastArrayName)
        else
            (List.concat [ [braced.[0]];
                           [String.concat "" ["auto "; arrayName; "__i"; string depth; " = "; (index (string depth) (String.concat "" [arrayName; "__i"; string (depth-1)])); ";\n"]]
                           tab (nextLoop);
                           [braced.[1]] ], lastArrayName)
    )



let rec naryLoop (arrayNames: string list) (extents: int list list) (inner: string list) (ctr: Counter) =
    assert (List.length arrayNames = List.length extents)
    match arrayNames with
    | [] -> failwith "Empty array names list." // Should be impossible for recursive calls; N-ary nested_for should terminate in loopNestRec
    | [head] -> unaryLoop head extents.[0] inner ctr 0
    | head::tail -> unaryLoop head extents.[0] (naryLoop tail extents.[1..] inner ctr) ctr 0




let c = Counter()
let d = Counter()
let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray2"]
let iextents = [ [2;3;4]; [5;6;7] ]
let oarray = "oarray"

let p, q = unaryLoop iarrays.[0] iextents.[0] (newln inner) c 0
