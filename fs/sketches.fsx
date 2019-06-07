// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "sketches.fs"
open iterators

// Define your library scripting code here

let loopText itype iname (imin: int) (imax: int) =
    String.concat "" ["for("; itype; " "; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]
        
type Counter() =
    let mutable num = 0
    member this.Last = num-1
    member this.This = num
    member this.Next = num+1
    member this.LastSuffix = 
        String.concat "" ["__i"; (string (num - 1))]
    member this.ThisSuffix = 
        String.concat "" ["__i"; (string num)]
    member this.NextSuffix = 
        num <- num + 1
        String.concat "" ["__i"; (string num)]

let loop (imin: int) (imax: int) (ctr: Counter) =
    loopText "int" ctr.ThisSuffix imin imax

let index i arrayName =
    String.concat "" [arrayName; "("; i; ")"]

let indexToTemp i arrayName (ctr: Counter) =
    if i = 0 then
        String.concat "" ["auto "; arrayName; ctr.NextSuffix; " = "; (index ctr.LastSuffix arrayName); ";\n"]
    else
        String.concat "" ["auto "; arrayName; ctr.NextSuffix; " = "; (index ctr.LastSuffix (String.concat "" [arrayName; ctr.ThisSuffix])); ";\n"]


let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["\n}"]

let rec loopNestRec (arrayName: string) (extents: int list) (inner: string list) (ctr: Counter) =
    let argCounter = Counter()
    match extents with
    | [] -> inner
    | head::tail -> (
        let braced = brace (loop 0 head ctr)
        List.concat [ [braced.[0]];
                      tab [(indexToTemp ctr.This arrayName ctr)];
                      tab (loopNestRec arrayName tail inner ctr);
                      [braced.[1]] ]

    )

let rec loopNestNary (arrayNames: string list) (extents: int list list) (inner: string list) (ctr: Counter) =
    assert (List.length arrayNames = List.length extents)
    match arrayNames with
    | [] -> failwith "Empty array names list." // Should be impossible for recursive calls; N-ary nested_for should terminate in loopNestRec
    | [head] -> loopNestRec head extents.[0] inner ctr
    | head::tail -> loopNestRec head extents.[0] (loopNestNary tail extents.[1..] inner ctr) ctr





let c = Counter()
let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray2"]
let iextents = [ [2;3;4]; [5;6;7] ]
let oarray = "oarray"

loopNestNary iarrays iextents (newln inner) c
