// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "sketches.fs"
open iterators

// Define your library scripting code here

let generate_for itype iname (imin: int) (imax: int) =
    String.concat "" ["for("; itype; " "; iname; " = "; (string imin); "; "; iname; " < "; (string imax); "; "; iname; "++)";]
        
type Counter() =
    let mutable num = 0
    member this.next = 
        num <- num + 1
        String.concat "" ["__i"; (string num)]

let counter = new Counter()
let loop (imin: int) (imax: int) =
    generate_for "int" counter.next imin imax


let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" ["\n"; y])
    
let brace x = 
     List.append (List.append ["{\n"] x) ["\n}"]

let nest_for loop inner =
    List.append [loop] ((brace << tab) inner)




let myfunc = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]

let a = generate_for "int" "i" 0 10
let b = generate_for "int" "j" 0 20

