// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

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

(* Unary nested_for loop *)
let rec unaryLoop (arrayName: string) (extents: int list) (inner: string list) (depth: int) (counter: int) (ompLevels: int) =
    match extents with
    | [] -> (inner, String.concat "" [arrayName; "__i"; string (counter - 1)], depth - 1)
    | head :: tail ->
        let nextLoop, lastArrayName, lastDepth = unaryLoop arrayName tail inner (depth + 1) (counter + 1) (ompLevels - 1)
        let braced = brace (loopLine 0 head counter)
        let ompline = if ompLevels > 0 then [ompLine counter] else []
        let nextArrayLine indexed = tab [String.concat "" ["auto "; arrayName; "__i"; string counter; " = "; indexed; ";\n"]]
        if depth = 0 then 
            tab [String.concat "" ["auto "; arrayName; "__i"; string counter; " = "; index counter arrayName; ";\n"]]
        else
            tab [String.concat "" ["auto "; arrayName; "__i"; string counter; " = "; (index counter (String.concat "" [arrayName; "__i"; string (counter-1)])); ";\n"]]
        |> fun x ->
            List.concat [ newln [declLine counter "int"]; newln ompline; [braced.[0]]; x; tab nextLoop; [braced.[1]] ],
            lastArrayName,
            lastDepth


(* N-ary nested_for loop *)
let rec naryLoop (arrayNames: string list) (extents: int list list) (inner: string list) (arg: int) (ompLevels: int list) =
    assert (List.length arrayNames = List.length extents)
    match arrayNames with
    | [] -> failwith "Empty array names list." // Should be impossible for recursive calls; N-ary nested_for should terminate in unaryLoop
    | [head] -> 
        let lastLoop, lastArrayName, lastDepth = unaryLoop head extents.[0] inner 0 0 ompLevels.[0]
        lastLoop, [lastArrayName], lastDepth
    | head :: tail ->
        let nextLoop, nextArrayName, nextDepth = naryLoop tail extents.[1..] inner (arg + 1) ompLevels.[1..]
        let thisLoop, thisArrayName, thisDepth = unaryLoop head extents.[0] (nextLoop) 0 (nextDepth + 1) ompLevels.[0]
        thisLoop, thisArrayName :: nextArrayName, thisDepth + nextDepth + 1



let rec rankList extents = 
    match extents with
    | []           -> []
    | head :: tail -> (List.length head) :: rankList tail

let indName (i: int) = String.concat "" ["__i"; (string i)]

let rec indNames min max =
    List.init (max - min) (fun index -> indName (index + min))

let rec indNames2 min ranks =
    match ranks with
    | []           -> []
    | head :: tail -> indNames min (min+head) :: indNames2 (min+head) tail

let rec comImins (comGroups: string list) (inames: string list list) =
    let inhead, intail = List.head inames, List.tail inames
    match comGroups with
    | []           -> []
    | [head]       -> [List.init (List.length inhead) (fun index -> string 0)]
    | head :: tail -> (List.init (List.length inhead) (fun index -> string 0)) ::
                      List.init (List.length tail) (
                          fun index ->
                              if comGroups.[index+1] = comGroups.[index] then
                                  List.init (List.length intail.[index]) (fun index -> inhead.[index])
                              else 
                                  List.init (List.length intail.[index]) (fun index -> string 0)
                      )

let rec isSym symGroup =
    match symGroup with
    | []           -> false
    | [head]       -> false
    | head :: tail -> if head = List.head tail then true else isSym tail


let symImins (symGroups: string list) (inames: string list) = 
    let rec symIminsRev (symGroupsInternal: string list) (inamesInternal: string list) = 
        match symGroupsInternal with
        | []                  -> []
        | [head]              -> [string 0]
        | head :: mid :: tail -> (if head = mid then inamesInternal.[1] else string 0) :: (symIminsRev (mid :: tail) (List.tail inamesInternal))
    List.rev (symIminsRev symGroups inames)

type SymcomState =
| Symmetric   = 0
| Commutative = 1
| Both        = 2
| Neither     = 3

let rec vStates (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
    match arrayNames with 
    | [] -> []
    | arrHead :: arrTail ->
        match arrTail with
        | [] -> [(if isSym (List.head symGroups) then SymcomState.Symmetric else SymcomState.Neither)]
        | _  -> 
            SymcomState.Neither :: 
            List.init ((List.length arrayNames)-1) (
                fun index ->
                    if comGroups.[index+1] = comGroups.[index] && arrayNames.[index+1] = arrayNames.[index] then
                        if isSym symGroups.[index+1] then SymcomState.Both else SymcomState.Commutative
                    else 
                        if isSym symGroups.[index+1] then SymcomState.Symmetric else SymcomState.Neither
            )

let imins (arrayNames: string list) (extents: int list list) (symGroups: string list list) (comGroups: string list) = 
    assert (List.length arrayNames = List.length extents)
    assert (List.length arrayNames = List.length symGroups)
    assert (List.length arrayNames = List.length comGroups)

    let ranks = rankList extents
    let indices = indNames2 0 ranks
    let cimins = comImins comGroups indices
    let states = vStates arrayNames symGroups comGroups

    List.init (List.length arrayNames) (
        fun index -> 
            match states.[index] with
            | SymcomState.Neither     -> List.init ranks.[index] (fun x -> string 0)
            | SymcomState.Symmetric   -> symImins symGroups.[index] indices.[index]
            | SymcomState.Commutative -> cimins.[index]
            | SymcomState.Both        -> cimins.[index] // can optimize more, just lazy
            | _                       -> failwith "Invalid symmetry/commutativity state"
    )

let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray1"; "iarray2"; "iarray3"]
let iextents = [ [2;3;4]; [5;6;7]; [7;8]; [2;3] ]
let oarray = "oarray"
let symm = [ ["1";"2";"3"]; ["1";"2";"3"]; ["1";"1"]; ["1";"2"] ]
let comm = ["1";"1";"2";"3"]

let p, q, r = unaryLoop iarrays.[0] iextents.[0] (newln inner) 0 3 2
let x, y, z = naryLoop iarrays iextents (newln inner) 0 [2;1;0]
