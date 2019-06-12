open System.IO
// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "sketches.fs"
open iterators

// Define your library scripting code here

let rec rankList extents = 
    match extents with
    | []           -> []
    | head :: tail -> (List.length head) :: rankList tail

let indName (i: int) = 
    String.concat "" ["__i"; (string i)]

let rec indNames min max =
    List.init (max - min) (fun index -> indName (index + min))

let rec indNames2 min ranks =
    match ranks with
    | []           -> []
    | head :: tail -> indNames min (min+head) :: indNames2 (min+head) tail

let rec comImins (comGroups: string list) (inames: string list list) =
    let inhead, intail = List.head inames, List.tail inames

    (List.init (List.length inhead) (fun index -> string 0)) ::
    match comGroups with
    | []           -> [] 
    | [head]       -> []
    | head :: tail -> 
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
    List.init (List.length symGroups) (
        fun i -> if i = 0 then string 0 else if symGroups.[i] = symGroups.[i-1] then inames.[i-1] else string 0
    )

type SymcomState =
| Symmetric   = 0
| Commutative = 1
| Both        = 2
| Neither     = 3

let rec vStates (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
    match arrayNames with 
    | [] -> []
    | arrHead :: arrTail ->
        (if isSym (List.head symGroups) then SymcomState.Symmetric else SymcomState.Neither) ::
        match arrTail with
        | [] -> []
        | _  -> 
            List.init ((List.length arrayNames)-1) (
                fun index ->
                    if comGroups.[index+1] = comGroups.[index] && arrayNames.[index+1] = arrayNames.[index] then
                        if isSym symGroups.[index+1] then SymcomState.Both else SymcomState.Commutative
                    else 
                        if isSym symGroups.[index+1] then SymcomState.Symmetric else SymcomState.Neither
            )

let iminList (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
    assert (List.length arrayNames = List.length symGroups)
    assert (List.length arrayNames = List.length comGroups)

    let ranks = rankList symGroups
    let indexNames = indNames2 0 ranks
    let cimins = comImins comGroups indexNames
    let states = vStates arrayNames symGroups comGroups

    List.init (List.length arrayNames) (
        fun index -> 
            match states.[index] with
            | SymcomState.Neither     -> List.init ranks.[index] (fun x -> string 0)
            | SymcomState.Symmetric   -> symImins symGroups.[index] indexNames.[index]
            | SymcomState.Commutative -> cimins.[index]
            | SymcomState.Both        -> cimins.[index] // can perhaps optimize more, just lazy
            | _                       -> failwith "Invalid symmetry/commutativity state"
    )

let loopLine iName iMin arrayName =
    String.concat "" ["for("; iName; " = "; iMin; "; "; iName; " < "; arrayName; ".current_extent(); "; iName; "++)";]

let index arrayName iName =
    String.concat "" [arrayName; "("; iName; ")"]

let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

let newln x = 
    x |> List.map (fun y -> String.concat "" [y; "\n"])
    
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["}\n"]

let ompLine iName =
    String.concat "" ["#pragma omp parallel for private("; iName; ")"]

let declLine iType iName =
    String.concat "" [iType; " "; iName; " = 0;"]

let unaryLoop (arrayName: string) (indNames: string list) (iMins: string list) (inner: string list) (ompLevels: int) =
    List.init (List.head (rankList [indNames])) (
        fun i -> 
            let ompline = if ompLevels > i then [ompLine indNames.[i]] else []
            let braced = match i with
                         | 0 -> brace (loopLine indNames.[i] iMins.[i] arrayName)
                         | _ -> brace (loopLine indNames.[i] iMins.[i] (String.concat "" [arrayName; indNames.[i-1]]))
            match i with
            | 0 -> tab [String.concat "" ["auto "; arrayName; indNames.[i]; " = "; index arrayName indNames.[i]; ";\n"]]
            | _ -> tab [String.concat "" ["auto "; arrayName; indNames.[i]; " = "; (index (String.concat "" [arrayName; indNames.[i-1]]) indNames.[i]); ";\n"]]
            |> fun x -> fun y -> List.concat [ newln [declLine "int" indNames.[i]]; newln ompline; [braced.[0]]; x; tab y; [braced.[1]] ]
    )
    |> List.rev
    |> List.fold (fun i elem -> elem i) inner

let rec catLoops (arrayNames: string list) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) = 
    match arrayNames with
    | [] -> failwith "Empty array names list."
    | _  ->
        let arrHead, arrTail = List.head arrayNames, List.tail arrayNames
        let indHead, indTail = List.head indNames, List.tail indNames
        let iminHead, iminTail = List.head iMins, List.tail iMins
        let ompHead, ompTail = List.head ompLevels, List.tail ompLevels

        match arrayNames with
        | [] -> failwith "Impossible match." // Should be impossible for recursive calls; N-ary nested_for should terminate in unaryLoop
        | [head]       -> [(fun i -> unaryLoop arrHead indHead iminHead i ompHead)]
        | head :: tail ->  (fun i -> unaryLoop arrHead indHead iminHead i ompHead) :: catLoops arrTail indTail iminTail inner ompTail

let rec naryLoop (arrayNames: string list) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) =
    assert (List.length arrayNames = List.length indNames)
    assert (List.length arrayNames = List.length iMins)
    assert (List.length arrayNames = List.length ompLevels)

    catLoops arrayNames indNames iMins inner ompLevels
    |> List.rev
    |> List.fold (fun i elem -> elem i) inner



let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray1"; "iarray2"; "iarray3"]
let iextents = [ [2;3;4]; [5;6;7]; [7;8]; [2;3] ]
let oarray = "oarray"
let symm = [ ["1";"1";"3"]; ["1";"1";"3"]; ["1";"1"]; ["1";"2"] ]
let comm = ["1";"1";"2";"3"]


let mins = iminList iarrays symm comm
let ind = 2
let a = unaryLoop iarrays.[ind] (indNames2 0 (rankList symm)).[ind] mins.[ind] (newln inner) 1
let b = naryLoop iarrays (indNames2 0 (rankList symm)) mins (newln inner) [1;0;0;0]
