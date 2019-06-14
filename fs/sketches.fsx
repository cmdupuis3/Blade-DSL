
open System.IO


#load "sketches.fs"
open iterators


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

    (List.init inhead.Length (fun index -> string 0)) ::
    match comGroups with
    | []           -> [] 
    | [head]       -> []
    | head :: tail -> 
        List.init tail.Length (
            fun index ->
                if comGroups.[index+1] = comGroups.[index] then
                    List.init intail.[index].Length (fun index -> inhead.[index])
                else 
                    List.init intail.[index].Length (fun index -> string 0)
        )

let rec isSym symGroup =
    match symGroup with
    | []           -> false
    | [head]       -> false
    | head :: tail -> if head = tail.Head then true else isSym tail
    
let symImins (symGroups: string list) (inames: string list) = 
    List.init symGroups.Length (
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
        (if isSym symGroups.Head then SymcomState.Symmetric else SymcomState.Neither) ::
        match arrTail with
        | [] -> []
        | _  -> 
            List.init (arrayNames.Length - 1) (
                fun index ->
                    if comGroups.[index+1] = comGroups.[index] && arrayNames.[index+1] = arrayNames.[index] then
                        if isSym symGroups.[index+1] then SymcomState.Both else SymcomState.Commutative
                    else 
                        if isSym symGroups.[index+1] then SymcomState.Symmetric else SymcomState.Neither
            )

let iminList (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
    assert (arrayNames.Length = symGroups.Length)
    assert (arrayNames.Length = comGroups.Length)

    let ranks = rankList symGroups
    let indexNames = indNames2 0 ranks
    let cimins = comImins comGroups indexNames
    let states = vStates arrayNames symGroups comGroups

    List.init arrayNames.Length (
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
    List.init (rankList [indNames]).Head (
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
        let arrHead,  arrTail  = arrayNames.Head, arrayNames.Tail
        let indHead,  indTail  = indNames.Head, indNames.Tail
        let iminHead, iminTail = iMins.Head, iMins.Tail
        let ompHead,  ompTail  = ompLevels.Head, ompLevels.Tail

        match arrayNames with
        | [] -> failwith "Impossible match."
        | [head]       -> [(fun i -> unaryLoop arrHead indHead iminHead i ompHead)]
        | head :: tail ->  (fun i -> unaryLoop arrHead indHead iminHead i ompHead) :: catLoops arrTail indTail iminTail inner ompTail

let naryLoop (arrayNames: string list) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) =
    assert (arrayNames.Length = indNames.Length)
    assert (arrayNames.Length = iMins.Length)
    assert (arrayNames.Length = ompLevels.Length)

    catLoops arrayNames indNames iMins inner ompLevels
    |> List.rev
    |> List.fold (fun i elem -> elem i) inner

let lastArrayNames (arrayNames: string list) (indNames: string list list) =
    List.init arrayNames.Length (
        fun i ->
            match indNames.[i] with
            | [] -> failwith "Empty index names list."
            | [head] -> head
            | head :: tail -> List.last indNames.[i]
            |> fun x -> String.concat "" [arrayNames.[i]; x]
    )



let rec countTokensImpl (line: string) (token: string) (ctr: int) =
    match line.IndexOf token with
    | -1 -> 0
    | i -> 1 + countTokensImpl (line.Substring (token.Length + i)) token i

let countTokens (line: string) (token: string) =
    countTokensImpl line token 0

let rec findTokensImpl (line: string) (token: string) (ctr: int) =
    match line.IndexOf token with
    | -1 -> []
    | i -> ctr + i :: findTokensImpl (line.Substring (token.Length + i)) token (token.Length + i + ctr)

let findTokens (line: string) (token: string) =
    findTokensImpl line token 0

let hasObjectFor (lines: string list) =
    List.init lines.Length (fun i -> lines.[i].Contains "object_for")

let hasMethodFor (lines: string list) =
    List.init lines.Length (fun i -> lines.[i].Contains "method_for")



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
