
#load "sketches.fs"
open iterators

/// Inserts a tab character at the beginning of each string in the input list.
let tab x = 
    x |> List.map (fun y -> String.concat "" ["\t"; y])

/// Inserts a tab character at the end of each string in the input list.
let newln x = 
    x |> List.map (fun y -> String.concat "" [y; "\n"])
    
/// Creates a two-element list of the input string plus '{', and '}'.
let brace x = 
    List.append [String.concat " " [x; "{\n"]] ["}\n"]

/// Module containing all the logic for autogenerating nested_for loops of variable arity.
/// Contains optimizations for symmetry and commutativity.
module NestedLoop =

    /// Finds the ranks corresponding to each extents list.
    /// <param name="extents"> A list of extents. </param>
    let rec private rankList extents = 
        match extents with
        | []           -> []
        | head :: tail -> (List.length head) :: rankList tail

    /// Generates a new iterator name based on an input number.
    /// <param name="i"> An integer to append to "__i". </param>
    let private indName (i: int) = 
        String.concat "" ["__i"; (string i)]

    /// Generates a list of new iterator names based on a range, e.g., "__i3"
    /// <param name="min"> First integer to append to "__i". </param>
    /// <param name="max"> Last integer to append to "__i". </param>
    let rec private indNames min max =
        List.init (max - min) (fun index -> indName (index + min))

    /// Generates a list of new iterator names for multiple variables based on a minumum and the ranks of variables to loop over.
    /// <param name="min"> First integer to append to "__i". </param>
    /// <param name="ranks"> List of variable ranks. </param>
    let rec private indNames2 min ranks =
        match ranks with
        | []           -> []
        | head :: tail -> indNames min (min+head) :: indNames2 (min+head) tail

    /// Finds which iterators should serve as a the minimum for a given loop if the variables are commutative; otherwise set the minimum to 0.
    /// <param name="comGroups"> A commutativity vector. </param>
    /// <param name="iNames"> Iterator names for all variables. </param>
    let rec private comImins (comGroups: string list) (iNames: string list list) =
        let inhead, intail = List.head iNames, List.tail iNames

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

    /// For each dimension, finds whether a dimension is symmetric with the next.
    /// <param name="symGroup"> A symmetry vector. </param>
    let rec private isSym symGroups =
        match symGroups with
        | []           -> false
        | [head]       -> false
        | head :: tail -> if head = tail.Head then true else isSym tail
        
    /// Finds which iterators should serve as a the minimum for a given loop if the dimensions are symmetric; otherwise set the minimum to 0.
    /// <param name="symGroups"> A symmetry vector. </param>
    /// <param name="iNames"> Iterator names for all variables. </param>
    let private symImins (symGroups: string list) (iNames: string list) =
        List.init symGroups.Length (
            fun i -> if i = 0 then string 0 else if symGroups.[i] = symGroups.[i-1] then iNames.[i-1] else string 0
        )

    /// Enum for the symmetry/commutativity state of a variable.
    type private SymcomState =
    | Symmetric   = 0
    | Commutative = 1
    | Both        = 2
    | Neither     = 3

    /// Finds the states of all the input variables, given symmetry and commutativity vectors.
    /// <param name="arrayNames"> List of variable names. </param>
    /// <param name="symGroups"> A list of symmetry vectors. </param>
    /// <param name="comGroups"> A commutativity vector. </param>
    let rec private vStates (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
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

    /// Chooses the correct iterator minimum for all input variables.
    /// <param name="arrayNames"> List of variable names. </param>
    /// <param name="symGroups"> A list of symmetry vectors. </param>
    /// <param name="comGroups"> A commutativity vector. </param>
    let private iminList (arrayNames: string list) (symGroups: string list list) (comGroups: string list) = 
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

    /// Generates a single "for(...)" statement.
    /// <param name="iName"> Iterator name. </param>
    /// <param name="iMin"> Iterator minimum name. </param>
    /// <param name="arrayName"> Previous variable name. </param>
    let private loopLine iName iMin arrayName =
        String.concat "" ["for("; iName; " = "; iMin; "; "; iName; " < "; arrayName; ".current_extent(); "; iName; "++)";]

    /// Generates a call to operator().
    /// <param name="arrayName"> Variable name. </param>
    /// <param name="iName"> Iterator name. </param>
    let private index arrayName iName =
        String.concat "" [arrayName; "("; iName; ")"]

    /// Generates an OpenMP parallelization line. Inserts a "private" clause for the input iterator name.    
    /// <param name="iName"> Iterator name. </param>
    let private ompLine iName =
        String.concat "" ["#pragma omp parallel for private("; iName; ")"]

    /// Generates an iterator declaration line.
    /// <param name="iType"> Iterator type. </param>
    /// <param name="iName"> Iterator name. </param>
    let private declLine iType iName =
        String.concat "" [iType; " "; iName; " = 0;"]

    /// Autogenerate a unary nested_for loop.
    /// <param name="arrayName"> Variable name. </param>
    /// <param name="indNames"> Iterator minimum names. </param>
    /// <param name="iMins"> Iterator minimum names. </param>
    /// <param name="inner"> "Inner" block, i.e., code to place inside all the loops. </param>
    /// <param name="ompLevels"> Number of OpenMP levels. </param>
    let private unaryLoop (arrayName: string) (indNames: string list) (iMins: string list) (inner: string list) (ompLevels: int) =
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

    /// Autogenerate an N-ary nested_for loop.
    /// <param name="arrayNames"> Variable names. </param>
    /// <param name="indNames"> Iterator minimum names. </param>
    /// <param name="iMins"> Iterator minimum names. </param>
    /// <param name="inner"> "Inner" block, i.e., code to place inside all the loops. </param>
    /// <param name="ompLevels"> Number of OpenMP levels. </param>
    let private naryLoop (arrayNames: string list) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) =
        assert (arrayNames.Length = indNames.Length)
        assert (arrayNames.Length = iMins.Length)
        assert (arrayNames.Length = ompLevels.Length)

        let rec naryLoop' (arrayNames: string list) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) = 
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
                | head :: tail ->  (fun i -> unaryLoop arrHead indHead iminHead i ompHead) :: naryLoop' arrTail indTail iminTail inner ompTail

        naryLoop' arrayNames indNames iMins inner ompLevels
        |> List.rev
        |> List.fold (fun i elem -> elem i) inner

    /// Autogenerate a unary nested_for loop.
    /// <param name="arrayName"> Variable name. </param>
    /// <param name="indNames"> Iterator minimum names. </param>
    /// <param name="iMins"> Iterator minimum names. </param>
    /// <param name="inner"> "Inner" block, i.e., code to place inside all the loops. </param>
    /// <param name="ompLevels"> Number of OpenMP levels. </param>
    let Unary (arrayName: string) (symGroups: string list) (inner: string list) (ompLevels: int) =
        unaryLoop arrayName (indNames2 0 (rankList [symGroups])).Head (iminList [arrayName] [symGroups] ["1"]).Head inner ompLevels

    /// Autogenerate an N-ary nested_for loop.
    /// <param name="arrayNames"> Variable names. </param>
    /// <param name="indNames"> Iterator minimum names. </param>
    /// <param name="iMins"> Iterator minimum names. </param>
    /// <param name="inner"> "Inner" block, i.e., code to place inside all the loops. </param>
    /// <param name="ompLevels"> Number of OpenMP levels. </param>
    let Nary (arrayNames: string list) (symGroups: string list list) (comGroups: string list) (inner: string list) (ompLevels: int list) =
        naryLoop arrayNames (indNames2 0 (rankList symGroups)) (iminList arrayNames symGroups comGroups) inner ompLevels

    /// Find the final array names for a list of variables; useful when substituting into inner blocks.
    /// <param name="arrayNames"> Variable names. </param>
    /// <param name="symGroups"> A list of symmetry vectors. </param>
    let LastArrayNames (arrayNames: string list) (symGroups: string list list) =
        let indNames = indNames2 0 (rankList symGroups)
        List.init arrayNames.Length (
            fun i ->
                match symGroups.[i] with
                | [] -> failwith "Empty index names list."
                | [head] -> head
                | head :: tail -> List.last indNames.[i]
                |> fun x -> String.concat "" [arrayNames.[i]; x]
        )


let inner = ["iarray.read();"; "oarray = iarray;"; "oarray.write();"]
let iarrays = ["iarray1"; "iarray1"; "iarray2"; "iarray3"]
let iextents = [ [2;3;4]; [5;6;7]; [7;8]; [2;3] ]
let oarray = "oarray"
let symm = [ ["1";"1";"3"]; ["1";"1";"3"]; ["1";"1"]; ["1";"2"] ]
let comm = ["1";"1";"2";"3"]


let ind = 2
let a = NestedLoop.Unary iarrays.[ind] symm.[ind] (newln inner) 1
let b = NestedLoop.Nary iarrays symm comm (newln inner) [1;0;0;0]
