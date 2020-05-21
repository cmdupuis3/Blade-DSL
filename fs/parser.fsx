


// For NetCDF integration, information about types and ranks is needed at compile time...

open System.Runtime.InteropServices

[<DllImport(@"/home/username/tropical/nested_funcs/Debug/libnested_funcs.so")>]
extern int get_num_dims(
        [<MarshalAs(UnmanagedType.LPStr)>] string fileName,
        [<MarshalAs(UnmanagedType.LPStr)>] string varName)

[<DllImport(@"/home/username/tropical/nested_funcs/Debug/libnested_funcs.so")>]
extern int get_var_type(
    [<MarshalAs(UnmanagedType.LPStr)>] string fileName,
    [<MarshalAs(UnmanagedType.LPStr)>] string varName)

[<DllImport(@"/home/username/tropical/nested_funcs/Debug/libnested_funcs.so")>]
extern void get_var_dim_types(
    [<MarshalAs(UnmanagedType.LPStr)>] string fileName,
    [<MarshalAs(UnmanagedType.LPStr)>] string varName,
    int[] dimTypes)


let matchNCtype = function
    | 1 | 2 -> "char"
    | 3 -> "short"
    | 4 -> "int"
    | 5 -> "float"
    | 6 -> "double"
    | 7 -> "uchar"
    | 8 -> "ushort"
    | 9 -> "uint"
    | 10 -> "int64"
    | 11 -> "uint64"
    | 12 -> "char*" // ?
    | _ -> failwith "This NetCDF type is currently unsupported."


let getNCnumDims fileName variableName = get_num_dims(fileName, variableName)

let getNCvarType fileName variableName = get_var_type(fileName, variableName) |> matchNCtype

let getNCdimTypes fileName variableName =
    let numDims = getNCnumDims fileName variableName
    let dimTypes = Array.init numDims (fun i -> -1)
    get_var_dim_types(fileName, variableName, dimTypes)
    dimTypes
    |> Array.map matchNCtype
    |> Array.toList

//let getNCvarDimIDs fileName variableName

(*

let file = @"/home/username/Data/indonesia/hqp_block11.nc"
let var = @"hqp"

get_num_dims(file, var)
get_var_type(file, var)

getNCnumDims file var
getNCvarType file var

*)


// Hack alert
let unquote (s: string) =
    s.[1..(s.Length-2)]

let quote (s: string) =
    String.concat "" ["\""; s; "\""]

type ArrayInfo =
    {
        ExtentsName: string
    }

type NetCDFInfo =
    {
        FileName: string
        VariableName: string
    }

type NestedArrayInfo =
    | Array of ArrayInfo
    | NetCDF of NetCDFInfo

/// Container for array pragma information
type NestedArray =
    {
        Name: string
        Type: string
        Rank: int
        Symm: int list Option
        Info: NestedArrayInfo
    }

let extentsName array =
    match array.Info with
    | Array info -> info.ExtentsName
    | NetCDF info -> String.concat "" [array.Name; "_extents"]

type NetCDFOutputInfo =
    {
        DimNames: string list
        DimExtents: int list
        DimValNames: string list
    }

/// Container for function pragma information
type NestedFunction =
    {
        Name:  string
        Arity: int Option
        INames: string list
        IRank: int list
        OName: string
        ORank: int
        Comm:  int list Option
        ParallelismLevels: int list
        Inner: string list
        NCInfo: NetCDFOutputInfo Option
    }


type Loop =
    {
        iarrayName: string
        iarrayType: string
        iarrayLevels: int
        iRank: int
        iExtents: string
        indNames: string list
        iMins: string list
        parLevels: int
    }

/// Inserts a tab character at the beginning of each string in the input list.
let tab x =
    x |> List.map (fun y -> String.concat "" ["\t"; y])

/// Inserts a tab character at the end of each string in the input list.
let newln (x: string list) =
    x |> List.map (fun y -> String.concat "" [y; "\n"])

/// Creates a two-element list of the input string plus '{', and '}'.
let brace x =
    [String.concat " " [x; "{\n"]] @ ["}\n"]

/// Generate all text for a single nested loop (language agnostic).
/// <param name="outerNested"> A list of functions for generating nested lines outside the loop line, based on a template. </param>
/// <param name="innerNested"> A list of functions for generating nested lines inside the loop line, based on a template. </param>
/// <param name="loopLine"> A template for the for-loop expression. </param>
/// <param name="scope"> A template for the scope enclosed by the for-loop, e.g., "{ (inner) }". </param>
/// <param name="loop"> Loop object constructed by NestedLoop.Unary or NestedLoop.Nary. </param>
/// <param name="i"> Current N-ary loop index; this is threaded through all loops. </param>
/// <param name="j"> Current unary loop index; this resets for every new nested loop. </param>
/// <param name="inner"> Inner block of text i.e. the actual algorithm. </param>
let loopBuilder iteratorDeclLine outerNested outerDistributed loopLine (scope: string -> string list) innerNested innerDistributed loop i j inner =
    let nestedText      = List.map (fun x -> [x loop j]   |> newln |> List.head)
    let distributedText = List.map (fun x -> [x loop j i] |> newln |> List.head)
    let outerText = (outerNested |> nestedText) @ (outerDistributed |> distributedText)
    let innerText = (innerNested |> nestedText) @ (innerDistributed |> distributedText)

    let iteratorDecl = iteratorDeclLine "int" loop.indNames.[j]
    let scoped = scope (loopLine loop.indNames.[j] loop.iMins.[j] loop.iExtents (string j))
    List.concat [newln [iteratorDecl]; outerText; [scoped.Head]; tab innerText; tab inner; [scoped.Tail.Head]]


let cppIteratorNameGenerator min index =
    String.concat "" ["__i"; (string (index + min))]

/// Generates a single "for(...)" statement.
/// <param name="iName"> Iterator name. </param>
/// <param name="iMin"> Iterator minimum name. </param>
/// <param name="extentName"> Extent vector name. </param>
/// <param name="extentIndex"> Element of extent vector to use as iterator maximum. </param>
let cppLoopLine iName iMin extentName extentIndex =
    String.concat "" ["for("; iName; " = "; iMin; "; "; iName; " < "; extentName; "["; extentIndex; "]; "; iName; "++)";]

/// Generates a call to operator().
/// <param name="arrayName"> Variable name. </param>
/// <param name="iName"> Iterator name. </param>
let cppIndex arrayName iName =
    String.concat "" [arrayName; "["; iName; "]"]

/// Generates an iterator declaration line.
/// <param name="iType"> Iterator type. </param>
/// <param name="iName"> Iterator name. </param>
let cppIteratorDeclLine iType iName =
    String.concat "" [iType; " "; iName; " = 0;"]

/// Generate a C++ array declaration.
let cppArrayDeclLine loop j =
    let pre = String.concat "" ["promote<"; loop.iarrayType; ", "; string (loop.iarrayLevels-j+loop.iRank-1); ">::type "; loop.iarrayName; loop.indNames.[j]; " = "]

    match j with
    | 0 -> cppIndex loop.iarrayName loop.indNames.[j]
    | _ -> cppIndex (String.concat "" [loop.iarrayName; loop.indNames.[j-1]]) loop.indNames.[j]
    |> fun x -> String.concat "" [pre; x; ";"]

/// Generates an OpenMP parallelization line. Inserts a " " clause for the input iterator name.
let cppOmpLine loop j i =
    if loop.parLevels > j then
        String.concat "" ["#pragma omp parallel for private("; loop.indNames.[j]; ")"]
    else ""

let cppNestedNCstart loop j =
    String.concat "" ["start["; string j; "] = "; loop.indNames.[j]; ";"]

type pushText<'T> =
    abstract member PushOuterNested: (Loop -> int -> string) -> unit
    abstract member PushOuterDistributed: (Loop -> int -> int -> string) -> unit
    abstract member PushInnerNested: (Loop -> int -> string) -> unit
    abstract member PushInnerDistributed: (Loop -> int -> int -> string) -> unit

[<AbstractClass>]
type LoopTextBase() =
    abstract member IteratorName: int -> int -> string
    abstract member Index: string -> string -> string
    abstract member Zero: string
    abstract member Text: Loop -> int -> int -> string list -> string list

type LoopTextGenerator (outerNested: (Loop -> int -> string) list,
                        outerDistributed: (Loop -> int -> int -> string) list,
                        innerNested: (Loop -> int -> string) list,
                        innerDistributed: (Loop -> int -> int -> string) list) =
    inherit  LoopTextBase()
    let mutable OuterNested      = outerNested
    let mutable OuterDistributed = outerDistributed
    let mutable InnerNested      = innerNested
    let mutable InnerDistributed = innerDistributed
    new() = LoopTextGenerator([], [], [], [])

    override this.IteratorName i j = ""
    override this.Index a b = ""
    override this.Zero = string 0
    override this.Text loop i j a = [""]

    interface pushText<LoopTextGenerator> with
        override this.PushOuterNested newOuterNested =
            OuterNested <- OuterNested @ [newOuterNested]
        override this.PushOuterDistributed newOuterDistributed =
            OuterDistributed <- OuterDistributed @ [newOuterDistributed]
        override this.PushInnerNested newInnerNested =
            InnerNested <- InnerNested @ [newInnerNested]
        override this.PushInnerDistributed newInnerDistributed =
            InnerDistributed <- InnerDistributed @ [newInnerDistributed]


type CppLoopTextGenerator (outerNested, outerDistributed, innerNested, innerDistributed) =
    inherit LoopTextGenerator(outerNested, outerDistributed, innerNested, innerDistributed)
    let mutable OuterNested      = outerNested
    let mutable OuterDistributed = outerDistributed
    let mutable InnerNested      = innerNested
    let mutable InnerDistributed = innerDistributed
    new() = CppLoopTextGenerator([], [], [], [])

    override this.IteratorName min index = cppIteratorNameGenerator min index
    override this.Index arrayName iName = cppIndex arrayName iName
    override this.Zero = string 0
    override this.Text loop i j inner = loopBuilder cppIteratorDeclLine OuterNested OuterDistributed cppLoopLine brace InnerNested InnerDistributed loop i j inner

    interface pushText<CppLoopTextGenerator> with
        override this.PushOuterNested newOuterNested =
            OuterNested <- OuterNested @ [newOuterNested]
        override this.PushOuterDistributed newOuterDistributed =
            OuterDistributed <- OuterDistributed @ [newOuterDistributed]
        override this.PushInnerNested newInnerNested =
            InnerNested <- InnerNested @ [newInnerNested]
        override this.PushInnerDistributed newInnerDistributed =
            InnerDistributed <- InnerDistributed @ [newInnerDistributed]

(********************************************************************************)

module NestedLoop =
    /// Union for the symmetry/commutativity state of a variable.
    type SymcomState =
        | Symmetric
        | Commutative
        | Both
        | Neither

    /// For each element, finds whether it's the same as the previous.
    /// <param name="elements"> A vector. </param>
    let rec private isSame (elements: 'a list) =
        List.init elements.Length (fun i -> if i = 0 then false else elements.[i] = elements.[i-1])

    /// Finds the states of all the input variables, given symmetry and commutativity vectors.
    /// <param name="arrayNames"> List of variable names. </param>
    /// <param name="symGroups"> A list of symmetry vectors. </param>
    /// <param name="comGroups"> A commutativity vector. </param>
    let rec private vStates (arrayNames: string list) (symGroups: int list list) (comGroups: int list) =
        let symModes = symGroups |> List.map isSame
        let comModes = comGroups |> isSame
        let sameArray = arrayNames |> isSame

        List.init arrayNames.Length (fun i ->
            List.init symModes.[i].Length (fun j ->
                match (comModes.[i] && sameArray.[i]), symModes.[i].[j] with
                | false, false -> Neither
                | false, true  -> Symmetric
                | true,  false -> Commutative
                | true,  true  -> Both
            )
        )
    // let states = vStates ["arr1"; "arr2"; "arr2"] [[1;1]; [1;2;2]; [1;2;2]] [1;2;2];;

    /// Create a map of iterator minima
    let private iminMap (states: SymcomState list list) =
        List.init states.Length (fun i ->
            List.init states.[i].Length (fun j ->
                match states.[i].[j] with
                | Neither     -> (i, j)
                | Symmetric   -> (i, j-1)
                | Commutative -> (i-1, j)
                | Both        -> if j = 0 then (i-1, j) else (i, j-1) // is this ideal?
            )
        )

    /// Rearrange a table of functions ("items") to fold differently for commutative functions
    /// <param name="states"> List of symmetry/commutativity states for each unary nested iterator. </param>
    /// <param name="items"> Table of functions to reorganize; takes a counter and an input and returns an output of the same type as the input </param>
    let private swap (states: SymcomState list list) (items: (int -> 'a -> 'a) list list) =
        /// <param name="acc"> A counter threaded through all functions. </param>
        let rec swap' (states: SymcomState list list) (acc: int) (items: (int -> 'a -> 'a) list list) =
            match states with
            | [] -> []
            | [head] -> [List.init (items.Head.Length) (fun i -> items.Head.[i] (i + acc))]
            | head :: tail ->
                match head.Head with
                | (Neither | Symmetric) ->
                    (List.init (items.Head.Length) (fun i -> items.Head.[i] (i + acc))) :: swap' tail (acc + items.Head.Length) items.Tail
                | (Commutative | Both)  ->
                    let sub = swap' tail (acc + items.Head.Length) items.Tail
                    ((List.init (items.Head.Length) (fun i -> items.Head.[i] (i + acc)), sub.Head) ||> List.map2 (>>)) :: sub.Tail
        swap' (List.rev states) 0 (List.rev items)

    /// Autogenerate a unary nested_for loop.
    let private unaryLoop (loop: Loop) (textGenerator: LoopTextGenerator) (i: int) =
        List.init loop.iarrayLevels (fun j inner-> textGenerator.Text loop i j inner)
        |> List.rev

    /// Autogenerate an N-ary nested_for loop.
    let private naryLoop (loops: Loop list) (states: SymcomState list list) (textGenerator: LoopTextGenerator) =
        List.init states.Length (fun i ->
            unaryLoop loops.[i] textGenerator i
            |> List.map (fun y (acc: int) -> y)
        )
        |> swap states

    /// Substitute the first string with the second in a list of strings
    /// <param name="subs"> A list of substitution pairs; find the first, swap the second in. </param>
    /// <param name="text"> The text to be substituted. </param>
    let private subInner (subs: (string * string) list) (inner: string list) =
        let rec subInner' (subs: (string * string) list) (str: string) =
            match subs with
            | []           -> str
            | head :: tail ->
                let rec sub (str': string) =
                    if str'.Contains (fst head) then
                        let index = str'.IndexOf (fst head)
                        let pre = str'.[0..(index-1)]
                        let post = str'.[(index+(fst head).Length)..]
                        String.concat "" [sub pre; snd head; sub post]
                    else subInner' tail str'
                sub str
        inner |> List.map (subInner' subs)

    /// Autogenerate an N-ary nested_for loop.
    /// <param name="iarrays"> A list of input array classes. </param>
    /// <param name="oarray"> An output array class. </param>
    /// <param name="func"> A function class. </param>
    let Nary (iarrays: NestedArray list) (oarray: NestedArray) (func: NestedFunction) (textGenerator: LoopTextGenerator) =
        let ilevels = (iarrays |> List.map (fun x -> x.Rank), func.IRank) ||> List.map2 (-)

        let states =
            let comm = func.Comm |> function | Some comm -> comm | None -> (List.init iarrays.Length id)
            let iNames = iarrays |> List.map (fun x -> x.Name)
            let iSymms = iarrays |> List.map (fun x -> x.Symm |> function | Some s -> s | None -> (List.init x.Rank id))
            vStates iNames iSymms comm

        /// Generates a list of new iterator names for multiple variables based on a minumum and the ranks of variables to loop over.
        /// <param name="min"> First integer to append to "__i". </param>
        /// <param name="ranks"> List of variable ranks. </param>
        let rec indNames min (ranks: int list) =
            let rec indNames' min max = List.init (max - min) (textGenerator.IteratorName min)
            if ranks.IsEmpty then [] else indNames' min (min+ranks.Head) :: indNames (min+ranks.Head) ranks.Tail
        let indexNames = indNames 0 ilevels

        /// Chooses the correct iterator minimum for all input variables.
        let imins =
            let iMaps = iminMap states
            List.init ilevels.Length (fun i ->
                List.init ilevels.[i] (fun j ->
                    if iMaps.[i].[j] = (i, j) then textGenerator.Zero else indexNames.[fst iMaps.[i].[j]].[snd iMaps.[i].[j]]
                )
            )

        /// Last input array intermediate names to be subbed into the inner block
        let lastINames =
            let lastInds = indexNames |> List.map List.last
            List.init iarrays.Length (fun i ->
                if iarrays.[i].Rank = func.IRank.[i] then "" else lastInds.[i]
                |> fun x -> String.concat "" [func.INames.[i]; x]
            )

        /// Last output array intermediate name to be subbed into the inner block
        let lastOName =
            let index = fun (acc: int) x -> textGenerator.Index x (textGenerator.IteratorName acc 0)
            let iDiff i = iarrays.[i].Rank - func.IRank.[i]
            let oDiff   = oarray.Rank      - func.ORank
            List.init states.Length (fun i ->
                if (iDiff i > 0) && (oDiff > 0) then
                    List.init (if iDiff i > oDiff then oDiff else iDiff i) (fun i acc -> index acc)
                else
                    [fun (acc: int) x -> x]
            )
            |> swap states
            |> List.map (List.reduce (>>))
            |> List.reduce (>>)
            |> fun x -> x func.OName

        let subINames = List.zip func.INames lastINames
        let subOName = (func.OName, lastOName)
        let subbedInner = subInner (subINames @ [subOName] |> List.rev) func.Inner

        let iExtents = func.INames |> List.map (fun x -> String.concat "" [x; "_extents"])

        let loops = List.init (iarrays.Length) (fun i ->
            {
                iarrayName = func.INames.[i];
                iarrayType = iarrays.[i].Type;
                iarrayLevels = ilevels.[i];
                iRank = func.IRank.[i];
                iExtents = iExtents.[i];
                indNames = indexNames.[i];
                iMins = imins.[i];
                parLevels = func.ParallelismLevels.[i]
            }
        )

        let ret =
            naryLoop loops states textGenerator
            |> List.fold (List.fold (|>)) subbedInner
        ret, lastINames @ [lastOName]

    /// Autogenerate a unary nested_for loop.
    let Unary (iarray: NestedArray) (oarray: NestedArray) (func: NestedFunction) (textGenerator: LoopTextGenerator) =
        Nary [iarray] oarray func textGenerator

    let private inputFileIDname (array: NestedArray) =
        String.concat "" [array.Name; "_in_file_ncid"]

    let private outputFileIDname (array: NestedArray) =
        String.concat "" [array.Name; "_out_file_ncid"]

    let private variableIDname (array: NestedArray) =
        String.concat "" [array.Name; "_var_ncid"]

    let private startsName (array: NestedArray) =
        String.concat "" [array.Name; "_starts"]

    let private countsName (array: NestedArray) =
        String.concat "" [array.Name; "_counts"]

    let private dimValsNames (array: NestedArray) =
        List.init array.Rank (fun i -> String.concat "" [array.Name; "_dim_"; string i; "_vals"])

    /// Autogenerate an N-ary nested_for loop
    /// <param name="array"> An input array class. </param>
    let ncGet (textGenerator: LoopTextGenerator) (array: NestedArray) =
        let ncFileName, ncVarName =
            match array.Info with
            | NetCDF info -> info.FileName, info.VariableName
            | _ -> failwith "Cannot use NetCDF routines on non-NetCDF arrays"

        let indexNames = List.init (array.Rank-1) (fun i -> textGenerator.IteratorName 0 i)

        let iSymms = array.Symm |> function | Some s -> s | None -> (List.init array.Rank id)
        let states = vStates [array.Name] [iSymms] [0]
        /// Chooses the correct iterator minimum for all input variables.
        let imins =
            let iMaps = iminMap states
            List.init array.Rank (fun j ->
                if iMaps.Head.[j] = (0, j) then textGenerator.Zero else indexNames.[snd iMaps.Head.[j]]
            )

        /// Last input array intermediate names to be subbed into the inner block
        let lastIName = String.concat "" [array.Name; indexNames |> List.rev |> List.head]

        let extents = extentsName array
        let ncDimTypes = getNCdimTypes ncFileName ncVarName

        // Assuming dimensions and dimension variables have the same name... (should, but don't necessarily)
        let ncDimValDeclLines =
            List.init array.Rank (fun i ->
                String.concat "" [ncDimTypes.[i]; "* "; (dimValsNames array).[i];" = new "; ncDimTypes.[i]; "["; extents; "["; string i;"]];"]
            )
        let ncDimValLines =
            List.init array.Rank (fun i ->
                String.concat "" ["nc_get_var_"; ncDimTypes.[i]; "("; inputFileIDname array; ", "; variableIDname array; ", &"; (dimValsNames array).[i]; ");"]
            )

        /// nc_open call
        let ncInit = ([
            String.concat "" ["int "; inputFileIDname array; ";"]
            String.concat "" ["nc_open("; quote ncFileName; ", NC_NOWRITE, &"; inputFileIDname array; ");"]
            String.concat "" ["int "; variableIDname array; ";";]
            String.concat "" ["nc_inq_varid("; inputFileIDname array; ", "; quote ncVarName; ", &"; variableIDname array; ");"]
            String.concat "" ["int* "; array.Name; "_dim_ncids = new int[";  string array.Rank; "];"]
            String.concat "" ["size_t* "; extentsName array; " = new size_t["; string array.Rank; "];"]
            String.concat "" ["size_t* "; startsName array;  " = new size_t["; string array.Rank; "];"]
            String.concat "" ["size_t* "; countsName array;  " = new size_t["; string array.Rank; "];"]
            String.concat "" ["for(int q = 0; q < "; string array.Rank; "; q++){"]
            String.concat "" ["\t"; "nc_inq_dimid(";  inputFileIDname array; ", "; variableIDname array;    ", &("; array.Name; "_dim_ncids[q]));"]
            String.concat "" ["\t"; "nc_inq_dimlen("; inputFileIDname array; ", "; array.Name; "_dim_ncids[q], &("; extentsName array; "[q]));"]
            String.concat "" ["\t"; startsName array; "[q] = 0;"]
            String.concat "" ["\t"; countsName array; "[q] = 1;"]
            String.concat "" ["}"]
            String.concat "" [countsName array; "["; string (array.Rank-1); "] = "; extentsName array; "["; string (array.Rank-1); "];"]
        ] @ ncDimValDeclLines @ ncDimValLines)

        (textGenerator :> pushText<_>).PushInnerNested (fun loop i ->
            String.concat "" [startsName array; "["; string i; "] = "; loop.indNames.[i]; ";\n"]
        )

        let inner = String.concat "" [
            "nc_get_vara_"; array.Type; "("; inputFileIDname array; ", "; variableIDname array; ", "; startsName array; ", "; countsName array; ", "; lastIName; ");\n"
        ]

        let loop =
            {
                iarrayName = array.Name;
                iarrayType = array.Type;
                iarrayLevels = array.Rank-1;
                iRank = 1;
                iExtents = extentsName array;
                indNames = indexNames;
                iMins = imins;
                parLevels = 0;
            }

        let ret =
            newln ncInit
            @ (unaryLoop loop textGenerator 0
               |> List.fold (|>) [inner])
            @ [String.concat "" ["nc_close("; inputFileIDname array; ");\n"]]
        ret

    let ncPut (textGenerator: LoopTextGenerator) (iarrays: NestedArray list) (func: NestedFunction) (oarray: NestedArray) =

        // This check is needed because writing NetCDF variables requires detailed information about dimensions
        // that isn't strictly needed for basic nested iterators. In theory, it's possible to mix NetCDF
        // and non-NetCDF arrays, the user just has to do the writes on their own.
        iarrays |> List.map (fun iarray ->
            match iarray.Info with
            | NetCDF info -> None
            | _ -> failwith "All input arrays must be NetCDF arrays to use ncPut"
        ) |> ignore

        let ncFileName, ncVarName =
            match oarray.Info with
            | NetCDF info -> info.FileName, info.VariableName
            | _ -> failwith "Cannot use NetCDF routines on non-NetCDF output arrays"

        let indexNames = List.init (oarray.Rank-1) (fun i -> textGenerator.IteratorName 0 i)

        let oSymms = oarray.Symm |> function | Some s -> s | None -> (List.init oarray.Rank id)
        let states = vStates [oarray.Name] [oSymms] [0]
        /// Chooses the correct iterator minimum for all input variables.
        let imins =
            let iMaps = iminMap states
            List.init oarray.Rank (fun j ->
                if iMaps.Head.[j] = (0, j) then textGenerator.Zero else indexNames.[snd iMaps.Head.[j]]
            )

        /// Last input array intermediate names to be subbed into the inner block
        let lastOName = String.concat "" [oarray.Name; indexNames |> List.rev |> List.head]

        /// Dimension algebra for names and values
        let iDims =
            match func.Arity with
            | Some arity -> List.init arity (fun i -> func.IRank.[i] - iarrays.[i].Rank)
            | None -> List.init iarrays.Length (fun i -> iarrays.Head.Rank)

        let oDims = (iDims |> List.sum) + func.ORank

        /// nc_open call
        let ncInit = [
            String.concat "" ["int "; outputFileIDname oarray; ";"]
            String.concat "" ["nc_create("; quote ncFileName; ", NC_CLOBBER, &"; outputFileIDname oarray; ");"]


            String.concat "" ["int "; variableIDname oarray; ";";]
            String.concat "" ["nc_inq_varid("; outputFileIDname oarray; ", "; quote ncVarName; ", &"; variableIDname oarray; ");"]
            String.concat "" ["int* "; oarray.Name; "_dim_ncids = new int[";  string oarray.Rank; "];"]
            String.concat "" ["size_t* "; extentsName oarray; " = new size_t["; string oarray.Rank; "];"]
            String.concat "" ["size_t* "; startsName oarray;  " = new size_t["; string oarray.Rank; "];"]
            String.concat "" ["size_t* "; countsName oarray;  " = new size_t["; string oarray.Rank; "];"]
            String.concat "" ["for(int q = 0; q < "; string oarray.Rank; "; q++){"]
            String.concat "" ["\t"; "nc_inq_dimid(";  outputFileIDname oarray; ", "; variableIDname oarray; ", &("; oarray.Name; "_dim_ncids[q]));"]
            String.concat "" ["\t"; "nc_inq_dimlen("; outputFileIDname oarray; ", "; oarray.Name; "_dim_ncids[q], &("; extentsName oarray; "[q]));"]
            String.concat "" ["\t"; startsName oarray; "[q] = 0;"]
            String.concat "" ["\t"; countsName oarray; "[q] = 1;"]
            String.concat "" ["}"]
            String.concat "" [countsName oarray; "["; string (oarray.Rank-1); "] = "; extentsName oarray; "["; string (oarray.Rank-1); "];"]
        ]

        []



(********************************************************************************)


open System.IO
open System.Text.RegularExpressions

//#load "parser.fs"

/// Union of all the relevant token types
type Token =
    | NewLine
    | WhiteSpace
    | Symbol of char
    | Str of string
    | Int of int
    | Quote of string
    | Other of string

/// Tokenizer helper pattern for regexes
let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

/// Convert string to a token based on a regex pattern
let toToken = function
    | Match @"^\n|^\r"                 s -> s, Token.NewLine
    | Match @"^\s+"                    s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>|^;|^:|^\^|^\*|^=" s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s -> s, Token.Str s
    | Match @"^\d+"                    s -> s, Token.Int (int s)
    | Match  "^\"[^\"]+\""             s -> s, Token.Quote (unquote s)
    | Match  "^\'[^\']+\""             s -> s, Token.Quote (unquote s)
    | Match @"."                       s -> s, Token.Other (string s)
    | _ -> failwith "Invalid Token"

/// Convert string into a list of parsable tokens
let tokenize (s: string) =
    // Convert substrings to tokens, starting at position 'index' in the string.
    let rec tokenize' index (s: string) =
        if index = s.Length then []
        else
            let text, token = toToken (s.Substring index)
            token :: tokenize' (index + text.Length) s
    tokenize' 0 s
    // strip out the whitespace tokens; note that since new line characters are distinct, they will remain.
    |> List.choose (function Token.WhiteSpace -> None | t -> Some t)

/// Megahack to convert tokens to ints. try to fix for proper Token.Int -> int conversion
let tokenToInt (tokens: Token list) =
    tokens |> List.map (string >> (fun x -> x.Substring 4) >> int)

/// Megahack to convert tokens to strings. try to fix for proper Token.Str -> string conversion
let tokenToStr (tokens: Token list) =
    tokens |> List.map (
        function
        | Token.NewLine -> "\n"
        | Token.WhiteSpace -> " "
        | Token.Symbol s -> string s
        | Token.Str    s -> string s
        | Token.Int    i -> string i
        | Token.Quote  q -> quote (string q)
        | Token.Other  o -> string o
    )

let rec respace = function
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s1 :: Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s2 :: tail -> s1 :: " " :: (respace (s2 :: tail))
    | Match @"^return" s1 :: Match @"^[a-zA-Z0-9_][a-zA-Z0-9_]*" s2 :: tail -> s1 :: " " :: (respace (s2 :: tail))
    | Match @"^\#" s1 :: Match @"^include" s2 :: tail -> s1 :: s2 :: " " :: respace tail
    | Match @"^\#" s1 :: Match @"^define" s2 :: s3 :: s4 :: tail -> s1 :: s2 :: " " :: s3 :: " " :: s4 :: respace tail
    | head :: tail -> head :: respace tail
    | [] -> []

let rec (|Reconcat|_|) = function
    | Match @"^\n|^\r" head :: tail -> match tail with Reconcat t -> Some t | _ -> None
    | head :: Match @"^\n|^\r" mid :: tail -> Some (head, tail)
    | head :: tail ->
        let mid, tail' = match tail with Reconcat t -> t | _ -> "", []
        Some (String.concat "" (head :: [mid]), tail')
    | [] -> None

let rec reconcat = function
    | Reconcat (head, tail) -> head :: (reconcat tail)
    | head :: tail -> [String.concat "" (head :: tail)]
    | [] -> []

let stringCollapse (sep: string) =
    List.reduce (fun acc elem -> String.concat sep [acc; elem])

/// Pragma clause type; a tuple of the clause name and a list of arguments
type Clause = string * Token list

/// Pragma type; consists of a directive, a list of clauses, and a scope
type Pragma = Clause * Clause list * Token list

/// Delete the "return" line of a function (needed for function expansion)
let rec deleteReturnLine = function
    | Token.Str "return" :: tail ->
        let rec aux = function
            | Token.Symbol ';' :: tail' -> tail'
            | head' :: tail'-> aux tail'
            | _ -> failwith "derp"
        aux tail
    | head :: tail -> head :: deleteReturnLine tail
    | _ -> []

/// Pattern for pragma scopes; basically dumps all the tokens inside the pragma scope into a buffer, and returns the tail separately
let rec (|ScopePattern|_|) = function
    | Token.Symbol '{' :: tail ->
        /// Make a list of tokens inside this scope by counting the number of braces (terrible, I know)
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol '}' :: Token.NewLine :: tail' ->
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: Token.NewLine :: h', t'
                else
                    [], tail'
            | Token.Symbol '}' :: tail' ->
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: h', t'
                else
                    [], tail'
            | Token.Symbol '{' :: tail' ->
                let h', t' = toScope tail' (ctr+1)
                Token.Symbol '{' :: h', t'
            | head' :: tail' ->
                let h', t' = toScope tail' ctr
                head' :: h', t'
        let scope, t = toScope tail 0
        Some (scope, (t: Token list))
    | Token.Symbol ';' :: tail -> None
    | head :: tail ->
        match tail with
        | ScopePattern t -> Some t
        | _ -> None
    | _ -> None

let rec (|ScopePatternParens|_|) = function
    | head :: Token.Symbol '(' :: tail ->
        /// Make a list of tokens inside this scope by counting the number of braces (terrible, I know)
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol ')' :: Token.NewLine :: tail' ->
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol ')' :: Token.NewLine :: h', t'
                else
                    [], tail'
            | Token.Symbol ')' :: tail' ->
                if ctr > 0 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol ')' :: h', t'
                else
                    [], tail'
            | Token.Symbol '(' :: tail' ->
                let h', t' = toScope tail' (ctr+1)
                Token.Symbol '(' :: h', t'
            | head' :: tail' ->
                let h', t' = toScope tail' ctr
                head' :: h', t'
        let scope, t = toScope tail 0
        Some (head :: Token.Symbol '(' :: (scope @ [Token.Symbol ')']), (t: Token list))
    | Token.Symbol ';' :: tail -> None
    | head :: tail ->
        match tail with
        | ScopePatternParens t -> Some t
        | _ -> None
    | _ -> None

// Pattern for all code after a certain code block where the code block is in scope
let rec (|PostScopePattern|_|) = function
    | head :: tail ->
        /// Make a list of tokens inside this scope by counting the number of braces (terrible, I know)
        let rec toScope t (ctr: int) =
            match t with
            | [] -> [], []
            | Token.Symbol '}' :: Token.NewLine :: tail' ->
                if ctr > -1 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: Token.NewLine :: h', t'
                else
                    [], tail'
            | Token.Symbol '}' :: tail' ->
                if ctr > -1 then
                    let h', t' = toScope tail' (ctr-1)
                    Token.Symbol '}' :: h', t'
                else
                    [], tail'
            | Token.Symbol '{' :: tail' ->
                let h', t' = toScope tail' (ctr+1)
                Token.Symbol '{' :: h', t'
            | head' :: tail' ->
                let h', t' = toScope tail' ctr
                head' :: h', t'
        let scope, t = toScope tail 0
        Some (head :: scope, (t: Token list))
    | _ -> None

//match tokenize "asd(8) {fjfj jfjf }\n" with | ScopePattern(s) -> Some(s) |_->None;;
//let a = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopePattern v -> Some(v) |_-> None ;;
//let b = match (tokenize "asd(8) {fjfj jfjf { 9 {a b c} 7 8 } }\n asdf {9 8}\n") with | ScopesPattern v -> Some(v) |_-> None ;;

/// Pattern for pragmas applied to individual lines; basically dumps all the tokens inside the pragma scope into a buffer, and returns the tail separately
let rec (|LinePattern|_|) = function
    | head :: Token.Symbol ';' :: Token.NewLine :: tail | head :: Token.Symbol ';' :: tail ->
        Some (head :: [Token.Symbol ';'], tail)
    | head :: tail ->
        let h, t =
            match tail with
            | LinePattern t -> t
            | _ -> [], []
        Some (head :: h, t)
    | _ -> None

//match tokenize "asd(8);\n fjfjfj" with | LinePattern(s) -> Some(s) |_->None;;
//match tokenize "asd(8);\n asdf;\n fjfjfjfj 9;\n" with | LinesPattern(s) -> Some(s) |_->None;;

/// Make a list of arguments from a comma-separated list and return the tail
let rec toElements s =
    match s with
    | head :: Token.Symbol ',' :: tail ->
        let elements, t = toElements tail
        (head :: elements), t
    | head :: Token.Symbol ')' :: tail -> [head], tail
    | _ -> [], []


/// Container for method_for loop information; able to recieve messages about array and function pragmas
type MethodLoop (nameIn: string, initIn: string list, callIn: (string * string * int) list, fposition: int, lposition: int)  =
    [<DefaultValue>] val mutable public iarrays: NestedArray list
    [<DefaultValue>] val mutable public oarrays: NestedArray list
    [<DefaultValue>] val mutable public funcs: NestedFunction list

    member this.Name = nameIn
    member this.Init = initIn // iarrays and their extents
    member this.Call = callIn // func, oarray, its extents, and call position
    member this.FPosition = fposition // position of the first token of the init pattern
    member this.LPosition = lposition // position of the last token of the init pattern

    member this.PushIarray (v: NestedArray) = this.iarrays <- this.iarrays @ [v]
    member this.PushOarray (v: NestedArray) = this.oarrays <- this.oarrays @ [v]
    member this.PushFunc (v: NestedFunction) = this.funcs <- this.funcs @ [v]

/// Container for object_for loop information; able to recieve messages about array and function pragmas
type ObjectLoop (nameIn: string, initIn: string, callIn: (string list * string * int) list, fposition: int, lposition: int) =
    [<DefaultValue>] val mutable public iarrays: NestedArray list list
    [<DefaultValue>] val mutable public oarrays: NestedArray list
    [<DefaultValue>] val mutable public func: NestedFunction list

    member this.Name = nameIn
    member this.Init = initIn // func
    member this.Call = callIn // iarrays and their extents, oarray and its extents, and call position
    member this.FPosition = fposition // position of the first token of the init pattern
    member this.LPosition = lposition // position of the last token of the init pattern

    member this.PushIarrays (v: NestedArray list) = this.iarrays <- this.iarrays @ [v]
    member this.PushOarray (v: NestedArray) = this.oarrays <- this.oarrays @ [v]
    member this.SetFunc (v: NestedFunction) = this.func <- [v]
    member this.GetFunc = this.func.Head

/// Pattern for method_for loops and all their calls
let rec (|MethodLoopPattern|_|) (position: int) = function
    | loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
        let args, tokens = toElements tail
        let iarrays = args |> tokenToStr
        let rec findCalls tokens' position' =
            match tokens' with
            | lname :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ',' :: Token.Str oarray :: Token.Symbol ')' :: Token.Symbol ';' :: tail' when lname = loop ->
                (func, oarray, position'-1) :: findCalls tail' (position' + 7)
            | head' :: tail' -> findCalls tail' (position' + 1)
            | [] -> []
        let lposition = position + 4 + (args.Length)
        let calls =
            match tokens with
            | PostScopePattern tokens' -> findCalls (fst tokens') lposition
            | _ -> []
        let out = MethodLoop((tokenToStr [loop]).Head, iarrays, calls, position-1, lposition)
        out.iarrays <- []
        out.oarrays <- []
        out.funcs <- []
        Some (out, tokens)
    | _ -> None

/// Scan code for all the method_for loops and return a list of them.
let scanMethodLoops (tokens: Token list) =
    let rec scan (position: int) = function
    | MethodLoopPattern position (loop,[]) -> [loop]
    | MethodLoopPattern position (loop, tail) -> loop :: (scan (loop.LPosition+1) tail)
    | head :: tail -> scan (position+1) tail
    | [] -> []
    scan 0 tokens

//let a = match tokenize code with | MethodLoopPattern 0 (s) -> Some(s) | _ -> None;;
//let mystr = "auto mloop = method_for(array1, array1, array3); auto ooarray = oloop(array1, array1, array3); auto moarray = mloop(sumThenMultiply);"
//let b = match tokenize mystr with | MethodLoopPattern 0 (s) -> Some(s) | _ -> None;;

/// Pattern for object_for loops and all their calls
let rec (|ObjectLoopPattern|_|) (position: int) = function
    | loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: Token.Symbol ';' :: tail ->
        let rec findCalls tokens' position' =
            match tokens' with
            | lname :: Token.Symbol '(' :: tail' when lname = loop ->
                let args, t = toElements tail'
                let arrs = args |> tokenToStr
                let iarrays, oarray = arrs |> List.rev |> fun x -> (x.Tail |> List.rev, x.Head)
                (iarrays, oarray, position') :: findCalls t (position' + 2 + (args.Length))
            | head' :: tail' -> findCalls tail' (position' + 1)
            | [] -> []
        let lposition = position + 6
        let calls =
            match tail with
            | PostScopePattern t' -> findCalls (fst t') lposition
            | _ -> []
        let out = ObjectLoop((tokenToStr [loop]).Head, string func, calls, position-1, lposition)
        out.iarrays <- []
        out.oarrays <- []
        out.func <- []
        Some (out, tail)
    | _ -> None

/// Scan code for all the method_for loops and return a list of them.
let scanObjectLoops (tokens: Token list) =
    let rec scan (position: int) = function
    | ObjectLoopPattern position (loop,[]) -> [loop]
    | ObjectLoopPattern position (loop, tail) -> loop :: (scan (loop.LPosition+1) tail)
    | head :: tail -> scan (position+1) tail
    | [] -> []
    scan 0 tokens

//let a = match tokenize code with | ObjectLoopPattern 0 (s) -> Some(s) | _ -> None;;
//let mystr = "auto oloop = object_for(sumThenMultiply); auto mloop = method_for(array1, array1, array3); auto ooarray = oloop(array1, array1, array3);"
//let b = match tokenize mystr with | ObjectLoopPattern 0 (s) -> Some(s) | _ -> None;;

/// Pattern for individual pragma clauses
let rec (|ClausePattern|_|) = function
    | Token.NewLine :: tail -> None
    | Token.Str head :: Token.Symbol '(' :: tail ->
        let elements, t = toElements tail
        if elements.Length = 1 then
            Some (Clause (head, elements), t)
        else
            Some (Clause (head, elements), t)
    | Token.Str head :: tail -> Some (Clause (head, []), tail)
    | _  -> None
/// Pattern for all pragma clauses
let (|ClausesPattern|_|) = function
    | ClausePattern (head, tail) ->
        let rec aux head' = function
            | ClausePattern (head, tail) -> aux (head :: head') tail
            | tail -> List.rev head', tail
        Some (aux [head] tail)
    | _ -> None
/// Pattern for pragmas
let (|PragmaPattern|_|) = function
    | Token.Symbol '#' :: Token.Str "pragma" :: Token.Str "edgi" :: ClausesPattern (cl, Token.NewLine :: tail) ->
        match tail with
        | ScopePattern (h, t) -> h, t
        | LinePattern (h, t) ->  h, t
        | _ -> failwith "edgi pragma must occur before a statement or a block."
        ||> fun h -> fun t ->
            Some (Pragma (cl.Head, cl.Tail, h), t)
    | _ -> None

/// Scan code for all the pragmas and return a list of them.
let scanPragmas (tokens: Token list) =
    let rec scan = function
    | PragmaPattern (pragma,[]) -> [pragma]
    | PragmaPattern (pragma, tail) -> pragma :: (scan tail)
    | head :: tail -> scan tail
    | [] -> []
    scan tokens


let sortPragmas (pragmas: Pragma list) =
    let bin (s: string) =
        List.filter (fun x ->
                        let directive, clauses, scope = x
                        fst directive = s
                    ) pragmas
    bin "array", bin "function"

let (|ArrayPragmaPattern|_|) = function
    | PragmaPattern (pragma, tail) ->
        if ([pragma] |> sortPragmas |> fst).IsEmpty then None else Some (pragma, tail)
    | _ -> None

let (|FunctionPragmaPattern|_|) = function
    | PragmaPattern (pragma, tail) ->
        if ([pragma] |> sortPragmas |> fst).IsEmpty then Some (pragma, tail) else None
    | _ -> None

let (|ArrayPattern|_|) (symGroups: int list Option) = function
    | Token.Str "edgi_nc_t" :: Token.Str name :: Token.Symbol '(' :: Token.Quote fileName :: Token.Symbol ',' :: Token.Quote varName :: Token.Symbol ')' :: Token.Symbol ';' :: tail ->
        Some ( {Name = name; Type = getNCvarType fileName varName; Rank = getNCnumDims fileName varName; Symm = symGroups; Info = NetCDF {FileName = fileName; VariableName = varName} }, tail)
    | Token.Str valtype :: Token.Symbol '^' :: Token.Int rank :: Token.Str name :: Token.Symbol '(' :: Token.Str extentsName :: Token.Symbol ')' :: Token.Symbol ';' :: tail ->
        Some ( {Name = name; Type = valtype; Rank = rank; Symm = symGroups; Info = Array {ExtentsName = extentsName} }, tail )
    | _ -> None


let hasClause name clauses =
    clauses |> List.exists (fst >> (fun x -> x = name))

let getClause name clauses =
    clauses |> List.find (fst >> (fun x -> x = name))


let getArray (clauses: Clause list) (block: Token list) =
    let hasSym = clauses |> hasClause "symmetry"
    let sym = if hasSym then
                  Some ((clauses |> getClause "symmetry") |> (snd >> tokenToInt))
              else None
    match block with
    | ArrayPattern sym s -> fst s
    | _ -> failwith "Array pragma applied to invalid array declaration."

let getFunction (name: string) (clauses: Clause list) (block: Token list) =
    let arity =
        clauses |> getClause "arity" |> snd
        |> fun x ->
            match x with
            | [Token.Str "any"] -> None
            | _ -> Some (x |> (tokenToInt >> List.head))

    let input  = (clauses |> getClause "input") |> (snd >> tokenToStr)
    let output = (clauses |> getClause "output") |> (snd >> tokenToStr >> List.head)
    let iranks = (clauses |> getClause "iranks") |> (snd >> tokenToInt)
    let orank  = (clauses |> getClause "orank") |> (snd >> tokenToInt >> List.head)

    let ompLevels =
        if clauses |> hasClause "ompLevels" then
            (clauses |> getClause "ompLevels") |> (snd >> tokenToInt)
        else
            List.init iranks.Length (fun i -> 0)

    let com =
        if clauses |> hasClause "commutativity" then
            Some ((clauses |> getClause "commutativity") |> (snd >> tokenToInt))
        else None

    let ncDimNames =
        if clauses |> hasClause "ncDimNames" then
            Some ((clauses |> getClause "ncDimNames") |> (snd >> tokenToStr))
        else None

    let ncDimLens =
        if clauses |> hasClause "ncDimLens" then
            Some ((clauses |> getClause "ncDimLens") |> (snd >> tokenToInt))
        else None

    let ncDimVals =
        if clauses |> hasClause "ncDimVals" then
            Some ((clauses |> getClause "ncDimVals") |> (snd >> tokenToStr))
        else None


    let ncInfo =
        match ncDimNames, ncDimLens, ncDimVals with
        | Some names, Some lens, Some vals -> Some {DimNames = names; DimExtents = lens; DimValNames = vals}
        | None, None, None -> None
        | _ -> failwith "Incomplete specification of NetCDF output in function \"%s\"" name

    { Name = name;
      Arity = arity;
      INames = input;
      IRank = iranks;
      OName = output;
      ORank = orank;
      Comm = com;
      ParallelismLevels = ompLevels;
      Inner = block |> deleteReturnLine |> tokenToStr |> respace |> reconcat |> newln;
      NCInfo = ncInfo }


let fst3 (c, _, _) = c
let snd3 (_, c, _) = c
let thd3 (_, _, c) = c

/// Message-passing function for sending info about array pragmas to nested loop objects
let sendArraysToLoops (arrays: NestedArray list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
    for i in 0..mloops.Length-1 do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..arrays.Length-1 do
            for k in 0..mloops.[i].Init.Length-1 do
                if mloops.[i].Init.[k] = arrays.[j].Name then
                    mloops.[i].PushIarray arrays.[j]
            for k in 0..mloops.[i].Call.Length-1 do
                if mloops.[i].Call.[k] |> snd3 = arrays.[j].Name then
                    mloops.[i].PushOarray arrays.[j]
    for i in 0..oloops.Length-1 do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..oloops.[i].Call.Length-1 do
            let mutable itemp = []
            for l in 0..(oloops.[i].Call.[j] |> fst3).Length-1 do
                for k in 0..arrays.Length-1 do
                    if (oloops.[i].Call.[j] |> fst3).[l] = arrays.[k].Name then
                        itemp <- itemp @ [arrays.[k]]
            oloops.[i].PushIarrays itemp
            for k in 0..arrays.Length-1 do
                if (oloops.[i].Call.[j] |> snd3) = arrays.[k].Name then
                    oloops.[i].PushOarray arrays.[k]

/// Message-passing function for sending info about function pragmas to nested loop objects
let sendFunctionsToLoops (funcs: NestedFunction list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
    // search for function names in funcs list and copy info to loop objects (results must be in order!)
    for i in 0..mloops.Length-1 do
        for j in 0..funcs.Length-1 do
            for k in 0..mloops.[i].Call.Length-1 do
                if fst3 mloops.[i].Call.[k] = funcs.[j].Name then
                    mloops.[i].PushFunc funcs.[j]
    for i in 0..oloops.Length-1 do
        for j in 0..funcs.Length-1 do
            if oloops.[i].Init = funcs.[j].Name then
                oloops.[i].SetFunc funcs.[j]

/// Find a loop API call and return the tail
let rec (|Init|_|) = function
    | loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail -> Some (tail |> toElements |> snd)
    | loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail -> Some (tail)
    | head :: tail ->
        match tail with
        | Init(s) -> Some (s)
        | _ -> None
    | _ -> None

/// Inserts a comma at the end of each string in the input list, except the last one
let withCommas (x: string list) =
    x
    |> List.rev
    |> List.tail
    |> List.rev
    |> List.map (fun y -> String.concat "" [y; ", "])
    |> fun y -> y @ [List.last x]

let symmVecLines (arrays: NestedArray list) =
    arrays
    |> List.filter (fun x -> x.Symm.IsSome)
    |> List.map (fun x -> x.Name, x.Symm |> Option.get)
    |> List.map (fun x ->
        let name, symm = x
        String.concat "" ["static constexpr const int "; name; "_symm["; symm.Length |> string; "] = {"; symm |> List.map string |> withCommas |> stringCollapse " "; "};\n"]
    )

let symmVecName (array: NestedArray) =
    if array.Symm.IsNone then "nullptr" else String.concat "" [array.Name; "_symm"]

let objectLoopTemplate (oloop: ObjectLoop) =

    let numCalls = oloop.Call.Length
    let itype = oloop.iarrays |> List.map (List.map (fun y -> y.Type))
    let otype = oloop.oarrays |> List.map (fun x -> x.Type)
    let irank = oloop.iarrays |> List.map (List.map (fun y -> y.Rank))
    let orank = oloop.oarrays |> List.map (fun x -> x.Rank)
    let isymm = oloop.iarrays |> List.map (List.map symmVecName)
    let osymm = oloop.oarrays |> List.map symmVecName

    let arity, inames =
        match oloop.GetFunc.Arity with
        | Some a -> // fixed arity => specify all argument names
            List.init numCalls (fun i -> a), List.init numCalls (fun i -> oloop.GetFunc.INames)
        | None ->  // variable arity => specify one argument name and template it
            let arity' = oloop.iarrays |> List.map (fun x -> x.Length)
            arity', List.init numCalls (fun i -> List.init arity'.[i] (fun j -> String.concat "" [oloop.GetFunc.INames.Head; "_"; string j]))

    let onames =  List.init numCalls (fun i -> oloop.GetFunc.OName)

    let ranks = (irank, orank) ||> List.map2 (fun x y -> (x |> List.map string) @ [string y])
    let types = (itype, otype) ||> List.map2 (fun x y -> x @ [y])
    let symms = (isymm, osymm) ||> List.map2 (fun x y -> x @ [y])
    let names = (inames, onames) ||> List.map2 (fun x y -> x @ [y])

    let templateTypes =
        arity |> List.map (fun a ->
            List.init a (fun i -> String.concat "" ["typename ITYPE"; string (i+1); ", const int IRANK"; string (i+1); ", const int* ISYM"; string (i+1)])
            |> fun x -> x @ ["typename OTYPE, const int ORANK, const int* OSYM"]
            |> withCommas
            |> stringCollapse ""
        )

    let templateArgTypes =
        (arity, inames) ||> List.map2 (fun a (names: string list) ->
            (List.init a (fun i -> String.concat "" ["\n\ttypename promote<ITYPE"; string (i+1); ", IRANK"; string (i+1); ">::type "; names.[i]])
            |> fun x -> x @ [String.concat "" ["\n\ttypename promote<OTYPE, ORANK>::type "; oloop.GetFunc.OName]])
            @ (List.init a (fun i -> String.concat "" ["\n\tconst int "; names.[i]; "_extents[IRANK"; string (i+1); "]"])
            |> fun x -> x @ [String.concat "" ["\n\tconst int "; oloop.GetFunc.OName; "_extents[ORANK]"]])
            |> withCommas
            |> stringCollapse ""
        )

    let aritySuffix = List.init numCalls (fun i -> if oloop.GetFunc.Arity.IsNone then String.concat "" ["_arity"; string arity.[i]] else "")
    let tmain = List.init numCalls (fun i -> String.concat "" ["template<"; templateTypes.[i]; "> void "; oloop.GetFunc.Name; aritySuffix.[i]; "("; templateArgTypes.[i]; "){\n\t// Nothing to see here.\n}"])


    let tSpecTypes =
        List.init numCalls (fun i ->
            List.init (arity.[i]+1) (fun j ->
                [types.[i].[j]; ranks.[i].[j]; symms.[i].[j]]
                |> (withCommas >> stringCollapse "")
            ) |> (withCommas >> stringCollapse "")
        )

    let tSpecArgs =
        List.init numCalls (fun i ->
            (List.init (arity.[i]+1) (fun j ->
                String.concat "" ["\n\ttypename promote<"; types.[i].[j]; ", "; ranks.[i].[j]; ">::type "; names.[i].[j]]
            ))
            @ (List.init (arity.[i]+1) (fun j ->
                String.concat "" ["\n\tconst int "; names.[i].[j]; "_extents["; ranks.[i].[j]; "]"]
            ))
            |> (withCommas >> stringCollapse "")
        )

    let rec (|HeadPattern|_|) (iname: string) = function
        | head :: Token.Symbol '=' :: tail ->
            let rec pre = function
                | h' :: Token.Str iname :: t' -> [h'], t'
                | h' :: t' ->
                    let a, b = pre t'
                    h' :: a, b
                | _ -> failwith "Input array name not found in variadic function."
            Some (head :: [Token.Symbol '='], pre tail |> fst, pre tail |> snd)
        | head :: tail ->
            match tail with
            | HeadPattern iname s -> Some (head :: (fst3 s), snd3 s, thd3 s)
            | _ -> None
        | _ -> None

    let rec (|TailPattern|_|) = function
        | head :: Token.Str "tail" :: tail -> Some ([head], tail)
        | head :: tail ->
            match tail with
            | TailPattern s ->
                let h, t = s
                Some (head :: h, t)
            | _ -> None
        | _ -> None

    /// Expand the function block for a single variadic function call.
    /// <param name="tokens"> Token stream from a single function block. </param>
    /// <param name="n"> List of variadic input names. </param>
    let expandVariadic (n: int) (tokens: Token list) =
        match tokens with
        | TailPattern (h, t) ->
            match h with
            | HeadPattern oloop.GetFunc.INames.Head (p', h', t') ->
                List.init arity.[n] (fun i -> (h' |> tokenToStr) @ (inames.[n].[i] :: (t' |> tokenToStr)))
                |> List.rev
                |> fun x -> ((x.Head |> String.concat "" |> tokenize |> function | ScopePatternParens s -> Some (s |> fst |> tokenToStr) | _ -> None) |> Option.get) :: x.Tail
                |> List.rev
                |> List.map (stringCollapse " ")
                |> stringCollapse ""
                |> fun x -> stringCollapse "" ((x :: (t |> tokenToStr)) |> respace |> reconcat |> newln)
                |> fun x -> String.concat "" [p' |> tokenToStr |> stringCollapse ""; x]
            | _ -> h |> (tokenToStr >> respace >> reconcat >> newln >> List.head)
        | _ -> tokens |> (tokenToStr >> respace >> reconcat >> newln >> List.head)

    let tSpecInner =
        let nOmp = oloop.GetFunc.ParallelismLevels |> List.sum
        match oloop.GetFunc.Arity with
        | Some a -> // fixed arity => specify all argument names
            List.init numCalls (fun i ->
                let textGenerator = CppLoopTextGenerator([], [cppOmpLine],[cppArrayDeclLine],[])
                String.concat "" ["omp_set_nested("; string nOmp;");\n"] ::
                (NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] oloop.GetFunc textGenerator |> fst)
            )
        | None ->
            let funcs = List.init numCalls (fun i ->
                {
                    Name = String.concat "" [oloop.GetFunc.Name; aritySuffix.[i]];
                    Arity = Some(arity.[i]);
                    INames = inames.[i];
                    IRank = List.init arity.[i] (fun j -> oloop.GetFunc.IRank.Head);
                    OName = oloop.GetFunc.OName;
                    ORank = oloop.GetFunc.ORank;
                    Comm = oloop.GetFunc.Comm;
                    ParallelismLevels = oloop.GetFunc.ParallelismLevels;
                    Inner = oloop.GetFunc.Inner |> List.map (tokenize >> deleteReturnLine >> (expandVariadic i))
                    NCInfo = oloop.GetFunc.NCInfo
                }
            )
            List.init numCalls (fun i ->
                let textGenerator = CppLoopTextGenerator([], [cppOmpLine],[cppArrayDeclLine],[])
                String.concat "" ["omp_set_nested("; string nOmp;");\n"] ::
                (NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] funcs.[i] textGenerator |> fst)
            )

    List.init numCalls (fun i ->
        (tSpecTypes.[i], tSpecArgs.[i], String.concat "" [oloop.GetFunc.Name; aritySuffix.[i]])
        |||> fun x y z -> ["template<> void "; z; "<"; x; ">("; y; ")"]
        |> stringCollapse ""
    )
    |> List.map brace
    |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> [y.Head] @ tab z @ y.Tail))
    |> List.map (fun x -> x |> stringCollapse "")
    |> fun x -> tmain @ x
    |> List.distinct


let methodLoopTemplate (mloop: MethodLoop) =
    let arity =
        mloop.funcs
        |> List.map (fun x -> x.Arity)
        |> List.map (function | Some a -> a | None -> failwith "method_for loops must have fixed arity.")
        |> fun x ->
            let a = List.distinct x
            if a.Length = 1 then a.Head else failwith "method_for loops must have fixed arity."

    let firank = mloop.funcs |> List.map (fun x -> x.IRank)
    let forank = mloop.funcs |> List.map (fun x -> x.ORank)

    let numCalls = mloop.Call.Length
    let itype = mloop.iarrays |> List.map (fun x -> x.Type)
    let otype = mloop.oarrays |> List.map (fun x -> x.Type)
    let irank = mloop.iarrays |> List.map (fun x -> x.Rank)
    let orank = mloop.oarrays |> List.map (fun x -> x.Rank)
    let isymm = mloop.iarrays |> List.map symmVecName
    let osymm = mloop.oarrays |> List.map symmVecName
    let inames = mloop.funcs  |> List.map (fun x -> x.INames)
    let onames = mloop.funcs  |> List.map (fun x -> x.OName)

    let ranks = orank |> List.map (fun y -> (irank |> List.map string) @ [string y])
    let types = otype |> List.map (fun y -> itype @ [y])
    let symms = osymm |> List.map (fun y -> isymm @ [y])
    let names = (inames, onames) ||> List.map2 (fun x y -> x @ [y])

    let templateTypes =
        List.init arity (fun i -> String.concat "" ["typename ITYPE"; string (i+1); ", const int IRANK"; string (i+1); ", const int* ISYM"; string (i+1)])
        |> fun x -> x @ ["typename OTYPE, const int ORANK, const int* OSYM"]
        |> withCommas
        |> stringCollapse ""

    let templateArgTypes =
        List.init numCalls (fun i ->
            (List.init arity (fun j -> String.concat "" ["\n\ttypename promote<ITYPE"; string (j+1); ", IRANK"; string (j+1); ">::type "; inames.[i].[j]])
            |> fun x -> x @ [String.concat "" ["\n\ttypename promote<OTYPE, ORANK>::type "; onames.[i]]])
            @ (List.init arity (fun j -> String.concat "" ["\n\tconst int "; inames.[i].[j]; "_extents[IRANK"; string (j+1); "]"])
            |> fun x -> x @ [String.concat "" ["\n\tconst int "; onames.[i]; "_extents[ORANK]"]])
            |> withCommas
            |> stringCollapse ""
        )

    let aritySuffix = if mloop.funcs.[0].Arity.IsNone then String.concat "" ["_arity"; string arity] else ""
    let tmain = List.init numCalls (fun i -> String.concat "" ["template<"; templateTypes; "> void "; mloop.funcs.[i].Name; aritySuffix; "("; templateArgTypes.[i]; "){\n\t// Nothing to see here.\n}"])

    let tSpecTypes =
        List.init numCalls (fun i ->
            List.init (arity+1) (fun j ->
                [types.[i].[j]; ranks.[i].[j]; symms.[i].[j]]
                |> (withCommas >> stringCollapse "")
            ) |> (withCommas >> stringCollapse "")
        )

    let tSpecArgs =
        List.init numCalls (fun i ->
            (List.init (arity+1) (fun j ->
                String.concat "" ["\n\ttypename promote<"; types.[i].[j]; ", "; ranks.[i].[j]; ">::type "; names.[i].[j]]
            ))
            @ (List.init (arity+1) (fun j ->
                String.concat "" ["\n\tconst int "; names.[i].[j]; "_extents["; ranks.[i].[j]; "]"]
            ))
            |> (withCommas >> stringCollapse "")
        )

    let tSpecInner =
        List.init numCalls (fun i ->
            let nOmp = mloop.funcs.[i].ParallelismLevels |> List.sum
            let textGenerator = CppLoopTextGenerator([], [cppOmpLine],[cppArrayDeclLine],[])
            String.concat "" ["omp_set_nested("; string nOmp;");\n"] ::
            (NestedLoop.Nary mloop.iarrays mloop.oarrays.[i] mloop.funcs.[i] textGenerator |> fst)
        )

    List.init numCalls (fun i ->
        (tSpecTypes.[i], tSpecArgs.[i], mloop.funcs.[i].Name)
        |||> fun x y z ->
            ["template<> void "; z; "<"; x; ">("; y; ")"]
        |> stringCollapse ""
    )
    |> List.map brace
    |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> [y.Head] @ tab z @ y.Tail))
    |> List.map (fun x -> x |> stringCollapse "")
    |> fun x -> tmain @ x
    |> List.distinct


let parse (tokens: Token list) =

    // Scan for loop API calls and create loop state machines
    let mloops = scanMethodLoops tokens
    let oloops = scanObjectLoops tokens

    // Sort pragma objects into array and function pragmas
    let arrayPragmas, functionPragmas = tokens |> (scanPragmas >> sortPragmas)

    // Retrieve array and function objects from pragma info
    let arrays =
        List.init arrayPragmas.Length (fun i ->
            let directive, clauses, scope = arrayPragmas.[i]
            getArray clauses scope
        )

    let funcs =
        List.init functionPragmas.Length (fun i ->
            let directive, clauses, scope = functionPragmas.[i]
            let name = ([(snd directive).Head] |> tokenToStr).Head
            getFunction name clauses scope
        )

    // Create text for symmetry vectors
    let symmVecs = symmVecLines arrays

    // Pass array and function objects to the loops that call them, if they are visible
    sendArraysToLoops arrays mloops oloops
    sendFunctionsToLoops funcs mloops oloops

    let templates =
        (oloops |> List.map objectLoopTemplate) @ (mloops |> List.map methodLoopTemplate)
        |> List.reduce (@)
        |> List.distinct
        |> fun x -> symmVecs @ x
        |> stringCollapse "\n\n"

    let arraySwaps =
        arrays |> List.map (fun array ->
            String.concat "" ["promote<"; array.Type; ", "; string array.Rank; ">::type "; array.Name; ";\n"]
            :: String.concat "" [array.Name; " = allocate<"; array.Type; ", "; string array.Rank; ", "; symmVecName array; ">();\n"]
            :: match array.Info with
                | Array _ -> [""]
                | NetCDF _ -> array |> NestedLoop.ncGet (CppLoopTextGenerator([],[],[cppArrayDeclLine],[]))
                |> stringCollapse ""
        )

    let rec deleteFunctionPragmas (tokens: Token list) =
        let rec deleteFrom = function
        | FunctionPragmaPattern (pragma,[]) -> []
        | FunctionPragmaPattern (pragma, tail) -> deleteFrom tail
        | head :: tail -> head :: deleteFrom tail
        | [] -> []
        deleteFrom tokens

    let arrayPragmaLookup (pragma: Pragma) =
        arrayPragmas |> List.findIndex (fun x -> x = pragma) |> (fun i -> arraySwaps.[i])

    /// Swap all pragmas for new code
    let substitute (tokens: Token list) =
        let rec substitute' (pre: Token list) = function
        | ArrayPragmaPattern (pragma, tail) ->
            (pre |> tokenToStr |> respace |> reconcat |> newln)
            @ [arrayPragmaLookup pragma]
            @ (tail |> substitute' [])
        | FunctionPragmaPattern (pragma, tail) ->
            (pre |> tokenToStr |> respace |> reconcat |> newln)
            @ [templates]
            @ (tail |> deleteFunctionPragmas |> substitute' [])
        | head :: tail -> substitute' (pre @ [head]) tail
        | [] -> pre |> tokenToStr |> respace |> reconcat |> newln
        substitute' [] tokens

    let FPositions = (oloops |> List.map (fun x -> x.Call |> List.map thd3)) @ (mloops |> List.map (fun x -> x.Call |> List.map thd3)) |> List.reduce (@)
    let LPositions =
        FPositions |> List.map (fun y ->
            tokens.[y..]
            |> List.findIndex (fun z -> z = Token.Symbol ';')
            |> (+) y
        )
    let callBounds = (FPositions, LPositions) ||> List.map2 (fun f l -> seq{f..l})

    let oloopCallSwaps =
        List.init oloops.Length (fun i ->
            let name = oloops.[i].Init
            let args =
                List.init oloops.[i].Call.Length (fun j ->
                      (oloops.[i].iarrays.[j] |> List.map (fun x -> x.Name))
                    @ [oloops.[i].oarrays.[j].Name]
                    @ (oloops.[i].iarrays.[j] |> List.map extentsName)
                    @ [oloops.[i].oarrays.[j] |> extentsName])
                |> List.map (withCommas >> (stringCollapse ""))

            let arity =
                match oloops.[i].GetFunc.Arity with
                | Some a -> List.init oloops.[i].Call.Length (fun i -> a)
                | None   -> oloops.[i].iarrays |> List.map (fun x -> x.Length)

            let itype = oloops.[i].iarrays |> List.map (List.map (fun y -> y.Type))
            let otype = oloops.[i].oarrays |> List.map (fun x -> x.Type)
            let irank = oloops.[i].iarrays |> List.map (List.map (fun y -> y.Rank))
            let orank = oloops.[i].oarrays |> List.map (fun x -> x.Rank)
            let isymm = oloops.[i].iarrays |> List.map (List.map symmVecName)
            let osymm = oloops.[i].oarrays |> List.map symmVecName

            let ranks = (irank, orank) ||> List.map2 (fun x y -> (x |> List.map string) @ [string y])
            let types = (itype, otype) ||> List.map2 (fun x y -> x @ [y])
            let symms = (isymm, osymm) ||> List.map2 (fun x y -> x @ [y])

            let tSpecTypes =
                List.init oloops.[i].Call.Length (fun j ->
                    List.init (arity.[j]+1) (fun k ->
                        [types.[j].[k]; ranks.[j].[k]; symms.[j].[k]]
                        |> (withCommas >> stringCollapse "")
                    ) |> (withCommas >> stringCollapse "")
                )

            List.init oloops.[i].Call.Length (fun j -> String.concat "" ["\n"; name; "<"; tSpecTypes.[j]; ">"; "("; args.[j]; ");\n"])
        )

    let mloopCallSwaps =
        List.init mloops.Length (fun i ->
            let names = mloops.[i].Call |> List.map fst3
            let args =
                List.init mloops.[i].Call.Length (fun j ->
                      [mloops.[i].iarrays.[j].Name]
                    @ [mloops.[i].oarrays.[j].Name]
                    @ [mloops.[i].iarrays.[j] |> extentsName]
                    @ [mloops.[i].oarrays.[j] |> extentsName])
                |> List.map (withCommas >> (stringCollapse ""))

            let arity = mloops.[i].funcs.[1].Arity |> Option.get // hack

            let itype = mloops.[i].iarrays |> List.map (fun x -> x.Type)
            let otype = mloops.[i].oarrays |> List.map (fun x -> x.Type)
            let irank = mloops.[i].iarrays |> List.map (fun x -> x.Rank)
            let orank = mloops.[i].oarrays |> List.map (fun x -> x.Rank)
            let isymm = mloops.[i].iarrays |> List.map symmVecName
            let osymm = mloops.[i].oarrays |> List.map symmVecName

            let ranks = orank |> List.map (fun y -> (irank |> List.map string) @ [string y])
            let types = otype |> List.map (fun y -> itype @ [y])
            let symms = osymm |> List.map (fun y -> isymm @ [y])

            let tSpecTypes =
                List.init mloops.[i].Call.Length (fun j ->
                    List.init (arity+1) (fun k ->
                        [types.[j].[k]; ranks.[j].[k]; symms.[j].[k]]
                        |> (withCommas >> stringCollapse "")
                    ) |> (withCommas >> stringCollapse "")
                )

            List.init mloops.[i].Call.Length (fun j -> String.concat "" ["\n"; names.[j]; "<"; tSpecTypes.[j]; ">"; "("; args.[j]; ");\n"])
        )

    let callSwaps = (oloopCallSwaps @ mloopCallSwaps) |> List.reduce (@) |> List.map tokenize

    let rec swap (position: int) (bounds: seq<int> list) (tokens: Token list) : Token list =
        match tokens with
        | [] -> []
        | head :: tail ->
            if FPositions |> List.contains position then
                let i = FPositions |> List.findIndex (fun x -> x = position)
                let mask = Seq.concat [seq{0 .. (i-1)}; seq{(i+1) .. (bounds.Length)}] |> Seq.toList
                callSwaps.[i] @ swap (position+1) (List.init (bounds.Length - 1) (fun i -> bounds.[mask.[i]])) tail
            else
                head :: swap (position+1) bounds tail

    let loopNames = (oloops |> List.map (fun x -> x.Name)) @ (mloops |> List.map (fun x -> x.Name))
    let deleteLoopLines = loopNames |> List.map (fun x -> List.filter (fun (y:string) -> not (y.Contains x))) |> List.reduce (>>)

    tokens
    |> (swap 0 callBounds
        >> substitute
        >> (fun x -> "#include <omp.h>\n" :: x)
        >> List.filter (fun x -> not (x.Contains "object_for"))
        >> List.filter (fun x -> not (x.Contains "method_for"))
        >> deleteLoopLines
        >> stringCollapse "")

[<EntryPoint>]
let main args =
    let iFileName, oFileName = args.[0], args.[1]

    File.ReadAllText iFileName
    |> tokenize
    |> parse
    |> fun x -> File.WriteAllText (oFileName, x)

    0

//let tokens = File.ReadAllText iFileName |> tokenize;;
//let args = [|"/home/Christopher.Dupuis/agu2019proj/covariance.edgi"; "/home/Christopher.Dupuis/agu2019proj/covariance.cpp"|];;
//main [|"/home/Christopher.Dupuis/agu2019proj/covariance.edgi"; "/home/Christopher.Dupuis/agu2019proj/covariance.cpp"|];;

//let args = [|"/home/username/Downloads/EDGI_nested_iterators/fs/covariance.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/covariance.cpp"|];;
//main [|"/home/username/Downloads/EDGI_nested_iterators/fs/covariance.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/covariance.cpp"|];;

//let args = [|"/home/username/Downloads/EDGI_nested_iterators/fs/10D.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/10D.cpp"|];;
//main [|"/home/username/Downloads/EDGI_nested_iterators/fs/10D.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/10D.cpp"|];;

//let args = [|"/home/username/Downloads/EDGI_nested_iterators/fs/10vars.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/10vars.cpp"|];;
//main [|"/home/username/Downloads/EDGI_nested_iterators/fs/10vars.edgi"; "/home/username/Downloads/EDGI_nested_iterators/fs/10vars.cpp"|];;

