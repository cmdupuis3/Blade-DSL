
module Parser.Parser

    // For NetCDF integration, information about types and ranks is needed at compile time...

    open System.IO
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

    // These should match the macros defined by NetCDF
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

    let revMatchNCtype = function
        | "char" -> 1
        | "short" -> 3
        | "int" -> 4
        | "float" -> 5
        | "double" -> 6
        | "uchar" -> 7
        | "ushort" -> 8
        | "uint" -> 9
        | "int64" -> 10
        | "uint64" -> 11
        | "char*" -> 12 // ?
        | _ -> failwith "Could not match to a NetCDF type."


    let getNCnumDims fileName variableName = get_num_dims(fileName, variableName)

    let getNCvarType fileName variableName = get_var_type(fileName, variableName) |> matchNCtype

    let getNCdimTypes fileName variableName =
        let numDims = getNCnumDims fileName variableName
        let dimTypes = Array.init numDims (fun i -> -1)
        get_var_dim_types(fileName, variableName, dimTypes)
        dimTypes
        |> Array.map matchNCtype
        |> Array.toList

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

    let symmVecName (array: NestedArray) =
        if array.Symm.IsNone then "nullptr" else String.concat "" [array.Name; "_symm"]

    type NetCDFOutputInfo =
        {
            DimNames: string list
            DimValNames: string list
            DimValTypes: string list
        }

    /// Container for function pragma information
    type NestedFunction =
        {
            Name:  string
            Arity: int Option
            INames: string list
            IRank: int list
            OName: string
            OType: string
            ORank: int
            TDimSymm: int list
            Comm:  int list Option
            ParallelismLevels: int list
            Inner: string list
            TDimExtents: int list
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

        let iteratorDecl = iteratorDeclLine "size_t" loop.indNames.[j]
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
        String.concat "" ["for("; iName; " = 0; "; iName; " < "; extentName; "["; extentIndex; "] - "; iMin; "; "; iName; "++)";]

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

    ///////////////////////////////////////////////////////////////////////////

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
            //let sameArray = arrayNames |> isSame

            List.init arrayNames.Length (fun i ->
                List.init symModes.[i].Length (fun j ->
                    match (comModes.[i](* && sameArray.[i]*)), symModes.[i].[j] with
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
            let iLevels = (iarrays |> List.map (fun arr -> arr.Rank), func.IRank) ||> List.map2 (-)

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
            let indexNames = indNames 0 iLevels

            /// Chooses the correct iterator minimum for all input variables.
            let imins =
                let iMaps = iminMap states
                List.init iLevels.Length (fun i ->
                    List.init iLevels.[i] (fun j ->
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
                let oDiff = (List.init (states.Length) iDiff |> List.reduce (+)) - func.ORank
                List.init states.Length (fun i ->
                    if (iDiff i > 0) && (oDiff > 0) then
                        List.init (if iDiff i > oDiff then oDiff else iDiff i) (fun j acc -> index acc)
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
                    iarrayLevels = iLevels.[i];
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

        /// Deduce symmetry vector of the output array from input arrays and function.
        /// <param name="iarrays"> Input arrays </param>
        /// <param name="func"> Function applied to input arrays </param>
        let OutputSymmetry (iarrays: NestedArray list) (func: NestedFunction) =
            let comm = func.Comm |> function | Some c -> c | None -> (List.init iarrays.Length id)
            let commGroups = comm |> List.distinct
            let commLengths = List.init commGroups.Length (fun i ->
                comm |> List.filter (fun x -> x = commGroups.[i]) |> List.length
            )

            let iLevels, iSymms =
                let listSelect commGroup list =
                    List.zip comm list
                    |> List.filter (fun x -> fst x = commGroup)
                    |> List.map snd

                List.init commGroups.Length (fun i ->
                    let iSymms = iarrays |> List.map (fun x -> x.Symm |> function
                        | Some s -> s
                        | None -> (List.init x.Rank id))

                    let iarraysSub = iarrays |> listSelect commGroups.[i]
                    let iranks = func.IRank |> listSelect commGroups.[i]
                    let iSymmDist =
                        iSymms
                        |> listSelect commGroups.[i]
                        |> List.distinct
                        |> fun x -> x.[0]

                    // Double-check that all ranks in the commutativity group are equal.
                    assert(iarraysSub |> List.map (fun x -> x.Rank) |> List.distinct |> List.length = 1)

                    let iLevels =
                        ((iarraysSub |> List.map (fun x -> x.Rank)), iranks)
                        ||> List.map2 (-)
                        |> List.distinct
                        |> fun x -> x.[0]

                    iLevels, iSymmDist
                )
                |> List.unzip

            /// Re-index before reducing (needs more testing)
            let reIndex symms =
                let symmsMaxs = symms |> List.map (fun x -> List.max x)
                let symmsMins = symms |> List.map (fun x -> List.min x)
                symms |> List.mapi (fun i symm ->
                    let start = if i = 0 then 0 else symmsMaxs.[0..(i-1)] |> List.reduce (+)
                    symm
                    |> List.map (fun x -> x - symmsMins.[i] + start + 1)
                )
                |> List.reduce (@)

            let oSymms = 
                // This is okay because we already guaranteed that comm groups can only contain
                // copies of the same variable
                List.init commGroups.Length (fun i ->
                    iSymms.[i].[0..(iLevels.[i]-1)] 
                    |> (List.replicate >> List.collect) commLengths.[i]
                    |> List.sort // to be on the safe side
                )
                |> reIndex

            oSymms @ (func.TDimSymm |> List.map ((+) (List.max oSymms)))


        let private fileIDname (name: string) =
            String.concat "" [name; "_file_ncid"]

        let private variableIDname (name: string) =
            String.concat "" [name; "_var_ncid"]

        let private startsName (name: string) =
            String.concat "" [name; "_starts"]

        let private countsName (name: string) =
            String.concat "" [name; "_counts"]

        let private indicesName (name: string) =
            String.concat "" [name; "_indices"]

        let private dimIDnames (name: string) =
            String.concat "" [name; "_dim_ncids"]

        let private dimVarIDnames (name: string) =
            String.concat "" [name; "_dim_var_ncids"]

        let private dimNames (name: string) =
            String.concat "" [name; "_dim_names"]

        let private dimValsNames (name: string) (rank: int) =
            List.init rank (fun i -> String.concat "" [name; "_dim_"; string i; "_vals"])

        /// Autogenerate an N-ary nested_for loop
        /// <param name="array"> An input array class. </param>
        /// <param name="arg"> The position of this array in the list of input arguments. </param>
        let ncGet (textGenerator: LoopTextGenerator) (array: NestedArray) (func: NestedFunction) (arg: int) =
            let ncFileName, ncVarName =
                match array.Info with
                | NetCDF info -> info.FileName, info.VariableName
                | _ -> failwith "Cannot use NetCDF routines on non-NetCDF arrays"

            let indexNames = List.init (array.Rank-1) (fun i -> String.concat "" [textGenerator.IteratorName 0 i; "_"; array.Name])

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

            let ncDimTypes = getNCdimTypes ncFileName ncVarName

            // Assuming dimensions and dimension variables have the same name... (should, but don't necessarily)
            let ncDimValDeclLines =
                List.init array.Rank (fun i ->
                    String.concat "" [ncDimTypes.[i]; "* "; (dimValsNames array.Name array.Rank).[i];" = new "; ncDimTypes.[i]; "["; extentsName array; "["; string i;"]];"]
                )
            let ncDimValLines =
                List.init array.Rank (fun i ->
                    String.concat "" ["nc_get_var("; fileIDname array.Name; ", "; dimVarIDnames array.Name; "["; string i; "], "; (dimValsNames array.Name array.Rank).[i]; ");"]
                )

            /// nc_open call
            let ncInit =
                [
                    String.concat "" ["int "; fileIDname array.Name; ";"]
                    String.concat "" ["nc_open("; quote ncFileName; ", NC_NOWRITE, &"; fileIDname array.Name; ");"]
                    String.concat "" ["int "; variableIDname array.Name; ";";]
                    String.concat "" ["nc_inq_varid("; fileIDname array.Name; ", "; quote ncVarName; ", &"; variableIDname array.Name; ");"]
                    String.concat "" ["int* "; dimIDnames array.Name; " = new int[";  string array.Rank; "];"]
                    String.concat "" ["int* "; dimVarIDnames array.Name; " = new int[";  string array.Rank; "];"]
                    String.concat "" ["size_t* "; extentsName array; " = new size_t["; string array.Rank; "];"]
                    String.concat "" ["size_t* "; startsName array.Name; " = new size_t["; string array.Rank; "];"]
                    String.concat "" ["size_t* "; countsName array.Name; " = new size_t["; string array.Rank; "];"]
                    String.concat "" ["char** "; dimNames array.Name;  " = new char*["; string array.Rank; "];"]
                    String.concat "" ["nc_inq_vardimid("; fileIDname array.Name; ", "; variableIDname array.Name; ", "; dimIDnames array.Name; ");"]
                    String.concat "" ["for(size_t q = 0; q < "; string array.Rank; "; q++){"]
                    String.concat "" ["\t"; dimNames array.Name; "[q] = new char[NC_MAX_NAME];"]
                    String.concat "" ["\t"; "nc_inq_dimname("; fileIDname array.Name; ", "; dimIDnames array.Name; "[q], "; dimNames array.Name; "[q]);"]
                    String.concat "" ["\t"; "nc_inq_dimlen("; fileIDname array.Name; ", "; dimIDnames array.Name; "[q], &("; extentsName array; "[q]));"]
                    String.concat "" ["\t"; "nc_inq_varid("; fileIDname array.Name; ", "; dimNames array.Name; "[q], &("; dimVarIDnames array.Name; "[q]));"]
                    String.concat "" ["\t"; startsName array.Name; "[q] = 0;"]
                    String.concat "" ["\t"; countsName array.Name; "[q] = 1;"]
                    String.concat "" ["}"]
                    String.concat "" [countsName array.Name; "["; string (array.Rank-1); "] = "; extentsName array; "["; string (array.Rank-1); "];"]
                ] @ ncDimValDeclLines @ ncDimValLines @
                [
                    String.concat "" ["promote<"; array.Type; ", "; string array.Rank; ">::type "; array.Name; ";\n"]
                    String.concat "" [array.Name; " = allocate<typename promote<"; array.Type; ", "; string array.Rank; ">::type,"; symmVecName array; ">("; extentsName array; ");\n"]
                ]

            match textGenerator with
            | :? CppLoopTextGenerator ->
                (textGenerator :?> CppLoopTextGenerator :> pushText<CppLoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [startsName array.Name; "["; string i; "] = "; loop.indNames.[i]; ";\n"]
                )
            | _ ->
                (textGenerator :> pushText<LoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [startsName array.Name; "["; string i; "] = "; loop.indNames.[i]; ";\n"]
                )


            let inner = String.concat "" [
                "nc_get_vara("; fileIDname array.Name; ", "; variableIDname array.Name; ", "; startsName array.Name; ", "; countsName array.Name; ", "; lastIName; ");\n"
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
                @ [String.concat "" ["nc_close("; fileIDname array.Name; ");\n"]]
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

            let ncFileName, ncVarName, ncInfo =
                match oarray.Info, func.NCInfo with
                | NetCDF info, Some dims -> info.FileName, info.VariableName, dims
                | NetCDF info, _ -> failwith "Function is missing NetCDF info."
                | _ -> failwith "Cannot use NetCDF routines on non-NetCDF output arrays."

            let indexNames = List.init (oarray.Rank-1) (fun i -> String.concat "" [textGenerator.IteratorName 0 i; "_"; func.OName])

            let oSymms = oarray.Symm |> function | Some s -> s | None -> (List.init oarray.Rank id)
            let states = vStates [func.OName] [oSymms] [0]
            /// Chooses the correct iterator minimum for all input variables.
            let imins =
                let iMaps = iminMap states
                List.init oarray.Rank (fun j ->
                    if iMaps.Head.[j] = (0, j) then textGenerator.Zero else indexNames.[snd iMaps.Head.[j]]
                )

            // Dimension algebra for names and values
            let iLevels = (iarrays |> List.map (fun x -> x.Rank), func.IRank) ||> List.map2 (-)
            let nInputDims = (iLevels |> List.sum)

            let inputDimLines =
                List.init iarrays.Length (fun i ->
                    let acc = if i = 0 then 0 else (iLevels.[0..(i-1)] |> List.sum)
                    let iFileName, iVarName =
                        match iarrays.[i].Info with
                        | NetCDF info -> info.FileName, info.VariableName
                        | _ -> failwith "Cannot use NetCDF routines on non-NetCDF output arrays"

                    let ncDimNames = dimNames func.INames.[i]
                    let ncDimTypes = getNCdimTypes iFileName iVarName
                    List.init iLevels.[i] (fun j ->
                        [
                            String.concat "" ["string "; ncDimNames; string j; "_str = "; ncDimNames; "["; string j; "];"]
                            String.concat "" [ncDimNames; string j; "_str += \"_"; string (i+1); "\";"]
                            String.concat "" ["char* "; ncDimNames; string j; " = (char*)("; ncDimNames; string j; "_str.c_str());"]
                            String.concat "" ["nc_def_dim("; fileIDname func.OName; ", "; ncDimNames; string j; ", "; func.INames.[i]; "_extents["; string j; "], &("; dimIDnames func.OName; "["; string (j+acc); "]));"]
                            String.concat "" ["nc_def_var("; fileIDname func.OName; ", "; ncDimNames; string j; ", "; ncDimTypes.[j] |> revMatchNCtype |> string; ", 1, &("; dimIDnames func.OName; "["; string (j+acc); "]), &("; func.OName; "_dim_var_ncids["; string (j+acc); "]));"]
                            String.concat "" ["nc_enddef(";  fileIDname func.OName; ");"]
                            String.concat "" ["nc_put_var("; fileIDname func.OName; ", "; func.OName; "_dim_var_ncids["; string (j+acc); "], "; (dimValsNames func.INames.[i] iarrays.[i].Rank).[j]; ");"]
                            String.concat "" ["nc_redef(";   fileIDname func.OName; ");"]
                        ]
                    ) |> List.reduce (@)
                ) |> List.reduce (@)


            let outputDimLines =
                if func.ORank = 0 then [] else
                    List.init func.ORank (fun j ->
                        [ //ncInfo.DimExtents.[j]
                            String.concat "" ["nc_def_dim("; fileIDname func.OName; ", "; quote ncInfo.DimNames.[j]; ", "; string func.TDimExtents.[j]; ", &("; dimIDnames func.OName; "["; string (j+nInputDims); "]));"]
                            String.concat "" ["nc_def_var("; fileIDname func.OName; ", "; quote ncInfo.DimNames.[j]; ", "; ncInfo.DimValTypes.[j] |> revMatchNCtype |> string; ", 1, &("; dimIDnames func.OName; "["; string (j+nInputDims); "]), &("; func.OName; "_dim_var_ncids["; string (j+nInputDims); "]));"]
                            String.concat "" ["nc_enddef(";  fileIDname func.OName; ");"]
                            String.concat "" ["nc_put_var("; fileIDname func.OName; ", "; func.OName; "_dim_var_ncids["; string j; "], "; ncInfo.DimValNames.[j]; ");"]
                            String.concat "" ["nc_redef(";   fileIDname func.OName; ");"]
                        ]
                    ) |> List.reduce (@)

            /// nc_create call
            let ncInit = (
                [
                    String.concat "" ["int "; fileIDname func.OName; ";"]
                    String.concat "" ["nc_create("; func.OName; "_file_name, NC_CLOBBER, &"; fileIDname func.OName; ");"]
                    //String.concat "" ["size_t* "; extentsName oarray; " = new size_t["; string oarray.Rank; "];"]
                    String.concat "" ["int* "; dimIDnames func.OName; " = new int["; string oarray.Rank; "];"]
                    String.concat "" ["int* "; func.OName; "_dim_var_ncids = new int["; string oarray.Rank; "];"]
                ] @ inputDimLines @ outputDimLines @
                [
                    String.concat "" ["int "; variableIDname func.OName; ";";]
                    String.concat "" ["nc_def_var("; fileIDname func.OName; ", "; func.OName; "_variable_name, "; oarray.Type |> revMatchNCtype |> string; ", "; string oarray.Rank; ", "; dimIDnames func.OName; ", &"; variableIDname func.OName; ");"]
                    String.concat "" ["size_t* "; startsName func.OName;  " = new size_t["; string oarray.Rank; "];"]
                    String.concat "" ["size_t* "; countsName func.OName;  " = new size_t["; string oarray.Rank; "];"]
                    String.concat "" ["size_t* "; indicesName func.OName; " = new size_t["; string (oarray.Rank-1); "];"]
                    String.concat "" ["for(size_t q = 0; q < "; string oarray.Rank; "; q++){"]
                    String.concat "" ["\t"; startsName func.OName; "[q] = 0;"]
                    String.concat "" ["\t"; countsName func.OName; "[q] = 1;"]
                    String.concat "" ["}"]
                    String.concat "" [countsName func.OName; "["; string (oarray.Rank-1); "] = out_extents["; string (oarray.Rank-1); "];"]
                    String.concat "" ["nc_enddef(";  fileIDname func.OName; ");"]
                ]
            )

            match textGenerator with
            | :? CppLoopTextGenerator ->
                (textGenerator :?> CppLoopTextGenerator :> pushText<CppLoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [startsName func.OName; "["; string i; "] = "; loop.indNames.[i]; ";"]
                )
                (textGenerator :?> CppLoopTextGenerator :> pushText<CppLoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [indicesName func.OName; "["; string i; "] = "; loop.indNames.[i]; ";"]
                )
            | _ ->
                (textGenerator :> pushText<LoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [startsName func.OName; "["; string i; "] = "; loop.indNames.[i]; ";"]
                )
                (textGenerator :> pushText<LoopTextGenerator>).PushInnerNested (fun loop i ->
                    String.concat "" [indicesName func.OName; "["; string i; "] = "; loop.indNames.[i]; ";"]
                )

            let inner =
                [
                    String.concat "" ["auto "; func.OName; "_slice = index<typename promote<"; oarray.Type; ", "; string oarray.Rank; ">::type, "; symmVecName oarray; ", "; string oarray.Rank; ", "; string (oarray.Rank-1); ">("; func.OName; ", "; indicesName func.OName; ");\n"]
                    String.concat "" ["nc_put_vara("; fileIDname func.OName; ", "; variableIDname func.OName; ", "; startsName func.OName; ", "; countsName func.OName; ", "; func.OName; "_slice);\n"]
                ]

            let loop =
                {
                    iarrayName = func.OName;
                    iarrayType = oarray.Type;
                    iarrayLevels = oarray.Rank-1;
                    iRank = 1;
                    iExtents = "out_extents";
                    indNames = indexNames;
                    iMins = List.init oarray.Rank (fun j -> textGenerator.Zero);
                    parLevels = 0;
                }

            let ret =
                (ncInit |> newln |> tab)
                @ (unaryLoop loop textGenerator 0
                    |> List.fold (|>) inner
                    |> tab)
                @ ([String.concat "" ["nc_close("; fileIDname func.OName; ");"]] |> newln |> tab)
            ret



    ////////////////////////////////////////////////////////////////////////


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
        | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s1 :: Match @"^\*+" s2 :: tail -> s1 :: s2 :: " " :: respace tail
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
        | Token.Symbol ')' :: tail -> [], tail
        | _ -> [], []


    /// Container for method_for loop information; able to recieve messages about array and function pragmas
    type MethodLoop (nameIn: string, initIn: string list, callIn: (string * string * NestedArrayInfo * int) list, fposition: int, lposition: int)  =
        [<DefaultValue>] val mutable public iarrays: NestedArray list
        [<DefaultValue>] val mutable public oarrays: NestedArray list
        [<DefaultValue>] val mutable public funcs: NestedFunction list

        member this.Name = nameIn
        member this.Init = initIn // iarrays
        member this.Call = callIn // func, oarray, and call position
        member this.FPosition = fposition // position of the first token of the init pattern
        member this.LPosition = lposition // position of the last token of the init pattern

        member this.PushIarray (v: NestedArray) = this.iarrays <- this.iarrays @ [v]
        member this.PushOarray (v: NestedArray) = this.oarrays <- this.oarrays @ [v]
        member this.PushFunc (v: NestedFunction) = this.funcs <- this.funcs @ [v]

    /// Container for object_for loop information; able to recieve messages about array and function pragmas
    type ObjectLoop (nameIn: string, initIn: string, callIn: (string list * string * NestedArrayInfo * int) list, fposition: int, lposition: int) =
        [<DefaultValue>] val mutable public iarrays: NestedArray list list
        [<DefaultValue>] val mutable public oarrays: NestedArray list
        [<DefaultValue>] val mutable public func: NestedFunction list

        member this.Name = nameIn
        member this.Init = initIn // func
        member this.Call = callIn // iarrays, oarray, and call position
        member this.FPosition = fposition // position of the first token of the init pattern
        member this.LPosition = lposition // position of the last token of the init pattern

        member this.PushIarrays (v: NestedArray list) = this.iarrays <- this.iarrays @ [v]
        member this.PushOarray (v: NestedArray) = this.oarrays <- this.oarrays @ [v]
        member this.SetFunc (v: NestedFunction) = this.func <- [v]
        member this.GetFunc = this.func.Head


    let (|MethodLoopCallPattern|_|) (loop: Token) (position: int) = function
        | Token.Str oarrayName :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')'
            :: Token.Symbol '(' :: Token.Quote oExtentsName :: Token.Symbol ')' :: Token.Symbol ';' :: tail' when lname = loop ->
            let oarrayInfo = { ExtentsName = oExtentsName }
            Some (func, oarrayName, Array oarrayInfo, 10, tail')
        | Token.Str oarrayName :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')'
            :: Token.Symbol '(' :: Token.Quote oFileName :: Token.Symbol ',' :: Token.Quote oVarName :: Token.Symbol ')' :: Token.Symbol ';' :: tail' when lname = loop ->
            let oarrayInfo = { FileName = oFileName; VariableName = oVarName }
            Some (func, oarrayName, NetCDF oarrayInfo, 12, tail')
        | _ -> None

    /// Pattern for method_for loops and all their calls
    let rec (|MethodLoopPattern|_|) (position: int) = function
        | loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
            let args, tokens = toElements tail
            let iarrays = args |> tokenToStr
            let rec findCalls tokens' position' =
                match tokens' with
                | MethodLoopCallPattern loop position' (func, oarrayName, oarrayInfo, nTokens, tail') ->
                    (func, oarrayName, oarrayInfo, position'-1) :: findCalls tail' (position' + nTokens)
                | head' :: tail' -> findCalls tail' (position' + 1)
                | [] -> []
            let lposition = position + 5 + (args.Length)
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

    let (|ObjectLoopCallPattern|_|) (loop: Token) (position: int) = function
        | Token.Str oarrayName :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: tail' when lname = loop ->
            let args, t = toElements tail'
            let iarrays = args |> tokenToStr
            match t with
            | Token.Symbol '(' :: Token.Str oExtentsName :: Token.Symbol ')' :: Token.Symbol ';' :: t' ->
                let oarrayInfo = { ExtentsName = oExtentsName }
                let nTokens = args.Length + 8
                Some (iarrays, oarrayName, Array oarrayInfo, nTokens, t')
            | Token.Symbol '(' :: Token.Quote oFileName :: Token.Symbol ',' :: Token.Quote oVarName :: Token.Symbol ')' :: Token.Symbol ';' :: t' ->
                let oarrayInfo = { FileName = oFileName; VariableName = oVarName }
                let nTokens = args.Length + 10
                Some (iarrays, oarrayName, NetCDF oarrayInfo, nTokens, t')
            | _ -> failwith "Must call object_for loop with extents info or NetCDF info."
        | _ -> None

    /// Pattern for object_for loops and all their calls
    let rec (|ObjectLoopPattern|_|) (position: int) = function
        | loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: Token.Symbol ';' :: tail ->
            let rec findCalls tokens' position' =
                match tokens' with
                | ObjectLoopCallPattern loop position' (iarrays, oarrayName, oarrayInfo, nTokens, tail') ->
                    (iarrays, oarrayName, oarrayInfo, position') :: findCalls tail' (position' + nTokens)
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

    /// Scan code for all the object_for loops and return a list of them.
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
            match tail with
            | Token.Symbol ')' :: t -> Some (Clause (head, []), t)
            | _ ->
                let elements, t = toElements tail
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
            let valtype = getNCvarType fileName varName
            let rank = getNCnumDims fileName varName
            Some ( {Name = name; Type = valtype; Rank = rank; Symm = symGroups; Info = NetCDF { FileName = fileName; VariableName = varName } }, tail)
        | Token.Str valtype :: Token.Symbol '^' :: Token.Int rank :: Token.Str name :: Token.Symbol '(' :: Token.Str extentsName :: Token.Symbol ')' :: Token.Symbol ';' :: tail ->
            Some ( {Name = name; Type = valtype; Rank = rank; Symm = symGroups; Info = Array { ExtentsName = extentsName } }, tail )
        | _ -> None


    let hasClause name clauses =
        clauses |> List.exists (fst >> (fun x -> x = name))

    let getClause name clauses =
        clauses |> List.find (fst >> (fun x -> x = name))

    let stripWhitespace (tokens: Token list) = 
        tokens |> List.filter (fun x -> (x <> NewLine) && (x <> WhiteSpace))

    let getArray (clauses: Clause list) (block: Token list) =
        let hasSym = clauses |> hasClause "symmetry"
        let sym = if hasSym then
                        Some ((clauses |> getClause "symmetry") |> (snd >> tokenToInt))
                    else None
        block 
        |> stripWhitespace
        |> function 
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
        let otype = (clauses |> getClause "otype") |> (snd >> tokenToStr >> List.head)

        let ompLevels =
            if clauses |> hasClause "ompLevels" then
                (clauses |> getClause "ompLevels") |> (snd >> tokenToInt)
            else List.init iranks.Length (fun i -> 0)

        let com =
            if clauses |> hasClause "commutativity" then
                Some ((clauses |> getClause "commutativity") |> (snd >> tokenToInt))
            else None

        let tDimLens =
            if clauses |> hasClause "TDimLens" then
                ((clauses |> getClause "TDimLens") |> (snd >> tokenToInt))
            else []

        let tDimSymm =
            if clauses |> hasClause "TDimSymm" then
                (clauses |> getClause "TDimSymm") |> (snd >> tokenToInt)
            else List.init orank id

        assert (tDimLens.Length = orank)

        let ncDimNames =
            if clauses |> hasClause "ncDimNames" then
                Some ((clauses |> getClause "ncDimNames") |> (snd >> tokenToStr))
            else None

        let ncDimVals =
            if clauses |> hasClause "ncDimVals" then
                Some ((clauses |> getClause "ncDimVals") |> (snd >> tokenToStr))
            else None

        let ncDimTypes =
            if clauses |> hasClause "ncDimTypes" then
                Some ((clauses |> getClause "ncDimTypes") |> (snd >> tokenToStr))
            else None

        let ncInfo =
            match ncDimNames, ncDimVals, ncDimTypes with
            | Some names, Some vals, Some types ->
                Some { DimNames = names; DimValNames = vals; DimValTypes = types }
            | None, None, None ->
                None
            | _ ->
                let msg = sprintf "Incomplete specification of NetCDF output in function \"%s\"" name
                failwith msg

        {
            Name = name;
            Arity = arity;
            INames = input;
            IRank = iranks;
            OName = output;
            OType = otype;
            ORank = orank;
            TDimSymm = tDimSymm;
            Comm = com;
            ParallelismLevels = ompLevels;
            Inner = block |> deleteReturnLine |> tokenToStr |> respace |> reconcat |> newln;
            TDimExtents = tDimLens
            NCInfo = ncInfo
        }


    let fst3 (c, _, _) = c
    let snd3 (_, c, _) = c
    let thd3 (_, _, c) = c
    let fst4 (c, _, _, _) = c
    let snd4 (_, c, _, _) = c
    let thd4 (_, _, c, _) = c
    let fth4 (_, _, _, c) = c

    /// Message-passing function for sending info about array pragmas to nested loop objects
    let sendArraysToLoops (arrays: NestedArray list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
        for i in 0..mloops.Length-1 do
            // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
            for j in 0..arrays.Length-1 do
                for k in 0..mloops.[i].Init.Length-1 do
                    if mloops.[i].Init.[k] = arrays.[j].Name then
                        mloops.[i].PushIarray arrays.[j]
        for i in 0..oloops.Length-1 do
            // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
            for j in 0..oloops.[i].Call.Length-1 do
                let mutable itemp = []
                for l in 0..(oloops.[i].Call.[j] |> fst4).Length-1 do
                    for k in 0..arrays.Length-1 do
                        if (oloops.[i].Call.[j] |> fst4).[l] = arrays.[k].Name then
                            itemp <- itemp @ [arrays.[k]]
                oloops.[i].PushIarrays itemp

    /// Message-passing function for sending info about function pragmas to nested loop objects
    let sendFunctionsToLoops (funcs: NestedFunction list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
        // search for function names in funcs list and copy info to loop objects (results must be in order!)
        for i in 0..mloops.Length-1 do
            for j in 0..funcs.Length-1 do
                for k in 0..mloops.[i].Call.Length-1 do
                    if fst4 mloops.[i].Call.[k] = funcs.[j].Name then
                        mloops.[i].PushFunc funcs.[j]
        for i in 0..oloops.Length-1 do
            for j in 0..funcs.Length-1 do
                if oloops.[i].Init = funcs.[j].Name then
                    oloops.[i].SetFunc funcs.[j]

    /// Generate output arrays from calls and type/rank deduction
    let sendOutputArraysToLoops (mloops: MethodLoop list) (oloops: ObjectLoop list) =
        let getORank iarrays func =
            iarrays
            |> List.map (fun x -> x.Rank)
            |> fun x -> (x, func.IRank) ||> List.map2 (-)
            |> List.sum
            |> (+) func.ORank
        let getOSymm iarrays func = NestedLoop.OutputSymmetry iarrays func

        for i in 0..mloops.Length-1 do
            for j in 0..mloops.[i].funcs.Length-1 do
                let otype = mloops.[i].funcs.[j].OType
                let orank = getORank mloops.[i].iarrays mloops.[i].funcs.[j]
                let osymm = getOSymm mloops.[i].iarrays mloops.[i].funcs.[j]
                for k in 0..mloops.[i].Call.Length-1 do
                    if fst4 mloops.[i].Call.[k] = mloops.[i].funcs.[j].Name then
                        let oname = snd4 mloops.[i].Call.[k]
                        match thd4 mloops.[i].Call.[k] with
                        | Array info  -> {Name = oname; Type = otype; Rank = orank; Symm = Some osymm; Info = Array { ExtentsName = info.ExtentsName } }
                        | NetCDF info -> {Name = oname; Type = otype; Rank = orank; Symm = Some osymm; Info = NetCDF info }
                        |> mloops.[i].PushOarray

        for i in 0..oloops.Length-1 do
            let otype = oloops.[i].GetFunc.OType
            for k in 0..oloops.[i].iarrays.Length-1 do
                let orank = getORank oloops.[i].iarrays.[k] oloops.[i].GetFunc
                let osymm = getOSymm oloops.[i].iarrays.[k] oloops.[i].GetFunc
                let oname = snd4 oloops.[i].Call.[k]
                match thd4 oloops.[i].Call.[k] with
                | Array info  -> {Name = oname; Type = otype; Rank = orank; Symm = Some osymm; Info = Array { ExtentsName = info.ExtentsName } }
                | NetCDF info -> {Name = oname; Type = otype; Rank = orank; Symm = Some osymm; Info = NetCDF info }
                |> oloops.[i].PushOarray

    /// Wrapper for message-passing routines to preserve correct calling order
    let populateLoops (arrays: NestedArray list) (funcs: NestedFunction list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
        sendArraysToLoops arrays mloops oloops
        sendFunctionsToLoops funcs mloops oloops
        sendOutputArraysToLoops mloops oloops

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
            String.concat "" ["static constexpr const size_t "; name; "_symm["; symm.Length |> string; "] = {"; symm |> List.map string |> withCommas |> stringCollapse " "; "};\n"]
        )

    let objectLoopArity (oloop: ObjectLoop) =
        match oloop.GetFunc.Arity with
        | Some a -> // fixed arity => specify all argument names
            List.init oloop.Call.Length (fun i -> a)
        | None ->  // variable arity => specify one argument name and template it
            oloop.iarrays |> List.map (fun x -> x.Length)

    let objectLoopDimValNames (i: int) (oloop: ObjectLoop) =
        let arity = oloop |> objectLoopArity
        let irank = oloop.iarrays |> List.map (List.map (fun x -> x.Rank))
        let iLevels = (irank.[i], oloop.GetFunc.IRank) ||> List.map2 (-)

        let idimTypes =
            oloop.iarrays.[i]
            |> List.map (fun iarray ->
                let file, var = iarray.Info |> function | NetCDF info -> info.FileName, info.VariableName | Array _ -> failwith "squuurrr"
                getNCdimTypes file var
            )

        let odimTypes =
            oloop.GetFunc.NCInfo
            |> Option.get
            |> fun x -> x.DimValTypes

        let idimValNames =
            List.init arity.[i] (fun j ->
                List.init iLevels.[j] (fun k ->
                    String.concat "" ["\n\t"; idimTypes.[j].[k]; "* "; oloop.GetFunc.INames.[j]; "_dim_"; string k; "_vals"]
                )
            ) |> List.reduce (@)

        let odimValNames =
            match odimTypes with
            | [] -> []
            |_ ->
                List.init oloop.GetFunc.ORank (fun j ->
                    String.concat "" ["\n\t"; odimTypes.[j]; "* "; oloop.GetFunc.OName; "_dim_"; string j; "_vals"]
                )

        idimValNames @ odimValNames


    let objectLoopTemplate (oloop: ObjectLoop) =

        let numCalls = oloop.Call.Length
        let arity = oloop |> objectLoopArity
        let itype = oloop.iarrays |> List.map (List.map (fun x -> x.Type))
        let otype = oloop.oarrays |> List.map (fun x -> x.Type)
        let irank = oloop.iarrays |> List.map (List.map (fun x -> x.Rank))
        let orank = oloop.oarrays |> List.map (fun x -> x.Rank)
        let isymm = oloop.iarrays |> List.map (List.map symmVecName)
        let osymm = oloop.oarrays |> List.map symmVecName

        let inames =
            match oloop.GetFunc.Arity with
            | Some _ -> // fixed arity => specify all argument names
                List.init numCalls (fun i -> oloop.GetFunc.INames)
            | None ->  // variable arity => specify one argument name and template it
                List.init numCalls (fun i -> List.init arity.[i] (fun j -> String.concat "" [oloop.GetFunc.INames.Head; "_"; string j]))

        let onames =  List.init numCalls (fun i -> oloop.GetFunc.OName)

        let ranks = (irank, orank) ||> List.map2 (fun x y -> (x |> List.map string) @ [string y])
        let types = (itype, otype) ||> List.map2 (fun x y -> x @ [y])
        let symms = (isymm, osymm) ||> List.map2 (fun x y -> x @ [y])
        let names = (inames, onames) ||> List.map2 (fun x y -> x @ [y])

        let templateTypes =
            arity |> List.map (fun a ->
                List.init a (fun i -> String.concat "" ["typename ITYPE"; string (i+1); ", const size_t IRANK"; string (i+1); ", const size_t* ISYM"; string (i+1)])
                |> fun x -> x @ ["typename OTYPE, const size_t ORANK, const size_t* OSYM"]
                |> withCommas
                |> stringCollapse ""
            )

        let templateArgTypes =
            (arity, inames) ||> List.map2 (fun a (names: string list) ->
                (List.init a (fun i -> String.concat "" ["\n\ttypename promote<ITYPE"; string (i+1); ", IRANK"; string (i+1); ">::type "; names.[i]])
                |> fun x -> x @ [String.concat "" ["\n\ttypename promote<OTYPE, ORANK>::type "; oloop.GetFunc.OName]])
                @ (List.init a (fun i -> String.concat "" ["\n\tconst size_t "; names.[i]; "_extents[IRANK"; string (i+1); "]"]))
                |> withCommas
                |> stringCollapse ""
            )

        let aritySuffix = List.init numCalls (fun i -> if oloop.GetFunc.Arity.IsNone then String.concat "" ["_arity"; string arity.[i]] else "")

        let tmain =
            List.init numCalls (fun i ->
                String.concat "" [
                    "template<"; templateTypes.[i]; "> void "; oloop.GetFunc.Name; aritySuffix.[i];
                    "("; templateArgTypes.[i]; ",";
                    "\n\tconst size_t "; oloop.GetFunc.OName; "_extents[ORANK]";
                    "){\n\t// Nothing to see here.\n}"
                ]
            )

        let tmainNC =
            match oloop.GetFunc.NCInfo with
            | None -> []
            | Some _ ->
                List.init numCalls (fun i ->
                    let idimNames =
                        inames.[i] |> List.map (fun iname -> String.concat "" ["\n\tchar** "; iname; "_dim_names"])

                    let odimNames =
                        if oloop.GetFunc.ORank = 0 then [] else
                            [String.concat "" ["\n\tchar** "; onames.[i]; "_dim_names"]]

                    let dimNames =
                        idimNames @ odimNames
                        |> withCommas
                        |> stringCollapse ""

                    let dimValNames =
                        oloop
                        |> objectLoopDimValNames i
                        |> withCommas
                        |> stringCollapse ""

                    String.concat "" [
                        "template<"; templateTypes.[i]; "> void "; oloop.GetFunc.Name; "_netcdf"; aritySuffix.[i];
                        "("; templateArgTypes.[i]; ",";
                        "\n\tconst size_t "; oloop.GetFunc.OName; "_extents[ORANK],"; // (still need this as long as truly nested I/O is unimplemented)
                        "\n\tchar* "; oloop.GetFunc.OName; "_file_name,";
                        "\n\tchar* "; oloop.GetFunc.OName; "_variable_name,";
                        dimNames; ",";
                        dimValNames;
                        "){\n\t// Nothing to see here.\n}"
                    ]
                )

        let tSpecTypes =
            List.init numCalls (fun i ->
                List.init (arity.[i]+1) (fun j ->
                    [types.[i].[j]; ranks.[i].[j]; symms.[i].[j]]
                    |> (withCommas >> stringCollapse "")
                ) |> (withCommas >> stringCollapse "")
            )

        let tSpecArgs =
            List.init numCalls (fun i ->
                let specArgs =    List.init (arity.[i]+1) (fun j -> String.concat "" ["\n\ttypename promote<"; types.[i].[j]; ", "; ranks.[i].[j]; ">::type "; names.[i].[j]])
                let extentsArgs = List.init (arity.[i]+1) (fun j -> String.concat "" ["\n\tconst size_t "; names.[i].[j]; "_extents["; ranks.[i].[j]; "]"])
                let oarrayArgs =
                    match oloop.oarrays.[i].Info with
                    | Array _ -> []
                    | NetCDF _ -> [
                        String.concat "" ["\n\tchar* "; oloop.GetFunc.OName; "_file_name"]
                        String.concat "" ["\n\tchar* "; oloop.GetFunc.OName; "_variable_name"]
                    ]
                let iarrayArgs =
                    List.init arity.[i] (fun j ->
                        match oloop.oarrays.[i].Info with
                        | Array _ -> []
                        | NetCDF _ -> [
                            String.concat "" ["\n\tchar** "; oloop.GetFunc.INames.[j]; "_dim_names"]
                        ]
                    )
                    |> List.reduce (@)

                match oloop.GetFunc.NCInfo with
                | None -> []
                | Some _ -> oloop |> objectLoopDimValNames i
                |> fun x -> specArgs @ extentsArgs @ oarrayArgs @ iarrayArgs @ x
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
                    (NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] oloop.GetFunc textGenerator |> fst) @
                    [match oloop.oarrays.[i].Info with
                        | Array _ -> [""]
                        | NetCDF _ -> oloop.oarrays.[i] |> NestedLoop.ncPut (CppLoopTextGenerator([],[],[],[])) oloop.iarrays.[i] oloop.GetFunc
                        |> stringCollapse ""]
                )
            | None ->
                let funcs = List.init numCalls (fun i ->
                    {
                        Name = String.concat "" [oloop.GetFunc.Name; aritySuffix.[i]];
                        Arity = Some(arity.[i]);
                        INames = inames.[i];
                        IRank = List.init arity.[i] (fun j -> oloop.GetFunc.IRank.Head);
                        OName = oloop.GetFunc.OName;
                        OType = oloop.GetFunc.OType;
                        ORank = oloop.GetFunc.ORank;
                        TDimSymm = oloop.GetFunc.TDimSymm;
                        Comm = oloop.GetFunc.Comm;
                        ParallelismLevels = oloop.GetFunc.ParallelismLevels;
                        Inner = oloop.GetFunc.Inner |> List.map (tokenize >> deleteReturnLine >> (expandVariadic i))
                        TDimExtents = oloop.GetFunc.TDimExtents
                        NCInfo = oloop.GetFunc.NCInfo
                    }
                )
                List.init numCalls (fun i ->
                    let textGenerator = CppLoopTextGenerator([], [cppOmpLine],[cppArrayDeclLine],[])
                    (String.concat "" ["omp_set_nested("; string nOmp;");\n"]) ::
                    (NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] funcs.[i] textGenerator |> fst) @
                    [match oloop.oarrays.[i].Info with
                        | Array _ -> [""]
                        | NetCDF _ -> oloop.oarrays.[i] |> NestedLoop.ncPut (CppLoopTextGenerator([],[],[],[])) oloop.iarrays.[i] funcs.[i]
                        |> stringCollapse ""]
                )

        List.init numCalls (fun i ->
            let name =
                match oloop.oarrays.[i].Info with | Array _ -> "" | NetCDF _ -> "_netcdf"
                |> fun x -> String.concat "" [oloop.GetFunc.Name; x; aritySuffix.[i]]
            (tSpecTypes.[i], tSpecArgs.[i], name)
            |||> fun x y z -> ["template<> void "; z; "<"; x; ">("; y; ")"]
            |> stringCollapse ""
        )
        |> List.map brace
        |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> [y.Head] @ tab z @ y.Tail))
        |> List.map (fun x -> x |> stringCollapse "")
        |> fun x -> tmain @ tmainNC @ x
        |> List.distinct

    let methodLoopArity (mloop: MethodLoop) =
        mloop.funcs
        |> List.map (fun x -> x.Arity)
        |> List.map (function | Some a -> a | None -> failwith "method_for loops must have fixed arity.")
        |> fun x ->
            let a = List.distinct x
            if a.Length = 1 then a.Head else failwith "method_for loops must have fixed arity."


    let methodLoopDimValNames (i: int) (mloop: MethodLoop) =
        let arity = mloop |> methodLoopArity
        let irank = mloop.iarrays |> List.map (fun x -> x.Rank)

        let iLevels = (irank, mloop.funcs.[i].IRank) ||> List.map2 (-)
        let idimTypes =
            mloop.iarrays
            |> List.map (fun iarray ->
                let file, var = iarray.Info |> function | NetCDF info -> info.FileName, info.VariableName | Array _ -> failwith "squuurrr"
                getNCdimTypes file var
            )

        let odimTypes =
            mloop.funcs.[i].NCInfo
            |> Option.get
            |> fun x -> x.DimValTypes

        let idimValNames =
            List.init arity (fun j ->
                List.init iLevels.[j] (fun k ->
                    String.concat "" ["\n\t"; idimTypes.[j].[k]; "* "; mloop.funcs.[i].INames.[j]; "_dim_"; string k; "_vals"]
                )
            ) |> List.reduce (@)

        let odimValNames =
            match odimTypes with
            | [] -> []
            |_ ->
                List.init mloop.funcs.[i].ORank (fun j ->
                    String.concat "" ["\n\t"; odimTypes.[j]; "* "; mloop.funcs.[i].OName; "_dim_"; string j; "_vals"]
                )

        idimValNames @ odimValNames

    let methodLoopTemplate (mloop: MethodLoop) =
        let arity = mloop |> methodLoopArity

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
            List.init arity (fun i -> String.concat "" ["typename ITYPE"; string (i+1); ", const size_t IRANK"; string (i+1); ", const size_t* ISYM"; string (i+1)])
            |> fun x -> x @ ["typename OTYPE, const size_t ORANK, const size_t* OSYM"]
            |> withCommas
            |> stringCollapse ""

        let templateArgTypes =
            List.init numCalls (fun i ->
                (List.init arity (fun j -> String.concat "" ["\n\ttypename promote<ITYPE"; string (j+1); ", IRANK"; string (j+1); ">::type "; inames.[i].[j]])
                |> fun x -> x @ [String.concat "" ["\n\ttypename promote<OTYPE, ORANK>::type "; onames.[i]]])
                @ (List.init arity (fun j -> String.concat "" ["\n\tconst size_t "; inames.[i].[j]; "_extents[IRANK"; string (j+1); "]"]))
                |> withCommas
                |> stringCollapse ""
            )

        let aritySuffix = if mloop.funcs.[0].Arity.IsNone then String.concat "" ["_arity"; string arity] else ""

        let tmain =
            List.init numCalls (fun i ->
                String.concat "" [
                    "template<"; templateTypes; "> void "; mloop.funcs.[i].Name; aritySuffix;
                    "("; templateArgTypes.[i]; ",";
                    "\n\tconst size_t "; mloop.funcs.[i].OName; "_extents[ORANK]";
                    "){\n\t// Nothing to see here.\n}"
                ]
            )

        let tmainNC =
            match mloop.iarrays.Head.Info with
            | Array _ -> []
            | NetCDF _ ->
                List.init numCalls (fun i ->
                    let idimNames =
                        inames.[i] |> List.map (fun iname -> String.concat "" ["\n\tchar** "; iname; "_dim_names"])

                    let odimNames =
                        if mloop.funcs.[i].ORank = 0 then [] else
                            [String.concat "" ["\n\tchar** "; onames.[i]; "_dim_names"]]

                    let dimNames =
                        idimNames @ odimNames
                        |> withCommas
                        |> stringCollapse ""

                    let dimValNames =
                        mloop
                        |> methodLoopDimValNames i
                        |> withCommas
                        |> stringCollapse ""

                    String.concat "" [
                        "template<"; templateTypes; "> void "; mloop.funcs.[i].Name; "_netcdf"; aritySuffix;
                        "("; templateArgTypes.[i]; ",";
                        "\n\tconst size_t "; mloop.funcs.[i].OName; "_extents[ORANK],"; // (still need this as long as truly nested I/O is unimplemented)
                        "\n\tchar* "; mloop.funcs.[i].OName; "_file_name,";
                        "\n\tchar* "; mloop.funcs.[i].OName; "_variable_name,";
                        dimNames; ",";
                        dimValNames;
                        "){\n\t// Nothing to see here.\n}"
                    ]
                )

        let tSpecTypes =
            List.init numCalls (fun i ->
                List.init (arity+1) (fun j ->
                    [types.[i].[j]; ranks.[i].[j]; symms.[i].[j]]
                    |> (withCommas >> stringCollapse "")
                ) |> (withCommas >> stringCollapse "")
            )

        let tSpecArgs =
            List.init numCalls (fun i ->
                let specArgs =    List.init (arity+1) (fun j -> String.concat "" ["\n\ttypename promote<"; types.[i].[j]; ", "; ranks.[i].[j]; ">::type "; names.[i].[j]])
                let extentsArgs = List.init (arity+1) (fun j -> String.concat "" ["\n\tconst size_t "; names.[i].[j]; "_extents["; ranks.[i].[j]; "]"])
                let oarrayArgs =
                    match mloop.oarrays.[i].Info with
                    | Array _ -> []
                    | NetCDF _ ->
                        [
                            String.concat "" ["\n\tchar* "; mloop.funcs.[i].OName; "_file_name"]
                            String.concat "" ["\n\tchar* "; mloop.funcs.[i].OName; "_variable_name"]
                        ]
                let iarrayArgs =
                    List.init arity (fun j ->
                        match mloop.oarrays.[i].Info with
                        | Array _ -> []
                        | NetCDF _ -> [
                            String.concat "" ["\n\tchar** "; mloop.funcs.[i].INames.[j]; "_dim_names"]
                        ]
                    )
                    |> List.reduce (@)

                match mloop.iarrays.Head.Info with
                | Array _ -> []
                | NetCDF _ -> mloop |> methodLoopDimValNames i
                |> fun x -> specArgs @ extentsArgs @ oarrayArgs @ iarrayArgs @ x
                |> (withCommas >> stringCollapse "")
            )

        let tSpecInner =
            List.init numCalls (fun i ->
                let nOmp = mloop.funcs.[i].ParallelismLevels |> List.sum
                let textGenerator = CppLoopTextGenerator([], [cppOmpLine],[cppArrayDeclLine],[])
                String.concat "" ["omp_set_nested("; string nOmp;");\n"] ::
                (NestedLoop.Nary mloop.iarrays mloop.oarrays.[i] mloop.funcs.[i] textGenerator |> fst) @
                [match mloop.oarrays.[i].Info with
                    | Array _ -> [""]
                    | NetCDF _ -> mloop.oarrays.[i] |> NestedLoop.ncPut (CppLoopTextGenerator([],[],[],[])) mloop.iarrays mloop.funcs.[i]
                    |> stringCollapse ""]
            )

        List.init numCalls (fun i ->
            let name =
                match mloop.oarrays.[i].Info with | Array _ -> "" | NetCDF _ -> "_netcdf"
                |> fun x -> String.concat "" [mloop.funcs.[i].Name; x]
            (tSpecTypes.[i], tSpecArgs.[i], name)
            |||> fun x y z ->
                ["template<> void "; z; "<"; x; ">("; y; ")"]
            |> stringCollapse ""
        )
        |> List.map brace
        |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> [y.Head] @ tab z @ y.Tail))
        |> List.map (fun x -> x |> stringCollapse "")
        |> fun x -> tmain @ tmainNC @ x
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
        populateLoops arrays funcs mloops oloops

        let templates =
            (oloops |> List.map objectLoopTemplate) @ (mloops |> List.map methodLoopTemplate)
            |> List.reduce (@)
            |> List.distinct
            |> fun x -> symmVecs @ x
            |> stringCollapse "\n\n"

        let arraySwaps =
            List.init arrays.Length (fun i ->
                funcs |> List.map (fun func ->
                    match arrays.[i].Info with
                        | Array  _ ->
                            [String.concat "" [
                                "promote<"; arrays.[i].Type; ", "; string arrays.[i].Rank; ">::type "; arrays.[i].Name; ";\n";
                                arrays.[i].Name; " = allocate<typename promote<"; arrays.[i].Type; ", "; string arrays.[i].Rank; ">::type,"; symmVecName arrays.[i]; ">("; extentsName arrays.[i]; ");\n"
                            ]]
                        | NetCDF _ -> NestedLoop.ncGet (CppLoopTextGenerator([],[],[cppArrayDeclLine],[])) arrays.[i] func i
                    |> stringCollapse ""
                )
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
                @ (arrayPragmaLookup pragma)
                @ (tail |> substitute' [])
            | FunctionPragmaPattern (pragma, tail) ->
                (pre |> tokenToStr |> respace |> reconcat |> newln)
                @ [templates]
                @ (tail |> deleteFunctionPragmas |> substitute' [])
            | head :: tail -> substitute' (pre @ [head]) tail
            | [] -> pre |> tokenToStr |> respace |> reconcat |> newln
            substitute' [] tokens

        let FPositions = (oloops |> List.map (fun x -> x.Call |> List.map fth4)) @ (mloops |> List.map (fun x -> x.Call |> List.map fth4)) |> List.reduce (@)
        let LPositions =
            FPositions |> List.map (fun y ->
                tokens.[y..]
                |> List.findIndex (fun z -> z = Token.Symbol ';')
                |> (+) y
            )

        let oloopCallSwaps =
            List.init oloops.Length (fun i ->
                let iarrays = oloops.[i].iarrays
                let oarrays = oloops.[i].oarrays
                let name =
                    oloops.[i].Init
                    |> (fun x ->
                        match oloops.[i].GetFunc.NCInfo with
                        | None -> x
                        | Some _ -> String.concat "" [x; "_netcdf"]
                    )

                let args =
                    let basic =
                        List.init oloops.[i].Call.Length (fun j ->
                            (iarrays.[j] |> List.map (fun x -> x.Name))
                            @ [oarrays.[j].Name]
                            @ (iarrays.[j] |> List.map extentsName)
                            @ [String.concat "" [extentsName oarrays.[j]; "_"; oarrays.[j].Name]])

                    let extra =
                        match oloops.[i].GetFunc.NCInfo with
                        | None -> [[]]
                        | Some _ ->
                            List.init oloops.[i].Call.Length (fun j ->
                                match oarrays.[j].Info with
                                | Array _ -> failwith "impossible"
                                | NetCDF info ->
                                    let func = oloops.[i].GetFunc
                                    let iLevels = ((iarrays.[j] |> List.map (fun x -> x.Rank)), func.IRank) ||> List.map2 (-)
                                    [String.concat "" ["(char*)\""; info.FileName; "\""]; String.concat "" ["(char*)\""; info.VariableName; "\""]]
                                    @ (iarrays.[j] |> List.map (fun array -> String.concat "" [array.Name; "_dim_names"]))
                                    @ ((iarrays.[j], iLevels)
                                        ||> List.map2 (fun array levels ->
                                            List.init levels (fun k -> String.concat "" [array.Name; "_dim_"; string k; "_vals"])
                                        ) |> List.reduce (@)
                                    ) @ (List.init func.ORank (fun k -> String.concat "" [oarrays.[j].Name; "_dim_"; string k; "_vals"]))
                            )
                    (basic, extra)
                    ||> List.map2 (@)
                    |> List.map (withCommas >> (stringCollapse ""))

                let arity = objectLoopArity oloops.[i]

                let itype = iarrays |> List.map (List.map (fun x -> x.Type))
                let irank = iarrays |> List.map (List.map (fun x -> x.Rank))
                let isymm = iarrays |> List.map (List.map symmVecName)

                let otype = oarrays |> List.map (fun x -> x.Type)
                let orank = oarrays |> List.map (fun x -> x.Rank)
                let osymm = oarrays |> List.map symmVecName

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

                List.init oloops.[i].Call.Length (fun j ->
                    let func = oloops.[i].GetFunc
                    let iLevels = ((iarrays.[j] |> List.map (fun x -> x.Rank)), func.IRank) ||> List.map2 (-)
                    let nInputDims = (iLevels |> List.sum)
                    let oExtentsName = String.concat "" [extentsName oarrays.[j]; "_"; oarrays.[j].Name]

                    let iExtentsLines =
                        List.init arity.[i] (fun k ->
                            let acc = if k = 0 then 0 else (iLevels.[0..(k-1)] |> List.sum)

                            List.init iLevels.[k] (fun l ->
                                [
                                    String.concat "" [oExtentsName; "["; string (acc+l); "] = "; extentsName iarrays.[j].[k]; "["; string l; "];"]
                                ]
                            ) |> List.reduce (@)
                        ) |> List.reduce (@)

                    let oExtentsLines =
                        if func.ORank = 0 then [] else
                            let tDimExtents = func.TDimExtents
                            List.init func.ORank (fun k ->
                                String.concat "" [oExtentsName; "["; string (nInputDims+k); "] = "; string tDimExtents.[k] ]
                            )

                    [
                        "\n"
                        String.concat "" ["size_t* "; oExtentsName; " = new size_t["; string oarrays.[j].Rank; "];"]
                    ]
                    @ iExtentsLines
                    @ oExtentsLines
                    @ [
                        String.concat "" ["promote<"; oarrays.[j].Type; ", "; string oarrays.[j].Rank; ">::type "; oarrays.[j].Name; ";"]
                        String.concat "" [oarrays.[j].Name; " = allocate<typename promote<"; oarrays.[j].Type; ", "; string oarrays.[j].Rank; ">::type,"; oarrays.[j].Name; "_symm>("; oExtentsName;");"]
                        String.concat "" [name; "<"; tSpecTypes.[j]; ">"; "("; args.[j]; ");"]
                    ]
                    |> newln
                    |> stringCollapse ""
                )
            )

        let mloopCallSwaps =
            List.init mloops.Length (fun i ->
                let iarrays = mloops.[i].iarrays
                let oarrays = mloops.[i].oarrays
                let names =
                    mloops.[i].Call
                    |> List.map (fst4 >> (fun x ->
                        match iarrays.Head.Info with
                        | Array _ -> x
                        | NetCDF _ -> String.concat "" [x; "_netcdf"]
                    ))

                let args =
                    let basic =
                        List.init mloops.[i].Call.Length (fun j ->
                            (iarrays |> List.map (fun x -> x.Name))
                            @ [oarrays.[j].Name]
                            @ (iarrays |> List.map extentsName)
                            @ [String.concat "" [extentsName oarrays.[j]; "_"; oarrays.[j].Name]])

                    let extra =
                        match iarrays.Head.Info with
                        | Array _ -> [[]]
                        | NetCDF info ->
                            let dims = iarrays |> List.map (fun array -> String.concat "" [array.Name; "_dim_names"])
                            List.init mloops.[i].Call.Length (fun j ->
                                let fileName, varName =
                                    match oarrays.[j].Info with
                                    | Array _ -> failwith "impossible 2"
                                    | NetCDF info -> info.FileName, info.VariableName

                                let func = mloops.[i].funcs.[j]
                                let iLevels = ((iarrays |> List.map (fun x -> x.Rank)), func.IRank) ||> List.map2 (-)
                                (iarrays, iLevels)
                                ||> List.map2 (fun array levels ->
                                    List.init levels (fun k -> String.concat "" [array.Name; "_dim_"; string k; "_vals"])
                                ) |> List.reduce (@)
                                |> (@) dims
                                |> fun x -> x @ (List.init func.ORank (fun k -> String.concat "" [oarrays.[j].Name; "_dim_"; string k; "_vals"]))
                                |> fun x -> ([String.concat "" ["(char*)\""; fileName; "\""]; String.concat "" ["(char*)\""; varName; "\""]]) @ x
                            )

                    (basic, extra)
                    ||> List.map2 (@)
                    |> List.map (withCommas >> (stringCollapse ""))

                let arity = mloops.[i].funcs.Head.Arity |> Option.get // hack

                let itype = iarrays |> List.map (fun x -> x.Type)
                let irank = iarrays |> List.map ((fun x -> x.Rank) >> string)
                let isymm = iarrays |> List.map symmVecName

                let otype = oarrays |> List.map (fun x -> x.Type)
                let orank = oarrays |> List.map ((fun x -> x.Rank) >> string)
                let osymm = oarrays |> List.map symmVecName

                let ranks = orank |> List.map (fun y -> irank @ [y])
                let types = otype |> List.map (fun y -> itype @ [y])
                let symms = osymm |> List.map (fun y -> isymm @ [y])

                let tSpecTypes =
                    List.init mloops.[i].Call.Length (fun j ->
                        List.init (arity+1) (fun k ->
                            [types.[j].[k]; ranks.[j].[k]; symms.[j].[k]]
                            |> (withCommas >> stringCollapse "")
                        ) |> (withCommas >> stringCollapse "")
                    )

                List.init mloops.[i].Call.Length (fun j ->
                    let func = mloops.[i].funcs.[j]
                    let iLevels = ((iarrays |> List.map (fun x -> x.Rank)), func.IRank) ||> List.map2 (-)
                    let nInputDims = (iLevels |> List.sum)
                    let oExtentsName = String.concat "" [extentsName oarrays.[j]; "_"; oarrays.[j].Name]

                    let iExtentsLines =
                        List.init arity (fun k ->
                            let acc = if k = 0 then 0 else (iLevels.[0..(k-1)] |> List.sum)

                            List.init iLevels.[k] (fun l ->
                                [
                                    String.concat "" [oExtentsName; "["; string (acc+l); "] = "; extentsName iarrays.[k]; "["; string l; "];"]
                                ]
                            ) |> List.reduce (@)
                        ) |> List.reduce (@)

                    let oExtentsLines =
                        if func.ORank = 0 then [] else
                            let tDimExtents = func.TDimExtents
                            List.init func.ORank (fun k ->
                                String.concat "" [oExtentsName; "["; string (nInputDims+k); "] = "; string tDimExtents.[k] ]
                            )

                    [
                        "\n"
                        String.concat "" ["size_t* "; oExtentsName; " = new size_t["; string oarrays.[j].Rank; "];"]
                    ]
                    @ iExtentsLines
                    @ oExtentsLines
                    @ [
                        String.concat "" ["promote<"; oarrays.[j].Type; ", "; string oarrays.[j].Rank; ">::type "; oarrays.[j].Name; ";"]
                        String.concat "" [oarrays.[j].Name; " = allocate<typename promote<"; oarrays.[j].Type; ", "; string oarrays.[j].Rank; ">::type,"; oarrays.[j].Name; "_symm>("; oExtentsName;");"]
                        String.concat "" [names.[j]; "<"; tSpecTypes.[j]; ">"; "("; args.[j]; ");"]
                    ]
                    |> newln
                    |> stringCollapse ""
                )
            )

        let callSwaps = (oloopCallSwaps @ mloopCallSwaps) |> List.reduce (@) |> List.map tokenize

        let bounds = (FPositions, LPositions) ||> List.zip
        let rec swap (bounds: (int * int) list) (callSwaps: Token list list) (tokens: Token list) : Token list = 
            if tokens.IsEmpty then [] else
                match bounds, callSwaps with
                | [], [] -> tokens
                | boundsHead :: boundsTail, swapsHead :: swapsTail ->
                    let f, l = boundsHead
                    let swapped = tokens[0..f] @ swapsHead @ tokens[(l+1)..tokens.Length]
                    swap boundsTail swapsTail swapped
        let swap = swap bounds callSwaps

        let loopNames = (oloops |> List.map (fun x -> x.Name)) @ (mloops |> List.map (fun x -> x.Name))
        let deleteLoopLines = loopNames |> List.map (fun x -> List.filter (fun (y:string) -> not (y.Contains x))) |> List.reduce (>>)

        let addOSymmLines tokens =

            let rec addOSymmLines' tokens ind max =
                match tokens with
                | [] -> max
                | Token.Symbol '#' :: Token.Str "include" :: tail -> addOSymmLines' tail (ind+2) ind
                | Token.Symbol '#' :: Token.Str "define"  :: tail -> addOSymmLines' tail (ind+2) ind
                | Token.Str "using" :: tail -> addOSymmLines' tail (ind+1) ind
                | head :: tail -> addOSymmLines' tail (ind+1) max
            let last = addOSymmLines' tokens 0 -1
            let nextNewln = last + (tokens.[last..] |> List.findIndex (fun x -> x = Token.NewLine))

            let oSymmLines =
                (oloops |> List.map (fun x -> x.oarrays |> symmVecLines))
                @ (mloops |> List.map (fun x -> x.oarrays |> symmVecLines))
                |> List.reduce (@)
                |> List.distinct
                |> List.map tokenize
                |> List.reduce (@)

            tokens.[..nextNewln] @ oSymmLines @ tokens.[(nextNewln+1)..]

        tokens
        |> addOSymmLines
        |> (swap
            >> substitute
            >> (fun x -> "#include <omp.h>\n" :: x)
            >> (fun x -> "#include <string>\nusing std::string;\n" :: x)
            >> List.filter (fun x -> not (x.Contains "object_for"))
            >> List.filter (fun x -> not (x.Contains "method_for"))
            >> deleteLoopLines
            >> stringCollapse "")


    let compile iFileName oFileName =
        iFileName
        |> File.ReadAllText 
        |> tokenize
        |> parse
        |> fun x -> File.WriteAllText (oFileName, x)
