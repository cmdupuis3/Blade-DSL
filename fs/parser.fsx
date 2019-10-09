

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
    | Other of string

/// Tokenizer helper pattern for regexes
let (|Match|_|) pattern input =
    let m = Regex.Match (input, pattern)
    if m.Success then Some m.Value else None

/// Convert string to a token based on a regex pattern
let toToken = function
    | Match @"^\n|^\r"                 s -> s, Token.NewLine
    | Match @"^\s+"                    s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>|^;|^:|^~|^\*|^=" s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s -> s, Token.Str s
    | Match @"^\d+"                    s -> s, Token.Int (int s)
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
        | Token.Other  o -> string o
    )

let rec respace = function
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s1 :: Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s2 :: tail -> s1 :: " " :: (respace (s2 :: tail))
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


/// Container for array pragma information
type NestedArray =
    { Name: string
      Type: string
      Rank: int
      Symm:  int list Option}

/// Container for function pragma information
type NestedFunction =
    { Name:  string
      Arity: int Option
      INames: string list
      IRank: int list
      OName: string
      ORank: int
      Comm:   int list Option
      OmpLevels: int list Option
      Inner: string list }

/// Container for method_for loop information; able to recieve messages about array and function pragmas
type MethodLoop (nameIn: string, initIn: string list, callIn: (string * string * int) list, fposition: int, lposition: int)  =
    [<DefaultValue>] val mutable public iarrays: NestedArray list
    [<DefaultValue>] val mutable public oarrays: NestedArray list
    [<DefaultValue>] val mutable public funcs: NestedFunction list

    member this.Name = nameIn
    member this.Init = initIn // iarrays
    member this.Call = callIn // oarray, func, and call position
    member this.FPosition = fposition // position of the first token of the init pattern
    member this.LPosition = lposition // position of the last token of the init pattern

    member this.PushIarray (v: NestedArray) = this.iarrays <- this.iarrays @ [v]
    member this.PushOarray (v: NestedArray) = this.oarrays <- this.oarrays @ [v]
    member this.PushFunc (v: NestedFunction) = this.funcs <- this.funcs @ [v]

/// Container for object_for loop information; able to recieve messages about array and function pragmas
type ObjectLoop (nameIn: string, initIn: string, callIn: ((string list) * string * int) list, fposition: int, lposition: int) =
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

/// Inserts a tab character at the beginning of each string in the input list.
let tab x =
    x |> List.map (fun y -> String.concat "" ["\t"; y])

/// Inserts a tab character at the end of each string in the input list.
let newln x =
    x |> List.map (fun y -> String.concat "" [y; "\n"])

/// Creates a two-element list of the input string plus '{', and '}'.
let brace x =
    [String.concat " " [x; "{\n"]] @ ["}\n"]

/// Module containing all the logic for autogenerating nested_for loops of variable arity.
/// Contains optimizations for symmetry and commutativity.
module NestedLoop =

    /// Generates a list of new iterator names for multiple variables based on a minumum and the ranks of variables to loop over.
    /// <param name="min"> First integer to append to "__i". </param>
    /// <param name="ranks"> List of variable ranks. </param>
    let rec private indNames min ranks =
        let rec indNames' min max =
            List.init (max - min) (fun index -> String.concat "" ["__i"; (string (index + min))])
        match ranks with
        | []           -> []
        | head :: tail -> indNames' min (min+head) :: indNames (min+head) tail

    /// Finds which iterators should serve as a the minimum for a given loop if the variables are commutative; otherwise set the minimum to 0.
    /// <param name="comGroups"> A commutativity vector. </param>
    /// <param name="iNames"> Iterator names for all variables. </param>
    let rec private comImins (comGroups: int list) (iNames: string list list) =
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
    let private symImins (symGroups: int list) (iNames: string list) =
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
    let rec private vStates (arrayNames: string list) (symGroups: int list list) (comGroups: int list) =
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
    let private iminList (arrayNames: string list) (symGroups: int list list) (comGroups: int list) =
        assert (arrayNames.Length = symGroups.Length)
        assert (arrayNames.Length = comGroups.Length)

        let ranks = symGroups |> List.map (fun x -> x.Length)
        let indexNames = indNames 0 ranks
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
    let private unaryLoop (iarrayName: string) (iarrayLevels: int) (oarrayName: string) (oarrayLevels: int) (indNames: string list) (iMins: string list) (inner: string list) (ompLevels: int) =
        assert (iarrayLevels = indNames.Length)
        assert (iarrayLevels = iMins.Length)

        List.init iarrayLevels (
            fun i ->
                let ompline = if ompLevels > i then [ompLine indNames.[i]] else []
                let braced = match i with
                             | 0 -> brace (loopLine indNames.[i] iMins.[i] iarrayName)
                             | _ -> brace (loopLine indNames.[i] iMins.[i] (String.concat "" [iarrayName; indNames.[i-1]]))
                let iline =
                    if i = 0 then                     tab [String.concat "" ["auto "; iarrayName; indNames.[i]; " = "; index iarrayName indNames.[i]; ";\n"]]
                    else                              tab [String.concat "" ["auto "; iarrayName; indNames.[i]; " = "; (index (String.concat "" [iarrayName; indNames.[i-1]]) indNames.[i]); ";\n"]]
                let oline =
                    if i = 0 && i < oarrayLevels then tab [String.concat "" ["auto "; oarrayName; indNames.[i]; " = "; index oarrayName indNames.[i]; ";\n"]]
                    else if i < oarrayLevels then     tab [String.concat "" ["auto "; oarrayName; indNames.[i]; " = "; (index (String.concat "" [oarrayName; indNames.[i-1]]) indNames.[i]); ";\n"]]
                    else                              []
                fun x -> List.concat [ newln [declLine "int" indNames.[i]]; newln ompline; [braced.[0]]; iline; oline; tab x; [braced.[1]] ]
        )
        |> List.rev
        |> List.fold (fun acc elem -> elem acc) inner

    /// Autogenerate an N-ary nested_for loop.
    /// <param name="arrayNames"> Variable names. </param>
    /// <param name="indNames"> Iterator minimum names. </param>
    /// <param name="iMins"> Iterator minimum names. </param>
    /// <param name="inner"> "Inner" block, i.e., code to place inside all the loops. </param>
    /// <param name="ompLevels"> Number of OpenMP levels. </param>
    let private naryLoop (iarrayNames: string list) (iarrayLevels: int list) (oarrayName: string) (oarrayLevels: int) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) =
        assert (iarrayNames.Length = indNames.Length)
        assert (iarrayNames.Length = iMins.Length)
        assert (iarrayNames.Length = ompLevels.Length)

        let rec naryLoop' (iarrayNames: string list) (iarrayLevels: int list) (oarrayName: string) (oarrayLevels: int) (indNames: string list list) (iMins: string list list) (inner: string list) (ompLevels: int list) =
            match iarrayNames with
            | [] -> failwith "Empty array names list."
            | _  ->
                match iarrayNames with
                | [] -> failwith "Impossible match."
                | [head]       -> [(fun i -> unaryLoop iarrayNames.Head iarrayLevels.Head oarrayName oarrayLevels indNames.Head iMins.Head i ompLevels.Head)]
                | head :: tail ->  (fun i -> unaryLoop iarrayNames.Head iarrayLevels.Head oarrayName oarrayLevels indNames.Head iMins.Head i ompLevels.Head) ::
                                             naryLoop' iarrayNames.Tail iarrayLevels.Tail oarrayName (oarrayLevels - iarrayLevels.Head) indNames.Tail iMins.Tail inner ompLevels.Tail

        naryLoop' iarrayNames iarrayLevels oarrayName oarrayLevels indNames iMins inner ompLevels
        |> List.rev
        |> List.fold (fun acc elem -> elem acc) inner

    /// Find the final array names for a list of variables; useful when substituting into inner blocks.
    /// <param name="iarrays"> A list of input array classes. </param>
    /// <param name="oarray"> An output array class. </param>
    /// <param name="func"> A function class. </param>
    let private lastArrayNames (iarrays: NestedArray list) (oarray: NestedArray) (func: NestedFunction) =
        let ilevels = ((iarrays |> List.map (fun x -> x.Rank)), func.IRank) ||> List.map2 (-)
        let inds = indNames 0 ilevels
        let lastInds = List.map List.last inds
        let iNames =
            List.init iarrays.Length (
                fun i ->
                    if iarrays.[i].Rank = func.IRank.[i] then "" else lastInds.[i]
                    |> fun x -> String.concat "" [func.INames.[i]; x]
            )

        let rec getOName (inds: string list list) (ctr: int) =
            let rec getOName' (inds: string list list) (ctr: int) =
                if ctr = 0 then
                    String.concat "" [func.OName; inds.[0].[0]]
                else
                    if inds.Head.Tail.IsEmpty then
                        getOName' (inds.Tail) (ctr - 1)
                    else
                        getOName' (inds.Head.Tail :: inds.Tail) (ctr - 1)
            if ctr = 0 then oarray.Name else getOName' inds (ctr-1)
        let oName = getOName inds (oarray.Rank - func.ORank)

        iNames, oName

    /// Substitute the first string with the second in a list of strings
    /// <param name="subs"> A list of substitution pairs; find the first, swap the second in. </param>
    /// <param name="text"> The text to be substituted. </param>
    let rec private subInner (subs: (string * string) list) (inner: string list) =
        match subs with
        | []           -> inner
        | head :: tail -> List.init inner.Length (fun i -> inner.[i].Replace(fst head, snd head)) |> subInner tail

    /// Autogenerate a unary nested_for loop.
    /// <param name="iarray"> An input array class. </param>
    /// <param name="oarray"> An output array class. </param>
    /// <param name="func"> A function class. </param>
    let Unary (iarray: NestedArray) (oarray: NestedArray) (func: NestedFunction) =
        let ilevels = iarray.Rank - func.IRank.Head
        let imins = (iminList [iarray.Name] [iarray.Symm |> function | Some s -> s | None -> (List.init iarray.Rank id)] [1]).Head

        let lastINames, lastOName = lastArrayNames [iarray] oarray func
        let subINames = (func.INames.Head, lastINames.Head)
        let subOName = (func.OName, lastOName)
        let subbedInner = func.Inner |> subInner (subINames :: [subOName])

        let ompLevels = match func.OmpLevels with | Some omp -> omp.Head | None -> 0
        let ret = unaryLoop func.INames.Head ilevels func.OName (oarray.Rank - func.ORank) (indNames 0 [iarray.Rank - func.IRank.Head]).Head imins subbedInner ompLevels
        ret, lastArrayNames [iarray] oarray func

    /// Autogenerate an N-ary nested_for loop.
    /// <param name="iarrays"> A list of input array classes. </param>
    /// <param name="oarray"> An output array class. </param>
    /// <param name="func"> A function class. </param>
    let Nary (iarrays: NestedArray list) (oarray: NestedArray) (func: NestedFunction) =
        let ilevels = (iarrays |> List.map (fun x -> x.Rank), func.IRank) ||> List.map2 (-)
        let comm = match func.Comm with | Some comm -> comm | None -> (List.init iarrays.Length id)
        let imins = iminList (iarrays |> List.map (fun x -> x.Name)) (iarrays |> List.map (fun x -> x.Symm |> function | Some s -> s | None -> (List.init x.Rank id))) comm

        let lastINames, lastOName = lastArrayNames iarrays oarray func
        let subINames = List.zip func.INames lastINames
        let subOName = (func.OName, lastOName)
        let subbedInner = func.Inner |> subInner (subINames @ [subOName])

        let ompLevels = match func.OmpLevels with | Some omp -> omp | None -> (List.init iarrays.Length (fun i -> 0))
        let ret = naryLoop func.INames ilevels func.OName (oarray.Rank - func.ORank) (indNames 0 ilevels) imins subbedInner ompLevels
        ret, lastArrayNames iarrays oarray func

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
            | _ -> failwith "asdfsdf"
        let scope, t = toScope tail 0
        Some (scope, (t: Token list))
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
            | _ -> failwith "asdfsdf"
        let scope, t = toScope tail 0
        Some (head :: Token.Symbol '(' :: (scope @ [Token.Symbol ')']), (t: Token list))
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
            | _ -> failwith "asdfsdf"
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

/// Pattern for method_for loops and all their calls
let rec (|MethodLoopPattern|_|) (position: int) = function
    | loop :: Token.Symbol '=' :: Token.Str "method_for" :: Token.Symbol '(' :: tail ->
        let elements, tokens = toElements tail
        let iarrays = tokenToStr elements
        let rec findCalls tokens' position' =
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail' when lname = loop -> (func, oarray, position') :: findCalls tail' (position' + 5)
            | head' :: tail' -> findCalls tail' (position' + 1)
            | [] -> []
        let lposition = position + 3 + (2*elements.Length)
        let calls =
            match tokens with
            | PostScopePattern tokens' -> findCalls (fst tokens') lposition
            | _ -> []
        let out = MethodLoop((tokenToStr [loop]).Head, iarrays, calls, position, lposition)
        out.iarrays <- []
        out.oarrays <- []
        out.funcs <- []
        Some (out, tokens)
(* for debugging:
    | head :: tail ->
        let nextPosition = position + 1
        match tail with
        | MethodLoopPattern nextPosition t -> Some (t)
        | _ -> None
*)
    | _ -> None

/// Scan code for all the method_for loops and return a list of them.
let scanMethodLoops (tokens: Token list) =
    let rec scan (position: int) = function
    | MethodLoopPattern position (loop,[]) -> [loop]
    | MethodLoopPattern position (loop, tail) -> loop :: (scan loop.LPosition tail)
    | head :: tail -> scan (position+1) tail
    | [] -> []
    scan 0 tokens

//let a = match tokenize code with | MethodLoopPattern 0 (s) -> Some(s) | _ -> None;;
//let mystr = "auto mloop = method_for(array1, array1, array3); auto ooarray = oloop(array1, array1, array3); auto moarray = mloop(sumThenMultiply);"
//let b = match tokenize mystr with | MethodLoopPattern 0 (s) -> Some(s) | _ -> None;;

/// Pattern for object_for loops and all their calls
let rec (|ObjectLoopPattern|_|) (position: int) = function
    | loop :: Token.Symbol '=' :: Token.Str "object_for" :: Token.Symbol '(' :: Token.Str func :: Token.Symbol ')' :: tail ->
        let rec findCalls tokens' position' =
            match tokens' with
            | Token.Str oarray :: Token.Symbol '=' :: lname :: Token.Symbol '(' :: tail' when lname = loop ->
                let iarrays, t = toElements tail'
                (tokenToStr iarrays, oarray, position') :: findCalls tail' (position + 3 + (2*iarrays.Length))
            | head' :: tail' -> findCalls tail' (position' + 1)
            | [] -> []
        let lposition = position + 5
        let calls =
            match tail with
            | PostScopePattern t' -> findCalls (fst t') lposition
            | _ -> []
        let out = ObjectLoop((tokenToStr [loop]).Head, string func, calls, position, lposition)
        out.iarrays <- []
        out.oarrays <- []
        out.func <- []
        Some (out, tail)
(* for debugging:
    | head :: tail ->
        let nextPosition = position + 1
        match tail with
        | ObjectLoopPattern nextPosition t -> Some (t)
        | _ -> None
*)
    | _ -> None

/// Scan code for all the method_for loops and return a list of them.
let scanObjectLoops (tokens: Token list) =
    let rec scan (position: int) = function
    | ObjectLoopPattern position (loop,[]) -> [loop]
    | ObjectLoopPattern position (loop, tail) -> loop :: (scan loop.LPosition tail)
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

let (|ArrayPattern|_|) (symGroups: int list Option) = function
    | Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {Name = name; Type = valtype; Rank = rank; Symm = symGroups}, tail )
    | Token.Str "promote" :: Token.Symbol '<' :: Token.Str valtype :: Token.Symbol ',' :: Token.Int rank :: Token.Symbol '>' :: Token.Symbol ':' :: Token.Symbol ':' :: Token.Str "type" :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {Name = name; Type = valtype; Rank = rank; Symm = symGroups}, tail )
    | _ -> None

let getArray (clauses: Clause list) (block: Token list) =
    let hasSym = clauses |> List.exists (fst >> (function | "symmetry" -> true | _ -> false))
    let sym = if hasSym then
                  Some ((clauses |> List.find (fst >> (function | "symmetry" -> true | _ -> false))) |> (snd >> tokenToInt))
              else None
    match block with
    | ArrayPattern sym s -> fst s
    | _ -> failwith "Array pragma applied to invalid array declaration."

let getFunction (name: string) (clauses: Clause list) (block: Token list) =
    let arity =
        clauses |> List.find (fst >> (function | "arity" -> true | _ -> false)) |> snd
        |> fun x ->
            match x with
            | [Token.Str "any"] -> None
            | _ -> Some (x |> (tokenToInt >> List.head))

    let input  = (clauses |> List.find (fst >> (function | "input" -> true | _ -> false))) |> (snd >> tokenToStr)
    let output = (clauses |> List.find (fst >> (function | "output" -> true | _ -> false))) |> (snd >> tokenToStr >> List.head)
    let iranks = (clauses |> List.find (fst >> (function | "iranks" -> true | _ -> false))) |> (snd >> tokenToInt)
    let orank  = (clauses |> List.find (fst >> (function | "orank" -> true | _ -> false))) |> (snd >> tokenToInt >> List.head)

    let hasOmp = clauses |> List.exists (fst >> (function | "ompLevels" -> true | _ -> false))
    let ompLevels =
        if hasOmp then
            Some ((clauses |> List.find (fst >> (function | "ompLevels" -> true | _ -> false))) |> (snd >> tokenToInt))
        else None

    let hasCom = clauses |> List.exists (fst >> (function | "commutativity" -> true | _ -> false))
    let com =
        if hasCom then
            Some ((clauses |> List.find (fst >> (function | "commutativity" -> true | _ -> false))) |> (snd >> tokenToInt))
        else None

    { Name = name;
      Arity = arity;
      INames = input;
      IRank = iranks;
      OName = output;
      ORank = orank;
      Comm = com;
      OmpLevels = ompLevels;
      Inner = block |> deleteReturnLine |> tokenToStr |> respace |> reconcat |> newln }

let sortPragmas (pragmas: Pragma list) =
    let bin (s: string) =
        List.filter (fun x ->
                        let directive, clauses, scope = x
                        fst directive = s
                    ) pragmas
    bin "array", bin "function"

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
                if snd3 mloops.[i].Call.[k] = arrays.[j].Name then
                    mloops.[i].PushOarray arrays.[j]
    for i in 0..oloops.Length-1 do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..oloops.[i].Call.Length-1 do
            let mutable itemp = []
            for l in 0..(fst3 oloops.[i].Call.[j]).Length-1 do
                for k in 0..arrays.Length-1 do
                    if (fst3 oloops.[i].Call.[j]).[l] = arrays.[k].Name then
                        itemp <- itemp @ [arrays.[k]]
            oloops.[i].PushIarrays itemp
            for k in 0..arrays.Length-1 do
                if snd3 oloops.[i].Call.[j] = arrays.[k].Name then
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
let commas (x: string list) =
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
        String.concat "" ["static constexpr const int["; symm.Length |> string; "] "; name; "_symm = {"; symm |> List.map string |> commas |> stringCollapse " "; "};\n"]
    )

let symmVecName (array: NestedArray) =
    if array.Symm.IsNone then "nullptr" else String.concat "" [array.Name; "_symm"]

let objectLoopTemplate (oloop: ObjectLoop) =


    let numCalls = oloop.Call.Length
    let itype = oloop.iarrays |> List.map (fun x -> x |> List.map (fun y -> y.Type))
    let otype = oloop.oarrays |> List.map (fun x -> x.Type)
    let irank = oloop.iarrays |> List.map (fun x -> x |> List.map (fun y -> y.Rank))
    let orank = oloop.oarrays |> List.map (fun x -> x.Rank)
    let isymm = oloop.iarrays |> List.map (fun x -> x |> List.map symmVecName)
    let osymm = oloop.oarrays |> List.map symmVecName

    let arity, inames =
        match oloop.GetFunc.Arity with
        | Some a -> // fixed arity => specify all argument names
            List.init numCalls (fun i -> a), List.init numCalls (fun i -> oloop.GetFunc.INames)
        | None ->  // variable arity => specify one argument name and template it
            let arity' = oloop.iarrays |> List.map (fun x -> x.Length)
            arity', List.init numCalls (fun i -> List.init arity'.[i] (fun j -> String.concat "" [oloop.GetFunc.INames.Head; "_"; string j]))

    let onames =  List.init numCalls (fun i -> oloop.GetFunc.OName)

    let templateTypes =
        arity |> List.map (fun a ->
            List.init a (fun i -> String.concat "" ["ITYPE"; string (i+1); ", IRANK"; string (i+1); ", ISYM"; string (i+1)])
            |> fun x -> x @ ["OTYPE, ORANK, OSYM"]
            |> commas
            |> stringCollapse ""
        ) |> List.distinct

    let templateArgTypes =
        (arity, inames) ||> List.map2 (fun a (names: string list) ->
            List.init a (fun i -> String.concat "" ["nested_array<ITYPE"; string (i+1); ", IRANK"; string (i+1); ", ISYM"; string (i+1); "> "; names.[i]])
            |> commas
            |> fun x -> x @ [", nested_array<OTYPE, ORANK, OSYM> "; oloop.GetFunc.OName]
            |> stringCollapse ""
        ) |> List.distinct

    let tmain = List.init numCalls (fun i -> String.concat "" ["template<"; templateTypes.[i]; "> void "; oloop.Name; "("; templateArgTypes.[i]; "){\n\t// Nothing to see here.\n}"]) |> List.distinct

    let ranks = (irank, orank) ||> List.map2 (fun x y -> (x |> List.map string) @ [string y])
    let types = (itype, otype) ||> List.map2 (fun x y -> x @ [y])
    let symms = (isymm, osymm) ||> List.map2 (fun x y -> x @ [y])
    let names = (inames, onames) ||> List.map2 (fun x y -> x @ [y])
    let tSpecTypes = (types, ranks) ||> List.map2 (List.fold2 (fun acc elem1 elem2 -> acc @ (elem1 :: [elem2])) [])
                                     |> List.map (commas >> (fun x -> String.concat "" x))

    let tSpecArgs = (types, ranks) ||> (fun x y -> List.init numCalls (fun i -> List.init (arity.[i]+1) (fun j -> String.concat "" ["nested_array<"; x.[i].[j]; ", "; y.[i].[j]; "> "; names.[i].[j]])))
                                    |> List.map (commas >> stringCollapse "")

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
            | _ -> h |> tokenToStr |> respace |> reconcat |> newln |> List.head
        | _ -> tokens |> tokenToStr |> respace |> reconcat |> newln |> List.head

    let tSpecInner =
        match oloop.GetFunc.Arity with
        | Some a -> // fixed arity => specify all argument names
            List.init numCalls (fun i -> NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] oloop.GetFunc |> fst)
        | None ->
            let funcs = List.init numCalls (fun i ->
                { Name = oloop.GetFunc.Name;
                  Arity = Some(arity.[i]);
                  INames = inames.[i];
                  IRank = List.init arity.[i] (fun j -> oloop.GetFunc.IRank.Head);
                  OName = oloop.GetFunc.OName;
                  ORank = oloop.GetFunc.ORank;
                  Comm = oloop.GetFunc.Comm;
                  OmpLevels = oloop.GetFunc.OmpLevels;
                  Inner = oloop.GetFunc.Inner |> List.map (tokenize >> deleteReturnLine >> (expandVariadic i)) }
            )
            List.init numCalls (fun i -> NestedLoop.Nary oloop.iarrays.[i] oloop.oarrays.[i] funcs.[i] |> fst)

    (tSpecTypes, tSpecArgs)
    ||> List.map2 (fun x y -> String.concat "" ["template<> void "; oloop.Name; "<"; x; ">("; y; ")"])
    |> List.map brace
    |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> List.concat [[y.Head]; tab z; y.Tail]))
    |> List.map (fun x -> x |> stringCollapse "")
    |> fun x -> List.concat [tmain; x]
    |> stringCollapse "\n\n"


let methodLoopTemplate (mloop: MethodLoop) =
    let arity = mloop.funcs |> List.map (fun x -> x.Arity)
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

    let templateTypes =
        List.init arity (fun i -> String.concat "" ["ITYPE"; string (i+1); ", IRANK"; string (i+1); ", ISYM"; string (i+1)])
        |> fun x -> x @ ["OTYPE, ORANK, OSYM"]
        |> commas
        |> stringCollapse ""

    let templateArgTypes =
        List.init numCalls (fun i ->
            List.init arity (fun j -> String.concat "" ["nested_array<ITYPE"; string (j+1); ", IRANK"; string (j+1); ", ISYM"; string (j+1); "> "; inames.[i].[j]])
            |> commas
            |> fun x -> x @ [", nested_array<OTYPE, ORANK, OSYM> "; onames.[i]]
            |> stringCollapse ""
        ) |> List.distinct

    let tmain = List.init numCalls (fun i -> String.concat "" ["template<"; templateTypes; "> void "; mloop.funcs.[i].Name; "("; templateArgTypes.[i]; "){\n\t// Nothing to see here.\n}"]) |> List.distinct

    let ranks = orank |> List.map (fun y -> (irank |> List.map string) @ [string y])
    let types = otype |> List.map (fun y -> itype @ [y])
    let symms = osymm |> List.map (fun y -> isymm @ [y])
    let names = (inames, onames) ||> List.map2 (fun x y -> x @ [y])

    let tSpecTypes =
        List.init numCalls (fun i ->
            List.init (arity+1) (fun j ->
                [types.[i].[j]; ranks.[i].[j]; symms.[i].[j]]
                |> commas
                |> stringCollapse ""
            ) |> (commas >> stringCollapse "")
        )

    let tSpecArgs =
        List.init numCalls (fun i ->
            List.init (arity+1) (fun j ->
                (types.[i].[j], ranks.[i].[j], symms.[i].[j])
                |||> fun x y z ->
                    if z = "nullptr" then
                        ["nested_array<"; x; ", "; y; ", nullptr> "; names.[i].[j]]
                    else
                        ["nested_array<"; x; ", "; y; ", "; z; "> "; names.[i].[j]]
                |> stringCollapse ""
            ) |> (commas >> stringCollapse "")
        )


    let tSpecInner = List.init numCalls (fun i -> NestedLoop.Nary mloop.iarrays mloop.oarrays.[i] mloop.funcs.[i] |> fst)

    List.init numCalls (fun i ->
            (tSpecTypes.[i], tSpecArgs.[i], mloop.funcs.[i].Name)
            |||> fun x y z ->
                ["template<> void "; z; "<"; x; ">("; y; ")"]
            |> stringCollapse ""
    )
    |> List.map brace
    |> (fun x -> (x, tSpecInner) ||> List.map2 (fun y z -> List.concat [[y.Head]; tab z; y.Tail]))
    |> List.map (fun x -> x |> stringCollapse "")
    |> fun x -> tmain @ x
    |> stringCollapse "\n\n"

let lex (tokens: Token list) =

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
        List.concat [symmVecs;
                     oloops |> List.map objectLoopTemplate;
                     mloops |> List.map methodLoopTemplate]
        |> stringCollapse ""





    []



let code = """

#include "things.hpp"
#include "stuff.hpp"
#pragma edgi function(sumThenMultiply) arity(3) input(iarray1, iarray2, iarray3) iranks(1, 1, 0) commutativity(1, 1, 3) output(oarray) orank(0)
auto sumThenMultiply = function(iarray1, iarray2, iarray3, oarray){
    // assume iarray1 and iarray2 last extents are same
    for(int i = 0; i < iarray1.current_extent(); i++){
        oarray += iarray1[i] + iarray2[i];
    }

    oarray *= iarray3;
    return oarray;
}
#pragma edgi function(divideThenSum) arity(3) input(iarray4, iarray5, iarray6) iranks(1, 1, 0) output(oarray) orank(0)
auto sumThenMultiply = function(iarray4, iarray5, iarray6, oarray){
    // assume iarray4 and iarray5 last extents are same
    for(int i = 0; i < iarray4.current_extent(); i++){
        oarray += iarray4[i] / iarray5[i];
    }

    oarray += iarray6;
    return oarray;
}
#pragma edgi function(add10) arity(1) input(iarray) iranks(0) output(oarray) orank(0)
{
    oarray = iarray + 10;
    return oarray;
}
#pragma edgi function(sinThenProduct) arity(any) input(iarray) iranks(0) output(oarray) orank(0)
{
    oarray = sin(iarray) * tail;
    return oarray;
}
int main(){

    #pragma edgi array symmetry(1, 2, 2, 3)
    promote<float, 4>::type array1;

    #pragma edgi array
    promote<float, 3>::type array3;

    #pragma edgi array
    promote<float, 3>::type ooarray;

    #pragma edgi array
    promote<float, 3>::type moarray;

    #pragma edgi array
    promote<float, 3>::type moarray2;

    #pragma edgi array
    promote<float, 3>::type voarray;

    auto oloop = object_for(sumThenMultiply);
    auto ooarray = oloop(array1, array1, array3);

    auto mloop = method_for(array1, array1, array3);
    auto moarray = mloop(sumThenMultiply);
    auto moarray2 = mloop(divideThenSum);

    auto oloopv = object_for(sinThenProduct);
    auto voarray = oloopv(array3, array3);




    auto newfunc = pipe(sumThenMultiply, add10);

    return 0;
}"""

let tokens = tokenize code;;
