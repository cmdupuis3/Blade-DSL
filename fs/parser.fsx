

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
    | Match @"^\n|^\r"               s -> s, Token.NewLine
    | Match @"^\s+"                  s -> s, Token.WhiteSpace
    | Match @"^\{|^\}|^\(|^\)|^\[|^\]|^,|^\#|^\<|^\>|^;|^:|^~|^\*|^="    s -> s, Token.Symbol s.[0]
    | Match @"^[a-zA-Z_][a-zA-Z0-9_]*" s -> s, Token.Str s
    | Match @"^\d+"                  s -> s, Token.Int (int s)
    | Match @"."                     s -> s, Token.Other (string s)
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
    let unquote (tokens': string) = tokens'.Substring(1, tokens'.Length-2)
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

/// Container for array pragma information
type NestedArray = 
    { arrName: string
      arrType: string
      arrRank: int 
      arrSym:  int list }

/// Container for function pragma information
type NestedFunction =
    { funcName:  string
      funcArity: int
      funcINames: string list
      funcIRank: int list
      funcOName: string
      funcORank: int
      funcCom:   int list
      funcOmpLevels: int list
      funcBlock: Token list }

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

    member this.PushIarray (v: NestedArray) = this.iarrays <- List.append this.iarrays [v]
    member this.PushOarray (v: NestedArray) = this.oarrays <- List.append this.oarrays [v]
    member this.PushFunc (v: NestedFunction) = this.funcs <- List.append this.funcs [v]

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

    member this.PushIarrays (v: NestedArray list) = this.iarrays <- List.append this.iarrays [v]
    member this.PushOarray (v: NestedArray) = this.oarrays <- List.append this.oarrays [v]
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

    /// Find the final array names for a list of variables; useful when substituting into inner blocks.
    /// <param name="arrayNames"> Variable names. </param>
    /// <param name="symGroups"> A list of symmetry vectors. </param>
    let private LastArrayNames (arrayNames: string list) (symGroups: string list list) =
        let indNames = indNames2 0 (rankList symGroups)
        List.init arrayNames.Length (
            fun i ->
                match symGroups.[i] with
                | [] -> failwith "Empty index names list."
                | [head] -> head
                | head :: tail -> List.last indNames.[i]
                |> fun x -> String.concat "" [arrayNames.[i]; x]
        )

    let Unary (array: NestedArray) (func: NestedFunction) = 
        let Unary' (arrayName: string) (symGroups: string list) (inner: string list) (ompLevels: int) =
            let ret = unaryLoop arrayName (indNames2 0 (rankList [symGroups])).Head (iminList [arrayName] [symGroups] ["1"]).Head inner ompLevels
            ret, LastArrayNames [arrayName] [symGroups]
        Unary' array.arrName (array.arrSym |> List.map string) (func.funcBlock |> tokenToStr) func.funcOmpLevels.Head

    let Nary (arrays: NestedArray list) (func: NestedFunction) = 
        let Nary' (arrayNames: string list) (symGroups: string list list) (comGroups: string list) (inner: string list) (ompLevels: int list) =
            let ret = naryLoop arrayNames (indNames2 0 (rankList symGroups)) (iminList arrayNames symGroups comGroups) inner ompLevels
            ret, LastArrayNames arrayNames symGroups
        Nary' (arrays |> List.map (fun x -> x.arrName)) (arrays |> List.map (fun x -> x.arrSym |> List.map string)) (func.funcCom |> List.map string) [(func.funcBlock |> tokenToStr |> respace |> String.concat "")] func.funcOmpLevels
(*
let rec reconcat = function
    | head :: Match @"^\n|^\r" mid :: tail-> String.concat "" (head :: [mid]) :: (reconcat tail)
    | head :: tail -> 
        let asdf = reconcat tail
        String.concat "" (head :: reconcat tail)
*)

/// Pragma clause type; a tuple of the clause name and a list of arguments
type Clause = string * Token list

/// Pragma type; consists of a directive, a list of clauses, and a scope
type Pragma = Clause * Clause list * Token list

/// Delete the "return" line of a function (needed for function expansion)
let rec deleteReturnLine = function
    | Token.Str "return" :: tail ->
        let rec aux = function
            | Token.Symbol ';' :: tail' -> Some tail'
            | head' :: tail'-> aux tail'
            | _ -> None
        aux tail
    | head :: tail -> Some (head :: tail)
    | _ -> None

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
(*
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
(*
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


let (|ArraySymmetryPattern|_|) = function
    | "symmetry", (vals: Token list) -> Some (vals |> tokenToInt)
    | _ -> failwith "Invalid array clause."

let getSymmetry (rank: int) (symGroups: int list) = 
    if symGroups.IsEmpty then 
        List.init rank id
    else
        if symGroups.Length = rank then
            symGroups
        else
            failwith "Symmetry vector length did not match the rank of the array."

let (|ArrayPattern|_|) (symGroups: int list) = function
    | Token.Str valtype :: Token.Symbol '~' :: Token.Int rank :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = getSymmetry rank symGroups}, tail )
    | Token.Str "promote" :: Token.Symbol '<' :: Token.Str valtype :: Token.Symbol ',' :: Token.Int rank :: Token.Symbol '>' :: Token.Symbol ':' :: Token.Symbol ':' :: Token.Str "type" :: Token.Str name :: Token.Symbol ';' :: tail ->
        Some ( {arrName = name; arrType = valtype; arrRank = rank; arrSym = getSymmetry rank symGroups}, tail )
    | _ -> None

let getArray (clauses: Clause list) (block: Token list) =
    let hasSym = clauses |> List.exists (function | ArraySymmetryPattern s -> true | _ -> false)
    let sym = if hasSym then
                  clauses |> List.pick (function | ArraySymmetryPattern s -> Some (s) | _ -> None)
              else []
    match block with
    | ArrayPattern sym s -> fst s
    | _ -> failwith "Array pragma applied to invalid array declaration."


let getFunction (name: string) (clauses: Clause list) (block: Token list) =
    let arity  = (clauses |> List.find (fst >> (function | "arity" -> true | _ -> false))) |> (snd >> tokenToInt >> List.head)
    let input  = (clauses |> List.find (fst >> (function | "input" -> true | _ -> false))) |> (snd >> tokenToStr)
    let output = (clauses |> List.find (fst >> (function | "output" -> true | _ -> false))) |> (snd >> tokenToStr >> List.head)
    let iranks = (clauses |> List.find (fst >> (function | "iranks" -> true | _ -> false))) |> (snd >> tokenToInt)
    let orank  = (clauses |> List.find (fst >> (function | "orank" -> true | _ -> false))) |> (snd >> tokenToInt >> List.head)

    let hasOmp = clauses |> List.exists (fst >> (function | "ompLevels" -> true | _ -> false))
    let ompLevels = 
        if hasOmp then
            (clauses |> List.find (fst >> (function | "ompLevels" -> true | _ -> false))) |> (snd >> tokenToInt)
        else List.init arity (fun x -> 0)

    let hasCom = clauses |> List.exists (fst >> (function | "commutativity" -> true | _ -> false))
    let com = 
        if hasCom then
            (clauses |> List.find (fst >> (function | "commutativity" -> true | _ -> false))) |> (snd >> tokenToInt)
        else List.init arity id

    { funcName = name;
      funcArity = arity;
      funcINames = input;
      funcIRank = iranks;
      funcOName = output;
      funcORank = orank;
      funcCom = com;
      funcOmpLevels = ompLevels;
      funcBlock = block |> deleteReturnLine |> Option.get }

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
                if mloops.[i].Init.[k] = arrays.[j].arrName then
                    mloops.[i].PushIarray arrays.[j]
            for k in 0..mloops.[i].Call.Length-1 do
                if snd3 mloops.[i].Call.[k] = arrays.[j].arrName then
                    mloops.[i].PushOarray arrays.[j]
    for i in 0..oloops.Length-1 do
        // search for iarray names in arrays list and copy info to loop objects (results must be in order!)
        for j in 0..oloops.[i].Call.Length-1 do
            let mutable itemp = []
            for l in 0..(fst3 oloops.[i].Call.[j]).Length-1 do
                for k in 0..arrays.Length-1 do
                    if (fst3 oloops.[i].Call.[j]).[l] = arrays.[k].arrName then
                        itemp <- List.append itemp [arrays.[k]]
            oloops.[i].PushIarrays itemp
            for k in 0..arrays.Length-1 do
                if snd3 oloops.[i].Call.[j] = arrays.[k].arrName then
                    oloops.[i].PushOarray arrays.[k]

/// Message-passing function for sending info about function pragmas to nested loop objects
let sendFunctionsToLoops (funcs: NestedFunction list) (mloops: MethodLoop list) (oloops: ObjectLoop list) =
    // search for function names in funcs list and copy info to loop objects (results must be in order!)
    for i in 0..mloops.Length-1 do
        for j in 0..funcs.Length-1 do
            for k in 0..mloops.[i].Call.Length-1 do
                if fst3 mloops.[i].Call.[k] = funcs.[j].funcName then
                    mloops.[i].PushFunc funcs.[j]
    for i in 0..oloops.Length-1 do
        for j in 0..funcs.Length-1 do
            if oloops.[i].Init = funcs.[j].funcName then
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
    |> fun y -> List.append y [List.last x]

let objectLoopTemplate (oloop: ObjectLoop) =
    let arity = oloop.func.Head.funcArity

    let firank = oloop.func.Head.funcIRank
    let forank = oloop.func.Head.funcORank

    let numCalls = oloop.Call.Length
    let irank = oloop.iarrays |> List.map (fun x -> x |> List.map (fun y -> y.arrRank))
    let orank = oloop.oarrays |> List.map (fun x -> x.arrRank)
    let itype = oloop.iarrays |> List.map (fun x -> x |> List.map (fun y -> y.arrType))
    let otype = oloop.oarrays |> List.map (fun x -> x.arrType)

    let tTypes = 
        List.init arity (fun i -> String.concat "" ["ITYPE"; string (i+1); ", IRANK"; string (i+1)])
        |> fun x -> List.append x ["OTYPE, ORANK"]
        |> commas
        |> List.fold (fun acc elem -> String.concat "" [acc; elem]) ""

    let argTypes = 
        List.init arity (fun i -> String.concat "" ["nested_array<"; "ITYPE"; string (i+1); ", IRANK"; string (i+1); ">"])
        |> fun x -> List.append x ["nested_array<OTYPE, ORANK>"]
        |> commas
        |> List.fold (fun acc elem -> String.concat "" [acc; elem]) ""

    let tmain = String.concat "" ["template<"; tTypes; "> void "; oloop.Name; "("; argTypes; "){\n\t// Nothing to see here.\n}"]

    let ranks = (irank, orank) ||> List.map2 (fun x -> fun y -> List.append (x |> List.map string) [string y])
    let types = (itype, otype) ||> List.map2 (fun x -> fun y -> List.append x [y])
    let tSpecTypes = (types, ranks) ||> List.map2 (List.fold2 (fun acc elem1 elem2 -> List.append acc (elem1 :: [elem2])) [])
                                     |> List.map (commas >> (fun x -> String.concat "" x))
    let tSpecArgs = (types, ranks) ||> List.map2 (fun x -> fun y -> List.init (arity+1) (fun i -> String.concat "" ["nested_array<"; x.[i]; ", "; y.[i]; ">"]))

    let tSpecs = tSpecTypes |> List.map (fun x -> String.concat "" ["template<> void "; oloop.Name; "<"; x; ">"])


    let inames = List.init oloop.iarrays.Length (fun i -> oloop.iarrays.[i] |> List.map (fun x -> x.arrName))

    



    String.concat "" ["template<"]

let lex (tokens: Token list) = 

    // Scan for loop API calls and create loop state machines
    let mloops = scanMethodLoops tokens
    let oloops = scanObjectLoops tokens

    // Sort pragma objects into array and function pragmas
    let alist, flist = tokens |> (scanPragmas >> sortPragmas)

    // Retrieve array and function objects from pragma info
    let arrays = List.init alist.Length (fun i ->
                                             let directive, clauses, scope = alist.[i]
                                             getArray clauses scope
                                        )
    let funcs = List.init flist.Length (fun i ->
                                            let directive, clauses, scope = flist.[i]
                                            let name = ([(snd directive).Head] |> tokenToStr).Head
                                            getFunction name clauses scope
                                       )

    // Pass array and function objects to the loops that call them, if they are visible
    sendArraysToLoops arrays mloops oloops
    sendFunctionsToLoops funcs mloops oloops


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
    //return oarray;
}
#pragma edgi function(add10) arity(1) input(iarray) iranks(0) output(oarray) orank(0)
{
    oarray = iarray + 10;
    //return oarray;
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

    auto oloop = object_for(sumThenMultiply);
    auto mloop = method_for(array1, array1, array3);

    auto ooarray = oloop(array1, array1, array3);
    auto moarray = mloop(sumThenMultiply);

    auto noarray = nested_for(sumThenMultiply, array1, array1, array3);

    auto newfunc = pipe(sumThenMultiply, add10);

    return 0;
}"""

let tokens = tokenize code;;
