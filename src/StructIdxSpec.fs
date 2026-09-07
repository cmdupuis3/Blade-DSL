/// The constrained-record COUNTING layer. No types, no lowering, no
/// emission: this file turns a fence-eligible struct declaration into the
/// LIST OF ITS SOLUTIONS and their count, and nothing else. (A later layer
/// owns the type -- IxKCompound + baked mask + offsets + nominal tag --
/// and the ergonomics.)
///
/// WHAT IS BEING COUNTED. A struct whose every field is an `Int` with
/// static bounds spans a rectangular BOX; its `where` conjuncts cut a
/// subset out of that box. At CONSTRUCTION a false conjunct is an error
/// (assert-not-solve, already shipped in StaticEval's fold); at
/// ENUMERATION -- the record used in index position -- a false conjunct is
/// EXCLUSION. One conjunct list, two readings; this file implements the
/// second and only the second.
///
///     static struct CGm112 { m1: Int in -1 .. 2, m2: Int in -1 .. 2,
///                            m_out: Int in -1 .. 2 } where m1 + m2 == m_out
///
/// box 27, solutions 7. `idx_card(CGm112)` folds to 7. Index-eligibility is
/// an OPT-IN: the `static` on that declaration is load-bearing, and a plain
/// `struct` of the same shape is refused by the fence.
///
/// COORDINATE CONVENTIONS, fixed HERE:
/// * Bounds are INCLUSIVE on both ends. The surface's `in lo .. hi` is
///   HALF-OPEN and the fence normalizes it before this file sees it:
///   `in -1 .. 2` arrives as Lo = -1, Hi = 1. Extent = Hi - Lo + 1, and a
///   non-positive extent is an EMPTY box (card 0), not an error.
/// * Declaration order = lex NESTING order, FIRST FIELD OUTERMOST: the
///   first field is the slowest-varying coordinate.
/// * Entries are emitted as VALUES, never as box coordinates. The shift
///   (coordinate + Lo) is applied inside the enumerator and is not part of
///   the public surface. Because the shift is order-preserving,
///   lex-over-coordinates = lex-over-values, so "lex ascending" is unambiguous.
/// * Storage offset = enumeration position (0-based).
///
/// THE CERTIFICATE: two enumerations, asserted on EVERY call. House
/// discipline: a counting module asserts its own answer by a second,
/// structurally different route, and a violation is a COMPILER BUG
/// (failwith), not a user error. The two routes:
///   ROUTE 1, FLAT FILTER: odometer over the whole box in lex order,
///   keeping the cells whose conjunct conjunction is true. Every box cell
///   is visited; the predicate is the only thing consulted.
///   ROUTE 2, ARROW HEADS (the `ck` arrow of BladeCompound.v:50-66 read as
///   an algorithm): extend a PREFIX one field at a time, and keep a prefix
///   only if it HAS A COMPLETION (`has_completion` / `ck_heads`) -- some
///   assignment of the remaining fields making the full cell satisfy the
///   predicate. Prefixes with no completion are pruned, subtree and all, so
///   route 2 visits a different set of nodes in a different order and
///   reaches the leaves through a different control structure entirely.
///
/// They must agree on SET **AND ORDER**. Set agreement catches a dropped or
/// duplicated solution; ORDER agreement catches an offset bug -- a wrong
/// shift permutes values without changing the set. `card = |entries|` is
/// asserted alongside, since cardinality is COMPUTED here, never assumed.
///
/// The two routes share the CELL PREDICATE deliberately: the predicate is
/// the fence's, not the algorithm's, and a certificate over two copies of
/// the same enumeration would certify nothing. What differs is the
/// enumeration. The predicate is MEMOIZED on the cell key, which is what
/// keeps route 2's completion search affordable (it would otherwise
/// re-derive up to `#fields x box` cells); with the memo, the number of
/// distinct predicate evaluations is at most the box volume for both
/// routes together.
///
/// CAPS AND FAILURE MODES:
/// * BOX CAP: the box volume PI (Hi - Lo + 1) over the inclusive bounds
///   must be <= 100,000 cells (the symLiftDecl precedent). Refused BEFORE
///   any enumeration, so a runaway declaration costs nothing.
/// * A conjunct that does not fold is an error naming the WITNESS CELL,
///   e.g. "(m1 = -1, m2 = 0, ...)", because the failure is per-cell and a
///   cell-free message is unactionable. The cell reported is the FIRST
///   FAILING cell in LEX order: route 1 walks the box in lex order and
///   stops at the first error, so an unfoldable conjunct is paid once for
///   the whole call. The per-cell budget itself (`StaticEval.cellBudget`)
///   is a cost model -- a cell predicate is a boolean over a few bound
///   integers -- rather than a safety margin.
/// * ONE RUNAWAY IS STILL THIS LAYER'S TO CATCH, and the evaluator cannot
///   help with it: `idx_card` is a SYNTACTIC builtin, so a conjunct may
///   reach the count of the struct being counted, and each hop restarts
///   the fold at depth zero where no depth guard can see it. See the
///   enumeration-in-progress set below, and index-types/157.
/// * A conjunct that folds to a NON-boolean is an error, not an exclusion:
///   silently excluding an ill-typed conjunct would make a typo look like a
///   tight constraint.
/// * EMPTINESS is legitimate here: card 0 folds successfully with no
///   warning. A derived-empty warning belongs to whichever later layer
///   makes a `range<R>` no-op loop actually reachable.
module Blade.StructIdxSpec

open Blade.Ast
open Blade.StaticEval
open Blade.StructIdxFence

// Caps

/// The box-volume cap in cells: PI (Hi - Lo + 1) over the INCLUSIVE
/// per-field bounds. 100,000 is the symLiftDecl precedent, and also the
/// other half of what bounds a fold: the per-cell budget is
/// `StaticEval.cellBudget` (10,000 steps), so the two together cap one
/// `idx_card` call's static evaluation rather than leaving it open-ended.
[<Literal>]
let maxBoxCells = 100_000

// The box

/// One field's contribution to the box: an INCLUSIVE integer interval.
/// `Hi < Lo` is a legal EMPTY interval (extent 0), not a malformed one.
///
/// This is the FENCE's own `FieldBox` under this layer's name, not a copy:
/// the box crosses the seam as one record type, so no conversion exists that
/// could transpose or drop a field on the way in.
type BoxField = FieldBox

/// `Hi - Lo + 1`, floored at 0 so an inverted interval reads as empty. The
/// fence's `extent`, re-exported under the name this layer's arithmetic uses.
let fieldExtent (f: BoxField) : int64 = extent f

/// The box volume as an int64 (no overflow argument needed below the cap, and
/// int64 keeps the CAP CHECK itself honest for declarations far above it).
/// The VOLUME and its cap are this layer's, not the fence's -- the fence knows
/// only per-field extents.
let boxVolume (fields: BoxField list) : int64 =
    fields |> List.fold (fun acc f -> acc * fieldExtent f) 1L

/// The result of a counting call: the box it was taken over, the solutions as
/// VALUES in lex-ascending order, and the computed cardinality.
type BoxEntries = {
    /// The struct name, or the caller's label for a bare-box call.
    Name: string
    /// The per-field inclusive box, DECLARATION order (first = outermost).
    Fields: BoxField list
    /// One list of field VALUES per solution, declaration order, the whole
    /// list lex ascending. Position in this list IS the storage offset.
    Entries: int64 list list
    /// |Entries|, asserted equal to it on every call.
    Card: int
}

/// A cell predicate: given every field bound by name (declaration order), is
/// this cell a solution? `Ok false` is EXCLUSION; `Error` is a fence failure
/// (a conjunct that did not fold, or did not fold to a boolean) and aborts the
/// whole call.
type CellPredicate = (string * int64) list -> Result<bool, string>

// Cell rendering -- the witness-cell suffix every per-cell failure carries

/// "(m1 = -1, m2 = 0, m_out = 1)". A per-cell failure without this is
/// unactionable: the user cannot tell WHICH assignment blew the fuel.
let renderCell (cell: (string * int64) list) : string =
    cell
    |> List.map (fun (n, v) -> $"{n} = {v}")
    |> String.concat ", "
    |> sprintf "(%s)"

// The memoized predicate

/// Wrap a cell predicate in a per-call memo keyed on the coordinate vector.
/// Both routes go through the SAME memo, so the box volume bounds the number
/// of real conjunct folds for the whole certificate -- without it, route 2's
/// completion search would re-fold up to `#fields x volume` cells.
///
/// An `Error` is memoized too: a cell whose conjuncts refuse to fold refuses
/// identically every time, and re-paying the fold on a revisit buys nothing.
/// It also keeps the WITNESS CELL stable -- the first failing cell in lex order
/// is the one both routes see, because the second route reads the first's memo
/// rather than re-deciding.
let private memoize (p: CellPredicate) : (int64 list -> (string * int64) list -> Result<bool, string>) =
    let cache = System.Collections.Generic.Dictionary<int64 list, Result<bool, string>>()
    fun key cell ->
        match cache.TryGetValue key with
        | true, r -> r
        | _ ->
            let r = p cell
            cache.[key] <- r
            r

// ROUTE 1 -- flat filter over the whole box, lex ascending

/// Odometer over every box cell in lex order (first field slowest), keeping
/// the cells the predicate accepts. Nothing is pruned and nothing is
/// structured: this is the definition, coded as directly as it can be.
let private routeFlat
        (fields: BoxField list)
        (pred: int64 list -> (string * int64) list -> Result<bool, string>)
        : Result<int64 list list, string> =
    let arr = List.toArray fields
    let n = arr.Length
    let out = System.Collections.Generic.List<int64 list>()
    let mutable err : string option = None
    // `coords` is the odometer state; `vals` the shifted values handed out.
    let coords : int64[] = Array.zeroCreate n
    let rec go (i: int) =
        if err.IsSome then ()
        elif i = n then
            let vals = [ for j in 0 .. n - 1 -> arr.[j].Lo + coords.[j] ]
            let cell = List.zip [ for j in 0 .. n - 1 -> arr.[j].Field ] vals
            match pred vals cell with
            | Ok true -> out.Add vals
            | Ok false -> ()
            | Error e -> err <- Some e
        else
            let ext = fieldExtent arr.[i]
            let mutable c = 0L
            while c < ext && err.IsNone do
                coords.[i] <- c
                go (i + 1)
                c <- c + 1L
    go 0
    match err with
    | Some e -> Error e
    | None -> Ok (List.ofSeq out)

// ROUTE 2 -- the ck arrow: prefix extension filtered by has_completion

/// `has_completion prefix` -- does SOME assignment of the remaining fields
/// complete this prefix to a solution? BladeCompound.v:50-66's
/// satisfiability-filtered arrow head test, and the whole reason route 2 is
/// a different algorithm: it decides membership of a PARTIAL tuple, which
/// route 1 has no notion of at all. Short-circuits on the first completion
/// found; errors propagate (pretending "no completion" would under-count).
let private hasCompletion
        (arr: BoxField[])
        (pred: int64 list -> (string * int64) list -> Result<bool, string>)
        (prefix: int64 list)
        : Result<bool, string> =
    let n = arr.Length
    let depth = List.length prefix
    let buf : int64[] = Array.zeroCreate n
    prefix |> List.iteri (fun i v -> buf.[i] <- v)
    let mutable err : string option = None
    let rec go (i: int) : bool =
        if err.IsSome then false
        elif i = n then
            let vals = List.ofArray buf
            let cell = List.zip [ for j in 0 .. n - 1 -> arr.[j].Field ] vals
            match pred vals cell with
            | Ok b -> b
            | Error e -> err <- Some e; false
        else
            let lo = arr.[i].Lo
            let ext = fieldExtent arr.[i]
            let mutable c = 0L
            let mutable found = false
            while not found && c < ext && err.IsNone do
                buf.[i] <- lo + c
                found <- go (i + 1)
                c <- c + 1L
            found
    // Search starts BELOW the prefix: the prefix's own coordinates are fixed.
    let r = go depth
    match err with
    | Some e -> Error e
    | None -> Ok r

/// Grow the solution list one FIELD at a time, level by level: at each
/// level take the surviving prefixes, extend each by every value of the
/// next field, and drop the extensions that have no completion. The final
/// level's survivors are the solutions.
///
/// Lex order is inherited structurally: the level list is kept in the
/// order prefixes were extended, and each prefix is extended in ascending
/// value order, so the level list is lex-sorted at every level
/// (BladeLex.v:119's `enumA_lex_sorted`, inherited by ck at :151). Nothing
/// sorts anything here -- if that inheritance were wrong, the ORDER half
/// of the certificate is what would catch it.
let private routeHeads
        (fields: BoxField list)
        (pred: int64 list -> (string * int64) list -> Result<bool, string>)
        : Result<int64 list list, string> =
    let arr = List.toArray fields
    let n = arr.Length
    // Level 0: the empty prefix, alive iff the whole box has any solution.
    // (At n = 0 this is the one-cell box, and the empty tuple is its cell.)
    let rec level (i: int) (prefixes: int64 list list) : Result<int64 list list, string> =
        if i = n then Ok prefixes
        else
            let lo = arr.[i].Lo
            let ext = fieldExtent arr.[i]
            let mutable err : string option = None
            let next = System.Collections.Generic.List<int64 list>()
            for p in prefixes do
                let mutable c = 0L
                while c < ext && err.IsNone do
                    let p' = p @ [ lo + c ]
                    match hasCompletion arr pred p' with
                    | Ok true -> next.Add p'
                    | Ok false -> ()
                    | Error e -> err <- Some e
                    c <- c + 1L
            match err with
            | Some e -> Error e
            | None -> level (i + 1) (List.ofSeq next)
    // The empty prefix survives iff it has a completion. At n = 0 that IS the
    // predicate at the empty cell, which is the right answer for a 0-field
    // record: card 1 if its (constant) conjuncts hold, 0 if not.
    hasCompletion arr pred []
    |> Result.bind (fun alive -> if alive then level 0 [ [] ] else Ok [])

// The certificate

/// Compare the two routes as SET and as ORDER, and pin the cardinality. A
/// failure here is a compiler bug (the house rule), so it raises rather than
/// returning an Error: no user program can make these disagree.
let private certify
        (label: string)
        (fields: BoxField list)
        (flat: int64 list list)
        (heads: int64 list list)
        : unit =
    let render (e: int64 list) =
        List.zip (fields |> List.map (_.Field)) e |> renderCell
    if List.length flat <> List.length heads then
        failwith $"internal: the two enumerations of {label} disagree on CARDINALITY -- the flat box filter found {(List.length flat)} solutions, the arrow heads-filtered enumeration found {(List.length heads)} (see the certificate block in StructIdxSpec.fs)"
    // ORDER agreement, position by position. This is the half that catches an
    // offset bug: a wrong shift permutes values without changing the set.
    List.iteri2 (fun i (a: int64 list) (b: int64 list) ->
        if a <> b then
            failwith $"internal: the two enumerations of {label} disagree at POSITION {i} -- the flat box filter has {(render a)}, the arrow heads-filtered enumeration has {(render b)}. Set agreement without order agreement is the offset-bug signature (retired constrained-index-types plan section 7 C1, the 5a-i third-route discipline)") flat heads
    // Every solution must actually lie in the box, and the list must be
    // strictly lex ascending -- the two properties the storage offset depends
    // on, checked against the box rather than against the other route.
    let arr = List.toArray fields
    flat |> List.iteri (fun i e ->
        if List.length e <> arr.Length then
            failwith $"internal: solution {i} of {label} has {(List.length e)} coordinates but the box has {arr.Length} fields"
        List.iteri (fun j v ->
            if v < arr.[j].Lo || v > arr.[j].Hi then
                failwith $"internal: solution {i} of {label} is outside the box -- field '{arr.[j].Field}' = {v} is not in the inclusive range {arr.[j].Lo} .. {arr.[j].Hi}") e)
    let rec ascending (xs: int64 list list) =
        match xs with
        | a :: (b :: _ as rest) ->
            if compare a b >= 0 then
                failwith $"internal: the enumeration of {label} is not strictly lex ascending -- {(render a)} is not before {(render b)}"
            ascending rest
        | _ -> ()
    ascending flat

// The public counting entry -- the bare-box route

/// Enumerate the solutions of `pred` over the inclusive box `fields`, in lex
/// ascending order of VALUES, with the certificate run on every call.
///
/// `label` names the thing being counted in diagnostics (the struct name on
/// the struct route; a caller-chosen tag on a bare-box call).
///
/// The box cap is checked BEFORE anything is enumerated, so an over-large
/// declaration costs nothing. Predicate errors (a conjunct that did not fold)
/// come back as `Error` with the witness cell already attached by the caller's
/// predicate -- this function does not add one, since it cannot know which
/// conjunct failed.
let enumerateBox (label: string) (fields: BoxField list) (pred: CellPredicate) : Result<BoxEntries, string> =
    let vol = boxVolume fields
    if vol > int64 maxBoxCells then
        let dims =
            fields
            |> List.map (fun f -> $"{f.Field}: {f.Lo} .. {f.Hi} ({fieldExtent f})")
            |> String.concat ", "
        Error ($"{label}: the index box has {vol} cells, over the {maxBoxCells}-cell cap -- enumeration visits every cell of the box before the constraints cut it down, so the CAP IS ON THE BOX, not on the solution count. Box: {dims}. Narrow the field bounds")
    else
        let memo = memoize pred
        routeFlat fields memo
        |> Result.bind (fun flat ->
            routeHeads fields memo
            |> Result.map (fun heads ->
                certify label fields flat heads
                let card = List.length flat
                { Name = label; Fields = fields; Entries = flat; Card = card }))
        |> Result.map (fun r ->
            // card = |entries|, restated at the boundary.
            if r.Card <> List.length r.Entries then
                failwith $"internal: {label} reported card {r.Card} but produced {List.length r.Entries} entries"
            r)

// The fence seam -- struct declaration -> box + cell predicate. The FENCE
// ITSELF IS NOT HERE: `Blade.StructIdxFence` owns eligibility, the
// inclusive-bounds normalization and the per-cell conjunct fold; this file
// owns the enumeration, the certificate, the box VOLUME and its cap, and
// the witness cell. The two functions below are the entire seam, and
// neither looks at an AST node: the box arrives as `FieldBox list` and the
// predicate as a closure. The split falls exactly here because of the
// certificate: two independent enumeration routes over ONE shared cell
// predicate is the whole discipline, and if the fence also enumerated, the
// routes would agree for the wrong reason.

/// Build the per-cell predicate of a fenced struct, and attach the WITNESS
/// CELL to whatever the fence reports. The exclusion reading is the fence's
/// (`Ok false` = excluded, never an error); what this adds is the cell: the
/// fence evaluates one cell at a time and has no idea which of the box's
/// cells it is on, while a per-cell failure without its cell is unactionable.
let cellPredicateOf (env: StaticEnv) (spec: StructBoxSpec) : CellPredicate =
    fun cell ->
        match evalConjunctsAtCell env spec cell with
        | Ok b -> Ok b
        | Error why ->
            // The cell is the whole of what this layer knows and the fence
            // does not. The fold-budget sentence is added ONLY to a
            // did-not-fold failure: a conjunct that folded fine but to the
            // wrong KIND of value is a typo, and telling its author about
            // recursion budgets is noise.
            //
            // "the first cell it is reached at" is a real claim about cost,
            // not a hedge: routeFlat stops at the first erroring cell, so the
            // budget below is spent once for the whole call rather than once
            // per cell of the box.
            let tail =
                if why.Contains "did not fold" then
                    $". Every conjunct is folded once per box cell under a budget of {cellBudget.Steps} steps and {cellBudget.Depth} nesting levels, so a conjunct that recurses without a static bound fails at the FIRST cell it is reached at"
                else ""
            Error $"{why} at {renderCell cell}{tail}"

// The re-entrancy guard

/// Structs whose enumeration is currently on the stack.
///
/// `idx_card` is a SYNTACTIC static builtin, which makes it reachable from
/// anywhere a static expression is, INCLUDING the `where` conjuncts of the
/// very struct it is counting:
///
///     static struct R { p: Int<min=0, max=2> } where p == idx_card(R)
///
/// Enumerating R folds that conjunct, which counts R, which enumerates R.
/// The evaluator's own depth guard cannot see this:
/// `registerSyntacticStaticBuiltin` hands a builtin a step COUNT, not the
/// live fold, so every hop through `idx_card` restarts the budget at depth
/// zero and the recursion is invisible to it -- without this guard the
/// cycle kills the compiler with a StackOverflowException.
///
/// Keyed on the struct NAME rather than on self-reference, so an indirect
/// cycle (R's conjunct counts S, S's conjunct counts R) is caught by the
/// same check. Thread-local because a fold is synchronous and the corpus
/// harness runs different programs on different threads concurrently, so a
/// single shared set would have them reporting each other's cycles.
let private enumerating =
    new System.Threading.ThreadLocal<Set<string>>(fun () -> Set.empty)

/// The struct route: name -> solutions + cardinality, fence and certificate
/// included. This is what `idx_card` calls.
let structEntries (env: StaticEnv) (name: string) : Result<BoxEntries, string> =
    if Set.contains name enumerating.Value then
        Error (sprintf "idx_card(%s): counting %s requires folding %s's own constraints, which reach idx_card(%s) again -- a struct's solution count cannot be one of the things its constraints depend on. Currently being counted: %s"
                   name name name name
                   (enumerating.Value |> Set.toList |> String.concat ", "))
    else
        enumerating.Value <- Set.add name enumerating.Value
        try
            structStaticFence env name
            |> Result.bind (fun spec -> enumerateBox spec.Name spec.Fields (cellPredicateOf env spec))
        finally
            // `finally`, not a trailing statement: `enumerateBox`'s certificate
            // failures are `failwith`s (a compiler bug, by house rule), and a
            // leaked name would turn one of those into a bogus cycle report on
            // every later call from this thread.
            enumerating.Value <- Set.remove name enumerating.Value

/// The cardinality alone.
let structCard (env: StaticEnv) (name: string) : Result<int, string> =
    structEntries env name |> Result.map (_.Card)

// `idx_card(R)`, the sizing surface. A CORE static builtin: it must fold in
// a module that never writes `import ml`, so it goes into StaticEval's own
// registry rather than the `ml.*` alias path. Its argument is a BARE
// IDENTIFIER naming a struct DECLARATION, not a value, which is why it
// registers as a SYNTACTIC builtin (unevaluated args): the evaluated-args
// registry would fold `CGm112` to "undefined variable" before this code
// ever ran. Resolution is statics-first, then the declaration registry.

let private idxCard (env: StaticEnv) (_fuel: int) (args: Expr list) : Result<StaticValue, string> =
    match args with
    | [ { Kind = ExprKind.ExprVar name } ] ->
        // Statics first: a `let static` of the same name shadows nothing here
        // (a struct name and a value name cannot collide in one scope), but
        // consulting Values first is the precedent's order and it makes the
        // "you passed a value" case diagnosable rather than confusing.
        match Map.tryFind name env.Values with
        | Some v when not (Map.containsKey name env.Structs) ->
            Error ($"idx_card({name}): '{name}' is a static VALUE ({(ppStaticValue v)}), not a struct declaration. idx_card counts the solutions of a constrained struct used as an index type; pass the struct's bare name")
        | _ ->
            structCard env name |> Result.map (fun c -> SVInt (int64 c))
    | [ other ] ->
        Error (sprintf "idx_card: the argument must be the BARE NAME of a declared struct (as in `idx_card(CGm112)`) -- an expression of any other form cannot name an index type%s"
                   (match other.Kind with
                    | ExprKind.ExprLit _ -> " (a literal was given)"
                    | ExprKind.ExprApp _ -> " (a call was given)"
                    | ExprKind.ExprField _ -> " (a field access was given)"
                    | _ -> ""))
    | _ ->
        Error $"idx_card expects exactly one argument, the bare name of a declared struct (got {List.length args})"

/// Idempotent registration of the counting layer's static builtins. Called
/// once from TypeCheck.typeCheck, ahead of every resolveStatics pass -- the
/// ProviderStatics.install precedent.
let install () =
    registerSyntacticStaticBuiltin "idx_card" idxCard

// ---------------------------------------------------------------------------
// The enumeration route of a static struct used as an INDEX TYPE
// (`range<R>`, `Array<T like R>`; docs/plans/structural/06).
// ---------------------------------------------------------------------------

open Blade.Types

type DomainRoute =
    /// Class A: enumerated in closed form from the plan, uncapped.
    | DomainClosedForm of DomainPlan
    /// Class B: the constraints are not closed-form; the box is under the
    /// cap, so the solutions are enumerated by the counting layer and baked
    /// as a static key table (lex order, certified).
    | DomainTable of entries: int64 list list * card: int * why: string

/// The route, with the house certificate: below the cap the closed-form
/// enumeration must equal the counting layer's set AND order (a third route
/// beside routeFlat / routeHeads); above it the plan stands alone.
let domainRoute (env: StaticEnv) (name: string) : Result<DomainRoute, string> =
    structStaticFence env name |> Result.bind (fun spec ->
        let vol = boxVolume spec.Fields
        match domainPlanOf env spec with
        | Ok plan ->
            if vol <= int64 maxBoxCells then
                enumerateBox spec.Name spec.Fields (cellPredicateOf env spec)
                |> Result.map (fun counted ->
                    let mine = Blade.Types.enumerateDomain plan |> List.map List.ofArray
                    if mine <> counted.Entries then
                        failwith $"internal: the closed-form enumeration of {name} ({List.length mine} cells) disagrees with the counting layer ({counted.Card} cells) -- the recogniser's projection is wrong for this constraint shape; please report it"
                    DomainClosedForm plan)
            else Ok (DomainClosedForm plan)
        | Error why ->
            if vol <= int64 maxBoxCells then
                enumerateBox spec.Name spec.Fields (cellPredicateOf env spec)
                |> Result.map (fun counted -> DomainTable (counted.Entries, counted.Card, why))
            else
                Error ($"{name} cannot be enumerated: its constraints are not closed-form ({why}), and its box has {vol} cells, over the {maxBoxCells}-cell cap for enumerating a domain by table. Write the constraints as linear inequalities on the fields (`i - j <= w`, `l3 <= l1 + l2`, `abs(i - j) <= w`, `m1 + m2 == m_out`), or narrow the field bounds"))
