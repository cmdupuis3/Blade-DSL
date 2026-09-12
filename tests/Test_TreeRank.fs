// Pins for the TreeIdx bijection layer (src/TreeRank.fs) — P1 of
// docs/plans/plan-graphs-trees.md: shape validation, the derived preorder
// tables, and the path <-> leaf-offset pair (`forward`/`backward`/`subtree`)
// of the feature doc §1.2.
//
// The ground truth is BRUTE FORCE over an INDEPENDENT representation, not a
// second closed form. This file carries its own tiny nested-shape ADT
//
//     type Shape = Node of Shape list          (leaf = Node [])
//
// with its own preorder flattener (Shape -> degree sequence) and its own naive
// recursive path enumerator. Nothing in the oracle reads TreeRank's tables:
// the enumerator recurses on the nest, so a bug in `Sizes`/`LeafStart`/the
// child CSR cannot hide by being consulted on both sides. The nest is also
// what the feature doc §2.1 actually writes down, so the two pinned literal
// shapes (crystal, skew) check the DERIVATION of their degree sequences, not
// only the sequences.
//
// The order is a CONTRACT, so it is pinned as an ORDER and not only as a set:
// leaf-offset order = the preorder DFS = ascending lexicographic on paths
// (plan §10). A forward/backward round trip cannot catch an order convention
// drifting — both halves shift together, the OrbRank/antisym post-mortem — so
// the enumeration is compared element by element against the naive one AND
// against `List.sort` (which on int lists IS lex, a shorter prefix sorting
// first; no complete path is a prefix of another, so the two agree exactly).
//
// Subtree contiguity gets its own oracle: for EVERY node, the brute-force leaf
// set under it (paths of the sub-nest, prefixed) is looked up in the global
// path->offset map and required to be exactly [leafStart, leafStart+leafCount),
// in order. That is the preorder invariant the whole storage story rests on
// (feature doc §1.2), stated where it can fail.
module Blade.Tests.TreeRankReview

open System.Collections.Generic
open Blade.Tests.TestHarness
open Blade.TreeRank

// ---------------------------------------------------------------------------
// The independent oracle: a nested shape ADT and naive recursion over it
// ---------------------------------------------------------------------------

/// The feature doc §2.1 surface form: a nest of shapes is a shape, `leaf` is
/// the base case. Deliberately NOT TreeRank's representation.
type private Shape = Node of Shape list

let private leaf = Node []

/// Shape -> preorder degree sequence. The elaborator's job (§2.3), written
/// here as the obvious two-line recursion.
let rec private toDegrees (s: Shape) : int list =
    match s with
    | Node kids -> List.length kids :: (kids |> List.collect toDegrees)

/// Every complete root-to-leaf path of the nest, by naive recursion, children
/// in ascending order. This is the ORACLE for both the set and the order.
let rec private naivePaths (s: Shape) : int list list =
    match s with
    | Node [] -> [ [] ]
    | Node kids ->
        kids
        |> List.mapi (fun k c -> naivePaths c |> List.map (fun p -> k :: p))
        |> List.concat

/// Node count of the nest.
let rec private naiveNodes (s: Shape) : int =
    match s with
    | Node kids -> 1 + (kids |> List.sumBy naiveNodes)

/// Arity of the nest's root.
let private naiveArity (s: Shape) : int =
    match s with
    | Node kids -> List.length kids

/// Every node of the nest in PREORDER, as (path from the root, sub-nest). The
/// position in this list is the node id, by definition of preorder — which is
/// how the oracle names nodes without borrowing TreeRank's numbering.
let rec private naiveNodeList (s: Shape) (rev: int list) : (int list * Shape) list =
    match s with
    | Node kids ->
        (List.rev rev, s)
        :: (kids |> List.mapi (fun k c -> naiveNodeList c (k :: rev)) |> List.concat)

/// A `TreeSubtree` literal, spelled once so the record labels never have to be
/// resolved from context.
let private view (node: int) (start: int) (count: int) : TreeSubtree =
    { Node = node; LeafStart = start; LeafCount = count }

// ---------------------------------------------------------------------------
// The shape menu
// ---------------------------------------------------------------------------

/// `d` unary levels stacked above `s`.
let rec private chain (d: int) (s: Shape) : Shape = if d <= 0 then s else chain (d - 1) (Node [ s ])

/// The complete k-ary tree of depth `d` (the feature doc §2.2 generated shape).
let rec private complete (k: int) (d: int) : Shape =
    if d <= 0 then leaf else Node(List.replicate k (complete k (d - 1)))

/// `[[leaf, leaf], [leaf, leaf, leaf]]` — feature doc §2.1/§2.3.
let private crystal = Node [ Node [ leaf; leaf ]; Node [ leaf; leaf; leaf ] ]

/// `[ leaf, [ leaf, [leaf, leaf] ], [ [leaf], leaf, leaf ] ]` — feature doc §2.1.
let private skew =
    Node [ leaf
           Node [ leaf; Node [ leaf; leaf ] ]
           Node [ Node [ leaf ]; leaf; leaf ] ]

/// The sweep menu covers every degenerate corner the plan's P1 row names —
/// single leaf, all-leaf (flat), deep-narrow, wide-shallow — plus the two
/// documented shapes and the generated complete trees, because a curated list
/// is exactly what forgets the corners (the OrbRank closure lesson).
let private shapeMenu : (string * Shape) list =
    [ "single leaf", leaf
      "unary chain d=3", chain 3 leaf
      "flat k=1", Node [ leaf ]
      "flat k=5", Node(List.replicate 5 leaf)
      "flat k=12", Node(List.replicate 12 leaf)
      "crystal", crystal
      "skew", skew
      "deep-narrow d=10", chain 10 (Node [ leaf; leaf ])
      "deep-narrow d=6 forked", chain 6 (Node [ chain 4 leaf; leaf ])
      "wide-shallow 6x7", Node(List.replicate 6 (Node(List.replicate 7 leaf)))
      "complete binary d=4", complete 2 4
      "complete ternary d=3", complete 3 3
      "mixed depths", Node [ chain 3 leaf; leaf; Node [ leaf; chain 2 (Node [ leaf; leaf; leaf ]) ] ] ]

// ---------------------------------------------------------------------------
// One shape, four aspects, all against the naive oracle
// ---------------------------------------------------------------------------

/// Returns (aspect, ok, detail) for the four things every shape must satisfy.
/// Shared by the menu sweep and the seeded-random sweep, so a random shape is
/// held to exactly the same standard as a named one.
let private verifyShape (s: Shape) : (string * bool * string) list =
    let deg = toDegrees s
    let naive = naivePaths s
    let naiveArr = List.toArray naive
    let card = naiveArr.Length
    let nodes = naiveNodeList s []
    match treeTables deg with
    | Error e -> [ "tables build", false, $"treeTables {showDegrees deg} = Error {e}" ]
    | Ok t ->
        // path -> global leaf offset, straight off the oracle's own enumeration.
        let offOf = Dictionary<int list, int>()
        naive |> List.iteri (fun i p -> offOf.[p] <- i)

        // (a) the enumeration, as a SET and as an ORDER.
        let got = treePaths t |> List.ofSeq
        let enumOk = got = naive
        let setOk = List.sort got = List.sort naive
        let enumDetail =
            if enumOk then $"{card} paths"
            elif setOk then "same set, WRONG ORDER"
            else $"{List.length got} paths vs {card}"

        // (b) round trip, both directions, over every offset and every path.
        let mutable rtOk = (cardinality t = card && nodeCount t = List.length deg)
        let mutable rtBad =
            if rtOk then ""
            else $"cardinality {cardinality t} / nodes {nodeCount t}, want {card} / {List.length deg}"
        for i in 0 .. card - 1 do
            if rtOk then
                let p = naiveArr.[i]
                // forward: checked and unchecked must agree, and both must be i.
                if treeForwardChecked t p <> Ok i then
                    rtOk <- false
                    rtBad <- $"forward {showPath p} = {treeForwardChecked t p}, want {i}"
                elif treeForward t p <> i then
                    rtOk <- false
                    rtBad <- $"unchecked forward {showPath p} = {treeForward t p}, want {i}"
                // backward: total on [0, card), and the inverse of forward.
                elif treeBackwardChecked t i <> Ok p then
                    rtOk <- false
                    rtBad <- $"backward {i} = {showPath (treeBackward t i)}, want {showPath p}"
                elif treeBackward t i <> p then
                    rtOk <- false
                    rtBad <- $"unchecked backward {i} = {showPath (treeBackward t i)}, want {showPath p}"
                elif treeForward t (treeBackward t i) <> i then
                    rtOk <- false
                    rtBad <- $"forward(backward({i})) <> {i}"

        // (c) lexicographic order — the contract, checked against List.sort and
        // as a strictly increasing chain (which also proves the paths distinct).
        let lexOk =
            got = List.sort got
            && (got |> List.pairwise |> List.forall (fun (a, b) -> compare a b < 0))

        // (d) every node: the derived tables and subtree contiguity.
        let nodeIdOf = Dictionary<int list, int>()
        nodes |> List.iteri (fun i (p, _) -> nodeIdOf.[p] <- i)
        let mutable subOk = true
        let mutable subBad = ""
        nodes
        |> List.iteri (fun i (q, sub) ->
            if subOk then
                let subPaths = naivePaths sub
                let subOffs = subPaths |> List.map (fun p -> offOf.[q @ p])
                let want = view i (List.min subOffs) (List.length subOffs)
                let fail (msg: string) =
                    subOk <- false
                    subBad <- $"node {i} at {showPath q}: {msg}"
                if treeSubtreeChecked t q <> Ok want then
                    fail $"subtree = {treeSubtreeChecked t q}, want {want}"
                elif treeSubtree t q <> want then
                    fail "the unchecked subtree disagrees with the checked one"
                elif subOffs <> [ want.LeafStart .. want.LeafStart + want.LeafCount - 1 ] then
                    // The preorder invariant itself: the block is contiguous AND
                    // in order, not merely the right size.
                    fail $"leaf offsets are not the block [{want.LeafStart},{want.LeafStart + want.LeafCount})"
                elif t.LeafCount.[i] <> want.LeafCount || t.LeafStart.[i] <> want.LeafStart then
                    fail "the LeafCount/LeafStart tables disagree with treeSubtree"
                elif t.Sizes.[i] <> naiveNodes sub then
                    fail $"Sizes = {t.Sizes.[i]}, want {naiveNodes sub}"
                elif t.Depth.[i] <> List.length q then
                    fail $"Depth = {t.Depth.[i]}, want {List.length q}"
                elif isLeaf t i <> (naiveArity sub = 0) then
                    fail "isLeaf disagrees with the nest"
                elif degreeOf t i <> naiveArity sub then
                    fail $"degreeOf = {degreeOf t i}, want {naiveArity sub}"
                elif t.Parent.[i] <> (if List.isEmpty q then -1
                                      else nodeIdOf.[List.truncate (List.length q - 1) q]) then
                    fail $"Parent = {t.Parent.[i]}"
                elif t.ChildIndex.[i] <> (if List.isEmpty q then 0 else List.last q) then
                    fail $"ChildIndex = {t.ChildIndex.[i]}"
                elif (treeSubtreePaths t q |> List.ofSeq) <> subPaths then
                    fail "treeSubtreePaths disagrees with the sub-nest's own paths"
                else
                    // the child accessor, against the oracle's own numbering
                    for k in 0 .. naiveArity sub - 1 do
                        if subOk then
                            let wantChild = nodeIdOf.[q @ [ k ]]
                            if treeChild t i k <> wantChild || treeChildChecked t i k <> Ok wantChild then
                                fail $"child {k} = {treeChild t i k}, want {wantChild}")

        // maxDepth, over the same oracle.
        let wantMaxDepth = nodes |> List.map (fun (q, _) -> List.length q) |> List.max
        let depthOk = maxDepth t = wantMaxDepth

        [ "enumeration = brute force, as set and as order", (enumOk && setOk), enumDetail
          "forward/backward round-trip, every offset and every path", rtOk,
          (if rtOk then $"{card} offsets" else rtBad)
          "enumeration is lexicographic", lexOk, ""
          "tables + subtree contiguity over every node", (subOk && depthOk),
          (if not depthOk then $"maxDepth {maxDepth t}, want {wantMaxDepth}"
           elif subOk then $"{List.length nodes} nodes, depth {wantMaxDepth}"
           else subBad) ]

// ---------------------------------------------------------------------------
// Seeded-random shapes
// ---------------------------------------------------------------------------

/// A random nest, node-budgeted. The budget is decremented per node created,
/// so the generator degenerates to leaves once it is spent — the depth/arity
/// caps then bound the overshoot to a few nodes.
let rec private randomShape (rng: System.Random) (budget: int ref) (depthLeft: int) : Shape =
    budget.Value <- budget.Value - 1
    if depthLeft <= 0 || budget.Value <= 4 || rng.Next 10 < 4 then leaf
    else
        let k = 1 + rng.Next 4
        Node(List.init k (fun _ -> randomShape rng budget (depthLeft - 1)))

// ---------------------------------------------------------------------------

let runTreeRankTests () : BlockResult =
    printHeader "TreeIdx bijections (TreeRank.fs)"
    let mutable passed = 0
    let mutable failed = 0
    let mutable failedNames: string list = []
    let check name ok detail =
        if ok then
            passed <- passed + 1
            resultLine Pass name detail
        else
            failed <- failed + 1
            failedNames <- failedNames @ [ name ]
            resultLine Fail name detail

    let isError r = match r with Error _ -> true | Ok _ -> false

    // ---- §2.3: the two documented shapes, DERIVED then pinned literally -----
    // The nest is the surface; the degree sequence is what the type carries.
    // Pinning both ends means a wrong flattener cannot be absorbed by a
    // matching-but-wrong table build.
    check "crystal: [[leaf,leaf],[leaf,leaf,leaf]] flattens to [2,2,0,0,3,0,0,0]"
          (toDegrees crystal = [ 2; 2; 0; 0; 3; 0; 0; 0 ])
          (showDegrees (toDegrees crystal))
    check "skew: the §2.1 nest flattens to [3,0,2,0,2,0,0,3,1,0,0,0]"
          (toDegrees skew = [ 3; 0; 2; 0; 2; 0; 0; 3; 1; 0; 0; 0 ])
          (showDegrees (toDegrees skew))

    let tCrystal = treeTablesUnchecked [ 2; 2; 0; 0; 3; 0; 0; 0 ]
    check "crystal: exactly the 5 paths (0,0) (0,1) (1,0) (1,1) (1,2), in that order"
          (treePaths tCrystal |> List.ofSeq = [ [ 0; 0 ]; [ 0; 1 ]; [ 1; 0 ]; [ 1; 1 ]; [ 1; 2 ] ]
           && cardinality tCrystal = 5
           && nodeCount tCrystal = 8
           && maxDepth tCrystal = 2)
          "cardinality 5, 8 nodes, depth 2"
    check "crystal: forward pins (0,0)=0 (0,1)=1 (1,0)=2 (1,1)=3 (1,2)=4, and the subtree blocks"
          (treeForwardChecked tCrystal [ 0; 0 ] = Ok 0
           && treeForwardChecked tCrystal [ 0; 1 ] = Ok 1
           && treeForwardChecked tCrystal [ 1; 0 ] = Ok 2
           && treeForwardChecked tCrystal [ 1; 1 ] = Ok 3
           && treeForwardChecked tCrystal [ 1; 2 ] = Ok 4
           && treeSubtreeChecked tCrystal [] = Ok(view 0 0 5)
           && treeSubtreeChecked tCrystal [ 0 ] = Ok(view 1 0 2)
           && treeSubtreeChecked tCrystal [ 1 ] = Ok(view 4 2 3))
          ""

    let tSkew = treeTablesUnchecked [ 3; 0; 2; 0; 2; 0; 0; 3; 1; 0; 0; 0 ]
    check "skew: exactly the 7 paths (0) (1,0) (1,1,0) (1,1,1) (2,0,0) (2,1) (2,2), in that order"
          (treePaths tSkew |> List.ofSeq
             = [ [ 0 ]; [ 1; 0 ]; [ 1; 1; 0 ]; [ 1; 1; 1 ]; [ 2; 0; 0 ]; [ 2; 1 ]; [ 2; 2 ] ]
           && cardinality tSkew = 7
           && nodeCount tSkew = 12
           && maxDepth tSkew = 3)
          "cardinality 7, 12 nodes, depth 3 -- leaves at DIFFERENT depths"
    check "skew: leaves at different depths still rank contiguously per subtree"
          (treeForwardChecked tSkew [ 0 ] = Ok 0
           && treeForwardChecked tSkew [ 1; 1; 1 ] = Ok 3
           && treeForwardChecked tSkew [ 2; 2 ] = Ok 6
           && treeSubtreeChecked tSkew [ 1 ] = Ok(view 2 1 3)
           && treeSubtreeChecked tSkew [ 2 ] = Ok(view 7 4 3)
           && treeSubtreeChecked tSkew [ 1; 1 ] = Ok(view 4 2 2))
          ""

    // ---- degenerate shapes, each pinned by hand ------------------------------
    // The single leaf is the one that catches an off-by-one everywhere: the
    // ONLY complete path is the EMPTY path, so cardinality is 1 while the path
    // itself has no steps to walk.
    let tLeaf = treeTablesUnchecked [ 0 ]
    check "single leaf [0]: cardinality 1, the only complete path is the EMPTY path"
          (validateDegrees [ 0 ] = Ok()
           && cardinality tLeaf = 1
           && nodeCount tLeaf = 1
           && maxDepth tLeaf = 0
           && isLeaf tLeaf 0
           && treePaths tLeaf |> List.ofSeq = [ [] ]
           && treeForwardChecked tLeaf [] = Ok 0
           && treeBackwardChecked tLeaf 0 = Ok []
           && treeSubtreeChecked tLeaf [] = Ok(view 0 0 1)
           && tLeaf.Parent.[0] = -1)
          "forward(()) = 0"
    let tChain = treeTablesUnchecked [ 1; 1; 1; 0 ]
    check "unary chain [1,1,1,0]: cardinality 1, the only path is (0,0,0)"
          (cardinality tChain = 1
           && nodeCount tChain = 4
           && maxDepth tChain = 3
           && treePaths tChain |> List.ofSeq = [ [ 0; 0; 0 ] ]
           && treeForwardChecked tChain [ 0; 0; 0 ] = Ok 0
           && treeBackwardChecked tChain 0 = Ok [ 0; 0; 0 ]
           && tChain.Sizes = [| 4; 3; 2; 1 |])
          ""
    // A flat shape IS Idx<k>: one slot, k cells, path (c) at offset c. This is
    // the compatibility pin — the tree family has to contain the dense one.
    let flatOk (k: int) =
        let t = treeTablesUnchecked (k :: List.replicate k 0)
        cardinality t = k
        && maxDepth t = 1
        && treePaths t |> List.ofSeq = [ for c in 0 .. k - 1 -> [ c ] ]
        && [ 0 .. k - 1 ]
           |> List.forall (fun c -> treeForwardChecked t [ c ] = Ok c && treeBackwardChecked t c = Ok [ c ])
    check "flat [k,0..0] behaves exactly as Idx<k>: path (c) is offset c, k = 1 and k = 5"
          (flatOk 1 && flatOk 5) "k = 12 also runs in the sweep"

    // ---- the menu sweep: every aspect against the naive oracle ---------------
    let mutable sweptPaths = 0
    let mutable sweptNodes = 0
    for (name, s) in shapeMenu do
        sweptPaths <- sweptPaths + List.length (naivePaths s)
        sweptNodes <- sweptNodes + naiveNodes s
        for (aspect, ok, detail) in verifyShape s do
            check $"{name}: {aspect}" ok detail

    // ---- seeded-random shapes ------------------------------------------------
    // Fixed seed, so a failure is reproducible and the corpus is stable across
    // runs; the shapes are still nothing anybody chose.
    let rng = System.Random 20260826
    let mutable rndOk = true
    let mutable rndBad = ""
    let mutable rndShapes = 0
    let mutable rndPaths = 0
    let mutable rndNodes = 0
    let mutable rndMaxNodes = 0
    for _ in 1 .. 40 do
        if rndOk then
            let budget = ref 200
            let s = randomShape rng budget 7
            rndShapes <- rndShapes + 1
            rndNodes <- rndNodes + naiveNodes s
            rndMaxNodes <- max rndMaxNodes (naiveNodes s)
            rndPaths <- rndPaths + List.length (naivePaths s)
            for (aspect, ok, detail) in verifyShape s do
                if rndOk && not ok then
                    rndOk <- false
                    rndBad <- $"{showDegrees (toDegrees s)}: {aspect} -- {detail}"
    check "seeded-random shapes (seed 20260826): round-trip, order and contiguity"
          rndOk
          (if rndOk then $"{rndShapes} shapes, {rndNodes} nodes (max {rndMaxNodes}), {rndPaths} paths"
           else rndBad)

    // ---- validation: four distinct refusals ---------------------------------
    // Each of these is a DIFFERENT mistake and must not be collapsed into one
    // message: a sequence that closes early has a tail belonging to no tree; a
    // truncated one has slots nothing fills.
    let badSequences =
        [ "[] empty", []
          "[1] truncated", [ 1 ]
          "[2,0] truncated", [ 2; 0 ]
          "[0,0] closes early", [ 0; 0 ]
          "[-1] negative", [ -1 ]
          "[2,-1,0,0] negative mid-sequence", [ 2; -1; 0; 0 ]
          "[2,0,0,0] closes early", [ 2; 0; 0; 0 ] ]
    let accepted = badSequences |> List.filter (fun (_, d) -> not (isError (validateDegrees d)))
    check "validateDegrees rejects empty / truncated / early-close / negative"
          (List.isEmpty accepted)
          (if List.isEmpty accepted then $"{List.length badSequences} refusals"
           else "accepted: " + (accepted |> List.map fst |> String.concat "; "))
    check "each refusal names its own mistake (no collapsed message)"
          ((badSequences
            |> List.map (fun (_, d) -> match validateDegrees d with Error e -> e | Ok() -> "!accepted")
            |> List.distinct
            |> List.length) = List.length badSequences)
          ""
    check "treeTables consumes the SAME gate: its verdict is validateDegrees' own string"
          (badSequences
           |> List.forall (fun (_, d) ->
               match validateDegrees d, treeTables d with
               | Error e1, Error e2 -> e1 = e2
               | _ -> false))
          ""
    check "validateDegrees accepts every menu shape and the two documented sequences"
          (shapeMenu |> List.forall (fun (_, s) -> validateDegrees (toDegrees s) = Ok())
           && validateDegrees [ 2; 2; 0; 0; 3; 0; 0; 0 ] = Ok()
           && validateDegrees [ 3; 0; 2; 0; 2; 0; 0; 3; 1; 0; 0; 0 ] = Ok())
          ""
    check "treeTablesUnchecked RAISES on a malformed sequence -- it never returns junk tables"
          (try
              treeTablesUnchecked [ 2; 0 ] |> ignore
              false
           with _ -> true)
          ""

    // ---- read refusals: an out-of-domain path is never a plausible offset ---
    check "treeForward refuses out-of-arity, over-long, and negative steps"
          (isError (treeForwardChecked tCrystal [ 0; 2 ])       // node 1 has 2 children
           && isError (treeForwardChecked tCrystal [ 2 ])       // the root has 2 children
           && isError (treeForwardChecked tCrystal [ 0; 0; 0 ]) // past a leaf
           && isError (treeForwardChecked tCrystal [ -1 ])
           && isError (treeForwardChecked tSkew [ 1; 1; 2 ]))
          ""
    check "treeForward refuses a path stopping at an INTERNAL node -- that is a subtree, not a cell"
          (isError (treeForwardChecked tCrystal [])
           && isError (treeForwardChecked tCrystal [ 0 ])
           && isError (treeForwardChecked tSkew [ 1; 1 ])
           // and the same prefixes resolve perfectly well through the subtree door
           && treeSubtreeChecked tCrystal [ 0 ] = Ok(view 1 0 2)
           && treeSubtreeChecked tSkew [ 1; 1 ] = Ok(view 4 2 2))
          ""
    check "treeBackward is total on [0, cardinality) and refuses outside it"
          (isError (treeBackwardChecked tCrystal (-1))
           && isError (treeBackwardChecked tCrystal 5)
           && isError (treeBackwardChecked tLeaf 1)
           && (treeBackwardChecked tCrystal 4 |> Result.map showPath) = Ok "(1,2)")
          ""
    check "treeSubtree refuses an out-of-arity prefix; a COMPLETE path is its own one-cell block"
          (isError (treeSubtreeChecked tCrystal [ 0; 2 ])
           && isError (treeSubtreeChecked tCrystal [ 5 ])
           && treeSubtreeChecked tCrystal [ 1; 2 ] = Ok(view 7 4 1))
          ""
    check "treeChild refuses a bad node id and a bad child index"
          (isError (treeChildChecked tCrystal 0 2)
           && isError (treeChildChecked tCrystal 2 0)      // node 2 is a leaf
           && isError (treeChildChecked tCrystal (-1) 0)
           && isError (treeChildChecked tCrystal 8 0)
           && treeChildChecked tCrystal 0 1 = Ok 4)
          ""
    check "each read refusal names the failing STEP, not a bare offset"
          ((match treeForwardChecked tSkew [ 1; 1; 2 ] with
            | Error e -> e.Contains "step 2" && e.Contains "child 2"
            | Ok _ -> false)
           && (match treeSubtreeChecked tCrystal [ 0; 2 ] with
               | Error e -> e.Contains "step 1"
               | Ok _ -> false))
          ""

    printFooter "TreeIdx bijections"
                [ sprintf "%d passed" passed
                  sprintf "%d failed" failed
                  sprintf "%d shapes" (List.length shapeMenu + rndShapes)
                  sprintf "%d nodes / %d paths swept" (sweptNodes + rndNodes) (sweptPaths + rndPaths) ]
    { Block = "TreeIdx Bijections"; Passed = passed; Failed = failed; Skipped = 0; FailedNames = failedNames }
