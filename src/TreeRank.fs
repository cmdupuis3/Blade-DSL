// TreeRank.fs -- the pure-function layer for TreeIdx: shape validation, the
// derived preorder tables, and the path <-> leaf-offset bijection.
//
// A tree shape is a PREORDER DEGREE SEQUENCE: one entry per node, holding that
// node's child count, in depth-first preorder.
//
//     crystal = [[leaf, leaf], [leaf, leaf, leaf]]  =>  deg = [2;2;0;0;3;0;0;0]
//
// That sequence is the ONLY form the type carries (features/graphs-trees.md
// §2.3); `sizes`, `off`, cardinality, depth, the leaf set and every subtree
// shape are derived from it in one linear pass, right here. Everything in this
// file is a function of the degree sequence only -- FSharp.Core, no Blade
// module opened, no project dependency -- so proofs/ scripts and external
// checkers can `#load` it standalone, exactly as OrbRank.fs is loadable.
//
// THE DOMAIN, stated once and loudly (plans/plan-graphs-trees.md §10, the P0
// appendix). A `TreeIdx<s>`'s value domain is the set of COMPLETE root-to-leaf
// paths, not the set of nodes. Consequently:
//
//   * cardinality = LEAF count, not node count. `deg` has one entry per node,
//     but the array has one cell per leaf. The two lengths differ and mixing
//     them up is the one arithmetic mistake this file exists to make impossible
//     (every table below is documented as node-indexed or leaf-indexed).
//   * leaves in preorder ARE lexicographic order on paths, so the formalism
//     §3.2 enumeration obligation holds by construction, not by a sort.
//   * internal nodes are addressable only as SUBTREE VIEWS -- `treeSubtree`,
//     which resolves a partial path to a node and the CONTIGUOUS leaf block
//     under it. Contiguity is the preorder invariant that pays for the whole
//     storage story (feature doc §1.2).
//
// What is here:
//   validateDegrees    the ONE structural gate: n >= 1, every degree >= 0, and
//                      the prefix walk closes EXACTLY (pending starts at 1;
//                      each node needs pending >= 1 then does
//                      pending <- pending - 1 + deg[i]; pending = 0 at the end).
//                      Four distinct refusals: empty, negative, early close,
//                      truncated. Every entry point below Errors through it.
//   treeTables         the one construction function: validates, then builds
//                      the whole derived record (sizes / leafCount / leafStart /
//                      parent / childIndex / depth / the child CSR / the leaf
//                      node table) in O(n).
//   treeForward        path -> leaf offset. O(|path|) table steps.
//   treeBackward       leaf offset -> path, its total inverse on [0, card).
//   treeSubtree        partial path -> (node, leafStart, leafCount); the empty
//                      prefix is the root, i.e. the whole array.
//   treePaths          every complete path in leaf-offset order. Written as an
//                      INDEPENDENT DFS -- it never reads LeafStart -- so
//                      `treePaths` and `treeForward` are two mechanisms whose
//                      agreement tests/Test_TreeRank.fs can actually pin.
//
// INVARIANT ("the one hard constraint", the TreeIdx twin of OrbRank's):
// leaf-offset order = the preorder DFS emission order = ascending lex on paths.
// tests/Test_TreeRank.fs asserts it against a brute-force recursive enumerator
// over an independent nested-shape ADT -- as a SET and as an ORDER, because a
// forward/backward round trip cancels an order convention exactly.
//
// CHECKED VS UNCHECKED, and why the split is not OrbRank's. OrbRank's
// `*Checked` suffix means OVERFLOW-checked: its binomials genuinely leave
// int64. Nothing here can overflow -- every count is bounded by the node count,
// which is a list length, so `int` is the natural and sufficient width (the
// validation walk's accumulator is the one int64, because a hostile degree
// entry could otherwise wrap the pending counter before the walk refuses it).
// So `*Checked` here means DOMAIN-checked: `treeForwardChecked` /
// `treeSubtreeChecked` / `treeBackwardChecked` return `Result<_, string>` and
// name the failing step, while the bare `treeForward` / `treeSubtree` /
// `treeBackward` are the guard-free hot path whose caller warrants the path is
// in-domain. An out-of-domain argument to a bare form lands wherever the
// arithmetic lands or raises -- it is never a diagnosis. Compiler seams call
// the checked form; a loop nest that already emitted only valid children calls
// the bare one.

module Blade.TreeRank

// ---------------------------------------------------------------------------
// The shape
// ---------------------------------------------------------------------------

/// A path, for message text: `(0,1)`, and `()` for the root path.
let showPath (p: int list) =
    "(" + (p |> List.map string |> String.concat ",") + ")"

/// A degree sequence, for message text: `[2,2,0,0,3,0,0,0]`.
let showDegrees (deg: int list) =
    "[" + (deg |> List.map string |> String.concat ",") + "]"

/// Structural validation of a shape, shared by EVERY entry point below, so a
/// malformed sequence cannot draw different verdicts from different doors.
///
/// A preorder degree sequence is well formed iff it has at least one node,
/// no negative degree, and the prefix walk closes exactly: `pending` (the
/// number of child slots still waiting for a node) starts at 1 for the root,
/// every node must find `pending >= 1`, and consumes one slot while opening
/// `deg[i]` of its own. After the last node `pending` must be 0.
///
/// The two ways it can fail are genuinely different mistakes and get different
/// messages: the walk closing before the sequence ends (a trailing tail that
/// belongs to no tree) versus the sequence ending before the walk closes
/// (slots nothing fills). `pending` is int64 so a hostile degree entry cannot
/// wrap the counter into a plausible-looking close.
let validateDegrees (deg: int list) : Result<unit, string> =
    let n = List.length deg
    if n = 0 then
        Error "empty degree sequence: a tree has at least one node"
    else
        let a = List.toArray deg
        let mutable err: string option = None
        let mutable pending = 1L
        let mutable i = 0
        while err.IsNone && i < n do
            if a.[i] < 0 then
                err <- Some $"node {i}: degree {a.[i]} must be >= 0"
            elif pending < 1L then
                err <- Some $"node {i}: the degree sequence closes early -- the tree is complete after {i} node(s), but {n} were given"
            else
                pending <- pending - 1L + int64 a.[i]
                i <- i + 1
        match err with
        | Some e -> Error e
        | None ->
            if pending <> 0L then
                Error $"truncated degree sequence: {pending} child slot(s) still unfilled after {n} node(s)"
            else Ok()

// ---------------------------------------------------------------------------
// The derived tables
// ---------------------------------------------------------------------------

/// Everything derivable from a well-formed degree sequence, in one record,
/// built by one O(n) construction (`treeTables`). Two index spaces meet here
/// and every field says which one it lives in:
///
///   NODE-indexed (length `nodes`): Deg, Sizes, LeafCount, LeafStart, Parent,
///                                  ChildIndex, Depth
///   LEAF-indexed (length `card = LeafCount.[0]`): LeafNodes
///   CSR:                           ChildOff (nodes+1), Children (nodes-1)
///
/// `LeafStart` IS the feature doc §1.2 `off` table read in the path domain:
/// node i's cells are exactly `LeafStart.[i] .. LeafStart.[i] + LeafCount.[i] - 1`,
/// contiguous by the preorder invariant. `Sizes` is the node-count table `size`,
/// which is what a subtree VIEW needs (how much of `deg` the sub-shape spans),
/// as opposed to how many cells it owns.
type TreeTables =
    { /// deg.[i] = child count of node i, in depth-first preorder. The shape.
      Deg: int[]
      /// sizes.[i] = number of NODES in the subtree rooted at i (>= 1, and
      /// sizes.[0] = nodes). Node i's subtree is `deg[i .. i + sizes[i] - 1]`.
      Sizes: int[]
      /// leafCount.[i] = number of complete paths under i (>= 1). CELLS, not nodes.
      LeafCount: int[]
      /// leafStart.[i] = leaf offset of the FIRST leaf of i's subtree.
      LeafStart: int[]
      /// parent.[i], and -1 at the root.
      Parent: int[]
      /// childIndex.[i] = i's position among its parent's children; 0 at the root.
      ChildIndex: int[]
      /// depth.[i], 0 at the root; equals the length of i's path from the root.
      Depth: int[]
      /// CSR over the child lists: node i's children are
      /// `Children.[ChildOff.[i] .. ChildOff.[i+1] - 1]`, in sibling order.
      /// ChildOff is the prefix sum of Deg, so ChildOff.[nodes] = nodes - 1.
      ChildOff: int[]
      Children: int[]
      /// leafNodes.[k] = the node id of the k-th leaf in preorder; the inverse
      /// of `LeafStart` restricted to leaves.
      LeafNodes: int[] }

/// Node count -- the length of the degree sequence.
let nodeCount (t: TreeTables) = t.Deg.Length

/// CARDINALITY: the number of cells a `TreeIdx<s>` array holds = the leaf count
/// = the number of complete root-to-leaf paths. NOT the node count.
let cardinality (t: TreeTables) = t.LeafCount.[0]

/// The deepest leaf's depth (0 for the single-leaf shape).
let maxDepth (t: TreeTables) = Array.max t.Depth

/// Is node i a leaf -- i.e. does a complete path end there?
let isLeaf (t: TreeTables) (node: int) = t.Deg.[node] = 0

/// Node i's child count (its arity).
let degreeOf (t: TreeTables) (node: int) = t.Deg.[node]

/// The node id of node's k-th child. Guard-free: the caller warrants
/// `0 <= k < deg[node]`. The CSR this reads was built by exactly the walk the
/// feature doc states -- children of i start at i+1 and each next child is the
/// previous one plus its subtree size -- so this is that walk, precomputed.
let treeChild (t: TreeTables) (node: int) (k: int) = t.Children.[t.ChildOff.[node] + k]

/// Domain-checked `treeChild`.
let treeChildChecked (t: TreeTables) (node: int) (k: int) : Result<int, string> =
    if node < 0 || node >= t.Deg.Length then
        Error $"treeChild: node {node} outside [0,{t.Deg.Length})"
    elif k < 0 || k >= t.Deg.[node] then
        Error $"treeChild: child {k} outside [0,{t.Deg.[node]}) at node {node}"
    else Ok(treeChild t node k)

/// Build every derived table from a degree sequence. This is the checked door:
/// the sequence goes through `validateDegrees` first, so the construction
/// itself needs no guards.
///
/// Three linear passes: (1) `ChildOff` = prefix sums of `Deg`; (2) a REVERSE
/// scan filling `Sizes` and `LeafCount` (a node's children are already done,
/// since preorder puts every descendant after its ancestor); (3) a FORWARD scan
/// filling `Parent`, `ChildIndex`, `Depth`, `LeafStart`, `Children` and
/// `LeafNodes`, walking each node's children by the size-stride rule. Total
/// work is O(nodes + edges) = O(nodes).
let treeTables (deg: int list) : Result<TreeTables, string> =
    match validateDegrees deg with
    | Error e -> Error e
    | Ok() ->
        let d = List.toArray deg
        let n = d.Length
        // (1) the child CSR's offsets.
        let childOff = Array.zeroCreate (n + 1)
        for i in 0 .. n - 1 do
            childOff.[i + 1] <- childOff.[i] + d.[i]
        // (2) subtree node counts and leaf counts, innermost first.
        let sizes = Array.create n 1
        let leafCount = Array.create n 1
        for i in n - 1 .. -1 .. 0 do
            if d.[i] > 0 then
                let mutable c = i + 1
                let mutable nodes = 1
                let mutable leaves = 0
                for _ in 1 .. d.[i] do
                    nodes <- nodes + sizes.[c]
                    leaves <- leaves + leafCount.[c]
                    c <- c + sizes.[c]
                sizes.[i] <- nodes
                leafCount.[i] <- leaves
        // (3) the parent/depth/offset pass, plus the child and leaf tables.
        let parent = Array.create n (-1)
        let childIndex = Array.zeroCreate n
        let depth = Array.zeroCreate n
        let leafStart = Array.zeroCreate n
        let children = Array.zeroCreate (max 0 (n - 1))
        let leafNodes = Array.zeroCreate leafCount.[0]
        for i in 0 .. n - 1 do
            if d.[i] = 0 then leafNodes.[leafStart.[i]] <- i
            else
                let mutable c = i + 1
                let mutable off = leafStart.[i]
                for k in 0 .. d.[i] - 1 do
                    parent.[c] <- i
                    childIndex.[c] <- k
                    depth.[c] <- depth.[i] + 1
                    leafStart.[c] <- off
                    children.[childOff.[i] + k] <- c
                    off <- off + leafCount.[c]
                    c <- c + sizes.[c]
        Ok { Deg = d
             Sizes = sizes
             LeafCount = leafCount
             LeafStart = leafStart
             Parent = parent
             ChildIndex = childIndex
             Depth = depth
             ChildOff = childOff
             Children = children
             LeafNodes = leafNodes }

/// RAW form of `treeTables`, for `#load` / oracle use where an exception is the
/// right loudness: a malformed sequence RAISES with the same text the checked
/// door returns, rather than producing tables nothing can trust.
let treeTablesUnchecked (deg: int list) : TreeTables =
    match treeTables deg with
    | Ok t -> t
    | Error e -> failwith $"TreeRank.treeTablesUnchecked: {e}"

// ---------------------------------------------------------------------------
// forward : Path -> Offset
// ---------------------------------------------------------------------------

/// Resolve a path prefix to the node it names, guard-free. The caller warrants
/// every step is in arity; see the header's checked/unchecked note.
let private descend (t: TreeTables) (path: int list) : int =
    let mutable node = 0
    for c in path do
        node <- t.Children.[t.ChildOff.[node] + c]
    node

/// Leaf offset of a COMPLETE root-to-leaf path. Guard-free: the caller warrants
/// the path is in-domain (every step in arity, and the last node a leaf).
///
/// The offset is the landing node's `LeafStart`, which for a leaf is that
/// leaf's own preorder index -- so this is O(|path|) table steps and one final
/// load, the "O(k) arithmetic + 1 access" of the feature doc's cost table.
let treeForward (t: TreeTables) (path: int list) : int =
    t.LeafStart.[descend t path]

/// Domain-checked `treeForward`: names the failing STEP, so a path that goes
/// wrong three levels down does not report as a bare out-of-range offset.
/// Refuses an over-long or out-of-arity path, and refuses a path that stops at
/// an internal node -- that is a subtree, not a cell, and `treeSubtree` is its
/// door.
let treeForwardChecked (t: TreeTables) (path: int list) : Result<int, string> =
    let mutable node = 0
    let mutable err: string option = None
    let mutable step = 0
    for c in path do
        if err.IsNone then
            let deg = t.Deg.[node]
            if c < 0 || c >= deg then
                err <-
                    Some $"treeForward: path {showPath path} step {step}: child {c} outside [0,{deg}) at node {node}"
            else
                node <- treeChild t node c
                step <- step + 1
    match err with
    | Some e -> Error e
    | None ->
        if t.Deg.[node] <> 0 then
            Error $"treeForward: path {showPath path} ends at internal node {node} with {t.Deg.[node]} child(ren) -- a complete path must end at a leaf"
        else Ok t.LeafStart.[node]

// ---------------------------------------------------------------------------
// backward : Offset -> Path
// ---------------------------------------------------------------------------

/// The path of the leaf at `off`. Guard-free: the caller warrants
/// `0 <= off < cardinality t`.
///
/// Total on [0, cardinality) by construction -- `LeafNodes` names the leaf and
/// the `Parent`/`ChildIndex` chain reads the path off backwards -- so the
/// inverse needs no search and no descent by leaf counts.
let treeBackward (t: TreeTables) (off: int) : int list =
    let mutable node = t.LeafNodes.[off]
    let acc = ResizeArray<int>()
    while t.Parent.[node] >= 0 do
        acc.Add t.ChildIndex.[node]
        node <- t.Parent.[node]
    acc.Reverse()
    List.ofSeq acc

/// Domain-checked `treeBackward`.
let treeBackwardChecked (t: TreeTables) (off: int) : Result<int list, string> =
    let card = cardinality t
    if off < 0 || off >= card then Error $"treeBackward: leaf offset {off} outside [0,{card})"
    else Ok(treeBackward t off)

// ---------------------------------------------------------------------------
// subtree : PartialPath -> (node, leaf block)
// ---------------------------------------------------------------------------

/// A resolved partial path: the node it names and the CONTIGUOUS block of leaf
/// offsets under it, `[LeafStart, LeafStart + LeafCount)`. Contiguity is the
/// preorder invariant, so a subtree view is pointer + length + sub-shape --
/// structurally a peeled `RaggedRow<T>` (feature doc §1.2).
type TreeSubtree =
    { Node: int
      LeafStart: int
      LeafCount: int }

/// Resolve a partial path to its subtree. Guard-free; the empty prefix is the
/// root, i.e. the whole array.
let treeSubtree (t: TreeTables) (prefix: int list) : TreeSubtree =
    let node = descend t prefix
    { Node = node; LeafStart = t.LeafStart.[node]; LeafCount = t.LeafCount.[node] }

/// Domain-checked `treeSubtree`, naming the failing step exactly as
/// `treeForwardChecked` does. Note there is no leaf/internal requirement here:
/// a complete path resolves to its own one-cell block, which is the degenerate
/// but correct answer.
let treeSubtreeChecked (t: TreeTables) (prefix: int list) : Result<TreeSubtree, string> =
    let mutable node = 0
    let mutable err: string option = None
    let mutable step = 0
    for c in prefix do
        if err.IsNone then
            let deg = t.Deg.[node]
            if c < 0 || c >= deg then
                err <-
                    Some $"treeSubtree: prefix {showPath prefix} step {step}: child {c} outside [0,{deg}) at node {node}"
            else
                node <- treeChild t node c
                step <- step + 1
    match err with
    | Some e -> Error e
    | None -> Ok { Node = node; LeafStart = t.LeafStart.[node]; LeafCount = t.LeafCount.[node] }

// ---------------------------------------------------------------------------
// The enumeration
// ---------------------------------------------------------------------------

/// Every complete path under `node`, in DFS child order, `prefix` reversed on
/// the way down. Deliberately reads only `Deg` and the child CSR: it never
/// touches `LeafStart` or `LeafNodes`, so the enumeration and the bijection are
/// two mechanisms and their agreement is a real pin, not a tautology.
let rec private pathsUnder (t: TreeTables) (node: int) (rev: int list) : seq<int list> =
    if t.Deg.[node] = 0 then Seq.singleton (List.rev rev)
    else
        seq {
            for k in 0 .. t.Deg.[node] - 1 do
                yield! pathsUnder t (t.Children.[t.ChildOff.[node] + k]) (k :: rev)
        }

/// EVERY complete path, in leaf-offset order. Lazy.
///
/// CONTRACT (the file's invariant, restated where it is produced): this order
/// is ascending LEXICOGRAPHIC on paths, and it equals the storage order, i.e.
/// `treeForward t (Seq.item k (treePaths t)) = k` for every k. Nothing sorts;
/// depth-first child-ascending emission IS lex, because no complete path is a
/// prefix of another (a leaf has no children). tests/Test_TreeRank.fs pins both
/// halves against an independent enumerator.
let treePaths (t: TreeTables) : seq<int list> = pathsUnder t 0 []

/// The complete paths of ONE subtree, in its own leaf-offset order; the k-th of
/// these is global offset `LeafStart + k`. Guard-free (`treeSubtreeChecked` is
/// the validating door).
let treeSubtreePaths (t: TreeTables) (prefix: int list) : seq<int list> =
    pathsUnder t (descend t prefix) []
