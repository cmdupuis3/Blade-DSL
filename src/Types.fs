// Blade type-system core: the expression-independent layer (element types,
// symmetry/kind classifiers, unit signatures), usable without IR.fs's
// 6k-line type/expression knot. IRType/IRIndexType stay in IR.fs since
// IRIndexType.Extent is an IRExpr; the boundary a decoupling rewrite would extend.
module Blade.Types

type IRId = int

/// Symmetry class for index types
type SymmetryClass =
    | SymNone          // No symmetry (dense)
    | SymSymmetric     // Symmetric (i <= j)
    | SymAntisymmetric // Antisymmetric (i < j, negate on swap)
    | SymHermitian     // Hermitian (conjugate on swap)
    /// ITERATED WREATH (`OrbIdx<[(r1,s1),...,(rd,sd)], n>`, d >= 2 after
    /// normalization -- docs/plan-orbit-index-types.md section 2): S_r1 wr
    /// ... wr S_rd on prod(ri) axes, character = product of per-level signs;
    /// placed via an iterated-binomial fold, not a single combinadic.
    /// Nullary DELIBERATELY: level list rides Extent as `IROrbitClass`,
    /// forcing every exhaustive match to decide this case. Depth 1 never
    /// reaches here -- `OrbIdx<[(r,+/-)],n>` normalizes to Sym/Antisym,
    /// empty to plain `Idx<n>`.
    | SymWreath

/// Per-parameter SIGN parity of a kernel, recorded on the callable for the
/// wreath-tie soundness gate (`IRLoopStructure.deduceWreathTie`); mirrors
/// `Deduce.SignParity` (IRCallable sits below Deduce.fs in compile order).
/// TypeCheck is the sole producer; empty means "never computed" (all-KspUnknown).
type KernelSignParity =
    | KspOdd      // provably f(.., -x, ..) = -f(.., x, ..) in that parameter
    | KspEven     // provably f(.., -x, ..) =  f(.., x, ..) in that parameter
    | KspUnknown  // neither provable

/// Placement (membership + ranking) class for index types -- the Level-1
/// axis, orthogonal to the symmetry-transform axis (SymmetryClass): which
/// tuples are stored and how a tuple ranks to a flat offset.
/// PlaceDense for SymNone; PlaceCombinatorial (carrying SymmetryClass, so
/// ranking distinguishes inclusive sym/herm from strict antisym) for the
/// rest. Tabulated types (CompoundIdx/SparseIdx) diverge: validity is
/// mask-/list-derived, not symmetry-derived. Exhaustiveness (FS0025) here
/// is deliberate -- adding a case forces every dispatch site to update.
type PlacementClass =
    | PlaceDense                          // row-major dense (Idx, EnumIdx)
    | PlaceCombinatorial of SymmetryClass // CNS-ranked compact (sym/antisym/herm)
    | PlaceTabulated                      // mask/list-derived, runtime table (CompoundIdx)

/// Core Level-1 classifier on the symmetry axis; the shared entry point
/// for placement-proxy sites (cardinality, compact grouping, allocator
/// choice) that read SymmetryClass today.
let placementClassOf (sym: SymmetryClass) : PlacementClass =
    match sym with
    | SymNone -> PlaceDense
    // Wreath is closed-form placed (iterated binomial, one C(.,.) per level)
    // so it's Combinatorial not Tabulated -- but not a single combinadic
    // over `Rank`: consumers must read the level list off the Extent
    // marker, not assume C(n+r-1,r). `bufferGroupCardinality` does this.
    | SymWreath -> PlaceCombinatorial SymWreath
    | SymSymmetric | SymAntisymmetric | SymHermitian -> PlaceCombinatorial sym

/// ORBIT PLACEMENT (design skeleton, NOT WIRED IN): generalizes the two
/// cases above as a TRIPLE -- R positions, POSITION GROUP G <= S_R of
/// interchangeable slot permutations, CHARACTER chi: G -> {+1,-1,conj}
/// (`SymmetryClass` above IS chi; `PositionGroup` the missing half).
/// Closed-form rank only for PgFullSym (C(N+R-1,R) inclusive / C(N,R)
/// strict -- BladeDMWF/BladeBinomial, today's comm/antisymm) and
/// PgProduct (prod_j C(N_j+R_j-1,R_j) -- BladeMixedRadix, tied-perm
/// layout); PgOpaque varies in stabilizer size, a Burnside sum with no
/// closed form (BladeCounting.v), PlaceTabulated. ZERO-SET: G-stabilizer
/// chi = -1 forces zero (v = chi(h)*v = -v) -- why PlaceCombinatorial
/// carries SymmetryClass, generalizing the antisymmetric zero-diagonal.
type PositionGroup =
    | PgTrivial                       // G = 1                -> dense
    | PgFullSym of rank: int          // G = S_R              -> closed form
    | PgProduct of ranks: int list    // G = prod_j S_{R_j}   -> mixed radix
    | PgOpaque of tag: string         // any other finite G   -> runtime table

/// Placement from the (G, chi) pair. Agrees with `placementClassOf` on
/// every shipped index type (`SymIdx<R,N>` is `PgFullSym R`, dense
/// `Idx<N>` is `PgTrivial`) -- a strict generalization, not a rewrite.
let placementOfOrbit (g: PositionGroup) (chi: SymmetryClass) : PlacementClass =
    match g with
    | PgTrivial -> PlaceDense
    | PgFullSym _ -> PlaceCombinatorial chi
    | PgProduct _ -> PlaceCombinatorial chi
    | PgOpaque _ -> PlaceTabulated

/// Commutativity/Symmetry state at each loop level (Section 13.1):
/// determines whether triangular iteration is valid at this position.
type SymcomState =
    | SCNeither       // Independent iteration - no optimization
    | SCSymmetric     // Same array, symmetric dimension - triangular valid
    | SCCommutative   // Different arrays but in comm group - triangular valid
    | SCBoth          // Same array + comm group - triangular valid, best case

// Array Identity Tracking (critical for symmetry exploitation)

/// Tracks the identity of arrays for commutativity detection
type ArrayIdentity =
    | AIDLiteral of IRId                    // Literal array with unique ID
    | AIDVariable of name: string           // Named variable
    | AIDParameter of name: string * idx: int  // Function parameter
    | AIDDerived of base': ArrayIdentity * op: string  // Derived from another array

/// Check if two array identities are the same
let sameIdentity a b =
    match a, b with
    | AIDVariable n1, AIDVariable n2 -> n1 = n2
    | AIDParameter (n1, i1), AIDParameter (n2, i2) -> n1 = n2 && i1 = i2
    | AIDLiteral id1, AIDLiteral id2 -> id1 = id2
    | _ -> false

// Index Types with Dependency Tracking

/// Dimension kind: S-dimensions (spatial, participate in symmetry
/// optimization) vs T-dimensions (temporal/time, do not).
type DimensionKind =
    | SDimension   // Spatial dimension - participates in symmetry
    | TDimension   // Temporal dimension - does not participate in symmetry

/// Reserved KIND of an index type (audit section 3.3): the discriminator
/// alongside `Tag` ("__..." sentinel strings sharing a namespace with user
/// index-type names) -- Tag is for NAMES, IxKind for KINDS. Constructors
/// mirror the sentinel into Tag too; the IR validator enforces the two AGREE.
/// All kind DISPATCH reads IxKind, directly or via IxSymmetryLike/
/// IxCompound/... active patterns.
type IxKind =
    | IxKPlain              // ordinary index type (user-named or anonymous)
    | IxKCompound           // "__compoundidx": masked product space
    | IxKCompoundDynamic    // "__compoundidx_dynamic": mask known only at runtime
    | IxKSparse             // "__sparseidx": explicit key enumeration, hash lookup
    | IxKOrbit              // "__orbidx": iterated-wreath (OrbIdx), depth >= 2.
                            // (rank,sign) level list rides Extent as IROrbitClass;
                            // Rank = product of level ranks. Always paired with
                            // Symmetry = SymWreath (depth <=1 normalizes away).
    | IxKDep                // "__depidx": dependent-extent head marker
    | IxKDepInner           // "__depidx_inner": the dependent inner dimension
    | IxKDepOuter           // "__depidx_outer": the outer dim a DepIdx depends on
    | IxKRagged             // "__raggedidx": ragged (per-row extent) dimension
    | IxKRaggedInline       // "__raggedidx_inline": ragged with inline lengths
    | IxKRaggedOpaque       // "__raggedidx_opaque": context-supplied extent
    | IxKGroupOuter         // "__group_outer": group_by outer (per-group) slot
    | IxKGroupMember        // "__group_member": group_by member slot
    | IxKSeq                // "__seq": sequence-combinator-produced dimension
    | IxKIrreps             // "__irreps:<name>:<payload>": block-structured dense
                            // index over an equivariant irreps spec; the tag is
                            // PARAMETERIZED (spec payload + optional alias name),
                            // so the kind maps from the prefix, not one sentinel
    | IxKPgIrreps           // "__pgirreps:<group>:<name>:<payload>": the SECOND
                            // block-spec member (point groups); same
                            // parameterized-tag discipline as IxKIrreps, over a
                            // DIFFERENT frozen prefix (O(3) format is byte-frozen).
    | IxKErrorRaggedNoPrior // "__error_ragged_no_prior": typecheck error marker
    | IxKErrorIrrepsBadSpec // "__error_irreps_bad_spec": typecheck error marker
    | IxKErrorPgIrrepsBadSpec // "__error_pgirreps_bad_spec": typecheck error marker

/// The Tag sentinel for a kind (None for IxKPlain). The single source of
/// the kind<->sentinel correspondence, used by constructors that mirror
/// the kind into Tag and by the validator's agreement check.
let ixKindSentinel (k: IxKind) : string option =
    match k with
    | IxKPlain -> None
    | IxKCompound -> Some "__compoundidx"
    | IxKCompoundDynamic -> Some "__compoundidx_dynamic"
    | IxKSparse -> Some "__sparseidx"
    | IxKOrbit -> Some "__orbidx"
    | IxKDep -> Some "__depidx"
    | IxKDepInner -> Some "__depidx_inner"
    | IxKDepOuter -> Some "__depidx_outer"
    | IxKRagged -> Some "__raggedidx"
    | IxKRaggedInline -> Some "__raggedidx_inline"
    | IxKRaggedOpaque -> Some "__raggedidx_opaque"
    | IxKGroupOuter -> Some "__group_outer"
    | IxKGroupMember -> Some "__group_member"
    | IxKSeq -> Some "__seq"
    | IxKIrreps -> None     // parameterized tag (mkIrrepsTag), no single sentinel;
                            // Tag missing the prefix FAILS validator agreement (intended)
    | IxKPgIrreps -> None   // ditto (mkPgIrrepsTag)
    | IxKErrorRaggedNoPrior -> Some "__error_ragged_no_prior"
    | IxKErrorIrrepsBadSpec -> Some "__error_irreps_bad_spec"
    | IxKErrorPgIrrepsBadSpec -> Some "__error_pgirreps_bad_spec"

// IrrepsIdx tag encoding: the spec payload rides IN the Tag string. Tag
// equality is already index-space identity everywhere, so no side registry
// or new IRIndexTypeG field is needed. Format: "__irreps:<name>:<payload>",
// <name> = alias ("" if anonymous), <payload> = "l,p,m|l,p,m|..." in spec
// order. Pure string ops; core stays ML-free, Blade.ML owns decoding.

let irrepsTagPrefix = "__irreps:"

/// Serialize an irreps spec (+ optional alias name) into its canonical Tag.
let mkIrrepsTag (aliasName: string option) (spec: (int * int * int) list) : string =
    let payload =
        spec |> List.map (fun (l, p, m) -> $"{l},{p},{m}") |> String.concat "|"
    $"""{irrepsTagPrefix}{(defaultArg aliasName "")}:{payload}"""

/// Parse an irreps Tag back into (alias name option, (l, parity, mult) list).
/// Total: any string not produced by mkIrrepsTag yields None.
let (|IrrepsTag|_|) (tag: string) : (string option * (int * int * int) list) option =
    if not (tag.StartsWith irrepsTagPrefix) then None
    else
        let rest = tag.Substring irrepsTagPrefix.Length
        match rest.IndexOf ':' with
        | -1 -> None
        | sep ->
            let name = rest.Substring(0, sep)
            let entryOf (s: string) =
                match s.Split ',' with
                | [| l; p; m |] ->
                    match System.Int32.TryParse l, System.Int32.TryParse p, System.Int32.TryParse m with
                    | (true, lv), (true, pv), (true, mv) -> Some (lv, pv, mv)
                    | _ -> None
                | _ -> None
            let entries = rest.Substring(sep + 1).Split '|' |> Array.toList |> List.map entryOf
            if List.forall Option.isSome entries then
                Some ((if name = "" then None else Some name), List.map Option.get entries)
            else None

// PgIrrepsIdx tag encoding -- the SECOND block-spec member, same discipline
// as the irreps tag but a DELIBERATELY SEPARATE format (O(3) `__irreps:` is
// BYTE-FROZEN): "__pgirreps:<group>:<name>:<payload>". <group> = point
// group's frozen registry name ("C4", "D4"); <name> = alias ("" if
// anonymous); <payload> = "LABEL,mult|..." in spec order. LABEL NAMES ride
// in the tag on purpose (frozen table data; the tag IS the diagnostic
// identity). Pure string ops; Blade.ML owns decoding incl. the
// unknown-label diagnostic. The two prefixes are disjoint, so tag equality
// decides CROSS-MEMBER identity for free: `IrrepsIdx<s>` and
// `PgIrrepsIdx<G, s'>` of the same extent are always distinct types.

let pgIrrepsTagPrefix = "__pgirreps:"

/// Serialize a point-group spec (group name + optional alias name +
/// (LABEL, mult) entries) into its canonical Tag.
let mkPgIrrepsTag (group: string) (aliasName: string option) (spec: (string * int) list) : string =
    let payload =
        spec |> List.map (fun (label, m) -> $"{label},{m}") |> String.concat "|"
    $"""{pgIrrepsTagPrefix}{group}:{(defaultArg aliasName "")}:{payload}"""

/// Parse a pg-irreps Tag back into (group, alias name option, (LABEL, mult) list).
/// Total: any string not produced by mkPgIrrepsTag yields None.
let (|PgIrrepsTag|_|) (tag: string) : (string * string option * (string * int) list) option =
    if not (tag.StartsWith pgIrrepsTagPrefix) then None
    else
        let rest = tag.Substring pgIrrepsTagPrefix.Length
        match rest.IndexOf ':' with
        | -1 -> None
        | sepG ->
            let group = rest.Substring(0, sepG)
            let rest2 = rest.Substring(sepG + 1)
            match rest2.IndexOf ':' with
            | -1 -> None
            | sepN ->
                let name = rest2.Substring(0, sepN)
                let entryOf (s: string) =
                    match s.Split ',' with
                    | [| label; m |] when label <> "" ->
                        match System.Int32.TryParse m with
                        | true, mv -> Some (label, mv)
                        | _ -> None
                    | _ -> None
                let entries = rest2.Substring(sepN + 1).Split '|' |> Array.toList |> List.map entryOf
                if group <> "" && List.forall Option.isSome entries then
                    Some (group, (if name = "" then None else Some name), List.map Option.get entries)
                else None

// Halo window tag encoding. Like IrrepsIdx, the payload rides IN the Tag:
// "__halowin|<k>:<innerName>|<o1,o2,..>", <k> = 'd' (dense inner) or 'c'
// (compound inner: ordinals walk PRESENT cells), <innerName> = wrapped
// index's alias, csv = static signed offset set (center = 0). Loop building
// re-derives the center's start offset per-slot (shared IRRange offset
// can't express multi-slot ranges); window reads re-derive offset set/kind.

let haloWinTagPrefix = "__halowin|"

/// Parse a halo window Tag into (isCompound, inner alias name, offset list).
/// Total: any string not shaped like a halo tag yields None.
let (|HaloWinTag|_|) (tag: string) : (bool * string * int list) option =
    if not (tag.StartsWith haloWinTagPrefix) then None
    else
        match tag.Split '|' with
        | [| _; marked; csv |] when marked.Length >= 2 && (marked.[0] = 'd' || marked.[0] = 'c') && marked.[1] = ':' ->
            let offs =
                csv.Split ',' |> Array.toList
                |> List.map (fun s -> match System.Int32.TryParse s with true, v -> Some v | _ -> None)
            if List.forall Option.isSome offs && not offs.IsEmpty then
                Some (marked.[0] = 'c', marked.Substring 2, List.map Option.get offs)
            else None
        | _ -> None

/// The halo ACCESS DESCRIPTION (docs/plans/structural/02, section 2.1): which
/// input ordinals one output ordinal's window may read. Parsed ONCE from the
/// slot tag (or built from the surface literals by the AD lane), and read by
/// every consumer -- the checker's offset and extent guards, the codegen and
/// interpreter BL8009 guards, the halo carousel, the reverse-mode gather --
/// rather than re-derived from the tag at each. The tag stays the CARRIER:
/// it already survives every index-record rewrite.
///
/// Reach = Offsets union {0}: the centre is always readable. `Start` is the
/// centre's first valid ordinal in the inner space (the shrunk loop begins at
/// 0), `Shrink` the interior lost to the reach on both sides; the output
/// extent M of a slot over an inner extent N is N - Shrink.
type HaloAccess = {
    /// The wrapped index alias ("" when anonymous or surface-built).
    Inner: string
    /// The DECLARED offset set, as written.
    Offsets: int list
    /// max 0 (-(min 0 (min Offsets)))
    Start: int64
    /// (-(min 0 (min Offsets))) + max 0 (max Offsets)
    Shrink: int64
    /// A "c:" tag: ordinals walk the PRESENT cells of a compound inner.
    IsCompound: bool
}

let haloAccessOf (inner: string) (offsets: int list) (isCompound: bool) : HaloAccess =
    let lo = min 0 (List.min offsets)
    let hi = max 0 (List.max offsets)
    { Inner = inner; Offsets = offsets
      Start = int64 (-lo); Shrink = int64 (hi - lo); IsCompound = isCompound }

/// Parse a slot tag into its access record. Total: None for a non-halo tag.
let haloAccessOfTag (tag: string) : HaloAccess option =
    match tag with
    | HaloWinTag (isC, inner, offs) -> Some (haloAccessOf inner offs isC)
    | _ -> None

/// The reach, sorted: the declared offsets and 0.
let haloReach (h: HaloAccess) : int list = (0 :: h.Offsets) |> List.distinct |> List.sort

/// Is a literal offset inside the reach the interior was shrunk for?
let haloOffsetInReach (h: HaloAccess) (o: int) : bool =
    let r = haloReach h
    o >= List.min r && o <= List.max r

/// FORWARD DEMAND: the input window [lo', hi') an output tile [lo, hi) reads.
/// Over the whole output [0, M) this is [0, M + Shrink) = [0, N): the extent
/// guards' "declared inner extent" is `snd (haloDemand h (0, M))`.
let haloDemand (h: HaloAccess) (lo: int64, hi: int64) : int64 * int64 =
    let r = haloReach h
    (lo + h.Start + int64 (List.min r), hi - 1L + h.Start + int64 (List.max r) + 1L)

/// TRANSPOSE: the (offset, output ordinal) pairs that read input ordinal `j`
/// -- the boundary is ZeroOutside (an output ordinal outside [0, M) does not
/// exist, so it contributes nothing). Every j in [0, N) has at least one
/// contributor because 0 is in the reach.
let haloContributors (h: HaloAccess) (outExtent: int64) (j: int64) : (int * int64) list =
    haloReach h
    |> List.map (fun o -> (o, j - h.Start - int64 o))
    |> List.filter (fun (_, i) -> 0L <= i && i < outExtent)

/// The center's first valid ordinal for a halo slot (a projection of the
/// record; kept for its call sites).
let haloStartOffsetOfTag (tag: string) : int64 option =
    haloAccessOfTag tag |> Option.map (fun h -> h.Start)

/// Interior loss of a halo slot (a projection of the record; kept for its
/// call sites). Dense slots fold this into the extent at typecheck; compound
/// slots (whose extent is the runtime mask cardinality) subtract it at the
/// loop bound.
let haloShrinkOfTag (tag: string) : int64 option =
    haloAccessOfTag tag |> Option.map (fun h -> h.Shrink)

// Provider PACKED-POOL provenance tags (icechunk plan section 5.3).
//
// A dense provider axis carries its provenance in a "__icaxis|" Tag, which is
// what makes cross-repo/diverged-axis arithmetic refuse: co-iteration decides
// axis agreement from the Tag alone. A PACKED variable's pool axis is not a
// dimension -- its extent is a derived cardinality, so it never joins the
// module's shared dim universe and never reached that tag -- and an untagged
// axis is "some axis of this cardinality", which two different repos' pools
// silently share. These prefixes give a pool axis the same provenance the
// dense axes have.
//
// TWO prefixes because the Tag doubles as the KIND sentinel and a pool comes
// in two kinds: a depth-1 simplex pool (SymIdx/AntisymIdx storage) is IxKPlain,
// an iterated-wreath pool (OrbIdx) is IxKOrbit and would otherwise carry the
// "__orbidx" sentinel. `ixKindOfTag` below maps each spelling back, which is
// what IRValidate's Tag/IxKind agreement check requires. Both start with
// `providerPoolTagStem`, so one predicate recognises the family, and both start
// with `__` so the seams that read a Tag as a user-written NAME
// (`checkArrayIndexTags`, `elemTypeForIterationIndex`, `Ide.indexNamesOf`)
// leave them alone -- exactly as for "__icaxis|". `unify` is the one seam that
// USED to be on that list and is not: see `isProviderAxisTag` below.
let providerPoolTagStem = "__icpool"
/// Depth-1 simplex pool (SymIdx/AntisymIdx/Hermitian storage): IxKPlain.
let providerPoolTagPrefix = "__icpool|"
/// Iterated-wreath pool (OrbIdx, depth >= 2): IxKOrbit.
let providerOrbPoolTagPrefix = "__icpoolorb|"

/// Does this Tag carry packed-pool provenance (either kind)?
let isProviderPoolTag (tag: string) : bool = tag.StartsWith providerPoolTagStem

/// A DENSE provider dimension's provenance prefix. The tag's SHAPE and the
/// reasoning behind it live at `IcechunkProvider.axisTag`, which re-exports
/// this binding as `axisTagPrefix`; the literal sits here because the two
/// predicates that read it (`Unify.indexPairIncompatible`,
/// `TypeLower.indexNamesCoIterable`) compile before the provider does.
let providerAxisTagPrefix = "__icaxis|"

/// Does this Tag carry PROVIDER provenance of any flavour -- a dense axis, a
/// simplex pool, or a wreath pool?
///
/// The family exists because these three tags are `__`-prefixed for one
/// narrow reason (the four seams above must not read them as user-written
/// names) and are otherwise NOT synthetic at all: a synthetic tag is a KIND
/// sentinel, structural and shared by every record of that kind, while a
/// provider tag is an IDENTITY -- which repo, which version of the dim -- and
/// two different ones are two different axes. Every seam that decides axis
/// AGREEMENT therefore has to treat them nominatively, which is what this
/// predicate marks. `__orbidx`, `__sparseidx`, `__anon` and friends stay
/// exempt; they are sentinels, and comparing them would refuse sound code.
let isProviderAxisTag (tag: string) : bool =
    tag.StartsWith providerAxisTagPrefix || isProviderPoolTag tag

// ---------------------------------------------------------------------------
// Provider seams that the DIAGNOSTIC layer needs (icechunk review pass)
// ---------------------------------------------------------------------------

/// Raised by a provider whose STORE RESOLUTION failed: the repo does not
/// exist, the ref does not resolve (typo'd, ambiguous, a deleted-tag
/// tombstone), the header is not spec 2, the status is Offline, the snapshot
/// carries a virtual chunk ref or a nested group -- every refusal that makes
/// the store's dims and variables untypeable.
///
/// WHY A TYPE AND NOT `failwith`. `TypeCheck.checkDecl`'s catch-all used to
/// swallow every icechunk store-resolution refusal and fall back to opaque
/// types, so `blade check` and the editor reported NOTHING and the error only
/// surfaced under `emit`/`run` once lowering re-opened the store. A named
/// type lets that arm re-raise as a spanned diagnostic while leaving every
/// other provider's silent fallback untouched: zarr/netcdf/csv do not raise
/// this.
///
/// WHY A CLASS AND NOT `exception ProviderResolutionError of string`. The
/// message has to survive as `.Message` -- `Lowering.lower` and
/// `lowerCheckedProgram` surface an escaping provider failure through exactly
/// that property, and an F# exception DECLARATION leaves `Message` at the
/// default "Exception of type ... was thrown", blanking every lowering-phase
/// refusal text in one stroke.
type ProviderResolutionError(message: string) =
    inherit System.Exception(message)

/// Extension point: the provider layer registers the decoder that turns one of
/// its axis Tags into the DISPLAY NAME a user would recognise
/// (`__icaxis|lat@wx:9f3a1c...` -> `lat`, and a split identity -> `lat#2`);
/// see `ProviderStatics.install`.
///
/// Behind a hook because the refusal sites that print tags -- `TypeLower`'s two
/// co-iteration messages and `TypeEnv.formatTypeError` -- must not depend on a
/// provider module, and because leaving it UNREGISTERED has to reproduce
/// today's behaviour exactly (the raw tag, verbatim). That is what keeps a
/// standalone `#load` of the front-end files honest, and what makes the
/// decoder's absence a display regression rather than a crash.
let mutable private providerAxisTagDecoder : (string -> string option) option = None

let registerProviderAxisTagDecoder (f: string -> string option) : unit =
    providerAxisTagDecoder <- Some f

/// The decoded display name of a Tag: `None` when no decoder is installed, or
/// when the tag is not one the installed decoder recognises. Never throws --
/// a diagnostic must not die while rendering itself.
let tryDecodeAxisTag (tag: string) : string option =
    match providerAxisTagDecoder with
    | Some f -> (try f tag with _ -> None)
    | None -> None

/// A Tag as it should APPEAR in a diagnostic: the decoded provider name when
/// there is one, the raw tag otherwise. The raw case covers every user-written
/// name (which is already its own display form), every kind sentinel, and every
/// tag at all before `ProviderStatics.install` has run.
let displayTagName (tag: string) : string =
    match tryDecodeAxisTag tag with
    | Some n -> n
    | None -> tag

/// Extension point: WHY a provider axis identity differs from the one it split
/// from ("coordinate content differs", "extent 5 -> 10", ...), keyed on the
/// tag that identity carries. The provider records this at mint time and,
/// until now, nothing ever printed it -- so a diverged-checkout refusal told
/// the user THAT two axes disagree and never WHICH FACT about the store made
/// them disagree, which is the one thing they cannot recover by reading the
/// program.
let mutable private providerAxisSplitReason : (string -> string option) option = None

let registerProviderAxisSplitReason (f: string -> string option) : unit =
    providerAxisSplitReason <- Some f

let tryAxisSplitReason (tag: string) : string option =
    match providerAxisSplitReason with
    | Some f -> (try f tag with _ -> None)
    | None -> None

/// The ONE clause a refusal between two DIFFERENT provider identities of the
/// SAME axis earns, or `None` when the pair is not that.
///
/// The case exists because the display name deliberately drops everything that
/// discriminates two identities except the split ordinal: two checkouts of one
/// repo print as 'lat' and 'lat#2' (informative but unexplained), and two
/// REPOS' `lat` print identically (uninformative). Both read as "these are the
/// same axis, why the refusal?" -- so the clause says which of the two it is,
/// and, when the mint table remembers, the store fact behind it.
///
/// One line, appended: every existing pin on the sentence in front of it keeps
/// matching.
let providerSplitClause (tagA: string) (tagB: string) : string option =
    if tagA = tagB || not (isProviderAxisTag tagA) || not (isProviderAxisTag tagB) then None
    else
        match tryDecodeAxisTag tagA, tryDecodeAxisTag tagB with
        | Some a, Some b when a = b ->
            // Same DISPLAY name and different tags: nothing on the line above
            // distinguishes them, so this clause is the whole diagnosis.
            match tryAxisSplitReason tagB |> Option.orElse (tryAxisSplitReason tagA) with
            | Some reason -> Some $"(these are two identities of axis '{a}': {reason})"
            | None ->
                Some $"(two different axes both print as '{a}' here: they carry different store provenance -- a diverged checkout, or a different repo)"
        | Some a, Some b ->
            // Different display names. When they share a base dim name the
            // ordinal already says "two identities"; the reason says why.
            let baseOf (n: string) = match n.IndexOf '#' with | i when i > 0 -> n.Substring(0, i) | _ -> n
            if baseOf a = baseOf b then
                match tryAxisSplitReason tagB |> Option.orElse (tryAxisSplitReason tagA) with
                | Some reason -> Some $"(these are two identities of axis '{(baseOf a)}': {reason})"
                | None -> None
            else None
        | _ -> None

/// Derive the kind from a (possibly user-supplied) Tag value: sentinel strings
/// map to their kind, anything else (user names, "__anon", None) is IxKPlain.
/// For dynamic-tag sites; a literal sentinel should state IxKind directly.
let ixKindOfTag (tag: string option) : IxKind =
    match tag with
    | Some "__compoundidx" -> IxKCompound
    | Some "__compoundidx_dynamic" -> IxKCompoundDynamic
    | Some "__sparseidx" -> IxKSparse
    | Some "__orbidx" -> IxKOrbit
    | Some "__depidx" -> IxKDep
    | Some "__depidx_inner" -> IxKDepInner
    | Some "__depidx_outer" -> IxKDepOuter
    | Some "__raggedidx" -> IxKRagged
    | Some "__raggedidx_inline" -> IxKRaggedInline
    | Some "__raggedidx_opaque" -> IxKRaggedOpaque
    | Some "__group_outer" -> IxKGroupOuter
    | Some "__group_member" -> IxKGroupMember
    | Some "__seq" -> IxKSeq
    | Some "__error_ragged_no_prior" -> IxKErrorRaggedNoPrior
    | Some "__error_irreps_bad_spec" -> IxKErrorIrrepsBadSpec
    | Some "__error_pgirreps_bad_spec" -> IxKErrorPgIrrepsBadSpec
    | Some t when t.StartsWith irrepsTagPrefix -> IxKIrreps
    | Some t when t.StartsWith pgIrrepsTagPrefix -> IxKPgIrreps
    // A provider pool tag REPLACES the record's kind sentinel, so it has to
    // decode back to the kind the record actually carries. The wreath spelling
    // stands in for "__orbidx"; the depth-1 spelling falls through to IxKPlain
    // below, which is what a simplex pool record carries (its symmetry, not its
    // IxKind, is what makes it compact).
    | Some t when t.StartsWith providerOrbPoolTagPrefix -> IxKOrbit
    // A compound-inner halo slot keeps IxKCompound: the compound machinery
    // (cidx materialization, cardinality bound) must still engage. Dense
    // halo slots fall through to IxKPlain like any other "__" placeholder.
    | Some t when t.StartsWith (haloWinTagPrefix + "c:") -> IxKCompound
    | _ -> IxKPlain

/// Ragged FAMILY: dimensions whose extent varies per outer row.
let isRaggedFamilyKind (k: IxKind) : bool =
    match k with
    | IxKRagged | IxKRaggedInline | IxKRaggedOpaque -> true
    | _ -> false

/// Ragged-ROW family: inner dims whose rank-1 rows carry their length inline
/// (`.len` on a RaggedRow<T>) rather than via `.extents` -- peeled ragged
/// rows, DepIdx-allocated inners, and group_by members all share this shape.
let isRaggedRowKind (k: IxKind) : bool =
    match k with
    | IxKRagged | IxKRaggedInline | IxKRaggedOpaque
    | IxKDepInner | IxKGroupMember -> true
    | _ -> false

/// The MAGNITUDE of a unit relative to the base-unit product named by its
/// `Dims` -- what makes `day` differ from `second` when both are {second: 1}.
/// Kept EXACT and SYMBOLIC: a rational Num/Den times a product of named
/// irrational constants with integer exponents. `86400 * second` is
/// {86400/1, {}}; `2 * pi * radian` is {2/1, {pi: 1}}.
///
/// Nothing rounds during unit ALGEBRA -- mul/div/pow compose the rationals
/// exactly and ADD the constant exponents -- so `turn / turn` cancels to
/// exactly 1 rather than to 6.28.../6.28.... A float appears only when a
/// conversion factor is materialized at a seam (scaleToCppFactor), and `pi`
/// materializes as the BACKEND's constant rather than a decimal we wrote
/// down, so the C++ value is correctly rounded.
type UnitScale = {
    Num: bigint
    /// Strictly positive; the sign of the rational lives on Num.
    Den: bigint
    /// Irrational factors by integer exponent, e.g. pi^1. Zero exponents are
    /// normalized away, so `Map.isEmpty` means "purely rational".
    Consts: Map<string, int>
}

/// Reduce to lowest terms, force Den > 0, and drop zero constant exponents.
/// Every UnitScale in circulation is normalized, so structural equality is
/// exact scale equality -- which is what the seam checks compare.
let private normScale (num: bigint) (den: bigint) (consts: Map<string, int>) : UnitScale =
    if den.IsZero then
        // Only reachable from `x / 0` in a unit RHS; the parser rejects a
        // zero literal before this, so this is a belt-and-braces invariant.
        failwith "unit scale: zero denominator"
    let signFix = if den.Sign < 0 then bigint -1 else bigint 1
    let num = num * signFix
    let den = abs den
    let g =
        let g0 = System.Numerics.BigInteger.GreatestCommonDivisor (abs num, den)
        if g0.IsZero then bigint 1 else g0
    { Num = num / g
      Den = den / g
      Consts = consts |> Map.filter (fun _ e -> e <> 0) }

/// The unity scale: every unit declared without an explicit factor has it.
let scaleOne : UnitScale = { Num = bigint 1; Den = bigint 1; Consts = Map.empty }

let scaleOfRational (num: bigint) (den: bigint) : UnitScale = normScale num den Map.empty

/// A named irrational factor (`pi`), carried symbolically to emission.
let scaleOfConst (name: string) : UnitScale = normScale (bigint 1) (bigint 1) (Map.ofList [(name, 1)])

let scaleIsOne (s: UnitScale) : bool =
    s.Num.IsOne && s.Den.IsOne && Map.isEmpty s.Consts

let private mergeConsts f (a: Map<string, int>) (b: Map<string, int>) =
    Map.fold (fun acc k v ->
        let existing = Map.tryFind k acc |> Option.defaultValue 0
        Map.add k (f existing v) acc) a b

let scaleMul (a: UnitScale) (b: UnitScale) : UnitScale =
    normScale (a.Num * b.Num) (a.Den * b.Den) (mergeConsts (+) a.Consts b.Consts)

let scaleDiv (a: UnitScale) (b: UnitScale) : UnitScale =
    // b.Num moves to the denominator, so a zero numerator on the divisor is
    // the one way normScale's zero-denominator guard can fire.
    normScale (a.Num * b.Den) (a.Den * b.Num) (mergeConsts (-) a.Consts b.Consts)

let scalePow (s: UnitScale) (n: int) : UnitScale =
    if n = 0 then scaleOne
    elif n > 0 then
        normScale (System.Numerics.BigInteger.Pow (s.Num, n))
                  (System.Numerics.BigInteger.Pow (s.Den, n))
                  (s.Consts |> Map.map (fun _ e -> e * n))
    else
        let k = -n
        normScale (System.Numerics.BigInteger.Pow (s.Den, k))
                  (System.Numerics.BigInteger.Pow (s.Num, k))
                  (s.Consts |> Map.map (fun _ e -> e * n))

/// Floor of the integer square root, but ONLY when it is exact -- None when
/// `n` is not a perfect square (and for a negative `n`, which no declared
/// magnitude produces but the type allows).
///
/// bigint has no Sqrt, and going through `float` would round: a magnitude is
/// arbitrary-precision (`(10^9)^2` is a legal unit RHS) and a double cannot
/// even represent every 64-bit square, so the answer has to be computed in
/// integers. Newton's iteration on x <- (x + n/x)/2 DECREASES monotonically
/// to floor(sqrt n) from any start above the true root, so the first
/// non-decrease is the fixed point; seeding at 2^ceil(bits/2) (safely above
/// the root, since n < 2^bits) keeps the step count logarithmic. The final
/// `x * x = n` is what makes the result exact rather than floored.
let private bigintSqrtExact (n: bigint) : bigint option =
    if n.Sign < 0 then None
    elif n.IsZero || n.IsOne then Some n
    else
        let mutable x = System.Numerics.BigInteger.Pow (bigint 2, int ((n.GetBitLength () + 1L) / 2L))
        let mutable next = (x + n / x) / bigint 2
        while next < x do
            x <- next
            next <- (x + n / x) / bigint 2
        if x * x = n then Some x else None

/// Exact square root of a MAGNITUDE, or None when there is not one. Both
/// Num and Den must be perfect squares and every named-constant exponent
/// must be even (`pi^2` halves to `pi`; a lone `pi` has no representable
/// root, since the grammar carries integer exponents only).
///
/// Refusing is the point: this module's invariant is that nothing rounds
/// during unit algebra, so `sqrt` of `4047 * meters^2` has no answer here --
/// approximating it would put a wrong conversion factor in a TYPE, where no
/// later check can catch it.
let scaleSqrt (s: UnitScale) : UnitScale option =
    if s.Consts |> Map.exists (fun _ e -> e % 2 <> 0) then None
    else
        match bigintSqrtExact s.Num, bigintSqrtExact s.Den with
        | Some num, Some den -> Some (normScale num den (s.Consts |> Map.map (fun _ e -> e / 2)))
        | _ -> None

/// Render a scale for diagnostics: `86400`, `1/60`, `2 * pi`, `pi^2 / 4`.
/// The rational numerator is elided when it is 1 and a constant carries the
/// numerator instead, so `2 * pi` does not print as `2 * pi` with a stray 1.
let ppUnitScale (s: UnitScale) : string =
    let ppConst (n, e) = if abs e = 1 then n else $"{n}^{abs e}"
    let numConsts = s.Consts |> Map.toList |> List.filter (fun (_, e) -> e > 0) |> List.map ppConst
    let denConsts = s.Consts |> Map.toList |> List.filter (fun (_, e) -> e < 0) |> List.map ppConst
    let numParts =
        (if s.Num.IsOne && not (List.isEmpty numConsts) then [] else [string s.Num]) @ numConsts
    let denParts = (if s.Den.IsOne then [] else [string s.Den]) @ denConsts
    let numStr = if List.isEmpty numParts then "1" else String.concat " * " numParts
    if List.isEmpty denParts then numStr
    else $"""{numStr} / {(String.concat " * " denParts)}"""

/// Irrational constants usable as unit scale factors, by double value.
/// `System.Math.PI` is the correctly-rounded double for pi, bit-identical to
/// C++'s `M_PI` on every platform Blade targets -- and codegen renders a
/// double through `floatToCppLiteral`, which is shortest-ROUND-TRIP, so the
/// constant reaches the compiler as the exact same bits rather than as a
/// decimal either side truncated. That is what lets a conversion factor be
/// an ordinary float literal without a symbolic-constant IR node.
///
/// `pi` resolves only AFTER `env.Units`, so a user's own `Unit pi` still wins
/// and no existing program changes meaning.
let unitScaleConstants : Map<string, float> =
    Map.ofList [ ("pi", System.Math.PI) ]

/// The double a conversion by this scale multiplies by: the numerator
/// product, then each denominator factor divided out in turn. Integers go in
/// as exact doubles and the division happens once, in IEEE, so `1/60` is the
/// correctly-rounded quotient rather than a decimal truncated on the way out.
///
/// An unknown constant yields nan; resolveUnitExpr rejects those names before
/// a signature carrying one can be built, so it is unreachable in practice.
let scaleToFloat (s: UnitScale) : float =
    let constTerms sign =
        s.Consts
        |> Map.toList
        |> List.filter (fun (_, e) -> sign * e > 0)
        |> List.collect (fun (n, e) ->
            List.replicate (abs e) (Map.tryFind n unitScaleConstants |> Option.defaultValue nan))
    let num = (float s.Num) :: constTerms 1 |> List.fold (*) 1.0
    (float s.Den) :: constTerms -1 |> List.fold (/) num

/// Unit of measure signature. `Dims` is the STRUCTURAL layer: a product of
/// base units with integer exponents (e.g. velocity = {meters: 1, seconds:
/// -1}); dimensionless = empty map. `Nominal` is the QUANTITY layer: a
/// nominal identity declared via `Unit speed: mps`, entailing exactly the
/// dims it was declared with. Structural units and plain aliases carry
/// Nominal = None; the nominal layer is exactly one level deep (quantity
/// names are TERMINAL in unit algebra). `Scale` is the MAGNITUDE layer: how
/// many of the Dims product one of this unit is (`day` = 86400 `second`).
///
/// Dims and Scale are deliberately independent: two signatures with equal
/// Dims and different Scale are the SAME physical quantity in different
/// magnitudes, so they are CONVERTIBLE (unitCompatible) but not
/// interchangeable (unitSameScale). Values are never canonicalized into base
/// units -- a `Float64<day>` holds a day-magnitude number -- so a conversion
/// factor is materialized only where two magnitudes actually meet.
type UnitSig = { Nominal: string option; Dims: Map<string, int>; Scale: UnitScale }

/// Unit arithmetic: dimensionless (no nominal, empty dims, unity scale)
let unitDimensionless : UnitSig = { Nominal = None; Dims = Map.empty; Scale = scaleOne }

/// A structural (non-nominal) signature over the given dims, unity scale.
let unitOfDims (dims: Map<string, int>) : UnitSig = { Nominal = None; Dims = dims; Scale = scaleOne }

/// A structural signature over the given dims at an explicit magnitude.
let unitOfDimsScaled (dims: Map<string, int>) (scale: UnitScale) : UnitSig =
    { Nominal = None; Dims = dims; Scale = scale }

/// Normalize: remove zero-exponent entries (nominal and scale untouched)
let unitNormalize (u: UnitSig) : UnitSig =
    { u with Dims = u.Dims |> Map.filter (fun _ exp -> exp <> 0) }

/// Unit multiplication: add exponents, MULTIPLY scales. Multiplicative
/// composition DROPS the nominal layer: a quantity is an identity, not a
/// factor, so `speed * s` yields the structural product of the dims.
///
/// The scale riding along is what makes `*` and `/` conversion-FREE:
/// `Float64<day> * Float64<meter/second>` needs no runtime work, because the
/// 86400 lands in the result TYPE rather than in the emitted expression.
let unitMul (a: UnitSig) (b: UnitSig) : UnitSig =
    let merged =
        Map.fold (fun acc k v ->
            let existing = Map.tryFind k acc |> Option.defaultValue 0
            Map.add k (existing + v) acc) a.Dims b.Dims
    unitNormalize { Nominal = None; Dims = merged; Scale = scaleMul a.Scale b.Scale }

/// Unit division: subtract exponents, DIVIDE scales (drops nominal, like unitMul)
let unitDiv (a: UnitSig) (b: UnitSig) : UnitSig =
    let merged =
        Map.fold (fun acc k v ->
            let existing = Map.tryFind k acc |> Option.defaultValue 0
            Map.add k (existing - v) acc) a.Dims b.Dims
    unitNormalize { Nominal = None; Dims = merged; Scale = scaleDiv a.Scale b.Scale }

/// Unit power: scale all exponents and raise the magnitude (drops nominal).
/// `(1000 * meter)^2` is 1e6 meter^2 -- the scale must be raised to the same
/// power as the dims or `km^2` would silently read as `1000 m^2`.
let unitPow (u: UnitSig) (n: int) : UnitSig =
    if n = 0 then unitDimensionless
    else
        unitNormalize { Nominal = None
                        Dims = u.Dims |> Map.map (fun _ exp -> exp * n)
                        Scale = scalePow u.Scale n }

/// Unit square root: halve the dimension exponents AND the magnitude, or
/// refuse. The inverse of unitPow at n = 2, so it has to halve the same two
/// layers unitPow raises -- halving dims alone turned `km^2` into
/// meters-magnitude `meters`, and the resulting conversion factor was wrong
/// by 1000 wherever the value later met another length.
///
/// Total-or-nothing, and None is a REFUSAL rather than "no constraint": an
/// odd exponent needs a half-integer dimension the grammar cannot carry, and
/// a non-square magnitude (`4047 * meters^2`) needs a rounded scale this
/// module never produces. Drops the nominal layer like unitMul/unitDiv/
/// unitPow -- the square root of a quantity is not that quantity.
let unitSqrt (u: UnitSig) : UnitSig option =
    let n = unitNormalize u
    if n.Dims |> Map.forall (fun _ exp -> exp % 2 = 0) then
        scaleSqrt n.Scale
        |> Option.map (fun sc -> unitOfDimsScaled (n.Dims |> Map.map (fun _ exp -> exp / 2)) sc)
    else None

/// Check if two unit signatures are CONVERTIBLE: dims must be equal, and the
/// nominal layers must AGREE -- both the same quantity, or at least one side
/// structural (None). Two DIFFERENT quantities are incompatible even over
/// identical dims (that is what the nominal layer is for).
///
/// Deliberately scale-BLIND: `day` and `second` are compatible, since a
/// factor relates them. Whether a seam may bridge that factor silently is a
/// separate question each seam answers with unitSameScale.
let unitCompatible (a: UnitSig) (b: UnitSig) : bool =
    (unitNormalize a).Dims = (unitNormalize b).Dims
    && (match a.Nominal, b.Nominal with
        | Some na, Some nb -> na = nb
        | _ -> true)

/// Do two signatures share a MAGNITUDE, so that values of one can stand in
/// for the other with no conversion factor? Every UnitScale is normalized,
/// so this is exact rational-and-symbolic equality, never a float compare.
let unitSameScale (a: UnitSig) (b: UnitSig) : bool = a.Scale = b.Scale

/// The factor that converts a value FROM signature `src` INTO signature
/// `dst` (multiply by it). Meaningful only for unitCompatible signatures.
/// Computed as one exact ratio -- `day -> second` is 86400/1, not a
/// round-trip through a canonical base -- so a value crosses at most one
/// multiply and never visits an intermediate magnitude that could overflow.
let unitConversionFactor (src: UnitSig) (dst: UnitSig) : UnitScale =
    scaleDiv src.Scale dst.Scale

/// Merge two CONVERTIBLE signatures (post-unitCompatible), keeping whichever
/// nominal is present: additive ops over `speed + m/s` stay `speed`. The
/// LEFT operand's magnitude wins, matching the nominal preference: `day +
/// second` is a number of days.
let unitJoin (a: UnitSig) (b: UnitSig) : UnitSig =
    match a.Nominal with
    | Some _ -> a
    | None -> { a with Nominal = b.Nominal }

/// Pretty-print a unit signature (error-message form). A quantity prints as
/// its nominal name; a structural signature prints its dims product; the
/// empty structural signature prints "dimensionless".
let ppUnitSig (u: UnitSig) : string =
    match u.Nominal with
    | Some n -> n
    | None ->
        let dims = u.Dims
        let dimsStr =
            let pos = dims |> Map.filter (fun _ e -> e > 0) |> Map.toList
            let neg = dims |> Map.filter (fun _ e -> e < 0) |> Map.toList
            let ppTerm (name, exp) =
                if exp = 1 then name
                elif exp = -1 then name
                else $"{name}^{exp}"
            let posStr = pos |> List.map ppTerm |> String.concat " * "
            let negStr = neg |> List.map (fun (n, e) -> ppTerm (n, -e)) |> String.concat " * "
            match pos, neg with
            | [], [] -> None
            | _, [] -> Some posStr
            | [], _ -> Some $"1 / ({negStr})"
            | _, _ -> Some ($"""{posStr} / {(if neg.Length > 1 then sprintf "(%s)" negStr else negStr)}""")
        // A magnitude is part of the identity, so it prints: without it a
        // `day` vs `second` mismatch would render "second vs second".
        match dimsStr, scaleIsOne u.Scale with
        | None, true -> "dimensionless"
        | None, false -> ppUnitScale u.Scale
        | Some d, true -> d
        | Some d, false -> $"{ppUnitScale u.Scale} * {d}"

/// Pretty-print a unit signature in TYPE-ARGUMENT position (`Float64<...>`).
/// A quantity renders as its nominal name; a structural signature as its dims;
/// an empty structural signature -- dims cancelled via division, e.g.
/// `speed/speed` or `m/m` -- renders as `Unitless`, distinct from a bare type
/// that never had units. Display provenance only: Unitless does not affect
/// unification (a dims-empty sig unifies freely either way).
let ppUnitSigType (u: UnitSig) : string =
    match u.Nominal with
    | Some n -> n
    | None ->
        // A SCALED dimensionless (`2 * pi`, from `Unit turn = 2 * pi`) is not
        // Unitless: the magnitude survives cancellation of the dims and still
        // drives conversions, so it has to render.
        if Map.isEmpty ((unitNormalize u).Dims) && scaleIsOne u.Scale then "Unitless"
        else ppUnitSig u

/// Value carried by an EnumIdx alias declaration: all-int or all-string
/// (mixed rejected at typecheck). All-int lowers to int64_t, all-string to
/// std::string; both share the same SQL-like ops, differing only in the
/// Case-2 comparison. Predeclared: IRTGroupKeys below needs the type.
type EnumValue =
    | EVInt of int64
    | EVString of string

/// Element types for arrays
type ElemType =
    | ETInt32
    | ETInt64
    | ETFloat32
    | ETFloat64
    | ETComplex64
    | ETComplex128
    | ETBool
    | ETUnit
    | ETString
    // Named index references (value and element position) are represented
    // at the IRType level as `IRTIdxTagged (inner, IRefNamed name)` below.

/// Explicit numeric cast heads: the surface scalar type names accepted in
/// CALL position (`Float32(x)`, `Int64(floor(x))`), mapped to the target
/// element type. Deliberately the same aliases as type position
/// (TypeLower.lowerTypeExpr's scalar arms: "Int" is Int32, "Float"/"Double"
/// are Float64) so a name means one thing in type and cast position alike.
/// Narrower than TypeLower.builtinScalarNames on purpose: Nat/Bool/String/
/// Char/Void/Poly/Array are not value conversions. Keep the three tables'
/// name sets consistent when adding a scalar.
let numericCastTargets : Map<string, ElemType> =
    Map.ofList [
        "Int", ETInt32; "Int32", ETInt32; "Int64", ETInt64
        "Float", ETFloat64; "Float64", ETFloat64; "Double", ETFloat64
        "Float32", ETFloat32
        "Complex64", ETComplex64; "Complex128", ETComplex128 ]

/// Target element type of a numeric cast head, None for any other name.
let castTargetOf (name: string) : ElemType option =
    Map.tryFind name numericCastTargets

/// The canonical cast-head spelling for an element type -- used by messages
/// that suggest an explicit cast ("make it explicit with Float64(...)").
let castNameOf (et: ElemType) : string =
    match et with
    | ETInt32 -> "Int32"
    | ETInt64 -> "Int64"
    | ETFloat32 -> "Float32"
    | ETFloat64 -> "Float64"
    | ETComplex64 -> "Complex64"
    | ETComplex128 -> "Complex128"
    | ETBool -> "Bool"
    | ETUnit -> "Void"
    | ETString -> "String"


// The IRType family, generic over the extent representation 'Ext. In IR.fs,
// IRIndexType.Extent is an IRExpr -- the coupling that fused types to
// expressions (index types depend on runtime values: ragged lengths,
// compound masks, formalism 4.5). Generic here lets this file own the full
// type structure with no expression dependency; IR.fs instantiates
// `type IRType = IRTypeG<IRExpr>` (et al.) unchanged.

/// Kind of loop object (leaf; referenced by LoopTypeG).
type LoopKind =
    | LKMethod   // MethodLoop - arrays bound, awaiting kernel
    | LKObject   // ObjectLoop - kernel bound, awaiting arrays

type IRIndexTypeG<'Ext> = {
    Id: IRId
    Rank: int               // Number of index components (1 for Idx, 2 for SymIdx<2>, etc.)
    Extent: 'Ext           // Size expression (may depend on outer indices)
    Symmetry: SymmetryClass
    Tag: string option       // Name (index space matching); mirrors IxKind's
                             // sentinel -- validator enforces agreement (3.3)
    Kind: DimensionKind      // S-dimension or T-dimension
    IxKind: IxKind           // Reserved kind discriminator (never a user name)
    Dependencies: IRId list  // Dependencies on outer loop indices (for triangular iteration)
}

/// Array type in IR with identity tracking. ElemType is a full IRTypeG<'Ext>
/// (IRTScalar/IRTNamed/IRTInfer for primitives/structs-sums/inference vars);
/// active patterns (PrimElem, AnyPrimElem, NamedElem, etc.) project it into
/// role-specific shapes for consumers.
and IRArrayTypeG<'Ext> = {
    ElemType: IRTypeG<'Ext>
    IndexTypes: IRIndexTypeG<'Ext> list
    IsVirtual: bool          // Virtual array (range, reverse, etc.)
    Identity: ArrayIdentity option  // For tracking array identity
}

/// Reference to an index type from the value side; the nominal tag used by
/// IRTIdxTagged (parallel to UnitSig for IRTUnitAnnotated). Carries identity
/// only, not structure (arity, symmetry, bijection), which lives on
/// IRIndexTypeG<'Ext> array records. Two tags are compatible iff IdxRefs
/// are structurally equal (name for named, nominalId for anonymous).
and IdxRefG<'Ext> =
    /// User-defined named index type: Nat<LatIdx>. Identity is the name.
    | IRefNamed of string
    /// Anonymous Idx<n> occurrence: Nat<Idx<n>>. Identity is the nominalId
    /// -- fresh per source TyIdx node, matching the IRIndexTypeG<'Ext>.Id.
    /// Extent is kept for diagnostics only, NOT part of unification identity.
    | IRefAnon of nominalId: int * extent: 'Ext
    /// The tag WILDCARD `Nat<_>` / `Int64<_>` / `Float64<_>`: matches any
    /// nominal index tag, unit signature, or none. Carries NO identity (a
    /// declining PARAMETER, not a value's tag); only originates from a
    /// declared parameter type. Soundness:
    ///   - Legal in parameter position only -- return types, let/field/
    ///     index slots reject it as a silent hole (irTypeHasTagWildcard,
    ///     BL4003).
    ///   - Unification is permissive both ways but does NOT absorb the
    ///     matched tag (checkArrayIndexTags: warn, allow) -- that would be
    ///     the tag-VARIABLE feature, which this is not.
    ///   - Arithmetic-transparent (`1.0 * m` works on a wildcard param,
    ///     refused for concrete `Nat<Y>`); erases to inner type at codegen.
    | IRefAny

/// IR Types
and IRTypeG<'Ext> =
    | IRTScalar of ElemType
    | IRTTuple of IRTypeG<'Ext> list
    | IRTLoop of LoopTypeG<'Ext>
    | IRTComputation of IRTypeG<'Ext>   // Suspended computation producing this type
    | IRTUnit
    | IRTPoly of baseType: IRTypeG<'Ext> * arityVar: string  // Arity-polymorphic type
    | IRTNat of int option  // Type-level natural number (None = variable)
    // IRTIdxTagged: a base type wrapped with a nominal index-type tag,
    // shaped like IRTUnitAnnotated (base * UnitSig) but with an
    // IdxRefG<'Ext> tag and NO multiplicative algebra -- tags are nominal
    // labels, not exponent vectors (formalism 4.18.5: Nat<LatIdx> alongside
    // Float<meters> matches shape only). Typical inner is IRTScalar
    // ETInt64 ("Nat<I>"), any IRTypeG<'Ext> accepted. Identity: structural
    // equality on (inner, idxRef) -- unify iff inner types unify AND
    // IdxRefs match (name=name for IRefNamed, nominalId=nominalId for
    // IRefAnon). Renders as inner type at codegen (tag is typecheck-only);
    // IRefNamed uses the C++ typedef as a doc hook. Distinct from
    // `IRTNat of int option`: this is a runtime value's type, IRTNat a
    // type-parameter for ranks.
    | IRTIdxTagged of inner: IRTypeG<'Ext> * tag: IdxRefG<'Ext>
    // IRTDist: a distribution at stochastic order `order` -- typed form of
    // the PPL dist tower (ppl/NOTES.md). `order` is a STATIC plain int (not
    // IRTNat; unification ignores its value); `elem` is the cumulant type
    // (typically ETFloat64); `axes` types the random vector so component
    // projection can be typed: cumulant k over axes D is Array<elem like
    // SymIdx<k, D>>. Strict-unification typecheck-time invariant (only the
    // dist-construction intrinsic and dist-typed ops produce one), ERASED
    // before codegen to the tuple of packed cumulants (kappa_1..kappa_order).
    | IRTDist of order: int * elem: IRTypeG<'Ext> * axes: IRIndexTypeG<'Ext> list
    | IRTNamed of string    // Named type (struct, sum type, etc.)
    | IRTInfer of int       // Unresolved type variable (id for unification)
    | IRTUnitAnnotated of IRTypeG<'Ext> * UnitSig  // Type with unit-of-measure annotation
    | IRTGroupKeys of outerIdx: IRIndexTypeG<'Ext> * sourceIdx: IRIndexTypeG<'Ext> * enumValues: EnumValue list option
      // GroupKeys: CSR mapping sourceIdx -> groups by outerIdx; enumValues carries key values for reverse lookup when keys are EnumIdx.
    // IRTArrow: the sole arrow-shaped type, subsuming function types (all-SVal
    // slots) and stored/virtual arrays (distinguished only by slot kind);
    // slots consumed left-to-right, result emerges once all are consumed.
    // SIdx = storage-backed index slot (Tag/Symmetry/Extent on the
    // IRIndexTypeG<'Ext>); SIdxVirt = virtual index slot, computed on-the-fly,
    // no storage (`range<I>`, `reverse<I>`); SVal = value/closure slot.
    // `identity`: None for pure functions/virtual arrays, Some when the
    // outermost dimension is stored.
    // Shape constraints (`mkVirtualArrayArrow`/`validateArrowShape`): (1) once
    // a slot is SIdxVirt every slot after it must be too -- no stored
    // sub-arrays/closures after virtual; (2) if any slot is SIdxVirt the
    // result must not be IRTArrow -- virtual elements must be simple values.
    // `IRTArrow ([], ret, None)` is reserved for nullary functions;
    // `ArrayElem` rejects it so a nullary call isn't misread as rank-0
    // indexing (rank-0 arrays collapse to element type at `mkArrayLike`).
    // Examples:
    //   [SVal; SVal] -> ret           : pure binary function
    //   [SIdx; SIdx] -> elem          : stored 2D array
    //   [SIdxVirt; SIdxVirt] -> elem  : virtual 2D generator
    //   [SIdx; SIdxVirt] -> elem      : stored array of virtual sub-arrays
    //   [SVal; SIdx] -> elem          : function returning a stored array
    //   [SIdxVirt; SIdx] -> elem      : INVALID -- stored after virtual
    | IRTArrow of slots: IRArrowSlotG<'Ext> list * result: IRTypeG<'Ext> * identity: ArrayIdentity option

and IRArrowSlotG<'Ext> =
    | SIdx of IRIndexTypeG<'Ext>       // Storage-backed slot, consumed by an index value
    | SIdxVirt of IRIndexTypeG<'Ext>   // Virtual slot -- values computed on-the-fly, no storage
    | SVal of IRTypeG<'Ext>            // Value/closure slot, consumed by any value of that type

/// Kind of loop object with arity tracking
and LoopTypeG<'Ext> = {
    Kind: LoopKind
    Arity: int option        // None = arity-polymorphic
    ArrayTypes: IRTypeG<'Ext> list  // Types of bound arrays (for MethodLoop)
    KernelType: IRTypeG<'Ext> option  // Type of bound kernel (for ObjectLoop)
}

/// Render an IxKIrreps record's identity for diagnostics: the round-trippable
/// `IrrepsIdx<[(l, p, m), ...]>`, prefixed with the alias if present. None
/// for non-irreps records (a pre-match arm ahead of Symmetry dispatch).
let (|IrrepsIdxLike|_|) (ix: IRIndexTypeG<'Ext>) : string option =
    if ix.IxKind <> IxKIrreps then None
    else
        match ix.Tag with
        | Some (IrrepsTag (nameOpt, triples)) ->
            let payload =
                triples
                |> List.map (fun (l, p, m) -> $"({l}, {p}, {m})")
                |> String.concat ", "
            let core = $"IrrepsIdx<[{payload}]>"
            Some (match nameOpt with
                  | Some n -> $"{n} (= {core})"
                  | None -> core)
        | _ ->
            // Kind says irreps but the tag is missing/unparseable -- a state
            // validateIR rejects; render a placeholder rather than crash.
            Some "IrrepsIdx<?>"

/// The pg sibling of `IrrepsIdxLike`: renders `PgIrrepsIdx<C4, [("A", 1),
/// ("E", 2)]>`, prefixed with the alias if present. NOT merged with
/// IrrepsIdxLike -- they render differently (group is part of pg identity;
/// labels are names, not (l, parity) integers) and diagnostics show both.
let (|PgIrrepsIdxLike|_|) (ix: IRIndexTypeG<'Ext>) : string option =
    if ix.IxKind <> IxKPgIrreps then None
    else
        match ix.Tag with
        | Some (PgIrrepsTag (group, nameOpt, entries)) ->
            let payload =
                entries
                |> List.map (fun (label, m) -> $"(\"{label}\", {m})")
                |> String.concat ", "
            let core = $"PgIrrepsIdx<{group}, [{payload}]>"
            Some (match nameOpt with
                  | Some n -> $"{n} (= {core})"
                  | None -> core)
        | _ -> Some "PgIrrepsIdx<?>"
