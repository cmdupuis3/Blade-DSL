# Plan: trees and graphs (`TreeIdx<shape>`)

Status: **P0–P5 LANDED** (declaration, construction, static whole-path reads
and their composition, the derived dense axes + `leaves(T)` retype, and the
function-signature door with its shape-identity guard — both lanes; full suite
5220/0/0, independently re-verified). **Next:** P6 (docs/census rows, formalism
amendment), then the P7 graph arc and the nested-`leaf` sugar. The module's
corpus category and harness key are **`trees`**. The semantics are in
[../features/graphs-trees.md](../features/graphs-trees.md) (revised 2026-08-25);
this document is the compiler-side plan. Method: the seam checklist and cost
band below were derived by tracing how the three most recent index types
(`RaggedIdx`, `SparseIdx`/`CompoundIdx`, `OrbIdx`) actually landed, file by
file. Line numbers cite the working tree as of this writing and will drift.

## 1. What and why

The feature is **trees**; graphs need no new index type. The refreshed design
deleted `Trace<N>` and `DAGIdx` (visited sets are values; acyclicity is a
checkable `where acyclic(g)` license), so the graph story reduces to adjacency
arrays over existing index types, `let rec` bounded walks, and one new `where`
attribute. All compiler work of substance is `TreeIdx<shape>`:

- static shapes (`let static` nest of `leaf` / `degrees([...])`), lowered to a
  preorder degree sequence carried in a parameterized tag;
- flat preorder storage — pool + `off` + `deg` + `size`, a superset of the
  `Ragged<T>` CSR descriptor;
- path indexing through one index slot whose domain is paths, with
  contiguous subtree views;
- derived dense axes (`LeafIdx`/`NodeIdx`/`ChildIdx`, `preorder`/`postorder`
  virtual arrays) so bulk numerics stay on plain `Idx` and keep every existing
  optimization.

**Cost band, calibrated on precedent.** SparseIdx = 34 files touched, zero new
files. RaggedIdx = zero new files, ~14 carrying kind dispatch. OrbIdx = five
new files (~4,000 lines: `OrbRank.fs`, a C++ header, its standalone test
binary, two F# test files), ~24 touched, two harness keys, three `proofs/`
artifacts. The split is entirely "does it need a new bijection". **TreeIdx
needs one, so budget the OrbIdx band**, and expect the refusal surface to be a
larger share than the happy path (OrbIdx's ~26 `TypeCheckInfer` seams are
mostly refusals).

## 2. P0 — the representation decision (this phase can kill the feature)

Nothing in Blade today varies an array's *slot count*. The formalism's index
contract (§3.2) is stated for "every index type of arity r"; `IRIndexTypeG.Rank`
is a static `int` (src/Types.fs:741); an array is `IRTArrow` over a
**fixed-length** slot list (src/Types.fs:846); rank is read as
`arr.IndexTypes.Length` at 10+ `TypeCheckInfer.fs` sites and summed statically
in codegen (`CodeGenExpr.fs:541`); the C++ key is `std::array<size_t, NDIM>`
(src/cpp/index_types.h:38). A variable-length path fits none of that as-is.

**The escape hatch is a fully worked precedent: `SparseIdx` partial reads
deliver variable-length addressing inside ONE slot.** The mechanism, verified
end to end:

1. `_` parses to `ExprWildcard` (ParserGrammar.fs:447); typecheck rewrites each
   wildcard to a **unit literal** (TypeCheckSupport.fs:1795-1814) — unit is
   never a valid coordinate, so nothing downstream needs a wildcard concept.
2. `tabulatedResidualType` (TypeCheckSupport.fs:1251-1275) is parameterized
   purely by the **count** of free positions, never by which ones; position
   info rides in the tuple as sentinels, read back at emission.
3. `classifyCompoundIndexTuple` (IR.fs:2353-2365) classifies once and is shared
   by typecheck, C++ codegen, and the interpreter, so the three cannot disagree.

A tree path is exactly a variable-length tuple with a pinned prefix — the same
split (shape from a count, addressing from sentinel positions) applies.

**Decision to write down in P0:**

- **Chosen (recommended): single tuple slot.** `TreeIdx<s>` occupies one arrow
  slot with path-domain; whole-path reads `T((0,1))` are the primitive;
  partial paths are short-tuple prefix pinning with an
  `IRTreeProject(parent, prefixLen)` residual extent marker (the
  `IRCompoundProject` pattern), and the residual stays in the tree family
  (the SparseIdx residual rule).
- **Rejected (record why): variable-arrow.** Making slot count path-dependent
  is a rank-representation rewrite touching every `IndexTypes.Length` site,
  `expandedRows`, the codegen rank sums, and the C++ `NDIM` key. The plan must
  name this cost explicitly, per house style.
- **Open sub-question P0 must settle in writing:** whether bare-scalar
  application `T(0)` can ride the same tabulated dispatch as a prefix-of-one
  pin (rank-1 sparse already takes a bare scalar, and 1-tuples do not exist in
  the parser), giving `T(0)(1) ≡ T((0,1))` through residual application. If it
  cannot, the curried spelling defers to its own arc and the feature doc's §3.2
  gets a scope note. This is the one place the census and the design pull in
  different directions; neither constrains the storage or the bijection.

P0 also runs the design's gate probes, since each is a check that could reshape
scope (feature doc §7.1):

- **G1 (load-bearing for the whole graph story):** `Array<Nat<I> like J>` —
  index values as array *elements* — is demonstrated nowhere in the corpus
  (only a comment at src/IR.fs:209). Write the probe first; if it fails, the
  graph half needs its own repair phase before anything else. (Index-tag
  *arithmetic* stays forbidden by design; the need is storage + round-trip.)
- **G2:** struct/tuple elements under `let rec` — confirm parallel recursive
  arrays remain sufficient for walk state.
- **G4:** non-affine bounds refuse `collapse(k)` on the node axis (the ragged
  precedent); confirm the derived dense axes recover it.

Deliverable: P0 appendix in this file — the decision, the rejected
alternative's cost census, and the three probe results.

## 3. Decided surface (from the feature doc — not re-litigated here)

- Shape: `let static` nested literal over `leaf`, or `degrees([...])`;
  canonical form = preorder degree sequence; identity = its hash.
- Kind `IxKTree`, parameterized tag `__tree:<name>:<payload>` — the
  `IxKIrreps` discipline (Types.fs:182-216): `mkTreeTag` / `(|TreeTag|_|)`,
  `ixKindSentinel -> None`, prefix arm in `ixKindOfTag`. No new
  `IRIndexTypeG` field.
- Storage: preorder pool + `off`/`deg`/`size` tables; subtree views are
  pointer + length + sub-shape (the `RaggedRow<T>` peel shape).
- Derived dense axes are plain `Idx<n>`; `preorder<s>`/`postorder<s>` virtual
  arrays erase.
- Diagnostics: **join existing families first** (the Sparse/Orb shortcut — all
  eight sparse subscript-form errors map to BL4003, runtime reads to BL8003).
  Mint only: **BL4022** (tree read of indeterminate depth), **BL4021**
  (*invalid tree shape* — registered in P2 with that title, covering BOTH the
  non-static shape, which steers to the future `DynTreeIdx`, and the malformed
  degree sequence; one code, two mistakes told apart by the message text, on
  BL4018's `RaggedLensMismatch`/`RaggedLensNotStatic` precedent), and for the
  graph arc **BL8012** (`acyclic` construction check — BL8010/BL8011 are
  already occupied by interpreter panic codes in `Interp/Numerics.fs` despite
  not being in the registry; do not reuse them).
- **The indeterminate-depth code is DEFERRED at P5, and the reason is that it
  is currently UNMINTABLE.** (It was reserved as BL4019 until the master
  merge; BL4019 and BL4020 went to the halo-reach and constrained-domain
  refusals while this branch was open, so the next free code is **BL4022**.
  Nothing was minted here, so the renumber costs no pin.)
  Not "not yet needed" — *unregisterable without breaking the build*. With
  dynamic coordinates fully refused, every path is literal, so leaf-vs-internal
  is decided at compile time by `treeForwardChecked` and both outcomes already
  have their own messages (`must end at a leaf` for an internal node, `outside
  [0,d)` for out-of-arity; `trees/108`, `trees/107`, `trees/109`). A registered
  code needs a `protocol/data/diagnostics.json` entry whose `examples[]` names a
  corpus file that literally contains the string `BL4022`, and **no reachable
  site can produce one** — so minting it now buys a code with no reachable
  message and a guaranteed-red Surface block. It becomes mintable the moment
  dynamic coordinates land, which is the same phase that needs
  `BoundedIdx<0, arity>` and the BL8003 runtime twin; record it there, not here.

## 4. Seam checklist

The full trace of how Ragged/Sparse/Orb each landed lives in the session notes;
what follows is the checklist a TreeIdx implementer walks, in pipeline order.

**Mandatory core** (all in `src/Types.fs` + `src/IR.fs` + `src/IRStorage.fs`):
`IxKind` case; `ixKindSentinel`; `ixKindOfTag` (prefix arm — note the
**silent catch-all** `| _ -> IxKPlain`); `IRValidate.checkKindAgreement` is the
hard gate that Tag and IxKind agree; `placementOf` (also has a catch-all);
the **exhaustive family active pattern** at IR.fs:1243-1274 — the one seam
that will not compile if forgotten, so route as much dispatch as possible
through it; `IRStorage` cardinality fold + `allocRoutineFor` (FS0025-exhaustive
by design) + an `IIndexTypeBehavior` object; a smart constructor stamping Tag
and IxKind together.

**Front end:** `Lexer.fs` keyword (`KwTreeIdx`, plus `leaf` as a contextual
constant); `Ast.fs` `TyTreeIdx of shape: Expr` (payloads that are values ride
as `Expr`, folded by StaticEval — the `TySparseIdx of keys: Expr` pattern);
`ParserTypes.fs` production with **named reject paths** (OrbIdx has three
dedicated reject tests just for its bracket grammar). If a builder *expression*
is ever added (`tree(...)`), it costs forwarding arms in **15 files** (every
domain elaborator, all four `Grad*` passes, `Unfold.fs`, `Ide.fs`) — v1 avoids
this by having no builder.

**Middle:** `TypeLower.lowerTypeExpr` arm — and lowering **has no error
channel**: copy RaggedIdx's plant-a-placeholder idiom (`IxKError*` record +
`irTypeHas*` scanner at the annotation consumers), never SparseIdx's
`failwith`. `Unify.indexPairIncompatible` (compares Rank/Tag/Symmetry —
**never extents**) needs its **own arm, and this is not optional**: the
"tag equality decides" reading is false for a PARAMETERIZED tag. The
gate that would compare two unequal tags is guarded by
`isSyntheticTag t = t.StartsWith "__"`, and every parameterized tag —
`__irreps:`, `__pgirreps:`, `__tree:` — starts with `__`, so it is classified
synthetic and **never gates**. Two different tree shapes are both Rank 1 and
both `SymNone`, so without an arm they UNIFY. That is why irreps carries the
`BlockSpecTag` arm ahead of the exemption, and the `TreeTag` arm goes in the
same place, ahead of it (P2 landed it: identity is the degree sequence plus the
optional nominative alias). Ragged gets away with no arm only because its
records carry no distinguishing payload at all. Then: `TypeEnv` message arm +
error→code map (two spots in one file); `TypeCheckInfer`/`TypeCheckSupport`
dispatch; `Zonk` must descend the new arm; `IndexTypeValidator` (which
annotation positions are legal, and `isKnownStatic` = true for v1).

**Back ends — both, always:** the 8 CodeGen files; `CodeGenCuda` and
`EmitLlvm` may **refuse by name** in v1 (providers do); `src/cpp/` header (the
tables may mostly inline at codegen — index_types.h's own comment notes dense
types need no runtime object); the 6 `src/Interp/` files. Interp and codegen
are differential twins; neither gate runs by default (`--interp` /
`--diff-oracle`).

**Diagnostics — five touch points, not three:** Unify DU case → TypeEnv
(formatTypeError + code map) → `Diagnostics.fs` registry → generated
`protocol/surface.json` (`blade ide surface`, LF-only) → hand-authored
`protocol/data/diagnostics.json` (title byte-identical, example paths must
exist and contain the code string). Gate: `blade test surface` — reachable
**only from the full suite**.

**Tests/docs:** corpus `tests/corpus/index-types/` (254 files today);
`tests/Test_TreeRank.fs` + `tests/Test_TypeStructure.fs` pins; harness keys in
`CliSelfTests.fs` (`"treerank" | "tree-rank"`, the orbrank pattern);
`docs/features.md` row; `docs/formalism.md` §3.2/§3.3 rows; this file's status
header + the `docs/plans/README.md` row.

**fsproj:** `EnableDefaultItems=false`, manual dependency order, and compile
order is **not** phase order — `src/TreeRank.fs` goes beside `OrbRank.fs`
(~line 103: after the Grad group, before IR.fs, dependency-free by policy so
`proofs/` scripts can `#load` it standalone).

### Traps (each observed live, not hypothetical)

- **T1 — structural `TypeExpr` walks silently skip unknown arms** (the
  `TyBounded` hazard class, three measured repros). A new arm has the same
  hazard in reverse: every walk lacking it treats TreeIdx as absent. The
  sneaky case is alias-body laundering — `type B = TreeIdx<s>` carries the arm
  into positions the walk never probes. P2's gate includes this probe.
- **T2 — three independent strictness seams**: `unify`, direct application
  (`dispatchAppOrIndex` never unifies plain-call args), and `let`-ascription
  (overwrites, does not unify). Plus the eager-vs-zonked sub-trap: run the
  predicate twice, eagerly and over the zonked module. Probe all three before
  believing a rule is closed. Extents are never compared anywhere.
- **T3 — `exprTypeIfKnown` whitelist** (IR.fs:2932): an unlisted node kind in
  HM argument position → no specialization → **BL6001 spray** naming stdlib
  helpers the program never mentions. Any new view/indexing IR node makes an
  explicit, justified call either way (`IRSlice`/`IRCurry`/`IRSubset` are
  deliberately OFF).
- **T6 — the two silent catch-alls** (`ixKindOfTag`, `placementOf`) will
  quietly classify TreeIdx as a plain dense `Idx` if the arms are missed;
  neither fails the build. Prefer the FS0025-exhaustive dispatch points.
- **T7 — paired refusal texts** (front-half TypeEnv renderer vs back-half IR
  producer) are corpus-pinned separately and nothing enforces agreement
  (OrbIdx's known hazard). Inherited by any type that refuses from both halves.
- **T10 — never store resolved payload values in `Extent`** — it would change
  index-type *identity* (two distinct shape bindings with equal values would
  start unifying). Recompute on demand (`RaggedLensSource` pattern); the
  degree sequence rides the Tag, not the Extent.
- **T11 — a marker payload with no consumer silently diverges** from what
  construction actually bakes (the BL4018 bug, found late). If a tree marker
  names data, land the agreement check in the same change.
- **Harness: exits 0 with failures** — gate on the TOTAL line, absence of
  `Failed tests:`, and the `, N skipped` suffix.

## 5. Phasing

| phase | deliverable | size | gate |
|---|---|---|---|
| **P0** | **LANDED f3df310** — single tuple slot; G1 passes, G5 found (Nat under `let rec` refuses). Representation decision + probes (§2); the census of `Rank`-assumption sites under the rejected option; this doc's §§1-4 finalized | doc only | written decision naming the rejected alternative and its cost; G1/G2/G4 probe results recorded |
| **P1** | **LANDED bf6705a** — 73 brute-force pins; one recorded deviation (`*Checked` = domain-checked, not OrbRank's overflow contract). `src/TreeRank.fs` — dependency-free pure-integer bijection: shape validation (`degrees` well-formedness), cardinality/size/off tables, `forward`/`backward` rank–unrank, `subtree` partial-path resolution | ~400-700 lines | `tests/Test_TreeRank.fs` + `blade test treerank`: round-trip pinned against **brute-force** DFS enumeration of every valid path (the OrbRank discipline), incl. degenerate shapes (single leaf, all-leaf, deep-narrow, wide-shallow) |
| **P2** — LANDED 60744b5 | Type-level registration, no storage: `IxKTree` + tag pair; lexer + parser with named reject paths; `TyTreeIdx`; `TypeLower` (placeholder idiom); `placementOf`; `Unify`; `IndexTypeValidator`; `Zonk`. Declaration + printing only; every *use* refuses loudly | ~600-900 lines, ~12 files | corpus `trees/` (its OWN category, not `index-types/`): parse OK; bad-shape rejects (empty / non-static / malformed `degrees` → BL4021; the parser's `TreeIdx<>` path → BL1999); the rendered class reaches the user through refusal messages, pinned with `ERROR-CONTAINS`; `checkKindAgreement` green; **T1 alias-laundering probe green; T2 pinned at the TYPE level only** — the three value-level strictness seams are physically unconstructible while every use refuses, so they are recorded as a P3 gate obligation in `trees/013`'s header |
| **P3+P4** — LANDED 26ab8fd (merged) | The two phases merged because corpus `// EXPECT:` pins validate through the CODEGEN lane, so an interp-only P3 can land no pinned value test. Outcome, and it is much smaller than the estimate: **~200 lines, 4 files, and NO back-end edits at all.** Construction turned out to be free — a tree binding is an ordinary rank-1 dense `Array<T,1>` that checks through the generic annotated-literal arm, so P3's construction work was *deleting* P2's let-annotation door. Reads fold at TYPECHECK: `T((c0,c1,...))` recovers the degree sequence from the Tag, resolves the leaf offset through `TreeRank.treeForwardChecked`, and rewrites to a constant subscript — so both lanes consume the same `IRLit` and are byte-identical by construction. `extents`, print and `reduce` needed zero edits. New `TreeIdxPath` error (BL4003, no new code). | ~200 lines, 4 files | `blade test trees` 28/0/0 skipped; `blade test treerank` 73/0 unchanged; **`blade test interp trees` 29/0 with every value-producing file reporting "values identical"** — that per-file report IS the byte comparison the P4 row demanded; `blade test diff-oracle trees` SKIPS (it is a pinned-oracle-binary lane, and no `oracle/Blade.exe` is pinned in this checkout — it is not an interp-vs-codegen differential); full `blade test` green |
| **P5** — LANDED | Derived dense axes, the pool retype, the open signature door, and static view composition — still with **no back-end edits**. `LeafIdx<S>`/`NodeIdx<S>` lower to PLAIN dense `Idx` records (Tag `None`, `IxKPlain`): `LeafIdx<crystal>` unifying with `Idx<5>` is the feature, since bulk numerics keep every optimization only if the axis is indistinguishable from a hand-written one. `leaves(T)` is a shadowable plain-call intrinsic that returns the SAME node with a new `.Type` — zero copy, zero nodes — which **permanently retires** the `method_for` limitation: `method_for(leaves(T)) <@> f` is an ordinary dense loop. The signature door is DELETED (P3's own result answers the ABI objection: nothing about a tree is decided at run time, and the callee folds against its own declared type). View composition `T((p))((q))` ≡ `T((p ++ q))` is a pre-typing SURFACE splice over the whole curried spine, so the landed fold is untouched and the nested-view identity holds by construction. **Escaping views CUT** — a bound partial path needs a real runtime object plus a new IR node in both lanes; it refuses through the existing "must end at a leaf". The indeterminate-depth code (BL4022) **deferred as unmintable** (see §3). | ~330 lines, 10 files | `blade test trees` 41/0/0; `treerank` 73/0 unchanged; `interp trees` 42/0 with `values identical` on all 18 value-producing files; full `blade test` green; `surface.json` regenerated for two keywords, `diagnostics.json` untouched |
| **P6** | Diagnostics through all five touch points; `docs/features.md` row; **formalism §3.2/§3.3 amended** (see risks); README row | small | `blade test surface` (full suite only) |
| **P7 (deferred arc)** | Graph arc: G1-dependent — `where acyclic(g)` + BL8012 check; `retree`/`flatten`; walk corpus (`let rec` walks, `guard` collapse) | unscoped | corpus for each; the walk examples in the feature doc §5 compile as written |
| **P8 (deferred arc)** | Sibling symmetry (`sym[...]` — iteration license only, per the design's negative storage result); `DynTreeIdx`; hash-consing | unscoped | P8 starts with math pins, not emitters |

Phase rows get their outcome edited in place as they land, icechunk-style.

## 6. v1 refusals (loud, specific, by name)

Non-static shapes (BL4021, steers to `DynTreeIdx`); reads of indeterminate
depth (BL4022, names the three ways out); out-of-arity dynamic child at
runtime (BL8003 family); shapes over a cardinality ceiling; writing a deduced
tree class down as an annotation (the BL4003/BL4015 precedent); `transpose` of
a tree; `collapse(k)` on the node axis; provider I/O; CUDA and LLVM lanes
(refuse by name); symmetric trees; dynamic insert/delete; `grad` with a shape
as the active variable (structure is not differentiable — refuse, don't
zero-fill).

Added by P3+P4, with the reasoning that decides each:

- **`method_for` / `object_for` over a tree operand: REFUSED.** Not because
  iterating a tree is meaningless — the pool *is* a rank-1 dense array — but
  because a loop former *produces* an array that inherits the operand's index
  record, and an inherited tree slot reaches output-storage classification, the
  identity/grouping machinery, fusion, and `exprTypeIfKnown`'s HM-argument
  whitelist, none of which has a tree reading (and whose failure mode there is a
  BL6001 spray, not a refusal). The clean spelling arrives with the derived
  dense leaf axis, which is a plain `Idx<card>`; re-take the verdict then.
  **P5: re-taken and KEPT, and it is now a boundary rather than a limitation.**
  `leaves(T)` retypes the same pool onto `LeafIdx` at zero cost, so
  `method_for(leaves(T)) <@> f` runs the very map this refuses (`trees/123`).
  Path addressing and bulk numerics are two jobs over one pool, and `leaves` is
  the one visible call where the user says which.
- **`reduce` over a tree: ALLOWED, and it needed no code.** The asymmetry with
  `method_for` is the whole point: `reduce` *consumes* the pool and yields a
  scalar, so nothing inherits the slot. Fold order is preorder over leaves,
  which is lexicographic over paths.
- ~~**A tree-typed function PARAMETER or RETURN: still refused.**~~ **OPENED at
  P5**, and the ABI objection turned out to be answered by P3's own result:
  nothing about a tree is decided at run time, so the callee folds against its
  OWN declared tree type, which is concrete and sits in the signature
  (`trees/125`). Two narrower refusals replace the door, both **measured to pass
  silently first** — the phase's most valuable finding:
  - **Tree argument → parameter with no tree slot** (an abstract `T^r`, or a
    plain array of matching rank). The callee would read the pool as a dense
    array and could then return, alias or iterate it with the identity gone
    (`trees/126`, `trees/008`).
  - **Tree argument → tree parameter of a DIFFERENT type.** `f(A)` with a
    `[5,0,0,0,0,0]` argument and a `[2,2,0,0,3,0,0,0]` parameter compiled and
    ran: the callee folded `(1,2)` against its own sequence and read offset 4 of
    an array with no such path. Equal cardinality is what let it through
    (`trees/111`); two aliases of the same sequence are likewise distinct
    (`trees/013`).

  Both live in a post-zonk sweep beside `collectAppRankErrors`, for that sweep's
  own reason: direct application does not unify plain-call arguments, and an
  abstract parameter is not closed at its call site, so no eager check can see
  it. The sweep restates `Unify`'s `TreeTag` predicate verbatim rather than
  approximating it — it exists because unify is never asked here, not because it
  wants a different answer.
- **Escaping subtree views: refused, and CUT from P5 deliberately.** A bound
  partial path (`let sub = T((1))`) needs a real runtime value — pointer plus
  length — hence a new IR node and both lanes, which is exactly the back-end
  work every phase of this module has found unnecessary. The three candidate
  shortcuts were each rejected on their own terms: the `RaggedRow` machinery
  would make the view wear a ragged `IxKind` (a plausible-wrong reading, and it
  destroys the view's tree identity so `sub((2))` could no longer fold);
  `IRSlice`/`IRSubset` are deliberately OFF `exprTypeIfKnown`'s whitelist and
  turning one ON is a cross-cutting change needing its own justification; and
  materializing a copy spends emitter work to buy a copy nobody asked for. The
  useful case survives anyway as *composition* (`trees/118`), and the refusal
  needed no new code — `T((1))` alone still meets "must end at a leaf"
  (`trees/119`).
- **A tree slot COMBINED with other slots: constructs, does not read.** The
  hybrid `Array<F64 like CrystalIdx, Idx<2>>` annotation is accepted and
  allocates as an ordinary rank-2 dense pool (`trees/116`); the read refuses
  (`trees/117`), because the fold rewrites the whole subscript to one literal
  and a residual trailing coordinate has nowhere to go in that rewrite. That is
  the residual-view shape, and it belongs with the partial paths.
- **A non-literal coordinate, or a wildcard, in a path: refused.** v1 folds at
  compile time, so every child selector must be a literal; a hole makes the
  path partial.

## 7. Verification

- **P1 is the oracle**: `TreeRank.fs` pinned against brute-force enumeration
  as set AND as order, standalone-`#load`-able for `proofs/` scripts (the
  OrbRank/OrbitEnum structure).
- **Interp-first (P3) then byte-diff (P4)** was the plan; what LANDED made the
  ordering moot, and better. Because the path folds to an `IRLit` at typecheck,
  the two lanes do not merely agree — they consume the identical node, so
  agreement is structural rather than tested. The gate that discharges the
  byte-comparison obligation is `blade test interp trees`, whose per-file
  verdict is literally "values identical"; `blade test diff-oracle <cat>` is a
  *pinned-oracle-binary* lane (current build vs a snapshot at `oracle/Blade.exe`)
  and skips absent that snapshot — it is not the interp/codegen differential and
  must not be reported as one.
- Corpus category is `trees` (its own, not `index-types`); `blade test interp
  trees` and `blade test diff-oracle trees` take the literal directory name.
- Full-suite runs for the Surface block; check the TOTAL line and skips.
- Provider lanes are standalone-only and out of scope until a tree ever gets
  an on-disk encoding.

## 8. Risks

- **The P0 decision is the whole feature.** The rejected variable-arrow option
  is a rank-representation rewrite; if the single-slot option later proves
  unable to express the curried spelling, the cost is a scope note, not a
  rewrite — that asymmetry is why single-slot is the recommendation.
- **G1 failing** would gut the graph arc (P7) — probe it in P0, not when P7
  starts.
- **T1 blast radius is invisible at compile time**; the P2 gate carries the
  probe for it.
- **Twin drift between P3 and P4** — the byte-comparison gate exists because
  "tests pass" hides divergence the corpus doesn't pin.
- **The formalism is a deliverable, not documentation.** §3.2's contract table
  says "arity r"; the single-tuple-slot decision fits it only once the table
  states that a slot's domain may be tuples/paths (as sparse already implies).
  Leaving the table unamended is how the next index type inherits a bad model.
- **`blade test surface` is full-suite-only**; a category-scoped green run
  will not catch a missing `diagnostics.json` entry.
- **StaticEval depth cliff** on deep generated shapes — depth, not
  cardinality, is the binding constraint; the P2 reject set includes a
  too-deep shape with a clean diagnostic rather than a stack fault.

## 9. Out of scope (recorded)

`Trace<N>` and `DAGIdx` as index types (deleted by the design revision — see
feature doc §4 for the argument); `Stream<T>` / unbounded iteration;
hash-consing / subtree sharing; dynamic and distributed trees; `fix` as a
compiler primitive (stdlib first, measure before promoting); `show_tree`
printing; MPI decomposition over subtree slabs.

## 10. P0 appendix — decision and probe results (2026-08-26)

**Decision: single tuple slot.** `TreeIdx<s>` occupies ONE arrow slot whose
domain is complete root-to-leaf paths. Whole-path application `T((c0, c1, …))`
is the primitive, routed through the tabulated dispatch (`slotPerArg` 1:1, one
tuple fills the slot — the SparseIdx rule). Partial paths are short-tuple
prefix pinning with the residual staying in the tree family (P5). Bare-scalar
`T(c)` as a prefix-of-one rides the rank-1-sparse bare-scalar precedent; probed
in P5, and if it conflicts, the curried spelling defers with a scope note in
the feature doc §3.3 — no storage or bijection impact either way.

**Rejected: variable-arrow slots.** Making slot count path-dependent rewrites
the rank representation: `IRIndexTypeG.Rank : int`, fixed-length `IRTArrow`
slots, `arr.IndexTypes.Length` at 10+ `TypeCheckInfer` sites,
`CodeGenExpr.fs:541`'s static rank sum, and the C++ `std::array<size_t, NDIM>`
key (the census in §2). Nothing about trees needs it once paths live in one
slot.

**Domain and cardinality, made explicit** (implicit in the feature doc): the
value domain is complete paths, so **cardinality = leaf count**, and leaves in
preorder are exactly lexicographic order on paths — the §3.2 enumeration
obligation holds by construction. Internal nodes are addressable only as
subtree views (P5); per-node structural computation uses the derived dense
`NodeIdx` axis. This is the precise generalization of a tensor, whose values
also live at complete index tuples.

**Payload staging (scope correction).** P2 lands the degree-sequence payload:
`let static spec = [2, 2, 0, 0, 3, 0, 0, 0]` then `TreeIdx<spec>`, validated
at lowering by `TreeRank.validateDegrees`. The nested-`leaf` literal sugar
(feature doc §2.1) is deferred to a polish commit — it needs a static `leaf`
constant (a StaticEval case or predefined binding), which is surface polish,
not substrate, and staging it keeps P2's blast radius to the proven
`IrrepsIdx` pattern.

**Probe results** (worktree baseline c8b3b49, Release build, `blade run`):

- **G1 PASSES.** `let nxt: Array<Nat<Node> like Node> = [1, 2, 0]` is accepted
  (the literal-coercion rule lifts elementwise into the tagged element type),
  and `nxt(nxt(0)) = 2` evaluates correctly; iteration-emitted `Nat<Node>`
  values also store as elements and index back (`A(ids(2))`). Caveat: a bare
  *literal* subscript against a tagged slot warns BL4003, steering to
  `(expr : Node)` — tagged flows are quiet. The graph arc's load-bearing
  assumption holds.
- **G2 CONFIRMED.** Tuple elements under `let rec` refuse — BL3999 "only
  Float/Int/Complex element types are supported" — so the parallel-recursive-
  arrays spelling in the feature doc §5.1 stays the story, as designed.
- **G5 (new).** `Array<Nat<Node> like Step>` under `let rec` is **also refused
  by the same BL3999 gate**: the §5.1 walk recurrence as written does not
  compile today. Graph-arc options (P7): spell walk state as untagged `Int64`
  and cast at the use sites, or a small BL3999 carve-out for Nat elements
  (storage-identical to Int). Does not block P1–P5.
- **G4 DEFERRED to P4** — collapse licensing is a codegen-phase concern; v1
  simply never offers the node axis to `collapse(k)`.
