# Deferred combinators — what it would take for the eager set to compose

**Status: EXPLORATORY, 2026-08-18 — with first blood the same day (commit 5a7b119):
the anonymous-operand slice of D0/D1 is IMPLEMENTED.** `collect` sees through an
anonymous `ExprCompute` at the reduce operand root (with silent decline back to the
materializing route on terminal-shape errors), and the llvm lane consumes sole-read
operands via a `ReadOnce` census. `reduce(x * y, (+))` in a kernel body went from
alloc-fill-refold per output cell to one fused pass — 25x on the gram fixture. The
named-let (S2/`joinDeferrableIds`) half of D0, D2's composed applies, and the
lowering-minted `IRApp(IRObjectFor …)` species for the C++ lane remain open as written.
 An investigation, not a commitment. Written under a
no-build regime: every claim below is from reading the tree at `a5afec1` (plus the
in-flight IRForRange edit to `src/EmitLlvm.fs`, so that file's line numbers may drift)
and from two `blade emit` probes on tiny programs; nothing here was measured under load.
The two probe artifacts (`p1_bodylocal.blade`/`p1.cpp`, `p2_module.blade`/`p2.cpp`) live
in the session scratchpad under `defer/`.

The question, from the user: several array combinators are **eager** — `reduce`,
`prodsum`, `gram`, the intrinsic array lifts, the `mask` family — where the rest of the
language **defers** (tensors-as-SSA until `|> compute`; `object_for` pipelines compose;
`<&!>` fuses multi-accumulator folds into one pass). What would making the eager set
deferred take, and is it worth it?

The short answer, defended below: **"deferred reduce" as a new mechanism is the wrong
unit of work.** The deferred fold already exists in the IR (`IRReduceCompute`, the fused
reduction terminal) and in the LLVM lane's native evaluation model; what is eager is a
set of *typecheck-time desugars that force* (`ExprCompute` wrappers minted in
`inferReduce`'s routes and the intrinsic lifts) and a set of *deliberate declines* in
`tryInferReduceCompute`. The cheapest path that delivers the composability is to remove
specific forces and declines and to give the existing fused terminal a stage-shaped
surface — not to invent result-deferral, which for `reduce` would mean deferred
*scalars*, a concept no consumer has.

## 0.5 Why the C++ lane still materializes the gram kernel — diagnosed 2026-08-18, NOT fixed

The anonymous-unwrap fix (§0) fused this shape on the llvm lane and left the C++ lane
3.6x slower on it (`bench_sym_gram_small`: cpp 829.9 ms vs llvm-serial 232.6 ms). The
reason is now exact, and it is *not* that the unwrap failed to reach the C++ lane — the
unwrap never fires for this shape at all. The llvm win came from the other half of that
commit, `ReadOnce` consumption of the hoisted operand.

The chain, verified by IR dump (`BLADE_LLVM_DUMP_IR=1`):

1. The kernel is `lambda(x: T^1, y: T^1) -> reduce(x * y, (+))`. At typecheck, `x` and
   `y` are ABSTRACT rank-1 params, not concrete `ArrayElem`, so `inferBinOp`'s `zipable`
   test fails and the `ExprCompute(method_for(zip …) <@> λ)` synthesis never runs.
   `tryInferReduceCompute.collect` therefore sees a bare `TExprBinOp` — neither
   `TExprApply` nor `TExprCompute` — and returns `None`. No fused terminal.
2. `lowerTypedBinOp` (`src/Lowering.fs:1243-1296`) mints the elementwise application
   *at lowering*, as **`IRApp (IRObjectFor …, [IRTuple [l; r]], ty)`** — a different
   species from the `IRApplyCombinator` typecheck produces.
3. `IRLift`'s `IRReduce` arm (`src/IRLift.fs:585-590`) hoists that apply into a `let`
   via `liftChildIncludingLoopApp`, because `genObjectForApplication` spells operands by
   name and would otherwise emit an undeclared `arr<i>`.
4. The C++ lane materializes the hoisted let. The llvm lane consumes it (sole-read).

So the fix is three coordinated pieces, none of which is a one-liner:

- **Recognize the species.** `genReduceComputeBinding` (`src/CodeGenBinding.fs:80`)
  accepts `IRApplyCombinator`, `IRParallel`/`IRFusion` and `IRZip` — there is no arm for
  `IRApp (IRObjectFor …)`. Either teach it that species, or normalize the two at
  lowering (changing what every elementwise array binop emits — large golden churn).
- **Synthesize the seed.** The fused terminal requires one, and `reduce(x * y, (+))` is
  the 2-arg form, so `init` is `None`. Typecheck mints identities for `(+)`/`(*)`
  sections; an IR-level rewrite would have to recognize the builtin fold op the way
  `foldKernelBuiltinOp` does, downstream of where that lives.
- **Land it in all three consumers.** The interpreter's `IRReduceCompute` arm
  (`src/Interp/Loops.fs:1961`) must accept the same widened species, or `test interp`
  catches the drift — which is the point of the gate.

Cheapest shape I can see: an `IRLift`-level peephole that turns
`IRLet(id, <deferred apply>, IRReduce(IRVar id, k, init))` into
`IRReduceCompute(<deferred apply>, k, seed)` when `id` is read once and a seed is
available — it reuses the existing terminal rather than widening it, and it is
backend-agnostic, so both lanes and the interpreter benefit from one change. The seed
requirement is what makes it a piece of work rather than a peephole today.

Worth stating plainly: on the measured shape this is worth ~3.6x to the C++ lane, which
is the largest single number left on the table in either backend plan.

---

## 1. The eagerness census

### 1.1 What "deferred" is, mechanically

Deferral in Blade is a property of **node kind**, not of type. A deferred array is a
`TExprApply`/`TExprFusion` (typed) or `IRApplyCombinator`/`IRComposeApply`/`IRZip`/
virtual-`range` (IR) that no `IRCompute` has forced; its type is an ordinary
`ArrayElem` — nothing in `IRType` marks it. Consumers discover deferral by *looking at
the node* (`resolveTypedExpr` walks bindings: `src/TypeCheckInfer.fs:1546-1558`), which
is why the whole surface-area problem below exists.

The regimes, per scope:

- **Module level**: `let c = A <@> k` stays deferred. The C++ lane emits
  `// c = <deferred computation>` and nothing else (probe `p2.cpp`; the binding neither
  allocates nor prints). Consumers force on read through
  `forceDeferredArrayInput` (`src/CodeGenBinding.fs:1844`) /
  `forceDeferredPositionalReads` (`:1879`), and a forced binding joins the auto-print
  list (`src/CodeGen.fs:2232`, `src/CodeGenState.fs:109`; corpus pin
  `tests/corpus/loops/095_deferred_binding_forced_on_read.blade`, plus 117/129 for the
  positional-read and capture variants).
- **Inside callables**: lowering **forces**. S2 `forceBareCombinatorLets`
  (`src/Lowering.fs:332-348`) wraps every body-local bare-combinator `let` in
  `IRCompute` (except join-deferrable ids, `:326-330`); S4 `forceReturnCombinator`
  (`:374-381`) forces return leaves — "the callee forces", with the ABI rationale spelled
  at `:350-366`. Laziness deliberately does not cross a named-function boundary.
- **The LLVM lane** is deferral-native: every array is a producer closure (`ArrVal`,
  `src/EmitLlvm.fs:415-433`) and "`reduce` over an unforced computation is run the
  producer inside the fold with no array in between" (same header). The C++ lane is the
  lane that pays for eagerness.

Deferred and composing today: `method_for`/`object_for` values, `<@>` applications,
`>>@`/`@>>` compositions (`IRComposeApply`), `zip`, virtual `range`/`reverse`, and the
`<&!>` fusion tree. **`reynolds` is NOT eager** — the suspicion is wrong: `reynolds(g)`
types as the kernel's own type (`src/TypeCheckInfer.fs:1451-1453`), is consumed in the
kernel slot of applies (`:6207`, `:6476`), and lowers to a kernel wrapper
(`src/Lowering.fs:854-855`). It is a kernel-level value, as deferred as any lambda.

### 1.2 The eager set, form by form

| form | where eagerness is decided | why (documented or archaeological) |
|---|---|---|
| `reduce`, partial fold (the **default**: scalar kernel, rank ≥ 2, axes < rank) | `partialFold`, `src/TypeCheckInfer.fs:2036-2185`; the `ExprCompute` at `:2176` | rewritten to `method_for(A) <@> lambda(row) -> reduce(row,..) \|> compute` — "no new node, no new loop, no new codegen" (`:2024-2035`); unnamed operands get a minted `let` because codegen row-views need a **named buffer** (`:2096-2112`) |
| `reduce`, leading-axis fold (array-valued lambda kernel) | `leadingAxisFold`, `:2226-2333`; per-step `ExprCompute` at `:2324`, seed copy at `:2304-2313` | sequential by construction; desugared to an internal `StmtForIn` with a `mut` accumulator, **one fresh materialization per fold step** (the `matCopy`/per-step-compute shape) |
| `reduce`, rank-k full fold (axes = rank ≥ 2) | `rankKDesugar`, `:2349-2446` | hand-built nest, one scalar accumulator; **declines deferred operands with an `omp` fold kernel** to the fused terminal so the licence is not silently dropped (`:2361-2370`) |
| `reduce`, rank-1 / base | `TExprReduce` at `:2574` → `IRReduce` (`src/Lowering.fs:777-797`; `:787-796` wraps an unnamed operand in `IRLet` so the buffer has a name) | the fold RESULT is a scalar; there is no deferred-scalar concept, so the fold must run where its value is needed |
| `prodsum` | `inferProdSum` (`src/TypeCheckInfer.fs:587-588`, `:2671-2826`) → `IRProdSum` (`src/Lowering.fs:799-800`) | already a fused single pass internally (Σ∏, one loop); scalar result — same reason as base reduce |
| `gram` | keyword; `src/TypeCheckInfer.fs:3027-3101` → `IRGram` (`src/Lowering.fs:806-807`) | contraction with BLAS routing and `isSameArray` syrk detection decided from the *typed operands at the site*; an eager pool with its own extents table. A **consumed** gram had to be added to the hoist census after the fact (`src/IRLift.fs:201-208`: "its omission was a plain oversight") — evidence that each eager node keeps taxing the blessed-position surface |
| intrinsic array lifts (`cos(A)`, `atan2(A,B)`, `complex(A,B)`, …) | synthesis sites wrap in `ExprCompute` explicitly: unary arms `src/TypeCheckInfer.fs:298/434/503/561`, binary `:5164` (mechanism documented `:5121-5143`) | re-synthesized as `method_for(..) <@> lambda .. \|> compute` to ride the proven `A + B` pipeline. **Asymmetry**: `A + B` at module level defers; `cos(A)` never does — the wrapper is unconditional |
| `mask` / `intersect` / `union` / `unique` (and `sort`, `compound`) | dedicated IR nodes; binding emitters force operands via `forceDeferredArrayInput` (`src/CodeGenBinding.fs:1913-2241`) | outputs have **runtime extents**; dedup/order semantics need a materialized scan. These four are also the only codegen-side auto-materialized inline forms (`src/IRLift.fs:136-145`) |
| `matmul` / `solve` / `eigh` | eager linalg intrinsics, "evaluate-once lift" (`src/IRLift.fs:645`) | library calls; single-evaluation semantics |

Route order inside `inferReduce` (`src/TypeCheckInfer.fs:1930` entry, dispatched from
`:1444`): leading-axis (`:2335-2338`) → partial (`:2342-2347`) → rank-k (`:2447`) →
base, with the fused terminal tried first on the base path (`:2469`). One level up, the
reduction **joins** are recognized syntactically: Form 2 `reduce(legs, (<&!>))` at
`:1430-1443`, Form 1 `object_for(<&!>) <@> (r1, r2, …)` at `:4205`, leg lists recorded
at `:9281` — reductions already behave as *values* there, but by syntactic capture at
typecheck, not by first-class deferral.

### 1.3 What makes `IRReduceCompute` different from plain `IRReduce`

`IRReduce (array, kernel, init)` folds a **materialized (or named) array**
(`src/IR.fs:125`). `IRReduceCompute (computation, kernel, init)` folds a **deferred
computation** — a single apply or an `<&!>` fusion tree — in one nest with one scalar
accumulator per leaf, seed always filled by the checker (`src/IR.fs:126-143`). It is
*operand*-deferral: the fold itself still executes at the `reduce` expression's
evaluation point. The checker even names the semantics: "this is the fold stage of the
loop-object composition algebra … typed here at the forcing site"
(`src/TypeCheckInfer.fs:2458-2468`). So the deferred fold **exists**; it is reached only
when `tryInferReduceCompute` (`:1542`) does not decline. The declines (each one a forced
materialization) are:

1. **Composed applies**: `reduce((f >>@ g) <@> A, (+))` errors "not supported yet —
   force it with `|> compute`" (`:1556-1557`).
2. **Body-local lets** (`alreadyMaterializedLet`, `:1627-1639`): because S2 has already
   forced the `let`, splicing would compute the producer **twice** — the measured
   `units/065` shape (3 arrays and 2 `std::exp` passes where 2 arrays and 1 pass
   suffice, `:1566-1575`). The decline is correct *given* S2; the waste is S2 itself.
3. **Captured outer bindings** (`:1577-1594`): capture forwarding materializes at the
   boundary, so the ordinary `IRReduce` is right — same trade.
4. **Compact symmetric output** (`:1645-1658`): the BL3999 family; semantics question,
   owned by `docs/plans/plan-compact-sym-folds.md`.
5. An `omp` fold kernel **keeps** the fused terminal on purpose (`:1610-1618`) — the
   licence-preservation invariant any change here must keep.

---

## 2. What deferring would take, per form

### 2.1 The load-bearing distinction: operand-deferral vs result-deferral

Everything deferred today is an **array producer**. `reduce`/`prodsum` results are
scalars (or lower-rank arrays); a *deferred result* would mean a deferred scalar flowing
into arithmetic, conditions, extents, call arguments, prints. No consumer has a scalar
thunk model: not the C++ emitters, not the interpreter (`src/Interp/Loops.fs:1946`,
`:1961`), not the LLVM lane (scalars are registers, `src/EmitLlvm.fs` throughout), not
the REPL echo. Introducing one would roughly double the value model of three consumers
for zero fusion the operand side cannot already deliver. **Result-deferral should be
ruled out**; the rest of this plan is operand-deferral and stage composition.

### 2.2 IR changes

- **None required for the core win.** `IRReduceCompute` already is the composable fold
  stage at the IR level; `IRProdSum` is already fused internally. Un-declining
  (§2.3/§2.4) reuses them as-is.
- A **fold stage as a pipeline value** (`object_for`-of-a-fold composed with `>>@`)
  needs either (a) a new stage kind inside `ComposeApplyInfo` (`src/IR.fs:75-80`) whose
  terminal collapses the chain into `IRReduceCompute` at the consumption site, or (b) a
  typecheck-only rewrite that recognizes `… >>@ fold_stage` and re-associates it into
  `reduce(chain-without-terminal, kernel)` — (b) touches no IR and no consumer, and is
  the recommended shape. Every IR-node addition pays the full tour: `IR.fs` child
  walker (`:1833-1835` pattern), `IRValidate`, `IRMono` (deferred `Arrays`-slot
  handling, `src/IRMono.fs:1508`; beware monomorphize deleting ApplyInfo-only callables),
  `IRLift` census, three emitters, interpreter.

### 2.3 Typecheck and the blessed-position surface

Deferral is invisible in types (§1.1), so every position that may now *receive* a
deferred value must either force it or be taught to consume it. The current census of
"positions that already cope":

- Codegen auto-materialize: **only** inline `mask`/`intersect`/`union`/`unique` in a
  loop form's `Arrays` slot (`src/IRLift.fs:142-145`).
- The four IRLift hoist folds, each added after a measured `'arr0' was not declared`
  failure: `isNestedLoopComputeArg` (`:184-215` — `IRCompute`, `IRApp(IRObjectFor …)`,
  array-typed `IRApp`/`IRIndex`, `IRMatmul`, `IRSolve`, `IRGram`),
  `isInlineArrayLitArg` (`:225-228`), `isArrayValuedSelect` (`:249-255`), and the
  chained-map loop-form fold (`:257-270`).
- The loop-nest blessed scans at `src/CodeGenLoopNest.fs:700`, `:2970`, `:3112`.

Newly deferring a form widens exactly this surface: an unforced `reduce(A, (+),
axes = 1)` (array-valued) could then appear in an `Arrays` slot, an index position, a
call argument, a `where` clause, a `mask` operand… Each is a new fold or a new blessing,
and each miss is a C++-side `arr<i>` compile error surfaced as BL7004. **This is the
principal cost driver, and it scales with the number of newly-deferred forms — which is
the strongest argument for deferring nothing beyond the reduce/intrinsic-lift seams.**

### 2.4 Lowering

The S2 "callee forces" regime (`src/Lowering.fs:332-348`) is where the body-local waste
is created (§1.3 decline 2 exists only because of it). The fix that composes:
generalize `joinDeferrableIds` (`:326-330`) into a **sole-consumer analysis** — a
body-local `let` whose RHS is a bare combinator and whose *only* consumer is a fold (or
another fusable position) keeps its deferral, and `tryInferReduceCompute` drops its
`alreadyMaterializedLet` decline for exactly those ids. That moves the decision to one
place, at the IR level, with use counts available — instead of the current typecheck-time
guess about what lowering will later do. S4 (`:374-381`) is untouched: laziness still
does not cross a named-function boundary (the ABI argument at `:357-366` stands).

### 2.5 The three consumers

- **C++ lane**: un-declining shapes that today materialize means the fused-nest emitter
  (`IRReduceCompute` paths through `src/CodeGenFusion.fs` /
  `src/CodeGenLoopNest.fs`) sees operand shapes it has not seen (compose chains,
  body-local producers with captures). Reference accounting rides `ShareDecl`
  (`src/IRStorage.fs:154`, set at `src/CodeGenBinding.fs:3299`, read at
  `src/CodeGenFusion.fs:360`) and capture forwarding
  (`collectDeferredKernelCaptures`, referenced at `src/TypeCheckInfer.fs:1591`) — both
  have memory-documented seams (grouped-capture forwarding, block-local forwarding).
- **Interpreter**: `src/Interp/Loops.fs:1946` (`IRReduce`) and `:1961`
  (`IRReduceCompute`) are the twins; fold order and seed semantics are pinned by the
  diff gates, so any operand-shape widening lands in both lanes in the same change or
  `test interp`/`diff-oracle` catch it.
- **LLVM lane**: already evaluates the deferred-operand fold natively (`emitReduce`,
  `src/EmitLlvm.fs:2672` area; fused-join arm near `:2829`); a wider fused terminal
  mostly *removes* refusal pressure there. New stage kinds, if any, need arms or the
  whole program refuses by name (the lane's all-or-absent contract).

### 2.6 The REPL / notebook lane

`src/ReplSession.fs` classifies cells textually (`declRe` `:323`, `bindingName`
`:263-267`) and wraps bare expressions as `let __cellK = …` (`wrapCellExpressions`,
`:1389-1417`; assembly `:1423+`, decision sites `:952`, `:1290`). Consequences:

- A bare `reduce(...)` cell becomes a *binding* whose echo forces it — work moves from
  cell-eval to echo, same cell, observably similar. Fine.
- The session's name-keyed value memo (the 16.1s→7.8s mechanism; see the REPL session
  memo) memoizes **values**. A deferred binding has no value to memo: it must join the
  existing exclusion set (frame emitters et al.), meaning cells that bind deferred
  reduces re-run in every downstream session replay. Deferring more forms shifts more
  cells from memo-hits to re-runs — a notebook-lane *performance regression* unless the
  memo learns to key on the forced result at first echo.

---

## 3. Semantic hazards (user-visible)

1. **Print presence.** Probe `p2.cpp`: a module-level deferred binding that is never
   forced emits `// e = <deferred computation>` and **does not print**, while every
   eager `reduce` result prints today. If reduce results ever became deferrable
   bindings, any corpus test pinning `// EXPECT: r = …` on a consumed-only result would
   silently lose its line. (Operand-deferral avoids this entirely: the reduce still
   runs at its site.)
2. **Error timing.** Typecheck/lowering errors are unaffected (inference is eager
   everywhere). *Runtime* faults move: a deferred kernel that is never forced never
   faults (BL8009 halo guards, BL8006 alloc panics, arithmetic aborts); a spliced copy
   forced twice faults twice. `// ABORT:` pins are the exposed census.
3. **Evaluation count.** The splice-copy hazard is the inverse of the fusion win —
   `units/065`'s double-`exp` was *caused* by adding deferral-consumption on top of a
   force. Any un-declining must be paired with the sole-consumer analysis (§2.4), or
   ShareDecl-style sharing, so one producer never runs twice.
4. **Extents.** `mask`/`unique` outputs have runtime extents; deferring them would
   poison downstream static-extent reasoning (`extents(...)` folding,
   `src/StaticEval.fs` route). Leave them eager.
5. **Licence preservation.** `IRReduce` carries no parallel clause; the fused terminal
   is what keeps a licensed `omp` fold chunked (`src/TypeCheckInfer.fs:2361-2370`,
   `:1610-1618`). Any new route must preserve this or licensed folds serialize
   silently — the exact failure both existing declines were written to prevent.
6. **Gate censuses.** BL4003's synthesized-buffer exemption keys on the `__` prefix
   (`src/TypeCheckSupport.fs:3062`, `:1110-1114`) — new desugars minting `__` names
   inherit it; but BL4010 storage suggestions and the strict-in-both-directions WARN
   pins mean every new deduction site must be swept under `--strict-pins` and the WARN
   harness before landing.
7. **Byte-identity.** `inferReduce` caches its operand inference specifically because
   an extra inference run shifts every generated `__lambda_N` downstream
   (`src/TypeCheckInfer.fs:1944-1951`). Route changes here move ids, and ids move every
   byte-pinned golden. Budget for a golden re-pin in any phase that touches routes.

---

## 4. The payoff case, honestly

**Fresh probe evidence (this session, `blade emit` only):**

- `p1_bodylocal.blade` — `function f(xs) = { let e = method_for(xs) <@> k;
  reduce(e, (+)) }`: the emitted `f` allocates pool `__v6`, runs a full elementwise
  pass into it, then a second pass folds it (`p1.cpp:118-137`). Two passes and an
  allocation where one fused pass computes the same scalar. This is §1.3 decline 2 +
  S2, alive today.
- `p2_module.blade` — same shape at module level: fused. `e` never allocates
  (`// e = <deferred computation>`), the fold consumes `a[i] * 2.0` inline. The gap is
  precisely the function-body regime, composed applies, and the intrinsic lifts.

**Standing witnesses:**

- `reduce(cos(A), (+))` is two passes *by construction* — the intrinsic lift's
  unconditional `ExprCompute` (§1.2) prevents the fused terminal from ever seeing the
  producer. One wrapper decision away from a fused single pass.
- The leading-axis fold materializes **per step** (`matCopy` seed + per-step
  `ExprCompute`, `src/TypeCheckInfer.fs:2304-2327`). This is the temp-alloc engine of
  the LSWOSA class (`examples/lswosa.blade` `family_spectra`, cited in the route's own
  comment at `:2192-2194`), where the measured serial deficit vs classic C was 1.35x
  and attributed to temporary allocations. Fusing the per-step combine into the
  accumulator write (the slices are rectangular; the kernel is elementwise) is the one
  route where deferral means *removing allocations from a loop*, not just removing one
  pool.
- `examples/03_signal_conditioning.blade`'s `<&!>` triple is the existing proof that
  multi-accumulator single-pass folding works and pays; the ask is to make that
  mechanism reachable from ordinary code shapes instead of only the join spellings.

**Interplay with the live plans:**

- `docs/plans/plan-compact-sym-folds.md` (§4.4 "the canonical fold still deserves a
  name", §5 "what a fix must touch"): once compact folds get semantics, a deferred
  reduce over compact storage is naturally a *terminal pipeline stage* over the packed
  pool — on the LLVM lane that is `emitCompactFold` fed by a producer instead of a
  pool, which the lane's closure model already supports. The two plans meet at the same
  seam: both want `reduce` to accept a producer, not demand an array.
- `docs/plans/plan-llvm-backend.md`: the lane already implements the end state
  (producers as closures, folds consume producers). Every un-declining in the front end
  narrows the C++/LLVM behavioral gap rather than widening it.

**The alternative, argued for:** the existing `<&!>`/`IRReduceCompute` mechanism *is*
the deferred reduce. It lacks: (a) acceptance breadth (the five declines), (b) a
stage-shaped surface (fold as a `>>@` terminal), and (c) reach into the intrinsic-lift
and leading-axis shapes. All three are surface/typecheck/lowering work over existing IR
and existing emitters. A new "deferred reduce" node would duplicate `IRReduceCompute`
under another name and re-pay the three-consumer tour for nothing.

**What deferral is NOT worth for:** `prodsum` (already one fused pass; join membership
already exists — deferring changes nothing measurable), `gram`/`matmul`/`solve`/`eigh`
(BLAS/library routes chosen from materialized-operand patterns; fusing into them is not
a thing), `mask`/`intersect`/`union`/`unique` (runtime extents, §3.4), `reynolds`
(already deferred, §1.1).

---

## 5. Phasing

Effort classes: S ≈ contained single-seam change; M ≈ multi-file with corpus churn;
L ≈ cross-consumer with golden re-pins.

| phase | deliverable | effort | gate |
|---|---|---|---|
| **D0-adjacent** (LANDED 2026-09-07, structural/03 piece D) | The deferred OUTER PRODUCT under the default partial fold: `reduce(method_for(A, B) <@> lambda(a, b) -> f(a, b), op[, init])` is rewritten at typecheck into an outer apply over `A` whose row kernel is the fused fold over `B` (`partialFold`'s `outerProductRewrite`); no M×N pool. Two named rank-1 sources, no `where` on the map kernel, `(+)`/`(*)` or a seeded fold; named operands and r ≥ 3 keep the materialized route | M | `loops/206` exact differentials; `OptimizeTests.outerProductPartialFoldStreams` (no rank-2 pool) |
| **D0** | Sole-consumer deferral for body-local folds: generalize `joinDeferrableIds` (`src/Lowering.fs:326-348`) to skip S2-forcing a `let` whose only consumer is a fold; drop `alreadyMaterializedLet`'s decline for those ids (`src/TypeCheckInfer.fs:1627-1639`) | M | `p1_bodylocal` shape emits ONE pass and zero temp pools; `units/065` emits 2 arrays/1 exp pass; full suite + `test interp` + a targeted `diff-oracle loops units` green; byte-golden re-pin budgeted |
| **D1** | Fuse the intrinsic lifts into fold consumers: when the lift's sole consumer is `reduce`/a join leg, synthesize *without* `ExprCompute` (unary arms `:298/434/503/561`, binary `:5164`) so `tryInferReduceCompute` sees the producer | M | `reduce(cos(A), (+))` emits one pass; no `arr<i>`/BL7004 regressions anywhere in the corpus (the IRLift census is the tripwire); WARN/BL4010 sweep clean |
| **D2** | Un-decline composed applies: extend `collect` (`:1546-1558`) to walk `TExprApply … IsComposeApply` chains into the fused terminal (or force-and-fuse the prefix) | M | `reduce((f >>@ g) <@> A, (+))` fuses; interp twin agrees; licence preservation test: an `omp` fold kernel over a composed operand still chunks |
| **D3** | Fold stage as a pipeline terminal (surface design + the §2.2(b) rewrite): `pipeline >>@ fold((+))` or an equivalent spelling that re-associates into the fused terminal at the consumption site | L | new corpus category; formalism.md §-addition; REPL memo exclusion handled; no new IR node unless (b) proves impossible |
| **D4** | Leading-axis per-step fusion: replace the `matCopy` + per-step `ExprCompute` desugar (`:2304-2327`) with an in-place combine when the kernel is elementwise | L | LSWOSA-class benchmark (per-bin numbers, house discipline) shows the temp-alloc deficit closing; `test interp` on recursive/leading-fold corpora green |
| **out of scope** | result-deferral (deferred scalars); deferring `mask`/`gram`/`matmul`/`prodsum`; crossing named-function boundaries | — | ruled out in §2.1/§4 with reasons |

## 6. Risks, honestly

1. **The differential-twin burden of touching `reduce` is large.** Three consumers
   (C++/interp/LLVM) × fold order × seed semantics × licence preservation, all pinned
   by diff gates and byte goldens. Every phase above is "small diff, big blast radius";
   the A/B discipline (scope the sweep to the change's guard) applies.
2. **Id-shift golden churn** (§3.7) makes even semantically-neutral route changes
   expensive to review. Batch D0-D2 into as few route-touching commits as possible.
3. **The blessed-position ratchet**: D1 and D3 create producers in positions that have
   never held one. The four IRLift folds each exist because a position was missed; the
   honest expectation is that D1/D3 will find a fifth. Refusal-with-a-name (BL7004
   channel) is the acceptable failure mode; silent wrong answers are not.
4. **Notebook-lane regression** (§2.6): more deferral = fewer memo hits. Measure the
   quickstart notebook's cell times before/after D3.
5. **The declines being removed were each written for a measured failure** (double
   materialization, undeclared captures, silent serialization). Removing one without
   re-establishing its invariant elsewhere re-opens the original bug; each phase's gate
   names that invariant explicitly.
