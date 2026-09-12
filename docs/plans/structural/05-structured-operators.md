# 05 — Structured operators: Gram-operator application without a Gram allocation

Status: STEP 1 BUILT 2026-09-07 on `feat/streaming-reductions-cd` -- the applied-action
node `gram_apply(A, B, x)` (D1 (i), D2 core, D3 no sugar, D4 conjugate, D6 the `j`-outer
unit-stride half, D7 not built), with typecheck, IR, lowering, the native C++ arm, the
routed arm (a new `GemvT` routine and `blade_gemv_t_{s,d,c,z}` adapters, policy rows
pinned), the interpreter twin, and the AD rules (trilinear jvp; reverse with the adjoint
action as a `gram_apply` and the factor cotangents as outer products of the cotangent
with two n-cell intermediates -- each an n-cell per-column prodsum over the factor's
transpose, the v1 spelling; no m × p object anywhere). Gate §4: values exact against the
materialized and factored references (`tests/corpus/math/077-082`: same-array, distinct,
generic action, units, complex-conjugate, static refusal, compact-factor refusal, dynamic
BL8011), emission pins gate on/off (`tests/LinAlgTests.fs`), hand-derived `da`/`db`/`dx`
plus the jvp-vs-grad identity (`tests/corpus/ad-jvp-comb/109`), `interp math` and the
`linalg` block green. Timing (§4 item 3, medians of 5 interleaved whole-process runs, ms,
sizes (601, 37) / (1201, 61) / (2401, 97); the process floor is ~65 ms so only the largest
size separates): gate off M = 71 / 78 / 123, F = 67 / 67 / 70, G = 68 / 70 / 70; gate on
(OpenBLAS) M = 73 / 86 / 125, F = 67 / 67 / 70, G = 70 / 70 / 71; pragmas suppressed at
(2401, 97), gate off: M = 235, F = 74, G = 68. G is within round-to-round noise of F at
every size and 1.8x (3.5x serial) ahead of M at the largest, with the ratio growing in N as
predicted. Checksums of G and F agree to the last digit at (1201, 61) and (2401, 97) and
differ in the 15th digit at (601, 37) under the default `-ffp-contract=fast` -- g++
contracts the two nests differently, not a different operation sequence: with
`BLADE_FP_CONTRACT=off` the two checksums are bit-identical (937.072865780409 both) while
M's differs (…411), which is exactly §1.6's ~1e-14 between the matrix and the factored
action. Step 2 (the operator VALUE, `solve`-family factorization values,
separability) stays a follow-on co-designed with fortran-killer-2 §6.2, as §3.4 says.
Elaborated (as a DESIGN) item 5 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md)
("Preserve structured operators until their consumers decide what to compute").
Baseline: HEAD `e7abf38`, read from a SHARED working tree that at the time carried other
sessions' uncommitted edits to `Blade.fsproj`, `src/Cli.fs`, `src/CliCommands.fs`,
`src/IR.fs`, `src/IRMono.fs`, `src/Lowering.fs`, `src/Optimize.fs`, `src/TypeCheckInfer.fs`,
`src/TypeCheckSupport.fs`, `src/TypeEnv.fs`, `src/TypedAst.fs`, `tests/OptimizeTests.fs` —
so a `file:line` in one of those files is exact for that tree and may be offset by a few
lines against `e7abf38` itself. `bin/Release/net10.0/Blade.exe` built 2026-09-06 00:43.
Probes were emitted and run from a private directory under `%TEMP%` (`blade-op05-*`) with
that binary, never from the checkout.

Companions this design must compose with, not duplicate:

- [plan-cosmology-nongaussianity.md §5.3](../plan-cosmology-nongaussianity.md) — separability as a
  *verified declaration* discharged through `Constraints.Discharge`; "advise, never rewrite".
- [plan-fortran-killer-2.md §6.2](../plan-fortran-killer-2.md) — a persistent factorization
  value (`solve` family: factor once, solve/transpose-solve/differentiate repeatedly).
- [plan-fortran-killer-2.md §6.3](../plan-fortran-killer-2.md) — matrix-free linearization
  (array-output JVP/VJP actions feeding a Newton–Krylov loop).
- [plan-deferred-combinators.md](../plan-deferred-combinators.md) — what "deferred" is
  mechanically (a property of node kind, not type) and why every new node pays the
  three-consumer tour.

Legend: **[V]** verified against source or by a probe run; **[I]** inference or design
judgement. "Gram of A" throughout means `gram(A, A) = A·Aᴴ` (N×N from A: N×D); the
memo's `A Aᵀ` is the real case.

---

## 1. Verified current state

### 1.1 `gram` end to end

**Surface.** `gram` is a core keyword (`src/Lexer.fs:102`, `:222`), parsed as a strictly
binary form `gram(A, B)` into `ExprGram (left, right)` (`src/ParserGrammar.fs:716-722`;
`src/Ast.fs:513`). `hermitian(A)` is parser sugar for `conj(transpose(A, [0, 1]))`
(`:702-710`) and is unrelated to gram's Hermitian *storage*. [V]

**Typecheck** (`inferGram`, `src/TypeCheckInfer.fs:3322-3422`, dispatched at `:1643-1644`):

- both operands must be rank 2 with exactly two *plain* slots — a compact rank-2 group
  (e.g. a gram result) is refused with `GramCompactOperand` → BL3007, message says
  `decompact` first (`:3336-3352`; corpus `tests/corpus/index-types/241`);
- contracted (trailing) extents must agree when both are static (`:3358-3364`);
- one element type, operands must already agree (no implicit widening; `:3383-3385`);
- units multiply (`unitRulesForOp OpMul`, `:3392`);
- **same-array is a syntactic test: both operands are bare `TExprVar`s with the same name**
  (`:3403-3406`). `let B = A; gram(A, B)` is dense. Same-array → one compact rank-2 group,
  `SymHermitian` if complex else `SymSymmetric`, extent from A's leading axis; distinct →
  two fresh plain slots m×p (`:3411-3421`). The result carries `identity = None`
  (`mkArrayArrow … None`, `:3417`, `:3421`). [V]

**Typed/IR node.** `TExprGram (l, r, isSameArray)` (`src/TypedAst.fs:304`) lowers 1:1 to
`IRGram (l, r, isSameArray)` (`src/Lowering.fs:817-818`; `src/IR.fs:176`). `typeOf` has a
twin of `inferGram`'s join (`src/IR.fs:2765-2797`), but it *reuses* the operand slots where
`inferGram` mints fresh ones — a known, contained drift (memory: gram-combinator-review). [V]

**Eagerness.** `IRGram` is an eager, materializing node. It is deliberately absent from
`isInlineForm` (`src/IRLift.fs:17`, `:25` — the header says it "enters only via the `gram`
keyword's let-RHS", a premise `:202-208` records as false for a *consumed* gram), but it is
in the operand-hoist census (`:60`) and the expression-position hoist arm (`:202-208`, added
after `gram(A, A) * 2.0` read an undeclared `arr0`), with an evaluate-once lift of its
operands (`:660-665`) because the emitter spells each operand several times. Nothing in
`IRType` marks gram as anything but an array. [V]

**Codegen.** A let-bound gram goes through `genGramBinding` (`src/CodeGenBinding.fs:71-72` →
`:2331-2348`), which calls the shared `materializeGramForm` (`src/CodeGenExpr.fs:2433`;
dispatch `:1475-1476`). That form:

- allocates the *result pool* itself — packed upper triangle for same-array (`allocate<…,
  G_symm>` with `symmVec = [1; 1]`), dense m×p otherwise;
- asks `LinAlgPatterns.classify (IRGram …)` then `resolveNodeRoute` for a backend
  (`:2506-2528`); a routed call emits ONE `blade_linalg::blade_gram_same_{s,d,c,z}` /
  `blade_gram_distinct_*` line; **the default (gate off) emits Blade's own loops**;
- same-array native arm: `BLADE_OMP_PARALLEL_FOR_DYNAMIC` over rows `__gi`, triangular
  `__gjr`, contracted `__gk` accumulating into `__gacc` — explicitly "not a reassociation
  and needs no licence" (`:2574-2600`);
- distinct arm: dense, with a BL8011 runtime contracted-extent guard when the two trailing
  extents are not the same literal (`~:2610-2640`) and an unroll-and-jam of the output axis
  (`jamR`, `:2740-2774`; corpus `math/063`). [V]

**BLAS route.** `classifyGram` (`src/LinAlgPatterns.fs:759-804`) maps same-array → `Syrk`
(`RouteGramSame`, packed-triangular result) and distinct → `Gemm` with B transposed
(`RouteGramDistinct`). The shim's same-array adapter **stages a full dense m×m `Cfull`** and
repacks it (`src/cpp/blade_linalg.hpp:220-222`, `:239-247`), so under BLAS the materialized
Gram costs N² *plus* the packed N(N+1)/2. Availability: `blasAvailable ()` =
`not (reproScopeActive ()) && resolveBlasTier () <> TierOff` (`:115-118`); tier resolution
reads `BLADE_BLAS` / `BLADE_BLAS_LINK` / `OPENBLAS_DIR` per call (`:87-92`). Policy rows:
`policy` (`:525-551`); omp-vs-BLAS precedence per level (`ompPolicy`, `:606-615`: L1/L2
omp wins, L3 BLAS wins). [V]

**Interpreter twin.** `Interp/Loops.fs:2163-2166` → `ArrayOps.gramArray`
(`src/Interp/ArrayOps.fs:1096-1150`): width-exact fold, upper-triangle compact write for
same-array, BL8011 twin for the contracted extent. `blade test interp math` byte-compares it
against the native arm; the harness clears `OPENBLAS_DIR`/`BLADE_BLAS` for that reason
(`src/CliSelfTests.fs:2252`). [V]

**LLVM lane.** `src/EmitLlvm.fs` has no arm for `IRGram`/`IRMatmul`/`IRSolve`/`IRTranspose`;
an unknown node refuses the *whole program* (`:2153-2154`) and the C++ lane compiles it.
Any new node inherits this fallback for free. [V]

### 1.2 `solve` and `matmul`

- `m.matmul(A, B)` / `m.solve(A, b)` are math-package intrinsics: `MathElaborate` rewrites
  them to `__math_matmul` / `__math_solve` markers (`src/math/compiler/MathElaborate.fs:227`),
  which `inferExpr` recognizes (`src/TypeCheckInfer.fs:1034`, `:1056`) and types via
  `inferMatmul` (`:3425-3470`, Float64 only, two plain axes each) / `inferSolve`
  (`:3571-3640`, Float64, square, rank-1 rhs, no virtual operands). They lower to
  `IRMatmul` / `IRSolve` (`src/Lowering.fs:819-824`; `src/IR.fs:181`, `:189`). [V]
- `materializeSolveForm` (`src/CodeGenExpr.fs:3091`) runs partial-pivoted LU on a **fresh
  working copy every call** (stack buffer ≤ 64 cells, else `std::vector`), byte-pinned to
  `ArrayOps.solveArray` (`:1206`); the LAPACK `?gesv` arm agrees only to ~1e-14 (`:3108`).
  No factorization value exists — exactly the §6.2 gap. [V]

### 1.3 Matrix–vector: how an operator is *applied* today

There is **no matvec node**. Matrix–vector is written as a per-row apply whose kernel is
`prodsum(row, x)` (`tests/corpus/math/060_gemv_linalg_dispatch.blade`), and
`(|BlasL2|_|)` (`src/LinAlgPatterns.fs:1113-1210`) recognizes exactly that nest — one real
rank-2 input peeled once, kernel body `IRProdSum [row; vec]` with the row FIRST, `vec` not
a kernel param — and routes it to `blade_gemv_{s,d,c,z}`, **`CblasNoTrans` only**
(`src/cpp/blade_linalg.hpp:42`, `:131`, `:147-152`). [V]

Consequences that shape this design:

- A **row** view is free: `IRCurry` renders as `A[i]` (`src/CodeGenExpr.fs:390`), i.e.
  `Array<double,1> { A.data[i], A.extents + 1 }` — no copy (seen in probe output). [V]
- A **column** has no view. `Aᵀx` must go through `transpose(A, [0, 1])`, which is a *hard*
  transpose: fresh pool at swapped extents plus a copy nest (`genTransposeBinding`,
  `src/CodeGenBinding.fs:2238-2255`; interpreter `Interp/Loops.fs:2142-2143`). [V]
- `prodsum` refuses compact (symmetric/Hermitian) operands (`inferProdSum`,
  `src/TypeCheckInfer.fs:3027`), and a rank-2 compact group cannot be peeled into rows
  (BL4004), so applying a *materialized* Gram needs `decompact(G, 0)` — a second dense N×N
  pool (`genDecompactBinding`, `src/CodeGenBinding.fs:2287`; the tail of
  `src/LinAlgPatterns.fs:1403-1416` records that `blade_symv` exists in the shim with no
  surface to reach it). [V]
- The transpose-free adjoint spelling (scale rows by `x(i)`, fold the leading axis) does not
  typecheck today: probe B was refused with BL3999 "this kernel combines rank-1 slices, but
  a rank-3 operand's leading-axis slices have rank 2" — the co-iterated `zip(A, v)` map is
  counted as rank 3 by the leading-axis route (`src/TypeCheckInfer.fs:2226ff`). [V] Not
  pursued further here; noted so nobody proposes it as the "best existing spelling".

### 1.4 Is there any lazy / deferred *operator* value today?

- Deferral is a property of **node kind**, not type (`plan-deferred-combinators.md` §1.1,
  lines 85-120): `IRApplyCombinator`/`IRComposeApply`/`IRZip`/virtual `range` are deferred
  until `IRCompute` or a consumer forces them; **inside callables lowering forces every
  bare-combinator let** (`forceBareCombinatorLets`, `src/Lowering.fs:332-348`;
  `forceReturnCombinator`, `:371-387`). At module level a deferred binding prints nothing
  unless some consumer forces it (`tests/corpus/loops/095`). [V]
- Loop objects (`method_for`, `object_for`), `<@>` applications, `>>@` compositions
  (`IRComposeApply`, `src/IR.fs:100`, `:430`) and `reynolds(g)` are values you store and
  apply later — Blade's existing "operator-shaped" values, all of them **per-cell kernels**
  over an index space (`TypedApplyInfo`, `src/TypedAst.fs:89-115`: `Kernel`, `Arrays`,
  `Identities`, `SymcomStates`, `KernelInputRanks/OutputRank`). None of them is a linear
  map on *whole arrays*. [V]
- `gram`, `prodsum`, `matmul`, `solve`, `transpose`, `decompact` are all eager. `|> compute`
  (`ExprCompute`/`IRCompute`, `src/Ast.fs:475`, `src/IR.fs:114`) is the only explicit
  materialization point and applies to *computations*, never to a matrix. [V]
- The optimizer (`optimizeModule`, `src/Optimize.fs:184-191`) does constant-match folding and
  elementwise-chain fusion only — no algebraic rewriting. Pipeline fusion (`>>@` chains) is
  a **pre-typecheck surface normalization** in `Grad.fuseProgram` (`src/Grad.fs:673-700`,
  called at `src/TypeCheck.fs:396-397`) justified by a *proved* identity (formalism 10.3).
  There is no factorization recognition anywhere (agrees with cosmology §5.3). [V]

### 1.5 What AD does with gram / solve / the factored action

AD is a **source-to-source pass on the surface AST before typecheck** (`src/Grad.fs:1-60`;
`TypeCheck.fs:396-397`). Hence: anything typecheck or lowering does is invisible to AD, and
any new construct needs its own AD arms or an explicit refusal. [V]

- `gram` has rules. JVP is bilinear, `gram(dA, B) + gram(A, dB)`, each term bound to its
  own temp (`src/GradSweeps.fs:687-692`, `:1057-1069`). Reverse: for `G = gram(A, B)`,
  `dA += gram(dG, Bᵀ)`, `dB += gram(dGᵀ, Aᵀ)` — *whole-array grams over the materialized
  cotangent buffer `__g_G`* (`:355-380`), operands must be named with static dims; gram
  nested under a reindexing wrapper is refused (BL5500, `:355-359`; corpus
  `ad-jvp-comb/090`, `091`; the rule itself `020`). So the AD of a materialized Gram carries
  an N×N cotangent — the O(N²) reappears in the backward pass by construction. [V]
- `solve`/`matmul`/`eigh` have **no** AD arms in `src/Grad*.fs` (grep returns nothing). [V]
- The factored action does not differentiate today either. Probe E (`transpose` + two
  `prodsum` row kernels under `ad.grad`): refused, `src/GradSweeps.fs:219` "cannot
  differentiate through 'prodsum'". Probe E2 (same with `reduce(col * x, (+))` kernels):
  refused, "kernel parameter 'col' is rank-carrying … reverse mode lowers rank-0
  (scalar-cell) kernels (v1)". Probe F (materialized: `gram` + `decompact` + row kernel):
  refused at `decompact` (`src/GradNormalize.fs:228`, "each map operand to be a named array
  with statically-known extents"). [V] Neither spelling of Gram-operator application is
  AD-able now; an operator node with its own rules would be *new* capability, not a
  performance tweak.

### 1.6 The emission probe: what `gram(A, A)` applied to a vector allocates

Program (N = 7, D = 5, `Idx<7>`/`Idx<5>`, values exact in binary), three spellings, run
with the ambient gate (`OPENBLAS_DIR` set ⇒ BLAS on) and with `BLADE_BLAS=0`:

```blade
let G     = gram(A, A)                                   // packed N(N+1)/2
let Gd    = decompact(G, 0)                              // dense N*N
let y_mat = method_for(Gd) <@> lambda(row) -> prodsum(row, v) |> compute
let At    = transpose(A, [0, 1])                         // dense D*N copy
let t     = method_for(At) <@> lambda(col) -> prodsum(col, v) |> compute   // D
let y_fac = method_for(A)  <@> lambda(row) -> prodsum(row, t) |> compute   // N
```

Emitted C++ (`blade emit`), both gates, allocation census [V]:

| binding | pool | cells (N=7, D=5) | general |
|---|---|---|---|
| `G` | `allocate<…, G_symm>` packed upper | 28 | N(N+1)/2 (+ N² `Cfull` staging inside `blade_gram_same_d` when routed) |
| `Gd` | dense | 49 | N² |
| `y_mat` | dense | 7 | N |
| `At` | dense | 35 | N·D |
| `t` | dense | 5 | D |
| `y_fac` | dense | 7 | N |

Gate on, the same-array gram is one `blade_gram_same_d(7, 5, A.data, …, G.data)` line and
each row-prodsum nest is one `blade_gemv_d` line; gate off, the gram is the threaded
triangular nest of §1.1 and the row-prodsum nests are serial `for (__i0)` loops with an
inline IIFE per row (no pragma). Values agree across the three spellings at this size
(`y_mat = y_fac = [682.5, 890, 1097.5, 1305, 1512.5, 1720, 1927.5]`). [V]

The *explicitly factored* spelling therefore already avoids every O(N²) object, but pays an
O(N·D) transpose copy that a transposed gemv would not. A generic
`function gram_apply(M: T^2, x: T^1) -> T^1` with that body typechecks and runs (probe A),
so the factored operator is *writable in the surface language today*. [V]

Indicative timing, single machine, checksum-only programs at N = 601, D = 37 (not a power
of two), three interleaved runs each, `completed in` line [V, indicative only]:

| gate | materialized (`gram`+`decompact`+gemv) | explicitly factored | ratio |
|---|---|---|---|
| BLAS on | 3.7 – 5.2 ms | 0.18 – 0.25 ms | ≈ 20× |
| BLAS off | 3.6 – 4.2 ms | 0.18 – 0.21 ms | ≈ 18× |

Bitwise, the two are **different computations**: `Σ|y_mat − y_fac|` = 1.73e-7 (gate on) /
6.79e-7 (gate off) against `Σ|y_mat|` = 3.74e7, i.e. ~1e-14 relative. [V] This is the
measured fact behind §2.4: `(A Aᵀ) v` and `A (Aᵀ v)` are not interchangeable under a
byte-identity contract.

### 1.7 What does not exist (the gap, stated precisely)

1. No node, type, or value represents "the operator A·Bᴴ *without* its matrix". [V]
2. No transposed-operand matvec: the shim has no `CblasTrans` gemv and no nest pattern for
   `method_for(transpose(A)) <@> prodsum` (`blade_linalg.hpp:42`; `LinAlgPatterns.fs:1113ff`). [V]
3. No AD rule closes over a matrix-free action (§1.5). [V]
4. `LinAlgPatterns` is *library routing*: it classifies IR nodes and lowered nests and picks
   an entry point; it never rewrites algebra (`:1-19` header; the memo's point stands). [V]

---

## 2. Semantics

### 2.1 What an "operator value" is, in Blade terms

An **operator** is a linear map between two indexed vector spaces, `O : (K → T) ⇒ (N → T)`,
carrying enough *constructor-derived* structure that its consumers can be answered without
the N×K matrix. Its consumers, and only these, are:

| consumer | meaning | result |
|---|---|---|
| `apply(O, x)` | y = O x | `Array<T like N>` |
| `adjoint(O)` | the operator Oᴴ : (N → T) ⇒ (K → T) | an operator |
| `materialize(O)` | the matrix | `Array<T like N, K>` (compact-symmetric/Hermitian when O is a self-Gram, exactly `gram`'s type today) |
| `solve`-family | x with O x = b; factor/refactor | §6.2's factorization value (later) |
| `ad` rules | tangents/cotangents of `apply` through O's factors | actions and outer products, never an N×K cotangent |

**Cell reads are not a consumer.** `O(i, j)` on an unmaterialized Gram costs O(D) per cell;
a consumer that reads cells one at a time (print, `decompact`, `(i, j)` indexing, a compact
fold) is a *materialization demand* and must either force `materialize(O)` or be refused —
never answered by recomputing per cell (that is O(N²D) work for the same N² storage saved,
i.e. strictly worse than materializing). [I — design rule derived from the cost model]

This differs from Blade's existing deferred values, which are all *per-cell producers*
(`plan-deferred-combinators.md` §2.1, lines 173-182): a deferred array answers `read cell`
cheaply; an operator answers `apply` cheaply. That is why an operator cannot be modelled as
"a deferred `gram`" — the consumer set is different, not merely the eagerness.

### 2.2 Constructor-derived structure: representation

The structure worth carrying is a small closed family, each with its action, adjoint and
materialization given by construction:

| constructor | action `O x` | adjoint | materialize | note |
|---|---|---|---|---|
| `Gram(A, B)` = A·Bᴴ (A: N×D, B: K×D) | `A (Bᴴ x)`, O(ND + KD) | `Gram(B, A)` | today's `gram(A, B)` | **`gram(U, V)` with D = r IS the rank-r factor form U·Vᴴ** — "low-rank covariance operator" is not a separate constructor |
| `Gram(A, A)` | `A (Aᴴ x)` | itself (self-adjoint) | packed sym/Herm, today's same-array `gram` | the memo's first target |
| `Diag(d)` | `d * x` (elementwise) | itself | dense with zeros, or a diagonal index type | elementwise lifting already spells the action |
| `Scale(c)` | `c * x` | itself | — | trivial |
| `Kron(P, Q)` (separable) | reshape–apply–apply | `Kron(Pᴴ, Qᴴ)` | dense P⊗Q | cosmology §5.3's declared separability; **not step 1** |
| `Sum`, `Compose` | by cases | reversed | by cases | later |

Where should this structure live — in the type system or in a deduction lattice?

- **Compare symmetry deduction.** `Deduce.fs` computes a *parity lattice* (`PInv/PNeg/PConj/
  PBottom`, closed-world default "no claim", `src/Deduce.fs:1-40`) over kernel bodies, and
  the *decision* what a parity means is taken at the apply seam and recorded on the node
  (`TypedApplyInfo.SymcomStates/Identities/SpeedupFactor`, `src/TypedAst.fs:89-115`).
  Storage class then rides an `IRIndexType.Symmetry` on the *result type*. So: facts are
  deduced bottom-up, licences are decided at a seam, and only the *result's* storage class
  reaches the type. [V]
- **Compare `ArrayIdentity`** (`src/Types.fs:98-102`: `AIDLiteral/AIDVariable/AIDParameter/
  AIDDerived (base, op)`): the existing provenance mechanism that licenses symmetric storage
  only when "the SAME array occupies the commuting positions". `AIDDerived` is defined but
  `grep` finds no producer in `src/*.fs` beyond the type. [V]
- **Recommendation for step 1 [I]:** operator structure is a property of the **node**, not
  of `IRType`. `gram` already works this way — `IRGram (l, r, isSameArray)` carries its
  factors and the symmetry decision, and `typeOf` derives the result type from them
  (`src/IR.fs:2765-2797`). An operator *application* node `IRGramApply (l, r, x)` carries
  the same factors; its result type is an ordinary `Array<T like N>`. No new `IRType`
  constructor, no change to unification, no new storage class. A first-class operator
  *value* (step 2, §3.4) would then be the un-applied node — and the plan-deferred-
  combinators warning applies: consumers discover it by inspecting the node
  (`resolveTypedExpr`, `src/TypeCheckInfer.fs:6468`), which is why step 1 does not introduce
  the value form.
- Type-level alternative (an `IRTOperator` slot kind beside `SIdx/SIdxVirt/SVal`,
  `src/Types.fs:1015-1018`) is *not* recommended now: every type walker (`Zonk`, `Unify`,
  `typeOf`, `normalizeType` `src/IR.fs:1076-1119`, the IDE surface) would need an arm for a
  single constructor family, and the ABI question (what does a function *parameter* of
  operator type receive?) is the §6.2 factorization-value question, better answered once,
  there. [I]

### 2.3 What forces materialization

For step 1 nothing does — `gram_apply` (placeholder name, see D1) returns a vector, and the
matrix is written `gram(A, B)` when wanted. The rules a *value-form* operator (step 2) must
implement, listed now so step 1 does not paint them out:

1. **Observability.** Every module-level binding is auto-printed (`src/CodeGen.fs:2358-2367`
   region; a *deferred* binding that stays unforced prints nothing, corpus `loops/095`). A
   bound operator value at module level must either print a *description* (like the
   `// c = <deferred computation>` line deferred applies emit) or be refused; it must never
   silently allocate N×N to print a matrix nobody asked for. [I]
2. **Cell demand** (`O(i, j)`, `decompact(O, d)`, `reduce`/`prodsum` over O, an `Array`-typed
   parameter or return position — "the callee forces", `src/Lowering.fs:350-366` rationale):
   force `materialize`, once, under the binding's name, and mark the binding materialized
   exactly as `forceDeferredArrayInput` does for deferred producers
   (`src/CodeGenBinding.fs:1869`). [I]
3. **Explicit request**: `gram(A, B)` today; for a value form, `O |> materialize` or reuse of
   `|> compute` — a syntax decision (D5). [I]
4. **Refusals are features**: an operator reaching a position with no rule (a `match`
   scrutinee, a `mut` initializer, a provider write) is BL7004-refused with a message naming
   `materialize`, never silently expanded. [I]

### 2.4 Numerical contract

1. **The factored action is a *declared* computation, not a rewrite.** `gram_apply(A, B, x)`
   is *defined* as the operation sequence

   ```
   t(k) = fold over i ascending from +0 of  conj(B(i, k)) * x(i)          // Bᴴ x, extent D
   y(i) = fold over k ascending from +0 of  A(i, k) * t(k)                 // A t,  extent N
   ```

   with every product and partial sum at the element width (the `narrowToElem` discipline of
   `gramArray`, `src/Interp/ArrayOps.fs:1118-1127`). The interpreter twin implements exactly
   this and `blade test interp` pins the native arm to it byte for byte, the same contract
   `solve` carries (`src/CodeGenExpr.fs:3101-3111`). [I, modelled on V]
2. **It is not the same number as `gram(A, B)` applied to `x`**: §1.6 measured ~1e-14
   relative disagreement. The compiler therefore **never** converts `gram(A, A)` +
   row-`prodsum` into `gram_apply`, nor the reverse. Same-emit/normalization principles cover
   spellings that are *provably the same operation sequence*; this pair is not one. Under the
   fastest-way taxonomy (memory: fastest-way-principles, v2 DOMINANT/PARAMETRIC/LICENSED)
   any such conversion would be LICENSED — and the licence *is* writing `gram_apply`. [I]
3. **`where repro`** (`src/Ast.fs:379-389`; `src/IR.fs:346-354`; corpus `functions/122-125`):
   inside a repro body `reproScopeActive ()` is true, so `blasAvailable`/`lapackAvailable`/
   `cublasAvailable` all answer false (`src/LinAlgPatterns.fs:115-118`, `:135`, `:221`) and
   the native loops are emitted under `BLADE_REPRO_FN` (no FMA contraction, noinline).
   `gram_apply` inherits this with no new code: its route goes through the same gates, and
   its native arm is the contract of item 1. A repro function calling `gram_apply` gets the
   declared sequence; it does **not** get `gram` — those are different functions, and repro
   pins the one written. [V for the gates; I for the composition]
4. **`omp`.** The native `A t` half is the existing per-row nest: parallel over rows is
   order-preserving (each `y(i)` is its own fold), the same argument `materializeGramForm`
   makes at `src/CodeGenExpr.fs:2574-2600`, so a row pragma needs no licence. The `Bᴴ x`
   half has two order-preserving loop orders — `k` outer / `i` inner (parallel over `k`
   without reassociation, strided reads of `B`) or `i` outer / `k` inner (unit-stride, but
   parallel over `i` would be a cross-row reduction = reassociation, unlicensed). Pick one,
   pin it in the interpreter twin, and treat the choice as a PARAMETRIC (machine-shaped)
   decision, not a semantic one (D6). An `omp` clause cannot license changing the fold
   order of a *compiler-defined* arithmetic — there is no user kernel here whose
   commutativity is in question (BL4016's territory is user folds). [I]
5. **BLAS arms** (`dgemv Trans` for `Bᴴ x`, existing `blade_gemv_*` for `A t`) agree to the
   usual last-ULP tolerance and never to the bit, exactly the standing policy for every route
   (`src/LinAlgPatterns.fs:525-551` policy text); differential harnesses run gate-off. [V]

---

## 3. Design: Gram-operator application without a Gram allocation

### 3.1 Option (a) — rewrite `gram(A, A)` applied-to-`v` into `A (Aᵀ v)` — is wrong for Blade

Four independent reasons, any one sufficient:

1. **Bits.** §1.6: the two computations differ at ~1e-14. A rewrite would change printed
   values and break byte-identity pins (`interp`/`diff-oracle` gates) unless licensed; no
   existing licence (`omp`, `comm`, `BLADE_FP_REASSOC`) means "you may factor my matrix".
   `where repro` forbids it outright, and a program *without* `repro` has still not asked. [V+I]
2. **Observability.** The consumer pattern is `method_for(decompact(gram(A, A), 0)) <@>
   prodsum(row, v)`; at module level `G` and `Gd` are auto-printed bindings, so removing them
   changes output; inside a function body they are not printed but `gram` is an eager node
   and `decompact` is "hard" — the rewrite would have to recognize a three-node consumer
   chain across lowering's forcing passes. [V]
3. **Doctrine.** Cosmology §5.3 records the repo's P3 rule: "advise, never rewrite; if the
   rewrite can't be written in the surface language, it isn't a suggestion". Probe A shows
   the factored operator *can* be written in the surface. The on-thesis compiler role is to
   *offer* the spelling (an advisory, §3.4) and to make the declared spelling fast, not to
   guess. [V for the doctrine text; I for its application]
4. **Fragility.** A nest-shape recognizer for "Gram applied" is a second copy of
   `(|BlasL2|_|)`'s matching logic with an algebraic step bolted on — exactly the "library
   routing and algebraic factorization are different decisions" confusion the memo warns
   about, and the mold `LinAlgPatterns` explicitly rejects (`:1-19`). [V]

### 3.2 Option (b), first step: an explicit **applied-action** node

**Shape.** One new surface form with three operands — the action of `gram(A, B)` on `x`:

```
gram_apply(A, B, x)   ≡   A · (Bᴴ · x)         A : N×D,  B : K×D,  x : K   →   y : N
```

(`gram_apply` is a **placeholder spelling**; see D1/D2.) The same-array case
`gram_apply(A, A, x)` is the memo's Gram-operator application. The two-operand form is
chosen deliberately: it mirrors `gram(A, B)` one-for-one, it *is* the rank-r factor form
U·Vᴴ applied (§2.2), and its AD rules close over the family (below) — a one-operand
`gram_apply(A, x)` would need the two-operand form anyway for its own tangents.

**Typecheck** (`inferGramApply`, beside `inferGram` in `src/TypeCheckInfer.fs`):

- `A`, `B`: rank 2, two plain slots each, one shared element type, units multiply — reuse
  `inferGram`'s checks verbatim (`:3336-3392`), including `GramCompactOperand` (a compact
  factor makes no sense here either) and the static contracted-extent check on the trailing
  axes;
- `x`: rank 1, one plain slot (compact refused with `prodsum`'s wording, `:3027`),
  element type unifies with the factors' bare element type; **unit of `x` multiplies in**:
  `unit(y) = unit(A)·unit(B)·unit(x)` via `unitRulesForOp OpMul` twice;
- static extent agreement of `x` with `B`'s *leading* extent (error), dynamic left to the
  runtime twin (BL8011, the wording of `materializeGramForm`'s guard);
- result: `mkArrayArrow [fresh plain slot of A's leading extent] outElem None` — **no
  symmetry claim, no `sameArray` flag needed** (the result is a vector). [I]
- No `minRank` synthesis beyond `requireArrayArgMinRank … 2 / 1` (corpus `functions/038`
  precedent). Abstract `T^2`/`T^1` parameters must work (probe A's generic function is the
  model). [I]

**Typed/IR.** `TExprGramApply (a, b, x)` → `IRGramApply (a, b, x)`; `typeOf` twin derives
the vector type from `a` (`src/IR.fs:2765-2797` pattern). Eager, statement-shaped like
`IRGram`: add to `isInlineForm`'s hoist census and the expression-position arm
(`src/IRLift.fs:25`, `:60`, `:200-208`) and the evaluate-once operand lift (`:660-665`).
Also `src/IRMono.fs:2258`, `src/CodeGen.fs:599`, `src/CodeGenLoopNest.fs:3169`, `:3284`
node lists. [I, sites V]

**Emitted C++ — native arm** (`materializeGramApplyForm`, beside `materializeGramForm`):

```cpp
// gram_apply: y = A (B^H x) -- no N x K pool
static constexpr const size_t y__t_extents[1] = { D };            // literal when baked, else B.extents[1]
Array<double, 1> y__t = { allocate<typename promote<double, 1>::type, nullptr>(y__t_extents), y__t_extents };
for (size_t __k = 0; __k < D; __k++) {                           // B^H x, transpose-free (D6 picks the order)
    double __gacc = double();
    for (size_t __i = 0; __i < K; __i++) {
        __gacc += nested_array_utilities::conj_scalar(B[__i][__k]) * x[__i];
    }
    y__t[__k] = __gacc;
}
static constexpr const size_t y_extents[1] = { N };
Array<double, 1> y = { allocate<typename promote<double, 1>::type, nullptr>(y_extents), y_extents };
BLADE_OMP_PARALLEL_FOR                                            // rows are independent folds; no licence needed
for (size_t __i = 0; __i < N; __i++) {
    const double* BLADE_RESTRICT __row = &A[__i][0];
    double __gacc = double();
    for (size_t __k = 0; __k < D; __k++) { __gacc += __row[__k] * y__t[__k]; }
    y[__i] = __gacc;
}
```

Two pools (D + N cells), no N×K, no transpose copy. The BL8011 runtime guard (`x.extents[0]
!= B.extents[0]`, and the trailing-extent guard from the gram emitter) precedes both nests.
Extents follow `literalOrRuntimeExtentOfArray` / `extentDimOfArray` / `emitExtentsTable`
exactly as in `materializeGramForm` (the `nExtent`/`mDim`/`extentsName` lines at the top of
the form, `src/CodeGenExpr.fs:2433ff`) so the value survives leaving a frame. Block comments
only (the IIFE space-join rule, `:2568`). [I, conventions V]

**Emitted C++ — routed arm.** `LinAlgPatterns.classifyGramApply` returns *two* calls: a new
routine `GemvT` (`cblas_?gemv` with `CblasTrans`, `CblasConjTrans` for complex — the same
flag logic the distinct-gram `gemm` uses at `blade_linalg.hpp:278-283`) for `t`, and the
existing `Gemv` for `y`. Add `blade_gemv_t_{s,d,c,z}` to the shim (four ~6-line adapters
beside `:138-175`), a `GemvT, L2, HostBlas, ViaShim` policy row plus its `CudaBlas, Native,
cudaPcieBound` row (the table is pinned by `blade test linalg`, so this is a test edit by
design, `:525-551`), and a `shimEntryPoint` arm (`:666-670`). Precedence: L2 is `OmpWins`
(`:606-615`); a `gram_apply` has no user kernel to carry `omp`, so the question does not
arise — the node routes whenever the gate is on. The dispatch marker names the backend as
today (`dispatchMarkerTag`). [I, sites V]

**Interpreter twin.** `ArrayOps.gramApplyArray (a, b, x, outType)`: the two folds of §2.4
item 1, width-exact, BL8011 twins; wired at `Interp/Loops.fs:2163` beside `IRGram` and the
`Core.fs:923` node list. `blade test interp math` then pins native = interpreter. [I]

**AD rules** (surface-AST, `src/GradSweeps.fs` beside the gram arms; `Grad.fs:180/203`
allowlists; `GradCommon/GradExpand/GradFusion/GradPackUnroll` walkers):

- JVP (trilinear): `d[A(Bᴴx)] = gram_apply(dA, B, x) + gram_apply(A, dB, x) + gram_apply(A, B, dx)`,
  inactive terms dropped, each active term bound to its own temp (the `:1057-1069` pattern).
- Reverse, cotangent `ȳ` (extent N), with `t = Bᴴ x` recomputed (D cells):
  `x̄ += gram_apply(B, A, ȳ)` (the adjoint action — again a `gram_apply`);
  `Ā  += outer(ȳ, conj(t))` (N×D, the shape of `A` itself — unavoidable and not quadratic);
  `B̄  += outer(x, conj(Aᴴ ȳ))` with `Aᴴȳ` = the first half of `gram_apply(·, A, ȳ)` (D cells).
  Outer products are the existing `method_for(u, w) <@> (*)` surface; **no N×K object is
  ever formed**, which is the memo's "keep the adjoint action available through AD" made
  concrete. Operands named with static dims (v1), same restriction as gram (`:360-380`);
  everything else BL5500-refused with a message naming the two-statement spelling. [I]

**Refusals** (no new BL code needed if the messages ride existing families): shape errors →
BL3007 "invalid builtin argument" (as `GramCompactOperand`); element-type disagreement → the
gram wording (`:3385`); runtime extent → BL8011; AD declines → BL5500. Minting a code costs
five touch points including `protocol/` (memory: adding-a-bl-diagnostic-code) — avoid. [I]

**Interpreter/REPL/IDE.** `Ide.fs:782` hover arm, `Zonk.fs:201`, `TypeCheckSupport.fs:178`,
`:2642` walker arms — mechanical. The REPL cell classifier is regex-on-names and unaffected. [V sites]

### 3.3 Decisions for the user (syntax-level and policy-level, none taken here)

- **D1 — Spelling of the applied action.** Candidates: (i) `gram_apply(A, B, x)` — a
  3-ary keyword form beside `gram` (placeholder used in this doc); (ii) `gram(A, B) <@> x`
  — reuses the loop-application operator; free today (an array on the left of `<@>` is a
  type error), reads as "apply the Gram loop to x", but makes `gram(A, B)` mean *matrix* in
  one position and *operator* in another — the node-kind trap of §2.2; (iii)
  `gram(A, B)(x)` — `()` is indexing; overloading it on an array argument would be the first
  non-index use of application on arrays. The doc recommends (i) for step 1 and reserves
  (ii) for the value form in step 2.
- **D2 — Where the keyword lives.** Core lexer/parser (like `gram`: `Lexer.fs:102`,
  `ParserGrammar.fs:716`) or the `math` package (`m.gram_apply`, marker `__math_gram_apply`
  like `__math_matmul`, `TypeCheckInfer.fs:1034`). Core keeps it beside `gram` and its tests;
  math avoids a new reserved word. Recommend core.
- **D3 — One-operand sugar.** Whether `gram_apply(A, x)` (= `gram_apply(A, A, x)`) is
  offered. The prior review recorded the user's decision *against* unary `gram(V)` sugar
  (memory: gram-combinator-review); consistency suggests no sugar.
- **D4 — Complex convention.** `Bᴴ` (conjugate) to match `gram`'s `A·Bᴴ`. The alternative
  (no conjugation, matching `prodsum`'s unconjugated fold) would make `gram_apply(A, A, x)`
  disagree with `gram(A, A)` applied for complex `A`. Recommend conjugate.
- **D5 — Value form (step 2) spelling and its materialization verb**: `gram_op(A, B)`, `O <@>
  x |> compute`, and `materialize(O)` vs. `O |> compute`. Deferred; listed so step 1's D1
  choice does not foreclose it.
- **D6 — Native loop order for the `Bᴴx` half** (§2.4 item 4): `k`-outer (parallel over
  `k`, strided `B` reads) vs `i`-outer (unit-stride, serial). Bit-identical per output cell
  either way; a machine-shaped choice to pin once in both twins.
- **D7 — Advisory.** Whether to add a BL4019+-style *suggestion* (never a rewrite) when
  the checker sees `method_for(decompact(gram(A, A), 0)) <@> prodsum(row, v)`: "an
  N×N matrix is formed and applied once; `gram_apply(A, A, v)` declares the factored
  action". Fits the fastest-way P3 channel; off by default until that channel exists.
  **Built 2026-09-07 as a decision record, not a diagnostic**: `Blade.Optimize.
  recordGramApplyAdvisory` matches the module-level shape (a same-array `gram`, its
  `decompact` at axis 0, an apply over it whose kernel is `prodsum(row, v)`) and records
  rule `gram-apply-advisory` as DECLINED "left as written (advise, never rewrite)" with
  the `gram_apply(A, A, v)` spelling in the evidence, so `blade plan` shows it and the
  P3 channel can lift it verbatim. Pinned in `tests/OptimizeTests.fs`.

### 3.4 Deliberately excluded from step 1, and how it composes later

- **A first-class operator *value*** (`gram_op`, `Diag`, `Kron`, `Sum`, `Compose`): needs
  the §2.3 forcing rules and a decision on whether structure stays node-level or becomes a
  type. Do it after step 1's node, twin and AD rules are pinned, and *together with*
  fortran-killer-2 §6.2's factorization value — both are "a value that is consumed by
  actions, not by cells", and one ABI/ownership answer (immutable snapshot, no aliasing to
  mutated storage) should serve both.
- **Matrix-free solvers (§6.3).** The Newton–Krylov target consumes exactly an operator's
  `apply` and `adjoint apply`. Once array-output JVP/VJP exist, a linearization *is* a
  `Compose`-shaped operator value; step 1's `gram_apply` is the smallest member of the family
  and a fixture for the gate identity `dot(w, Jv) = dot(Jᵀw, v)`.
- **Separability (§5.3)** is `Kron` with a *discharge obligation*; its action is two
  reshaped applies. Same consumer contract, its own verification story — not duplicated here.
- **Contraction-order search / Galley-style planning.** Out of scope until the declared
  semantics above have a corpus.

---

## 4. First gate

**Programs.** Three spellings of the same map, in one file per size, checksum + full vector:

- M: `gram(A, A)` → `decompact` → row-`prodsum` (the materialized reference);
- F: `transpose(A, [0, 1])` → row-`prodsum` → row-`prodsum` (the explicitly factored
  reference, today's best surface spelling);
- G: `gram_apply(A, A, v)` (the candidate);

plus a distinct-factor case `gram_apply(A, B, x)` against `gram(A, B)` applied, and one
complex case. Sizes (N, D) ∈ {(601, 37), (1201, 61), (2401, 97)} — none a power of two
(memory: stride-layout facts, ~7× cache artifact); inputs generated by implicit lifting over
`range<N>`/`range<K>` so files stay small (probe `cmat.blade` shape).

**Measurements and pins.**

1. *Values.* `Σ|G − F| = 0` exactly, gate off (same operation sequence per cell — the
   interpreter pins it); `Σ|G − M| / Σ|M|` recorded (expect ~1e-14), **not** pinned to zero.
   Corpus: `tests/corpus/math/0NN_gram_apply_*.blade` with integer-valued exact inputs so
   `EXPECT:` pins are honest and `interp math` byte-compares (the `062` discipline);
   distinct, same, complex-conjugate, unit-product, extent-mismatch (`(rejects)` static,
   `(aborts)` BL8011 dynamic) cases.
2. *Allocations.* Emitted-text pins in `tests/LinAlgTests.fs` (the `:150-169` pattern): a
   `gram_apply`-only program contains exactly two `allocate<` lines for the node (D and N),
   contains no `blade_gram_same_`, no `Gd`/`decompact` nest, no `transpose` copy; gate on it
   contains `blade_gemv_t_d(` and `blade_gemv_d(` and the shim include exactly once; gate
   off, neither. `BLADE_MEMCHECK=1` run of the size-(601, 37) program as a sanity check
   (ASan misses `new[]` — memory: memcheck profile — so the text pin is the primary).
3. *Time.* Interleaved runs, medians of ≥ 5, both gates, both `BLADE_OMP_THREADS` on and
   off. Required: G ≤ F at every size (G removes one O(N·D) copy and one allocation; it may
   not be slower), and G ≪ M with the ratio growing in N. Record, do not promise, the
   speedup.
4. *AD.* `ad-jvp-comb/1NN_gram_apply_*`: jvp along random seeds equals the reverse gradient
   contracted with the seed (`resid = 0` exactly, the `020`/`091` pattern); hand-derived
   `da`, `db`, `dx` on a 3×2 case; a refusal test for the unnamed-operand case.
5. *Twin gates.* `blade test interp math` green with the new files; the `linalg` block green
   with the new policy rows.

**STOP conditions.** Stop and write the verdict instead of continuing if any holds:

- G and F cannot be made bit-identical on the native arm without a reassociation licence
  (would mean the declared operation sequence is not implementable as written);
- implementing the node requires a new `IRType` constructor or changes to `Unify`;
- the reverse rule needs an N×K cotangent buffer, or needs `transpose` copies to spell the
  adjoint action (would refute "adjoint action available through AD");
- G is slower than F at any of the three sizes on either gate;
- the node touches more than the files in §5 (a sign the "eager node like `gram`" premise
  is wrong).

---

## 5. Size and risk

**Estimate [I].** The `ExprGram` census is ~45 match sites in ~30 files (§1.1 greps): every
one needs a sibling arm. Mechanical but wide.

- Node + typecheck + lowering + C++ native arm + interpreter twin + corpus values:
  1.5 – 3 engineer-days.
- Transposed gemv route (shim adapters ×4, classifier, policy rows, `linalg` test edits):
  0.5 day.
- AD arms (jvp trilinear, reverse with outer products) + `ad-jvp-comb` corpus: 1 – 2 days.
- Docs (`formalism.md` §6.4 list at `docs/formalism.md:718`, keyword table `:1664`;
  `features.md:111` row): 0.5 day.

**Risks.**

- *Consumer expectation.* Users who write `gram(A, A)` and apply it will not be sped up;
  the design says so on purpose (§3.1). Mitigation: D7 advisory, later.
- *Twin drift.* Three consumers (C++ native, interpreter, and — by refusal — the LLVM lane)
  plus the BLAS arm; `interp math` catches the first pair, the emission tests the route.
- *Golden churn.* None expected: no typecheck desugar mints fresh ids into existing programs
  (the reason a *node* is preferred over a `TExprLet`-based desugar).
- *Complex conjugation placement* (D4) — the one place two correct implementations can
  silently disagree; pinned by a complex corpus case with a hand value.
- *Scope creep* toward the value form. The STOP conditions and §3.4 bound it.

**Ordered files to touch** (compile order matters for `.fs`; `Blade.fsproj` has
`EnableDefaultItems=false` but no new file is proposed):

1. `src/Lexer.fs` (keyword), `src/ParserGrammar.fs` (arm beside `:716`) — or
   `src/math/compiler/MathElaborate.fs` `opNames` + marker, per D2.
2. `src/Ast.fs` (`ExprKind`, beside `:513`), `src/TypedAst.fs` (beside `:304`).
3. `src/TypeCheckInfer.fs` (`inferGramApply` beside `:3322`; dispatch beside `:1643`),
   `src/TypeCheckSupport.fs` (`:178`, `:2642`), `src/Zonk.fs` (`:201`), `src/Ide.fs` (`:782`).
4. `src/IR.fs` (node beside `:176`; children beside `:1905`; `typeOf` beside `:2765`),
   `src/IRMono.fs` (`:2258`), `src/IRLift.fs` (`:25`, `:60`, `:208`, `:660`).
5. `src/Lowering.fs` (beside `:817`).
6. `src/LinAlgPatterns.fs` (`LinAlgRoutine.GemvT`, `LinAlgRoute`, policy rows `:525`,
   `shimEntryPoint` `:666`, `classifyGramApply` beside `:759`, `classify` `:954`),
   `src/cpp/blade_linalg.hpp` (`blade_gemv_t_{s,d,c,z}` beside `:138`),
   `src/cpp/blade_linalg_cuda.hpp` (no route; Native row only).
7. `src/CodeGenExpr.fs` (`materializeGramApplyForm`; dispatch beside `:1475`),
   `src/CodeGenBinding.fs` (beside `:71`; `genGramApplyBinding` beside `:2331`),
   `src/CodeGen.fs` (`:599`), `src/CodeGenLoopNest.fs` (`:3169`, `:3284`).
8. `src/Interp/ArrayOps.fs` (`gramApplyArray` beside `:1096`), `src/Interp/Loops.fs`
   (beside `:2163`), `src/Interp/Core.fs` (`:923`).
9. `src/Grad.fs` (`:180`, `:203`), `src/GradCommon.fs` (`:338`, `:405`, `:516`, `:706`),
   `src/GradExpand.fs` (`:437`, `:645`, `:793`, `:1421`), `src/GradFusion.fs` (`:255`),
   `src/GradPackUnroll.fs` (`:359`), `src/GradSweeps.fs` (`:355-380`, `:406`, `:687`, `:1057`).
10. Rewrite-arm walkers: `src/ProviderDesugar.fs:261`, `src/Unfold.fs:111`,
    `src/display/compiler/DisplayElaborate.fs:238`, `src/math/compiler/MathElaborate.fs:522`,
    `src/ml/compiler/MLElaborate.fs:1846`, `src/ml/compiler/MLCertShell.fs:122`,
    `src/ppl/compiler/PplElaborate.fs:316`, `:4026`, `src/rand/compiler/RandElaborate.fs:250`,
    `src/sgs/compiler/SgsElaborate.fs:385`, `src/spectra/compiler/SpectraElaborate.fs:407`.
11. Tests: `tests/corpus/math/` (values, rejects, aborts), `tests/corpus/ad-jvp-comb/`,
    `tests/LinAlgTests.fs` (emission + policy pins). Docs: `docs/formalism.md`,
    `docs/features.md`, this file's Status line, `docs/plans/README.md` index row.
    `protocol/` only if a BL code is minted (avoid).

---

## 6. Recommendation

**GO — for the applied-action node only**, with the value form, `solve`-family and
separability explicitly deferred to a follow-on that is co-designed with fortran-killer-2
§6.2.

Why GO: the asymptotic win is real and measured to be a *different declared computation*
(§1.6), which settles the semantics question the memo left open — no rewrite, a declaration;
the design is a fourth linalg node in an established mold (`gram`/`matmul`/`solve`: node +
`materialize*Form` + interpreter twin + `LinAlgPatterns` route), so cost and risk are
bounded and the STOP conditions are checkable; and it creates the first AD rule in Blade
whose adjoint is *matrix-free*, which is the genuinely new capability (§1.5 shows neither
current spelling differentiates).

Why not broader: an operator *type*, forcing rules, and factorization values are one design
with §6.2 and should not be decided by the Gram case alone; separability has its own
verified-declaration story in the cosmology plan.

Decisions D1–D7 must be taken by the user before implementation starts; D1/D2/D4 gate the
parser and the twin, the rest can follow.
