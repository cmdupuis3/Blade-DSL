# Plan: forward-mode AD (`ad.jvp`)

Status: **EXECUTED through F3** on this branch (F0 shared fixes + the
array-literal adjoint fix; F1 parity jvp + BL5501 + `ad-jvp/` corpus; F2
forward-only extensions incl. units; F3 composition — HVP via
`ad.jvp(ad.grad(f))` with depth-indexed tangent prefixes, and
`ad.jvp(ad.jvp(f))`; the sqrt F0.7 unit fix cherry-picked). F4
(combinators) remains future work — the per-combinator spec draft is the
companion plan. Originally written 2026-08-15 against master `db83e85`;
file:line cites below reflect that baseline, not the executed state.
Prior art: the retired future plan §2.1 (forward mode as "DComp, tangent
propagation") — never implemented; its vocabulary appears nowhere else in the
tree. The Coq side is ahead of the implementation: `proofs/BladeJacobian.v:351`
defines the literal jvp schema `tangent a da b db e := d_a e·da + d_b e·db` and
proves `tangent_joint_swap` (+ the `semantic_hypothesis_insufficient` refutation
and `per_dim_swap_not_symmetry`), currently consumed by nothing.

## 1. What and why

`ad.jvp(f)` — forward-mode AD as a pre-typecheck source-to-source transform,
sibling to `ad.grad` (`src/Grad.fs`). Tangents propagate in the same direction
as the primal, so the transform needs no adjoint sweep, no replay, and none of
grad's write-after-read discipline. Generated tangent code is ordinary Blade
source: it re-typechecks (units included), lowers, and codegens through the
existing pipeline, and the interpreter twins it for free.

What forward mode buys that reverse mode cannot:

- **Directional derivatives / tall Jacobians**: n seeds give n Jacobian
  columns; `wrt` selection is just a zero seed (no `wrt`-list machinery —
  closes half of the remaining-work item at
  `docs/features/equivariant-nn.md:362`).
- **HVPs** via forward-over-reverse `ad.jvp(f__grad)` (stage F3) — the missing
  ingredient for exact-Hessian samplers (PPL `hmc`/NUTS emit surface `ad.*`
  source at `src/ppl/compiler/PplElaborate.fs:4086-4090`) and an alternative
  route for the `ppl.laplace` open item (`docs/features/ppl.md:960-962`, which
  currently plans grad + jet).
- **A larger differentiable subset**: grad's three discipline checks
  (`checkWriteAfterRead` Grad.fs:906, `checkNoScalarOverwrite` :969,
  `checkLoopDiscipline` :989) exist only because the reverse sweep re-reads
  variables at final values (:900-905). All three are vacuous forward, so F2
  admits overwrites, general loop-carried recurrences, non-additive folds, and
  if/match.

## 2. Surface and ABI (decided)

```blade
import ad as ad
function f(x: Float, w: Array<Float like Idx<3>>) -> Float = ...

let (fv, df) = ad.jvp(f)(x0, w0, 1.0, [0.0, 0.0, 0.0])   // seed dx=1, dw=0
let j = ad.jvp(f)                                          // binding form, free
```

- `ad.jvp(f)` rewrites to `ExprVar "f__jvp"` exactly as grad does
  (Grad.fs:1390-1395); both call and binding forms come from the same
  mechanism. Argument-validation errors reused verbatim (:1396-1399).
- **Signature**: originals, then one tangent per *differentiable* param
  (shared `classifyParam`, Grad.fs:770-775), appended in original param order,
  named `__t_<param>`. Tangent param type = the primal param's declared
  `TypeExpr` **verbatim** — bare `Float` is unit-polymorphic (probe-verified),
  so substituting `TyNamed("Float",[])` would silently defeat unit checking.
- **Return**: `(τret, τret)` tuple — primal first, tangent second. No `mut`
  params at all: scalars cannot be `mut` (BL4005, TypeEnv.fs:682), forward
  mode does not accumulate, and the tuple ABI is probe-verified for scalar,
  array, and mixed component returns *when components are named bindings*.
  The known BL7001 hole is inline array literals in tuple expression position
  — the emitter must always bind components to names first, and F1 pins that
  exact shape in the corpus as a guard.
- Zero differentiable params stays an error, mirroring Grad.fs:1276-1277.
- Cost note for docs: full n×m Jacobian = n jvp calls vs m grad calls; jvp
  wins on tall Jacobians and HVPs, grad on scalar losses.

## 3. Architecture

- **One driver, one pass.** jvp lands *inside* `src/Grad.fs` for F1:
  `expandModule` scans `ad` aliases once, rewrites both `.grad` and `.jvp`
  heads, synthesizes grad requests first, then jvp over the enlarged decl
  list, iterating to a fixpoint with a depth cap, and strips `import ad`
  exactly once at the end. This dissolves the landmine at Grad.fs:1476 (grad
  deletes `import ad`, so a naïve second pass silently no-ops), needs no
  fsproj change, and shares every `private` helper without churn. Extracting
  `src/AdCore.fs` (intrinsic tables, `derivRule`, builders, `Ctx`) is the
  designated split once the file passes ~2.5k lines — with re-export shims so
  the three external consumers of the intrinsic sets (TypeCheck.fs:3848/3851/
  3867, Ide.fs:2096-2100, StaticEval.fs:754 comment) never notice.
- **Pipeline slot unchanged**: last elaborator (TypeCheck.fs:17582), so jvp
  differentiates through ML/PPL/sgs-elaborated source exactly as grad does.
  The Int-table-vs-Float-carrier classifier (`analyze`/`isFloatLit`,
  Grad.fs:835-850) must be shared, not copied — a drifted copy mis-classifies
  ML path tables.
- **Reused phases**: `preNormalizeBody` (minus the rec-array triangular
  unroll), `convertStmts`/`NStmt`, `hoistCalls`/`inlineCall`/renaming,
  `analyze`, `classifyParam`, `derivRule` (already mode-agnostic: returns
  d/du as an Expr of u; forward multiplies by the operand tangent). New code
  is one statement-parallel forward sweep `tangentOf` emitting `let __t_x =`
  beside each `let x =`, plus ABI assembly.
- **Rec arrays**: do NOT reuse grad's triangular scatter-add unroll
  (Grad.fs:455-530, an O(n²) reverse-mode artifact). Emit a paired recursive
  array with the same recursion structure (probe-verified shape) or desugar
  both to the `StmtForIn` private codegen lane (BL1003 is parser-only;
  TypeCheck.fs:6515-6517 supports the lane fully). F1 uses the lane (lower
  risk); either way the additive-only restriction lifts for free.
- **Reduce**: desugar to the lane with lockstep dual accumulators — compute
  `tacc = ∂₁g·tacc + ∂₂g·dA(k)` before `acc = g(acc, A(k))`. `<&!>` cannot
  express this (reduction joins have no cross-leg coupling,
  TypeCheck.fs:6049-6053), so the lane is the only route — and it generalizes
  past `(+)` in F2.
- **Naming**: tangents `__t_<name>`; rename grad's `__t` fresh-counter hoist
  prefix (Grad.fs:357) to `__hc` so the conventions stay apart. Prefix
  inventory to stay clear of: `__g_ __rj __red __rik __in __c __primal` (`__rk`/`__rm`, the triangular
  unroll's ordinals, retired with it -- docs/plans/structural/01).
- **Diagnostics**: new code `BL5501` "jvp elaboration error". Registration in
  the `registryEntries` list (Diagnostics.fs:224-230) **and** a phase arm at
  :296-310 — the match defaults unlisted BL55xx codes to `PhElaborate "ml"`,
  which would mislabel every jvp error. Reject pins carry `// ERROR: BL5501`
  + `// ERROR-CONTAINS:` (the bare-code-only pins in ad/007/008 are a
  weakness not to inherit).

## 4. Stage ladder

### F0 — shared-layer fixes (land first, each with pins; all fix grad too)

1. **Collision gate** (~10 lines): refuse (BL5500/BL5501) any differentiated
   function whose bindings/params start with `__g_`/`__t_` or equal
   `__primal`, or whose mangled name already exists. Fixes a probe-confirmed
   silent wrong answer in grad today: a user `let __g_x` is shadowed by the
   synthesized cotangent and `f(x)=x⁴` returns `df=0` instead of `32`.
2. **Widen `isFloatTy`/`classifyParam`** (Grad.fs:759-775) to recognize
   `TyNamed("Float",[unit])` and resolve aliases (`type Speed = Float<mps>`),
   turning today's *silent derivative drop* for unit-carrying params (mixed
   `Float`/`Float<meters>` compiles clean, partial missing, probe-confirmed)
   into either support (jvp: tangent type mirrors primal — dimensionally
   correct) or an explicit refusal naming units (grad, where `__g_x: p.Type`
   is dimensionally wrong anyway). Complex128 gets an explicit refusal on the
   same fall-through.
3. **Unknown-named-call refusal**: `adjointOf`'s catch-all (Grad.fs:1109-1111)
   silently contributes zero gradient for any named call that is neither a
   known decl nor a listed intrinsic. Refuse instead (the `ad/014` doctrine).
4. **Combinator-operator message arm**: `<@>`/`>>@`/`<&!>` etc. slip past
   `walkExpr`'s BinOp arm to a generic adjoint error; add an explicit arm
   reusing the good :290-291 message.
5. **Span fix**: `synthSpan` is reset (Grad.fs:1526) before `expand`'s
   `mapError` reads it (:1551), so synthesis-phase BL5500s land at `noSpan`.
6. **Hoist binary-intrinsic partials** (atan2/log_base, Grad.fs:1095-1108)
   into a shared `binaryDerivRule` table.
7. **`sqrt` unit hole — two defects, both wrong answers today, fixed here,
   not inherited** (analysis memo, probe-verified). (i) Odd dims: the arm
   returns `Ok None`, so `sqrt(Float<meters>)` unifies with any unit.
   (ii) Even dims, scaled unit: the arm builds its result with
   `unitOfDims`, which hardcodes `Scale = scaleOne`, so
   `sqrt(1 acre) + 1 m` evaluates to 2 instead of 64.6 — a regression from
   90d3642 (raised the scale in `unitPow` but not the sibling sqrt arm).
   Fix: `unitSqrt` in Types.fs — a signature iff all Dims exponents are
   even AND the Scale has an exact square root; otherwise BL3006 (matching
   the shipped `x ^ 0.5` refusal). Census: zero corpus/example sites rely
   on either branch. AD consequence: `d(√u) = u̇/(2√u)` then unit-checks by
   construction; without the fix, OpDiv's one-sided arm hands the tangent
   the PRIMAL's unit while the primal stays unconstrained — contradictory
   judgments on one value. (Also stale: features.md's "carries dimensions,
   not scale factors" line predates 90d3642.)

### F1 — jvp at grad parity

Policy statement: *F1 accepts exactly the programs grad accepts, by running
grad's own admissibility checks, even where forward mode is sound without
them.* That buys the differential gate; F1→F2 is then deleting check calls,
not rewriting the transform. Two deliberate deltas: the paired-rec-array/lane
encoding (§3) and the tuple ABI.

Verification (see §5): `tests/corpus/ad-jvp/` (new dir — do not grow `ad/`,
whose byte-pinned outputs and duplicate ordinals make it a bad host), 10
mirrors by slug + multi-input seeding + a basis sweep reconstructing the full
gradient + 3-4 rejects + the named-tuple-return guard pin; BL5501; interp
slice membership with a zero-skip requirement; the reject-parity census; F#
oracle jvp twins.

### F2 — forward-only wins

Drop the three vacuous checks; admit arbitrary overwrites and read-then-write;
non-additive folds via the lockstep lane; arbitrary smooth rank-1 recurrences;
if/match with a **pinned subgradient convention** (the ml oracle's zero-norm
convention, `oracles/ml/Autodiff.fs:103` — admit, don't pretend exactness).
Convert `ad/007_overwrite_rejected` into an `ad-jvp` *positive* test with a
header documenting the boundary shift. FD verification pins thresholded
booleans (`abs(diff) < tol` → `EXPECT: agree = true`), never FD residuals —
residuals are catastrophic-cancellation quantities that would flake the
byte-exact interp gate; the `loops/143` idiom is the model.

### F3 — composition

- **F3b first** (cheap): `jvp(jvp(f))` — validates the fixpoint driver with no
  new ABI (nested/flat tuple returns probe-verified).
- **F3a — HVP, forward-over-reverse** `ad.jvp(ad.grad(f))`. Reverse-over-
  forward is blocked twice (grad's `-> Float` gate and its tuple-value
  refusal); forward-over-reverse's target signature — mut gradient buffer +
  mut tangent-of-gradient buffer + `(Float, Float)` return — already compiles
  and runs (probe-verified). Scope: tangent twins for `mut` out-buffer params
  (`__t___g_p(i) += ė` beside every `__g_p(i) += e` — the only operation grad
  emits on them), flat tuple interleave for `f__grad`'s scalar-grads return,
  and BL4005 call-site compliance (callers hold `let mut` for both buffers;
  `MutPassable` forwarding already covers mut-through-mut).
- E2E: **one HVP composition corpus test, not a training loop** (`ml-e2e/002`
  already costs 147 s Debug in the interp gate). Three in-program oracles:
  FD-of-gradient (thresholded boolean), Hessian symmetry `⟨u,Hv⟩ = ⟨v,Hu⟩`
  (exact, residual pins to 0, needs no reference value), analytic quartic.

### F4 — combinators (future, own stage)

The tangent of `L <@> k` is `L <@> k'` over the same loop object — same
iteration order, symmetry, omp license, BLAS routing. Storage inheritance for
`where comm(a,b)` kernels is already proved (`tangent_joint_swap`) under one
constraint: the license is the **joint** pair swap `(a,da) ↔ (b,db)`;
declaring `comm(a,b)` and `comm(da,db)` separately is unsound
(`per_dim_swap_not_symmetry`). The param-order pairing rule of §2 preserves
recoverability. This is the largest capability jvp can reach that grad cannot,
and the C-track cross product — scope it only after F3 ships.

## 5. Verification plan

- **Pins are tolerance-based**: `tests/Expect.fs:560/645` — 1e-9 combined
  abs/rel; ~10 significant digits suffice (17-digit corpus values are
  authoring discipline). Byte-exactness binds only evaluator-vs-evaluator.
- **The F1 gate** is in-program: compute grad buffers, compute jvp tangents,
  pin `resid = tang - reduce(dbuf * seed, (+))` as `EXPECT: resid = 0`
  (residual-to-zero rides the tolerant absolute branch and is interp-safe).
  Highest-value mirrors: `010` (active-exponent two-term rule — the term a
  forward implementation drops silently) and `012` (forward gather stays a
  gather; reverse's is a scatter — different code path).
- **Reject-parity census** `blade test jvp-subset` (model:
  `tests/Test_RepRejectCensus.fs`, not Test_RepDifferential): for every
  `(rejects)` probe in `ad/`, assert same refusal or deliberate listed
  positive. In-process, no C++. This is the only guard against silent subset
  drift; a bespoke jvp-vs-grad runtime gate is NOT warranted (both transforms
  emit ordinary Blade — a corpus test is a strictly better artifact).
- **F# oracle** (`oracles/ml/Autodiff.fs` + `Tests_Autodiff.fs`, separate
  BladeML.fsproj — no Blade.fsproj change): `jvpOp` twins for the seven vjps
  in the e2e path + one-liner `jvpGather = gather` / `jvpScatterAdd =
  scatterAdd`; adjointness gate `⟨u, Jv⟩ = ⟨Jᵀu, v⟩` at ~1e-12 **plus**
  `checkJvpAgainstFd` (adjointness alone passes shared sign errors); extend
  the 27-partial model sweep with one random-direction jvp-vs-packed-gradient
  dot.
- **Harness wiring**: `category "ad-jvp"` (Test_Basic.fs:15 style) →
  `RunAll.fs:99` (default suite: yes) → Cli key `"ad-jvp"|"adjvp"`
  (Cli.fs:2861) → `m5Slice` (InterpDiff.fs:172) only after a standalone
  `blade test interp ad-jvp` prints `N/0/0` with **zero skipped** (skips
  don't affect exit codes — a jvp corpus could silently lose its differential
  coverage); update the slice doc block (its `ad` line is already stale).
  Leave `denseSlice` alone until the next oracle re-pin. New `.fs`/`.blade`
  files land LF-in-index (`git ls-files --eol` before commit); corpus files
  are byte-pinned assets. No `// WARN:` pins in F1 (strict both directions;
  would force auditing every `import ad` file). One missing pin to add while
  in the area: no ad-corpus test asserts that a grad call site with a
  plain-`let` (non-`mut`) buffer is refused (BL4005) — the call-site rule's
  applicability to synthesized functions is currently untested.

## 6. Risks and mitigations (condensed)

| Risk | Disposition |
|---|---|
| Tuple-of-array return hits BL7001 | Only for inline literals in tuple position; emitter binds names first; pinned guard test |
| `import ad` stripped by grad pass | Single driver strips once (§3) |
| ML-elaborated Int path tables misclassified | Share `analyze`/`isFloatLit`, never copy |
| Units: silent derivative drop (live grad bug) | F0.2 widens the shared classifier |
| `sqrt` odd-dimension unit hole | Promoted to F0.7 (two defects incl. a numeric wrong answer); fixed, not inherited |
| `pow` derivative emits `e - 1.0` which `staticPowExponent` can't read (dimensioned base rejected) | Emit `iLit (n-1)` for static exponents |
| Name collisions (`__g_`/`__t_`/`__primal`) | F0.1 collision gate (fixes live grad bug) |
| digamma-style unknown-derivative silent zero | Preserve the zero-vs-unknown `None` split; jvp twin of `ad/014` |
| Captures | Non-issue: synthesized decls are top-level, capture machinery is lambda-only (`buildCaptures` single call site TypeCheck.fs:12663); five corpus tests prove module-scope arrays work |
| Mut-param write permission (BL4005) | F1/F2 have zero mut params; F3a call sites use `let mut`, already `MutPassable`-covered |
| Complex | Explicit refusal (F0.2); Wirtinger needs a different ABI — do not paint in now |

## 7. Docs impact (when stages land)

`docs/features/equivariant-nn.md` §11 (drop "forward mode" from Remaining; add
jvp ABI/subset; state the certificate posture — tangent equivariance is a
theorem given the primal, mirroring the grad statement at
`oracles/ml/README.md:324-325`); `docs/features.md:369` (row hard-codes "reverse
mode"); `oracles/ml/README.md` §5 (jvp oracle description); `examples/README.md`
04/07 (natural teaching surfaces); `docs/proofs.md` (note `tangent_joint_swap`
is now consumed). House rule: cite deleted design docs as "retired <stem>
plan" only.

## 8. Out of scope (recorded)

Reverse-over-forward composition; complex/Wirtinger tangents; rank ≥ 2
recursive arrays; `axes = n` and leading-axis-fold reduce routes; combinator
differentiation before F4; framework bindings; changing `ppl.laplace`'s
grad+jet route (flag only — an HVP route exists after F3a).
