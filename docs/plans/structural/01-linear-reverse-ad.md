# Structural opportunity 01: linear reverse AD through additive recurrences

Status: DESIGN, 2026-09-06. Nothing here is built. Elaborates item 1 of
[plan-structural-performance-opportunities.md](../plan-structural-performance-opportunities.md)
("Preserve recurrences through reverse AD"); the companion round-two audit's
Arc 6 framing is [plan-fortran-killer.md](../plan-fortran-killer.md) section 6
(lines 225-236), and its units-on-cotangents seam is
[plan-fortran-killer-2.md](../plan-fortran-killer-2.md) lines 243-246.

Source snapshot: master at `e7abf38` (working tree of 2026-09-06); the Release
binary used for every emission below was built 2026-09-06 00:43. Every
`file:line` cites that snapshot. "Verified" means read in source or observed in
an emission/run from a private temp directory; "inferred" is labelled as such.

**Verdict in one paragraph.** The defect is real and narrower than the memo's
"reverse branch" wording suggests. The reverse-mode pre-pass rewrites an
additive recurrence `s(n) = s(n-1) + INC(n)` into a triangular scatter-add
(`GradExpand.fs:1181-1200`) *before* anything else in the AD pipeline sees it,
so the primal half of `f__grad` is already O(n^2) and the adjoint half is
O(n^2) three times over. The forward-mode branch of the same function
(`:1145-1179`) emits the O(n) direct loop. The generic reverse machinery that
follows -- the "general overwrite" adjoint of an element write
(`GradSweeps.fs:476-488`) plus the array-read scatter rule (`:157-160`) --
already produces exactly the per-step backward carry when handed that direct
loop; the one thing it gets wrong is the *direction* of the adjoint loop
(`:489-523` replays in forward order). So the design is: emit the direct loop
in both modes, admit that one loop shape past the accumulator-discipline check
that currently refuses it (`GradNormalize.fs:972-976`), and reverse the adjoint
loop for that shape. No new IR node, no interpreter work, no change to the set
of accepted programs in the first milestone. Recommendation: **GO**.

---

## 1. Verified current state

### 1.1 Where AD sits in the pipeline

`ad.grad`/`ad.jvp` are AST-to-AST transforms run *before* typecheck:
`TypeCheck.fs:396-399` runs `Blade.Grad.fuseProgram` then `Blade.Grad.expand`
ahead of `checkProgram`. The synthesized `f__grad` is an ordinary
`FunctionDecl` (assembled at `Grad.fs:225-256`) that the typechecker, lowering,
codegen *and the interpreter* then process like user code (`Grad.fs:26-31`
states this as the design rationale). Consequence used throughout: there is no
AD-specific interpreter code (`src/Interp/Run.fs:113-115` is the only mention
of `Grad`, about declaration order), so the interpreter twin of any change here
is automatic as long as the synthesized surface uses constructs both lanes
already evaluate.

The reverse driver `synthesize` (`Grad.fs:96-256`) runs, in order:
`prepareForSweeps` (`GradSweeps.fs:1136`), then `checkLoopDiscipline`,
`checkWriteAfterRead`, `analyze`, `checkNoScalarOverwrite` (`Grad.fs:123-126`),
builds cotangent buffers for differentiable array locals from their zero
literals (`localDecls`, `Grad.fs:148-160`), seeds `adjointOf rc finalE 1.0`
(`Grad.fs:216`) and folds `adjointOfStmt` over the statements in reverse (`:218-222`).

Normalization order inside `prepareForSweeps` -> `normalizeBody`
(`GradNormalize.fs:575-621`): `preNormalizeBody` (surface pre-pass; this is
where recursive arrays are expanded), `convertBody` (surface `Stmt` ->
`NStmt`, `GradCommon.fs:596-627`), then `hoistCalls`/`inlineCall` over
`NStmt`. Results are memoized per `(callee, depth)` (`GradNormalize.fs:580-584`)
and inlined bodies are alpha-renamed by `renameNStmts`
(`GradExpand.fs:975-988`) -- which is why any recognizer added later must be
*structural*, not name-keyed (the sort machinery learned this:
`GradSweeps.fs:111-114`).

The statement fragment is three constructors (`GradCommon.fs:171-173`):

```fsharp
type internal NStmt =
    | NLet of name: string * isMut: bool * value: Expr
    | NAssign of lhs: Expr * rhs: Expr
    | NFor of var: string * lo: Expr * hi: Expr * body: NStmt list
```

`NFor` is ascending-only by construction: `toStmts` renders it as
`StmtForIn (var, lo..hi, ...)` (`GradSweeps.fs:1101`), `inferForIn` accepts
only the `a..b` range form (`TypeCheckInfer.fs:11494-11583`), lowering makes it
`IRForRange` (`Lowering.fs:1136-1143`), and codegen renders
`for (int64_t v = lo; v < hi; v++)` (`CodeGen.fs:230`, `CodeGenBinding.fs:277`;
observed in every emission below). The interpreter's arm is
`Interp/Core.fs:609`.

### 1.2 The node that destroys the recurrence

`preNormalizeBody`'s statement walk meets
`StmtLet { Value = ExprRecArray def; Type = Some annot }` and calls
`expandRecArray` (`GradNormalize.fs:445-448`), recording the buffer's extent
for later `reduce` hoisting. `expandRecArray` (`GradExpand.fs:1091-1217`)
decides everything about recurrences in AD. Verified branch map:

| Lines | Condition | Result |
|---|---|---|
| 1099-1100 | `def.Guard.IsSome` | refuse: `while`-guarded (v1); pinned by `tests/corpus/ad/023` (BL5500) |
| 1103-1104 | annotation not a literal-extent array | refuse |
| 1105-1106 | element type not Float | refuse |
| 1145-1179 | mode = jvp AND slice reads `prefix` | **direct loop** `s(0) = seed; for n in 1..N { s(n) = SLICE[prefix := s] }`; requires a seed arm (1167) and `onlyPrevReads` -- every prefix read is literally `prefix(n - 1)` (1153-1165, refusal 1170) |
| 1181-1200 | mode = grad, slice is `prefix(n-1) + REST` or `REST + prefix(n-1)` with REST prefix-free (`additiveRest`, 1137-1141), seed arm present | **triangular scatter-add**: `for k in 1..N { [s(k) += seed]; for m in 1..k+1 { s(k) += REST[n := m] } }` (inner 1188-1191, seed carry 1192-1194, outer 1195-1196) |
| 1201-1213 | slice does not mention `prefix` | direct element-write construction loop (both modes) |
| 1214-1215 | anything else with a prefix read (grad) | refuse: "only additive prefix recurrences ... and prefix-free construction are supported" |
| 1216-1217 | rank >= 2 | refuse: "only rank-1 (scalar-slice) recursive arrays are differentiable (v1)" |

The memo's citations are accurate: `:1145` is the forward branch, `:1188` the
triangular inner loop. The branch's own comment (`:1145-1151`) already
acknowledges "O(n) where grad's unroll is O(n^2)", and
`plan-forward-mode-ad.md:98-99` called the unroll "an O(n^2) reverse-mode
artifact" when jvp was designed. The unroll predates the repo reorganization
commit `a32e6d7` (the oldest history `git log -S` reaches for that text).

Why the unroll exists (`:1084-1089`, and the corpus header
`tests/corpus/ad/008_recarray_grad.blade:8-12`): the generic adjoint of an
`NFor` is a *same-direction* replay (`GradSweeps.fs:489-523`), which is exact
for independent accumulations and wrong for a scan. Rather than change the
loop rule, the pre-pass made the scan look like independent accumulations.

### 1.3 What the sweeps do with the result

Two `adjointOfStmt` arms matter (`GradSweeps.fs:421-523`):

- `NAssign` (`:461-488`). If `additiveSelf lhs rhs` matches (`x = x + e`,
  including element forms; `GradNormalize.fs:845-860`), the adjoint is
  `adjointOf e (cotangent of lhs)`. Otherwise -- the "general overwrite" branch
  (`:476-488`) -- it emits `let __c = <cot of lhs>; <cot of lhs> = 0.0` and
  then `adjointOf rhs __c`. Note that `s(n) = s(n-1) + INC` is **not**
  additive-self (the index expressions `n-1` and `n` differ), so it takes the
  general branch.
- `adjointOf` on an array read `a(idxs)` with `a` a differentiable array
  (`:157-160`) emits `__g_a(idxs) += cot` -- the scatter.

So for the direct loop body `s(n) = s(n-1) + INC(n)`, the *existing* rules
already produce, per step: save `c = __g_s(n)`, zero it, `__g_s(n-1) += c`
(the carry), and the increment's adjoint with cotangent `c`. That is the
textbook backward step. The `NFor` arm then wraps the body adjoints in a loop
over the **same ascending range** and replays the whole body first
(`:493-523`). Ascending order consumes `__g_s(n-1)` before step `n` has
deposited its carry into it, which is exactly why a same-direction adjoint is
wrong for a scan and why the triangular unroll was chosen.

The accumulator-discipline check refuses the direct loop before the sweep
could even see it: `checkLoopDiscipline` (`GradNormalize.fs:944-987`) treats
an element write whose rhs reads the array being written as an "array
recurrence ... not differentiable (v1)" (`:972-976`). This check is
reverse-mode only; jvp skips all three discipline checks
(`Grad.fs:312-320`, the F2 comment), which is why the forward branch may emit
the direct loop.

`analyze` (`GradNormalize.fs:775-830`) runs its taint pass twice precisely to
handle loop-carried dependence (`:823`, "second pass: loop-carried"), so the
direct loop already taints `s` correctly in both modes. `checkWriteAfterRead`
(`:868-920`) treats a loop as one opaque position and would not object.

### 1.4 Generated C++ for `tests/corpus/ad/008_recarray_grad.blade`

Emitted with the Release binary from a private temp directory
(`blade emit`, exit 0). The primal `h` has one loop; `h__grad` has two
triangular nests. Reformatted only for width; names are the compiler's:

```cpp
// h  (primal, untouched by AD): ONE loop
__v7[0L] = 0.0;
for (int64_t __k8 = 1L; __k8 < 3L; __k8++) {
    __v7[__k8] = (__v7[(__k8 - 1L)] + (w[(__k8 - 1L)] * xs[(__k8 - 1L)]));
}

// h__grad, forward half: the triangular unroll (O(n^2) even for the PRIMAL value)
for (int64_t __k17 = 1L; __k17 < 3L; __k17++) {
    for (int64_t __k18 = 1L; __k18 < (__k17 + 1L); __k18++) {
        __v16[__k17] = (__v16[__k17] + (w[(__k18 - 1L)] * xs[(__k18 - 1L)]));
    }
}
auto __v19 = __v16[2L];
__v21[2L] = (__v21[2L] + 1.0);                       // seed: cotangent of s(2)

// h__grad, adjoint half: outer loop REPLAYS the inner triangle, then the inner
// adjoint loop replays the accumulation AGAIN and scatters into __g_w
for (int64_t __k22 = 1L; __k22 < 3L; __k22++) {
    for (int64_t __k23 = 1L; __k23 < (__k22 + 1L); __k23++) {
        __v16[__k22] = (__v16[__k22] + (w[(__k23 - 1L)] * xs[(__k23 - 1L)]));
    }
    for (int64_t __k24 = 1L; __k24 < (__k22 + 1L); __k24++) {
        __v16[__k22] = (__v16[__k22] + (w[(__k24 - 1L)] * xs[(__k24 - 1L)]));
        double __v25 = __v21[__k22];
        __g_w[(__k24 - 1L)] = (__g_w[(__k24 - 1L)] + (__v25 * xs[(__k24 - 1L)]));
    }
}
```

Work count for extent N: the primal half does N(N-1)/2 multiply-adds; the
adjoint half does two more triangles of primal recomputation (the replay of a
replay -- `s` is accumulated three times in total, "soundly" because nothing
reads it afterwards) plus one triangle of cotangent scatters. About 2N^2
flops where 3N would do. No runtime speedup was measured; at the corpus and
gate sizes (N <= 509) this is sub-millisecond either way, and the gate below
counts loops rather than timing them.

A whole-trajectory loss (`reduce(s, (+))`, probe `t_traj`) emits the same
triangles plus one hoisted fold loop and its ascending adjoint
`__g_s(k) += __g___red` -- confirming that the trajectory case is already
*accepted* today and only its cost is wrong. A non-zero seed (`zero :: w(0) *
1.5`, probe `t_seed`) adds the seed to every `s(k)` inside the outer loop
(`seedCarry`) and scatters `__g_w(0) += c * 1.5` once per k.

The jvp emission for the same program (probe `t_jvp`, `h__jvp`) is one loop
with the tangent buffer updated in lockstep ahead of the primal:

```cpp
for (int64_t __k19 = 1L; __k19 < 3L; __k19++) {
    __v18[__k19] = (__v18[(__k19 - 1L)] + (__t_w[(__k19 - 1L)] * xs[(__k19 - 1L)]));
    __v16[__k19] = (__v16[(__k19 - 1L)] + (w[(__k19 - 1L)] * xs[(__k19 - 1L)]));
}
```

### 1.5 Which recurrence shapes reach AD at all (verified by probe)

All probes were `blade emit`/`check`/`run` from the private directory; refusal
texts are the compiler's.

| Shape (inductive arm) | grad | jvp |
|---|---|---|
| `prefix :: prefix(n-1) + f(n, params)` with `zero :: n` seed, final-only or `reduce` loss | accepted, O(n^2) (008, `t_traj`) | accepted, O(n) (`t_jvp`, ad-jvp/004) |
| same with a parameter-dependent seed `zero :: w(0) * 1.5` | accepted (`t_seed`) | accepted |
| same with **no** seed arm | refused BL5500 "only additive prefix recurrences ... (with a `zero :: n` seed arm ...)" (`t_noseed`) | refused "needs a `zero :: n` seed arm" (`:1167`) |
| lag >= 2 anywhere: `prefix(n-1) + w(n-1) * prefix(n-2)` | refused (REST mentions prefix) (`t_lag2`) | refused: "immediate predecessor `prefix(n - 1)` only" (`:1170`) |
| nonlinear in the carry: `prefix(n-1) * w(n-1)` | refused, same message (`t_nonlin`) | accepted (ad-jvp/016, ad-jvp-comb/011) |
| `if` in the slice | refused: `walkExpr` rejects `if/else` in reverse mode (`GradExpand.fs:529-539`) | accepted (ad-jvp-comb/095) |
| rank >= 2 slice | refused "only rank-1" (`t_rank2`) | refused (same arm, mode-independent) |
| `while`-guarded | refused (ad/023) | refused (`:1099`) |
| zero-history reads (`prefix(n-3)` at n < 3) | never reach: a lag >= 2 is refused first | same |
| prefix-free construction `prefix :: g(n)` | accepted, direct loop (ad/011, 012, 013; ml-e2e, ml-equiv, sgs training corpora) | accepted |

The only programs in the repository that exercise the **carry** path under
`ad.grad` are `tests/corpus/ad/008` (array param, final-only, exact dyadic
pins), `tests/corpus/ad-jvp/004` (`gd` from `ad.grad(f)(2.0)`, scalar param,
`reduce` loss, dyadic pins, plus the `tresid = jt - gd = 0` residual), and
`examples/04_trajectory_sensitivity.blade:76-85` (scalar params, final-only
`0.02 * s(40)`, transcendental increment, FD cross-checked in-file). The ML
training corpora use `let rec` under `ad.grad` only as prefix-free
construction (e.g. `ml-e2e/001:110-113`), and their SGD `wtraj` carries sit
*outside* the differentiated function. `blade test` has no `examples`
category that I could find (the one `"examples"` string in
`CliSelfTests.fs:952` is the diagnostics-registry example check), so
`examples/04`'s pins are not harness-enforced -- the implementer should
confirm.

### 1.6 Gates that exist today

- `blade test ad` = the corpus directory (`Test_Basic.fs:19`,
  `CliSelfTests.fs:2674`, loader `Corpus.fs:80`); a new `.blade` under
  `tests/corpus/ad/` is picked up automatically.
- `blade test --interp` runs the `ad`, `ad-jvp`, `ad-jvp-comb` categories
  through the interpreter and compares against the compiled binary value by
  value (`InterpDiff.fs:1-9`, slice at `:179`). This catches *evaluator* drift
  on the synthesized program; it is not an AD-correctness oracle.
- `EXPECT` floats compare at 1e-9 combined absolute/relative tolerance
  (`Expect.fs:560-568`, `:645`); an expected `0` with a ~1e-14 actual passes
  (the `scale < 1e-12` arm), which is how `tresid = 0` style pins work.
- `WARN` pins are strict both ways (`tests/corpus/README.md:46-58`). Relevant
  because a named index type (`type N = Idx<509>`) plus untagged reads
  `w(n - 1)` raises BL4003 five times (probe `t_509`, `blade check`), while an
  inline `Array<Float like Idx<509>>` annotation raises none (`t_509b`). New
  corpus tests must use the inline spelling, as 008 does.

---

## 2. The mathematics

Notation. Extent N; the seed arm writes `s_0 = sigma(theta)` and the
inductive arm runs for t = 1..N-1. Parameters theta are the differentiable
inputs (scalars and/or array cells). Loss `L = ell(s_0, ..., s_{N-1})` with
direct cotangents `g_t = d ell / d s_t` (final-only: `g_{N-1} = 1`, others 0;
`reduce(s, (+))`: all `g_t = 1`).

### 2.1 Additive carry `s_t = s_{t-1} + f_t(theta)`

Define the total cotangent (the carry) `c_t = dL/ds_t`. Each `s_t` has exactly
two consumers -- `ell` directly and `s_{t+1}` with unit Jacobian -- so the
chain rule gives

```
c_{N-1} = g_{N-1}
c_{t-1} = g_{t-1} + c_t                      for t = N-1 down to 1
dL/dtheta = sum_{t=1}^{N-1} c_t * df_t/dtheta  +  c_0 * dsigma/dtheta
```

Linear work: N-1 additions for the carry, one local derivative per step. Note
that `c_t` must be *complete* before step t's increment adjoint runs, and
`c_{t-1}` receives step t's carry -- hence descending t. Final-only loss:
`c_t = 1` for every t <= N-1, so `dL/dtheta = sum_t df_t/dtheta +
dsigma/dtheta`, and for an array parameter read once per step at a distinct
cell (`w(t-1)`) each cotangent cell gets exactly one term.

Today's triangular form computes the same value as
`sum_k g_k * sum_{m<=k} df_m/dtheta` -- N(N-1)/2 local-derivative terms
instead of N-1 -- because it treats each `s_k` as an independent sum.

Verified numerically: `t_traj` (`reduce` loss, xs = [4,5,6]) runs today to
`dh = [8, 5, 0]`, `hv = 18`, which equals `c_1 * xs_0 = 2*4`, `c_2 * xs_1 =
1*5`. The 509-length probe (`t_509b`, final-only) gives `dh(0) = xs(0)`,
`dh(507) = xs(507)`, `dh(508) = 0`, and `sum(dh) - (sum(xs) - xs(508)) = 0`
exactly.

### 2.2 Bounded lag `s_t = s_{t-1} + f(theta_t, s_{t-k})`

Two consumers become three: `s_j` feeds `ell`, `s_{j+1}` (unit Jacobian) and
`s_{j+k}` (Jacobian `df/ds_{t-k}` evaluated at the primal). The backward pass
is still one descending sweep:

```
c_{t-1} += c_t
c_{t-k} += c_t * (df/ds)(theta_t, s_{t-k})      dropped when t - k < 0
dL/dtheta_t += c_t * (df/dtheta)(theta_t, s_{t-k})
```

What must be available at adjoint time: the primal `s_{t-k}` -- which the
recursive array's buffer already holds, since the language materializes the
whole trajectory (formalism 7.5, `docs/formalism.md:977-982`) -- and the
zero-history convention (`:954-976`): a read at `t-k < 0` is the constant
zero, so its cotangent contribution is dropped, which is the exact transpose
of the implicit-zero read. In the *forward* direct loop the same convention
needs the guarded read the primal pipeline synthesizes (`guardsFor`/`guardWrap`,
`TypeCheckInfer.fs:10420-10441`), i.e. an `if`, which reverse-mode `walkExpr`
refuses (`GradExpand.fs:529-539`). This is why lags >= 2 stay refused in this
milestone (jvp refuses them for the same reason, `:1170`); lifting it needs
either `if` in the reverse fragment or a peeled startup segment (steps
1..k-1 unrolled without the lag term). Named, not designed here.

If `f` is linear in `s_{t-k}` (an AR(k) filter, `a_t * s_{t-k}`), the carry
pass is still primal-independent apart from the coefficients `a_t`; if it is
nonlinear the carry needs primal values but the structure is the same.

### 2.3 General first-order nonlinear `s_t = g(s_{t-1}, x_t)`

```
c_{t-1} = g_{t-1} + c_t * (dg/ds)(s_{t-1}, x_t)
dL/dx_t = c_t * (dg/dx)(s_{t-1}, x_t)
```

Every step needs the primal `s_{t-1}` (full storage or recomputation). Blade
already stores the full trajectory, so the buffer *is* the tape (Arc 6's
observation, `plan-fortran-killer.md:230-233`); checkpointing only becomes a
question if the rolling-window storage policy (formalism 7.5 "consumers that
read only a trailing segment get a rolling window") is ever applied to a
differentiated recurrence -- then AD must opt the buffer back into full
storage or recompute. Out of scope beyond this sentence. Also note that the
carry now *depends on the primal*, so the "carry pass then increment pass"
split possible in 2.1 is impossible here; the fused descending loop of 3.2 is
the form that generalizes.

### 2.4 Exactly which floating-point orders change, and which pins could move

Compared with today's triangular emission, under the design of section 3:

1. **Primal value `s_t`**: unchanged bitwise. Today `s(k)` is formed as
   `((0 + inc_1) + inc_2) + ... + inc_k` in ascending m (the triangle's inner
   loop), or `((0 + seed) + inc_1) + ...` with a seed; the direct loop forms
   `s(k) = s(k-1) + inc_k`, the same left-to-right sequence, and `0.0 + seed
   == seed`. The primal `h` was never triangular.
2. **Array-parameter cotangents, final-only loss**: unchanged bitwise. Today
   each cell `__g_w(m-1)` receives one nonzero term `c * xs(m-1)` at k = N-1
   and N-2 zero terms `0.0 * xs(m-1)`; adding `+-0.0` (or `fma(0, y, x)` under
   the default `-ffp-contract=fast`) returns x exactly for finite operands.
   The new form emits the one nonzero term. `tests/corpus/ad/008` is in this
   class.
3. **Scalar-parameter cotangents, final-only loss**: same set of nonzero terms
   (one per step, `c = 1`) but summed in **descending** t instead of today's
   ascending m. Round-off can differ at the ULP level. `examples/04`'s
   `dE_dvx0`/`dE_dvy0` are in this class; they are cross-checked in-file
   against central differences at ~1e-8 and are not harness pins, so no pin
   moves, but the file's header comment (lines 70-75) describes the triangular
   route and should be updated. `ad-jvp/004`'s `gd` is a `reduce` loss with
   dyadic values (2x = 4.0 exactly); it is exact under any order.
4. **Whole-trajectory losses**: different rounding *by construction*. Today
   `dL/dtheta = sum_k g_k * sum_{m<=k} d_m` via N(N-1)/2 multiply-adds; the new
   form computes `c_t` by N-1 additions and then N-1 multiply-adds
   `c_t * d_t`. Mathematically equal, not bitwise. No existing corpus pin has
   a non-dyadic trajectory-loss carry (verified by the sweep in 1.5).
5. **Interpreter and diff gates**: unaffected in kind. They compare two
   evaluators of the *same* synthesized program; both see the new program.

The 1e-9 `EXPECT` tolerance means none of classes 3-4 can flip an existing
value pin unless a pin were bit-sensitive; none are.

An alternative that preserves class 3 bit-for-bit -- run the pure carry as a
descending loop, then apply increment adjoints in a second *ascending* loop --
exists for 2.1 only (the carry is primal-independent there). It is not
recommended: it does not extend to 2.2/2.3, costs one more pass, and protects
an ordering no pin depends on. The design below states the order it chooses.

---

## 3. Design

### 3.1 Principle: the direct loop *is* the visible recurrence

The memo asks to keep the recurrence visible until the reverse rule is
selected. The recurrence dies in exactly one place -- `expandRecArray`'s grad
branch (`GradExpand.fs:1181-1200`), running inside `preNormalizeBody` before
`convertBody`, inlining, discipline checks, or either sweep. The forward
branch (`:1145-1179`) already emits a form in which the recurrence *is*
visible: a single `NFor` whose body is `s(n) = s(n-1) + INC`. That form is
also what the primal pipeline emits (`inferRecArray`, `TypeCheckInfer.fs:
10598-10600`), so keeping it makes `f__grad`'s primal half identical in shape
to `f`.

Two ways to make the recurrence recognizable downstream were weighed:

- **A new `NStmt` case** (`NCarry` / `NRec`) carrying buffer, step variable,
  extent, seed and increment. Principled, but every `NStmt` walker needs an
  arm: `convertStmts` (`GradCommon.fs:622`), `renameNStmts`/`boundNames`
  (`GradExpand.fs:983-995`), the inliner (`GradNormalize.fs:619`), `analyze`
  (`:821`), `assignedNames` (`:840`), `checkWriteAfterRead` (`:878, :902,
  :916`), `checkNoScalarOverwrite` (`:934`), `checkLoopDiscipline` (`:980`),
  `collectSortPlans` (`GradSweeps.fs:130`), both sweeps (`:489`, `:1084`),
  `toStmts` (`:1101`) and `validateAll` (`:1184`) -- fourteen sites, most of
  which would delegate to a `carryAsFor` view anyway. And the surface
  `ExprRecArray` would have to survive `preNormalizeBody`'s other surface
  walkers until `convertStmts`. Kept as the fallback if the structural
  recognizer proves fragile (Appendix B lists the touch points).
- **A structural recognizer over the existing `NFor` shape.** `NFor` only
  ever arises from the pre-pass's own synthesis -- users cannot write loops
  (BL1003 / BL1999) -- so the shape `for n in 1..N { s(n) = s(n-1) + INC }`
  with `INC` not mentioning `s` is unambiguous, survives `renameNStmts`
  (names are renamed consistently), and survives `hoistCalls` (hoisted call
  lets land as loop-body `NLet`s *before* the assignment; the recognizer
  allows a prefix of lets that do not mention `s`). This is the recommended
  route. It adds no case, and it mirrors how `additiveSelf`
  (`GradNormalize.fs:845-860`) already selects an adjoint rule by shape.

### 3.2 The changes, in pipeline order

**(a) `GradExpand.expandRecArray` -- one lowering for both modes.**
Replace the two mode-split branches (`:1145-1200`) with: for a slice that
mentions `prefix`, require the seed arm (both modes, existing messages
`:1167`/`:1214`), require `onlyPrevReads` (both modes, message `:1170`), and in
grad mode *additionally* require `additiveRest` to match (keeping refusal
`:1214-1215` verbatim for nonlinear slices -- milestone A does not widen the
accepted set). Then emit exactly what the forward branch emits today
(`:1172-1179`): `bufLet`, `seedWrite`, and
`StmtForIn (stepVar, 1..N, [s(stepVar) = SLICE[prefix := s]])`. Delete the
triangular emitter; the `__rk`/`__rm` fresh prefixes become unused (they are
listed in `plan-forward-mode-ad.md:104`'s prefix inventory -- update that
line). The forward branch's *statements* must stay byte-identical so no
`ad-jvp` pin moves; the natural refactor is to hoist the direct-loop
construction into a local and call it from both mode paths. Update the
function's doc comment (`:1080-1090`) and 008's header comment (lines 8-13),
which documents the route being removed; 008's pins are exact and stay.

**(b) `GradNormalize` -- the recognizer.** Beside `additiveSelf` (`:845`),
add an active pattern

```fsharp
/// `for t in 1..N { lets*; s(t) = s(t - 1) + INC }` -- the additive carry the
/// rec-array pre-pass emits. INC and the lets must not mention `s`; the lets
/// are hoisted-call plumbing and are replayed like any loop-local let.
let internal (|CarryLoop|_|) (s: NStmt)
    : (string * string * Expr * Expr * NStmt list * float * Expr) option
    // (buf, stepVar, lo, hi, leadingLets, sign, inc)
```

Match: `NFor (t, lo, hi, body)`; `body = lets @ [NAssign (lhs, rhs)]` with
every `lets` element an `NLet`; `lhs = ExprApp (ExprVar buf, [ExprVar t])`;
`rhs` is `ExprBinOp (_, OpAdd, a, b)` with exactly one side the read
`ExprApp (ExprVar buf, [ExprBinOp (_, OpSub, ExprVar t, ExprLit (LitInt 1))])`
(accept `ExprTyped` wrappers as `additiveSelf` does) and the other side plus
all `lets` values free of `buf` -- test with `mentionsDeep` (exhaustive walker,
`GradExpand.fs:1064-1075` explains why a fragment walker is unsafe here). A
`-` on the right (`s(t-1) - INC`) can be admitted with `sign = -1.0`, mirroring
`additiveSelf`; the pre-pass never emits it today, so this is optional. Lower
bound `lo` need not be checked (the pre-pass emits 1; a seed-less shape cannot
arrive because (a) refuses it), but pin `lo` as a literal 1 anyway so nothing
else can masquerade.

**(c) `checkLoopDiscipline` (`:944-987`) -- admit the shape.** In the `NFor`
arm (`:980-986`), when `CarryLoop` matches, run the body check with `buf`
removed from `accums` for that loop. That neutralizes both refusals the
direct loop would hit: the "array recurrence on 'a'" element-write refusal
(`:972-976`) and, for hoisted lets that read `s(t-1)` in a later milestone,
the "loop-body let reads accumulator" refusal (`:948-955`). Everything else
in the check is untouched; a self-reading construction loop that is *not* the
carry shape keeps today's refusal message.

**(d) `adjointOfStmt`'s `NFor` arm (`GradSweeps.fs:489-523`) -- reverse the
loop for the carry shape.** Before the generic path:

```fsharp
| CarryLoop (buf, t, lo, hi, _, _, _) & NFor (_, _, _, body) ->
    // Backward carry: the body's generic adjoints are already right
    // (general-overwrite saves/zeros __g_buf(t), the read rule scatters
    // into __g_buf(t-1), INC's adjoint follows); only the ORDER is wrong.
    let j = fresh rc.Ctx "__rj"
    let tLet = NLet (t, false, sub (sub hi (iLit 1L)) (v j))   // t = hi-1-j, descending
    (replay / localCots / bodyAdjoints exactly as the generic arm computes them)
    |> Result.map (fun bodyAdjoints ->
        [NFor (j, iLit 0L, sub hi lo, tLet :: replay @ localCots @ bodyAdjoints)])
```

The body's references to the step variable resolve to the loop-body `let t`
(`inferForIn` binds body lets in scope, `TypeCheckInfer.fs:11515-11556`);
`j` is fresh so nothing shadows. Correctness argument for the replay: the
generic arm replays the whole body to reconstruct loop-local lets. Replaying
`s(t) = s(t-1) + INC` at adjoint time rewrites `s(t)` with the identical
value -- `s(t-1)` is final (written once, at forward step t-1, and in the
descending sweep step t-1 has not been replayed yet), `INC` reads only
non-mutated inputs -- so the replay is an exact no-op. It costs one extra
O(N) primal pass; eliding the assignment from the replay is a safe
follow-up, not a correctness need. The general-overwrite zeroing
`__g_s(t) = 0.0` after the save is harmless: nothing reads `__g_s(t)` after
step t in a descending sweep.

**(e) Nothing else changes.** `analyze` already taints `s` through the
second pass and registers it as an array from the zero-literal `bufLet`
(`:790-814`); `synthesize` already builds `__g_s` from that literal
(`Grad.fs:148-160`) and its dims (`:130-141`); `toStmts`, typecheck, lowering,
codegen and the interpreter already handle `StmtForIn` with a body `let`, an
`ExprAssign`, and `a..b` ranges. The seed write `s(0) = seed` outside the loop
takes the general-overwrite branch today and keeps doing so, now receiving
`c_0` after the carry has accumulated into `__g_s(0)`. A `reduce(s, (+))`
loss is hoisted into a fold loop whose ascending adjoint `__g_s(k) +=
__g___red` runs *before* the carry loop's adjoint in reverse statement order,
so `g_t` is deposited before `c_t` is formed -- the order 2.1 requires.

### 3.3 Target emission for 008

```cpp
// h__grad (proposed). Primal half: one loop, same shape as h.
__v16[0L] = 0.0;
for (int64_t __k17 = 1L; __k17 < 3L; __k17++) {
    __v16[__k17] = (__v16[(__k17 - 1L)] + (w[(__k17 - 1L)] * xs[(__k17 - 1L)]));
}
auto __v19 = __v16[2L];
__v21[2L] = (__v21[2L] + 1.0);                       // g_{N-1}
// Adjoint half: ONE backward loop over the pre-allocated cotangent buffer __v21.
for (int64_t __rj = 0L; __rj < 2L; __rj++) {
    int64_t n = (3L - 1L) - __rj;                    // descending step
    __v16[n] = (__v16[(n - 1L)] + (w[(n - 1L)] * xs[(n - 1L)]));   // replay (exact no-op; elidable)
    double __c = __v21[n]; __v21[n] = 0.0;           // save + zero (general-overwrite rule)
    __v21[(n - 1L)] = (__v21[(n - 1L)] + __c);       // the carry
    __g_w[(n - 1L)] = (__g_w[(n - 1L)] + (__c * xs[(n - 1L)]));     // increment adjoint
}
double __c0 = __v21[0L]; __v21[0L] = 0.0;            // seed adjoint (literal 0.0: nothing flows)
```

Two loops in `h__grad` (was five, two of them triangular); four with a
`reduce` loss (forward, fold, fold adjoint, backward carry). No `< (__k + 1L)`
bound anywhere.

### 3.4 Interpreter twin

None required, and that is a property to *preserve*: the design emits only
`StmtForIn` over `a..b`, loop-body `let`, and `ExprAssign`, all of which
`Interp/Core.fs:609` and its block evaluator already run. `blade test --interp`
(`InterpDiff.fs:179`) then diffs interpreter against compiled output on the
new programs for free. If a future milestone needed a descending `IRForRange`
or a new node, both lanes would have to change together -- avoid it; the
`t = hi-1-j` spelling is the reason nothing new is needed.

### 3.5 Refusals that must remain, and what falls back

- `while`-guarded recurrences: unchanged refusal (`:1099`, ad/023). The
  memo's boundary applies: a satisfied guard does not license an implicit
  derivative.
- Rank >= 2 slices, non-Float elements, non-literal extents: unchanged.
- Missing seed arm with a prefix-reading slice: unchanged message.
- Lags >= 2: unchanged refusal (`additiveRest` fails because REST mentions
  `prefix`; the jvp arm's `onlyPrevReads` message names the reason). Do not
  admit them until the forward loop can express zero-history reads (2.2).
- Nonlinear carries (`prefix(n-1) * w`): unchanged refusal in milestone A.
  Milestone B (section 5) lifts it by dropping the `additiveRest` requirement
  in grad mode and widening the recognizer's rhs to "reads `buf` only as
  `buf(t - 1)`" -- the descending arm is already correct for it (2.3).
- `if` inside the slice: unchanged (`walkExpr`, `:529-539`).
- Any `NFor` that is not the carry shape: the generic same-direction arm,
  byte-identical output to today. In particular prefix-free construction
  loops (every ML/sgs corpus program) are untouched.
- A construction loop whose rhs reads its own buffer at any index other than
  exactly `t - 1`: still refused by `checkLoopDiscipline` (`:972-976`).

---

## 4. First gate

### 4.1 Corpus programs (`tests/corpus/ad/`, one per file)

All use inline `Idx<n>` annotations (1.6: named tags trip strict BL4003
pins), build parameter arrays with `method_for(range<Idx<n>>) <@> lambda(i)
-> ... |> compute` (verified spelling in `t_509b`), build the constant table
from an anonymous range (`let xs = 1.0 + 0.001 * Float64(0..n)`, verified),
and pin scalar summaries rather than 127-element arrays. Extents 127, 257,
509 -- non-powers of two per the benchmark discipline.

1. `025_recarray_carry_final_n127.blade` -- final-only loss `s(126)` over an
   additive carry with `w(n-1) * xs(n-1)`. Pins: `hv` (closed form
   `sum_{m<126} w * xs(m)`; state the decimal and rely on the 1e-9 tolerance),
   `gap = reduce(dh, (+)) - (reduce(xs, (+)) - xs(126))` pinned `= 0`,
   `dh(0) = 1`, `dh(125) = xs(125)`, `dh(126) = 0`. This is the class-2
   bitwise-unchanged case, so its pins are also a regression floor for the
   refactor.
2. `026_recarray_carry_trajectory_n257.blade` -- `reduce(s, (+))` loss.
   Analytic `dh(m) = (256 - m) * xs(m)` for m <= 255, `dh(256) = 0`. Pin
   `dh(0)`, `dh(100)`, `dh(255)`, `dh(256)`, `hv`, and `gap = reduce(dh,(+))
   - <closed form>` `= 0` at 1e-9. This is the class-4 case; its pins are
   analytic, never read off a run.
3. `027_recarray_carry_seed_n509.blade` -- parameter-dependent seed
   `zero :: w(0) * 1.5`, final-only. Analytic: `dh(0) = xs(0) + 1.5`,
   `dh(m) = xs(m)` for 1 <= m <= 507, `dh(508) = 0`.
4. `028_recarray_carry_scalar_param_fd.blade` -- scalar parameters with a
   transcendental increment (a 41-step drag-like integrand, mirroring
   `examples/04:76-85`), final-only, with in-file central differences pinned
   as thresholded booleans (`abs(ad - fd) < 1e-6` printed as `true`), the
   pattern `ad-jvp/021` uses. Covers class 3 (descending summation).
5. `029_recarray_carry_hvp.blade` -- `ad.jvp(ad.grad(h))` through a carry with
   a quadratic increment `w(n-1) * w(n-1) * xs(n-1)`; Hessian is diagonal
   `2 * xs(m)` for m <= N-2. The composition route re-runs the pre-pass over
   `h__grad`'s body (`expandModule`, `Grad.fs:469`), which now contains the descending
   loop and general overwrites; jvp admits both (F2), but this must be
   *shown*, not assumed.
6. Leave 008, 011-013, 023 untouched (pins exact / refusal unchanged); update
   008's header comment only.
7. `ad-jvp/004`'s `tresid = 0` residual (jvp vs grad) is the cross-mode gate
   and must stay green with no pin edit.

### 4.2 Emission pins (harness block, not corpus)

Corpus pins cannot see generated code (the only codegen-side pin is
`WARN-CODEGEN`, `tests/corpus/README.md:47`). Add an F# block modeled on
`runLinAlgEmissionTests` (`tests/LinAlgTests.fs:615-655`, helper `cppOf`
`:80-91` which runs `lower` + `CodeGen.genSelfContainedProgramFromIR`), registered
the way `linalg` is (`src/CliSelfTests.fs:2302-2309`) under a new key
`ad-emission`, and folded into whatever the full suite runs for `linalg`. For
each of 008, 025 and 026 sources:

- slice the `h__grad = [&](` lambda text up to its closing `};`;
- assert `Regex.Matches(slice, @"for \(int64_t").Count` equals 2 (final-only)
  or 4 (`reduce` loss);
- assert no match for `< \(__k\d+ \+ 1L\)` (the triangular bound);
- assert the primal lambda `h = [&](` still has exactly one `for (`.

Also keep one emission case for a prefix-free construction program (013's
`layer`/`gate`) asserting its `for` count is unchanged from a baseline taken
before the change, so the generic arm is proven untouched.

### 4.3 Scaling check (manual, recorded in the PR, not a harness pin)

Loop *count* is the complexity evidence at 127/257/509. Optionally time 008's
shape at N = 4093 and 8191 (`blade run`, interleaved A/B against the
pre-change binary, medians of >= 5) -- today's ~2N^2 flops is ~130M at 8191,
tens of milliseconds, so a difference is measurable; but the gate does not
depend on it.

### 4.4 Stop conditions

- Any existing pin in `ad`, `ad-jvp`, `ad-jvp-comb`, `ml-e2e`, `ml-equiv`,
  `sgs` moves: stop; the construction path and every non-carry loop must be
  byte-identical (compare emissions before/after for 011-013 and one ML
  program).
- 029 (HVP through the carry) fails to typecheck or disagrees with the
  analytic Hessian: do not ship a silent wrong answer -- either fix the
  composition route or make `synthesizeJvp` refuse `CarryLoop`-shaped bodies
  with a named message, and record it.
- 028's FD disagreement exceeds 1e-6 relative on any parameter: stop and
  inspect the emitted adjoint; the descending order changes bits only, never
  1e-6.
- The recognizer fires on any loop the pre-pass did not emit as a carry
  (grep the corpus emissions for the descending `let` in functions without a
  `let rec`): tighten the shape or fall back to the explicit node
  (Appendix B).

---

## 5. Size, risk, files in order

**Milestone A (this design): 1-2 engineer-days including tests.** Roughly
-30/+60 lines of compiler source plus five corpus files and one harness block.

1. `src/GradNormalize.fs` -- `(|CarryLoop|_|)` beside `additiveSelf` (`:845`);
   `checkLoopDiscipline` `NFor` arm exemption (`:980-986`). ~40 lines.
2. `src/GradExpand.fs` -- `expandRecArray`: unify the mode branches on the
   direct loop, delete `:1181-1200`, keep every refusal string; doc comment
   `:1080-1090`. ~-30/+15.
3. `src/GradSweeps.fs` -- `adjointOfStmt` `NFor` arm: `CarryLoop` branch
   emitting the descending loop (`:489`). ~25 lines. (`GradSweeps` already
   opens `GradNormalize`, `:11`.)
4. `tests/corpus/ad/025-029` (new); `tests/corpus/ad/008` header comment.
5. Harness: an `ad-emission` block (new `tests/AdEmissionTests.fs` needs a
   `<Compile>` entry in `Blade.fsproj`'s tests group, in dependency order
   after `LinAlgTests.fs`'s neighbours -- read the fsproj comments first) and
   its `CliSelfTests.fs` key.
6. Docs: `plan-forward-mode-ad.md:104` prefix inventory (`__rk __rm` gone);
   `plan-fortran-killer.md` Arc 6 note that its first step landed;
   `docs/plans/README.md` index row for this file (the memo notes two plans
   were already missing from the index); `examples/04:70-75` comment;
   `docs/features.md` if it describes the triangular route (not checked).

**Milestone B (Arc 6, first step; +1 day, mostly tests).** Drop the
`additiveRest` requirement in grad mode and widen the recognizer's rhs to
"mentions `buf` only as `buf(t-1)`". The descending arm is already the
correct adjoint (2.3): the general-overwrite branch evaluates `dg/ds` at the
final `s(t-1)`, which is the primal. Tests: `ad-jvp/016`'s shape
(`prefix(n-1) * 0.5 + x`) with a jvp-vs-grad residual pin; a logistic map;
a damped nonlinear step with FD booleans. Hoisted-call lets reading `s(t-1)`
(a `function step(prev: Float, ...)` in the slice) need the (c) exemption to
also cover the let-reads-accumulator refusal -- (c) as written does.

**Milestone C (bounded lags): blocked**, on `if` in the reverse fragment or a
peeled startup segment; separate design.

Risks, honestly ranked:

- *Recognizer false negative under inlining*: `hoistCalls` may leave the
  increment as a let-bound temp (`s(t) = s(t-1) + __h3`), which the shape
  above still matches (REST is a variable) -- but verify with 013-style
  callee chains inside a carry.
- *HVP composition* (4.1 item 5): the most likely surprise; contained by the
  stop condition.
- *Replay cost*: doubles `f__grad`'s primal work; elide the carry assignment
  from `replay` once 4.1 is green (the lets must still replay).
- *Bit movement in classes 3-4* (2.4): documented, tolerance-covered, no pin
  depends on it.
- *Freeze-idiom interaction*: none. `recognizeFreezeIdiom` runs in
  `inferRecArray` on the primal's typed path (`TypeCheckInfer.fs:10276-10300`);
  the AD pre-pass sees the raw `RecArrayDef`, and an `if` in the slice is
  refused in reverse mode anyway.
- *Units on cotangents*: unchanged refusal (`GradNormalize.fs:708-716`,
  `Grad.fs:108-112`); orthogonal, see fortran-killer-2 section 6.1.

---

## 6. Recommendation

**GO.** The defect is verified in generated code, the fix is a re-selection of
an existing adjoint rule (loop direction) rather than new AD theory, the
accepted-program set is unchanged in milestone A, the interpreter needs no
work, and the corpus/harness gates are concrete. It is performance catch-up
(Futhark's scan rules are prior art, per the memo), and it is the prerequisite
for the nonlinear first-order case that Arc 6 wants -- which milestone B gets
for the cost of a wider recognizer, because the trajectory buffer is already
the tape.

---

## Appendix A: probe programs and observed results

All from a private `%TEMP%` directory with `/c/msys64/ucrt64/bin` prepended;
sources are the 008 shape with the stated variation. Files: `t008` (008
verbatim, `emit` OK), `t_traj` (`reduce(s,(+))` loss; `run` -> `hv = 18`,
`dh = [8, 5, 0]`), `t_seed` (`zero :: w(0) * 1.5`; `emit` OK, seed carried per
k), `t_noseed` (no seed arm; BL5500 "only additive prefix recurrences ..."),
`t_lag2` (`w(n-1) * prefix(n-2)`; same BL5500), `t_nonlin` (`prefix(n-1) *
w(n-1)`; same BL5500), `t_rank2` (`Array<Float like Idx<3>, X>`; BL5500
"only rank-1"), `t_jvp` (`ad.jvp(h)`; one lockstep loop), `t_509` (named
`type N = Idx<509>`; `check` OK with 5 x BL4003), `t_509b` (inline
`Idx<509>`; 0 warnings; `run` -> `gap = 0`, `d0 = 1`, `d507 = 1.507 = x507`,
`d508 = 0`, `hv = 1273.556`).

## Appendix B: the explicit-node alternative (fallback)

If the structural recognizer proves fragile, add
`NCarry of buf * step * extent * seed: Expr * inc: Expr` to `NStmt`
(`GradCommon.fs:171-173`) with a `carryAsFor : NCarry -> NStmt` view, and
give each of these sites an arm (delegating through the view unless noted):
`convertStmts` `GradCommon.fs:622` (construct from a surviving surface
`StmtLet { Value = ExprRecArray }`); `renameNStmts` `GradExpand.fs:983`;
`boundNames` `:995` (returns `[buf; step]`); inliner `GradNormalize.fs:619`;
`analyze` `:821` (also add `buf` to `arrays`); `assignedNames` `:840`;
`checkWriteAfterRead` `:878/:902/:916`; `checkNoScalarOverwrite` `:934`;
`checkLoopDiscipline` `:980` (`Ok ()`); `collectSortPlans`
`GradSweeps.fs:130`; `adjointOfStmt` `:489` (the rule of 3.2(d));
`tangentOfStmt` `:1084` (via the view -- reproduces today's jvp output);
`toStmts` `:1101` (via the view); `validateAll` `:1184`; plus `synthesize`'s
`dimsEnv`/`localDecls` (`Grad.fs:130-160`) which key off `NLet` today. The
surface `ExprRecArray` must also survive `preNormalizeBody`'s other walkers
(`dropDeadLoopBindings`, the sort/eager-map arms, `GradFusion.fs:281`) until
conversion. Fourteen-plus sites versus three; prefer the recognizer.
