# subset / split — the missing inverse of join

Status: PLANNED (Fable planning pass 2026-08-26) — nothing built. P0 is
sized; the payoff is collapsing every hand-enumerated segment-extraction
literal in the notebooks (the 499-slot `sgd_step` unpack, the eight
`ad.grad`'d weight slicers per notebook).

`docs/formalism.md:157` specifies `subset(A, d, (s,e))` / `split(A, d, i)`
beside `join` ("concatenate / range-extract / split = two subsets;
split-join round-trips"); both are BL2001-unbound today (probe-verified).
Meanwhile `join` itself is far more capable than its corpus coverage —
heterogeneous extents, leading/trailing-axis rank-2, nested 2-D block
assembly, provider-read operands all run (census row 50;
`tests/corpus/stack-join/014_join_heterogeneous_extents.blade` pins the
first four; the provider-read shape lives in F#-harness territory and is
deferred). The asymmetry — assembly works, disassembly doesn't — is the
whole motivation.

## Ground truth: the join template, end to end

| Phase | Anchor | Notes |
|---|---|---|
| Lexer | `src/Lexer.fs:113,231` | keyword table |
| Parser | `src/ParserGrammar.fs:551-565` | dim = trailing int literal, ≥2 arrays |
| AST/TAST | `src/Ast.fs:461`, `src/TypedAst.fs:210` | `ExprJoin`/`TExprJoin` |
| TypeCheck | `src/TypeCheckInfer.fs:3577-3686` | `stackJoinOperandTypes`; `isDenseStackableSlot` fence `:3555`; result axis minted `:3679` as `{Id=fresh; Extent=sum; Tag=None}` — Tag=None severs named-axis identity |
| Walkers | `Zonk.fs:241`, `TypeCheckSupport.fs:2418`, `DeduceRep.fs:662` | one-liners |
| Lowering/IR | `Lowering.fs:669` → `IRJoin` (`IR.fs:189`); **`IRSubset` EXISTS DORMANT** at `IR.fs:190` (`array*dim*start*length`), children `:1874`, IRLift rebuild `IRLift.fs:621`, deliberately-untyped list `:2900`, excluded from `exprTypeIfKnown` per `:2984` |
| CodeGen | dispatch `CodeGenBinding.fs:61` → `genStackJoinBinding :2213`; `materializeJoinForm` `CodeGenExpr.fs:1745-1801` — COPY, offset-write nest; expression position refused `:379` |
| Interp | `Core.fs:886` → `Loops.fs:2085` → `ArrayOps.fs:928` (byte-twin) |
| AD fwd | `GradCommon.fs:652-660` `(|LinearForm|_|)` |
| AD rev | `GradSweeps.fs:329` `ExprJoin(parts,0)` scatter via `accumLoop` (`GradExpand.fs:1424`); dispatch `:397`; pre-flight fence `Grad.fs:178,201` (an unlisted combinator dies HERE with the alias message) |
| Diags | BL4004 bucket (`TypeEnv.fs:901-902`) |
| Tests | `Test_StackJoin.fs:9` auto-globs the corpus dir — zero harness edits |

Also verified: `a..b` is half-open and parses in argument position
(`anon-ranges/001`, `ParserGrammar.fs:310-315`); `subset`/`split` are safe
as keywords (no corpus/example/stdlib identifier collisions); `eigh` is the
only tuple-returning combinator and needed bespoke emitters — the cautionary
precedent for split.

## P0 — `subset(A, d, lo..hi)` in ordinary code

Semantics LOCKED: range spelling only (refuse the `(s,e)` tuple; edit
formalism:157 in the same change), HALF-OPEN like every other `..`; `d` a
trailing int literal (join parity); `lo`/`hi` full exprs folded by
StaticEval (so `let static OFF` names work — non-negotiable for the aspirin
case), non-static → `SubsetBoundsNotStatic` in the BL4004 bucket (no new BL
code); result axis `{Id=fresh; Extent=IRLit(hi-lo); Tag=None}` — join's
rule byte-for-byte, so no BL4003 interaction by construction; eager COPY
(`materializeSubsetForm` = the offset-READ inverse of join's offset-write;
result extent is a literal so it takes the static companion-extents arm);
refusals = single-operand `isDenseStackableSlot` (all packed/ragged/sparse/
compound/orb/irreps slots refuse "decompact first"; irreps fence inherited
for census-#51 consistency, revisit on demand) + dim range + `0 ≤ lo < hi ≤
extent` when static.

Touch points: ~20 existing files, each a small mirrored arm beside its join
twin (list with line anchors in the planning transcript); revive `IRSubset`
(remove from the `:2900` untyped list, amend `:2984`); NO new files, NO
fsproj churn; interp twin lands in the SAME change (differential-twin
obligation). `exprTypeIfKnown`: stay OFF the whitelist (join parity).

Tests (stack-join/, auto-globbed): 015 rank-1 values, 016 trailing axis,
017 fnbody-returned (return-extent ABI, mirror 011), 018 static-name
bounds, 019 subset∘join round-trip law (difference reduces to 0), rejects
020-023 (dim range / non-static bound / empty-inverted / compact slot, each
`// ERROR: BL4004`).

## P1 — AD admissibility (the notebook payoff)

Forward: one `(|LinearForm|_|)` line + mechanical walker one-liners
(`producesArray`, taint walkers, rebuilds incl. GradFusion/GradPackUnroll/
Unfold + 8 domain elaborators + `MLCertShell.fs:122`). Reverse: `ExprSubset`
arm beside `GradSweeps.fs:329` — scatter-at-offset `dA(i+lo,…) += cot(i,…)`
via the existing `accumInto` lane (join's arm with the offset moved from the
cotangent read to the primal write); dispatch `:397`; `staticDimsOf`/
`staticExtentOf` arms; **and BOTH `Grad.fs:178,201` allowlists in the same
commit** or subset dies pre-flight with the alias message (the
ml-cert-walker-drift failure mode — add the reject pin). Arbitrary `d`
preferred; d=0 fallback if probes disagree. Tests: ad-jvp-comb jvp/grad/
in-callee/round-trip-grad + one named reject. Acceptance probe: rewrite one
aspirin slicer (`we_of`) as `subset(__s, 0, OFF_E..OFF_E+48)` under
`ad.grad` and diff the trained loss.

## P2 — `split(A, d, i)`

LOCKED: a typecheck-level DESUGAR, no IR node — `let (l, r) = split(A,d,i)`
elaborates to two sibling `TExprSubset` bindings (`0..i`, `i..n`);
expression-position split refused ("split = two subsets; bind it as
`let (l, r) = ...`"). Rationale: eigh, the only tuple-returning combinator,
needed bespoke emitters and dodged the flat-vs-nested tuple hole; the
desugar gets AD/interp/codegen free from P0/P1 and matches the formalism's
own definition. Tests: split leading, split∘join round-trip, expr-position
reject, 2-D block DISassembly (the mirror of 014's assembly).

## Risks, ranked

1. Grad bound folding: does Unfold substitute `let static` names before
   grad reads the body? Probe before P1.
2. Fence/adjoint drift (`Grad.fs` allowlists vs `adjointOfInit` arms) —
   land atomically + reject pin.
3. `expandEagerMap` interaction: `ExprSubset` falls through safely, but pin
   one `subset(...) |> compute` case.
4. Blessed-position folds: `reduce(subset(...), (+))` inline likely needs a
   let (join parity) — pin whichever behavior lands.
5. exprTypeIfKnown BL6001 seam: parity with join for v1; whitelisting both
   is a separate testable change.
