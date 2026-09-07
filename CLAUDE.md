# CLAUDE.md

Guidance for AI coding agents working in the Blade repository.

## What Blade is

Blade is an array-functional programming language: ML-style syntax over a numpy/R-flavored
math surface. The compiler is F# (`src/*.fs`, one exe project `Blade.fsproj`) emitting C++20,
compiled with g++ and run. A tree-walking interpreter (`src/Interp/`) mirrors codegen for
differential testing and the REPL.

Three concepts carry the language ("structure-first" programming, `docs/quickstart-1.md`):

- **Loop reification** — for-loops are first-class values (`method_for`, `object_for`) you
  store, compose, join, and apply later.
- **Dimensional currying** — an `Array<T like I, J>` *is* the function `I → J → T`;
  `A(i, j) ≡ A(i)(j)`, and partial indexing yields a lower-rank view.
- **Arity polymorphism** — one function typechecks and compiles differently depending on how
  many array arguments it receives, deducing output rank and symmetry.

The design invariant is **"the fastest way is the only way"**: iteration order, storage
layout, parallelism, and BLAS routing are derived from types and kernel annotations, never
hand-authored. Declaring structure (`where comm(...)`, `where omp(...)`, index types) *is*
the optimization interface.

## Repo layout

```
Blade.fsproj / Blade.sln   root exe project: compiler + test harness in one binary.
                           EnableDefaultItems=false — new .fs files MUST be added to
                           Blade.fsproj by hand, in dependency order.
src/*.fs                   compiler pipeline. Runtime phases: Lexer/Parser → TypeCheck/
                           Unify/Deduce → Lowering → IR passes → EmitCpp/CodeGen. The big
                           phases are FILE GROUPS in dependency order, each with one atomic
                           rec-chain file that must not be split further: Parser* (grammar
                           in ParserGrammar.fs), Grad* (AD pipeline), IR* (hot surface stays
                           in IR.fs), TypeCheck* (the ~13k inferExpr chain in
                           TypeCheckInfer.fs; Blade.TypeCheck = the driver), CodeGen*
                           (chains in CodeGenExpr.fs/CodeGenBinding.fs; Blade.CodeGen =
                           assembly + re-exported surface). NOTE: fsproj compile order is
                           NOT phase order — read the fsproj (and its group comments)
                           before choosing where to insert a <Compile> entry.
src/cpp/                   C++ runtime headers, deployed beside every generated .cpp.
src/Interp/                IR interpreter (used by `test interp`, `repl`, diff gates).
src/{ml,ppl,math,rand,display,spectra,sgs}/compiler/
                           domain elaborators compiled INTO Blade.exe.
oracles/{math,ml,ppl,sgs,spectra}/
                           5 SEPARATE oracle .fsproj (BladeMath, BladeML, MomentAlgebra,
                           BladeSgs, BladeSpectra) — standalone reference programs whose
                           dump verbs generate corpus EXPECT pins; not part of Blade.exe.
                           Run: dotnet run --project oracles/<domain>.
tests/                     harness compiled into Blade.fsproj (no xunit). Corpus tests live
                           in tests/corpus/<category>/*.blade, one test per file.
stdlib/                    Blade-source stdlib (units/SI.blade, plot.blade), deployed beside
                           the binary; BLADE_STDLIB adds a search root. Read at RUN time, so
                           edits need no rebuild: in a CHECKOUT the resolver prefers this
                           directory over the deployed copy (ModuleResolve's Blade.fsproj
                           gate) — it used to prefer the copy, which made edits look inert.
                           An edit can still outrun the BINARY by using a compiler feature it
                           predates; `blade doctor`'s stdlib row names the root that answered
                           and flags a diverging copy.
examples/                  9 numbered worked programs plus lsdft.blade / lswosa.blade /
                           lseof.bladenb — the best source of idiomatic Blade; physics/ is
                           a second, self-contained 47-program corpus with its own README.
docs/                      formalism.md (canonical semantics), features.md (feature census),
                           quickstart-1/2.md (tutorial), proofs.md (Coq correspondence),
                           plans/*.md (living design docs; plans/README.md indexes status).
proofs/                    the Coq/Rocq formalization itself.
generated_cpp_tests/       gitignored scratch dir the harness writes .cpp/.exe into.
                           It is CWD-RELATIVE: never run two `blade test` processes from the
                           same working directory.
legacy/                    pre-rename Blade-DSL artifacts. Do not imitate.
```

## Build and CLI

```bash
dotnet build Blade.fsproj -c Release
```

The binary is `bin/Release/net10.0/Blade.exe` (below, `blade` = that exe or
`dotnet run --project Blade.fsproj -c Release --`):

```bash
blade check prog.blade        # typecheck only
blade emit prog.blade         # emit C++ without compiling
blade compile prog.blade      # produce an executable
blade run prog.blade          # compile and run (--verbose, --mpi N, --memcheck, --run-record out.json)
blade plan prog.blade         # optimization decisions + the input manifest (--json)
blade test                    # full default suite
```

`--strict-pins` (valid on `check`/`emit`/`compile`/`run`) promotes BL4010 pin suggestions to
errors; it also has its own test block, `blade test strict-pins`.

Requirements: .NET 10 SDK (F# 10); MSYS2 **ucrt64** g++ on PATH for anything that compiles C++
(`C:\msys64\ucrt64\bin`). `blade test cuda` additionally needs a vcvars64 environment
(cl.exe + nvcc) with ucrt64 layered on top.

## Testing

- `blade test` runs the default suite. OMP/CUDA/MPI/timing/interp/diff-oracle are
  **excluded by default** — opt in with
  `blade test --omp --cuda --mpi --timing --interp --diff-oracle`. The NetCDF/Zarr/CSV/hybrid
  provider blocks are reachable **only standalone** (`blade test netcdf|zarr|csv|hybrid`) —
  no flag folds them into the full suite.
- Missing toolchain pieces make tests **skip, not fail**, and skips don't affect the exit
  code — always check the `, N skipped` suffix in the totals before trusting a green run.
- Two different category namespaces:
  - `blade test <key>` uses alias keys from `src/CliSelfTests.fs` (dispatchTest's key
    map); multiword categories accept both spellings (`index-types` / `indextypes`).
  - `blade test interp <dir>` / `blade test diff-oracle <dir>` take the **literal**
    `tests/corpus/<dir>` name: `blade test interp index-types`.
- Every corpus category has a standalone `blade test <key>`; `blade test sql` runs the
  same sql-* union the full suite does.
- To iterate on a single corpus test, `blade run tests/corpus/<cat>/<file>.blade` (fast, but
  pins are only validated by the harness, not by `run`).
- Full-suite runs from concurrent sessions must use private working directories (the scratch
  dir is cwd-relative) and should not overlap in time.

### Corpus test conventions

One `.blade` file per test; pins are comments (grammar documented in `tests/corpus/README.md`):

- `// TEST: <name>` — required first line; a `(rejects)` suffix is semantically load-bearing.
- `// EXPECT: <var> = <value>` — pins a printed value.
- `// ERROR: BLxxxx [@ l:c[-l:c]]`, `// ERROR-CONTAINS: <substring>`, `// ABORT: <substring>`,
  `// REJECT-AT: lower|codegen`.
- `// WARN: BLxxxx` and `// WARN-CODEGEN: <substring>` are **strict in both directions**: an
  unpinned warning fails the test AND a pin that never fires fails the test. If you fix a
  warning false-positive, remove its now-dead pins in the same change.
- `// MODULE: <name>` for multi-file tests (pins union across member files).

`.editorconfig` exempts `tests/corpus/**` and `examples/**` from trailing-whitespace and
final-newline fixing — these are byte-pinned assets; never auto-reformat them.

## Environment variables (read per-call, not cached — keep it that way)

| Variable | Effect |
|---|---|
| `OPENBLAS_DIR` | BLAS/LAPACK install root; presence alone enables the BLAS route when `BLADE_BLAS` is unset |
| `BLADE_BLAS` | `1`/`on` force BLAS routing, `0`/`off` force off; unset defers to `OPENBLAS_DIR` (set ⇒ on). Off only when neither is set (byte-identity with the interpreter beats the last ULP) |
| `BLADE_CUBLAS` | `1`/`on` routes L3 contractions to cuBLAS; strictly opt-in |
| `BLADE_MEMCHECK` | non-`0` → ASan-instrumented Debug profile (clang64 preferred, MSVC fallback) |
| `BLADE_OMP_THREADS` | `1`/`0`/`off` **suppresses OMP pragma emission**; runtime thread count is plain `OMP_NUM_THREADS` |
| `BLADE_FP_REASSOC` | `1`/`on` licenses reassociated (lane-parallel) fold codegen |
| `BLADE_AD_HALO_GATHER` | reverse-mode rule for a `halo` stencil map: unset/`1` keeps the map and emits the GATHER adjoint (default); `0`/`off` lowers it into the construction loop and scatters. Same function either way; `blade test access` compares the two byte-for-byte |
| `BLADE_MARCH` / `BLADE_FP_CONTRACT` | g++ `-march=` (default `native`) / `-ffp-contract=` (default `fast`) |
| `BLADE_STDLIB` | extra stdlib search root |
| `BLADE_TILE_CACHE` | revision reuse (structural/04): unset = OFF; `1`/`on` = `%LOCALAPPDATA%\Blade\tile-cache`, an absolute path = that store. Read by the COMPILER (plans tiled bindings) and by the COMPILED PROGRAM (probe/load/store); `BLADE_TILE_CACHE_VERBOSE=1` prints the planner's admissions and the run's `[tiles]`/`[chunks]` census |
| `BLADE_RUN_RECORD` | read by the COMPILED PROGRAM at exit: path of the JSON run record (input manifest + observed size/mtime per input, executable/compiler identity, FP policy and library routes from the `-DBLADE_RR_*` build defines, RNG generator, ok / BLxxxx status). `blade run --run-record path` sets it |
| `NETCDF_DIR` | NetCDF provider include/link root |

## Writing Blade: language essentials

A `.blade` file is a flat sequence of declarations; there is no `main` — bare top-level
expressions evaluate and print. `module Name` / `import X` / `from X import name` handle
multi-file programs.

```blade
let static n = 4                 // compile-time, immutable everywhere
let x = 1                        // reassignable in its OWN scope only
let mut y = 2                    // reassignable; `mut` params are ARRAY-only (element writes alias the caller; scalars: return instead, BL4005)

function add1(array: T^2) -> T^2 = { array + 1 }

function mean(row: T^1) -> T^0 = reduce(row, (+)) / extents(row)
function covariance(a: T^1, b: T^1) where comm(a, b) =
    mean((a - mean(a)) * (b - mean(b)))

Unit meters
Unit seconds
Unit mps = meters / seconds
type Speed = Float<mps>          // unit-carrying scalar type

type LatIdx = Idx<180>           // named index types give structural identity
type LonIdx = Idx<360>
type EarthArray = Array<Float like LatIdx, LonIdx>
```

- Functions see **abstract types** `T^r` (element type + rank + symmetry); arrays have
  concrete types `Array<T like I, J, ...>`. `T<u>` marks a unit-carrying type variable.
- **Index types are the type system's spine**: `Idx<N>`, `EnumIdx<S>`,
  `SymIdx<r, N>` (canonical tuples `i₁ ≤ … ≤ iᵣ`, triangular storage),
  `AntisymIdx<r, N>` (strict `i₁ < … < iᵣ`, sign-tracked, **no stored diagonal**),
  `HermitianIdx<N>`, `CompoundIdx<mask>` (flat `B(lat, lon)` subscripts),
  `SparseIdx<keys>` (hashed; owns wildcard/partial reads `S((lat, _))`), `RaggedIdx`,
  `OrbIdx` (declarable at any depth, but beyond depth 1 only *deduced* classes reach
  storage), `IrrepsIdx<spec>` (equivariant ML).
- `Nat<LatIdx>` and `Nat<LonIdx>` don't unify even at equal extent — index provenance is
  part of the type, which is what makes `A(i)` bounds-safe by construction.
- `()` is application/indexing (curried); `[]` is ONLY tuple/pack structural access
  (`t[0]`, `args[k]`), never array indexing.
- `where` clauses carry kernel metadata: `comm(a, b)` / `anticomm(...)` (interchangeable
  args), `omp(x: n)` (parallelism license, depth-capped), `cuda`, `tdim({...})`. A bare
  `omp` on a `reduce` kernel licenses fold reordering and requires commutativity (else
  BL4016). `reynolds(g)` *manufactures* a commutative kernel by symmetrization when you
  can't annotate one.

## Style guide

**The golden rule: iteration is declarative. Parallel structure is a loop object;
sequential structure is a recursive array. There is nothing else, by design.**

The imperative loop is not merely missing — it was removed and diagnosed:

```blade
function f() -> Int64 = {
    let mut s = 0
    for k in 0..3 { s += k }   // ERROR BL1003: removed from the language
    s
}
```

(BL1003 fires inside a block; at top level the same text dies earlier as a BL1999 parse
error.)

Do not try to reintroduce it through workarounds. Every "loop-shaped" problem has a
first-class construct:

| You want | Write | Not |
|---|---|---|
| elementwise op over same index space | `A + B`, or `method_for(zip(A, B)) <@> f` | index-juggling |
| all-pairs / outer product | `A [+] B`, `A [*] B`, or `method_for(A, B) <@> f` | nested loops |
| full fold to a scalar | rank 1: `reduce(A, (+))`; rank k: `reduce(A, (+), axes = k)` | accumulator mutation |
| peel the innermost axis (partial reduction) | bare `reduce(A, (+))` — the default is `axes = 1`, so on rank ≥ 2 it returns an array, not a scalar | manual row loops |
| several statistics in ONE pass | `reduce((L <@> k1) <&!> (L <@> k2) <&!> (L <@> k3), (+))` then tuple-destructure | separate passes |
| filter / WHERE | `mask(xs, pred)` + `compound(data, mask)`; compose masks with `&&`/`\|\|` | a filter loop |
| stencil / lags / rolling window | `method_for(halo<I, [-1, 0, 1]>) <@> lambda(w) -> A(w(1)) - A(w(-1))` | index arithmetic with edge guards |
| index generation | `0..8` anonymous range (a first-class rank-1 array), or `range<I>` when the named tag should flow | materialized iota; `method_for(range) <@> lambda(i) -> i \|> compute` |
| coordinate axis / linspace | `x0 + dx * Float64(0..n)` — implicit lifting over the range | `method_for(range<I>) <@> lambda(j) -> x0 + dx * Float64(j)` for a body that is just affine in the index |
| sum/product of an index range | `reduce(0..n, (+))` | wrapping the range in a map first |
| numeric width/class conversion | `Float64(n)`, `Float32(x)`, `Int64(floor(x))` — scalar type name in call position; arrays lift elementwise | `* 1.0` fudges; implicit int→float mixes (warn BL3020); bare `Int64(x)` on a float (BL3019 — the rounding must be visible at the cast site) |
| pipeline of stages | compose values: `object_for(f) >>@ object_for(g)`, apply with `<@>`, materialize with `\|> compute` | eager temporaries per stage |
| recurrence / time-stepping / running state | `let rec` recursive array (see below) | `let mut` + a loop |
| iterate to convergence (trip count not known up front) | the inductive arm's `while` guard over a BUDGET extent: `\| prefix :: n while <cond> -> prefix :: <step>` — frozen once the guard goes false, runtime BL8010 if the budget runs out with it still true | a `while` loop; running the full budget unconditionally and hoping |
| symmetric pairwise stats (covariance, comoments) | `where comm(a, b)` kernels, `reynolds(...)`, `gram(R, R)` | hand-written triangular loops |
| several solves against one matrix (right-hand sides, transposed systems) | `let f = m.lu(A)` once, then `m.lu_solve(f, b)` / `m.lu_solve_t(f, b)` per side -- the factorization is a VALUE (a tuple of the L\U factor and the pivots), bitwise `m.solve` | `m.solve(A, b)` per side, refactoring A each time |
| Jacobian actions of an array-valued function, no Jacobian formed (Newton-Krylov, sensitivities) | `ad.jvp(f)(args..., seeds...)` = `(y, J v)`; `ad.vjp(f)(args..., w, buffers...)` accumulates `Jᵀ w` (the body's value a NAMED array); through `m.lu_solve(f, b)` both are one more solve against the same factors | a dense Jacobian by finite differences; differentiating the factorization |
| random draws that survive chunking, reordering, restart (Monte Carlo sample identity) | `import rand as r` then `r.uniform_at(key, stream, offset, n)` (and `normal_at`, `gamma_at`, ...): cell i is logical sample `offset + i`, a pure function of its address, so a chunk drawn alone equals the same slice of the whole | `r.uniform(key, n)` sliced -- sequenced draws shift when the fill's extent or order changes |
| apply a Gram operator to a vector (`(A Bᴴ) x`) | `gram_apply(A, B, x)` -- two rank-1 temporaries, never the N×N matrix; differentiates (the adjoint action is a `gram_apply` too) | `gram(A, B)` then `decompact` then a row `prodsum` -- forms and reads the whole matrix once |
| per-chunk / per-file work over a chunked or multi-file axis | `type CI = Chunked<I, K>` (or `Chunked<s.index.d, store>`, `Chunked<s1.index.d, [[s1, store], [s2, store]]>`), then `let seg = segments(CI)` / `files(T)` and `group_by(a, seg)` for per-run kernels; `ungroup(g)` restores the axis, `ungroup([v1, v2], T)` names a multi-store variable (docs/features/sql.md 7c) | hand-computed offsets; `join` (mints a fresh axis) |
| normalized / weighted pairwise sums (kernel smoothing, softmax rows) | a per-row kernel over `range<I>` whose body NAMES the deferred producer and folds it with a reduction join: `let w = method_for(xs) <@> k; let z, u = object_for(<&!>) <@> (reduce(w, (+)), prodsum(w, v)); u / z` -- or a tuple-state fold for the one-pass online form (`examples/10`) | an M x N weight array then two partial folds |

Pick the highest idiom rung the body admits: implicit lifting over a range
(`x0 + dx * Float64(0..n)`) beats `method_for(range<...>) <@> lambda`, which in
turn beats `let rec` — but only when the body really is arithmetic in the
index. `method_for(range<...>)` stays the right spelling when the kernel is a
block or a gather/conditional, when the loop composes with combinators
(`>>@`, `<&!>`, multi-operand `method_for(range<I>, A, B)`), when the range is
multi-slot (`range<Y, X>`) or non-plain (`SymIdx`/`CompoundIdx`/`halo`), or
when you want the named index tag to flow into the result.

Real code (from `examples/` and `tests/corpus/` — these compile):

```blade
// Filter + aggregate (examples/01_weather_stations.blade)
let qc_ok = mask(r_qc, lambda(q) -> q == 0)
let good_temps = compound(r_temp, qc_ok)
let n_good = extents(good_temps)
let sum_good = reduce(good_temps, (+))

// Pipeline as a value; nothing runs until compute (examples/03_signal_conditioning.blade)
let condition = object_for(calibrate) >>@ object_for(deadband)
let cond0 = condition <@> r0 |> compute

// One traversal, several accumulators (examples/03_signal_conditioning.blade)
let (s0, ss0, n0) = reduce((L0 <@> k_id) <&!> (L0 <@> square) <&!> (L0 <@> k_one), (+))
```

Sequential structure — the replacement for accumulation loops — is `let rec` by structural
induction on the leading axis (`docs/formalism.md` §7.5):

```blade
// tests/corpus/recursive-arrays/002_running_reduce.blade
type Step = Idx<6>
let rec x: Array<Float like Step> =
    match x with
    | zero -> zero
    | zero :: s -> zero :: 1.0
    | prefix :: n -> prefix :: prefix(n - 1) * 0.5 + 1.0
let total = reduce(x, (+))
```

This compiles to one pre-allocated buffer (no recursion frames). Reads past the built prefix
are implicitly zero, so lag schemes need **no** bounds guards — don't write
`if n >= k then prefix(n - k) else 0.0`. The recursion axis is always the leading axis and
its extent must be static. The same shape scales to RK4 time-stepping and DP tables
(`tests/corpus/recursive-arrays/007`, `008`).

### Rules that prevent real bugs

- **`method_for(A, B)` is the outer product; `method_for(zip(A, B))` is co-iteration.**
  Confusing them silently changes the output rank.
- **`reduce` folds right-to-left, innermost axis first.** Irrelevant for `(+)`; decisive the
  moment your fold kernel isn't commutative.
- **Don't hand-optimize.** No manual triangular index math, no hand-placed OpenMP pragmas,
  no manual BLAS calls. Declare `comm`/`anticomm`/`omp` and use array identity at the call
  site; the compiler derives triangular storage/iteration (r! savings), pragma placement,
  and BLAS routing. Symmetric output storage is *only* licensed when the SAME array occupies
  the commuting positions — two different arrays over the same index space get nothing, and
  that refusal is proved correct.
- **Named runtime functions can self-recurse; mutual recursion is rejected (BL2001)** — a
  body sees only names bound before it. Restructure into one function, or make both
  `static function`s (compile-time, may mutually recurse).
- **Refusals are features.** Unit clashes, index-type mismatches, extent disagreements
  (BL3016), unlicensed symmetry claims: the compiler refuses to compile a plausible-but-wrong
  fast path. When you hit one, fix the declaration, don't cast around it.
- Benchmark discipline: never measure at power-of-two extents (known ~7x cache artifact);
  interleave A/B runs and report medians, not single passes.

### Naming and formatting

- Types and index aliases: `PascalCase` (`StationIdx`, `Speed`); rank-polymorphic type vars
  are single capitals (`T`, `X`). Units: lowercase physical names (`meters`, `mps`).
- Values/functions: `snake_case` (`station_means`, `co_gain_dense`); short math scratch
  names (`k1`, `Z2`) are fine locally. Leading `__` marks internal non-API bindings.
- `///` doc comment above a declaration; `//` banner blocks as section headers; trailing
  `// EXPECT:` comments pin values in examples and corpus tests.
- 4-space indent inside `{ }` blocks. `where` goes between the parameter list and the return
  type; both same-line and own-line placements exist in the codebase — be consistent within
  a file.

## Working on the compiler (F#)

- Compile order in `Blade.fsproj` is dependency order and default items are disabled: adding
  a file means adding a `<Compile Include=...>` entry in the right position.
- The git index is uniformly LF except `src/cpp/index_types.{cpp,h}` (and `legacy/` `.edgi`
  files); there is no `.gitattributes`, and `core.autocrlf=true` makes working trees CRLF.
  Editing tools can silently flip a file's *stored* endings — check with
  `git ls-files --eol <path>`, and don't flip a file wholesale in an unrelated change.
- Environment gates (`Build.fs`, `LinAlgPatterns.fs`, `CodeGen.fs`) are deliberately
  functions, not module-level `let`s, so tests can pin/restore them mid-process. Don't
  "optimize" them into cached values.
- The interpreter (`src/Interp/`) and codegen are differential twins: behavior changes must
  land in both or `test interp` / `diff-oracle` gates will catch the drift.
- Diagnostics have stable `BLxxxx` codes (`src/Diagnostics.fs`); corpus tests pin them, so
  changing a code or message means updating pins in the same change.

## Docs map

`docs/formalism.md` is canonical for semantics (types, index types, loop objects,
combinators, symmetry, §15 concrete syntax). `docs/features.md` is the feature census; its
Status column can lag the implementation, so trust formalism.md and the corpus when they
disagree. `docs/proofs.md` maps which guarantees are machine-checked in `proofs/` versus
implemented-and-corpus-pinned. Tutorials: `docs/quickstart-1.md`, `docs/quickstart-2.md`.
The richest idiom sources are `examples/01_weather_stations.blade`,
`examples/03_signal_conditioning.blade`, and `examples/lsdft.blade`.
