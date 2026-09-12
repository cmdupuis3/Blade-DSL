# Blade examples

Nine self-contained programs, each solving a realistic problem with the
idioms that fit it. They are a tour of the language's surface at working
scale (~150-250 lines each), not feature probes — for those see
`tests/corpus/`.

Run any of them from the repo root (needs g++ on PATH):

```
dotnet run -- run examples/01_weather_stations.blade
dotnet run -- check examples/01_weather_stations.blade   # typecheck only
```

(Exception: `09_qg_atmosphere.blade` reads a committed zarr store by a
relative path, so run it from this directory —
`cd examples && dotnet run --project .. -- run 09_qg_atmosphere.blade`.)

Each file keeps the corpus conventions — a unique `// TEST:` first line and
`// EXPECT: name = value` comments documenting the verified output — so any
of them can be promoted into a suite category unchanged. Expected values
were derived independently of the compiler (exact hand arithmetic, plus
PowerShell reference implementations for cumulants, quadrature, and the
Euler simulation), and several examples also pin internal cross-checks.

| File | Problem | Idioms on display |
| --- | --- | --- |
| `01_weather_stations.blade` | Sensor-network QC and aggregation | unit-annotated columns (`Float<meters>`/`Float<celsius>` element types surviving the whole SQL surface), `mask`/`compound` WHERE, semijoin via `contains`, `group_by` + per-group `reduce`, foreign-key re-broadcast of computed aggregates, key-extractor `sort`, positional mask AND/OR, `intersect`/`union`, single-pass multi-statistic sweep (`<&!>`-fused `reduce`) |
| `02_portfolio_moments.blade` | Two-asset return analytics | `moments`/`comoments`/`cumulants` formers, the fused pool == `gram` == pooled-covariance three-way pin, `where comm` + `reynolds` packed symmetric storage, `decompact`, `independent()` + `where indep` licensed `Dist` algebra, `dist_affine` hedging pushforward |
| `03_signal_conditioning.blade` | Multi-channel DAQ conditioning | deferred `object_for` stages composed `>>@`, `@>>`, the duality identity, 3-way `<&!>` single-pass RMS/peak/mean, `<$>` dB post-map, `guard` gating, `<|>` failover, `object_for(<@>)`/`object_for(<&>)` sections with `|@>` chains, 3-way `zip`, `>>=`, `sequence`, `replicate`, `range`/`reverse` (with a `Float<seconds>`-annotated time axis) |
| `04_trajectory_sensitivity.blade` | Projectile with drag | unit-checked kinematics (derived `velocity`/`acceleration` units) both scalar and lifted to arrays (broadcasts compose unit signatures elementwise; the `Float<meters>` result annotation is checked, not trusted), `grad()` over a quadrature carried as a recursive array (an additive prefix sum) with in-file finite-difference cross-checks, and the drag-coupled Euler dynamics as a recursive array — a genuine sequential recurrence expressed by structural induction on extent, not an imperative loop |
| `05_streaming_telemetry.blade` | Distributed latency telemetry | `mstate` sufficient-statistic states, chained `mstate_merge` (monoid), merged == batch cumulant pin, `comoments_merge` pooled covariance, `dist_affine` KPI transform, `moments(d, k)` Wick/Isserlis reconstruction |
| `06_cg_selection_rules.blade` | Clebsch-Gordan selection rules | dependent record with range-refined fields and a named static `where` predicate — the record guards ARE the selection rules; construction outside the valid set panics |
| `07_subgrid_closure_discovery.blade` | Learning a subgrid stress closure (3-D, rotational) | `sgs.grad`/`sgs.stress` field formers (the exact stress IS a comoment), `ml.tensor_to_irreps`/`ml.sym_to_irreps` Cartesian bridges, a `where ml.equiv(O3)` CERTIFIED closure (`derive_tp` → `gated` → `derive_linear`) trained with `ad.grad`, and the trained model's baked-Wigner rotation certificate — inadmissible closures are unwritable |
| `08_burgers_les.blade` | Burgers DNS→LES closure discovery (a posteriori, Galilean) | in-language DNS truth, box filter + exact subgrid stress, least-squares Smagorinsky coefficient (a comoment ratio), a `where ml.galilean(...)` CERTIFIED closure (frame-dependent closures are BL4009), and the payoff pin: the learned closure beats the no-model LES 4× against filtered DNS |
| `09_qg_atmosphere.blade` | Quasigeostrophic atmosphere (pyqg port) | pseudo-spectral solver on `sp.fft2`/`sp.ifft2`, NOMINAL axes (`type Y`/`type X`, per-axis wavenumber arrays, typed-literal probes, `RF`/`CF` whole-array aliases), spectral tables as unary data-driven maps + `<*>` outer products, every pyqg stage a named function refreshed by whole-array `state = stage(...)` assignment, an `object_for` \|z\|² kernel reused across diagnostics, reductions as row-kernel + `prodsum`/`reduce` towers, rotating AB3 buffers, `import zarr` ICs + baked filter — only the time loops are imperative; three verified regimes (Rossby wave vs analytic phase speed, McWilliams-84 selective decay, two-layer baroclinic growth), every pin mirrored to the ulp by `tools/qg_reference.fsx`; the header documents the v1 deferrals it works around (rank-2 multi-array co-iteration) |
| `10_pairwise_normalized_sums.blade` | Normalized pairwise sums without the pairwise array | kernel smoothing, two-pass softmax rows, the one-pass tuple-state online form, and matrix-valued rows -- each a per-row kernel whose body names a DEFERRED producer and folds it with a reduction join (`object_for(<&!>) <@> (reduce(w, (+)), prodsum(w, v))`), so the only allocation is the output; differential pins against the materialized M x N reference (docs/plans/structural/03) |

Beyond the numbered tour: `lsdft.blade` and `lswosa.blade` are full Lomb-Scargle
paper pipelines (the largest single programs in the repo), `lseof.bladenb` is their
notebook-form EOF sibling, and `physics/` is a separate 47-program corpus of short
physics derivations with its own README. `tools/` holds the `.fsx` reference
implementations the pins above cite; `data/` is the committed zarr store for `09`.
