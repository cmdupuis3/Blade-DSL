/// Bit-exact F# mirror of cpp/rand_runtime.hpp (namespace blade_rand) for the
/// Blade tree-walking interpreter. Every draw here must byte-match what the
/// generated C++ binary produces so interpreter output == compiled output.
///
/// Source of truth: cpp/rand_runtime.hpp -- if that file changes, this mirror
/// must be updated in lockstep. The `mix64`/`Mt19937_64`/`bitsToUnit` core is
/// a faithful copy of spectra/Rand.fs (module BladeSpectra.Rand), the
/// pre-existing, oracle-validated uniform mirror; it is copied here rather
/// than shared to avoid a cross-project (.fsproj) file-include coupling and
/// to keep this module self-contained under Blade.Interp.
///
/// Codegen contract mirrored (CodeGen.fs genRandGenBinding):
///   blade_rand::<kind>(pool_base(A.data), card, key [, p1 [, p2]])
/// where `card` = product of ALL extents (dense SymNone, row-major flat pool),
/// `key` is the int64 stream key, `p1`/`p2` are the family's runtime Float64
/// parameters (cast to double by codegen, evaluated by the interpreter), one
/// draw per pool slot, filled in flat order.
///
/// DRAW BUDGET per element -- the property that keeps the streams in step:
///   uniform      1 uniform
///   normal       2 uniforms (Box-Muller, cos branch only; the sin partner is
///                NOT cached, see `nextNormal`)
///   exponential  1 uniform
///   bernoulli    1 uniform
///   poisson      lam < 10: lam+1 uniforms in expectation (Knuth, data-
///                dependent); lam >= 10: 2 uniforms per PTRS iteration (U
///                then V), ~2.2 per draw. The split constant and both routes
///                are mirrored (`poissonKnuthMaxLam`, `nextPoissonKnuth`,
///                `nextPoissonPtrs`).
///   gamma        per Marsaglia-Tsang iteration: 1 normal, plus 1 uniform when
///                the iteration is not rejected at v <= 0; plus 1 more uniform
///                for the shape<1 boost. Data-dependent -- so every branch
///                condition here must be evaluated in the SAME order as the
///                header or the two streams desynchronize at the first
///                rejection and never recover.
///   beta         two gammas, in the order (a, then b).
///   categorical  1 uniform, unconditionally (including the degenerate-weights
///                branch, which draws before returning 0).
///
/// ELEMENT TYPE. Every family here returns `float[]` EXCEPT `categorical`, which
/// returns `int64[]` because its fill writes an `int64_t` pool (see the element
/// type note in rand_runtime.hpp). That is why the dispatch is split into
/// `draws` (float families) and `drawsCategorical`, rather than one function
/// with a widened return: the two feed different interpreter stores (SFloat vs
/// SInt) and a common `float[]` return would reintroduce exactly the Float64
/// index the int64 pool exists to avoid.
module Blade.Interp.RandMirror

// Core stream (copy of spectra/Rand.fs BladeSpectra.Rand).

/// SplitMix64 finalizer (rand_runtime.hpp `mix64`): decorrelates nearby keys
/// before seeding the engine.
let mix64 (z0: uint64) : uint64 =
    let z = z0 + 0x9E3779B97F4A7C15UL
    let z = (z ^^^ (z >>> 30)) * 0xBF58476D1CE4E5B9UL
    let z = (z ^^^ (z >>> 27)) * 0x94D049BB133111EBUL
    z ^^^ (z >>> 31)

/// std::mt19937_64: MT19937-64 with the single-uint64 seed constructor
/// (bit-exact per the C++ standard).
type Mt19937_64(seed: uint64) =
    let n = 312
    let m = 156
    let matrixA = 0xB5026F5AA96619E9UL
    let upperMask = 0xFFFFFFFF80000000UL
    let lowerMask = 0x7FFFFFFFUL
    let mt = Array.zeroCreate<uint64> n
    let mutable mti = n
    do
        mt.[0] <- seed
        for i in 1 .. n - 1 do
            mt.[i] <- 6364136223846793005UL * (mt.[i-1] ^^^ (mt.[i-1] >>> 62)) + uint64 i
    member _.Next() : uint64 =
        if mti >= n then
            for i in 0 .. n - 1 do
                let x = (mt.[i] &&& upperMask) ||| (mt.[(i + 1) % n] &&& lowerMask)
                let mutable xa = x >>> 1
                if x &&& 1UL <> 0UL then xa <- xa ^^^ matrixA
                mt.[i] <- mt.[(i + m) % n] ^^^ xa
            mti <- 0
        let mutable y = mt.[mti]
        mti <- mti + 1
        y <- y ^^^ ((y >>> 29) &&& 0x5555555555555555UL)
        y <- y ^^^ ((y <<< 17) &&& 0x71D67FFFEDA60000UL)
        y <- y ^^^ ((y <<< 37) &&& 0xFFF7EEE000000000UL)
        y ^^^ (y >>> 43)

/// 2^-53 = 1.0 / 9007199254740992.0 (rand_runtime.hpp scale/floor constant).
let private twoPow53Inv : float = 1.0 / 9007199254740992.0

/// Top 53 bits scaled to [0,1) (rand_runtime.hpp `bits_to_unit`).
let bitsToUnit (x: uint64) : float =
    float (x >>> 11) * twoPow53Inv

/// rand_runtime.hpp `next_uniform`: one uniform draw from the engine.
let inline private nextUniform (g: Mt19937_64) : float =
    bitsToUnit (g.Next())

/// rand_runtime.hpp `next_normal`: Box-Muller, TWO uniforms -> one N(0,1).
/// u1 is floored away from 0 so log(u1) stays finite. The sin partner is NOT
/// produced or cached: every call consumes exactly two fresh uniform draws and
/// returns only the cos branch. Arithmetic/order/literal match the header.
let private nextNormal (g: Mt19937_64) : float =
    let twoPi = 6.283185307179586476925286766559
    let mutable u1 = nextUniform g
    let u2 = nextUniform g
    if u1 < twoPow53Inv then u1 <- twoPow53Inv
    sqrt (-2.0 * log u1) * cos (twoPi * u2)

/// rand_runtime.hpp `next_exponential`: ONE uniform -> Exp(rate) by inverse CDF.
/// The prefix `-` binds tighter than `/` in F# exactly as in C++, so this is
/// `(-log(1-u)) / rate`, not `-(log(1-u)/rate)` -- the two differ in the sign of
/// a zero result and would print differently.
let private nextExponential (g: Mt19937_64) (rate: float) : float =
    let u = nextUniform g
    -(log (1.0 - u)) / rate

/// rand_runtime.hpp `next_gamma_ge1`: Marsaglia-Tsang for shape >= 1, scale 1.
/// The `while` loop reproduces the header's `for(;;)` with `continue` on
/// v <= 0: that rejection consumes the normal (two uniforms) and NOTHING else,
/// which is why the uniform draw sits inside the v > 0 branch.
let private nextGammaGe1 (g: Mt19937_64) (shape: float) : float =
    let d = shape - (1.0 / 3.0)
    let c = 1.0 / sqrt (9.0 * d)
    let mutable result = 0.0
    let mutable accepted = false
    while not accepted do
        let x = nextNormal g
        let v0 = 1.0 + c * x
        if v0 > 0.0 then
            let v = v0 * v0 * v0
            let u = nextUniform g
            let x2 = x * x
            if u < 1.0 - 0.0331 * x2 * x2 then
                result <- d * v
                accepted <- true
            elif log u < 0.5 * x2 + d * (1.0 - v + log v) then
                result <- d * v
                accepted <- true
    result

/// rand_runtime.hpp `next_gamma`: any shape > 0. shape < 1 takes the boost
/// branch (Gamma(shape+1) THEN one uniform, in that draw order).
let private nextGamma (g: Mt19937_64) (shape: float) (rate: float) : float =
    if shape < 1.0 then
        let gg = nextGammaGe1 g (shape + 1.0)
        let u = nextUniform g
        gg * (u ** (1.0 / shape)) / rate
    else
        nextGammaGe1 g shape / rate

/// rand_runtime.hpp `kPoissonKnuthMaxLam`: the lam at and above which
/// `next_poisson` takes the PTRS route -- Hormann's stated domain (mu >= 10)
/// and numpy's crossover; the header's comment carries the cost argument.
let private poissonKnuthMaxLam = 10.0

/// rand_runtime.hpp `next_poisson_knuth`: Knuth's product-of-uniforms.
/// Consumes lam+1 uniforms in expectation; the count is returned as a float,
/// matching the header's `double k`.
let private nextPoissonKnuth (g: Mt19937_64) (lam: float) : float =
    let L = exp (-lam)
    let mutable p = 1.0
    let mutable k = 0.0
    let mutable go = true
    while go do
        p <- p * nextUniform g
        if p <= L then go <- false
        else k <- k + 1.0
    k

/// rand_runtime.hpp `poisson_loggam`: log(Gamma(x)) for x >= 1 (numpy's
/// `loggam`, Zhang & Jin section 3.1.2) -- shift to x0 >= 7, ten-term
/// Stirling series, shift back. Same constants, same association, same
/// literal 0.9189385332046727 for log(2 pi)/2. `int (7.0 - x)` truncates
/// toward zero exactly as the header's static_cast<int>.
let private poissonLoggamCoeffs =
    [| 8.333333333333333e-02; -2.777777777777778e-03
       7.936507936507937e-04; -5.952380952380952e-04
       8.417508417508418e-04; -1.917526917526918e-03
       6.410256410256410e-03; -2.955065359477124e-02
       1.796443723688307e-01; -1.39243221690590e+00 |]

let private poissonLoggam (x: float) : float =
    if x = 1.0 || x = 2.0 then 0.0
    else
        let a = poissonLoggamCoeffs
        let mutable x0 = x
        let mutable n = 0
        if x <= 7.0 then
            n <- int (7.0 - x)
            x0 <- x + float n
        let x2 = 1.0 / (x0 * x0)
        let mutable gl0 = a.[9]
        for k in 8 .. -1 .. 0 do
            gl0 <- gl0 * x2 + a.[k]
        let mutable gl = gl0 / x0 + 0.9189385332046727 + (x0 - 0.5) * log x0 - x0
        for _ in 1 .. n do
            gl <- gl - log (x0 - 1.0)
            x0 <- x0 - 1.0
        gl

/// rand_runtime.hpp `next_poisson_ptrs`: Hormann's PTRS for lam >= 10. Each
/// iteration draws U THEN V and decides in the header's order -- squeeze
/// accept, k < 0 / tiny-us reject, exact log test -- so the two streams stay
/// in step through every rejection. `us` = 0 (a zero uniform) gives k = -inf
/// and the k < 0 reject, on both sides, without a trap.
let private nextPoissonPtrs (g: Mt19937_64) (lam: float) : float =
    let slam = sqrt lam
    let loglam = log lam
    let b = 0.931 + 2.53 * slam
    let a = -0.059 + 0.02483 * b
    let invalpha = 1.1239 + 1.1328 / (b - 3.4)
    let vr = 0.9277 - 3.6224 / (b - 2.0)
    let mutable result = 0.0
    let mutable accepted = false
    while not accepted do
        let U = nextUniform g - 0.5
        let V = nextUniform g
        let us = 0.5 - abs U
        let k = floor ((2.0 * a / us + b) * U + lam + 0.43)
        if us >= 0.07 && V <= vr then
            result <- k
            accepted <- true
        elif k < 0.0 || (us < 0.013 && V > us) then
            ()
        elif log V + log invalpha - log (a / (us * us) + b)
             <= -lam + k * loglam - poissonLoggam (k + 1.0) then
            result <- k
            accepted <- true
    result

/// rand_runtime.hpp `next_poisson`: the split, nothing else.
let private nextPoisson (g: Mt19937_64) (lam: float) : float =
    if lam >= poissonKnuthMaxLam then nextPoissonPtrs g lam
    else nextPoissonKnuth g lam

/// rand_runtime.hpp `next_bernoulli`: ONE uniform, 1.0 iff u < p.
let private nextBernoulli (g: Mt19937_64) (p: float) : float =
    if nextUniform g < p then 1.0 else 0.0

/// rand_runtime.hpp `next_beta`: g1/(g1+g2) from two unit-rate gammas drawn in
/// the order (a, then b).
let private nextBeta (g: Mt19937_64) (a: float) (b: float) : float =
    let g1 = nextGamma g a 1.0
    let g2 = nextGamma g b 1.0
    let s = g1 + g2
    if s <= 0.0 then 0.0 else g1 / s

// Public draw APIs (one per blade_rand fill).

/// Run one fill: seed a fresh engine from `key` and draw `n` values with
/// `next`, exactly as every `blade_rand::<kind>` body does.
let inline private fill (key: int64) (n: int) (next: Mt19937_64 -> float) : float[] =
    let g = Mt19937_64(mix64 (uint64 key))
    Array.init n (fun _ -> next g)

/// blade_rand::uniform(out, n, key): `n` draws ~ U[0,1) for stream `key`.
let uniform (key: int64) (n: int) : float[] = fill key n nextUniform

/// blade_rand::normal(out, n, key): `n` draws ~ N(0,1) via Box-Muller for
/// stream `key`. Each element consumes two uniform draws (no caching).
let normal (key: int64) (n: int) : float[] = fill key n nextNormal

/// blade_rand::exponential(out, n, key, rate).
let exponential (key: int64) (rate: float) (n: int) : float[] =
    fill key n (fun g -> nextExponential g rate)

/// blade_rand::gamma(out, n, key, shape, rate).
let gamma (key: int64) (shape: float) (rate: float) (n: int) : float[] =
    fill key n (fun g -> nextGamma g shape rate)

/// blade_rand::poisson(out, n, key, lam).
let poisson (key: int64) (lam: float) (n: int) : float[] =
    fill key n (fun g -> nextPoisson g lam)

/// blade_rand::bernoulli(out, n, key, p).
let bernoulli (key: int64) (p: float) (n: int) : float[] =
    fill key n (fun g -> nextBernoulli g p)

/// blade_rand::beta(out, n, key, a, b).
let beta (key: int64) (a: float) (b: float) (n: int) : float[] =
    fill key n (fun g -> nextBeta g a b)

/// blade_rand::categorical(out, n, key, w, k): `n` indices in [0, k) with
/// P(i) proportional to w_i. Mirrors the header body statement for statement --
/// the clamp of non-positive/NaN weights to 0, the running left-to-right scan,
/// the in-place division by the total, the degenerate short-circuit that still
/// consumes its uniform, and the strict `u >= cum[j]` walk with the j+1 < k
/// clamp. Note this fill does NOT go through `fill`: it returns int64 and it
/// hoists a per-call scan the way the C++ body does.
let categorical (key: int64) (weights: float[]) (n: int) : int64[] =
    let g = Mt19937_64(mix64 (uint64 key))
    let k = weights.Length
    let cum = Array.zeroCreate<float> k
    let mutable acc = 0.0
    for i in 0 .. k - 1 do
        acc <- acc + (if weights.[i] > 0.0 then weights.[i] else 0.0)
        cum.[i] <- acc
    let total = acc
    let degenerate = not (total > 0.0)
    if not degenerate then
        for i in 0 .. k - 1 do cum.[i] <- cum.[i] / total
    Array.init n (fun _ ->
        let u = nextUniform g
        if degenerate then 0L
        else
            let mutable j = 0
            while j + 1 < k && u >= cum.[j] do j <- j + 1
            int64 j)

/// Dispatch on the runtime `kind` string that codegen records in
/// RandGen (IR.fs RandomFillSpec), with the already-evaluated runtime Float64
/// parameters in surface order. Matches the internal builtin call surface
/// (__rand_<fam>): one int64 key, the family's params, `n` flat draws.
/// The parameter-count mismatch cases are unreachable -- the typechecker arm
/// fixes each family's arity -- but they fail loudly rather than silently
/// drawing from the wrong transform.
let draws (kind: string) (key: int64) (pars: float list) (n: int) : float[] =
    match kind, pars with
    | "uniform", []             -> uniform key n
    | "normal", []              -> normal key n
    | "exponential", [rate]     -> exponential key rate n
    | "gamma", [shape; rate]    -> gamma key shape rate n
    | "poisson", [lam]          -> poisson key lam n
    | "bernoulli", [p]          -> bernoulli key p n
    | "beta", [a; b]            -> beta key a b n
    | ("uniform" | "normal" | "exponential" | "gamma" | "poisson" | "bernoulli" | "beta"), _ ->
        failwith $"RandMirror.draws: rand kind '{kind}' got {List.length pars} parameter(s)"
    | "categorical", _ ->
        // Not a widening oversight: categorical draws are int64 and belong to
        // `drawsCategorical`. Reaching here means a caller routed the array-
        // parameter family through the scalar-parameter dispatch.
        failwith "RandMirror.draws: 'categorical' is an int64 fill -- use drawsCategorical"
    | other, _ ->
        failwith $"RandMirror.draws: unknown rand kind '{other}' (expected uniform | normal | exponential | gamma | poisson | bernoulli | beta)"

/// Int64 counterpart of `draws` for the one array-parameter family. Kept
/// separate rather than folded into `draws` because the return type differs
/// (see the element-type note in this module's header).
let drawsCategorical (kind: string) (key: int64) (weights: float[]) (n: int) : int64[] =
    match kind with
    | "categorical" -> categorical key weights n
    | other -> failwith $"RandMirror.drawsCategorical: unknown int64 rand kind '{other}' (expected categorical)"

// RandomFillSpec executor.

/// A filled random array: the flat, row-major pool plus its extents. Mirrors
/// the dense-SymNone pool codegen emits (genRandGenBinding): a single flat
/// `card`-length draw sequence, where `card` = product of extents.
type FilledRandom = { Data: float[]; Extents: int list }

/// Execute a rand fill exactly as CodeGen.fs genRandGenBinding emits it:
/// card = product of extents (row-major flat pool), one blade_rand call of
/// `card` draws keyed by `key`, filled in flat order. `kind` is the RandGen
/// kind; `key` is the already-evaluated int64 stream key; `pars` are the
/// already-evaluated runtime Float64 parameters in surface order; `extents`
/// are the (all positive, static) dense extents.
let runFill (kind: string) (key: int64) (pars: float list) (extents: int list) : FilledRandom =
    let card = extents |> List.fold (fun acc e -> acc * e) 1
    { Data = draws kind key pars card; Extents = extents }
