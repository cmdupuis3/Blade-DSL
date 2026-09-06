// Blade `rand` module runtime -- deterministic, cross-compiler-stable RNG.
//
// The `rand` module cannot be expressed in Blade source (the language has no
// unsigned integers and no bitwise operators), so the compiler emits calls into
// this header. std::mt19937_64 supplies the raw 64-bit stream (bit-exact per the
// C++ standard, identical across libstdc++/libc++/MSVC); the [0,1) mapping and
// the normal transform are implemented HERE rather than via std::uniform_real_
// distribution / std::normal_distribution (both implementation-defined), so a
// corpus EXPECT pinned once stays valid on any toolchain.
//
// API surface (called from generated main()):
//   blade_rand::uniform    (double* out, size_t n, int64_t key)                  -- U[0,1)
//   blade_rand::normal     (double* out, size_t n, int64_t key)                  -- N(0,1)
//   blade_rand::exponential(double* out, size_t n, int64_t key, double rate)     -- Exp(rate)
//   blade_rand::gamma      (double* out, size_t n, int64_t key, double sh, double rt)
//   blade_rand::poisson    (double* out, size_t n, int64_t key, double lam)
//   blade_rand::bernoulli  (double* out, size_t n, int64_t key, double p)
//   blade_rand::beta       (double* out, size_t n, int64_t key, double a, double b)
//   blade_rand::categorical(int64_t* out, size_t n, int64_t key, const double* w, size_t k)
//
// ELEMENT TYPE. Every fill EXCEPT `categorical` writes `double`, including the
// two integer-valued families (poisson counts, bernoulli 0/1). That is
// deliberate: one `double*` out-pointer contract keeps the codegen seam
// (genRandGenBinding allocates a dense Float64 pool and hands over pool_base)
// and the interpreter mirror (RandMirror's `float[]`) uniform across those
// families, at the cost of an exactly-representable integer round-trip. Counts
// below 2^53 are exact, so nothing is lost numerically there.
//
// `categorical` is the exception, and writes `int64_t`. Its output is not a
// measurement that happens to be integral -- it is a SUBSCRIPT, and the whole
// point of drawing it is to index the array the weights came from. A Float64
// index would need a coercion the rand surface does not have, so this family
// carries its own out-pointer type. The seam that made this cheap is that
// codegen was already element-type-generic: genRandGenBinding allocates
// `Array<elemTypeToCpp(ElemType), rank>`, so the checker choosing
// `IRTScalar ETInt64` for this one family is what selects an `int64_t` pool,
// and the fill signature follows. The interpreter mirror correspondingly
// returns `int64[]` into an SInt store rather than `float[]`/SFloat.
//
// PARAMETERS are runtime doubles: the Blade surface accepts any Float64-typed
// expression for rate/shape/lam/p/a/b (only the SHAPE must be static). They are
// passed after the key so the (out, n, key) prefix stays identical everywhere.
// `categorical` instead takes an ARRAY parameter -- a pointer to the rank-1
// Float64 weights pool plus its (static, checker-pinned) length -- passed in
// the same position, after the key.
//
// `key` is the stream key: same key => same sequence; nearby keys decorrelate
// (SplitMix64 finalizer). The key-first signature is the seam for a future
// counter-based (Philox-style) backend -- only these function bodies change.
//
// EVERY transform below is hand-rolled and consumes the per-call mt19937_64
// stream STRICTLY SEQUENTIALLY, one `next_uniform`/`next_normal` at a time, with
// no buffering, no caching and no std::*_distribution anywhere. That is what
// lets src/Interp/RandMirror.fs replicate each draw operation-for-operation and
// keeps interpreter output byte-identical to the compiled binary's. Rejection
// loops are legal (and used by `gamma`) precisely because the accept/reject
// decision is itself a deterministic function of the stream.
#pragma once
#include <cstdint>
#include <cstddef>
#include <cmath>
#include <random>
#include <vector>

namespace blade_rand {

// SplitMix64 finalizer: decorrelates nearby keys before seeding the engine.
inline uint64_t mix64(uint64_t z) {
    z += 0x9E3779B97F4A7C15ULL;
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

// Top 53 bits of a 64-bit word, scaled to [0, 1). Explicit casts keep the
// int->double conversion clear of -Werror=narrowing / -Werror=float-conversion.
inline double bits_to_unit(uint64_t x) {
    return static_cast<double>(x >> 11) * (1.0 / 9007199254740992.0); // 2^-53
}

inline double next_uniform(std::mt19937_64& g) {
    return bits_to_unit(g());
}

// Box-Muller (our own; NOT std::normal_distribution). Two uniforms -> one
// standard normal. u1 is floored away from 0 so log(u1) stays finite.
inline double next_normal(std::mt19937_64& g) {
    const double two_pi = 6.283185307179586476925286766559;
    double u1 = next_uniform(g);
    double u2 = next_uniform(g);
    if (u1 < (1.0 / 9007199254740992.0)) u1 = (1.0 / 9007199254740992.0);
    return std::sqrt(-2.0 * std::log(u1)) * std::cos(two_pi * u2);
}

// Exp(rate) by inverse CDF: -log(1-u)/rate. ONE uniform per draw. u in [0,1)
// => 1-u in (0,1], so log() is finite without a floor (the u==0 endpoint that
// would be the singular one is unreachable from the OPEN end of the interval).
inline double next_exponential(std::mt19937_64& g, double rate) {
    double u = next_uniform(g);
    return -std::log(1.0 - u) / rate;
}

// Gamma(shape, 1) for shape >= 1 -- Marsaglia-Tsang (2000) squeeze. Each
// iteration consumes one normal (= two uniforms) and, when v > 0, one further
// uniform; a v <= 0 rejection consumes ONLY the normal and retries. The cheap
// polynomial squeeze is tried first and the log test is the fallback, exactly
// as published. This ordering is part of the mirror contract: RandMirror.fs
// must branch on the same conditions in the same sequence or the two streams
// desynchronize after the first rejection.
inline double next_gamma_ge1(std::mt19937_64& g, double shape) {
    const double d = shape - (1.0 / 3.0);
    const double c = 1.0 / std::sqrt(9.0 * d);
    for (;;) {
        double x = next_normal(g);
        double v = 1.0 + c * x;
        if (v <= 0.0) continue;
        v = v * v * v;
        double u = next_uniform(g);
        double x2 = x * x;
        if (u < 1.0 - 0.0331 * x2 * x2) return d * v;
        if (std::log(u) < 0.5 * x2 + d * (1.0 - v + std::log(v))) return d * v;
    }
}

// Gamma(shape, rate) for any shape > 0, rate > 0. shape < 1 uses the standard
// Marsaglia-Tsang BOOST: draw Gamma(shape+1, 1) and scale by u^(1/shape). Draw
// order is gamma-THEN-uniform (the mirror replicates it verbatim). `rate` is an
// inverse-scale, applied last by division.
inline double next_gamma(std::mt19937_64& g, double shape, double rate) {
    if (shape < 1.0) {
        double gg = next_gamma_ge1(g, shape + 1.0);
        double u = next_uniform(g);
        return gg * std::pow(u, 1.0 / shape) / rate;
    }
    return next_gamma_ge1(g, shape) / rate;
}

// Poisson(lam): two routes, split at kPoissonKnuthMaxLam.
//
//   lam <= kPoissonKnuthMaxLam  Knuth's product-of-uniforms (next_poisson_knuth):
//                               multiply U[0,1) draws until the running product
//                               drops to or below e^-lam; the number of
//                               multiplications after the first is the variate.
//   lam >  kPoissonKnuthMaxLam  Hormann's PTRS transformed rejection
//                               (next_poisson_ptrs).
//
// WHY TWO ROUTES. Knuth's comparison `p <= L` is exact-in-distribution only
// while p and L = e^-lam are NORMAL doubles. e^-lam is subnormal past lam ~708
// and exactly +0 past ~745, and the running product reaches +0 after ~745
// multiplications whatever lam is -- so for lam = 1000 and lam = 10000 the old
// single route returned the SAME draws (~740-780, the underflow step count), a
// confirmed wrong answer (plan-fortran-killer-2.md appendix B). The product
// route is kept for the lam it serves correctly and the rejection route
// covers the rest.
//
// WHY THE SPLIT SITS AT 500, NOT AT numpy's 10. At lam = 500 the product
// first underflows at k ~745, which is 11 standard deviations above the mean
// (P ~ 1e-27): the route is exact in practice there, and every lam below it
// is safer still. numpy switches to PTRS at lam >= 10 for COST -- Knuth is
// O(lam) uniforms per draw -- but moving the split changes every pinned draw
// between the two thresholds, and this header's contract is that a pinned
// stream never changes under it (RandMirror.fs mirrors both routes and the
// split). Cost, not correctness, is what a lower split would buy; the
// constant is the one place to move it, in lockstep with the mirror.
//
// TERMINATION of the product route: for lam <= 500, L is a normal double and
// `p <= L` fires after finitely many steps with probability 1. lam == 0
// gives L == 1.0 and terminates on the first draw with k == 0, which is
// correct.
constexpr double kPoissonKnuthMaxLam = 500.0;

inline double next_poisson_knuth(std::mt19937_64& g, double lam) {
    const double L = std::exp(-lam);
    double p = 1.0;
    double k = 0.0;
    for (;;) {
        p *= next_uniform(g);
        if (p <= L) return k;
        k += 1.0;
    }
}

// The PTRS route's arithmetic has `a * b + c` shapes (the k formula, the
// loggam series) that g++ would contract into FMAs under the shipping
// `-ffp-contract=fast`. RyuJIT never contracts, so a fused multiply-add here
// would desynchronize the interpreter mirror at a floor()/comparison knife
// edge -- rare, but the contract is byte identity, not "usually". GCC carries
// the override as a function attribute (blade_portability.hpp's BLADE_REPRO_FN
// uses the same mechanism; noinline because GCC will not inline across
// differing optimize options anyway). Clang honours the standard pragma
// inside the bodies below; MSVC's default is no contraction.
#if defined(__GNUC__) && !defined(__clang__)
  #define BLADE_RAND_NO_CONTRACT __attribute__((noinline, optimize("-ffp-contract=off")))
#else
  #define BLADE_RAND_NO_CONTRACT
#endif
#if defined(__clang__)
  #define BLADE_RAND_FP_CONTRACT_OFF_BODY _Pragma("STDC FP_CONTRACT OFF")
#else
  #define BLADE_RAND_FP_CONTRACT_OFF_BODY
#endif

// log(Gamma(x)) for the PTRS acceptance test, x >= 1. numpy's `loggam`
// (Zhang & Jin, Computation of Special Functions, section 3.1.2): shift x up
// to >= 7, Stirling series with ten Bernoulli terms, shift back down.
// Written out in plain sequential arithmetic -- NOT std::lgamma, whose mingw
// and ucrt implementations are different functions (see blade_runtime.hpp's
// lgamma note) -- so RandMirror.fs can run the identical statements.
// 0.9189385332046727 = log(2*pi) / 2, a literal on both sides.
BLADE_RAND_NO_CONTRACT inline double poisson_loggam(double x) {
    BLADE_RAND_FP_CONTRACT_OFF_BODY
    static const double a[10] = {
        8.333333333333333e-02, -2.777777777777778e-03,
        7.936507936507937e-04, -5.952380952380952e-04,
        8.417508417508418e-04, -1.917526917526918e-03,
        6.410256410256410e-03, -2.955065359477124e-02,
        1.796443723688307e-01, -1.39243221690590e+00 };
    if (x == 1.0 || x == 2.0) return 0.0;
    double x0 = x;
    int n = 0;
    if (x <= 7.0) {
        n = static_cast<int>(7.0 - x);
        x0 = x + static_cast<double>(n);
    }
    const double x2 = 1.0 / (x0 * x0);
    double gl0 = a[9];
    for (int k = 8; k >= 0; --k) gl0 = gl0 * x2 + a[k];
    double gl = gl0 / x0 + 0.9189385332046727 + (x0 - 0.5) * std::log(x0) - x0;
    for (int k = 1; k <= n; ++k) {
        gl -= std::log(x0 - 1.0);
        x0 -= 1.0;
    }
    return gl;
}

// Poisson(lam) for lam > kPoissonKnuthMaxLam by PTRS -- Hormann, "The
// transformed rejection method for generating Poisson random variables",
// Insurance: Mathematics and Economics 12 (1993) 39-45; constants and test
// order as in numpy's random_poisson_ptrs. Each iteration consumes exactly
// TWO uniforms (U then V) and decides accept / reject / continue in the order
// written: the cheap squeeze accepts most candidates, the k < 0 and tiny-us
// tests reject without a log, and the exact test runs last. Acceptance is
// ~0.9+ at these lam, so a draw costs ~2.2 uniforms whatever lam is. Draw
// order and branch order are the mirror contract; RandMirror.fs reproduces
// them statement for statement. `us` can be exactly 0 when U == -0.5 (a
// zero uniform): 2a/us is then +inf, k is -inf, and the k < 0 branch
// rejects -- no trap, same on both sides.
BLADE_RAND_NO_CONTRACT inline double next_poisson_ptrs(std::mt19937_64& g, double lam) {
    BLADE_RAND_FP_CONTRACT_OFF_BODY
    const double slam = std::sqrt(lam);
    const double loglam = std::log(lam);
    const double b = 0.931 + 2.53 * slam;
    const double a = -0.059 + 0.02483 * b;
    const double invalpha = 1.1239 + 1.1328 / (b - 3.4);
    const double vr = 0.9277 - 3.6224 / (b - 2.0);
    for (;;) {
        const double U = next_uniform(g) - 0.5;
        const double V = next_uniform(g);
        const double us = 0.5 - std::fabs(U);
        const double k = std::floor((2.0 * a / us + b) * U + lam + 0.43);
        if (us >= 0.07 && V <= vr) return k;
        if (k < 0.0 || (us < 0.013 && V > us)) continue;
        if (std::log(V) + std::log(invalpha) - std::log(a / (us * us) + b)
                <= -lam + k * loglam - poisson_loggam(k + 1.0))
            return k;
    }
}

// The dispatcher every `poisson` fill calls: the split above, nothing else.
inline double next_poisson(std::mt19937_64& g, double lam) {
    if (lam > kPoissonKnuthMaxLam) return next_poisson_ptrs(g, lam);
    return next_poisson_knuth(g, lam);
}

// Bernoulli(p): ONE uniform, 1.0 iff u < p. Returned as a double (see the
// element-type note in the header comment). Note this transform involves no
// libm call at all -- only a comparison -- so it is the one family whose draws
// are bit-identical between mirror and binary by construction.
inline double next_bernoulli(std::mt19937_64& g, double p) {
    return next_uniform(g) < p ? 1.0 : 0.0;
}

// Beta(a, b) = g1 / (g1 + g2) with g1 ~ Gamma(a,1), g2 ~ Gamma(b,1), drawn in
// that order. The s <= 0 guard catches the degenerate case where both gammas
// underflow to 0 (only reachable for very small a and b); 0.0 is returned
// rather than a NaN so the fill stays printable.
inline double next_beta(std::mt19937_64& g, double a, double b) {
    double g1 = next_gamma(g, a, 1.0);
    double g2 = next_gamma(g, b, 1.0);
    double s = g1 + g2;
    if (s <= 0.0) return 0.0;
    return g1 / s;
}

inline void uniform(double* out, size_t n, int64_t key) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_uniform(g);
}

inline void normal(double* out, size_t n, int64_t key) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_normal(g);
}

inline void exponential(double* out, size_t n, int64_t key, double rate) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_exponential(g, rate);
}

inline void gamma(double* out, size_t n, int64_t key, double shape, double rate) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_gamma(g, shape, rate);
}

inline void poisson(double* out, size_t n, int64_t key, double lam) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_poisson(g, lam);
}

inline void bernoulli(double* out, size_t n, int64_t key, double p) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_bernoulli(g, p);
}

inline void beta(double* out, size_t n, int64_t key, double a, double b) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    for (size_t i = 0; i < n; ++i) out[i] = next_beta(g, a, b);
}

// Categorical(w): an INDEX in [0, k) with P(i) = w_i / sum(w). The weights need
// not be normalized. Unlike every other family this one has no `next_*` helper:
// the normalized cumulative scan is loop-invariant, so it is computed ONCE per
// fill and shared by all `n` draws, and a per-draw helper would either recompute
// it or need the scan threaded through it.
//
// DRAW BUDGET: exactly ONE uniform per element, unconditionally -- including the
// degenerate branch below, which still draws before returning. That keeps the
// stream position a function of `n` alone, so the mirror stays in step no matter
// what the weights are.
//
// WEIGHT VALIDATION follows the wave-1 convention exactly: these fills never
// panic and never validate (gamma with shape <= 0 does not check, it just lets
// the arithmetic produce what it produces), and the one guard that exists --
// beta's `s <= 0` -> 0.0 -- is there to keep the output PRINTABLE rather than to
// report an error. Two guards here are of that same kind:
//   * A negative or NaN weight contributes 0 to the scan (`w[i] > 0.0` is false
//     for both). This is not error reporting; a non-monotone cumulative array
//     would make the inverse-CDF walk meaningless, so clamping is what gives the
//     walk a defined answer at all. A negative weight is therefore silently read
//     as zero probability.
//   * If the total is not positive (all weights zero/negative/NaN), every draw
//     returns index 0. Like beta's guard this keeps the fill printable and
//     in-range instead of producing a NaN or an out-of-bounds subscript.
// Neither case is diagnosed. Callers wanting rejection must check the weights in
// Blade before the call.
//
// SCALE INVARIANCE: scaling every weight by a power of two leaves the draws
// BIT-identical (the scan and the division by the total scale exactly), which is
// what the corpus scale-invariance test pins. For a general scale factor the
// draws agree up to the rounding of the scan, as with any float reduction.
inline void categorical(int64_t* out, size_t n, int64_t key, const double* w, size_t k) {
    std::mt19937_64 g(mix64(static_cast<uint64_t>(key)));
    // One-time cumulative scan, running sum left to right (the mirror sums in
    // this same order -- a different association would round differently).
    std::vector<double> cum(k);
    double acc = 0.0;
    for (size_t i = 0; i < k; ++i) {
        acc += (w[i] > 0.0) ? w[i] : 0.0;
        cum[i] = acc;
    }
    const double total = acc;
    const bool degenerate = !(total > 0.0);
    if (!degenerate) {
        for (size_t i = 0; i < k; ++i) cum[i] /= total;
    }
    // After normalization cum[k-1] == 1.0 exactly and every u is < 1.0, so the
    // walk always finds a j; the `j + 1 < k` bound is a belt-and-braces clamp.
    // Zero-weight indices are unreachable: they leave cum flat, and the strict
    // `u >= cum[j]` step walks past every flat run.
    for (size_t i = 0; i < n; ++i) {
        double u = next_uniform(g);
        if (degenerate) { out[i] = 0; continue; }
        size_t j = 0;
        while (j + 1 < k && u >= cum[j]) ++j;
        out[i] = static_cast<int64_t>(j);
    }
}

} // namespace blade_rand
