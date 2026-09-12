// Blade runtime error support: shadow call stack + panic, plus the scalar
// math the interpreter cannot borrow from libm (lgamma, digamma). Host-only;
// device compilation sees no-op stubs.
//
// The shadow call stack is a thread_local array of Frames (correct under
// OpenMP) pushed/popped by an RAII Scope at each Blade function-body entry
// (BLADE_FRAME). On failure, blade_rt::panic prints an `error[BLxxxx]:`
// line, the failing source location (when carried), and the Blade call
// stack (innermost first), then exits(1).
//
// __CUDA_ARCH__ is defined ONLY during nvcc's device passes: host passes get
// the real implementation, device passes get a no-op BLADE_FRAME macro and
// no blade_rt symbols.
#pragma once
#include <iostream>
#include <cstdlib>
#include <cmath>
#if !defined(__CUDA_ARCH__)
namespace blade_rt {
  struct Frame { const char* fn; const char* file; int line; };
  // 65 slots, not 64: slots 0..63 are the trace, slot 64 is a write-only
  // scratch sink for overflow frames (see Scope below). panic never reads it.
  inline thread_local Frame stack[65];
  inline thread_local int   depth = 0;
  struct Scope {
    Scope(const char* fn, const char* file, int line) {
      // BRANCHLESS on purpose: the obvious `if (depth < 64) stack[depth] = ...`
      // costs 13-27 ns PER ELEMENT in a loop over an inlined kernel. NOT the
      // emulated-TLS lookup (GCC hoists it out of the loop) -- it is the
      // branch on a value reloaded from TLS-opaque memory each iteration,
      // which the compiler cannot prove doesn't alias the user pool, forcing
      // a loop-carried load->branch->store->load chain. Clamping to a
      // scratch slot compiles to a `cmov` instead: ~0.02 ns/element.
      //
      // Observably IDENTICAL to the guarded form: slots 0..63 get the same
      // frames, overflow frames land in slot 64 (never read by panic). The
      // interpreter twin (src/Interp/Core.fs InterpState.FrameNames) pins
      // that window, staying correct.
      stack[depth < 64 ? depth : 64] = {fn, file, line};
      ++depth;
    }
    ~Scope() { --depth; }
  };
  // INVARIANT, load-bearing: the ONLY reader of the shadow stack, called
  // ONLY from generated code. resolveShadowFrames (CodeGen) omits the frame
  // for kernel bodies that reach no panic, from generated text alone, so a
  // panic call added to another runtime header would silently cost those
  // kernels their trace frame -- add one only with a matching
  // resolveShadowFrames rule (tripwire in tests/Test_Diagnostics.fs).
  // The failure that ended the run, for the run record (blade_run_record.hpp
  // reads these at exit): empty code = the program exited normally.
  inline const char* exit_code = "";
  inline const char* exit_message = "";
  [[noreturn]] inline void panic(const char* code, const char* msg,
                                 const char* file, int line) {
    exit_code = code;
    exit_message = msg;
    std::cerr << "error[" << code << "]: " << msg << "\n";
    if (file && line > 0) std::cerr << "  --> " << file << ":" << line << "\n";
    int d = depth < 64 ? depth : 64;
    for (int i = d - 1; i >= 0; --i) {
      std::cerr << "  at " << stack[i].fn;
      if (stack[i].file && stack[i].line > 0)
        std::cerr << " (" << stack[i].file << ":" << stack[i].line << ")";
      std::cerr << "\n";
    }
    std::exit(1);
  }

  // ---- lgamma(x) = log Gamma(x), x > 0. Lanczos approximation, g = 7, n = 9.
  //
  // HAND-ROLLED ON PURPOSE; this is deliberately NOT std::lgamma. The
  // tree-walking interpreter (src/Interp/Numerics.fs) must reproduce every
  // double these binaries print, BIT FOR BIT. It manages that for the other
  // intrinsics by P/Invoking the very ucrtbase.dll that MinGW's libstdc++
  // forwards <cmath> to -- a trick with no counterpart here, because .NET has
  // no gamma function at all to fall back on and the ucrt/glibc lgamma
  // implementations are not the same function. A series written out in plain
  // sequential double arithmetic is the only form BOTH sides can execute
  // identically, so this function is transcribed statement for statement into
  // Interp/Numerics.fs `lgammaLanczos`. KEEP THE TWO IN LOCKSTEP: same
  // coefficients, same association, no reassociation, no reordering. (Byte
  // identity also needs FMA contraction off, which is what the differential
  // gates already pin -- BLADE_FP_CONTRACT=off, see src/Build.fs:38.)
  //
  // DOMAIN: x > 0 only. `!(x > 0.0)` also catches NaN. Log-densities (Gamma /
  // Poisson / Beta, the callers this exists for) never need a non-positive
  // argument, so the reflection formula that would extend it to x < 0 is
  // deliberately absent: one fewer thing to keep bit-identical, and a
  // non-positive argument is a caller bug that panics rather than seeping
  // through as a silent NaN.
  //
  // The panic is also why a generated body calling this KEEPS its shadow
  // frame: `blade_rt::` is deliberately absent from CodeGen's
  // panicFreeNamespaces (CodeGen.fs:15178), so the frame analysis classifies
  // such a body as UNKNOWN and resolveShadowFrames leaves its BLADE_FRAME in
  // place. That is the reason this must live HERE and nowhere else -- the
  // tripwire in tests/Test_Diagnostics.fs fails if any other src/cpp header
  // names blade_rt::panic.
  inline double lgamma(double x) {
    if (!(x > 0.0))
      panic("BL8008", "lgamma: argument must be positive", nullptr, 0);
    // Gamma(1) = Gamma(2) = 1 exactly; the series lands a few ulp off zero.
    // The same two comparisons run on the interpreter side, so this stays
    // exact on both.
    if (x == 1.0 || x == 2.0) return 0.0;
    // The series is usually written over z = x - 1 with denominators (z + k);
    // it is written over x here instead (same values, z + k == x + (k-1)),
    // because forming `(x - 1) + k` cancels catastrophically for small x --
    // (1e-8 - 1) + 1 is not 1e-8 -- and costs ~9 digits below x ~ 1e-6.
    double s = 0.99999999999980993;
    s += 676.5203681218851     / x;
    s += -1259.1392167224028   / (x + 1.0);
    s += 771.32342877765313    / (x + 2.0);
    s += -176.61502916214059   / (x + 3.0);
    s += 12.507343278686905    / (x + 4.0);
    s += -0.13857109526572012  / (x + 5.0);
    s += 9.9843695780195716e-6 / (x + 6.0);
    s += 1.5056327351493116e-7 / (x + 7.0);
    const double t = x + 6.5;   // (x - 1) + g + 0.5, with g = 7
    // 0.9189385332046727 = log(2*pi) / 2
    return 0.9189385332046727 + (x - 0.5) * std::log(t) - t + std::log(s);
  }

  // ---- digamma(x) = psi(x) = d/dx log Gamma(x), x > 0.
  //
  // HAND-ROLLED for the same reason lgamma above is, and under the same
  // contract: transcribed statement for statement into Interp/Numerics.fs
  // `digammaSeries`, and the two must stay in LOCKSTEP -- same constants,
  // same association, no reassociation, no reordering. Neither ucrtbase nor
  // .NET has a digamma to borrow, so a series in plain sequential double
  // arithmetic is again the only form both evaluators can run identically.
  //
  // METHOD: recurrence down to the asymptotic regime, then the Stirling-type
  // asymptotic series. psi(x) = psi(x+1) - 1/x is applied until x >= 10,
  // accumulating the shifts in `r`; then
  //
  //   psi(x) ~ log(x) - 1/(2x) - sum_{n>=1} B_{2n} / (2n x^{2n})
  //
  // truncated after n = 7. The seven coefficients B_{2n}/(2n) are, with
  // B_2..B_14 = 1/6, -1/30, 1/42, -1/30, 5/66, -691/2730, 7/6:
  //
  //   n=1  B_2 /2  =  1/12          n=5  B_10/10 =  1/132
  //   n=2  B_4 /4  = -1/120         n=6  B_12/12 = -691/32760
  //   n=3  B_6 /6  =  1/252         n=7  B_14/14 =  1/12
  //   n=4  B_8 /8  = -1/240
  //
  // WHY x >= 10 AND SEVEN TERMS: the series is asymptotic, so the pair is a
  // measured choice, not a convention. Against a long-double reference
  // (same series, shifted to x >= 40) over 206,001 probes -- a 200,000-point
  // linear sweep of (0, 100] and a 6,001-point geometric sweep of
  // 1e-30..1e30, scored as |err| / max(|psi|, 1) so the metric stays
  // meaningful across psi's zero at x = 1.4616... -- the worst error is:
  //
  //   shift to  4 terms   5 terms   6 terms   7 terms
  //   x >= 6    ...       8.8e-12   9.3e-13   1.3e-13
  //   x >= 8    ...       2.9e-13   1.8e-14   2.1e-15
  //   x >= 10   ...       2.1e-14   1.8e-15   1.1e-15   <-- chosen
  //
  // 1.1e-15 is the double-roundoff floor of the recurrence itself (the
  // accumulated -1/x cancels against log(x) to about one digit near x ~ 1.2),
  // so more terms or a larger shift buy nothing. The customary "shift to
  // x >= 6" would have left ~1e-13, two orders short.
  //
  // FMA-FREE BY CONSTRUCTION: the series is summed as `c / p` with p stepped
  // by `p = p * x2`, never as the Horner `s = s * f + c`. Both forms measure
  // identically (1.130e-15 worst, same argument), but this one contains no
  // multiply-add pair anywhere, so no contraction setting on either side can
  // perturb it -- the byte-identity claim does not rest on -ffp-contract=off
  // for this function. (lgamma above is FMA-free for the same reason: its
  // terms are add-of-divide.)
  //
  // Nothing is pinned at special points, unlike lgamma's exact zeros at
  // x = 1 and x = 2: psi has no rational value at any convenient argument
  // (psi(1) = -gamma), so there is nothing exact to return.
  //
  // DOMAIN: x > 0, `!(x > 0.0)` so NaN is refused too, panicking with the
  // same BL8008 lgamma uses. Same reasoning: a non-positive argument here is
  // a caller bug, and a silent NaN would surface much later as an
  // unexplained NaN gradient. The reflection formula is deliberately absent.
  inline double digamma(double x) {
    if (!(x > 0.0))
      panic("BL8008", "digamma: argument must be positive", nullptr, 0);
    // psi(x) = psi(x+1) - 1/x, applied until the asymptotic series is good.
    double r = 0.0;
    while (x < 10.0) { r -= 1.0 / x; x += 1.0; }
    const double x2 = x * x;
    double p = x2;                                  // x^2, then x^4, x^6, ...
    double s =      (1.0 / 12.0)      / p;  p = p * x2;
    s = s +        (-1.0 / 120.0)     / p;  p = p * x2;
    s = s +         (1.0 / 252.0)     / p;  p = p * x2;
    s = s +        (-1.0 / 240.0)     / p;  p = p * x2;
    s = s +         (1.0 / 132.0)     / p;  p = p * x2;
    s = s +      (-691.0 / 32760.0)   / p;  p = p * x2;
    s = s +         (1.0 / 12.0)      / p;
    return r + std::log(x) - 0.5 / x - s;
  }
}
#define BLADE_FRAME(fn, file, line) blade_rt::Scope __blade_frame_(fn, file, line)
#else
#define BLADE_FRAME(fn, file, line)
#endif
