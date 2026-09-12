// blade_portability.hpp
//
// Compiler-portable spellings for the optimization annotations codegen emits
// into generated programs. This header is the ONLY place either spelling is
// chosen; CodeGen.fs emits `BLADE_RESTRICT` / `BLADE_IVDEP` and never the bare
// GCC keyword or pragma.
//
// THE PATTERN (see docs/plan-cpp-perf-exploitation.md, "Round D completion"):
// all compiler-specific syntax in EMITTED code goes through a macro defined
// here. Codegen emits only macro spellings; a new toolchain is a new `#elif`
// branch in this file and zero codegen changes.
//
// WHY THIS EXISTS: Phases 1 and 3 were written against g++ and emitted
// `__restrict__` on peeled row/pool pointers plus `#pragma GCC ivdep` on the
// innermost header. `__restrict__` is not an MSVC keyword, so cl.exe parses
//     double* __restrict__ __fp_R = pool_base(R.data);
// as a declaration of a POINTER VARIABLE NAMED `__restrict__`, then chokes on
// the identifier that follows:
//     error C2373: '__restrict__': redefinition; different type modifiers
//     error C2146: syntax error: missing ';' before identifier '__fp_R'
// and `#pragma GCC ivdep` is `warning C4068: unknown pragma`. Every generated
// program was therefore uncompilable by MSVC, which is what the Windows CUDA
// path uses: Build.compileCudaSplit drives cl.exe through nvcc for BOTH halves
// (one ABI across the two objects), so the host `.cpp` -- the oracle half of
// every cuda differential -- never built and `blade test cuda`'s kernel block
// failed wholesale on Windows. g++ paths were unaffected, which is why this
// survived: nothing on Linux, and no CPU test on Windows (they use g++ too),
// ever compiled a generated `.cpp` with cl.
#pragma once

// ---------------------------------------------------------------------------
// BLADE_RESTRICT -- restrict qualifier on a pointer declaration.
//
//  * nvcc's DEVICE pass accepts `__restrict__` regardless of host compiler, and
//    on Windows it also has _MSC_VER defined -- so __CUDA_ARCH__ is tested
//    FIRST, otherwise device code would get MSVC's spelling. (No emitted `.cu`
//    includes this header today; the ordering is the rule, not a live fix.)
//  * MSVC spells it `__restrict` and defines neither __GNUC__ nor __clang__
//    (clang-cl defines both, and takes the GCC spelling below).
//  * An unknown compiler gets nothing. restrict is a pure optimization hint;
//    dropping it costs speed, never correctness.
//
// `#ifndef`-guarded so an explicit `-DBLADE_RESTRICT=...` on a build line wins
// silently rather than colliding; `#pragma once` already makes the header
// itself idempotent.
#ifndef BLADE_RESTRICT
  #if defined(__CUDA_ARCH__)
    #define BLADE_RESTRICT __restrict__
  #elif defined(_MSC_VER) && !defined(__clang__)
    #define BLADE_RESTRICT __restrict
  #elif defined(__GNUC__) || defined(__clang__)
    #define BLADE_RESTRICT __restrict__
  #else
    #define BLADE_RESTRICT
  #endif
#endif

// ---------------------------------------------------------------------------
// BLADE_IVDEP -- "this loop has no loop-carried dependence", emitted on the
// line immediately before a `for` header and nowhere else (an OpenMP construct
// rejects any intervening line between itself and its loop, so at most one of
// the two ever lands on a given header).
//
// It MUST carry `_Pragma` rather than be `#ifdef`'d at the emission site: a
// `#pragma` line cannot be produced by a macro, and `_Pragma` is the only form
// that can. Under GCC it destringizes to exactly the `#pragma GCC ivdep` that
// was emitted before, so preprocessed g++ output is token-identical.
//
// GCC-only on purpose. clang does not implement `GCC ivdep` (its spelling is
// `#pragma clang loop vectorize(assume_safety)`) and MSVC does not either
// (C4068); both already ignored the raw emission, so an empty expansion
// preserves exactly the behaviour they had, minus the warning. Wiring up the
// clang spelling would be a behaviour change, not a port, so it is left to
// whoever measures it -- which is why the two guards differ.
//
// The proof obligation that licenses the assertion is discharged at the
// emission site (CodeGen.fs `ivdepEligible` and the flat-elementwise fast
// path), not here -- this header only picks the spelling.
#ifndef BLADE_IVDEP
  #if defined(__GNUC__) && !defined(__clang__)
    #define BLADE_IVDEP _Pragma("GCC ivdep")
  #else
    #define BLADE_IVDEP
  #endif
#endif

// ---------------------------------------------------------------------------
// BLADE_OMP_PARALLEL_FOR / BLADE_OMP_PARALLEL_FOR_DYNAMIC -- "thread the loop
// that immediately follows", for the INTRINSIC emitters (gram / matmul).
//
// WHY A MACRO AND NOT `#pragma omp parallel for`. The loop-nest emitter can
// write a real `#pragma` line because its output is always statement text on
// its own line. The intrinsic materializers cannot: `materializeInlineForm`
// SPACE-JOINS a form's lines into a single-line IIFE at expression positions
// (the same hazard that forces `/* */` over `//` in those emitters, see
// CodeGen.fs materializeGramForm). A `#pragma` line cannot survive that join,
// and a `#pragma` cannot come out of a macro either -- `_Pragma` is the only
// spelling that is both an expression-position-safe single token sequence and
// a pragma. Same argument as BLADE_IVDEP above.
//
// GATED ON `_OPENMP`, not on a compiler. Build.compileCppWithExtra always
// passes `-fopenmp`, and `#pragma omp` needs no header (only the omp_* RUNTIME
// API does, which is why CodeGen appends `#include <omp.h>` on its own separate
// gate and why these two macros need no include at all). But a generated `.cpp`
// compiled BY HAND without `-fopenmp` must still build, and an unrecognized
// `omp` pragma is a warning (or an error under -Werror) rather than silence --
// so the guard is the standard `_OPENMP` feature macro the compiler defines
// exactly when OpenMP is enabled. Off => empty expansion => serial, correct.
//
// The DYNAMIC variant is for a loop whose per-iteration work shrinks with the
// index (gram's same-array triangular arm); the plain variant is for a balanced
// one. Which is which is decided at the emission site, as with ivdep.
//
// SOUNDNESS is discharged at the emission site, never here: threading the outer
// loop of gram/matmul partitions OUTPUT ROWS, which are disjoint, and leaves
// every output cell's summation order untouched -- it is not a reassociation,
// so the interpreter byte-identity differentials are unaffected.
#ifndef BLADE_OMP_PARALLEL_FOR
  #if defined(_OPENMP)
    #define BLADE_OMP_PARALLEL_FOR _Pragma("omp parallel for")
  #else
    #define BLADE_OMP_PARALLEL_FOR
  #endif
#endif

#ifndef BLADE_OMP_PARALLEL_FOR_DYNAMIC
  #if defined(_OPENMP)
    #define BLADE_OMP_PARALLEL_FOR_DYNAMIC _Pragma("omp parallel for schedule(dynamic)")
  #else
    #define BLADE_OMP_PARALLEL_FOR_DYNAMIC
  #endif
#endif

// ---------------------------------------------------------------------------
// BLADE_OMP_SIMD_REDUCTION(spec) -- "vectorize the loop that immediately
// follows, accumulating into `acc` through `op`", where `spec` is an OpenMP
// reduction-spec such as `+:__ps`. This is the BLADE_FP_REASSOC accumulation
// form (CodeGen.fs `fpReassocSimdStmts`): the knob's licence to reassociate a
// serial floating-point accumulation chain, spent by handing the chain to the
// vectorizer instead of by hand-interleaving named lane accumulators.
//
// NO THREADS. `omp simd` is a SIMD construct, not a worksharing one: it creates
// no team, so it is legal inside an already-parallel region (where the emitters
// routinely land -- the comm-triangular covariance loop) and adds nothing to a
// serial one but vector width.
//
// PARAMETERIZED, so unlike the two macros above it cannot be a fixed _Pragma:
// the operator and the accumulator's name are both emission-site facts. The
// two-step stringize is the standard idiom (`_Pragma` takes a string literal;
// `#spec` makes one out of the argument's tokens). `spec` must therefore be
// comma-free, which every `<op>:<name>` reduction-spec is.
//
// GATED ON OpenMP 4.0 (`_OPENMP >= 201307`), not merely on `_OPENMP`: `simd`
// entered the standard in 4.0, and MSVC's `/openmp` is still 2.0 (`_OPENMP` =
// 200203), where `#pragma omp simd` is an unknown-pragma diagnostic. Under a
// pre-4.0 OpenMP -- or none -- the expansion is EMPTY and the loop below runs
// as the ordinary serial accumulation, which is correct, just unvectorized.
// Same argument as BLADE_IVDEP: the header picks the spelling, the emission
// site discharges the licence (`foldReorderLicensed` + the knob).
#ifndef BLADE_OMP_SIMD_REDUCTION
  #if defined(_OPENMP) && (_OPENMP >= 201307)
    #define BLADE_OMP_SIMD_PRAGMA_(x) _Pragma(#x)
    #define BLADE_OMP_SIMD_REDUCTION(spec) BLADE_OMP_SIMD_PRAGMA_(omp simd reduction(spec))
  #else
    #define BLADE_OMP_SIMD_REDUCTION(spec)
  #endif
#endif

// ---------------------------------------------------------------------------
// BLADE_REPRO_FN -- the `where repro` discharge on an emitted function
// definition: the body's floating-point operation sequence is the source's
// (no FMA contraction), and the function is never re-inlined into a caller
// compiled under the contraction licence (`-ffp-contract=fast` on the
// shipping flag line), where the caller's licence would re-fuse it.
//
// GCC carries per-function optimization overrides through the `optimize`
// attribute; measured on ucrt64 g++ at -O3 -march=native -ffp-contract=fast:
// the attributed twin of `a + b * c` emits mul+add where the bare one emits
// vfmadd. Clang ignores `optimize` (noinline still holds; the memcheck
// profile compiles Debug, where contraction is off anyway) and MSVC gets
// `__declspec(noinline)` (the MSVC lane exists for CUDA hosts; repro x cuda
// is refused upstream). The emission site is genFuncDef/genForwardDecls; the
// licence-side vetoes (fold reorder, BLAS routing) are the compiler's.
#ifndef BLADE_REPRO_FN
  #if defined(__clang__)
    #define BLADE_REPRO_FN __attribute__((noinline))
  #elif defined(__GNUC__)
    #define BLADE_REPRO_FN __attribute__((noinline, optimize("-ffp-contract=off")))
  #elif defined(_MSC_VER)
    #define BLADE_REPRO_FN __declspec(noinline)
  #else
    #define BLADE_REPRO_FN
  #endif
#endif
