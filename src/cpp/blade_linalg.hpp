#pragma once
// blade_linalg.hpp
// Blade DSL Runtime Support Library -- dense linear-algebra dispatch layer.
//
// Every dense contraction the compiler recognises (`gram`, `matmul`, `dot`, `gemv`) emits ONE call into this header
// instead of an inline loop nest or an inline `cblas_*` call.
//
// THIS HEADER IS BLAS-ONLY. IT HAS NO NATIVE FALLBACKS. It compiles ONLY under `-DBLADE_HAS_BLAS`; the guard below
// makes that a hard error rather than a silent divergence. That is architecture, not a limitation:
//
//   * The Blade compiler knows AT ITS OWN COMPILE TIME whether BLAS will be available
//     (`LinAlgPatterns.blasAvailable`, the single gate, shared with Build.fs). When it is not, no route is classified
//     as routed, so no call into this header is emitted and the `#include` is never written.
//   * The native math therefore comes from Blade's PRE-EXISTING emission paths: gram's and matmul's own scalar loops,
//     and for dot/gemv the ordinary loop-nest emitters -- the paths the interpreter and pinned-oracle differentials
//     have always covered.
//   * So byte-identity between "BLAS off" and the interpreter is STRUCTURAL -- there is exactly one copy of the
//     native arithmetic in the whole system. Hand-written fallbacks here would be a SECOND copy, and their agreement
//     would be an obligation maintained by discipline rather than by construction.
//
// Seeing this file's `#error` means an emit and a compile disagreed about the gate -- e.g. a `.cpp` emitted with BLAS
// on, compiled somewhere with it off. Failing loudly is the intent.
//
// BLAS is explicitly ALLOWED to differ in the last ULP, which is why the gate is default-OFF and why Blade's own
// loops remain the verification truth.
//
// LAYOUT / STAGING -- see blade_linalg_views.hpp. The contiguity probe (`row_major_base`) and the two staging views
// live in `blade_linalg_views.hpp`, which this header includes: pure pointer logic with no BLAS dependency, split out
// so `cpp/linalg_probe_tests.cpp` can exercise them on machines with no BLAS at all, while every ENTRY POINT below
// stays behind the guard. That header also carries the LAYOUT CONTRACT and the rectangular-rows PRECONDITION.
//
// Each adapter below therefore takes, per skeleton operand, that operand's allocated pool leaf count. The emitter
// supplies it (CodeGen's `denseCellCountExpr`) because nothing reachable from a `double**` can.
//
// SCOPE -- one entry point per (route x precision). Every entry point is named `blade_<route>_<p>` with p in
// {s, d, c, z}, and each contains exactly ONE cblas call of the matching letter. The name is chosen by
// `LinAlgPatterns.shimEntryPoint`, which appends the letter from the classified `Precision` -- so the EMITTED C++
// names the exact routine family and width, and a mis-dispatch is visible in generated text. That matters because it
// is the ONLY place it would be visible: the values agree to a ULP whichever routine ran.
//
//   L1  blade_dot_{s,d,c,z}            -- complex is dotu, NEVER dotc
//   L2  blade_gemv_{s,d,c,z}           -- CblasNoTrans at every precision
//   L2  blade_gemv_t_{s,d,c,z}         -- real Trans, COMPLEX **ConjTrans** (gram_apply's first half, t = B^H x)
//   L2  blade_symv_{s,d}               -- packed; layout proven, surface pending
//   L3  blade_gram_same_{s,d,c,z}      -- real syrk, COMPLEX **herk**
//   L3  blade_gram_distinct_{s,d,c,z}  -- real Trans, COMPLEX **ConjTrans**
//   L3  blade_matmul_{s,d,c,z}         -- plain product; only `d` reachable today
//
// Still out of scope: nrm2 (no sqrt-shape matching), axpy/scal (deliberately NATIVE -- bandwidth-bound, the flat loop
// already vectorises them), a CONJUGATING inner product (a different operation needing its own surface form), and LAPACK.

#include "blade_linalg_views.hpp"

#ifndef BLADE_HAS_BLAS
#error "blade_linalg.hpp requires -DBLADE_HAS_BLAS: the Blade compiler emits native loops when BLAS is unavailable; this call should not have been emitted"
#endif

#include <cstddef>
#include <vector>
#include <complex>
// CBLAS header flavor. `-DBLADE_BLAS_MKL` is emitted by Build.fs when BLADE_BLAS_FLAVOR=mkl (the define is added by
// LinAlgPatterns.blasBuildFlags). MKL implements the standard CBLAS interface -- same functions, same enums, same
// semantics, so nothing below changes -- but ships it under its own header names rather than the netlib ones.
#if defined(BLADE_BLAS_MKL)
#include <mkl_cblas.h>
#else
#include <cblas.h>
#endif

namespace blade_linalg {

    // The four blocks below are deliberately spelled out rather than generated from one template. cblas's signatures
    // are NOT uniform across precisions: `herk` takes REAL alpha/beta while `gemm` takes complex ones BY POINTER,
    // `sdot` returns by value while `zdotu` writes through an out parameter (the `_sub` form), and the complex arrays
    // cross as `void*`. A template would special-case all of that anyway -- and the special cases are exactly where a
    // wrong binding would hide.

    // L1 -- dot.  s = seed + x . y
    //
    // The SEED is a parameter rather than an assumed zero because the call replaces `reduce`'s fused fold, whose
    // accumulator starts at the `(+)` identity 0 OR at a user `init` (`reduce(c, op, init)`). The gate-off path --
    // Blade's own fold nest -- starts from the same value, so the two arms agree on what is being computed and differ
    // only in association order, which is the whole point of calling BLAS.
    //
    // Operands are the pointers the loop path subscripts, not copies: a rank-1 Blade array's `.data` IS its pool, and
    // `Array<T,1>::operator[]` is `data[i]`.
    //
    // COMPLEX IS `dotu`, NEVER `dotc`. The Blade shape this route matches is `reduce(zip(x, y) under *, (+))`, whose
    // kernel is `a * b` -- no conjugation appears anywhere in it. `zdotc` computes sum(conj(x_i)*y_i) and would
    // silently return a DIFFERENT NUMBER; most visibly for `dot(x, x)`, where dotc gives the real squared norm and
    // Blade's fold gives the complex sum of x_i^2. A conjugating inner product is a different operation needing its
    // own surface form (a `prodsum` with an explicit `conj`) -- deliberately NOT here.

    inline float blade_dot_s(size_t n,
                             const float* BLADE_RESTRICT x,
                             const float* BLADE_RESTRICT y,
                             float seed) {
        return seed + cblas_sdot((blasint)n, x, 1, y, 1);
    }

    inline double blade_dot_d(size_t n,
                              const double* BLADE_RESTRICT x,
                              const double* BLADE_RESTRICT y,
                              double seed) {
        return seed + cblas_ddot((blasint)n, x, 1, y, 1);
    }

    inline std::complex<float> blade_dot_c(size_t n,
                                           const std::complex<float>* x,
                                           const std::complex<float>* y,
                                           std::complex<float> seed) {
        std::complex<float> r(0.0f, 0.0f);
        // `_sub` form: the value-returning `cblas_cdotu` is not portable across cblas implementations (struct-return ABI); the out-parameter form is.
        cblas_cdotu_sub((blasint)n, x, 1, y, 1, &r);
        return seed + r;
    }

    inline std::complex<double> blade_dot_z(size_t n,
                                            const std::complex<double>* x,
                                            const std::complex<double>* y,
                                            std::complex<double> seed) {
        std::complex<double> r(0.0, 0.0);
        cblas_zdotu_sub((blasint)n, x, 1, y, 1, &r);
        return seed + r;
    }

    // L2 -- gemv.  y(m) = A(m x n) * x(n), A as a Blade ROW SKELETON
    //
    // Replaces a per-row apply whose kernel body is `prodsum(row, x)`; when the gate is off, that nest is emitted as
    // it always was (rows ascending, one accumulator per row seeded +0, contracted axis ascending through `A.data[i][t]`).
    //
    // `prodsum` CONJUGATES NOTHING, so every precision -- complex included -- uses CblasNoTrans. There is nothing to
    // conjugate: this is a plain contraction of A's row against x.
    //
    // BLAS needs contiguous storage, so each runs the contiguity PROBE and stages only if it refuses. `Acells` is A's
    // allocated pool leaf count; a fresh dense Blade pool always passes, so the common case stages nothing. `x` and
    // `y` are rank-1 pools handed over directly -- no skeleton, no probe.

    inline void blade_gemv_s(size_t m, size_t n,
                             float** Arows, size_t Acells,
                             const float* BLADE_RESTRICT x,
                             float* BLADE_RESTRICT y) {
        in_view<float> A(Arows, m, n, Acells);
        cblas_sgemv(CblasRowMajor, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0f, A.p, (blasint)n, x, 1, 0.0f, y, 1);
    }

    inline void blade_gemv_d(size_t m, size_t n,
                             double** Arows, size_t Acells,
                             const double* BLADE_RESTRICT x,
                             double* BLADE_RESTRICT y) {
        in_view<double> A(Arows, m, n, Acells);
        cblas_dgemv(CblasRowMajor, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0, A.p, (blasint)n, x, 1, 0.0, y, 1);
    }

    inline void blade_gemv_c(size_t m, size_t n,
                             std::complex<float>** Arows, size_t Acells,
                             const std::complex<float>* x,
                             std::complex<float>* y) {
        in_view<std::complex<float>> A(Arows, m, n, Acells);
        const std::complex<float> alpha(1.0f, 0.0f), beta(0.0f, 0.0f);
        cblas_cgemv(CblasRowMajor, CblasNoTrans,
                    (blasint)m, (blasint)n, &alpha, A.p, (blasint)n, x, 1, &beta, y, 1);
    }

    inline void blade_gemv_z(size_t m, size_t n,
                             std::complex<double>** Arows, size_t Acells,
                             const std::complex<double>* x,
                             std::complex<double>* y) {
        in_view<std::complex<double>> A(Arows, m, n, Acells);
        const std::complex<double> alpha(1.0, 0.0), beta(0.0, 0.0);
        cblas_zgemv(CblasRowMajor, CblasNoTrans,
                    (blasint)m, (blasint)n, &alpha, A.p, (blasint)n, x, 1, &beta, y, 1);
    }

    // L2 -- gemv, TRANSPOSED.  t(n) = A(m x n)^H * x(m), A as a Blade ROW SKELETON
    //
    // The first half of `gram_apply(A, B, x)` (t = B^H x). Real is CblasTrans, COMPLEX is CblasConjTrans -- the
    // conjugation Blade's scalar loop applies (`conj_scalar(B[j][k]) * x[j]`), the same flag logic as the distinct
    // gram's gemm. Same staging contract as gemv above; `x` has m cells and `t` has n.

    inline void blade_gemv_t_s(size_t m, size_t n,
                               float** Arows, size_t Acells,
                               const float* BLADE_RESTRICT x,
                               float* BLADE_RESTRICT t) {
        in_view<float> A(Arows, m, n, Acells);
        cblas_sgemv(CblasRowMajor, CblasTrans,
                    (blasint)m, (blasint)n, 1.0f, A.p, (blasint)n, x, 1, 0.0f, t, 1);
    }

    inline void blade_gemv_t_d(size_t m, size_t n,
                               double** Arows, size_t Acells,
                               const double* BLADE_RESTRICT x,
                               double* BLADE_RESTRICT t) {
        in_view<double> A(Arows, m, n, Acells);
        cblas_dgemv(CblasRowMajor, CblasTrans,
                    (blasint)m, (blasint)n, 1.0, A.p, (blasint)n, x, 1, 0.0, t, 1);
    }

    inline void blade_gemv_t_c(size_t m, size_t n,
                               std::complex<float>** Arows, size_t Acells,
                               const std::complex<float>* x,
                               std::complex<float>* t) {
        in_view<std::complex<float>> A(Arows, m, n, Acells);
        const std::complex<float> alpha(1.0f, 0.0f), beta(0.0f, 0.0f);
        cblas_cgemv(CblasRowMajor, CblasConjTrans,
                    (blasint)m, (blasint)n, &alpha, A.p, (blasint)n, x, 1, &beta, t, 1);
    }

    inline void blade_gemv_t_z(size_t m, size_t n,
                               std::complex<double>** Arows, size_t Acells,
                               const std::complex<double>* x,
                               std::complex<double>* t) {
        in_view<std::complex<double>> A(Arows, m, n, Acells);
        const std::complex<double> alpha(1.0, 0.0), beta(0.0, 0.0);
        cblas_zgemv(CblasRowMajor, CblasConjTrans,
                    (blasint)m, (blasint)n, &alpha, A.p, (blasint)n, x, 1, &beta, t, 1);
    }

    // L2 -- symv.  y(n) = A(n x n) * x(n) for SYMMETRIC A in PACKED storage
    //
    // LAYOUT (verified empirically against the real allocator). Blade's rank-2 sym-compact pool, produced by
    // `allocate<T**, {1,1}>`, holds row i's logical entries j = i..n-1 contiguously, rows ascending:
    //
    //     ap = A(0,0) A(0,1) ... A(0,n-1) A(1,1) ... A(1,n-1) ... A(n-1,n-1)
    //
    // which is EXACTLY cblas row-major UPPER packed order, cell for cell, with the diagonal included and no dead
    // entries. Checked for n = 1..7: pool order, count_leaves cardinality n(n+1)/2, and the row offsets
    // i*n - i(i-1)/2 all agree. So this route needs ZERO staging -- `pool_base(S.data)` is a valid AP argument as is.
    //
    // NO EMISSION SITE YET, a statement about the SURFACE, not a gap here: Blade cannot currently express a
    // sym-compact matvec. Peeling a rank-2 compact group into rank-1 fibers is refused at typecheck (BL4004), and
    // reduce/prodsum over compact storage is refused for the canonical-vs-mirrored fold ambiguity. `decompact` first,
    // and the operand is dense -- which is the gemv route above.
    //
    // ONLY THE REAL INSTANCES EXIST. A complex packed matrix Blade would deduce here is HERMITIAN storage (same
    // reasoning as gram-same below), whose routine is `hpmv`; complex-symmetric packed (`zspmv`) is not in reference
    // BLAS at all. The complex packed matvec arrives with the surface form that produces one.

    inline void blade_symv_s(size_t n, const float* BLADE_RESTRICT ap,
                             const float* BLADE_RESTRICT x, float* BLADE_RESTRICT y) {
        cblas_sspmv(CblasRowMajor, CblasUpper, (blasint)n, 1.0f, ap, x, 1, 0.0f, y, 1);
    }

    inline void blade_symv_d(size_t n, const double* BLADE_RESTRICT ap,
                             const double* BLADE_RESTRICT x, double* BLADE_RESTRICT y) {
        cblas_dspmv(CblasRowMajor, CblasUpper, (blasint)n, 1.0, ap, x, 1, 0.0, y, 1);
    }

    // L3 -- gram(A, A).  One triangle into Blade's PACKED upper storage
    //
    // Row i holds the logical entries j = i..m-1 at `C[i][j - i]`; the lower triangle is recovered lazily on read, so
    // only the triangle is computed.
    //
    //   REAL    : C = A * A^T, a SYMMETRIC rank-k update (ssyrk / dsyrk)
    //   COMPLEX : C = A * A^H, a HERMITIAN rank-k update (cherk / zherk)
    //
    // THE COMPLEX CASE IS HERMITIAN, AND THAT IS NOT A NAMING CHOICE. Blade's own scalar loop for a complex gram
    // accumulates acc += A[i][k] * conj_scalar(A[j][k]) -- conjugating the SECOND factor -- which is A * A^H, exactly
    // what herk computes, agreeing with the Hermitian storage class Blade deduces for the result. Binding this to
    // zsyrk (the name-symmetric choice) would silently compute a different matrix. herk also takes REAL alpha/beta,
    // unlike gemm.
    //
    // C IS STAGED in every arm, unavoidably: syrk/herk want a dense square C with a leading dimension, so a full
    // m x m buffer is written and then repacked into Blade's left-justified triangle. The gate-off path never gets
    // here -- it writes the packed triangle directly from materializeGramForm's own loops, with no staging at all.
    //
    // `Acells` is A's allocated pool leaf count. C needs none: the packed triangle is written through `Crows[i][jr]`
    // with jr < m-i, the packed row's own length, so C never goes through a view.

    inline void blade_gram_same_s(size_t m, size_t n, float** Arows, size_t Acells,
                                  float** Crows) {
        in_view<float> A(Arows, m, n, Acells);
        std::vector<float> Cfull(m * m);
        cblas_ssyrk(CblasRowMajor, CblasUpper, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0f, A.p, (blasint)n,
                    0.0f, Cfull.data(), (blasint)m);
        for (size_t i = 0; i < m; i++)
            for (size_t jr = 0; jr < m - i; jr++)
                Crows[i][jr] = Cfull[i * m + i + jr];
    }

    inline void blade_gram_same_d(size_t m, size_t n, double** Arows, size_t Acells,
                                  double** Crows) {
        in_view<double> A(Arows, m, n, Acells);
        std::vector<double> Cfull(m * m);
        cblas_dsyrk(CblasRowMajor, CblasUpper, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0, A.p, (blasint)n,
                    0.0, Cfull.data(), (blasint)m);
        for (size_t i = 0; i < m; i++)
            for (size_t jr = 0; jr < m - i; jr++)
                Crows[i][jr] = Cfull[i * m + i + jr];
    }

    inline void blade_gram_same_c(size_t m, size_t n, std::complex<float>** Arows,
                                  size_t Acells, std::complex<float>** Crows) {
        in_view<std::complex<float>> A(Arows, m, n, Acells);
        std::vector<std::complex<float>> Cfull(m * m);
        // cherk: REAL alpha/beta -- a Hermitian update cannot scale by a complex number and stay Hermitian.
        cblas_cherk(CblasRowMajor, CblasUpper, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0f, A.p, (blasint)n,
                    0.0f, Cfull.data(), (blasint)m);
        for (size_t i = 0; i < m; i++)
            for (size_t jr = 0; jr < m - i; jr++)
                Crows[i][jr] = Cfull[i * m + i + jr];
    }

    inline void blade_gram_same_z(size_t m, size_t n, std::complex<double>** Arows,
                                  size_t Acells, std::complex<double>** Crows) {
        in_view<std::complex<double>> A(Arows, m, n, Acells);
        std::vector<std::complex<double>> Cfull(m * m);
        cblas_zherk(CblasRowMajor, CblasUpper, CblasNoTrans,
                    (blasint)m, (blasint)n, 1.0, A.p, (blasint)n,
                    0.0, Cfull.data(), (blasint)m);
        for (size_t i = 0; i < m; i++)
            for (size_t jr = 0; jr < m - i; jr++)
                Crows[i][jr] = Cfull[i * m + i + jr];
    }

    // L3 -- gram(A, B), distinct.  A is m x n, B is p x n, C dense m x p
    //
    //   REAL    : C = A * B^T   (transB = CblasTrans)
    //   COMPLEX : C = A * B^H   (transB = CblasConjTrans)
    //
    // The conjugation is not an embellishment. Blade's scalar loop for a complex distinct gram is
    // acc += A[i][k] * conj_scalar(B[j][k]), i.e. sum_k A[i][k]*conj(B[j][k]) = (A * conj(B)^T)[i][j] = (A * B^H)[i][j].
    // CblasConjTrans is the flag that reproduces it; plain CblasTrans would drop the conjugation and compute a
    // different matrix.
    //
    // The `*cells` arguments are each operand's allocated pool leaf count.

    inline void blade_gram_distinct_s(size_t m, size_t n, size_t p,
                                      float** Arows, size_t Acells,
                                      float** Brows, size_t Bcells,
                                      float** Crows, size_t Ccells) {
        in_view<float> A(Arows, m, n, Acells);
        in_view<float> B(Brows, p, n, Bcells);
        out_view<float> C(Crows, m, p, Ccells);
        cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasTrans,
                    (blasint)m, (blasint)p, (blasint)n,
                    1.0f, A.p, (blasint)n, B.p, (blasint)n, 0.0f, C.p, (blasint)p);
        C.flush();
    }

    inline void blade_gram_distinct_d(size_t m, size_t n, size_t p,
                                      double** Arows, size_t Acells,
                                      double** Brows, size_t Bcells,
                                      double** Crows, size_t Ccells) {
        in_view<double> A(Arows, m, n, Acells);
        in_view<double> B(Brows, p, n, Bcells);
        out_view<double> C(Crows, m, p, Ccells);
        cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasTrans,
                    (blasint)m, (blasint)p, (blasint)n,
                    1.0, A.p, (blasint)n, B.p, (blasint)n, 0.0, C.p, (blasint)p);
        C.flush();
    }

    inline void blade_gram_distinct_c(size_t m, size_t n, size_t p,
                                      std::complex<float>** Arows, size_t Acells,
                                      std::complex<float>** Brows, size_t Bcells,
                                      std::complex<float>** Crows, size_t Ccells) {
        in_view<std::complex<float>> A(Arows, m, n, Acells);
        in_view<std::complex<float>> B(Brows, p, n, Bcells);
        out_view<std::complex<float>> C(Crows, m, p, Ccells);
        const std::complex<float> alpha(1.0f, 0.0f), beta(0.0f, 0.0f);
        cblas_cgemm(CblasRowMajor, CblasNoTrans, CblasConjTrans,
                    (blasint)m, (blasint)p, (blasint)n,
                    &alpha, A.p, (blasint)n, B.p, (blasint)n, &beta, C.p, (blasint)p);
        C.flush();
    }

    inline void blade_gram_distinct_z(size_t m, size_t n, size_t p,
                                      std::complex<double>** Arows, size_t Acells,
                                      std::complex<double>** Brows, size_t Bcells,
                                      std::complex<double>** Crows, size_t Ccells) {
        in_view<std::complex<double>> A(Arows, m, n, Acells);
        in_view<std::complex<double>> B(Brows, p, n, Bcells);
        out_view<std::complex<double>> C(Crows, m, p, Ccells);
        const std::complex<double> alpha(1.0, 0.0), beta(0.0, 0.0);
        cblas_zgemm(CblasRowMajor, CblasNoTrans, CblasConjTrans,
                    (blasint)m, (blasint)p, (blasint)n,
                    &alpha, A.p, (blasint)n, B.p, (blasint)n, &beta, C.p, (blasint)p);
        C.flush();
    }

    // L3 -- matmul(A, B) = A * B.  A is m x k, B is k x n, C dense m x n
    //
    // The `math.matmul` intrinsic's one emission target. NO transposes and NO conjugation at any precision: matmul is
    // the plain product.
    //
    // ONLY THE `d` INSTANCE IS REACHABLE TODAY. `TypeCheck.inferMatmul` requires Float64 elements at the SURFACE
    // (BL3999), so an f32/complex call is rejected before codegen. The other three exist because widening matmul's
    // surface is a LANGUAGE decision (what should a complex `matmul(A, B)` mean -- A*B, or A*B^H?), and when that
    // decision is taken the binding should already be here, already plain-product.

    inline void blade_matmul_s(size_t m, size_t k, size_t n,
                               float** Arows, size_t Acells,
                               float** Brows, size_t Bcells,
                               float** Crows, size_t Ccells) {
        in_view<float> A(Arows, m, k, Acells);
        in_view<float> B(Brows, k, n, Bcells);
        out_view<float> C(Crows, m, n, Ccells);
        cblas_sgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                    (blasint)m, (blasint)n, (blasint)k,
                    1.0f, A.p, (blasint)k, B.p, (blasint)n, 0.0f, C.p, (blasint)n);
        C.flush();
    }

    inline void blade_matmul_d(size_t m, size_t k, size_t n,
                               double** Arows, size_t Acells,
                               double** Brows, size_t Bcells,
                               double** Crows, size_t Ccells) {
        in_view<double> A(Arows, m, k, Acells);
        in_view<double> B(Brows, k, n, Bcells);
        out_view<double> C(Crows, m, n, Ccells);
        cblas_dgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                    (blasint)m, (blasint)n, (blasint)k,
                    1.0, A.p, (blasint)k, B.p, (blasint)n, 0.0, C.p, (blasint)n);
        C.flush();
    }

    inline void blade_matmul_c(size_t m, size_t k, size_t n,
                               std::complex<float>** Arows, size_t Acells,
                               std::complex<float>** Brows, size_t Bcells,
                               std::complex<float>** Crows, size_t Ccells) {
        in_view<std::complex<float>> A(Arows, m, k, Acells);
        in_view<std::complex<float>> B(Brows, k, n, Bcells);
        out_view<std::complex<float>> C(Crows, m, n, Ccells);
        const std::complex<float> alpha(1.0f, 0.0f), beta(0.0f, 0.0f);
        cblas_cgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                    (blasint)m, (blasint)n, (blasint)k,
                    &alpha, A.p, (blasint)k, B.p, (blasint)n, &beta, C.p, (blasint)n);
        C.flush();
    }

    inline void blade_matmul_z(size_t m, size_t k, size_t n,
                               std::complex<double>** Arows, size_t Acells,
                               std::complex<double>** Brows, size_t Bcells,
                               std::complex<double>** Crows, size_t Ccells) {
        in_view<std::complex<double>> A(Arows, m, k, Acells);
        in_view<std::complex<double>> B(Brows, k, n, Bcells);
        out_view<std::complex<double>> C(Crows, m, n, Ccells);
        const std::complex<double> alpha(1.0, 0.0), beta(0.0, 0.0);
        cblas_zgemm(CblasRowMajor, CblasNoTrans, CblasNoTrans,
                    (blasint)m, (blasint)n, (blasint)k,
                    &alpha, A.p, (blasint)k, B.p, (blasint)n, &beta, C.p, (blasint)n);
        C.flush();
    }

} // namespace blade_linalg
