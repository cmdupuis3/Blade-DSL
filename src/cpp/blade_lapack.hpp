#pragma once
// blade_lapack.hpp
// Blade DSL Runtime Support Library -- LAPACK dispatch layer: the dense/packed eigensolvers and the general
// linear solve. Same architecture as `blade_linalg.hpp`, one level up.
//
// THIS HEADER IS LAPACK-ONLY. IT HAS NO NATIVE FALLBACKS. It compiles ONLY under `-DBLADE_HAS_LAPACK`; the guard
// below makes that a hard error rather than a silent divergence: the Blade compiler knows at ITS compile time
// whether LAPACK will be available (`LinAlgPatterns.lapackAvailable`), and when it is not it does not emit a call --
// the math package's SYNTHESIZED Jacobi source runs instead. That synthesized source is the single copy of the
// native math and the one the interpreter and pinned-oracle differentials cover, so a hand-written fallback here
// would be a second copy whose agreement is maintained by discipline rather than by construction.
//
// `blade_solve_d` IS THE EXCEPTION TO THE PARAGRAPH ABOVE, and is documented as such at its own definition: its
// native counterpart is a loop nest CODEGEN EMITS, not source the elaborator declines to synthesize, so declining
// that route is the ordinary default rather than an impossibility. Everything about the `#error` guard still
// applies -- a call reaching a build without `-DBLADE_HAS_LAPACK` is still a compiler bug -- only the reason the
// fallback cannot live here changes: for solve it simply lives somewhere better.
//
// NUMERICS -- LAPACK ROUTES ARE PERMANENTLY OUTSIDE BYTE-IDENTITY. Unlike the BLAS routes, which differ from Blade's
// loops only in the last ULP, an eigensolver's OUTPUT IS NOT UNIQUE: eigenvector signs are arbitrary, and within a
// degenerate eigenvalue's subspace any orthonormal basis is a correct answer. The entry points below remove the two
// determinate parts of that freedom so a gate-on build lands as close to the native path as it can --
//
//   * eigenvalues are returned DESCENDING (LAPACK produces ascending), and
//   * each eigenvector is sign-fixed so the component of largest magnitude is positive (real) / has positive real
//     part (complex),
//
// which are exactly the conventions `MathDecls.eighDecl` documents. What CANNOT be normalised away is the basis
// choice inside a degenerate subspace. So: gate-on results are correct but not bit-reproducible against the native
// path, and `interp`/`diff-oracle` must never run with the LAPACK gate on.
//
// LAYOUT -- SELF-DUALITY, MEASURED (the zero-conversion argument). Blade's rank-2 sym-compact pool is row-major UPPER
// packed. LAPACK is column-major. The bridge is a duality that costs nothing:
//
//   REAL: position k of COL-MAJOR-LOWER packed holds A(j,i) exactly where position k of ROW-MAJOR-UPPER packed holds
//   A(i,j). For a SYMMETRIC matrix A(j,i) = A(i,j), so THE TWO SEQUENCES ARE IDENTICAL, element for element (verified
//   against the real allocator for n = 1..6). Hence `LAPACK_COL_MAJOR` + `uplo='L'` reads Blade's pool DIRECTLY -- no
//   conversion, no LAPACKE internal temporary.
//
//   COMPLEX: the same positions relate by A(j,i) = conj(A(i,j)) for a HERMITIAN matrix, so the two conventions are
//   CONJUGATES, not equal. Measured: feeding the pool verbatim to COL_MAJOR/'L' yields correct eigenvalues and
//   CONJUGATED eigenvectors (residual 4.8 against the true pairs, vs 1.8e-15 for the conjugates) -- a silent wrong
//   answer, which is exactly the trap this comment exists to close.
//
// The fix costs nothing, and that is why it is preferred to LAPACKE's row-major mode: `?spev`/`?hpev`/`?syev`/`?heev`
// all DESTROY their input, so a scratch copy is mandatory whatever the layout. The complex arms simply apply `conj`
// while making that copy. LAPACKE's `LAPACK_ROW_MAJOR` would instead allocate a second temporary and convert into it
// (measured working, residual 2.2e-15, but strictly more work for the same answer).
//
// The same duality extends to DENSE operands: a row-major n x n read as column-major is A^T, which is A for
// symmetric and conj(A) for Hermitian -- so the dense arms use the identical trick with the identical conj fix.

#include <cstddef>
#include <complex>
#include <vector>
#include <algorithm>

#ifndef BLADE_HAS_LAPACK
#error "blade_lapack.hpp requires -DBLADE_HAS_LAPACK: the Blade compiler emits its synthesized Jacobi source when LAPACK is unavailable; this call should not have been emitted"
#endif

// LAPACKE header flavor. `-DBLADE_BLAS_MKL` is emitted by Build.fs when BLADE_BLAS_FLAVOR=mkl (the define is added by
// LinAlgPatterns.blasBuildFlags). MKL implements the standard LAPACKE interface -- same functions, same layout
// constants, same semantics, so nothing below changes -- but ships it under its own header names.
#if defined(BLADE_BLAS_MKL)
#include <mkl_lapacke.h>
#else
#include <lapacke.h>
#endif

namespace blade_lapack {

    // Shared post-processing: LAPACK order/sign -> Blade's documented convention.
    //
    // LAPACK returns eigenvalues ASCENDING and eigenvectors column-major (eigenvector k occupies z[k*n .. k*n+n-1]).
    // `MathDecls.eighDecl` documents DESCENDING eigenvalues, Q's COLUMNS as the eigenvectors, and a sign fix making
    // the largest-magnitude component positive. These helpers are the translation, applied identically by every arm
    // so the four precisions cannot drift.

    /// Copy z (column-major, ascending) into Blade's row-major V with columns as eigenvectors, REVERSING the order,
    /// and sign-fix each column. `V[i][k]` = component i of the k-th eigenvector, k descending by value.
    template <typename T>
    inline void emit_vectors_desc(size_t n, const std::vector<T>& z, T** V) {
        for (size_t k = 0; k < n; k++) {
            const T* src = z.data() + (n - 1 - k) * n;   // ascending -> descending
            // Sign fix: the component of largest magnitude is made positive (for complex, given positive real part).
            size_t best = 0;
            double bigv = -1.0;
            for (size_t i = 0; i < n; i++) {
                double mag = std::abs(src[i]);
                if (mag > bigv) { bigv = mag; best = i; }
            }
            T flip = T(1);
            if (std::real(src[best]) < 0.0) flip = T(-1);
            for (size_t i = 0; i < n; i++) V[i][k] = src[i] * flip;
        }
    }

    /// Reverse LAPACK's ascending eigenvalues into Blade's descending `lam`.
    inline void emit_values_desc(size_t n, const std::vector<double>& w, double* lam) {
        for (size_t k = 0; k < n; k++) lam[k] = w[n - 1 - k];
    }
    inline void emit_values_desc(size_t n, const std::vector<float>& w, float* lam) {
        for (size_t k = 0; k < n; k++) lam[k] = w[n - 1 - k];
    }

    // eigh, PACKED operand -- the zero-conversion route.
    //
    // `ap_pool` is `pool_base(S.data)` of a rank-2 compact array: row-major upper packed, n(n+1)/2 cells. It is READ
    // ONLY here -- the scratch copy is what LAPACK destroys. Returns LAPACK's `info` (0 = converged); the caller
    // decides what a non-zero means.

    inline int blade_eigh_packed_s(size_t n, const float* ap_pool,
                                   float* lam, float** V) {
        const size_t cells = n * (n + 1) / 2;
        std::vector<float> ap(ap_pool, ap_pool + cells);     // self-dual: verbatim
        std::vector<float> w(n), z(n * n);
        lapack_int info = LAPACKE_sspev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        ap.data(), w.data(), z.data(), (lapack_int)n);
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, z, V); }
        return (int)info;
    }

    inline int blade_eigh_packed_d(size_t n, const double* ap_pool,
                                   double* lam, double** V) {
        const size_t cells = n * (n + 1) / 2;
        std::vector<double> ap(ap_pool, ap_pool + cells);    // self-dual: verbatim
        std::vector<double> w(n), z(n * n);
        lapack_int info = LAPACKE_dspev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        ap.data(), w.data(), z.data(), (lapack_int)n);
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, z, V); }
        return (int)info;
    }

    // HERMITIAN packed. The mandated scratch copy applies `conj` (see the LAYOUT note): row-major-upper of A is
    // col-major-lower of conj(A), so conjugating on the way in makes COL_MAJOR/'L' see A itself. Feeding it verbatim
    // instead returns the CONJUGATE eigenvectors -- measured, and silent.

    inline int blade_eigh_packed_c(size_t n, const std::complex<float>* ap_pool,
                                   float* lam, std::complex<float>** V) {
        const size_t cells = n * (n + 1) / 2;
        std::vector<std::complex<float>> ap(cells);
        for (size_t k = 0; k < cells; k++) ap[k] = std::conj(ap_pool[k]);
        std::vector<float> w(n);
        std::vector<std::complex<float>> z(n * n);
        lapack_int info = LAPACKE_chpev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        reinterpret_cast<lapack_complex_float*>(ap.data()),
                                        w.data(),
                                        reinterpret_cast<lapack_complex_float*>(z.data()),
                                        (lapack_int)n);
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, z, V); }
        return (int)info;
    }

    inline int blade_eigh_packed_z(size_t n, const std::complex<double>* ap_pool,
                                   double* lam, std::complex<double>** V) {
        const size_t cells = n * (n + 1) / 2;
        std::vector<std::complex<double>> ap(cells);
        for (size_t k = 0; k < cells; k++) ap[k] = std::conj(ap_pool[k]);
        std::vector<double> w(n);
        std::vector<std::complex<double>> z(n * n);
        lapack_int info = LAPACKE_zhpev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        reinterpret_cast<lapack_complex_double*>(ap.data()),
                                        w.data(),
                                        reinterpret_cast<lapack_complex_double*>(z.data()),
                                        (lapack_int)n);
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, z, V); }
        return (int)info;
    }

    // eigh, DENSE operand.
    //
    // `Arows` is a Blade dense rank-2 row skeleton, n x n, SYMMETRIC (or Hermitian) by the caller's assertion -- the
    // current `eigh` surface says "symmetry is ASSUMED, not checked", and this layer inherits that domain exactly.
    //
    // The same duality applies: a row-major n x n read as column-major is A^T, which is A for symmetric and conj(A)
    // for Hermitian. So the copy into the (mandatory) scratch is verbatim for real and conjugating for complex, and
    // COL_MAJOR/'L' then sees the intended matrix. Rows are copied through `Arows[i][j]` -- the same subscript
    // Blade's own loops use, so a staged or sliced operand contributes exactly the values the native path read.

    inline int blade_eigh_dense_s(size_t n, float** Arows, float* lam, float** V) {
        std::vector<float> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[i * n + j] = Arows[i][j];
        std::vector<float> w(n);
        lapack_int info = LAPACKE_ssyev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        a.data(), (lapack_int)n, w.data());
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, a, V); }
        return (int)info;
    }

    inline int blade_eigh_dense_d(size_t n, double** Arows, double* lam, double** V) {
        std::vector<double> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[i * n + j] = Arows[i][j];
        std::vector<double> w(n);
        lapack_int info = LAPACKE_dsyev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        a.data(), (lapack_int)n, w.data());
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, a, V); }
        return (int)info;
    }

    inline int blade_eigh_dense_c(size_t n, std::complex<float>** Arows,
                                  float* lam, std::complex<float>** V) {
        std::vector<std::complex<float>> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[i * n + j] = std::conj(Arows[i][j]);
        std::vector<float> w(n);
        lapack_int info = LAPACKE_cheev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        reinterpret_cast<lapack_complex_float*>(a.data()),
                                        (lapack_int)n, w.data());
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, a, V); }
        return (int)info;
    }

    inline int blade_eigh_dense_z(size_t n, std::complex<double>** Arows,
                                  double* lam, std::complex<double>** V) {
        std::vector<std::complex<double>> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[i * n + j] = std::conj(Arows[i][j]);
        std::vector<double> w(n);
        lapack_int info = LAPACKE_zheev(LAPACK_COL_MAJOR, 'V', 'L', (lapack_int)n,
                                        reinterpret_cast<lapack_complex_double*>(a.data()),
                                        (lapack_int)n, w.data());
        if (info == 0) { emit_values_desc(n, w, lam); emit_vectors_desc(n, a, V); }
        return (int)info;
    }

    // solve -- A.x = b through ?gesv (partial-pivoted LU, one right-hand side).
    //
    // THE ONE ENTRY POINT HERE THAT HAS A NATIVE TWIN, and the difference is
    // worth stating because everything above this line does not. The eigh arms
    // exist because the compiler declines to synthesize its Jacobi source when
    // LAPACK is present -- there is no other implementation, which is why the
    // `#error` at the top of this file is a hard failure. `solve` is the
    // opposite: `CodeGen.materializeSolveForm` emits a complete partial-pivoted
    // LU loop nest whenever this route is not taken, and that loop nest is the
    // DEFAULT (the gate is off unless asked for) and the byte-identity truth
    // the interpreter differential covers. So this function is a faster
    // replacement for working code, not the only copy of it.
    //
    // CONSEQUENCE FOR NUMERICS: the two arms agree to about 1e-14, not to the
    // ULP. Both are partial-pivoted LU and both break ties by strict `>` (the
    // native arm by construction, LAPACK through `idamax`), so they factorize
    // the SAME matrix with the SAME pivot sequence -- but ?gesv is blocked over
    // ?trsm/?gemm panels and accumulates its updates in a different order. That
    // is the ordinary BLAS-route situation, not eigh's non-uniqueness, and it
    // is why byte-identity harnesses run gate-off here exactly as they do for
    // gemm.
    //
    // LAYOUT: ?gesv DESTROYS its `a`, so a scratch copy is mandatory whatever
    // the layout -- and since it is mandatory, transposing INTO it is free.
    // `a[j * n + i] = Arows[i][j]` makes the scratch column-major, so
    // LAPACK_COL_MAJOR reads the intended matrix with no LAPACKE temporary.
    // (The self-duality trick the eigh arms use does not apply: it relies on
    // the operand being symmetric, and a general LU operand is not.)
    //
    // `b` is READ ONLY; the solution is written into `x`, which ?gesv then
    // overwrites in place with the answer. A right-hand side is layout-agnostic
    // at nrhs = 1, so no bridging is needed for it.
    //
    // Returns LAPACK's `info` (0 = success, i > 0 = U(i,i) is exactly zero, so
    // the matrix is singular; i < 0 = bad argument). THE CALLER DECIDES what a
    // non-zero means -- codegen emits the BL8007 panic, so the singular message
    // is spelled once (`CodeGen.solveSingularMessage`) and shared with the
    // native arm rather than duplicated in a header that includes no runtime.

    inline int blade_solve_d(size_t n, double** Arows, const double* b, double* x) {
        std::vector<double> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[j * n + i] = Arows[i][j];   // row-major in -> col-major scratch
        for (size_t i = 0; i < n; i++) x[i] = b[i];
        std::vector<lapack_int> ipiv(n ? n : 1);
        lapack_int info = LAPACKE_dgesv(LAPACK_COL_MAJOR, (lapack_int)n, 1,
                                        a.data(), (lapack_int)n, ipiv.data(),
                                        x, (lapack_int)n);
        return (int)info;
    }

    // lu -- the factorization KEPT: `m.lu(A)` -> (LU, piv) through ?getrf, then
    // `m.lu_solve[_t](LU, piv, b)` through ?getrs. The row-major <-> column-major
    // bridge is the solve adapter's, both ways; `piv` is stored 0-BASED (LAPACK's
    // ipiv less one) so the native twin and this arm agree on the pivot rows.
    // Within one program the factor and its solves take the same arm (the gate
    // is process-wide), so a routed factor is never consumed natively.

    inline int blade_lu_d(size_t n, double** Arows, double** LUrows, int64_t* piv) {
        std::vector<double> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[j * n + i] = Arows[i][j];
        std::vector<lapack_int> ipiv(n ? n : 1);
        lapack_int info = LAPACKE_dgetrf(LAPACK_COL_MAJOR, (lapack_int)n, (lapack_int)n,
                                         a.data(), (lapack_int)n, ipiv.data());
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) LUrows[i][j] = a[j * n + i];
        for (size_t k = 0; k < n; k++) piv[k] = (int64_t)ipiv[k] - 1;
        return (int)info;
    }

    inline int blade_lu_solve_d(size_t n, double** LUrows, const int64_t* piv, const double* b, double* x, int trans) {
        std::vector<double> a(n * n);
        for (size_t i = 0; i < n; i++)
            for (size_t j = 0; j < n; j++) a[j * n + i] = LUrows[i][j];
        std::vector<lapack_int> ipiv(n ? n : 1);
        for (size_t k = 0; k < n; k++) ipiv[k] = (lapack_int)(piv[k] + 1);
        for (size_t i = 0; i < n; i++) x[i] = b[i];
        lapack_int info = LAPACKE_dgetrs(LAPACK_COL_MAJOR, trans ? 'T' : 'N', (lapack_int)n, 1,
                                         a.data(), (lapack_int)n, ipiv.data(), x, (lapack_int)n);
        return (int)info;
    }

    // NOT PROVIDED, and why.
    //
    // OTHER PRECISIONS OF `solve` (`sgesv` / `cgesv` / `zgesv`). They exist and
    // would drop straight in, but `TypeCheck.inferSolve` pins the surface at
    // real Float64 -- and widening it means the NATIVE arm must widen too, in
    // both the emitted C++ and the interpreter twin, since those two are held
    // to byte-identity. That makes it a language decision with a differential
    // cost, not a precision swap, so it is not taken here silently.
    //
    // MULTIPLE RIGHT-HAND SIDES (a rank-2 `b`). `?gesv` already takes `nrhs`,
    // so this layer would change by one argument. What stops it is one level
    // up: the surface returns ONE array whose rank is fixed by
    // `IR.CarriedType`'s IRSolve arm, and a matrix-RHS `solve` returns a rank-2
    // x -- a rank that depends on an argument's rank, which is a different
    // typing rule rather than a wider domain. Recorded as the natural next
    // step, with `nrhs = 1` hard-coded above marking exactly where it lands.
    //
    // PACKED / SYMMETRIC `solve` (`?spsv` / `?posv`). No surface can express a
    // compact operand argument today (`MathElaborate.arrayShape` resolves plain
    // axes one at a time), the same wall the packed eigh route sits behind.
    //
    // COMPLEX-SYMMETRIC (A = A^T without conjugation). There is NO LAPACK eigensolver for it -- `zsyev` does not
    // exist, and the packed sibling `zspev` does not either. Such a matrix is not normal in general, so its
    // eigenvalues are complex and the right routine is the GENERAL `zgeev`, which returns a complex spectrum and
    // non-orthogonal eigenvectors: a different operation with a different result TYPE, not a precision swap. The
    // classifier therefore DECLINES a SymSymmetric complex operand to the native path rather than routing it.
    //
    // SVD (`?gesvd` / `?gesdd`). Recorded as the next route; the surface (`math.svd`) already exists as synthesized
    // one-sided Jacobi, so it lands by the same conditional-elaboration mechanism eigh uses.

} // namespace blade_lapack
