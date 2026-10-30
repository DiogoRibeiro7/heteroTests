/*
 * A BLAS that changes the last bits of its results, for the "noisy BLAS" job
 * of .github/workflows/cran-blas.yml. Not part of the package.
 *
 * Preloaded (LD_PRELOAD) in front of the BLAS R uses, it forwards each call
 * of the reductions and products R and LAPACK make to that BLAS, and moves
 * the result, element by element, by at most one unit in the last place, up
 * or down or not at all, from a pseudo-random sequence. Two computations of
 * the same numbers then differ in their last bits, as they do under Intel
 * MKL on the processors where its results depend on how the data are
 * aligned in memory (CRAN's check of 0.16.0, 2026-10-30), whatever processor
 * runs the job.
 *
 *   gcc -O2 -shared -fPIC -o libnoisyblas.so tools/noisy-blas.c -ldl -lm
 *   LD_PRELOAD=$PWD/libnoisyblas.so R CMD check ...
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static uint64_t noisy_state = 0x9E3779B97F4A7C15ULL;

static double nudge(double x)
{
    noisy_state ^= noisy_state << 13;
    noisy_state ^= noisy_state >> 7;
    noisy_state ^= noisy_state << 17;
    if (!isfinite(x) || x == 0.0) {
        return x;
    }
    switch (noisy_state % 3U) {
    case 1:
        return nextafter(x, INFINITY);
    case 2:
        return nextafter(x, -INFINITY);
    default:
        return x;
    }
}

static void *next_symbol(const char *name)
{
    void *f = dlsym(RTLD_NEXT, name);
    if (f == NULL) {
        fprintf(stderr, "noisy-blas: no %s after this library\n", name);
        abort();
    }
    return f;
}

/* The Fortran character arguments of the BLAS carry hidden lengths, which R
 * passes (FCONE); they are forwarded as they come. */

typedef double (*ddot_fn)(const int *, const double *, const int *, const double *,
                          const int *);
double ddot_(const int *n, const double *x, const int *incx, const double *y,
             const int *incy)
{
    static ddot_fn real = NULL;
    if (real == NULL) real = (ddot_fn) next_symbol("ddot_");
    return nudge(real(n, x, incx, y, incy));
}

typedef double (*dnrm2_fn)(const int *, const double *, const int *);
double dnrm2_(const int *n, const double *x, const int *incx)
{
    static dnrm2_fn real = NULL;
    if (real == NULL) real = (dnrm2_fn) next_symbol("dnrm2_");
    return nudge(real(n, x, incx));
}

typedef double (*dasum_fn)(const int *, const double *, const int *);
double dasum_(const int *n, const double *x, const int *incx)
{
    static dasum_fn real = NULL;
    if (real == NULL) real = (dasum_fn) next_symbol("dasum_");
    return nudge(real(n, x, incx));
}

static void nudge_matrix(int m, int n, double *a, int lda)
{
    for (int j = 0; j < n; j++) {
        for (int i = 0; i < m; i++) {
            a[i + (size_t) j * lda] = nudge(a[i + (size_t) j * lda]);
        }
    }
}

typedef void (*dgemm_fn)(const char *, const char *, const int *, const int *, const int *,
                         const double *, const double *, const int *, const double *,
                         const int *, const double *, double *, const int *, size_t, size_t);
void dgemm_(const char *ta, const char *tb, const int *m, const int *n, const int *k,
            const double *alpha, const double *a, const int *lda, const double *b,
            const int *ldb, const double *beta, double *c, const int *ldc, size_t la,
            size_t lb)
{
    static dgemm_fn real = NULL;
    if (real == NULL) real = (dgemm_fn) next_symbol("dgemm_");
    real(ta, tb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc, la, lb);
    nudge_matrix(*m, *n, c, *ldc);
}

typedef void (*dgemv_fn)(const char *, const int *, const int *, const double *,
                         const double *, const int *, const double *, const int *,
                         const double *, double *, const int *, size_t);
void dgemv_(const char *trans, const int *m, const int *n, const double *alpha,
            const double *a, const int *lda, const double *x, const int *incx,
            const double *beta, double *y, const int *incy, size_t lt)
{
    static dgemv_fn real = NULL;
    if (real == NULL) real = (dgemv_fn) next_symbol("dgemv_");
    real(trans, m, n, alpha, a, lda, x, incx, beta, y, incy, lt);
    int len = (*trans == 'N' || *trans == 'n') ? *m : *n;
    int step = *incy < 0 ? -*incy : *incy;
    for (int i = 0; i < len; i++) {
        y[(size_t) i * step] = nudge(y[(size_t) i * step]);
    }
}

typedef void (*dsyrk_fn)(const char *, const char *, const int *, const int *,
                         const double *, const double *, const int *, const double *,
                         double *, const int *, size_t, size_t);
void dsyrk_(const char *uplo, const char *trans, const int *n, const int *k,
            const double *alpha, const double *a, const int *lda, const double *beta,
            double *c, const int *ldc, size_t lu, size_t lt)
{
    static dsyrk_fn real = NULL;
    if (real == NULL) real = (dsyrk_fn) next_symbol("dsyrk_");
    real(uplo, trans, n, k, alpha, a, lda, beta, c, ldc, lu, lt);
    int upper = (*uplo == 'U' || *uplo == 'u');
    for (int j = 0; j < *n; j++) {
        for (int i = upper ? 0 : j; i <= (upper ? j : *n - 1); i++) {
            c[i + (size_t) j * *ldc] = nudge(c[i + (size_t) j * *ldc]);
        }
    }
}
