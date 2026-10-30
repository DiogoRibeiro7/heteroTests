/*
 * A BLAS that changes the last bits of its results, for the "noisy BLAS" jobs
 * of .github/workflows/cran-blas.yml. Not part of the package.
 *
 * Preloaded (LD_PRELOAD) in front of the BLAS R uses, it forwards the calls R
 * and LAPACK make to that BLAS, and moves each element of the result by at
 * most one unit in the last place, up or down or not at all. Two
 * computations of the same numbers then differ in their last bits, as they
 * do under Intel MKL on the processors where its results depend on how the
 * data are aligned in memory (CRAN's check of 0.16.0, 2026-10-30), whatever
 * processor runs the job.
 *
 * It leaves alone:
 *   - an integral result, such as the counts crossprod() makes of 0/1 values,
 *     which are exact under any IEEE BLAS;
 *   - what the reference BLAS leaves exact: a call with alpha 0 (daxpy,
 *     dgemv, dgemm, dger), dscal by 1 or -1, and an element of dger whose
 *     x_i or y_j is 0.
 * Equal results of one call, such as the fitted values of equal design rows,
 * move alike: the move is a function of the value and of a salt drawn for
 * each call, and the same values in two calls move differently. A real BLAS
 * can break such a tie, when a row falls to a peel or tail loop whose kernel
 * rounds otherwise; that is not modelled here, so the tests whose ties turn
 * on it were reviewed by hand and run off CRAN only (test-szroeter-registry.R).
 *
 * The salts come from a pseudo-random sequence whose seed is NOISY_BLAS_SEED
 * (1 when unset), so a run can be repeated.
 *
 *   gcc -O2 -shared -fPIC -o libnoisyblas.so tools/noisy-blas.c -ldl -lm
 *   NOISY_BLAS_SEED=2 LD_PRELOAD=$PWD/libnoisyblas.so R CMD check ...
 */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <math.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static uint64_t noisy_state = 0;
static uint64_t noisy_salt = 0;

static uint64_t splitmix64(uint64_t x)
{
    x += 0x9E3779B97F4A7C15ULL;
    x = (x ^ (x >> 30)) * 0xBF58476D1CE4E5B9ULL;
    x = (x ^ (x >> 27)) * 0x94D049BB133111EBULL;
    return x ^ (x >> 31);
}

/* A new salt for each call of the BLAS. */
static void new_call(void)
{
    if (noisy_state == 0) {
        const char *seed = getenv("NOISY_BLAS_SEED");
        noisy_state = splitmix64(seed != NULL ? strtoull(seed, NULL, 10) : 1ULL) | 1ULL;
    }
    noisy_state = splitmix64(noisy_state);
    noisy_salt = noisy_state;
}

static double nudge(double x)
{
    if (!isfinite(x) || x == 0.0 || x == nearbyint(x)) {
        return x;
    }
    uint64_t bits;
    memcpy(&bits, &x, sizeof bits);
    switch (splitmix64(bits ^ noisy_salt) % 3U) {
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

static void nudge_vector(int n, double *x, int inc)
{
    int step = inc < 0 ? -inc : inc;
    for (int i = 0; i < n; i++) {
        x[(size_t) i * step] = nudge(x[(size_t) i * step]);
    }
}

static void nudge_matrix(int m, int n, double *a, int lda)
{
    for (int j = 0; j < n; j++) {
        for (int i = 0; i < m; i++) {
            a[i + (size_t) j * lda] = nudge(a[i + (size_t) j * lda]);
        }
    }
}

/* The Fortran character arguments of the BLAS carry hidden lengths, which R
 * passes (FCONE); they are forwarded as they come. */

/* Level 1. */

typedef double (*ddot_fn)(const int *, const double *, const int *, const double *,
                          const int *);
double ddot_(const int *n, const double *x, const int *incx, const double *y,
             const int *incy)
{
    static ddot_fn real = NULL;
    if (real == NULL) real = (ddot_fn) next_symbol("ddot_");
    new_call();
    return nudge(real(n, x, incx, y, incy));
}

typedef double (*dnrm2_fn)(const int *, const double *, const int *);
double dnrm2_(const int *n, const double *x, const int *incx)
{
    static dnrm2_fn real = NULL;
    if (real == NULL) real = (dnrm2_fn) next_symbol("dnrm2_");
    new_call();
    return nudge(real(n, x, incx));
}

typedef double (*dasum_fn)(const int *, const double *, const int *);
double dasum_(const int *n, const double *x, const int *incx)
{
    static dasum_fn real = NULL;
    if (real == NULL) real = (dasum_fn) next_symbol("dasum_");
    new_call();
    return nudge(real(n, x, incx));
}

typedef void (*daxpy_fn)(const int *, const double *, const double *, const int *, double *,
                         const int *);
void daxpy_(const int *n, const double *alpha, const double *x, const int *incx, double *y,
            const int *incy)
{
    static daxpy_fn real = NULL;
    if (real == NULL) real = (daxpy_fn) next_symbol("daxpy_");
    real(n, alpha, x, incx, y, incy);
    /* The reference returns y as it is when alpha is 0. */
    if (*alpha == 0.0) return;
    new_call();
    nudge_vector(*n, y, *incy);
}

typedef void (*dscal_fn)(const int *, const double *, double *, const int *);
void dscal_(const int *n, const double *alpha, double *x, const int *incx)
{
    static dscal_fn real = NULL;
    if (real == NULL) real = (dscal_fn) next_symbol("dscal_");
    real(n, alpha, x, incx);
    /* A product by 1 or -1 is exact. */
    if (*alpha == 1.0 || *alpha == -1.0) return;
    new_call();
    nudge_vector(*n, x, *incx);
}

/* Level 2. */

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
    /* The reference returns with y = beta y when alpha is 0. */
    if (*alpha == 0.0) return;
    new_call();
    nudge_vector((*trans == 'N' || *trans == 'n') ? *m : *n, y, *incy);
}

typedef void (*dsymv_fn)(const char *, const int *, const double *, const double *,
                         const int *, const double *, const int *, const double *, double *,
                         const int *, size_t);
void dsymv_(const char *uplo, const int *n, const double *alpha, const double *a,
            const int *lda, const double *x, const int *incx, const double *beta, double *y,
            const int *incy, size_t lu)
{
    static dsymv_fn real = NULL;
    if (real == NULL) real = (dsymv_fn) next_symbol("dsymv_");
    real(uplo, n, alpha, a, lda, x, incx, beta, y, incy, lu);
    new_call();
    nudge_vector(*n, y, *incy);
}

typedef void (*dtrmv_fn)(const char *, const char *, const char *, const int *, const double *,
                         const int *, double *, const int *, size_t, size_t, size_t);
void dtrmv_(const char *uplo, const char *trans, const char *diag, const int *n,
            const double *a, const int *lda, double *x, const int *incx, size_t lu, size_t lt,
            size_t ld)
{
    static dtrmv_fn real = NULL;
    if (real == NULL) real = (dtrmv_fn) next_symbol("dtrmv_");
    real(uplo, trans, diag, n, a, lda, x, incx, lu, lt, ld);
    new_call();
    nudge_vector(*n, x, *incx);
}

typedef void (*dtrsv_fn)(const char *, const char *, const char *, const int *, const double *,
                         const int *, double *, const int *, size_t, size_t, size_t);
void dtrsv_(const char *uplo, const char *trans, const char *diag, const int *n,
            const double *a, const int *lda, double *x, const int *incx, size_t lu, size_t lt,
            size_t ld)
{
    static dtrsv_fn real = NULL;
    if (real == NULL) real = (dtrsv_fn) next_symbol("dtrsv_");
    real(uplo, trans, diag, n, a, lda, x, incx, lu, lt, ld);
    new_call();
    nudge_vector(*n, x, *incx);
}

typedef void (*dger_fn)(const int *, const int *, const double *, const double *, const int *,
                        const double *, const int *, double *, const int *);
void dger_(const int *m, const int *n, const double *alpha, const double *x, const int *incx,
           const double *y, const int *incy, double *a, const int *lda)
{
    static dger_fn real = NULL;
    if (real == NULL) real = (dger_fn) next_symbol("dger_");
    real(m, n, alpha, x, incx, y, incy, a, lda);
    /* The reference returns A as it is when alpha is 0, and leaves an element
     * exact where x_i or y_j is 0. With a negative increment the vector is
     * read from its last element, as the reference reads it. */
    if (*alpha == 0.0) return;
    new_call();
    for (int j = 0; j < *n; j++) {
        size_t jy = *incy > 0 ? (size_t) j * *incy : (size_t) (*n - 1 - j) * -*incy;
        if (y[jy] == 0.0) continue;
        for (int i = 0; i < *m; i++) {
            size_t ix = *incx > 0 ? (size_t) i * *incx : (size_t) (*m - 1 - i) * -*incx;
            if (x[ix] == 0.0) continue;
            a[i + (size_t) j * *lda] = nudge(a[i + (size_t) j * *lda]);
        }
    }
}

/* Level 3. */

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
    /* The reference returns with C = beta C when alpha is 0. */
    if (*alpha == 0.0) return;
    new_call();
    nudge_matrix(*m, *n, c, *ldc);
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
    new_call();
    int upper = (*uplo == 'U' || *uplo == 'u');
    for (int j = 0; j < *n; j++) {
        for (int i = upper ? 0 : j; i <= (upper ? j : *n - 1); i++) {
            c[i + (size_t) j * *ldc] = nudge(c[i + (size_t) j * *ldc]);
        }
    }
}

typedef void (*dtrmm_fn)(const char *, const char *, const char *, const char *, const int *,
                         const int *, const double *, const double *, const int *, double *,
                         const int *, size_t, size_t, size_t, size_t);
void dtrmm_(const char *side, const char *uplo, const char *transa, const char *diag,
            const int *m, const int *n, const double *alpha, const double *a, const int *lda,
            double *b, const int *ldb, size_t ls, size_t lu, size_t lt, size_t ld)
{
    static dtrmm_fn real = NULL;
    if (real == NULL) real = (dtrmm_fn) next_symbol("dtrmm_");
    real(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb, ls, lu, lt, ld);
    new_call();
    nudge_matrix(*m, *n, b, *ldb);
}

typedef void (*dtrsm_fn)(const char *, const char *, const char *, const char *, const int *,
                         const int *, const double *, const double *, const int *, double *,
                         const int *, size_t, size_t, size_t, size_t);
void dtrsm_(const char *side, const char *uplo, const char *transa, const char *diag,
            const int *m, const int *n, const double *alpha, const double *a, const int *lda,
            double *b, const int *ldb, size_t ls, size_t lu, size_t lt, size_t ld)
{
    static dtrsm_fn real = NULL;
    if (real == NULL) real = (dtrsm_fn) next_symbol("dtrsm_");
    real(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb, ls, lu, lt, ld);
    new_call();
    nudge_matrix(*m, *n, b, *ldb);
}
