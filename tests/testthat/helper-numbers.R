# Two computations of the same numbers: bit for bit off CRAN, to a relative
# tolerance of 1e-10 on CRAN.
#
# Many tests compare what two paths compute: a cached result and an uncached
# one, a call with `data` and one without, an lm and the Gaussian glm of the
# same model, an old call form and the new one, the streaming runner and the
# exact one, a test and its registered name. On R's reference BLAS these agree
# to the last bit, and this repository's CI and the author's machine, where
# NOT_CRAN is "true", keep checking that. Under another BLAS the same numbers
# can differ in their last bits, and Intel MKL can give different last bits
# for the same call made twice. CRAN's check of 0.16.0 with MKL (2026-10-30)
# failed on 179 such comparisons, at relative differences of up to 1.6e-12:
# the statistic of performVarianceFormTest(), 0.008329376120413 against
# 0.008329376120400. On CRAN, where NOT_CRAN is unset, the numbers are
# compared to 1e-10: 64 times that difference, and far below what a wrong
# row, weight, offset or term changes.
#
# Only numbers two computations give are compared so. What holds under any
# BLAS (classes, names, row numbers, messages and refusals) is compared with
# expect_identical(). Where the last bits decide a discrete outcome, such as
# a tie, a rank, a cache hit or the count behind a permutation p-value, the
# expectation runs off CRAN only (skip_on_cran()). The jobs of
# .github/workflows/cran-blas.yml run the tests as CRAN does, one of them
# with a BLAS that changes the last bits of its results (tools/noisy-blas.c).
identical_numbers_tolerance <- 1e-10

expect_identical_numbers <- function(object, expected, info = NULL, label = NULL,
                                     expected.label = NULL) {
  if (is.null(label)) {
    label <- paste(deparse(substitute(object), width.cutoff = 500L), collapse = " ")
  }
  if (is.null(expected.label)) {
    expected.label <- paste(deparse(substitute(expected), width.cutoff = 500L), collapse = " ")
  }
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    expect_identical(object, expected, info = info, label = label,
                     expected.label = expected.label)
  } else {
    expect_equal(object, expected, tolerance = identical_numbers_tolerance, info = info,
                 label = label, expected.label = expected.label)
  }
}
