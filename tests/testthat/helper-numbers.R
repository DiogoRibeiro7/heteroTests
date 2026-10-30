# Two computations of the same numbers: bit for bit off CRAN, to a tolerance
# of 1e-10 on CRAN.
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
# 0.008329376120400. On CRAN the numbers are compared with expect_equal() to
# a tolerance of 1e-10, as waldo applies it to each vector of numbers: the
# mean absolute difference of the elements that differ, relative to the mean
# absolute value they are expected to have, or absolute where that mean is at
# most 1e-10. That is 64 times the largest difference under MKL, and far
# below what a wrong row, weight, offset or term changes. A number that can be
# smaller than 1e-10, such as a p-value deep in a tail, is compared on a scale
# where it is not (its log()), so that the absolute rule does not pass it.
#
# Only numbers two computations give are compared so. What holds under any
# BLAS (classes, names, row numbers, messages and refusals) is compared with
# expect_identical(). Where the last bits decide a discrete outcome, such as
# a tie, a rank, a cache hit or the count behind a permutation p-value, the
# expectation runs off CRAN only (skip_on_cran()). The jobs of
# .github/workflows/cran-blas.yml run the tests as CRAN does, three of them
# with a BLAS that changes the last bits of its results (tools/noisy-blas.c).
identical_numbers_tolerance <- 1e-10

# Whether the tests run as on CRAN, as testthat's on_cran() decides it
# (testthat 3.2.3): not interactively, and with NOT_CRAN unset or not a true
# value such as "true" or "TRUE".
numbers_on_cran <- function() {
  !interactive() && !isTRUE(as.logical(Sys.getenv("NOT_CRAN", "false")))
}

# The tolerance of a comparison that amplifies the last bits of the BLAS, as on
# a design far from zero: `tolerance` off CRAN, and at least 1e-8 on CRAN. The
# noisy BLAS of cran-blas.yml moved White's test on a design 1e6 from zero
# away from lmtest::bptest() by 1.7e-10.
blas_tolerance <- function(tolerance) {
  if (numbers_on_cran()) max(tolerance, 1e-8) else tolerance
}

expect_identical_numbers <- function(object, expected, info = NULL, label = NULL,
                                     expected.label = NULL) {
  if (is.null(label)) {
    label <- paste(deparse(substitute(object), width.cutoff = 500L), collapse = " ")
  }
  if (is.null(expected.label)) {
    expected.label <- paste(deparse(substitute(expected), width.cutoff = 500L), collapse = " ")
  }
  if (numbers_on_cran()) {
    expect_equal(object, expected, tolerance = identical_numbers_tolerance, info = info,
                 label = label, expected.label = expected.label)
  } else {
    expect_identical(object, expected, info = info, label = label,
                     expected.label = expected.label)
  }
}
