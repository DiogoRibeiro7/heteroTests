library(testthat)

# Reference comparisons for the two panel statistics.
#
# Both were wrong against their published definitions until 0.11.0 -- the
# random-effects LM test rejected 32.5% of the time under the null, and the CD
# statistic was 1/T too small and never rejected. Neither had a reference
# implementation among the packages the accuracy table compares against, which
# is why simulated size was the only thing that caught them. plm supplies both,
# so the corrections can now be checked against a canonical implementation
# rather than only against a formula transcribed from a paper.

skip_unless_plm <- function() {
  skip_on_cran()
  skip_if_not_installed("plm")
  # plm::pcdtest() calls plm() internally without qualifying the namespace, so
  # the package has to be attached rather than merely installed. The attachment
  # is undone when the calling test_that() block exits.
  suppressMessages(withr::local_package("plm", .local_envir = parent.frame()))
}

panel_fixture <- function(seed = 20260904, n_i = 30L, n_t = 6L, sd_u = 1.5) {
  set.seed(seed)
  id <- rep(seq_len(n_i), each = n_t)
  tt <- rep(seq_len(n_t), times = n_i)
  x <- runif(n_i * n_t, 1, 5)
  u <- rep(rnorm(n_i, sd = sd_u), each = n_t)
  data.frame(id = id, time = tt, x = x,
             y = 1 + 2 * x + u + rnorm(n_i * n_t))
}

test_that("performBPRandomEffectsTest reproduces plm::plmtest", {
  skip_unless_plm()
  d <- panel_fixture()
  pd <- plm::pdata.frame(d, index = c("id", "time"))

  ours <- suppressWarnings(
    performBPRandomEffectsTest(lm(y ~ x, data = d), d, "id"))
  ref <- plm::plmtest(y ~ x, data = pd, type = "bp", effect = "individual")

  expect_equal(as.numeric(ours$statistic), as.numeric(ref$statistic),
               tolerance = 1e-8)
  expect_equal(unname(ours$p.value), unname(ref$p.value),
               tolerance = 1e-8)
})

test_that("performPesaranTest reproduces plm::pcdtest on pooling residuals", {
  skip_unless_plm()
  # pcdtest() defaults to model = NULL, which tests the series rather than any
  # model's residuals. performPesaranTest() takes a fitted lm, whose residuals
  # are the pooling residuals, so model = "pooling" is the comparison that puts
  # both on the same quantity.
  d <- panel_fixture()
  pd <- plm::pdata.frame(d, index = c("id", "time"))

  ours <- suppressWarnings(performPesaranTest(lm(y ~ x, data = d), d, "id", "time"))
  ref <- plm::pcdtest(y ~ x, data = pd, test = "cd", model = "pooling")

  expect_equal(as.numeric(ours$statistic), as.numeric(ref$statistic),
               tolerance = 1e-8)
  expect_equal(unname(ours$p.value), unname(ref$p.value),
               tolerance = 1e-8)
})

test_that("the agreement holds across panel shapes", {
  skip_unless_plm()
  for (shape in list(c(20L, 5L), c(40L, 8L), c(15L, 10L))) {
    d <- panel_fixture(seed = 7L + shape[1], n_i = shape[1], n_t = shape[2])
    pd <- plm::pdata.frame(d, index = c("id", "time"))

    ours_lm <- suppressWarnings(
      performBPRandomEffectsTest(lm(y ~ x, data = d), d, "id"))
    ref_lm <- plm::plmtest(y ~ x, data = pd, type = "bp", effect = "individual")
    expect_equal(as.numeric(ours_lm$statistic), as.numeric(ref_lm$statistic),
                 tolerance = 1e-8,
                 info = sprintf("N = %d, T = %d", shape[1], shape[2]))

    ours_cd <- suppressWarnings(
      performPesaranTest(lm(y ~ x, data = d), d, "id", "time"))
    ref_cd <- plm::pcdtest(y ~ x, data = pd, test = "cd", model = "pooling")
    expect_equal(as.numeric(ours_cd$statistic), as.numeric(ref_cd$statistic),
                 tolerance = 1e-8,
                 info = sprintf("N = %d, T = %d", shape[1], shape[2]))
  }
})

test_that("panel p-values do not underflow to zero", {
  # 1 - pchisq() and 1 - pnorm() collapse to exactly 0 for large statistics,
  # which loses the magnitude and breaks anything working on a log scale. The
  # upper tail keeps it: at LM = 182.9 the subtraction gives 0 where the upper
  # tail gives 1.1e-41.
  d <- panel_fixture(sd_u = 3)
  r <- suppressWarnings(performBPRandomEffectsTest(lm(y ~ x, data = d), d, "id"))

  expect_gt(as.numeric(r$statistic), 100)
  expect_gt(r$p.value, 0)
  expect_lt(r$p.value, 1e-20)
})
