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

test_that("performPesaranTest reproduces plm::pcdtest on unbalanced panels", {
  # N12 of the design of the model context: on an unbalanced panel the CD
  # statistic is computed from the periods each pair of units shares, as
  # plm::pcdtest(test = "cd") computes it (Pesaran 2004, Section 9, equation
  # 31): each pair correlated over its common periods, weighted by the square
  # root of their number, over the pairs that share two periods or more.
  # Before 0.16.0 the test stopped on such a panel or recycled the residuals
  # of a unit; the development version of 0.16.0 refused it.
  skip_unless_plm()
  d <- panel_fixture(seed = 20261026, n_i = 12L, n_t = 8L)
  d$w <- 1 / d$x
  set.seed(11)
  shapes <- list(
    late_entry = d$id %in% 1:3 & d$time <= 3,
    early_exit = d$id %in% 4:5 & d$time >= 6,
    gaps = d$id %in% c(2, 7, 9) & d$time %in% c(3, 5),
    random_5 = seq_len(nrow(d)) %in% sample(nrow(d), 5),
    random_20 = seq_len(nrow(d)) %in% sample(nrow(d), 20),
    one_period = d$id == 6 & d$time != 4
  )
  # plm's statistic on the Pearson residuals of a fit, as a pseries indexed by
  # the units and periods of its observations: plm tests a pseries as the
  # pooled regression of the variable on a constant.
  plm_cd <- function(m, rows) {
    r <- stats::residuals(m, type = "pearson")
    obs <- rows[names(r), ]
    pd <- plm::pdata.frame(data.frame(id = obs$id, time = obs$time, e = unname(r)),
                           index = c("id", "time"))
    suppressWarnings(plm::pcdtest(pd$e, test = "cd"))
  }
  same <- function(ours, ref, info) {
    expect_equal(as.numeric(ours$statistic), as.numeric(ref$statistic), tolerance = 1e-10,
                 info = info)
    expect_equal(ours$p.value, ref$p.value, tolerance = 1e-10, info = info)
  }
  for (shape in names(shapes)) {
    dropped <- shapes[[shape]]
    rows <- d[!dropped, ]
    full <- d
    full$x[dropped] <- NA
    # The unweighted fit against plm's own pooled fit.
    pd <- plm::pdata.frame(rows, index = c("id", "time"))
    ref <- suppressWarnings(plm::pcdtest(y ~ x, data = pd, test = "cd", model = "pooling"))
    ours <- suppressWarnings(performPesaranTest(lm(y ~ x, data = rows), rows, "id", "time"))
    same(ours, ref, shape)
    fits <- list(
      lm = function(data) lm(y ~ x, data = data),
      weighted = function(data) lm(y ~ x, data = data, weights = w),
      glm = function(data) glm(y ~ x, data = data, weights = w, family = gaussian())
    )
    for (kind in names(fits)) {
      info <- paste(shape, kind)
      ref <- plm_cd(fits[[kind]](rows), rows)
      same(suppressWarnings(performPesaranTest(fits[[kind]](rows), rows, "id", "time")), ref,
           info)
      # The full data of a fit with na.omit, and the runner.
      m_full <- fits[[kind]](full)
      same(suppressWarnings(performPesaranTest(m_full, full, "id", "time")), ref,
           paste(info, "na.omit"))
      same(suppressWarnings(runPanelTests(m_full, full, "id", "time"))$pesaran, ref,
           paste(info, "runPanelTests"))
    }
  }
  # A unit observed at one period shares at most one with any other unit: its
  # pairs are left out, with a warning, as plm leaves them out.
  rows <- d[!shapes$one_period, ]
  expect_warning(performPesaranTest(lm(y ~ x, data = rows), rows, "id", "time"),
                 "11 of the 66 pairs of units share fewer than two periods", fixed = TRUE)
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
