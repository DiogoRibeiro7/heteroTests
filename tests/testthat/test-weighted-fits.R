library(testthat)

# ---------------------------------------------------------------------------
# Weighted fits.
#
# lm(..., weights = w) states that Var(e_i) = sigma^2 / w_i, so the raw
# residuals of a weighted fit are heteroscedastic by assumption. The residuals
# that have constant variance when the weights are right are the Pearson
# residuals sqrt(w_i) e_i. Before 0.12.0 every test read the raw residuals and
# rejected a correctly weighted fit as often as an unweighted one, which made
# "fit by WLS, then test again" unable to succeed.
#
# The checks below state what each statistic is on a weighted fit, and the
# simulated guards at the end are what would have caught the fault: no
# reference comparison did, because lmtest::bptest() also tests the raw
# residuals of a weighted fit.
# ---------------------------------------------------------------------------

make_weighted <- function(seed = 7, n = 150) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x^2 * rnorm(n)
  d$g <- cut(d$x, 3, labels = c("a", "b", "c"))
  d$w <- 1 / d$x^4
  list(
    data = d,
    weighted = lm(y ~ x + z, data = d, weights = w),
    unweighted = lm(y ~ x + z, data = d)
  )
}

# --- the residuals themselves ----------------------------------------------

test_that("rpearson_residuals leaves an unweighted fit untouched", {
  obj <- make_weighted()
  expect_identical(
    heteroTests:::rpearson_residuals(obj$unweighted),
    stats::residuals(obj$unweighted)
  )
})

test_that("rpearson_residuals returns sqrt(w) * e for a weighted fit", {
  obj <- make_weighted()
  expect_equal(
    heteroTests:::rpearson_residuals(obj$weighted),
    sqrt(obj$data$w) * stats::residuals(obj$weighted),
    tolerance = 1e-12
  )
})

test_that("the Pearson residuals are those of the equivalent unweighted regression", {
  # This identity is the whole justification: weighted least squares is
  # ordinary least squares on sqrt(w) y and sqrt(w) X, and that regression has
  # homoscedastic errors when the weights are right.
  obj <- make_weighted()
  d <- obj$data
  s <- sqrt(d$w)
  transformed <- lm(I(s * y) ~ 0 + s + I(s * x) + I(s * z), data = d)

  expect_equal(
    unname(heteroTests:::rpearson_residuals(obj$weighted)),
    unname(residuals(transformed)),
    tolerance = 1e-10
  )
})

test_that("rpearson_residuals keeps the padding of na.exclude", {
  obj <- make_weighted()
  d <- obj$data
  d$y[5] <- NA
  m <- lm(y ~ x + z, data = d, weights = w, na.action = na.exclude)
  r <- heteroTests:::rpearson_residuals(m)

  expect_length(r, nrow(d))
  expect_true(is.na(r[5]))
  expect_equal(unname(r[-5]), unname(sqrt(d$w[-5]) * m$residuals), tolerance = 1e-12)
})

test_that("a fit with a zero weight is refused", {
  # A zero weight drops the observation from the fit. Its Pearson residual is
  # an artificial zero, and counting it as a residual would bias every test.
  obj <- make_weighted()
  d <- obj$data
  d$w[3] <- 0
  m <- lm(y ~ x + z, data = d, weights = w)

  expect_error(heteroTests:::rpearson_residuals(m), "zero or non-finite weights")
  expect_error(performKoenkerTest(m, d), "zero or non-finite weights")
})

test_that("a glm keeps its own residuals", {
  # Prior weights of a glm are not inverse error variances, and its default
  # residuals are deviance residuals. Nothing changes for it.
  set.seed(3)
  d <- data.frame(x = runif(80))
  d$y <- rpois(80, exp(0.5 + d$x))
  m <- glm(y ~ x, family = poisson, data = d, weights = rep(2, 80))

  expect_identical(heteroTests:::rpearson_residuals(m), stats::residuals(m))
})

# --- auxiliary-regression tests against a direct reconstruction -------------

test_that("Koenker on a weighted fit is n R^2 from the squared Pearson residuals", {
  obj <- make_weighted()
  d <- obj$data
  e2 <- (sqrt(d$w) * residuals(obj$weighted))^2
  aux <- lm(e2 ~ x + z, data = d)
  expected <- nrow(d) * summary(aux)$r.squared

  ours <- suppressWarnings(performKoenkerTest(obj$weighted, d))
  expect_equal(unname(ours$statistic), expected, tolerance = 1e-8)
  expect_equal(unname(ours$parameter), 2)
  expect_equal(ours$p.value, pchisq(expected, 2, lower.tail = FALSE), tolerance = 1e-8)
})

test_that("the two studentized Breusch-Pagan implementations agree on a weighted fit", {
  # They are one statistic under two names on unweighted fits. Before 0.12.0
  # they parted on weighted ones: performKoenkerTest() ignored the weights and
  # performStudentizedBPTest() weighted the auxiliary regression instead of the
  # residuals, and could not run at all when the weights were a data column.
  obj <- make_weighted()
  koenker <- suppressWarnings(performKoenkerTest(obj$weighted, obj$data))
  studentized <- suppressWarnings(performStudentizedBPTest(obj$weighted, obj$data))

  expect_equal(unname(studentized$statistic), unname(koenker$statistic), tolerance = 1e-8)
  expect_equal(unname(studentized$parameter), unname(koenker$parameter))
  expect_equal(studentized$p.value, koenker$p.value, tolerance = 1e-8)
})

test_that("classical Breusch-Pagan on a weighted fit uses the squared Pearson residuals", {
  obj <- make_weighted()
  d <- obj$data
  e2 <- (sqrt(d$w) * residuals(obj$weighted))^2
  f <- e2 / mean(e2) - 1
  aux <- lm(f ~ x + z, data = d)
  expected <- 0.5 * sum(fitted(aux)^2)

  ours <- suppressWarnings(performBPTest(obj$weighted, d))
  expect_equal(unname(ours$statistic), expected, tolerance = 1e-8)
  expect_equal(ours$p.value, pchisq(expected, 2, lower.tail = FALSE), tolerance = 1e-8)
})

test_that("White on a weighted fit uses the squared Pearson residuals", {
  obj <- make_weighted()
  d <- obj$data
  e2 <- (sqrt(d$w) * residuals(obj$weighted))^2
  aux <- lm(e2 ~ x + z + I(x^2) + I(z^2) + I(x * z), data = d)
  expected <- nrow(d) * summary(aux)$r.squared

  ours <- suppressWarnings(performWhiteTest(obj$weighted, d))
  expect_equal(unname(ours$statistic), expected, tolerance = 1e-8)
  expect_equal(unname(ours$parameter), 5)
})

test_that("the streaming tests agree with the exact ones on a weighted fit", {
  obj <- make_weighted()
  d <- obj$data
  pairs <- list(
    list(performKoenkerTest, performKoenkerTestStreaming),
    list(performBPTest, performBPTestStreaming),
    list(performWhiteTest, performWhiteTestStreaming)
  )
  for (pair in pairs) {
    exact <- suppressWarnings(pair[[1]](obj$weighted, d))
    streamed <- suppressWarnings(
      pair[[2]](obj$weighted, d, chunk_size = 40, progress = FALSE)
    )
    expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8)
  }
})

# --- against established implementations -------------------------------------

test_that("performNCVTest reproduces car::ncvTest on a weighted fit", {
  # car::ncvTest() uses Pearson residuals, so it is a reference here. It was
  # one before 0.12.0 too, and the package disagreed with it on weighted fits.
  skip_if_not_installed("car")
  obj <- make_weighted()
  assign("weighted_fit_data", obj$data, envir = globalenv())
  on.exit(rm("weighted_fit_data", envir = globalenv()), add = TRUE)
  m <- lm(y ~ x + z, data = weighted_fit_data, weights = w)

  ours <- performNCVTest(m)
  ref <- car::ncvTest(m)
  expect_equal(unname(ours$statistic), ref$ChiSquare, tolerance = 1e-8)
  expect_equal(unname(ours$parameter), ref$Df, tolerance = 0)
  expect_equal(ours$p.value, ref$p, tolerance = 1e-8)

  ours_x <- performNCVTest(m, var_formula = ~x)
  ref_x <- car::ncvTest(m, var.formula = ~x)
  expect_equal(unname(ours_x$statistic), ref_x$ChiSquare, tolerance = 1e-8)
  expect_equal(ours_x$p.value, ref_x$p, tolerance = 1e-8)
})

test_that("Goldfeld-Quandt on a weighted fit is lmtest::gqtest on the transformed regression", {
  skip_if_not_installed("lmtest")
  obj <- make_weighted()
  d <- obj$data
  s <- sqrt(d$w)
  td <- data.frame(ys = s * d$y, c0 = s, xs = s * d$x, zs = s * d$z)

  for (fraction in c(0.1, 0.2, 0.3)) {
    ours <- suppressWarnings(
      performGQTest(obj$weighted, d, order_by = "x", fraction = fraction)
    )
    ref <- lmtest::gqtest(ys ~ 0 + c0 + xs + zs, data = td,
                          order.by = d$x, point = 0.5, fraction = fraction)
    expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-8)
    expect_equal(unname(ours$parameter), unname(ref$parameter))
    expect_equal(ours$p.value, ref$p.value, tolerance = 1e-8)
  }
})

test_that("RESET on a weighted fit is the F test between nested weighted fits", {
  # Before 0.12.0 the augmented model was refitted without the weights and
  # compared with the weighted restricted fit, so the two were not nested.
  obj <- make_weighted()
  d <- obj$data
  d$fit <- fitted(obj$weighted)
  augmented <- lm(y ~ x + z + I(fit^2) + I(fit^3), data = d, weights = w)
  ref <- anova(obj$weighted, augmented)

  ours <- performRESETTest(obj$weighted)
  expect_equal(unname(ours$statistic), ref$F[2], tolerance = 1e-8)
  expect_equal(unname(ours$parameter), c(2, ref$Res.Df[2]))
  expect_equal(ours$p.value, ref$`Pr(>F)`[2], tolerance = 1e-8)
  expect_gt(unname(ours$statistic), 0)
})

# --- procedures that refit without the weights -------------------------------

test_that("procedures that would drop the weights refuse a weighted fit", {
  obj <- make_weighted()
  d <- obj$data
  refusal <- "does not support weighted fits"

  expect_error(performWildBootstrapTest(obj$weighted, d, B = 19, progress = FALSE), refusal)
  expect_error(performWhiteTestBootstrap(obj$weighted, d, B = 19), refusal)
  expect_error(performWhiteTestRobust(obj$weighted, d, bootstrap = TRUE, B = 19), refusal)
  expect_error(
    rbootstrap_test_statistic(performWhiteTest, obj$weighted, d, B = 19, progress = FALSE),
    refusal
  )
  skip_if_not_installed("quantreg")
  expect_error(performQuantileRegressionTest(obj$weighted, d), refusal)
})

# --- what users see ------------------------------------------------------------

test_that("compareModelDiagnostics shows that weighting worked", {
  # The README workflow. With a correctly specified variance function the
  # weighted fit must come out far less heteroscedastic than the original; on
  # raw residuals the two rows were nearly equal.
  set.seed(3)
  n <- 300
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + exp(0.4 * d$x) * rnorm(n)
  ols <- lm(y ~ x + z, data = d)
  wls <- fitWLS(ols)

  cmp <- suppressWarnings(
    compareModelDiagnostics(list(ols, wls), d, tests = c("white", "breusch_pagan"))
  )
  expect_lt(cmp[2, "white"], cmp[1, "white"] / 5)
  expect_lt(cmp[2, "breusch_pagan"], cmp[1, "breusch_pagan"] / 5)
})

test_that("plotBeforeAfter draws the Pearson residuals of a weighted remedy", {
  obj <- make_weighted()
  p <- plotBeforeAfter(obj$unweighted, obj$weighted)
  drawn <- p$data$resid[p$data$model == "remedied"]

  expect_equal(
    unname(drawn),
    unname(sqrt(obj$data$w) * residuals(obj$weighted)),
    tolerance = 1e-12
  )
})

# --- simulated guards -----------------------------------------------------------

test_that("the tests hold their level on a correctly weighted fit", {
  skip_on_cran()
  # Heteroscedastic data, sd = x^2, fitted with the weights that undo it. Each
  # of these rejected 100% of the time before 0.12.0. 150 replications put the
  # Monte Carlo standard error at 1.8%, so a test at its nominal level lands
  # well inside the band.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  tests <- list(
    koenker = function(o) performKoenkerTest(o$weighted, o$data),
    white = function(o) performWhiteTest(o$weighted, o$data),
    breusch_pagan = function(o) performBPTest(o$weighted, o$data),
    ncv = function(o) performNCVTest(o$weighted),
    harvey = function(o) performHarveyTest(o$weighted),
    davidian_carroll = function(o) performDavidianCarrollTest(o$weighted),
    spread_level = function(o) performSpreadLevelTest(o$weighted),
    spearman = function(o) performSpearmanTest(o$weighted),
    glejser = function(o) performGlejserTest(o$weighted, o$data, "x"),
    park = function(o) performParkTest(o$weighted, o$data, "x"),
    goldfeld_quandt = function(o) performGQTest(o$weighted, o$data, "x"),
    szroeter = function(o) performSzroeterTest(o$weighted, o$data, "x"),
    levene = function(o) performLeveneTest(o$weighted, o$data, "g"),
    brown_forsythe = function(o) performBrownForsytheTest(o$weighted, o$data, "g")
  )
  reps <- 150

  for (nm in names(tests)) {
    p <- vapply(seq_len(reps), function(i) {
      suppressWarnings(suppressMessages(tests[[nm]](make_weighted(seed = 1000 + i))$p.value))
    }, numeric(1))
    expect_lt(mean(p < 0.05), 0.15, label = paste("rejection rate of", nm))
  }
})

test_that("weights that are too weak are still rejected", {
  skip_on_cran()
  # The correction must not have removed the power: with sd = x^2 and weights
  # 1 / x, the weighted fit is still strongly heteroscedastic.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  p <- vapply(seq_len(100), function(i) {
    d <- make_weighted(seed = 2000 + i)$data
    d$weak <- 1 / d$x
    m <- lm(y ~ x + z, data = d, weights = weak)
    suppressWarnings(suppressMessages(performKoenkerTest(m, d)$p.value))
  }, numeric(1))

  expect_gt(mean(p < 0.05), 0.9)
})
