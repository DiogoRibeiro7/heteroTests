library(testthat)

# ---------------------------------------------------------------------------
# Pass A of the statistical validation matrix: classical regression
# diagnostics checked against an established implementation where one exists,
# and otherwise against a reconstruction of the primary reference.
#
# Each test states the definition it is reproducing so the check is auditable
# without re-deriving the algebra.
# ---------------------------------------------------------------------------

make_hetero_model <- function(seed = 42, n = 120) {
  set.seed(seed)
  d <- data.frame(x1 = runif(n, 1, 5), x2 = rnorm(n))
  d$y <- 2 + 1.5 * d$x1 + 0.7 * d$x2 + rnorm(n, sd = 0.4 + 0.5 * d$x1)
  list(data = d, model = lm(y ~ x1 + x2, data = d), n = n)
}

# --- Cook-Weisberg / NCV score test ----------------------------------------

test_that("performNCVTest reproduces car::ncvTest with the default variance model", {
  skip_if_not_installed("car")
  obj <- make_hetero_model()
  ours <- performNCVTest(obj$model)
  ref <- car::ncvTest(obj$model)

  expect_equal(unname(ours$statistic), ref$ChiSquare, tolerance = 1e-8)
  expect_equal(unname(ours$parameter), ref$Df, tolerance = 0)
  expect_equal(ours$p.value, ref$p, tolerance = 1e-8)
})

test_that("performNCVTest reproduces car::ncvTest for an explicit variance model", {
  skip_if_not_installed("car")
  obj <- make_hetero_model()

  for (fml in list(~x1, ~x2, ~ x1 + x2)) {
    ours <- performNCVTest(obj$model, var_formula = fml)
    ref <- car::ncvTest(obj$model, var.formula = fml)
    expect_equal(unname(ours$statistic), ref$ChiSquare, tolerance = 1e-8)
    expect_equal(unname(ours$parameter), ref$Df, tolerance = 0)
    expect_equal(ours$p.value, ref$p, tolerance = 1e-8)
  }
})

test_that("performNCVTest matches a direct reconstruction of Cook-Weisberg (1983)", {
  obj <- make_hetero_model()
  e <- residuals(obj$model)
  n <- length(e)

  # Score statistic: regress e^2 / (RSS / n) on the fitted values and halve the
  # explained sum of squares.
  u <- e^2 / (sum(e^2) / n)
  aux <- lm(u ~ fitted(obj$model))
  ess_half <- sum((fitted(aux) - mean(fitted(aux)))^2) / 2

  ours <- performNCVTest(obj$model)
  expect_equal(unname(ours$statistic), ess_half, tolerance = 1e-10)
  expect_equal(ours$p.value, pchisq(ess_half, 1, lower.tail = FALSE), tolerance = 1e-10)
})

test_that("performCookWeisbergTest is the fitted-value case of the score test", {
  skip_if_not_installed("car")
  obj <- make_hetero_model()
  ours <- performCookWeisbergTest(obj$model)
  ref <- car::ncvTest(obj$model)

  expect_equal(unname(ours$statistic), ref$ChiSquare, tolerance = 1e-8)
  expect_equal(ours$p.value, ref$p, tolerance = 1e-8)

  # The two exports must not drift apart.
  ncv <- performNCVTest(obj$model)
  expect_equal(unname(ours$statistic), unname(ncv$statistic), tolerance = 0)
  expect_equal(ours$p.value, ncv$p.value, tolerance = 0)
})

# --- Harvey ----------------------------------------------------------------

test_that("performHarveyTest matches the Harvey (1976) LM statistic", {
  obj <- make_hetero_model()
  Z <- model.matrix(obj$model)[, -1, drop = FALSE]
  aux <- lm(log(residuals(obj$model)^2) ~ Z)
  ess <- sum((fitted(aux) - mean(fitted(aux)))^2)

  # Var(log chi^2_1) = pi^2 / 2 = 4.9348...
  expected <- ess / (pi^2 / 2)

  ours <- performHarveyTest(obj$model)
  expect_equal(unname(ours$statistic), expected, tolerance = 1e-10)
  expect_equal(unname(ours$parameter), ncol(Z), tolerance = 0)
  expect_equal(ours$p.value, pchisq(expected, ncol(Z), lower.tail = FALSE), tolerance = 1e-10)
})

test_that("performHarveyTest studentized form matches the auxiliary F statistic", {
  obj <- make_hetero_model()
  Z <- model.matrix(obj$model)[, -1, drop = FALSE]
  fs <- summary(lm(log(residuals(obj$model)^2) ~ Z))$fstatistic

  ours <- performHarveyTest(obj$model, studentize = TRUE)
  expect_equal(unname(ours$statistic), unname(fs[1]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter), unname(fs[2:3]), tolerance = 0)
  expect_equal(
    ours$p.value,
    unname(pf(fs[1], fs[2], fs[3], lower.tail = FALSE)),
    tolerance = 1e-10
  )
})

test_that("performHarveyTest auxiliary = 'fitted' uses the fitted values and their square", {
  obj <- make_hetero_model()
  yh <- fitted(obj$model)
  aux <- lm(log(residuals(obj$model)^2) ~ yh + I(yh^2))
  ess <- sum((fitted(aux) - mean(fitted(aux)))^2)

  ours <- performHarveyTest(obj$model, auxiliary = "fitted")
  expect_equal(unname(ours$statistic), ess / (pi^2 / 2), tolerance = 1e-10)
  expect_equal(unname(ours$parameter), 2, tolerance = 0)
})

test_that("performNCVTest takes degrees of freedom from the auxiliary rank", {
  set.seed(11)
  n <- 80
  d <- data.frame(x1 = runif(n, 1, 5), x2 = rnorm(n))
  d$y <- 1 + d$x1 + d$x2 + rnorm(n)
  model <- lm(y ~ x1 + x2, data = d)

  # I(2 * x1) is an exact multiple of x1, so the variance model supplies two
  # columns but only one of them survives lm()'s pivoting. Crediting the test
  # with df = 2 here would make it conservative for the wrong reason.
  deficient <- performNCVTest(model, var_formula = ~ x1 + I(2 * x1))
  expect_equal(unname(deficient$parameter), 1, tolerance = 0)

  # Same statistic as the non-redundant variance model, on 1 df.
  plain <- performNCVTest(model, var_formula = ~x1)
  expect_equal(unname(deficient$statistic), unname(plain$statistic), tolerance = 1e-10)
  expect_equal(deficient$p.value, plain$p.value, tolerance = 1e-10)
})

test_that("performHarveyTest reports one degree of freedom per variance regressor", {
  obj <- make_hetero_model()
  expect_equal(unname(performHarveyTest(obj$model)$parameter), 2, tolerance = 0)
  expect_equal(unname(performHarveyTest(obj$model, auxiliary = "fitted")$parameter), 2,
               tolerance = 0)
})

# --- Szroeter --------------------------------------------------------------

test_that("performSzroeterTest matches a reconstruction of Szroeter (1978)", {
  obj <- make_hetero_model()
  n <- obj$n

  # h_i = i weights: h is the rank-weighted average of squared residuals, with
  # null mean (n + 1) / 2 and null variance (n^2 - 1) / (6 n).
  e_ordered <- residuals(obj$model)[order(obj$data$x1)]
  h <- sum(seq_len(n) * e_ordered^2) / sum(e_ordered^2)
  q <- (h - (n + 1) / 2) / sqrt((n^2 - 1) / (6 * n))

  ours <- performSzroeterTest(obj$model, obj$data, order_by = "x1")
  expect_equal(unname(ours$statistic), q, tolerance = 1e-10)
  expect_equal(unname(ours$estimate), h, tolerance = 1e-10)
  expect_equal(unname(ours$parameter), n, tolerance = 0)
  expect_equal(ours$p.value, pnorm(q, lower.tail = FALSE), tolerance = 1e-12)

  expect_equal(
    performSzroeterTest(obj$model, obj$data, "x1", alternative = "two.sided")$p.value,
    2 * pnorm(-abs(q)),
    tolerance = 1e-12
  )
  expect_equal(
    performSzroeterTest(obj$model, obj$data, "x1", alternative = "less")$p.value,
    pnorm(q),
    tolerance = 1e-12
  )
})

test_that("performSzroeterTest has power against ordered heteroscedasticity", {
  # Regression guard for the pre-0.7.0 standardisation bug, which shrank Q by a
  # factor of roughly 2 / sqrt(n) and left the test with no power at all.
  set.seed(404)
  n <- 200
  x <- sort(runif(n))
  d <- data.frame(x = x, y = 2 + 0.5 * x + rnorm(n, sd = 0.2 + 2 * x))
  model <- lm(y ~ x, data = d)

  expect_lt(performSzroeterTest(model, d, order_by = "x")$p.value, 0.01)
})

test_that("Szroeter Q is centred and scaled correctly under the null", {
  # Under homoskedasticity the standardised statistic should have mean ~0 and
  # standard deviation ~1. The pre-0.7.0 version had SD ~ 2 / sqrt(n).
  set.seed(9)
  n <- 60
  qs <- replicate(300, {
    d <- data.frame(x = runif(n), z = rnorm(n))
    d$y <- 1 + 2 * d$x + rnorm(n)
    unname(performSzroeterTest(lm(y ~ x, data = d), d, order_by = "z")$statistic)
  })
  expect_lt(abs(mean(qs)), 0.25)
  expect_gt(sd(qs), 0.75)
  expect_lt(sd(qs), 1.30)
})

# --- ARCH LM and McLeod-Li --------------------------------------------------

test_that("performArchLMTest matches Engle's n R^2 auxiliary regression", {
  set.seed(7)
  d <- data.frame(v = rnorm(300))
  model <- lm(v ~ 1, data = d)

  for (q in c(1L, 3L, 6L)) {
    r2v <- residuals(model)^2
    mat <- embed(r2v, q + 1)
    aux <- summary(lm(mat[, 1] ~ mat[, -1, drop = FALSE]))
    # T is the number of usable rows n - q, as in FinTS::ArchTest().
    expected <- aux$r.squared * length(residuals(aux))

    ours <- performArchLMTest(model, lags = q)
    expect_equal(unname(ours$statistic), expected, tolerance = 1e-10)
    expect_equal(unname(ours$parameter), q, tolerance = 0)
    expect_equal(ours$p.value, pchisq(expected, q, lower.tail = FALSE), tolerance = 1e-10)
  }
})

test_that("performMcLeodLiTest matches Ljung-Box on squared residuals", {
  set.seed(7)
  d <- data.frame(v = rnorm(300))
  model <- lm(v ~ 1, data = d)

  for (m in c(5L, 10L, 20L)) {
    ref <- Box.test(residuals(model)^2, lag = m, type = "Ljung-Box")
    ours <- performMcLeodLiTest(model, lags = m)
    expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10)
    # McLeod and Li (1983) show no adjustment for estimated mean-model
    # parameters is needed, so df equals the lag order.
    expect_equal(unname(ours$parameter), m, tolerance = 0)
    expect_equal(ours$p.value, ref$p.value, tolerance = 1e-10)
  }
})

# --- htest hygiene ----------------------------------------------------------

test_that("Pass A tests report named degrees of freedom", {
  obj <- make_hetero_model()
  results <- list(
    harvey = performHarveyTest(obj$model),
    ncv = performNCVTest(obj$model),
    cook_weisberg = performCookWeisbergTest(obj$model),
    park = performParkTest(obj$model, obj$data, "x1"),
    glejser = performGlejserTest(obj$model, obj$data, "x1"),
    arch = performArchLMTest(obj$model, lags = 2)
  )
  for (nm in names(results)) {
    param_names <- names(results[[nm]]$parameter)
    expect_true(
      !is.null(param_names) && all(nzchar(param_names)),
      info = paste(nm, "must name its parameter vector so print.htest labels it")
    )
  }
})

test_that("Harvey and Park warn when residuals are floored before taking logs", {
  set.seed(3)
  n <- 40
  d <- data.frame(x = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + rnorm(n)
  model <- lm(y ~ x, data = d)
  # Force one residual to be exactly zero.
  model$residuals[1] <- 0

  expect_warning(performHarveyTest(model), "numerically zero")
  expect_warning(performParkTest(model, d, "x"), "numerically zero")
})

# --- diagnostic workflow ----------------------------------------------------

test_that("compareModelDiagnostics reports NA when a diagnostic was substituted", {
  # White requires 20 observations and Breusch-Pagan 15, so at n = 18 the
  # orchestrator's fallback chain replaces White with Breusch-Pagan and tags the
  # result accordingly. The two statistics are on different scales, so the
  # comparison table must not present the substitute under the "white" column.
  set.seed(5)
  n <- 18
  d <- data.frame(x1 = runif(n, 1, 5), x2 = rnorm(n))
  d$y <- 1 + 2 * d$x1 + 0.5 * d$x2 + rnorm(n)
  model <- lm(y ~ x1 + x2, data = d)

  suite <- suppressWarnings(runHeteroTests(model, d, tests = "white"))
  expect_identical(attr(suite$white, "diagnostic"), "breusch_pagan")

  expect_warning(
    cmp <- compareModelDiagnostics(list(A = model), data = d, tests = c("white")),
    "substituted"
  )
  expect_true(is.na(cmp[1, "white"]))
})
