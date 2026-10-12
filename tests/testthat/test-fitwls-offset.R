library(testthat)

# ---------------------------------------------------------------------------
# fitWLS() and offsets.
#
# fitWLS() estimates a variance function from the residuals of a model and
# refits the model with the weights it implies. The refit built its design by
# evaluating the call of the fit again in the model frame, which holds log(x)
# and not x, and weights and an offset given as arguments as "(weights)" and
# "(offset)", so it failed on a transformed regressor, on a model fitted with
# weights and on most offsets; where it did not fail, it called lm.wfit()
# without the offset and dropped it. Each check below fits the same model
# with lm() and the estimated weights, which is what fitWLS() is documented
# to return, and compares.
# ---------------------------------------------------------------------------

make_offset_data <- function(seed = 11, n = 200) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  # Not in the span of the regressors, or dropping it would not show.
  d$o <- 0.5 * d$x^2
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$o + exp(0.3 * d$x) * rnorm(n)
  d
}

# The weights fitWLS() estimates by default, written out: the log squared
# residuals regressed on the regressors of the model, centred and inverted.
estimated_weights <- function(model, regressors) {
  aux <- lm(log(residuals(model)^2) ~ ., data = regressors)
  g <- fitted(aux)
  unname(1 / exp(g - mean(g)))
}

expect_same_fit <- function(ours, reference) {
  expect_equal(unname(coef(ours)), unname(coef(reference)), tolerance = 1e-8)
  expect_equal(unname(fitted(ours)), unname(fitted(reference)), tolerance = 1e-8)
  expect_equal(unname(residuals(ours)), unname(residuals(reference)), tolerance = 1e-8)
}

test_that("fitWLS() keeps an offset given as an argument", {
  d <- make_offset_data()
  m <- lm(y ~ x + z, data = d, offset = o)
  d$w <- estimated_weights(m, d[c("x", "z")])

  wls <- fitWLS(m)
  expect_equal(unname(wls$weights), d$w, tolerance = 1e-10)
  expect_same_fit(wls, lm(y ~ x + z, data = d, offset = o, weights = w))
  expect_equal(unname(wls$offset), d$o)
})

test_that("fitWLS() keeps an offset written in the formula", {
  d <- make_offset_data()
  m <- lm(y ~ x + z + offset(o), data = d)
  d$w <- estimated_weights(m, d[c("x", "z")])

  wls <- fitWLS(m)
  expect_same_fit(wls, lm(y ~ x + z + offset(o), data = d, weights = w))
})

test_that("fitWLS() does not drop an offset that the old refit could find", {
  # An offset that is a variable of the calling environment rather than a
  # column of the data. Evaluating the call again found it, so the refit ran,
  # and lm.wfit() was then called without it.
  d <- make_offset_data()
  shift <- d$o
  m <- lm(y ~ x + z, data = d, offset = shift)
  d$w <- estimated_weights(m, d[c("x", "z")])

  expect_same_fit(fitWLS(m), lm(y ~ x + z, data = d, offset = shift, weights = w))
})

test_that("fitWLS() refits a model with a transformed regressor", {
  # The model frame holds log(x), not x, so evaluating the call of the fit
  # again in it failed with "object 'x' not found".
  d <- make_offset_data()
  m <- lm(y ~ log(x) + z, data = d)
  d$w <- estimated_weights(m, data.frame(lx = log(d$x), z = d$z))

  expect_same_fit(fitWLS(m), lm(y ~ log(x) + z, data = d, weights = w))
})

test_that("fitWLS() refits a model that was fitted with weights", {
  # A model frame holds weights given as an argument as "(weights)", so
  # evaluating the call of the fit again in it failed with "object 'w' not
  # found" whenever the weights were a column of the data. fitWLS() replaces
  # them with the weights of the variance function it estimates, as it did
  # when the old refit could find them.
  d <- make_offset_data()
  d$w <- 1 / d$x
  m <- lm(y ~ x + z, data = d, weights = w)
  r <- residuals(m)
  aux <- lm(log(r^2) ~ x, data = d)
  d$w_new <- unname(1 / exp(fitted(aux) - mean(fitted(aux))))

  wls <- fitWLS(m, d, var_formula = ~x)
  expect_equal(unname(wls$weights), d$w_new, tolerance = 1e-10)
  expect_same_fit(wls, lm(y ~ x + z, data = d, weights = w_new))
  expect_same_fit(fitWLS(m, var_formula = ~x), wls)
})

test_that("performVarianceFormTest() tests the weighted fit with its offset", {
  # The F test of the squares and the product of x and z added to the
  # regression of the squared standardized residuals on x and z, written out
  # from a weighted fit made with lm().
  d <- make_offset_data()
  m <- lm(y ~ x + z, data = d, offset = o)
  d$w <- estimated_weights(m, d[c("x", "z")])
  wls <- lm(y ~ x + z, data = d, offset = o, weights = w)
  d$r <- d$w * residuals(wls)^2
  reference <- anova(
    lm(r ~ x + z, data = d),
    lm(r ~ x + z + I(x^2) + I(z^2) + I(x * z), data = d)
  )

  for (model in list(m, fitWLS(m))) {
    ours <- performVarianceFormTest(model)
    expect_equal(unname(ours$statistic), reference$F[2], tolerance = 1e-8)
    expect_equal(unname(ours$parameter), c(3, reference$Res.Df[2]))
    expect_equal(ours$p.value, reference$`Pr(>F)`[2], tolerance = 1e-8)
  }
})

test_that("the variance-form test holds its level on a fit with an offset", {
  skip_on_cran()
  # The exponential variance function in x is right, so the rejection rate is
  # a size. 2000 replications put the 99% interval around 0.05 at
  # [0.037, 0.063]. The offset, 2 x^2, is large enough that a weighted fit
  # without it rejects about 70% of the time: its residuals keep the part of
  # 2 x^2 that x and z do not explain, and the squared standardized residuals
  # then follow x^2, one of the terms the test adds.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(20261014)
  n <- 150
  p <- vapply(seq_len(2000), function(i) {
    heteroTests:::clearValidationCache()
    d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
    d$o <- 2 * d$x^2
    d$y <- 1 + 2 * d$x + 0.5 * d$z + d$o + exp(0.3 * d$x) * rnorm(n)
    m <- lm(y ~ x + z, data = d, offset = o)
    tryCatch(suppressWarnings(performVarianceFormTest(m)$p.value), error = function(e) NA_real_)
  }, numeric(1))

  expect_true(all(is.finite(p)))
  expect_gte(mean(p < 0.05), 0.037)
  expect_lte(mean(p < 0.05), 0.063)
})
