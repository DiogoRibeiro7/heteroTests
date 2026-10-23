library(testthat)


test_that("performQuantileRegressionTest matches quantreg joint slope test", {
  skip_if_not_installed("quantreg")

  set.seed(404)
  n <- 180
  x1 <- rnorm(n)
  x2 <- runif(n, -1, 1)
  y <- 1 + 0.8 * x1 - 0.4 * x2 + rnorm(n, sd = 0.7 + 0.5 * abs(x1))
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  model <- lm(y ~ x1 + x2, data = df)
  taus <- c(0.25, 0.5, 0.75)

  ours <- performQuantileRegressionTest(
    model,
    df,
    taus = taus,
    se_type = "nid",
    iid = TRUE
  )

  reference_fit <- quantreg::rq(y ~ x1 + x2, tau = taus, data = df)
  reference <- stats::anova(reference_fit, se = "nid", iid = TRUE, joint = TRUE)
  reference_table <- reference$table

  expect_equal(unname(ours$statistic), as.numeric(reference_table[1, 3]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter[["df1"]]), as.numeric(reference_table[1, 1]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter[["df2"]]), as.numeric(reference_table[1, 2]), tolerance = 1e-10)
  expect_equal(unname(ours$p.value), as.numeric(reference_table[1, 4]), tolerance = 1e-10)
  expect_equal(ours$quantiles, taus)
})


test_that("performQuantileRegressionTest validates quantile inputs", {
  skip_if_not_installed("quantreg")

  data <- mtcars
  model <- lm(mpg ~ wt + hp, data = data)

  expect_error(
    performQuantileRegressionTest(model, data, taus = 0.5),
    "at least two"
  )
  expect_error(
    performQuantileRegressionTest(model, data, taus = c(0, 0.75)),
    "strictly between 0 and 1"
  )
  expect_error(
    performQuantileRegressionTest(model, data, taus = c(0.5, 0.5)),
    "distinct quantiles"
  )
  expect_error(
    performQuantileRegressionTest(model, data, iid = NA),
    "single non-missing logical"
  )
})

test_that("performQuantileRegressionTest matches quantreg on a model with one regressor", {
  skip_if_not_installed("quantreg")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Before 0.16.0 such a model stopped with "subscript out of bounds": the
  # slope of a one-column design was named "", and was read by name. Through
  # runHeteroTests() the failure was masked by the fallback, the rank
  # permutation test, returned under the name "quantile_regression".
  model <- lm(stations ~ mag, data = quakes)
  ours <- performQuantileRegressionTest(model, quakes)

  reference_fit <- quantreg::rq(stations ~ mag, tau = c(0.25, 0.75), data = quakes)
  reference <- stats::anova(reference_fit, se = "nid", iid = TRUE, joint = TRUE)$table
  expect_equal(unname(ours$statistic), as.numeric(reference[1, 3]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter[["df1"]]), as.numeric(reference[1, 1]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter[["df2"]]), as.numeric(reference[1, 2]), tolerance = 1e-10)
  expect_equal(unname(ours$p.value), as.numeric(reference[1, 4]), tolerance = 1e-10)
  expect_identical(dimnames(ours$estimate), list("mag", c("tau_0.25", "tau_0.75")))
  expect_equal(ours$estimate["mag", ], stats::coef(reference_fit)["mag", ],
               tolerance = 1e-10, ignore_attr = TRUE)

  # The runner returns the test, not its fallback, and so does the factory.
  suite <- runHeteroTests(model, quakes, tests = "quantile_regression", use_cache = FALSE)
  expect_identical(attr(suite$quantile_regression, "diagnostic"), "quantile_regression")
  expect_identical(suite$quantile_regression$method, ours$method)
  expect_identical(suite$quantile_regression$statistic, ours$statistic)
  expect_identical(suite$quantile_regression$p.value, ours$p.value)
  factory <- test_factory$run_test("quantile_regression", model, quakes)
  factory$test_metadata <- NULL
  expect_identical(factory, ours)
})

test_that("performQuantileRegressionTest refuses a model without a slope", {
  skip_if_not_installed("quantreg")
  # It compares the slopes across quantiles; before 0.16.0 an intercept-only
  # model stopped with "argument is of length zero".
  expect_error(
    performQuantileRegressionTest(lm(stations ~ 1, data = quakes), quakes),
    "needs a regressor beyond the intercept",
    fixed = TRUE
  )
})
