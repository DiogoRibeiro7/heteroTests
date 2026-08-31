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
