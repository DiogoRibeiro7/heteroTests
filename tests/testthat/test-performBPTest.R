library(testthat)
library(heteroTests)

test_that("performBPTest enforces sample size minimum", {
  set.seed(24)
  df <- data.frame(
    y = rnorm(12),
    x1 = rnorm(12),
    x2 = rnorm(12)
  )
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performBPTest(model, df),
    "requires at least 15"
  )
})

test_that("performBPTest warns about missing values and succeeds", {
  set.seed(99)
  n <- 40
  df <- data.frame(
    y = 1 + rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  df$x1[1] <- NA
  df$x2[3] <- NA
  model <- lm(y ~ x1 + x2, data = df)
  expect_warning(
    res <- performBPTest(model, df),
    "Removed 2 observations due to missing values"
  )
  expect_s3_class(res, "htest")
})

test_that("performBPTest detects insufficient residual variation", {
  set.seed(56)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rnorm(n, sd = 1e-8)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performBPTest(model, df),
    "residual variation greater than numerical precision"
  )
})

test_that("performBPTest aborts for perfect fits", {
  df <- data.frame(
    x = seq_len(20),
    z = rnorm(20)
  )
  df$y <- 1 + 2 * df$x + 3 * df$z
  model <- lm(y ~ x + z, data = df)
  expect_error(
    performBPTest(model, df),
    "Model has perfect fit"
  )
})
