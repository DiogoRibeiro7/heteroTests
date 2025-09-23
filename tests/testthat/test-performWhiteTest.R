library(testthat)
library(heteroTests)

test_that("White test works on homoscedastic data", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performWhiteTest(model, df)
  expect_s3_class(res, "htest")
  expect_gt(res$p.value, 0.05)
})

test_that("White test detects heteroscedasticity", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performWhiteTest(model, df)
  expect_lt(res$p.value, 0.05)
})

test_that("Cross products can be disabled", {
  set.seed(1)
  n <- 100
  x1 <- runif(n)
  x2 <- runif(n)
  y <- 1 + 2 * x1 + 3 * x2 + rnorm(n)
  df <- data.frame(x1 = x1, x2 = x2, y = y)
  model <- lm(y ~ x1 + x2, data = df)
  res_cp <- performWhiteTest(model, df, cross_products = TRUE)
  res_no <- performWhiteTest(model, df, cross_products = FALSE)
  expect_true(res_cp$statistic >= res_no$statistic)
})

test_that("cross_products must be logical", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_error(performWhiteTest(m, mtcars, cross_products = "yes"),
               "cross_products")
})

test_that("performWhiteTest enforces sample size minimum", {
  set.seed(42)
  df <- data.frame(
    y = rnorm(18),
    x1 = rnorm(18),
    x2 = rnorm(18)
  )
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performWhiteTest(model, df),
    "requires at least 20"
  )
})

test_that("performWhiteTest warns about missing values and succeeds", {
  set.seed(123)
  n <- 40
  df <- data.frame(
    y = 1 + rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  df$x1[1] <- NA
  df$x2[5] <- NA
  model <- lm(y ~ x1 + x2, data = df)
  expect_warning(
    res <- performWhiteTest(model, df),
    "Removed 2 observations due to missing values"
  )
  expect_s3_class(res, "htest")
})

test_that("performWhiteTest flags rank-deficient auxiliary regression", {
  set.seed(321)
  n <- 30
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- x1 * x2
  y <- 1 + 2 * x1 + 0.5 * x2 + 0.2 * x3 + rnorm(n)
  df <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3)
  model <- lm(y ~ x1 + x2 + x3, data = df)
  expect_error(
    performWhiteTest(model, df, cross_products = TRUE),
    "Auxiliary regression became rank deficient"
  )
  expect_s3_class(
    performWhiteTest(model, df, cross_products = FALSE),
    "htest"
  )
})

test_that("performWhiteTest aborts for perfect fits", {
  df <- data.frame(
    x = seq_len(25),
    y = 3 + 2 * seq_len(25)
  )
  model <- lm(y ~ x, data = df)
  expect_error(
    performWhiteTest(model, df),
    "Residual variance is too small to evaluate White",
    fixed = FALSE
  )
})

