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

