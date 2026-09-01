library(testthat)
library(heteroTests)


## Curry-Walsh Test

## Davidian-Carroll Test

test_that("Davidian-Carroll test detects variance structure", {
  set.seed(2)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 3 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performDavidianCarrollTest(model)
  expect_s3_class(res, "htest")
  expect_true(is.numeric(res$p.value))
})

## Modified Bartlett Test

## Rice Test

## Spread-Level Test

test_that("Spread-Level test detects heteroscedasticity", {
  set.seed(5)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 4 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performSpreadLevelTest(model)
  expect_s3_class(res, "htest")
  expect_true(is.numeric(res$p.value))
})

test_that("Spread-Level test handles negative fitted values", {
  set.seed(6)
  n <- 100
  x <- rnorm(n)
  y <- -1 + 0.5 * x + rnorm(n)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performSpreadLevelTest(model)
  expect_true(is.finite(res$statistic))
})

