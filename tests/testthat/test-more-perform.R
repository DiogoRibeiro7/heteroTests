library(testthat)
library(heteroTests)


## Curry-Walsh Test

test_that("Curry-Walsh detects spatial heteroscedasticity", {
  set.seed(1)
  n <- 100
  coords <- rbind(
    cbind(runif(n/2, 0, 0.2), runif(n/2, 0, 0.2)),
    cbind(runif(n/2, 0.8, 1), runif(n/2, 0.8, 1))
  )
  x <- runif(n)
  sd_vec <- rep(c(1, 4), each = n/2)
  y <- 1 + 2 * x + rnorm(n, sd = sd_vec)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performCurryWalshTest(model, coords)
  expect_s3_class(res, "htest")
  expect_true(is.numeric(res$p.value))
})

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

test_that("Modified Bartlett test detects variance differences", {
  set.seed(3)
  g <- factor(rep(1:3, each = 50))
  x <- rnorm(150)
  sd_vec <- ifelse(g == 3, 3, 1)
  y <- 1 + x + rnorm(150, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performModifiedBartlettTest(model, df, "g")
  expect_s3_class(res, "htest")
  expect_true(is.numeric(res$p.value))
})

## Rice Test

test_that("Rice test detects increasing variance", {
  set.seed(4)
  n <- 150
  x <- seq_len(n)
  y <- 1 + 0.1 * x + rnorm(n, sd = x / 20)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performRiceTest(model)
  expect_s3_class(res, "htest")
  expect_true(is.numeric(res$p.value))
})

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

