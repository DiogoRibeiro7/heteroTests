library(testthat)
library(heteroTests)

context("Additional diagnostics")

test_that("VIF diagnostic returns numeric vector", {
  data(mtcars)
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- performVIFDiagnostic(model)
  expect_type(res, "double")
  expect_true(all(res >= 1))
})

test_that("RESET test detects nonlinearity", {
  set.seed(1)
  x <- rnorm(200)
  y <- 1 + x + 0.5 * x^2 + rnorm(200)
  model <- lm(y ~ x)
  res <- performRESETTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("Influence diagnostics flag outliers", {
  data(mtcars)
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  inf <- performInfluenceDiagnostics(model)
  expect_true(is.numeric(inf$cooks_distance))
  expect_true(is.numeric(inf$cutoff))
})

test_that("runDiagnostics returns list", {
  data(mtcars)
  model <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runDiagnostics(model, mtcars)
  expect_type(res, "list")
  expect_true("vif" %in% names(res))
  expect_true("reset" %in% names(res))
  expect_true("influence" %in% names(res))
})

context("Custom diagnostic registry")

test_that("registerDiagnostic adds new test", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  custom <- function(model, data) list(stat = 1)
  registerDiagnostic("custom_test", custom)
  res <- runHeteroTests(m, mtcars, tests = c("white", "custom_test"))
  expect_equal(names(res), c("white", "custom_test"))
  expect_equal(res$custom_test$stat, 1)
})

test_that("fitWLS returns lm", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  wls <- fitWLS(m)
  expect_s3_class(wls, "lm")
  expect_true(!is.null(wls$weights))
})

test_that("fitRobust returns rlm", {
  data(mtcars)
  m <- fitRobust(mpg ~ wt + qsec, mtcars)
  expect_s3_class(m, "rlm")
})

test_that("autoTransform chooses a method", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- autoTransform(m)
  expect_true(res$method %in% c("none", "log", "sqrt", "boxcox"))
  expect_s3_class(res$model, "lm")
})
