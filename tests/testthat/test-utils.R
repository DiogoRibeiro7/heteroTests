library(testthat)
context("Input validation")

test_that("checkModel errors on invalid input", {
  expect_error(heteroTests:::checkModel(1), "Model must be fitted")
})

test_that("checkData errors on invalid input", {
  expect_error(heteroTests:::checkData(1), "data.frame")
})

test_that("validateTestInputs detects small datasets", {
  df <- data.frame(x = 1:5, y = 1:5)
  model <- lm(y ~ x, data = df)
  expect_error(
    heteroTests:::validateTestInputs(model, df, "white", min_obs = 10),
    "Insufficient observations"
  )
})

test_that("checkModelEnhanced warns on near-perfect fit", {
  df <- data.frame(x = 1:10, y = 1:10)
  model <- lm(y ~ x, data = df)
  expect_warning(heteroTests:::checkModelEnhanced(model), "Near-perfect fit")
})

test_that("safe_var handles near-zero variance with warning", {
  expect_warning(
    val <- heteroTests:::safe_var(rep(1, 5)),
    "Near-zero variance detected"
  )
  expect_equal(val, .Machine$double.eps)
})

test_that("safe_var returns variance for regular input", {
  x <- c(1, 2, 3, 4)
  expect_silent(v <- heteroTests:::safe_var(x))
  expect_equal(v, var(x))
})

test_that("safe_var handles NA values", {
  x <- c(1, 1, NA, 1)
  expect_warning(v <- heteroTests:::safe_var(x), "Near-zero variance detected")
  expect_equal(v, .Machine$double.eps)
})

test_that("safe_var errors on non-numeric input", {
  expect_error(heteroTests:::safe_var("a"), "must be numeric")
})

test_that("safe_lm logs and rethrows errors", {
  df <- data.frame(x = 1:3, y = 1:3)
  expect_s3_class(heteroTests:::safe_lm(y ~ x, df), "lm")
  expect_error(heteroTests:::safe_lm(y ~ z, df), "z")
})
