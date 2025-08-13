library(testthat)
library(heteroTests)

context("Invalid inputs")

test_that("runDiagnostics requires data with formula", {
  expect_error(runDiagnostics(mpg ~ wt, data = NULL), "`data` must be supplied")
})

test_that("HeteroDiagnostic requires data with formula", {
  expect_error(HeteroDiagnostic(mpg ~ wt, data = NULL), "`data` must be supplied")
})

test_that("runDiagnostics errors on bad data", {
  expect_error(runDiagnostics(mpg ~ wt, data = 1), "data.frame")
})

test_that("HeteroDiagnostic errors on bad data", {
  expect_error(HeteroDiagnostic(mpg ~ wt, data = 1), "data.frame")
})

test_that("runDiagnostics errors on non-model", {
  expect_error(runDiagnostics(1), "lm")
})

test_that("HeteroDiagnostic errors on non-model", {
  expect_error(HeteroDiagnostic(1), "lm")
})

# integration

test_that("HeteroDiagnostic uses runDiagnostics", {
  data(mtcars)
  res1 <- runDiagnostics(mpg ~ wt + qsec, mtcars)
  d <- HeteroDiagnostic(mpg ~ wt + qsec, mtcars)
  res2 <- test(d)
  expect_equal(names(res1), names(res2))
})

test_that("runHeteroTests rejects unknown tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_error(runHeteroTests(m, mtcars, tests = "bogus"), "Unknown tests")
})
