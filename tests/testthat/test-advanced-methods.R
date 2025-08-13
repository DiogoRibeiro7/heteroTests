library(testthat)
library(heteroTests)

skip_if_not_installed("mgcv")

context("Advanced methods")

test_that("analyzeMLResiduals works", {
  data(mtcars)
  res <- analyzeMLResiduals(mpg ~ wt + qsec, mtcars)
  expect_true(is.list(res))
  expect_true("rmse_reduction" %in% names(res))
})

test_that("compareModelDiagnostics returns data frame", {
  data(mtcars)
  m1 <- lm(mpg ~ wt + qsec, mtcars)
  m2 <- lm(mpg ~ wt + hp, mtcars)
  cmp <- compareModelDiagnostics(list(m1, m2))
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 2)
})
