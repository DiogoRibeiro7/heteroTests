library(testthat)
library(heteroTests)

context("Time-series and panel diagnostics")

test_that("runTimeSeriesTests works", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, mtcars)
  res <- runTimeSeriesTests(m, lags = 2)
  expect_type(res, "list")
  expect_length(res, 2)
  expect_s3_class(res[[1]], "htest")
})

test_that("runPanelTests works", {
  df <- data.frame(id = rep(1:3, each = 4), time = rep(1:4, 3),
                   x = runif(12), y = rnorm(12))
  m <- lm(y ~ x, df)
  res <- runPanelTests(m, df, id = "id", time = "time")
  expect_type(res, "list")
  expect_length(res, 2)
  expect_s3_class(res[[1]], "htest")
})
