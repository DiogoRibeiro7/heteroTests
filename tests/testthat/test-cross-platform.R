library(testthat)
library(heteroTests)

test_that("runDiagnostics handles spaces in column names", {
  df <- data.frame(`x one` = 1:10, y = 1:10 + rnorm(10), check.names = FALSE)
  expect_silent(runDiagnostics(y ~ `x one`, df))
})
