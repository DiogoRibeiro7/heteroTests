library(testthat)
library(heteroTests)

test_that("runDiagnostics handles spaces in column names", {
  df <- data.frame(`x one` = 1:25, y = 1:25 + rnorm(25), check.names = FALSE)
  expect_silent(suppressMessages(runDiagnostics(y ~ `x one`, df)))
})
