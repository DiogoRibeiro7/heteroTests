library(testthat)
library(heteroTests)

context("runDiagnosticPlots")

test_that("runDiagnosticPlots returns named list", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runDiagnosticPlots(m, plots = c("density", "qq"))
  expect_true(is.list(res))
  expect_named(res, c("density", "qq"))
  lapply(res, expect_ggplot)
})

test_that("runDiagnosticPlots errors on unknown plot", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_error(runDiagnosticPlots(m, plots = "foo"), "Unknown plots")
})
