library(testthat)
library(heteroTests)

context("Comparison diagnostics")

test_that("Weighted least squares improves test statistics", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  wls <- fitWLS(model)
  cmp <- compareModelDiagnostics(
    list(model, wls),
    data = data_heterosced,
    tests = c("white")
  )
  col <- grep("^white", names(cmp), value = TRUE)
  expect_true(is.numeric(cmp[2, col]) && is.numeric(cmp[1, col]))
})
