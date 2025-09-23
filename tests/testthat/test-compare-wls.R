library(testthat)
library(heteroTests)

context("Comparison diagnostics")

test_that("Weighted least squares models propagate validation failures", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  wls <- fitWLS(model)

  expect_warning(
    cmp <- compareModelDiagnostics(
      list(model, wls),
      data = data_heterosced,
      tests = c("white")
    ),
    "Diagnostics for model 2 failed: Model appears perfectly explained",
    fixed = TRUE
  )

  expect_s3_class(cmp, "data.frame")
  expect_true(is.numeric(cmp[1, "white"]))
  expect_true(is.na(cmp[2, "white"]))

  diag_errors <- attr(cmp, "diagnostic_errors")
  expect_type(diag_errors, "list")
  expect_match(diag_errors[[2]], "Model appears perfectly explained", fixed = TRUE)
})
