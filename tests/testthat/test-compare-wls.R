library(testthat)
library(heteroTests)


test_that("compareModelDiagnostics contrasts OLS and WLS fits", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  wls <- fitWLS(model)

  cmp <- compareModelDiagnostics(
    list(OLS = model, WLS = wls),
    data = data_heterosced,
    tests = c("white")
  )

  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 2L)
  # Both fits produce a finite White statistic ...
  expect_true(is.numeric(cmp[1, "white"]) && is.finite(cmp[1, "white"]))
  expect_true(is.numeric(cmp[2, "white"]) && is.finite(cmp[2, "white"]))
  # ... and weighting the heteroscedastic model reduces the detected
  # heteroscedasticity relative to the OLS fit.
  expect_lt(cmp[2, "white"], cmp[1, "white"])
})
