library(testthat)
library(heteroTests)


test_that("compareModelDiagnostics contrasts OLS and WLS fits", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  wls <- fitWLS(model)

  cmp <- suppressWarnings(compareModelDiagnostics(
    list(OLS = model, WLS = wls),
    data = data_heterosced,
    tests = c("white")
  ))

  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 2L)
  # The OLS fit yields a finite White statistic.
  expect_true(is.numeric(cmp[1, "white"]) && is.finite(cmp[1, "white"]))
  # fitWLS() weights by 1 / e^2, so the weighted fit is perfectly explained by
  # construction (R^2 ~ 1) and no heteroscedasticity diagnostic can validly run
  # on it. The comparison must therefore report NA. Before 0.7.0 this cell
  # silently held an NCV statistic: White failed the perfect-fit guard, the
  # orchestrator substituted the then-unvalidated NCV test, and the substitute
  # was reported under the "white" column.
  expect_true(is.na(cmp[2, "white"]))
  expect_false(is.null(attr(cmp, "diagnostic_errors")))
})
