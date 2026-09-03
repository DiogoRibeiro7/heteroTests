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

  # So does the WLS fit, since 0.9.0. This cell has held three different things.
  # Before 0.7.0 it silently held an NCV statistic: fitWLS() weighted by 1 / e^2,
  # which made the weighted fit perfectly explained by construction, White failed
  # the perfect-fit guard, the orchestrator substituted the then-unvalidated NCV
  # test, and the substitute was reported under the "white" column. From 0.7.0
  # the substitution was removed and the cell correctly reported NA. From 0.9.0
  # fitWLS() estimates its weights from a variance model, so the weighted fit is
  # ordinary -- R^2 about 0.90 rather than 1 -- and White runs on it validly.
  expect_true(is.numeric(cmp[2, "white"]) && is.finite(cmp[2, "white"]))
  expect_null(attr(cmp, "diagnostic_errors"))
})

test_that("fitWLS weights come from a variance model, not the raw residuals", {
  # The failure this guards against: weighting by 1 / e_i^2 hands essentially
  # all of the weight to whichever observations the initial fit reproduced most
  # closely. On quakes that was 99.5 per cent of the total weight on five of a
  # thousand points, and it made standard errors from the fit meaningless.
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  w <- weights(fitWLS(model))

  expect_length(w, nobs(model))
  expect_true(all(is.finite(w)) && all(w > 0))

  # A variance model produces weights that vary smoothly with the fitted
  # variance rather than spanning many orders of magnitude.
  expect_lt(max(w) / stats::median(w), 100)

  top5 <- sum(sort(w, decreasing = TRUE)[1:5]) / sum(w)
  expect_lt(top5, 0.5)
})

test_that("fitWLS falls back to OLS when the variance model is unusable", {
  # A fit whose residuals carry no variance signal must degrade to equal
  # weights rather than error or produce degenerate ones.
  set.seed(42)
  d <- data.frame(x = 1:40, y = 1:40 + rnorm(40, sd = 1e-8))
  m <- lm(y ~ x, data = d)
  w <- suppressWarnings(weights(fitWLS(m)))

  expect_length(w, 40L)
  expect_true(all(is.finite(w)) && all(w > 0))
})
