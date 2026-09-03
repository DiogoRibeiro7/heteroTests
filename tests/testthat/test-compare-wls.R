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
  # variance rather than spanning many orders of magnitude. Compare the full
  # range: a median-based ratio would miss a single extreme *small* weight,
  # which concentrates the fit just as effectively as an extreme large one.
  expect_lt(max(w) / min(w), 100)

  top5 <- sum(sort(w, decreasing = TRUE)[1:5]) / sum(w)
  expect_lt(top5, 0.5)
})

test_that("fitWLS falls back to equal weights when the variance model is flat", {
  # Deterministic: an exactly linear response leaves residuals at floating-point
  # zero, so the logged squared residuals are constant, the auxiliary fit has no
  # slope, and there is no variance signal to weight by. The result must be the
  # unweighted fit, not merely some finite positive weights -- assert equality
  # rather than plausibility, or the test passes on any successful run.
  d <- data.frame(x = 1:40)
  d$y <- 3 + 2 * d$x
  m <- lm(y ~ x, data = d)
  w <- suppressWarnings(weights(fitWLS(m)))

  expect_equal(w, rep(1, 40L))
})

test_that("fitWLS handles na.exclude models", {
  # residuals(model) is padded back to the original row count under
  # na.exclude while the model matrix covers only the fitted rows, so taking
  # weights from it made lm.wfit() fail with "incompatible dimensions".
  set.seed(1)
  d <- data.frame(x = runif(60, 1, 5))
  d$y <- 1 + 2 * d$x + rnorm(60, sd = 0.5 * d$x)
  d$y[7] <- NA

  fit_ex <- fitWLS(lm(y ~ x, data = d, na.action = na.exclude))
  fit_om <- fitWLS(lm(y ~ x, data = d))

  # The weights used in the fit cover the 59 complete rows.
  expect_length(fit_ex$weights, 59L)

  # weights() then pads back to the original 60 under na.exclude, which is the
  # point of that na.action; the hole sits at the row that was dropped.
  expect_length(weights(fit_ex), 60L)
  expect_equal(unname(which(is.na(weights(fit_ex)))), 7L)

  # Dropping the row either way must give the same fit.
  expect_equal(unname(coef(fit_ex)), unname(coef(fit_om)))
})
