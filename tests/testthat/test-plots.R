library(testthat)
library(heteroTests)

context("Diagnostic plotting")

test_that("plotResidualsFitted returns ggplot", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  p <- plotResidualsFitted(model)
  expect_s3_class(p, "ggplot")
})

test_that("plotSpreadLevel returns ggplot", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  p <- plotSpreadLevel(model)
  expect_s3_class(p, "ggplot")
})

test_that("plotDiagnosticSuite returns list of plots", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  res <- plotDiagnosticSuite(model)
  expect_true(is.list(res))
  expect_named(res, c("residuals_fitted", "spread_level",
                      "density", "qq", "bubble_variance"))
  lapply(res, expect_s3_class, class = "ggplot")
})

test_that("plotBeforeAfter overlays models", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  wls <- fitWLS(model)
  p <- plotBeforeAfter(model, wls)
  expect_s3_class(p, "ggplot")
})

test_that("additional plot functions return ggplot", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  expect_s3_class(plotResidualDensity(model), "ggplot")
  expect_s3_class(plotResidualQQ(model), "ggplot")
  expect_s3_class(plotBubbleVariance(model), "ggplot")
})


test_that("plotResidualsFittedEnhanced returns ggplot", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  p <- plotResidualsFittedEnhanced(model)
  expect_s3_class(p, "ggplot")
})

test_that("plotDiagnosticSuiteEnhanced returns list of plots", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  res <- plotDiagnosticSuiteEnhanced(model)
  expect_true(is.list(res))
  expect_named(res, c("residuals_fitted", "spread_level",
                      "density", "qq", "bubble_variance"))
  lapply(res, expect_s3_class, class = "ggplot")
})

test_that("cachedTest stores results", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  clearTestCache()
  res1 <- cachedTest("white", model, data_heterosced)
  res2 <- cachedTest("white", model, data_heterosced)
  expect_identical(res1, res2)
})
