library(testthat)
library(heteroTests)


m <- lm(y ~ x1 + x2, data = data_heterosced)
results <- runHeteroTests(m, data_heterosced, tests = c("white", "breusch_pagan"))

suggest <- suggestRemediation(results)

test_that("suggestRemediation returns expected class", {
  expect_s3_class(suggest, "remediation_suggestions")
})

test_that("severity reported when heteroscedasticity detected", {
  expect_true(!is.null(suggest$severity))
})

test_that("print dispatches to the registered method", {
  expect_output(print(suggest), "Heteroscedasticity Remediation Suggestions")
  expect_output(print(suggest), "Severity:")
})

m2 <- lm(y ~ x1 + x2, data = data_homosced)
res2 <- runHeteroTests(m2, data_homosced, tests = c("white", "breusch_pagan"))

suggest2 <- suggestRemediation(res2)

test_that("no evidence returns conclusion", {
  expect_true(!is.null(suggest2$conclusion))
  expect_match(suggest2$action, "No remediation")
})

# The default battery of runHeteroTests() is White and Koenker since 0.15.0.
# Variance modelling was suggested only when "breusch_pagan" rejected, so a
# default suite would never have suggested it.
test_that("variance modelling is suggested when Koenker's test rejects", {
  fake <- function(p) structure(list(p.value = p), class = "htest")
  koenker_only <- suggestRemediation(list(white = fake(0.5), koenker = fake(0.001)))
  expect_identical(koenker_only$variance_modeling,
                   c("Weighted Least Squares", "Robust Standard Errors"))
  expect_null(koenker_only$transformations)
  bp_only <- suggestRemediation(list(white = fake(0.5), breusch_pagan = fake(0.001)))
  expect_identical(bp_only$variance_modeling, koenker_only$variance_modeling)
  neither <- suggestRemediation(list(white = fake(0.001), koenker = fake(0.5)))
  expect_null(neither$variance_modeling)
  expect_identical(neither$transformations, c("log", "sqrt"))
  missing_p <- suggestRemediation(list(white = fake(0.001), koenker = fake(NA_real_)))
  expect_null(missing_p$variance_modeling)
})
