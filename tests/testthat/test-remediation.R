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
