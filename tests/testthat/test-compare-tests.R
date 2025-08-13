library(testthat)
library(heteroTests)

context("compareTestResults")

test_that("compareTestResults summarises diagnostics", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  cmp <- compareTestResults(model, tests = c("white", "breusch_pagan"))
  expect_true(all(c("test", "statistic", "p.value") %in% colnames(cmp)))
  expect_equal(nrow(cmp), 2)
  expect_lt(cmp$p.value[1], 0.05)
})
