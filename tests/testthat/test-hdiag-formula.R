library(testthat)
library(heteroTests)

test_that("HeteroDiagnostic handles formulas", {
  data(mtcars)
  d <- HeteroDiagnostic(mpg ~ wt + qsec, mtcars)
  expect_s3_class(d, "HeteroDiagnostic")
  res <- test(d)
  expect_type(res, "list")
  expect_true("vif" %in% names(res))
  expect_true("reset" %in% names(res))
  expect_true("influence" %in% names(res))
  plots <- plot(d)
  expect_true(is.list(plots))
  summ <- summary(d)
  expect_true(is.numeric(summ))
})

