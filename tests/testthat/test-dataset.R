library(testthat)
context("Package dataset")

test_that("hetero_data is available and has correct structure", {
  data(hetero_data, package = "heteroTests")
  expect_true(is.data.frame(hetero_data))
  expect_equal(ncol(hetero_data), 2)
  expect_equal(colnames(hetero_data), c("x", "y"))
  expect_equal(nrow(hetero_data), 100)
})

test_that("diagnostic_data loads", {
  data(diagnostic_data, package = "heteroTests")
  expect_true(is.data.frame(diagnostic_data))
  expect_equal(ncol(diagnostic_data), 3)
})
