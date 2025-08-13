library(testthat)
library(heteroTests)

test_that("runMultivariateTests runs BoxM", {
  res <- runMultivariateTests(iris[,1:4], iris$Species)
  expect_type(res, "list")
  expect_s3_class(res[[1]], "htest")
})

