library(testthat)
library(heteroTests)


test_that("runHeteroTests returns list with default tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(m, mtcars)
  expect_true(is.list(res))
  expect_equal(names(res), c("white", "breusch_pagan"))
  lapply(res, expect_s3_class, class = "htest")
})

test_that("runHeteroTests runs specified tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
  expect_equal(names(res), c("white", "koenker", "cook_weisberg"))
  lapply(res, expect_s3_class, class = "htest")
})
