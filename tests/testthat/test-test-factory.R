
library(heteroTests)

test_that("TestFactory registers and runs tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)

  tmp_factory <- TestFactory$new()
  tmp_factory$register("white", performWhiteTest)

  res <- tmp_factory$run_test("white", m, mtcars)

  expect_s3_class(res, "htest")
  expect_true("test_metadata" %in% names(res))
})
