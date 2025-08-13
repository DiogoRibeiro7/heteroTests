library(heteroTests)

test_that("performBoxMTest works on iris", {
  res <- performBoxMTest(iris[,1:4], iris$Species)
  expect_s3_class(res, "htest")
  expect_true(res$p.value <= 1 && res$p.value >= 0)
})
