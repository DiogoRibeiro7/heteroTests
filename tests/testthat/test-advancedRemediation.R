
library(heteroTests)

test_that("autoCompareRemediations returns expected structure", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- autoCompareRemediations(m)
  expect_type(res, "list")
  expect_true(all(c("models", "metrics", "best") %in% names(res)))
  expect_s3_class(res$metrics, "data.frame")
  expect_true(any(res$metrics$Recommended))
})
