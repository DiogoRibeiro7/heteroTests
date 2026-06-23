library(testthat)
library(heteroTests)


test_that("downloadTeachingData handles network failures gracefully", {
  skip_if_not_installed("curl")
  asNamespace("curl")
  with_mocked_bindings(
    has_internet = function() FALSE,
    .package = "curl",
    {
      expect_message(
        result <- downloadTeachingData(quiet = FALSE),
        "No internet connection"
      )
      expect_true(isFALSE(result) || is.null(result))
    }
  )

  skip_if_offline()
  expect_silent(downloadTeachingData(quiet = TRUE))
})

test_that("functions work without internet dependency", {
  data(mtcars)
  model <- lm(mpg ~ wt, data = mtcars)

  expect_no_error(performWhiteTest(model, mtcars))
  expect_no_error(performBreuschPaganTest(model, mtcars))
  # Add tests for other core functions
})
