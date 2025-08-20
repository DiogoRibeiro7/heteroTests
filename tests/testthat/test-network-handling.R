library(testthat)
library(heteroTests)

context("Network handling")

test_that("downloadTeachingData handles network failures gracefully", {
  skip_if_not_installed("curl")
  with_mocked_bindings(
    `curl::has_internet` = function() FALSE,
    {
      expect_message(
        result <- downloadTeachingData(quiet = FALSE),
        "No internet connection"
      )
      expect_false(result)
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
