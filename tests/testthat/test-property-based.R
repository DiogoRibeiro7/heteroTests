library(testthat)
if (!requireNamespace("quickcheck", quietly = TRUE)) {
  skip("quickcheck not installed")
} else {
  library(quickcheck)
}

context("Property-based checks")

# Example property: Weighted least squares should reduce residual variance

test_that("wls reduces variance", {
  for_all(
    df = data_frame_(x = numeric_(len = 20), y = numeric_(len = 20)),
    property = function(df) {
      df$y <- df$y + rnorm(nrow(df), 0, 0.1)
      fit1 <- lm(y ~ x, df)
      wls <- fitWLS(fit1)
      expect_true(
        var(residuals(wls)) <= var(residuals(fit1)) + 1e-8
      )
    },
    tests = 10L
  )
})
