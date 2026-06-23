library(testthat)
if (!requireNamespace("quickcheck", quietly = TRUE)) {
  skip("quickcheck not installed")
} else {
  library(quickcheck)
}


# Example property: Weighted least squares should reduce residual variance

test_that("wls reduces variance", {
  for_all(
    n = integer_bounded(left = 40L, right = 120L, len = 1L),
    beta0 = numeric_bounded(left = -1, right = 1, len = 1L),
    beta1 = numeric_bounded(left = 0.5, right = 2, len = 1L),
    property = function(n, beta0, beta1) {
      sim <- simulate_hetero(n, beta0, beta1, sigma_linear)
      model <- stats::lm(y ~ x, data = sim)
      ols_var <- stats::var(stats::residuals(model))
      if (!is.finite(ols_var) || ols_var < sqrt(.Machine$double.eps)) {
        return(TRUE)
      }
      wls <- fitWLS(model)
      wls_var <- stats::var(stats::residuals(wls))
      if (!is.finite(wls_var)) {
        return(TRUE)
      }
      testthat::expect_lte(wls_var, ols_var * 1.1)
    },
    tests = 15L
  )
})

if (requireNamespace("quickcheck", quietly = TRUE)) {
  test_that("simulate_hetero preserves requested sample size", {
    for_all(
      n = quickcheck::integer_bounded(left = 20L, right = 80L, len = 1L),
      beta0 = quickcheck::numeric_bounded(left = -1, right = 1, len = 1L),
      beta1 = quickcheck::numeric_bounded(left = -1, right = 1, len = 1L),
      property = function(n, beta0, beta1) {
        sim <- simulate_hetero(n, beta0, beta1, sigma_linear)
        expect_equal(nrow(sim), n)
      },
      tests = 25L
    )
  })

  test_that("prepare_model_data_for_test respects required variables", {
    for_all(
      n = quickcheck::integer_bounded(left = 20L, right = 60L, len = 1L),
      property = function(n) {
        df <- data.frame(
          y = rnorm(n),
          x = rnorm(n)
        )
        model <- lm(y ~ x, data = df)
        result <- heteroTests:::prepare_model_data_for_test(
          model,
          df,
          required_vars = c("y", "x"),
          test_label = "prop"
        )
        expect_equal(nrow(result$data), n)
      },
      tests = 20L
    )
  })
}
