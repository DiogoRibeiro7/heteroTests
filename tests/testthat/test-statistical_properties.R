library(testthat)

context("Statistical properties via quickcheck")

skip_if_not_installed("quickcheck")
library(quickcheck)

# Ensure all registered tests return valid htest objects

test_that("all tests produce valid htest", {
  quickcheck::forall(
    n = quickcheck::qinteger(min = 20, max = 100),
    {
      data <- data.frame(x = rnorm(n), y = 1 + 2 * rnorm(n) + rnorm(n))
      model <- lm(y ~ x, data = data)
      tests <- .test_factory$get_available()
      for (test_name in tests) {
        res <- .test_factory$run_test(test_name, model, data)
        expect_s3_class(res, "htest")
        expect_true(is.numeric(res$statistic))
        expect_true(is.numeric(res$p.value))
        expect_true(res$p.value >= 0 && res$p.value <= 1)
        expect_true(is.finite(res$statistic))
        expect_true(is.finite(res$p.value))
      }
    }
  )
})

# Verify Type I error rates are near the nominal level for key tests

test_that("Type I error rates near nominal", {
  skip_on_cran()
  key_tests <- c("white", "breusch_pagan", "koenker")
  for (test_name in key_tests) {
    fn <- function(model, data) {
      .test_factory$run_test(test_name, model, data)
    }
    typeI <- simulate_type_I_errors(fn, n_sims = 200, n_obs = 50, alpha = 0.05)
    expect_true(abs(typeI$type_I_rate - 0.05) < 0.03,
                info = paste("Type I error rate for", test_name,
                               "is", typeI$type_I_rate))
  }
})
