library(testthat)


skip_if_not_installed("quickcheck")
library(quickcheck)
factory <- get(".test_factory", envir = asNamespace("heteroTests"))
test_specs <- list(
  white = list(),
  breusch_pagan = list(),
  koenker = list(),
  student_bp = list(),
  white_bootstrap = list(B = 100L),
  szroeter = list(order_by = "x")
)

# Ensure all registered tests return valid htest objects

test_that("all tests produce valid htest", {
  quickcheck::for_all(
    n = quickcheck::integer_bounded(left = 50L, right = 120L, len = 1L),
    property = function(n) {
      data <- data.frame(x = rnorm(n), y = 1 + 2 * rnorm(n) + rnorm(n))
      model <- lm(y ~ x, data = data)
      available <- intersect(factory$get_available(), names(test_specs))
      for (test_name in available) {
        extra_args <- test_specs[[test_name]]
        args <- c(
          list(test_name = test_name, model = model, data = data),
          extra_args
        )
        res <- suppressMessages(do.call(factory$run_test, args))
        expect_s3_class(res, "htest")
        expect_true(is.numeric(res$statistic))
        expect_true(is.numeric(res$p.value))
        expect_true(res$p.value >= 0 && res$p.value <= 1)
        expect_true(is.finite(res$statistic))
        expect_true(is.finite(res$p.value))
      }
      TRUE
    },
    tests = 20L
  )
})

# Verify Type I error rates are near the nominal level for key tests

test_that("Type I error rates near nominal", {
  skip_on_cran()
  key_tests <- c("white", "breusch_pagan", "koenker")
  for (test_name in key_tests) {
    fn <- function(model, data) {
      suppressMessages(factory$run_test(test_name, model, data))
    }
    typeI <- simulate_type_I_errors(fn, n_sims = 200, n_obs = 50, alpha = 0.05)
    expect_true(abs(typeI$type_I_rate - 0.05) < 0.03,
                info = paste("Type I error rate for", test_name,
                               "is", typeI$type_I_rate))
  }
})
