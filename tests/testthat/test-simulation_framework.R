
library(heteroTests)

basic_test <- function(model, data) {
  performWhiteTest(model, data)
}

test_that("simulate_type_I_errors returns expected structure", {
  res <- simulate_type_I_errors(basic_test, n_sims = 10, n_obs = 20, seed = 1)
  expect_true(is.list(res))
  expect_true("type_I_rate" %in% names(res))
  expect_true(res$type_I_rate >= 0 && res$type_I_rate <= 1)
})

test_that("simulate_power_analysis returns data frame", {
  res <- simulate_power_analysis(basic_test,
                                sigma_functions = list(sigma_linear),
                                effect_sizes = c(0.5),
                                n_sims = 5,
                                n_obs = 20)
  expect_s3_class(res, "power_analysis")
  expect_true("power" %in% names(res))
  expect_equal(nrow(res), 1)
})

test_that("simulate_hetero produces variance patterns consistent with sigma", {
  set.seed(111)
  sim <- simulate_hetero(400, beta0 = 1, beta1 = 2, sigma_func = sigma_linear)
  model <- lm(y ~ x, data = sim)
  corr <- suppressWarnings(cor(abs(residuals(model)), sim$x))
  expect_gt(as.numeric(corr)[1], 0.25)
})

test_that("simulate_arch1 approximates theoretical unconditional variance", {
  set.seed(222)
  sim <- simulate_arch1(1200, alpha0 = 0.4, alpha1 = 0.5)
  observed <- mean((sim[["sigma"]])^2)
  theoretical <- 0.4 / (1 - 0.5)
  expect_lt(abs(observed - theoretical), 0.15)
})

test_that("power increases with effect size in simulation framework", {
  skip_on_cran()
  set.seed(333)
  res <- simulate_power_analysis(
    function(model, data) performWhiteTest(model, data),
    sigma_functions = list(sigma_linear),
    effect_sizes = c(0.1, 0.4),
    n_sims = 150,
    n_obs = 80,
    alpha = 0.05
  )
  expect_lt(res$power[1], res$power[2])
})
