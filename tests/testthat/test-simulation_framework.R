context("Simulation framework")

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
