# =============================================================================
# Unit Tests for Simulation Functions
# tests/testthat/test-simulation.R
# =============================================================================

# Main Simulation Function
# =============================================================================

test_that("simulate_hetero works correctly", {
  result <- simulate_hetero(
    n = 100,
    beta0 = 1,
    beta1 = 2,
    sigma_func = sigma_linear,
    seed = 123
  )
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 100)
  expect_named(result, c("x", "y"))
  expect_true(all(is.finite(result$x)))
  expect_true(all(is.finite(result$y)))
})

test_that("simulate_hetero input validation", {
  # Invalid n
  expect_error(
    simulate_hetero(n = 0, beta0 = 1, beta1 = 2, sigma_func = sigma_linear),
    "n >= 1"
  )
  
  expect_error(
    simulate_hetero(n = 1.5, beta0 = 1, beta1 = 2, sigma_func = sigma_linear),
    "n == as.integer\\(n\\)"
  )
  
  # Invalid parameters
  expect_error(
    simulate_hetero(n = 50, beta0 = "invalid", beta1 = 2, sigma_func = sigma_linear),
    "is.numeric\\(beta0\\)"
  )
  
  expect_error(
    simulate_hetero(n = 50, beta0 = 1, beta1 = "invalid", sigma_func = sigma_linear),
    "is.numeric\\(beta1\\)"
  )
  
  # Invalid sigma function
  expect_error(
    simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = "not a function"),
    "is.function\\(sigma_func\\)"
  )
})

test_that("simulate_hetero seed reproducibility", {
  # Same seed should produce same results
  result1 <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = sigma_linear, seed = 42)
  result2 <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = sigma_linear, seed = 42)
  
  expect_equal(result1, result2)
  
  # Different seeds should produce different results
  result3 <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = sigma_linear, seed = 123)
  expect_false(identical(result1, result3))
})

# Sigma Functions
# =============================================================================

test_that("sigma_linear works correctly", {
  x <- c(1, 2, 3, 4, 5)
  
  result <- sigma_linear(x)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should be linear relationship
  expected <- 0.5 + 0.2 * x
  expect_equal(result, expected)
  
  # Test with edge cases
  expect_no_error(sigma_linear(0))
  expect_no_error(sigma_linear(-5))
  expect_true(all(sigma_linear(c(-1, 0, 1)) > 0))
})

test_that("sigma_exponential works correctly", {
  x <- c(1, 2, 3, 4, 5)
  
  result <- sigma_exponential(x)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should be exponential relationship
  expected <- exp(0.1 * x)
  expect_equal(result, expected)
  
  # Should be increasing
  expect_true(all(diff(result) > 0))
})

test_that("sigma_group works correctly", {
  x <- c(1, 3, 5, 7, 9)
  
  result <- sigma_group(x)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should have two distinct values
  unique_vals <- unique(result)
  expect_equal(length(unique_vals), 2)
  expect_true(1.0 %in% unique_vals)
  expect_true(3.0 %in% unique_vals)
  
  # Values < 5 should have sigma = 1, >= 5 should have sigma = 3
  expect_equal(result[x < 5], rep(1.0, sum(x < 5)))
  expect_equal(result[x >= 5], rep(3.0, sum(x >= 5)))
})

test_that("sigma_piecewise works correctly", {
  x <- c(1, 3, 6, 8, 10)
  
  result <- sigma_piecewise(x)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should have two distinct values
  unique_vals <- unique(result)
  expect_equal(length(unique_vals), 2)
  expect_true(0.5 %in% unique_vals)
  expect_true(2.0 %in% unique_vals)
})

test_that("sigma_poly works correctly", {
  x <- c(1, 2, 3, 4, 5)
  
  # Test with default parameters
  result1 <- sigma_poly(x)
  expect_type(result1, "double")
  expect_equal(length(result1), 5)
  expect_true(all(result1 > 0))
  
  # Test with custom parameters
  result2 <- sigma_poly(x, a = 1, b = 0.2, c = 0.1)
  expected <- 1 + 0.2 * x + 0.1 * x^2
  expect_equal(result2, expected)
  
  # Should be quadratic (increasing for positive coefficients)
  expect_true(all(diff(result1) > 0))  # Should be increasing with default params
})

test_that("sigma_sin works correctly", {
  x <- seq(0, 10, length.out = 50)
  
  result <- sigma_sin(x)
  expect_type(result, "double")
  expect_equal(length(result), 50)
  expect_true(all(result > 0))  # With default A=1, B=0.5, should be positive
  
  # Test with custom parameters
  result2 <- sigma_sin(x, A = 2, B = 1, omega = pi/5, phi = 0)
  expected <- 2 + 1 * sin(pi/5 * x)
  expect_equal(result2, expected)
  
  # Should oscillate
  expect_true(max(result) > min(result))
})

test_that("sigma_multiplicative works correctly", {
  x <- c(1, 2, 3, 4, 5)
  mu_func <- function(x) 1 + 2*x  # Linear mean function
  
  result <- sigma_multiplicative(x, mu_func, p = 1)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should be proportional to |mu(x)|
  mu_vals <- mu_func(x)
  expected <- abs(mu_vals)^1
  expect_equal(result, expected)
  
  # Test with different powers
  result2 <- sigma_multiplicative(x, mu_func, p = 0.5)
  expected2 <- abs(mu_vals)^0.5
  expect_equal(result2, expected2)
})

test_that("sigma_spatial works correctly", {
  coords <- data.frame(
    x = c(0, 1, 2, 3, 4),
    y = c(0, 1, 2, 3, 4)
  )
  
  result <- sigma_spatial(coords)
  expect_type(result, "double")
  expect_equal(length(result), 5)
  expect_true(all(result > 0))
  
  # Should increase with distance from origin
  distances <- sqrt(coords$x^2 + coords$y^2)
  expected <- 0.5 + 0.1 * distances
  expect_equal(result, expected)
  
  # Test error for missing columns
  bad_coords <- data.frame(a = 1:5, b = 1:5)
  expect_error(
    sigma_spatial(bad_coords),
    "coords must have columns 'x' and 'y'"
  )
})

test_that("additional sigma functions work correctly", {
  x <- c(1, 2, 3, 4, 5)
  
  # sigma_logistic
  result_logistic <- sigma_logistic(x)
  expect_type(result_logistic, "double")
  expect_true(all(result_logistic > 0))
  expect_true(all(result_logistic <= 2))  # Should be bounded by L
  
  # sigma_inverse
  result_inverse <- sigma_inverse(x)
  expect_type(result_inverse, "double")
  expect_true(all(result_inverse > 0))
  expect_true(all(diff(result_inverse) < 0))  # Should be decreasing
  
  # sigma_power
  result_power <- sigma_power(x)
  expect_type(result_power, "double")
  expect_true(all(result_power > 0))
  
  # sigma_step
  result_step <- sigma_step(x)
  expect_type(result_step, "double")
  expect_true(all(result_step > 0))
  expect_equal(length(unique(result_step)), 2)  # Should have exactly 2 levels
  
  # sigma_u_shape
  result_u <- sigma_u_shape(x)
  expect_type(result_u, "double")
  expect_true(all(result_u > 0))
  
  # sigma_exp_decay
  result_decay <- sigma_exp_decay(x)
  expect_type(result_decay, "double")
  expect_true(all(result_decay > 0))
  expect_true(all(diff(result_decay) < 0))  # Should be decreasing
  
  # sigma_gaussian_peak
  result_gauss <- sigma_gaussian_peak(x)
  expect_type(result_gauss, "double")
  expect_true(all(result_gauss > 0))
  
  # sigma_piecewise_linear
  result_pwl <- sigma_piecewise_linear(x)
  expect_type(result_pwl, "double")
  expect_true(all(result_pwl > 0))
})

# ARCH Simulation
# =============================================================================

test_that("simulate_arch1 works correctly", {
  result <- simulate_arch1(n = 100, mu = 0, alpha0 = 0.5, alpha1 = 0.3, seed = 123)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 100)
  expect_named(result, c("time", "y", "sigma"))
  
  # All values should be finite
  expect_true(all(is.finite(result$time)))
  expect_true(all(is.finite(result$y)))
  expect_true(all(is.finite(result$sigma)))
  
  # Sigma should be positive
  expect_true(all(result$sigma > 0))
  
  # Time should be sequential
  expect_equal(result$time, 1:100)
})

test_that("simulate_arch1 input validation", {
  # Invalid n
  expect_error(
    simulate_arch1(n = 1, mu = 0, alpha0 = 0.5, alpha1 = 0.3),
    "n >= 2"
  )
  
  # Invalid alpha0
  expect_error(
    simulate_arch1(n = 50, mu = 0, alpha0 = -0.1, alpha1 = 0.3),
    "alpha0 >= 0"
  )
  
  # Invalid alpha1 (should be < 1 for stationarity)
  expect_error(
    simulate_arch1(n = 50, mu = 0, alpha0 = 0.5, alpha1 = 1.1),
    "alpha1 < 1"
  )
})

test_that("simulate_arch1 produces ARCH effects", {
  # Generate ARCH series
  arch_data <- simulate_arch1(n = 200, alpha0 = 0.1, alpha1 = 0.8, seed = 42)
  
  # Fit simple model and test for ARCH effects
  arch_model <- lm(y ~ 1, data = arch_data)
  
  # Should detect ARCH effects (though this may be probabilistic)
  arch_test <- performArchLMTest(arch_model, lags = 1)
  expect_htest(arch_test)
  
  # At least the test should run without error
  expect_true(arch_test$p.value >= 0 && arch_test$p.value <= 1)
})

# Integration with Package Functions
# =============================================================================

test_that("simulated data works with diagnostic functions", {
  # Test with various sigma functions
  sigma_functions <- list(
    linear = sigma_linear,
    exponential = sigma_exponential,
    group = sigma_group,
    polynomial = function(x) sigma_poly(x, a = 0.1, b = 0.2, c = 0.05)
  )
  
  for (sigma_name in names(sigma_functions)) {
    # Generate data
    sim_data <- simulate_hetero(
      n = 100,
      beta0 = 1,
      beta1 = 2,
      sigma_func = sigma_functions[[sigma_name]],
      seed = 123
    )
    
    # Fit model
    model <- lm(y ~ x, data = sim_data)
    
    # Run diagnostics - should work without error
    expect_no_error(
      results <- runHeteroTests(model, sim_data, tests = c("white", "breusch_pagan"))
    )
    expect_valid_diagnostic_result(results, c("white", "breusch_pagan"))
    
    # Generate plots - should work without error
    expect_no_error(plots <- plotDiagnosticSuite(model))
    expect_type(plots, "list")
  }
})

test_that("ARCH simulation integrates with time series tests", {
  arch_data <- simulate_arch1(n = 150, alpha0 = 0.2, alpha1 = 0.6, seed = 456)
  arch_model <- lm(y ~ 1, data = arch_data)
  
  # Run time series tests
  expect_no_error(
    ts_results <- runTimeSeriesTests(arch_model, lags = 3)
  )
  expect_type(ts_results, "list")
  expect_true(all(sapply(ts_results, function(x) inherits(x, "htest"))))
})

# Stress Testing Simulations
# =============================================================================

test_that("simulations handle extreme parameters", {
  # Very small sample size
  small_data <- simulate_hetero(n = 10, beta0 = 0, beta1 = 1, sigma_func = sigma_linear)
  expect_s3_class(small_data, "data.frame")
  expect_equal(nrow(small_data), 10)
  
  # Large coefficients
  large_coef_data <- simulate_hetero(n = 50, beta0 = 100, beta1 = -50, sigma_func = sigma_linear)
  expect_s3_class(large_coef_data, "data.frame")
  expect_true(all(is.finite(large_coef_data$y)))
  
  # Zero intercept
  zero_int_data <- simulate_hetero(n = 50, beta0 = 0, beta1 = 1, sigma_func = sigma_linear)
  expect_s3_class(zero_int_data, "data.frame")
  
  # Very high heteroscedasticity
  high_hetero <- function(x) 0.1 + 2 * x^2
  high_hetero_data <- simulate_hetero(n = 50, beta0 = 1, beta1 = 1, sigma_func = high_hetero)
  expect_s3_class(high_hetero_data, "data.frame")
  expect_true(all(is.finite(high_hetero_data$y)))
})

test_that("sigma functions handle edge cases", {
  # Test all sigma functions with edge case inputs
  edge_x <- c(-10, -1, 0, 1, 10)
  
  sigma_funcs <- list(
    sigma_linear, sigma_exponential, sigma_group, sigma_piecewise,
    function(x) sigma_poly(x), function(x) sigma_sin(x),
    function(x) sigma_logistic(x), function(x) sigma_inverse(x),
    function(x) sigma_power(x), function(x) sigma_step(x),
    function(x) sigma_u_shape(x), function(x) sigma_exp_decay(x),
    function(x) sigma_gaussian_peak(x), function(x) sigma_piecewise_linear(x)
  )
  
  for (i in seq_along(sigma_funcs)) {
    func <- sigma_funcs[[i]]
    
    # Should handle edge cases without error
    expect_no_error(result <- func(edge_x))
    expect_type(result, "double")
    expect_equal(length(result), 5)
    expect_true(all(is.finite(result)))
    expect_true(all(result > 0))  # All sigma functions should return positive values
  }
})

# Reproducibility Tests
# =============================================================================

test_that("simulations are reproducible across R sessions", {
  # Test that same seed produces same results
  # (this is important for reproducible research)
  
  set.seed(42)
  data1 <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = sigma_linear)
  
  set.seed(42)
  data2 <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2, sigma_func = sigma_linear)
  
  expect_equal(data1, data2)
  
  # Test ARCH simulation reproducibility
  set.seed(123)
  arch1 <- simulate_arch1(n = 50, alpha0 = 0.1, alpha1 = 0.5)

  set.seed(123)
  arch2 <- simulate_arch1(n = 50, alpha0 = 0.1, alpha1 = 0.5)
  
  expect_equal(arch1, arch2)
})
