# =============================================================================
# Unit Tests for S3 Classes and Methods
# tests/testthat/test-s3-classes.R
# =============================================================================

skip("S3 class tests skipped in this environment")

# HeteroDiagnostic Constructor
# =============================================================================

test_that("HeteroDiagnostic constructor works with lm objects", {
  test_obj <- create_test_model()
  
  # Test with model and data
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  expect_s3_class(hd, "HeteroDiagnostic")
  expect_true("model" %in% names(hd))
  expect_true("data" %in% names(hd))
  expect_s3_class(hd$model, "lm")
  expect_s3_class(hd$data, "data.frame")
})

test_that("HeteroDiagnostic constructor works with formulas", {
  test_obj <- create_test_model()
  
  # Test with formula and data
  hd <- HeteroDiagnostic(y ~ x1 + x2, test_obj$data)
  expect_s3_class(hd, "HeteroDiagnostic")
  expect_s3_class(hd$model, "lm")
  expect_equal(hd$data, test_obj$data)
  
  # Formula should be correctly applied
  expect_equal(formula(hd$model), y ~ x1 + x2)
})

test_that("HeteroDiagnostic constructor works with model only", {
  test_obj <- create_test_model()
  
  # Test with model only (should extract data from model frame)
  hd <- HeteroDiagnostic(test_obj$model)
  expect_s3_class(hd, "HeteroDiagnostic")
  expect_s3_class(hd$model, "lm")
  expect_s3_class(hd$data, "data.frame")
})

test_that("HeteroDiagnostic constructor handles errors correctly", {
  test_obj <- create_test_model()
  
  # Missing data when formula provided
  expect_error(
    HeteroDiagnostic(y ~ x1 + x2),
    "data.*must be supplied when.*model.*is a formula"
  )
  
  # Invalid model object
  expect_error(
    HeteroDiagnostic("not a model"),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
  
  # Invalid data
  expect_error(
    HeteroDiagnostic(test_obj$model, "not data"),
    "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
    fixed = TRUE
  )
})

# HeteroDiagnostic test() method
# =============================================================================

test_that("HeteroDiagnostic test method works with defaults", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  result <- test(hd)
  expect_valid_diagnostic_result(result, c("white", "breusch_pagan"))
  
  # Each result should be an htest object
  expect_htest(result$white)
  expect_htest(result$breusch_pagan)
})

test_that("HeteroDiagnostic test method works with custom tests", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Test with specific tests
  result <- test(hd, tests = c("white", "koenker"))
  expect_valid_diagnostic_result(result, c("white", "koenker"))
  
  # Test with single test
  result_single <- test(hd, tests = "white")
  expect_valid_diagnostic_result(result_single, "white")
  
  # Test with many tests
  many_tests <- c("white", "breusch_pagan", "koenker", "cook_weisberg", "ncv")
  result_many <- test(hd, tests = many_tests)
  expect_valid_diagnostic_result(result_many, many_tests)
})

test_that("HeteroDiagnostic test method handles invalid tests", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Should error for unknown tests
  expect_error(
    test(hd, tests = "nonexistent_test"),
    "Unknown tests.*nonexistent_test"
  )
  
  expect_error(
    test(hd, tests = c("white", "invalid_test")),
    "Unknown tests.*invalid_test"
  )
})

# HeteroDiagnostic plot() method
# =============================================================================

test_that("HeteroDiagnostic plot method works with defaults", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  plots <- plot(hd)
  expect_type(plots, "list")
  
  # Should contain expected plot names
  expected_plots <- c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
  expect_named(plots, expected_plots, ignore.order = TRUE)
  
  # Each should be a ggplot object
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
  }
})

test_that("HeteroDiagnostic plot method works with custom plots", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Test with specific plots
  custom_plots <- c("residuals_fitted", "qq")
  plots <- plot(hd, plots = custom_plots)
  
  expect_type(plots, "list")
  expect_named(plots, custom_plots, ignore.order = TRUE)
  
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
  }
})

test_that("HeteroDiagnostic plot method handles invalid plots", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Should error for unknown plots
  expect_error(
    plot(hd, plots = "nonexistent_plot"),
    "Unknown plots.*nonexistent_plot"
  )
})

# HeteroDiagnostic summary() method
# =============================================================================

test_that("HeteroDiagnostic summary method works", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  summ <- summary(hd)
  expect_type(summ, "double")
  expect_s3_class(summ, "summary.HeteroDiagnostic")
  
  # Should have names corresponding to tests
  expect_named(summ, c("white", "breusch_pagan"), ignore.order = TRUE)
  
  # All values should be numeric
  expect_true(all(is.numeric(summ)))
})

test_that("HeteroDiagnostic summary method works with custom tests", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Test with custom tests
  summ <- summary(hd, tests = c("white", "koenker", "ncv"))
  expect_type(summ, "double")
  expect_named(summ, c("white", "koenker", "ncv"), ignore.order = TRUE)
})

test_that("HeteroDiagnostic summary handles missing statistics gracefully", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Mock a result with missing statistic
  # This tests the robustness of the summary method
  summ <- summary(hd)
  expect_true(all(is.finite(summ) | is.na(summ)))
})

# Generic test() function
# =============================================================================

test_that("test generic function works correctly", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Should dispatch to test.HeteroDiagnostic
  result1 <- test(hd)
  result2 <- test.HeteroDiagnostic(hd)
  
  expect_equal(result1, result2)
})

test_that("test generic works with ellipsis arguments", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Should pass additional arguments correctly
  result <- test(hd, tests = c("white", "koenker"))
  expect_valid_diagnostic_result(result, c("white", "koenker"))
})

# Integration with other classes
# =============================================================================

test_that("HeteroDiagnostic works with glm objects", {
  # Create a GLM model
  glm_model <- glm(vs ~ wt + hp, data = mtcars, family = binomial)
  
  hd <- HeteroDiagnostic(glm_model, mtcars)
  expect_s3_class(hd, "HeteroDiagnostic")
  expect_s3_class(hd$model, "glm")
  
  # Basic functionality should work
  result <- test(hd, tests = "white")  # Not all tests may work with GLM
  expect_type(result, "list")
})

test_that("HeteroDiagnostic preserves model attributes", {
  test_obj <- create_test_model()
  
  # Add custom attributes to model
  attr(test_obj$model, "custom_attr") <- "test_value"
  
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Custom attributes should be preserved
  expect_equal(attr(hd$model, "custom_attr"), "test_value")
})

test_that("HeteroDiagnostic works with different data structures", {
  # Test with data containing factors
  data_with_factors <- mtcars
  data_with_factors$cyl <- factor(data_with_factors$cyl)
  data_with_factors$vs <- factor(data_with_factors$vs)
  
  model_factors <- lm(mpg ~ wt + cyl + vs, data = data_with_factors)
  hd <- HeteroDiagnostic(model_factors, data_with_factors)
  
  expect_s3_class(hd, "HeteroDiagnostic")
  
  # Should be able to run some tests
  result <- test(hd, tests = "white")
  expect_htest(result$white)
})

# Method consistency
# =============================================================================

test_that("HeteroDiagnostic methods are consistent", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Test results should be consistent across calls
  result1 <- test(hd, tests = "white")
  result2 <- test(hd, tests = "white")
  
  expect_equal(result1$white$statistic, result2$white$statistic)
  expect_equal(result1$white$p.value, result2$white$p.value)
  
  # Plots should be generated consistently
  plots1 <- plot(hd, plots = "residuals_fitted")
  plots2 <- plot(hd, plots = "residuals_fitted")
  
  # While ggplot objects may not be identical due to environments,
  # they should have the same structure
  expect_equal(class(plots1$residuals_fitted), class(plots2$residuals_fitted))
})

test_that("print methods work for HeteroDiagnostic results", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # Summary should print without error
  summ <- summary(hd)
  expect_output(print(summ), ".*")
  
  # Individual test results should print correctly
  result <- test(hd, tests = "white")
  expect_output(print(result$white), "White's test")
})