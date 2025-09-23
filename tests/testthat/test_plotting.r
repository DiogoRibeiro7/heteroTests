# =============================================================================
# Unit Tests for Plotting Functions
# tests/testthat/test-plotting.R
# =============================================================================

# Individual Plot Functions
# =============================================================================

test_that("plotResidualsFitted works correctly", {
  test_obj <- create_test_model()
  
  p <- plotResidualsFitted(test_obj$model)
  expect_ggplot(p)
  
  # Should have appropriate labels
  expect_equal(p$labels$x, "Fitted values")
  expect_equal(p$labels$y, "Residuals")
  expect_equal(p$labels$title, "Residuals vs Fitted")
  
  # Should have data
  expect_true(nrow(p$data) > 0)
  expect_true("fitted" %in% names(p$data))
  expect_true("resid" %in% names(p$data))
})

test_that("plotSpreadLevel works correctly", {
  test_obj <- create_test_model()
  
  p <- plotSpreadLevel(test_obj$model)
  expect_ggplot(p)
  
  # Should have appropriate labels
  expect_equal(p$labels$x, "Fitted values")
  expect_equal(p$labels$y, "sqrt(|Residual|)")
  expect_equal(p$labels$title, "Spread-Level Plot")
  
  # Should have data with positive sqrt values
  expect_true(nrow(p$data) > 0)
  expect_true("fitted" %in% names(p$data))
  expect_true("res_sqrt" %in% names(p$data))
  expect_true(all(p$data$res_sqrt >= 0))
})

test_that("plotResidualDensity works correctly", {
  test_obj <- create_test_model()
  
  p <- plotResidualDensity(test_obj$model)
  expect_ggplot(p)
  
  # Should have appropriate labels
  expect_equal(p$labels$x, "Residuals")
  expect_equal(p$labels$y, "Density")
  expect_equal(p$labels$title, "Residual Density")
  
  # Should have residual data
  expect_true(nrow(p$data) > 0)
  expect_true("resid" %in% names(p$data))
})

test_that("plotResidualQQ works correctly", {
  test_obj <- create_test_model()
  
  p <- plotResidualQQ(test_obj$model)
  expect_ggplot(p)
  
  # Should have appropriate labels
  expect_equal(p$labels$x, "Theoretical Quantiles")
  expect_equal(p$labels$y, "Sample Quantiles")
  expect_equal(p$labels$title, "Residual QQ Plot")
  
  # Should have residual data
  expect_true(nrow(p$data) > 0)
  expect_true("resid" %in% names(p$data))
})

test_that("plotBubbleVariance works correctly", {
  test_obj <- create_test_model()
  
  # Test with default variable (should pick first predictor)
  p1 <- plotBubbleVariance(test_obj$model)
  expect_ggplot(p1)
  expect_equal(p1$labels$title, "Bubble Plot of Residual Variance")
  expect_equal(p1$labels$size, "|residual|")
  
  # Test with specified variable
  p2 <- plotBubbleVariance(test_obj$model, variable = "x1")
  expect_ggplot(p2)
  expect_equal(p2$labels$x, "x1")
  
  # Should have appropriate data
  expect_true(nrow(p2$data) > 0)
  expect_true("resid" %in% names(p2$data))
  expect_true("abs_resid" %in% names(p2$data))
  expect_true(all(p2$data$abs_resid >= 0))
})

test_that("plotBubbleVariance handles errors correctly", {
  test_obj <- create_test_model()
  
  # Should error for non-existent variable
  expect_error(
    plotBubbleVariance(test_obj$model, variable = "nonexistent"),
    "variable not found in model frame"
  )
})

# Enhanced Plot Functions
# =============================================================================

test_that("plotResidualsFittedEnhanced works correctly", {
  test_obj <- create_test_model()
  
  p <- plotResidualsFittedEnhanced(test_obj$model)
  expect_ggplot(p)
  
  # Should have enhanced features
  expect_equal(p$labels$title, "Enhanced Residuals vs Fitted")
  expect_true(grepl("LOWESS", p$labels$subtitle))
    expect_true(!is.null(p$labels$colour))
  expect_true(!is.null(p$labels$size))
  
  # Should have data with influential points marked
  expect_true("influential" %in% names(p$data))
  expect_true("abs_resid" %in% names(p$data))
  expect_type(p$data$influential, "logical")
})

test_that("plotDiagnosticSuiteEnhanced works correctly", {
  test_obj <- create_test_model()
  
  plots <- plotDiagnosticSuiteEnhanced(test_obj$model)
  expect_type(plots, "list")
  
  # Should contain expected plots
  expected_names <- c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
  expect_named(plots, expected_names, ignore.order = TRUE)
  
  # Each should be a ggplot
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
  }
  
  # Enhanced version should be used for residuals_fitted
  expect_equal(plots$residuals_fitted$labels$title, "Enhanced Residuals vs Fitted")
})

# Plot Suite Functions
# =============================================================================

test_that("plotDiagnosticSuite works correctly", {
  test_obj <- create_test_model()
  
  plots <- plotDiagnosticSuite(test_obj$model)
  expect_type(plots, "list")
  
  # Should contain expected plots
  expected_names <- c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
  expect_named(plots, expected_names, ignore.order = TRUE)
  
  # Each should be a ggplot object
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
    
    # Should have appropriate data
    expect_true(nrow(plots[[plot_name]]$data) > 0)
  }
})

test_that("plotBeforeAfter works correctly", {
  test_obj <- create_test_model()
  wls_model <- fitWLS(test_obj$model)
  
  p <- plotBeforeAfter(test_obj$model, wls_model)
  expect_ggplot(p)
  
  # Should have appropriate labels
  expect_equal(p$labels$title, "Before/After Residual Comparison")
  expect_equal(p$labels$colour, "Model")
  
  # Should have data from both models
  expect_true(nrow(p$data) > 0)
  expect_true("model" %in% names(p$data))
  expect_true(all(c("original", "remedied") %in% p$data$model))
  
  # Should have residuals from both models
  original_rows <- sum(p$data$model == "original")
  remedied_rows <- sum(p$data$model == "remedied")
  expect_equal(original_rows, length(residuals(test_obj$model)))
  expect_equal(remedied_rows, length(residuals(wls_model)))
})

# Plot Input Validation
# =============================================================================

test_that("plotting functions validate model inputs", {
  # All plotting functions should error for invalid models
  plot_functions <- list(
    plotResidualsFitted,
    plotSpreadLevel,
    plotResidualDensity,
    plotResidualQQ,
    plotBubbleVariance
  )
  
  for (func in plot_functions) {
    expect_error(
      func("not a model"),
      "Provide a model fitted with stats::lm() or stats::glm().",
      fixed = TRUE
    )

    expect_error(
      func(NULL),
      "Provide a model fitted with stats::lm() or stats::glm().",
      fixed = TRUE
    )
  }
})

test_that("plotBeforeAfter validates both model inputs", {
  test_obj <- create_test_model()
  
  # Should error for invalid first model
  expect_error(
    plotBeforeAfter("not a model", test_obj$model),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
  
  # Should error for invalid second model  
  expect_error(
    plotBeforeAfter(test_obj$model, "not a model"),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
})

# Plot Edge Cases
# =============================================================================

test_that("plotting functions handle edge cases", {
  # Test with very small dataset
  small_data <- data.frame(x = 1:5, y = 1:5 + rnorm(5, 0, 0.1))
  small_model <- lm(y ~ x, data = small_data)
  
  expect_no_error(p1 <- plotResidualsFitted(small_model))
  expect_ggplot(p1)
  
  expect_no_error(p2 <- plotSpreadLevel(small_model))
  expect_ggplot(p2)
  
  # Test with perfect fit (zero residuals)
  perfect_data <- data.frame(x = 1:10, y = 1:10)
  perfect_model <- lm(y ~ x, data = perfect_data)
  
  # Should handle zero residuals gracefully
  expect_no_error(p3 <- plotResidualsFitted(perfect_model))
  expect_ggplot(p3)
  
  # Spread level plot should handle zero residuals
  expect_no_error(p4 <- plotSpreadLevel(perfect_model))
  expect_ggplot(p4)
})

test_that("plotting functions handle missing values", {
  # Create data with some missing values in predictors
  data_na <- mtcars
  data_na$wt[1:3] <- NA
  
  # Fit model with na.action
  model_na <- lm(mpg ~ wt + hp, data = data_na, na.action = na.omit)
  
  # Plots should work with the complete cases
  expect_no_error(p1 <- plotResidualsFitted(model_na))
  expect_ggplot(p1)
  
  expect_no_error(p2 <- plotDiagnosticSuite(model_na))
  expect_type(p2, "list")
})

test_that("plotting functions work with different model types", {
  # Test with GLM
  glm_model <- glm(vs ~ wt + hp, data = mtcars, family = binomial)
  
  expect_no_error(p1 <- plotResidualsFitted(glm_model))
  expect_ggplot(p1)
  
  expect_no_error(p2 <- plotResidualDensity(glm_model))
  expect_ggplot(p2)
  
  # Test with model containing factors
  mtcars_factor <- mtcars
  mtcars_factor$cyl <- factor(mtcars_factor$cyl)
  factor_model <- lm(mpg ~ wt + cyl, data = mtcars_factor)
  
  expect_no_error(p3 <- plotDiagnosticSuite(factor_model))
  expect_type(p3, "list")
})

# Plot Customization and Themes
# =============================================================================

test_that("plots use consistent ggplot2 themes", {
  test_obj <- create_test_model()
  
  plots <- plotDiagnosticSuite(test_obj$model)
  
  # All plots should be ggplot objects with theme information
  for (plot_name in names(plots)) {
    p <- plots[[plot_name]]
    expect_ggplot(p)
    
    # Should have layers (points, lines, etc.)
    expect_true(length(p$layers) > 0)
  }
})

test_that("enhanced plots have additional visual elements", {
  test_obj <- create_test_model()
  
  # Compare regular vs enhanced residuals fitted plot
  p_regular <- plotResidualsFitted(test_obj$model)
  p_enhanced <- plotResidualsFittedEnhanced(test_obj$model)
  
  # Enhanced should have more aesthetic mappings
  enhanced_aes <- names(p_enhanced$mapping)
  regular_aes <- names(p_regular$mapping)
  
  # Enhanced plot should have additional aesthetics
  expect_true(length(enhanced_aes) >= length(regular_aes))
})

# Integration with Other Components
# =============================================================================

test_that("plots integrate with HeteroDiagnostic class", {
  test_obj <- create_test_model()
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  
  # plot() method should work
  plots <- plot(hd)
  expect_type(plots, "list")
  
  # Should produce same plots as direct function calls
  direct_plots <- plotDiagnosticSuite(test_obj$model)
  
  # Should have same structure
  expect_equal(names(plots), names(direct_plots))
  
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
    expect_ggplot(direct_plots[[plot_name]])
  }
})

test_that("plots work with registry system", {
  test_obj <- create_test_model()
  
  # Should be able to run registered plots
  result <- runDiagnosticPlots(test_obj$model)
  expect_type(result, "list")
  
  # Should contain default plots
  expected_plots <- c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
  expect_named(result, expected_plots, ignore.order = TRUE)
  
  for (plot_name in names(result)) {
    expect_ggplot(result[[plot_name]])
  }
})

# Performance and Memory
# =============================================================================

test_that("plotting functions are reasonably efficient", {
  skip_on_cran()  # Performance test
  
  # Test with larger dataset
  large_data <- generate_test_data(n = 1000)
  large_model <- lm(y ~ x1 + x2, data = large_data)
  
  # Should complete within reasonable time
  start_time <- Sys.time()
  plots <- plotDiagnosticSuite(large_model)
  end_time <- Sys.time()
  
  execution_time <- as.numeric(end_time - start_time, units = "secs")
  expect_true(execution_time < 5)  # Should be fast
  
  # Should produce valid plots
  expect_type(plots, "list")
  for (plot_name in names(plots)) {
    expect_ggplot(plots[[plot_name]])
  }
})