# =============================================================================
# Integration Tests
# tests/testthat/test-integration.R
# =============================================================================

# Full Workflow Integration Tests
# =============================================================================

test_that("complete heteroscedasticity analysis workflow", {
  # Step 1: Create heteroscedastic data
  set.seed(42)  # For reproducibility
  test_obj <- create_test_model(heteroscedastic = TRUE, n = 100)

  # Step 2: Create diagnostic object
  hd <- HeteroDiagnostic(test_obj$model, test_obj$data)
  expect_s3_class(hd, "HeteroDiagnostic")

  # Step 3: Run comprehensive tests
  test_results <- test(hd, tests = c("white", "breusch_pagan", "koenker", "cook_weisberg"))
  expect_valid_diagnostic_result(test_results, c("white", "breusch_pagan", "koenker", "cook_weisberg", "vif", "reset", "influence"))

  # Step 4: Generate diagnostic plots
  plots <- plot(hd)
  expect_type(plots, "list")
  expect_true(all(sapply(plots, inherits, "ggplot")))

  # Step 5: Get summary statistics
  summary_stats <- summary(hd, tests = c("white", "breusch_pagan", "koenker", "cook_weisberg"))
  expect_type(summary_stats, "double")
  expect_s3_class(summary_stats, "summary.HeteroDiagnostic")

  # Step 6: Get remediation suggestions
  suggestions <- suggestRemediation(test_results)
  expect_remediation_suggestions(suggestions)

  # Step 7: Apply remediation methods
  wls_model <- fitWLS(test_obj$model)
  expect_s3_class(wls_model, "lm")

  robust_model <- fitRobust(test_obj$model)
  expect_s3_class(robust_model, "rlm")

  transform_result <- autoTransform(test_obj$model)
  expect_type(transform_result, "list")
  expect_s3_class(transform_result$model, "lm")
})
