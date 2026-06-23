# =============================================================================
# Unit Tests for Core Validation Functions
# tests/testthat/test-core-functions.R
# =============================================================================

test_that("checkModel validates model inputs correctly", {
  # Valid models
  valid_lm <- lm(mpg ~ wt, data = mtcars)
  valid_glm <- glm(vs ~ wt, data = mtcars, family = binomial)
  
  expect_invisible(checkModel(valid_lm))
  expect_invisible(checkModel(valid_glm))
  
  # Invalid inputs
  expect_error(
    checkModel("not a model"),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
  expect_error(
    checkModel(NULL),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
  expect_error(
    checkModel(list(coefficients = 1:3)),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
  expect_error(
    checkModel(data.frame(x = 1:5)),
    "Provide a model fitted with stats::lm() or stats::glm().",
    fixed = TRUE
  )
})

test_that("checkData validates data inputs correctly", {
  # Valid data
  valid_data <- data.frame(x = 1:10, y = 1:10)
  valid_data2 <- mtcars
  
  expect_invisible(checkData(valid_data))
  expect_invisible(checkData(valid_data2))
  
  # Invalid inputs
  expect_error(
    checkData("not a data frame"),
    "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
    fixed = TRUE
  )
  expect_error(
    checkData(NULL),
    "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
    fixed = TRUE
  )
  expect_error(
    checkData(matrix(1:10, ncol = 2)),
    "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
    fixed = TRUE
  )
  expect_error(
    checkData(list(x = 1:5, y = 1:5)),
    "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
    fixed = TRUE
  )
})

test_that("validateTestInputs performs comprehensive validation", {
  test_obj <- create_test_model()
  model <- test_obj$model
  data <- test_obj$data
  
  # Valid inputs should pass silently
  expect_invisible(validateTestInputs(model, data, "test", min_obs = 10))
  expect_invisible(validateTestInputs(model, data, "test", min_obs = 5))
  
  # Invalid model class
  err_invalid <- expect_error(
    validateTestInputs("not a model", data, "test"),
    "Input validation for test failed",
    fixed = FALSE
  )
  expect_match(
    conditionMessage(err_invalid),
    "Provide an object fitted with stats::lm() or stats::glm() before running test",
    fixed = TRUE
  )
  
  # Invalid data
  err_invalid_data <- expect_error(
    validateTestInputs(model, "not data", "test"),
    "Input validation for test failed",
    fixed = FALSE
  )
  expect_match(
    conditionMessage(err_invalid_data),
    "Expected a data.frame for data supplied to test; coerce your object with as.data.frame() before running diagnostics.",
    fixed = TRUE
  )
  
  # Insufficient observations
  small_data <- data[1:5, ]
  small_model <- lm(y ~ x1 + x2, data = small_data)
  err_small <- expect_error(
    validateTestInputs(small_model, small_data, "test", min_obs = 10),
    "Input validation for test failed",
    fixed = FALSE
  )
  expect_match(
    conditionMessage(err_small),
    "Data Supplied to Test contains only 5 observations but at least 10 are required",
    fixed = FALSE
  )
  expect_match(
    conditionMessage(err_small),
    "Only 5 observations detected but test requires at least 10",
    fixed = FALSE
  )
  
  # Model with NA residuals
  model_na <- model
  model_na$residuals[1] <- NA_real_
  err_resid <- expect_error(
    validateTestInputs(model_na, data, "test"),
    "Input validation for test failed",
    fixed = FALSE
  )
  expect_match(
    conditionMessage(err_resid),
    "Model residuals must be finite before running test.",
    fixed = TRUE
  )
})

test_that("validateTestInputs warns about potential issues", {
  # Test warnings for outliers
  data_outliers <- generate_test_data(n = 50)
  data_outliers$y[1] <- 100  # Create extreme outlier
  model_outliers <- lm(y ~ x1 + x2, data = data_outliers)
  
  expect_warning(
    validateTestInputs(model_outliers, data_outliers, "test"),
    "Residual outliers detected",
    fixed = FALSE
  )
  
  # Test warnings for large datasets
  large_data <- generate_test_data(n = 15000)
  large_model <- lm(y ~ x1 + x2, data = large_data)
  
  expect_warning(
    validateTestInputs(large_model, large_data, "test"),
    "Large dataset detected",
    fixed = FALSE
  )
})

test_that("checkModelEnhanced provides additional diagnostics", {
  test_obj <- create_test_model()
  
  # Should pass without warning for normal model
  expect_invisible(checkModelEnhanced(test_obj$model))
  
  # Test warning for few degrees of freedom
  small_data <- generate_test_data(n = 8)  # Will have few df
  small_model <- lm(y ~ x1 + x2, data = small_data)
  
  expect_warning(
    checkModelEnhanced(small_model),
    "Very few residual degrees of freedom"
  )
  
  # Test warning for near-perfect fit
  perfect_data <- data.frame(x = 1:10, y = 1:10 + rnorm(10, 0, 0.001))
  perfect_model <- lm(y ~ x, data = perfect_data)
  
  expect_warning(
    checkModelEnhanced(perfect_model),
    "Near-perfect fit detected"
  )
})

test_that("std_error generates consistent error messages", {
    expect_error(
      std_error("invalid_model"),
      "Provide a model fitted with stats::lm() or stats::glm().",
      fixed = TRUE
    )
    expect_error(
      std_error("invalid_data"),
      "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
      fixed = TRUE
    )
    expect_error(
      std_error("missing_variable", variable = "test_var"),
      "Variable 'test_var' not found in the supplied data. Check names(data).",
      fixed = TRUE
    )
  expect_error(
    std_error("negative_values", variable = "x1"),
    "Test requires positive values in variable 'x1'"
  )
  expect_error(
    std_error("invalid_logical", arg = "cross_products"),
    "'cross_products' must be a single logical value"
  )
})

test_that("std_error handles unknown error types", {
  expect_error(
    std_error("unknown_error_type"),
    "Unknown error type: unknown_error_type"
  )
})

test_that("error message templating works correctly", {
  # Test template substitution
  expect_error(
    std_error("missing_variable", variable = "my_var"),
    "Variable 'my_var' not found in the supplied data. Check names(data).",
    fixed = TRUE
  )
  
  # Test multiple substitutions if implemented
  # This would require extending the message system
})

test_that("validation works with different model types", {
  # Test with glm
  glm_model <- glm(vs ~ wt + hp, data = mtcars, family = binomial)
  expect_invisible(validateTestInputs(glm_model, mtcars, "test"))
  
  # Test with model containing factors
  mtcars_factor <- mtcars
  mtcars_factor$cyl <- factor(mtcars_factor$cyl)
  factor_model <- lm(mpg ~ wt + cyl, data = mtcars_factor)
  expect_invisible(validateTestInputs(factor_model, mtcars_factor, "test"))
})

test_that("validation handles edge cases gracefully", {
  # Model with zero residual variance (perfect fit)
  perfect_data <- data.frame(x = 1:10, y = 1:10)
  perfect_model <- lm(y ~ x, data = perfect_data)
  
  expect_error(
    validateTestInputs(perfect_model, perfect_data, "test"),
    "Residual variance is too small to evaluate test",
    fixed = FALSE
  )
  
  # Model with single predictor
  simple_model <- lm(mpg ~ wt, data = mtcars)
  expect_invisible(validateTestInputs(simple_model, mtcars, "test"))
  
  # Model with many predictors
  complex_model <- lm(mpg ~ ., data = mtcars)
  expect_invisible(validateTestInputs(complex_model, mtcars, "test"))
})

test_that("validateTestInputs enforces test name and min_obs arguments", {
  model <- lm(mpg ~ wt, data = mtcars)
  expect_error(
    validateTestInputs(model, mtcars, NA_character_),
    "Argument `test_name` must be a single non-empty string",
    fixed = FALSE
  )
  expect_error(
    validateTestInputs(model, mtcars, "demo", min_obs = -1),
    "`min_obs` must be a non-negative integer.",
    fixed = TRUE
  )
  expect_error(
    validateTestInputs(model, mtcars, "demo", min_obs = NA_integer_),
    "`min_obs` must be a non-negative integer.",
    fixed = TRUE
  )
})

test_that("checkModelEnhanced warns when multicollinearity is extreme", {
  set.seed(99)
  n <- 30
  df <- data.frame(
    y = rnorm(n),
    x1 = rnorm(n)
  )
  df$x2 <- df$x1 * 0.99 + rnorm(n, sd = 1e-3)
  model <- lm(y ~ x1 + x2, data = df)
  expect_warning(
    checkModelEnhanced(model, df),
    "High multicollinearity detected (VIF > 10)",
    fixed = TRUE
  )
})
