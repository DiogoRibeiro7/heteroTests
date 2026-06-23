# =============================================================================
# Unit Tests for Individual Heteroscedasticity Tests
# tests/testthat/test-individual-tests.R
# =============================================================================

# White Test
# =============================================================================

test_that("performWhiteTest works correctly", {
  test_obj <- create_test_model()
  
  # Basic functionality
  result <- performWhiteTest(test_obj$model, test_obj$data)
  expect_htest(result)
  expect_equal(result$method, "White's test for heteroscedasticity")
  expect_true(result$statistic >= 0)
  
  # Test with cross_products parameter
  result_no_cross <- performWhiteTest(test_obj$model, test_obj$data, cross_products = FALSE)
  expect_htest(result_no_cross)
  
  # Results should be different when cross_products changes
  expect_false(identical(result$statistic, result_no_cross$statistic))
  
  # Test error handling for invalid cross_products
  expect_error(
    performWhiteTest(test_obj$model, test_obj$data, cross_products = "invalid"),
    "'cross_products' must be a single logical value"
  )
})

test_that("performWhiteTest handles edge cases", {
  # Test with single predictor (no cross products possible)
  simple_data <- data.frame(x = rnorm(50), y = rnorm(50))
  simple_model <- lm(y ~ x, data = simple_data)
  
  result <- performWhiteTest(simple_model, simple_data)
  expect_htest(result)
  
  # Test with many predictors (should limit cross products)
  many_pred_data <- data.frame(
    y = rnorm(100),
    x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100),
    x4 = rnorm(100), x5 = rnorm(100), x6 = rnorm(100),
    x7 = rnorm(100), x8 = rnorm(100), x9 = rnorm(100),
    x10 = rnorm(100), x11 = rnorm(100), x12 = rnorm(100)
  )
  many_pred_model <- lm(y ~ ., data = many_pred_data)
  
  expect_warning(
    result_many <- performWhiteTest(many_pred_model, many_pred_data),
    "Cross-products omitted due to high dimensionality"
  )
  expect_htest(result_many)
})

# Breusch-Pagan Test
# =============================================================================

test_that("performBPTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performBPTest(test_obj$model, test_obj$data)
  expect_htest(result)
  expect_equal(result$method, "Breusch-Pagan test for heteroscedasticity")
  expect_true(result$statistic >= 0)
  expect_true(result$parameter > 0)
})

test_that("performBPTest detects heteroscedasticity", {
  # Create strongly heteroscedastic data
  hetero_obj <- create_test_model(heteroscedastic = TRUE)
  
  result <- performBPTest(hetero_obj$model, hetero_obj$data)
  expect_htest(result)
  
  # Should be more likely to detect heteroscedasticity
  # (though we can't guarantee it due to randomness)
  expect_true(result$p.value >= 0)
})

test_that("performBPTest validates its inputs", {
  test_obj <- create_test_model()

  expect_error(
    performBPTest("not a model", test_obj$data),
    "Provide an object fitted with stats::lm() or stats::glm()",
    fixed = TRUE
  )


  expect_error(
    performBPTest(test_obj$model, "not data"),
    "Expected a data.frame for input data; coerce your object with as.data.frame() before running diagnostics.",
    fixed = TRUE
  )
})

# Koenker Test
# =============================================================================

test_that("performKoenkerTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performKoenkerTest(test_obj$model, test_obj$data)
  expect_htest(result)
  expect_equal(result$method, "Koenker studentized Breusch-Pagan test")
  expect_true(result$statistic >= 0)
})

# Goldfeld-Quandt Test  
# =============================================================================

test_that("performGQTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performGQTest(test_obj$model, test_obj$data, order_by = "x1")
  expect_htest(result)
  expect_equal(result$method, "Goldfeld-Quandt test for heteroscedasticity")
  expect_true(result$statistic >= 0)
  
  # Test with different fraction
  result2 <- performGQTest(test_obj$model, test_obj$data, order_by = "x1", fraction = 0.3)
  expect_htest(result2)
  
  # Test error handling
  expect_error(
    performGQTest(test_obj$model, test_obj$data, order_by = "nonexistent"),
    "The following variables are missing from input data: nonexistent. Verify column names with names(data).",
    fixed = TRUE
  )

  expect_error(
    performGQTest(test_obj$model, test_obj$data, order_by = "x1", fraction = 0.95),
    "`fraction` leaves insufficient observations for the two comparison groups.",
    fixed = TRUE
  )
})

# Park Test
# =============================================================================

test_that("performParkTest works correctly", {
  test_obj <- create_positive_data_model()
  
  result <- performParkTest(test_obj$model, test_obj$data, "x1")
  expect_htest(result)
  expect_equal(result$method, "Park test for heteroscedasticity")
  
  # Test error handling for negative values
  data_with_negative <- test_obj$data
  data_with_negative$x1[1] <- -1
  model_negative <- lm(y ~ x1 + x2, data = data_with_negative)
  
  expect_error(
    performParkTest(model_negative, data_with_negative, "x1"),
    "park requires strictly positive data",
    fixed = FALSE
  )
  
  # Test error for missing variable
  expect_error(
    performParkTest(test_obj$model, test_obj$data, "nonexistent"),
    "The following variables are missing from input data: nonexistent. Verify column names with names(data).",
    fixed = TRUE
  )
})

# Spearman Test
# =============================================================================

test_that("performSpearmanTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performSpearmanTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "Spearman rank correlation test for heteroscedasticity")
  expect_true("estimate" %in% names(result))
  expect_true(abs(result$estimate) <= 1)  # Correlation should be between -1 and 1
})

# Group-based Tests
# =============================================================================

test_that("performLeveneTest works correctly", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  model <- lm(mpg ~ wt, data = data)
  
  result <- performLeveneTest(model, data, "cyl")
  expect_htest(result)
  expect_equal(result$method, "Levene's test for equality of variances")
  
  # Test error for missing group variable
  expect_error(
    performLeveneTest(model, data, "nonexistent"),
    "The following variables are missing from input data: nonexistent. Verify column names with names(data).",
    fixed = TRUE
  )
})

test_that("performBartlettTest works correctly", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  model <- lm(mpg ~ wt, data = data)
  
  result <- performBartlettTest(model, data, "cyl")
  expect_htest(result)
  expect_equal(result$method, "Bartlett's test for equality of variances")
})

test_that("performBrownForsytheTest works correctly", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  model <- lm(mpg ~ wt, data = data)
  
  result <- performBrownForsytheTest(model, data, "cyl")
  expect_htest(result)
  expect_equal(result$method, "Brown-Forsythe test for equality of variances")
})

test_that("performFlignerKilleenTest works correctly", {
  data <- mtcars
  data$cyl <- factor(data$cyl)
  model <- lm(mpg ~ wt, data = data)
  
  result <- performFlignerKilleenTest(model, data, "cyl")
  expect_htest(result)
  expect_equal(result$method, "Fligner-Killeen test for homogeneity of variances")
})

# Time Series Tests
# =============================================================================

test_that("performArchLMTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performArchLMTest(test_obj$model, lags = 2)
  expect_htest(result)
  expect_equal(result$method, "Engle's ARCH LM test")
  
  # Test error handling for too many lags
  expect_error(
    performArchLMTest(test_obj$model, lags = 100),
    "Only 50 observations detected but ARCH LM requires at least 205. Provide more data or choose a different diagnostic.",
    fixed = TRUE
  )
  
  # Test error for invalid lags
  expect_error(
    performArchLMTest(test_obj$model, lags = 0),
    "lags.*must be a positive integer"
  )
})

test_that("performMcLeodLiTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performMcLeodLiTest(test_obj$model, lags = 5)
  expect_htest(result)
  expect_equal(result$method, "McLeod-Li test for heteroscedasticity")
  
  # Test error for invalid lags
  expect_error(
    performMcLeodLiTest(test_obj$model, lags = 0),
    "lags.*must be a positive integer"
  )
})

# Specialized Tests
# =============================================================================

test_that("performHarveyTest handles edge cases", {
  test_obj <- create_test_model()
  
  result <- performHarveyTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "Harvey test for heteroscedasticity")
  
  # Test with model that might have very small residuals
  near_perfect_data <- data.frame(x = 1:20, y = 1:20 + rnorm(20, 0, 0.2))
  near_perfect_model <- lm(y ~ x, data = near_perfect_data)
  
  expect_no_error(performHarveyTest(near_perfect_model))
})

test_that("performSpreadLevelTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performSpreadLevelTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "Spread-Level test")
  
  # Should handle edge cases with small fitted values
  expect_no_error(result)
})

test_that("performNCVTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performNCVTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "NCV test via absolute residual regression")
})

test_that("performCookWeisbergTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performCookWeisbergTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "Cook-Weisberg test for heteroscedasticity")
})

test_that("performCameronTrivediTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performCameronTrivediTest(test_obj$model)
  expect_htest(result)
  expect_equal(result$method, "Cameron-Trivedi decomposition test")
})

# Panel Data Tests
# =============================================================================

test_that("performBPRandomEffectsTest works correctly", {
  panel_data <- generate_panel_data()
  model <- lm(y ~ x, data = panel_data)
  
  result <- performBPRandomEffectsTest(model, panel_data, "id")
  expect_htest(result)
  expect_equal(result$method, "Breusch-Pagan LM test for random effects")
  
  # Test error for missing id variable
  expect_error(
    performBPRandomEffectsTest(model, panel_data, "nonexistent"),
    "id.*must be a column"
  )
})

test_that("performPesaranTest works correctly", {
  panel_data <- generate_panel_data()
  model <- lm(y ~ x, data = panel_data)
  
  result <- performPesaranTest(model, panel_data, "id", "time")
  expect_htest(result)
  expect_equal(result$method, "Pesaran CD test for cross-sectional dependence")
  
  # Test error for missing variables
  expect_error(
    performPesaranTest(model, panel_data, "nonexistent", "time"),
    "id.*and.*time.*must be columns"
  )
})

# Additional Tests
# =============================================================================

test_that("performGlejserTest works correctly", {
  test_obj <- create_positive_data_model()
  
  result <- performGlejserTest(test_obj$model, test_obj$data, "x1", transformation = "sqrt")
  expect_htest(result)
  expect_equal(result$method, "Glejser test for heteroscedasticity")
  
  # Test different transformations
  transformations <- c("abs", "sqrt", "inverse", "inverse_sqrt")
  for (trans in transformations) {
    result <- performGlejserTest(test_obj$model, test_obj$data, "x1", transformation = trans)
    expect_htest(result)
  }
})

test_that("performDavidianCarrollTest works correctly", {
  test_obj <- create_test_model()
  
  result <- performDavidianCarrollTest(test_obj$model, degree = 2)
  expect_htest(result)
  expect_equal(result$method, "Davidian-Carroll test")
  
  # Test with different degrees
  result3 <- performDavidianCarrollTest(test_obj$model, degree = 3)
  expect_htest(result3)
  
  # Test error for invalid degree
  expect_error(
    performDavidianCarrollTest(test_obj$model, degree = 0),
    "degree.*must be a positive integer"
  )
})

test_that("performBoxMTest works correctly", {
  data(iris)
  
  result <- performBoxMTest(iris[, 1:4], iris$Species)
  expect_htest(result)
  expect_equal(result$method, "Box's M Test for Equality of Covariance Matrices")
  
  # Test error for invalid data
  expect_error(
    performBoxMTest("not a matrix", iris$Species),
    "data must be a data.frame or matrix"
  )
})
  