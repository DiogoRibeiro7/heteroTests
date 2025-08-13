# =============================================================================
# Unit Tests for Registry Systems
# tests/testthat/test-registry-systems.R
# =============================================================================

# Environment-based Registry (.diagnostic_registry)
# =============================================================================

test_that("diagnostic registry registration works", {
  # Create a simple test function
  custom_test <- function(model, data) {
    list(
      statistic = c("test-stat" = 1.23),
      p.value = 0.05,
      method = "Custom Test",
      data.name = deparse(substitute(model))
    )
  }
  class(custom_test) <- "htest"
  
  # Register the test
  expect_invisible(registerDiagnostic("custom_test", custom_test))
  
  # Test should now be in registry
  registry <- as.list(.diagnostic_registry)
  expect_true("custom_test" %in% names(registry))
  expect_identical(registry$custom_test, custom_test)
})

test_that("diagnostic registry validation works", {
  # Should error for invalid name
  expect_error(
    registerDiagnostic(123, function(model, data) {}),
    "name.*must be a single string"
  )
  
  expect_error(
    registerDiagnostic(c("test1", "test2"), function(model, data) {}),
    "name.*must be a single string"
  )
  
  # Should error for invalid function
  expect_error(
    registerDiagnostic("test", "not a function"),
    "fun.*must be a function"
  )
})

test_that("registered diagnostics can be executed", {
  test_obj <- create_test_model()
  
  # Register a working test
  working_test <- function(model, data) {
    e <- residuals(model)
    stat <- sum(e^2)
    structure(
      list(
        statistic = c("custom-stat" = stat),
        p.value = 0.5,
        method = "Working Custom Test",
        data.name = deparse(substitute(model))
      ),
      class = "htest"
    )
  }
  
  registerDiagnostic("working_test", working_test)
  
  # Should be able to run via runHeteroTests
  result <- runHeteroTests(test_obj$model, test_obj$data, tests = "working_test")
  expect_named(result, "working_test")
  expect_htest(result$working_test)
  expect_equal(result$working_test$method, "Working Custom Test")
})

test_that("built-in diagnostics are pre-registered", {
  registry <- as.list(.diagnostic_registry)
  
  # Should contain standard tests
  expected_tests <- c("white", "breusch_pagan", "koenker", "cook_weisberg", "ncv", "spread_level")
  
  for (test_name in expected_tests) {
    expect_true(test_name %in% names(registry), 
                info = paste("Test", test_name, "should be pre-registered"))
    expect_true(is.function(registry[[test_name]]))
  }
})

# Plot Registry (.plot_registry)
# =============================================================================

test_that("plot registry registration works", {
  # Create a simple test plot function
  custom_plot <- function(model) {
    ggplot2::ggplot() + 
      ggplot2::geom_point(ggplot2::aes(x = 1, y = 1)) +
      ggplot2::labs(title = "Custom Plot")
  }
  
  # Register the plot
  expect_invisible(registerPlot("custom_plot", custom_plot))
  
  # Plot should now be in registry
  registry <- as.list(.plot_registry)
  expect_true("custom_plot" %in% names(registry))
  expect_identical(registry$custom_plot, custom_plot)
})

test_that("plot registry validation works", {
  # Should error for invalid name
  expect_error(
    registerPlot(123, function(model) {}),
    "name.*must be a single string"
  )
  
  # Should error for invalid function
  expect_error(
    registerPlot("test_plot", "not a function"),
    "fun.*must be a function"
  )
})

test_that("registered plots can be executed", {
  test_obj <- create_test_model()
  
  # Register a working plot
  working_plot <- function(model) {
    data <- data.frame(
      fitted = fitted(model),
      residuals = residuals(model)
    )
    ggplot2::ggplot(data, ggplot2::aes(fitted, residuals)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = "Working Custom Plot")
  }
  
  registerPlot("working_plot", working_plot)
  
  # Should be able to run via runDiagnosticPlots
  result <- runDiagnosticPlots(test_obj$model, plots = "working_plot")
  expect_named(result, "working_plot")
  expect_ggplot(result$working_plot)
})

test_that("built-in plots are pre-registered", {
  registry <- as.list(.plot_registry)
  
  # Should contain standard plots
  expected_plots <- c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
  
  for (plot_name in expected_plots) {
    expect_true(plot_name %in% names(registry),
                info = paste("Plot", plot_name, "should be pre-registered"))
    expect_true(is.function(registry[[plot_name]]))
  }
})

test_that("runDiagnosticPlots handles invalid plots", {
  test_obj <- create_test_model()
  
  expect_error(
    runDiagnosticPlots(test_obj$model, plots = "nonexistent_plot"),
    "Unknown plots.*nonexistent_plot"
  )
  
  expect_error(
    runDiagnosticPlots(test_obj$model, plots = c("residuals_fitted", "invalid")),
    "Unknown plots.*invalid"
  )
})

# Test Factory (R6-based, if available)
# =============================================================================

test_that("test factory works if R6 is available", {
  skip_if_not_installed("R6")
  
  # Test basic functionality
  available_tests <- test_factory$get_available()
  expect_type(available_tests, "character")
  expect_true(length(available_tests) > 0)
  
  # Should include some standard tests
  expect_true("white" %in% available_tests)
  expect_true("breusch_pagan" %in% available_tests)
})

test_that("test factory registration works", {
  skip_if_not_installed("R6")
  
  # Create a test function
  factory_test <- function(model, data) {
    structure(
      list(
        statistic = c("factory-stat" = 2.5),
        p.value = 0.1,
        method = "Factory Test"
      ),
      class = "htest"
    )
  }
  
  # Register with metadata
  expect_invisible(
    test_factory$register(
      "factory_test", 
      factory_test,
      metadata = list(
        description = "Test from factory",
        min_observations = 20
      )
    )
  )
  
  # Should appear in available tests
  available <- test_factory$get_available()
  expect_true("factory_test" %in% available)
})

test_that("test factory can execute tests", {
  skip_if_not_installed("R6")
  
  test_obj <- create_test_model()
  
  # Run a standard test
  result <- test_factory$run_test("white", test_obj$model, test_obj$data)
  expect_htest(result)
  
  # Should include metadata
  expect_true("test_metadata" %in% names(result))
})

test_that("test factory filtering works", {
  skip_if_not_installed("R6")
  
  # Filter by minimum observations
  available_large <- test_factory$get_available(min_n = 50)
  available_small <- test_factory$get_available(min_n = 5)
  
  expect_type(available_large, "character")
  expect_type(available_small, "character")
  
  # Should have fewer tests with higher minimum
  expect_true(length(available_large) <= length(available_small) + 1)
})

test_that("test factory handles errors gracefully", {
  skip_if_not_installed("R6")
  
  test_obj <- create_test_model()
  
  # Should error for unknown test
  expect_error(
    test_factory$run_test("unknown_test", test_obj$model, test_obj$data),
    "Unknown test.*unknown_test"
  )
})

# Registry Consistency
# =============================================================================

test_that("both registry systems contain same core tests", {
  # Get tests from environment registry
  env_tests <- names(as.list(.diagnostic_registry))
  
  if (requireNamespace("R6", quietly = TRUE)) {
    # Get tests from factory registry
    factory_tests <- test_factory$get_available()
    
    # Core tests should be in both
    core_tests <- c("white", "breusch_pagan", "koenker")
    
    for (test in core_tests) {
      expect_true(test %in% env_tests, 
                  info = paste("Core test", test, "missing from env registry"))
      expect_true(test %in% factory_tests,
                  info = paste("Core test", test, "missing from factory registry"))
    }
  }
})

test_that("registry systems produce consistent results", {
  skip_if_not_installed("R6")
  
  test_obj <- create_test_model()
  
  # Run same test through both systems
  env_result <- runHeteroTests(test_obj$model, test_obj$data, tests = "white")
  factory_result <- test_factory$run_test("white", test_obj$model, test_obj$data)
  
  # Should have same statistical content (allowing for metadata differences)
  expect_equal(env_result$white$statistic, factory_result$statistic)
  expect_equal(env_result$white$p.value, factory_result$p.value)
  expect_equal(env_result$white$method, factory_result$method)
})

# Registry Isolation
# =============================================================================

test_that("registries don't interfere with each other", {
  # Register something in diagnostic registry
  test_func <- function(model, data) list(statistic = 1, p.value = 0.5, method = "Test")
  registerDiagnostic("isolation_test", test_func)
  
  # Should not affect plot registry
  plot_registry <- as.list(.plot_registry)
  expect_false("isolation_test" %in% names(plot_registry))
  
  # Register something in plot registry  
  plot_func <- function(model) ggplot2::ggplot()
  registerPlot("isolation_plot", plot_func)
  
  # Should not affect diagnostic registry
  diag_registry <- as.list(.diagnostic_registry)
  expect_false("isolation_plot" %in% names(diag_registry))
})

test_that("registry modifications are persistent within session", {
  # Register a test
  persistent_test <- function(model, data) {
    structure(
      list(statistic = 99, p.value = 0.01, method = "Persistent"),
      class = "htest"
    )
  }
  
  registerDiagnostic("persistent_test", persistent_test)
  
  # Should be available in subsequent calls
  registry1 <- as.list(.diagnostic_registry)
  expect_true("persistent_test" %in% names(registry1))
  
  # Should still be there after other operations
  test_obj <- create_test_model()
  runHeteroTests(test_obj$model, test_obj$data, tests = "white")
  
  registry2 <- as.list(.diagnostic_registry)
  expect_true("persistent_test" %in% names(registry2))
  
  # Should be functional
  result <- runHeteroTests(test_obj$model, test_obj$data, tests = "persistent_test")
  expect_htest(result$persistent_test)
  expect_equal(result$persistent_test$statistic, 99)
})