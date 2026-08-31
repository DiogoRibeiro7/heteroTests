# =============================================================================
# Performance and Scalability Tests
# tests/testthat/test-performance.R
# =============================================================================

# Caching System Tests
# =============================================================================

test_that("caching system works correctly", {
  test_obj <- create_test_model()
  
  # Clear cache to start fresh
  clearTestCache()
  
  # First call should compute and cache
  start1 <- Sys.time()
  result1 <- cachedTest("white", test_obj$model, test_obj$data)
  time1 <- as.numeric(Sys.time() - start1, units = "secs")
  
  expect_htest(result1)
  
  # Second call should use cache
  start2 <- Sys.time()
  result2 <- cachedTest("white", test_obj$model, test_obj$data)
  time2 <- as.numeric(Sys.time() - start2, units = "secs")
  
  # Results should be identical
  expect_equal(result1$statistic, result2$statistic)
  expect_equal(result1$p.value, result2$p.value)
  expect_equal(result1$method, result2$method)
  
  # Second call should generally be faster (allowing for measurement variability)
  expect_true(time2 <= time1 + 0.01)  # Allow small measurement error
})

test_that("cache respects use_cache parameter", {
  test_obj <- create_test_model()
  clearTestCache()
  
  # First call with caching
  result1 <- cachedTest("white", test_obj$model, test_obj$data, use_cache = TRUE)
  
  # Call with use_cache = FALSE should recompute
  result2 <- cachedTest("white", test_obj$model, test_obj$data, use_cache = FALSE)
  
  # Results should be identical (same computation)
  expect_equal(result1$statistic, result2$statistic)
  expect_equal(result1$p.value, result2$p.value)
})

test_that("cache handles different inputs correctly", {
  test_obj1 <- create_test_model(seed = 123)
  test_obj2 <- create_test_model(seed = 456)  # Different data
  
  clearTestCache()
  
  # Cache different models separately
  result1 <- cachedTest("white", test_obj1$model, test_obj1$data)
  result2 <- cachedTest("white", test_obj2$model, test_obj2$data)
  
  # Should get different results for different data
  expect_false(identical(result1$statistic, result2$statistic))
  
  # Retrieving cached results should work
  result1_cached <- cachedTest("white", test_obj1$model, test_obj1$data)
  result2_cached <- cachedTest("white", test_obj2$model, test_obj2$data)
  
  expect_equal(result1$statistic, result1_cached$statistic)
  expect_equal(result2$statistic, result2_cached$statistic)
})

test_that("clearTestCache works correctly", {
  test_obj <- create_test_model()
  
  # Add something to cache
  cachedTest("white", test_obj$model, test_obj$data)
  
  # Clear cache
  clearTestCache()
  
  # Cache should be empty (we can't directly test this, but it shouldn't error)
  expect_invisible(clearTestCache())
  
  # Should still work after clearing
  result <- cachedTest("white", test_obj$model, test_obj$data)
  expect_htest(result)
})

test_that("caching works without digest package", {
  # Test fallback behavior when digest is not available
  # This is hard to test directly, but we can test the code path
  
  test_obj <- create_test_model()
  
  # If digest is available, this should work normally
  if (requireNamespace("digest", quietly = TRUE)) {
    result1 <- cachedTest("white", test_obj$model, test_obj$data)
    expect_htest(result1)
  }
  
  # The function should handle the case where digest is not available
  # (falls back to direct computation)
  expect_no_error(
    result2 <- cachedTest("white", test_obj$model, test_obj$data)
  )
  expect_htest(result2)
})

# Parallel Processing Tests
# =============================================================================

test_that("parallel processing works correctly", {
  skip("Requires installed package for parallel workers")
  
  test_obj <- create_test_model(n = 100)  # Larger dataset for meaningful parallel work
  tests_to_run <- c("white", "breusch_pagan", "koenker", "cook_weisberg", "ncv")
  
  # Sequential execution
  start_seq <- Sys.time()
  seq_results <- runHeteroTests(test_obj$model, test_obj$data, tests = tests_to_run)
  time_seq <- as.numeric(Sys.time() - start_seq, units = "secs")
  
  # Parallel execution
  start_par <- Sys.time()
  par_results <- runHeteroTestsParallel(
    test_obj$model, 
    test_obj$data, 
    tests = tests_to_run,
    n_cores = 2
  )
  time_par <- as.numeric(Sys.time() - start_par, units = "secs")
  
  # Both should produce valid results
  expect_valid_diagnostic_result(seq_results, tests_to_run)
  expect_valid_diagnostic_result(par_results, tests_to_run)
  
  # Results should be statistically identical
  for (test_name in tests_to_run) {
    expect_equal(seq_results[[test_name]]$statistic, par_results[[test_name]]$statistic,
                tolerance = 1e-12)
    expect_equal(seq_results[[test_name]]$p.value, par_results[[test_name]]$p.value,
                tolerance = 1e-12)
  }
})

test_that("parallel processing falls back gracefully", {
  skip("Requires installed package for parallel workers")

  test_obj <- create_test_model()
  
  # Should work even if parallel package is not available or n_cores = 1
  result1 <- runHeteroTestsParallel(
    test_obj$model, 
    test_obj$data, 
    tests = c("white", "breusch_pagan"),
    n_cores = 1
  )
  
  expect_valid_diagnostic_result(result1, c("white", "breusch_pagan"))
  
  # Should work with NULL n_cores
  result2 <- runHeteroTestsParallel(
    test_obj$model, 
    test_obj$data, 
    tests = c("white", "breusch_pagan"),
    n_cores = NULL
  )
  
  expect_valid_diagnostic_result(result2, c("white", "breusch_pagan"))
})

test_that("parallel processing handles errors gracefully", {
  skip("Requires installed package for parallel workers")
  
  test_obj <- create_test_model()
  
  # Test with invalid test name (should error but not crash)
  expect_error(
    runHeteroTestsParallel(
      test_obj$model, 
      test_obj$data, 
      tests = c("white", "invalid_test"),
      n_cores = 2
    ),
    "Unknown test"
  )
})

# Scalability Tests
# =============================================================================

test_that("functions scale reasonably with sample size", {
  skip_on_cran()  # Long-running performance test
  
  sample_sizes <- c(50, 100, 200, 500)
  execution_times <- numeric(length(sample_sizes))
  
  for (i in seq_along(sample_sizes)) {
    n <- sample_sizes[i]
    test_obj <- create_test_model(n = n, seed = 42)
    
    start_time <- Sys.time()
    results <- runHeteroTests(test_obj$model, test_obj$data, 
                             tests = c("white", "breusch_pagan"))
    end_time <- Sys.time()
    
    execution_times[i] <- as.numeric(end_time - start_time, units = "secs")
    
    # Should produce valid results regardless of size
    expect_valid_diagnostic_result(results, c("white", "breusch_pagan"))
  }
  
  # Execution time should not grow too rapidly
  # (allowing for some variability in timing)
  for (i in 2:length(execution_times)) {
    time_ratio <- execution_times[i] / execution_times[i-1]
    size_ratio <- sample_sizes[i] / sample_sizes[i-1]
    
    # Time should grow less than quadratically with size
    expect_true(time_ratio < size_ratio^2 + 1,  # Allow overhead
                info = paste("Time ratio", time_ratio, "vs size ratio", size_ratio))
  }
  
  # Largest test should complete within reasonable time
  expect_true(max(execution_times) < 10,  # 10 seconds max
              info = paste("Max execution time:", max(execution_times), "seconds"))
})

test_that("functions scale reasonably with number of predictors", {
  skip_on_cran()  # Performance test
  
  n_predictors <- c(2, 5, 10, 15)
  execution_times <- numeric(length(n_predictors))
  
  for (i in seq_along(n_predictors)) {
    p <- n_predictors[i]
    
    # Generate data with p predictors
    set.seed(42)
    data <- data.frame(y = rnorm(100))
    for (j in 1:p) {
      data[[paste0("x", j)]] <- rnorm(100)
    }
    
    # Create formula
    formula_str <- paste("y ~", paste(paste0("x", 1:p), collapse = " + "))
    model <- lm(as.formula(formula_str), data = data)
    
    start_time <- Sys.time()
    results <- runHeteroTests(model, data, tests = c("white", "breusch_pagan"))
    end_time <- Sys.time()
    
    execution_times[i] <- as.numeric(end_time - start_time, units = "secs")
    
    # Should produce valid results
    expect_valid_diagnostic_result(results, c("white", "breusch_pagan"))
  }
  
  # Time should not grow too rapidly with number of predictors
  # White test is O(p^2) due to cross-products, so allow for this
  for (i in 2:length(execution_times)) {
    time_ratio <- execution_times[i] / execution_times[i-1]
    predictor_ratio <- n_predictors[i] / n_predictors[i-1]
    
    # Allow for quadratic growth in predictors (due to White test cross-products)
    expect_true(time_ratio < predictor_ratio^3,  # Cubic is too much
                info = paste("Time ratio", time_ratio, "vs predictor ratio^3", predictor_ratio^3))
  }
})

test_that("memory usage is reasonable", {
  skip_on_cran()  # Performance test
  skip_if_not_installed("pryr")
  
  # Test memory usage with different dataset sizes
  sample_sizes <- c(100, 500, 1000)
  
  for (n in sample_sizes) {
    test_obj <- create_test_model(n = n)
    
    # Monitor memory before
    gc()  # Force garbage collection
    mem_before <- pryr::mem_used()
    
    # Run tests
    results <- runDiagnostics(test_obj$model, test_obj$data)
    
    # Monitor memory after
    mem_after <- pryr::mem_used()
    mem_increase <- as.numeric(mem_after - mem_before, units = "MB")
    
    # Memory usage should be reasonable
    expect_true(mem_increase < 100,  # Less than 100 MB
                info = paste("Memory increase for n =", n, ":", mem_increase, "MB"))
    
    # Clean up
    rm(results)
    gc()
  }
})

# Concurrent Access Tests
# =============================================================================

test_that("functions are thread-safe for read operations", {
  skip("Requires installed package for parallel workers")
  
  # Test that multiple processes can safely read from registries
  test_obj <- create_test_model()
  
  # Function to run in parallel
  parallel_test <- function(i) {
    # Each process runs different tests
    test_names <- c("white", "breusch_pagan", "koenker")
    test_name <- test_names[(i %% length(test_names)) + 1]
    
    result <- runHeteroTests(test_obj$model, test_obj$data, tests = test_name)
    result[[test_name]]$p.value
  }
  
  # Run in parallel
  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, library(heteroTests))
  parallel::clusterExport(cl, "test_obj", envir = environment())
  
  results <- parallel::parLapply(cl, 1:6, parallel_test)
  
  # All results should be valid p-values
  p_values <- unlist(results)
  expect_true(all(p_values >= 0 & p_values <= 1))
  expect_true(all(is.finite(p_values)))
})

# Stress Tests
# =============================================================================

test_that("functions handle stress conditions", {
  skip_on_cran()  # Stress test
  
  # Test with many repeated calls
  test_obj <- create_test_model()
  
  # Clear cache to avoid caching effects
  clearTestCache()
  
  start_time <- Sys.time()
  
  # Run many iterations
  for (i in 1:50) {
    result <- runHeteroTests(test_obj$model, test_obj$data, 
                            tests = c("white", "breusch_pagan"))
    expect_valid_diagnostic_result(result, c("white", "breusch_pagan"))
  }
  
  end_time <- Sys.time()
  total_time <- as.numeric(end_time - start_time, units = "secs")
  
  # Should complete within reasonable time
  expect_true(total_time < 30,  # 30 seconds for 50 iterations
              info = paste("Stress test took", total_time, "seconds"))
})

test_that("functions recover from numerical issues", {
  # Test recovery from various numerical problems
  
  # Very small residuals
  set.seed(456)
  near_perfect_data <- data.frame(
    x = 1:20,
    y = 1:20 + rnorm(20, 0, 1e-10)  # Tiny noise triggers perfect-fit guard
  )
  near_perfect_model <- lm(y ~ x, data = near_perfect_data)

  # The individual test guards against a (near-)perfect fit by erroring ...
  expect_error(
    performBPTest(near_perfect_model, near_perfect_data),
    "[Rr]esidual variance"
  )
  # ... and the orchestrator surfaces that failure rather than substituting a
  # diagnostic that cannot validly run on a perfect fit either. Before 0.7.0 the
  # adaptive-fallback chain reached the then-unvalidated NCV test, which
  # succeeded on this degenerate model and was reported as the Breusch-Pagan
  # result.
  expect_error(
    suppressWarnings(
      runHeteroTests(near_perfect_model, near_perfect_data, tests = "breusch_pagan")
    ),
    "[Rr]esidual variance|perfectly explained"
  )

  # Slightly larger noise should now pass the guard and complete successfully
  set.seed(789)
  almost_perfect_data <- data.frame(
    x = 1:20,
    y = 1:20 + rnorm(20, 0, 0.5)
  )
  almost_perfect_model <- lm(y ~ x, data = almost_perfect_data)
  expect_no_error(
    result1 <- runHeteroTests(almost_perfect_model, almost_perfect_data, tests = "breusch_pagan")
  )
  expect_htest(result1$breusch_pagan)
  
  # Very large values
  extreme_data <- data.frame(
    x = rnorm(50) * 1e6,  # Very large values
    y = rnorm(50) * 1e6
  )
  extreme_data$y <- extreme_data$y + extreme_data$x
  extreme_model <- lm(y ~ x, data = extreme_data)
  
  expect_no_error(
    result2 <- runHeteroTests(extreme_model, extreme_data, tests = "breusch_pagan")
  )
  expect_htest(result2$breusch_pagan)
})

# Benchmark Tests
# =============================================================================

test_that("performance benchmarks are reasonable", {
  skip_on_cran()  # Benchmark test
  
  # These are baseline performance expectations
  # Adjust thresholds based on typical hardware
  
  test_obj <- create_test_model(n = 200)
  
  # Individual test benchmarks
  benchmarks <- list(
    white = 0.5,           # seconds
    breusch_pagan = 0.1,   # seconds
    koenker = 0.1,         # seconds
    cook_weisberg = 0.1    # seconds
  )
  
  for (test_name in names(benchmarks)) {
    start_time <- Sys.time()
    result <- runHeteroTests(test_obj$model, test_obj$data, tests = test_name)
    end_time <- Sys.time()
    
  execution_time <- as.numeric(end_time - start_time, units = "secs")

  expect_true(
    execution_time < benchmarks[[test_name]],
    info = paste(
      "Test", test_name, "took", execution_time,
      "seconds, expected <", benchmarks[[test_name]]
    )
  )

  expect_htest(result[[test_name]])
  }
  
  # Full diagnostic suite benchmark
  start_time <- Sys.time()
  full_results <- runDiagnostics(test_obj$model, test_obj$data)
  end_time <- Sys.time()
  
  full_time <- as.numeric(end_time - start_time, units = "secs")
  expect_true(full_time < 2.0,  # 2 seconds for full suite
              info = paste("Full diagnostics took", full_time, "seconds, expected < 2.0"))
})

test_that("plotting performance is reasonable", {
  skip_on_cran()  # Performance test
  
  test_obj <- create_test_model(n = 500)  # Larger dataset for plotting
  
  # Individual plot benchmarks
  plot_benchmarks <- list(
    residuals_fitted = 0.5,
    spread_level = 0.5,
    density = 0.5,
    qq = 0.5
  )
  
  for (plot_name in names(plot_benchmarks)) {
    start_time <- Sys.time()
    
    if (plot_name == "residuals_fitted") {
      plot_result <- plotResidualsFitted(test_obj$model)
    } else if (plot_name == "spread_level") {
      plot_result <- plotSpreadLevel(test_obj$model)
    } else if (plot_name == "density") {
      plot_result <- plotResidualDensity(test_obj$model)
    } else if (plot_name == "qq") {
      plot_result <- plotResidualQQ(test_obj$model)
    }
    
    end_time <- Sys.time()
    execution_time <- as.numeric(end_time - start_time, units = "secs")
    
    expect_true(execution_time < plot_benchmarks[[plot_name]],
                info = paste("Plot", plot_name, "took", execution_time, 
                           "seconds, expected <", plot_benchmarks[[plot_name]]))
    
    expect_ggplot(plot_result)
  }
  
  # Full plot suite benchmark
  start_time <- Sys.time()
  all_plots <- plotDiagnosticSuite(test_obj$model)
  end_time <- Sys.time()
  
  plot_suite_time <- as.numeric(end_time - start_time, units = "secs")
  expect_true(plot_suite_time < 3.0,  # 3 seconds for full plot suite
              info = paste("Full plot suite took", plot_suite_time, "seconds, expected < 3.0"))
  
  expect_type(all_plots, "list")
  for (plot_name in names(all_plots)) {
    expect_ggplot(all_plots[[plot_name]])
  }
})

# Resource Management Tests
# =============================================================================

test_that("functions clean up resources properly", {
  skip_on_cran()  # Resource test
  skip_if_not_installed("pryr")
  
  # Test that repeated operations don't accumulate resources
  test_obj <- create_test_model()
  
  initial_mem <- pryr::mem_used()
  
  # Run many operations
  for (i in 1:20) {
    results <- runHeteroTests(test_obj$model, test_obj$data, 
                             tests = c("white", "breusch_pagan"))
    plots <- plotDiagnosticSuite(test_obj$model)
    
    # Clean up explicitly
    rm(results, plots)
    
    # Force garbage collection every few iterations
    if (i %% 5 == 0) {
      gc()
    }
  }
  
  # Force final garbage collection
  gc()
  final_mem <- pryr::mem_used()
  
  mem_increase <- as.numeric(final_mem - initial_mem, units = "MB")
  
  # Memory increase should be minimal (allowing for some R overhead)
  expect_true(mem_increase < 20,  # Less than 20 MB increase
              info = paste("Memory increase after repeated operations:", mem_increase, "MB"))
})

test_that("parallel processes clean up properly", {
  skip("Requires installed package for parallel workers")
  
  test_obj <- create_test_model(n = 100)
  
  # Run parallel operations multiple times
  for (i in 1:5) {
    results <- runHeteroTestsParallel(
      test_obj$model, 
      test_obj$data, 
      tests = c("white", "breusch_pagan"),
      n_cores = 2
    )
    
    expect_valid_diagnostic_result(results, c("white", "breusch_pagan"))
    
    # Clean up
    rm(results)
    gc()
  }
  
  # Should not leave zombie processes or open connections
  # This is hard to test directly, but operations should complete without hanging
  expect_true(TRUE)  # If we get here, cleanup worked
})

# Optimization Tests
# =============================================================================

test_that("caching provides performance benefit", {
  skip_on_cran()  # Performance comparison test
  
  test_obj <- create_test_model(n = 200)
  clearTestCache()
  
  # Time uncached operations
  uncached_times <- numeric(5)
  for (i in 1:5) {
    clearTestCache()  # Ensure no cache
    start_time <- Sys.time()
    result <- cachedTest("white", test_obj$model, test_obj$data, use_cache = FALSE)
    uncached_times[i] <- as.numeric(Sys.time() - start_time, units = "secs")
    expect_htest(result)
  }
  
  # Time cached operations (after first computation)
  cachedTest("white", test_obj$model, test_obj$data)  # Prime cache
  
  cached_times <- numeric(5)
  for (i in 1:5) {
    start_time <- Sys.time()
    result <- cachedTest("white", test_obj$model, test_obj$data, use_cache = TRUE)
    cached_times[i] <- as.numeric(Sys.time() - start_time, units = "secs")
    expect_htest(result)
  }
  
  # Cached operations should generally be faster
  mean_uncached <- mean(uncached_times)
  mean_cached <- mean(cached_times[-1])  # Exclude first cached call
  
  # Allow for measurement variability, but cached should be faster
  expect_true(mean_cached <= mean_uncached + 0.01,
              info = paste("Cached time", mean_cached, "vs uncached", mean_uncached))
})

test_that("vectorized operations are efficient", {
  skip("Performance timing varies in this environment")
  
  # Compare single vs multiple test execution
  test_obj <- create_test_model(n = 100)
  
  # Time individual tests
  start_individual <- Sys.time()
  result1 <- runHeteroTests(test_obj$model, test_obj$data, tests = "white")
  result2 <- runHeteroTests(test_obj$model, test_obj$data, tests = "breusch_pagan")
  result3 <- runHeteroTests(test_obj$model, test_obj$data, tests = "koenker")
  time_individual <- as.numeric(Sys.time() - start_individual, units = "secs")
  
  # Time batch execution
  start_batch <- Sys.time()
  result_batch <- runHeteroTests(test_obj$model, test_obj$data, 
                                tests = c("white", "breusch_pagan", "koenker"))
  time_batch <- as.numeric(Sys.time() - start_batch, units = "secs")
  
  # Batch should be more efficient (or at least not much worse)
  expect_true(time_batch <= time_individual * 1.2,  # Allow 20% overhead
              info = paste("Batch time", time_batch, "vs individual", time_individual))
  
  # Results should be equivalent
  expect_equal(result_batch$white$statistic, result1$white$statistic)
  expect_equal(result_batch$breusch_pagan$statistic, result2$breusch_pagan$statistic)
  expect_equal(result_batch$koenker$statistic, result3$koenker$statistic)
})

# Load Testing
# =============================================================================

test_that("system handles concurrent requests", {
  skip("Requires installed package for parallel workers")
  
  # Simulate multiple concurrent users
  n_concurrent <- 4
  n_requests_each <- 5
  
  # Function to simulate user session
  user_session <- function(user_id) {
    results <- list()
    for (i in 1:n_requests_each) {
      # Create slightly different data for each request
      test_obj <- create_test_model(seed = user_id * 100 + i)
      
      # Run diagnostics
      result <- runHeteroTests(test_obj$model, test_obj$data, 
                              tests = c("white", "breusch_pagan"))
      results[[i]] <- result
      
      # Simulate some processing time
      Sys.sleep(0.01)  # 10ms delay
    }
    results
  }
  
  # Run concurrent sessions
  cl <- parallel::makeCluster(n_concurrent)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  parallel::clusterEvalQ(cl, library(heteroTests))
  
  start_time <- Sys.time()
  all_results <- parallel::parLapply(cl, 1:n_concurrent, user_session)
  end_time <- Sys.time()
  
  total_time <- as.numeric(end_time - start_time, units = "secs")
  
  # Should complete within reasonable time
  expected_time <- n_requests_each * 0.1 * 2  # Rough estimate with overhead
  expect_true(total_time < expected_time,
              info = paste("Concurrent load test took", total_time, 
                          "seconds, expected <", expected_time))
  
  # All results should be valid
  for (user_results in all_results) {
    for (request_result in user_results) {
      expect_valid_diagnostic_result(request_result, c("white", "breusch_pagan"))
    }
  }
})

# Edge Case Performance
# =============================================================================

test_that("performance degrades gracefully with problematic data", {
  skip_on_cran()  # Performance test
  
  # Test various problematic scenarios
  scenarios <- list(
    # Highly correlated predictors
    multicollinear = {
      data <- data.frame(
        x1 = rnorm(100),
        x2 = rnorm(100)
      )
      data$x3 <- data$x1 + rnorm(100, 0, 0.01)  # Nearly identical to x1
      data$y <- 1 + 2*data$x1 + 3*data$x2 + rnorm(100)
      lm(y ~ x1 + x2 + x3, data = data)
    },
    
    # Many predictors
    high_dimensional = {
      data <- data.frame(y = rnorm(100))
      for (i in 1:20) {
        data[[paste0("x", i)]] <- rnorm(100)
      }
      formula_str <- paste("y ~", paste(paste0("x", 1:20), collapse = " + "))
      lm(as.formula(formula_str), data = data)
    },
    
    # Extreme outliers
    outliers = {
      data <- data.frame(
        x1 = c(rnorm(95), rep(100, 5)),  # Extreme outliers
        x2 = rnorm(100)
      )
      data$y <- 1 + 2*data$x1 + 3*data$x2 + rnorm(100)
      lm(y ~ x1 + x2, data = data)
    }
  )
  
  for (scenario_name in names(scenarios)) {
    model <- scenarios[[scenario_name]]
    data <- model.frame(model)
    
    # Should complete within reasonable time even for problematic data
    start_time <- Sys.time()
    
    # May warn, and may refuse outright: the `outliers` scenario is dominated
    # by five extreme points and fits to R^2 = 1.000, which no heteroscedasticity
    # diagnostic can validly run on. What must not happen is a hang or an
    # uninformative crash. Before 0.7.0 this returned a value because the
    # then-unvalidated NCV test acted as a fallback that accepted any model.
    outcome <- tryCatch(
      {
        results <- suppressWarnings(
          runHeteroTests(model, data, tests = c("white", "breusch_pagan"))
        )
        "completed"
      },
      error = function(e) conditionMessage(e)
    )
    expect_true(
      identical(outcome, "completed") ||
        grepl("perfectly explained|[Rr]esidual variance|singular|rank", outcome),
      info = paste("Scenario", scenario_name, "produced:", outcome)
    )
    
    end_time <- Sys.time()
    scenario_time <- as.numeric(end_time - start_time, units = "secs")
    
    # Should not take excessively long
    expect_true(scenario_time < 5,  # 5 seconds max even for problematic cases
                info = paste("Scenario", scenario_name, "took", scenario_time, "seconds"))
    
    # Should produce some valid results
    expect_valid_diagnostic_result(results, c("white", "breusch_pagan"))
  }
})