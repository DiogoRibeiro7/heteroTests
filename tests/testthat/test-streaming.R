# Tests for streaming diagnostics and caching behaviour

library(testthat)

setup_test_model <- function(n = 120, seed = 123) {
  create_test_model(n = n, seed = seed)
}

test_that("streaming White test matches exact implementation", {
  test_obj <- setup_test_model(n = 160, seed = 42)
  exact <- performWhiteTest(test_obj$model, test_obj$data)
  streamed <- performWhiteTestStreaming(
    test_obj$model,
    test_obj$data,
    chunk_size = 40,
    progress = FALSE
  )
  expect_equal(streamed$statistic, exact$statistic, tolerance = 1e-8)
  expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-8)
})

test_that("streaming Breusch-Pagan test matches exact implementation", {
  test_obj <- setup_test_model(n = 150, seed = 99)
  exact <- performBPTest(test_obj$model, test_obj$data)
  streamed <- performBPTestStreaming(
    test_obj$model,
    test_obj$data,
    chunk_size = 30,
    progress = FALSE
  )
  expect_equal(streamed$statistic, exact$statistic, tolerance = 1e-8)
  expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-8)
})

test_that("streaming Koenker test matches exact implementation", {
  test_obj <- setup_test_model(n = 140, seed = 321)
  exact <- performKoenkerTest(test_obj$model, test_obj$data)
  streamed <- performKoenkerTestStreaming(
    test_obj$model,
    test_obj$data,
    chunk_size = 35,
    progress = FALSE
  )
  expect_equal(streamed$statistic, exact$statistic, tolerance = 1e-8)
  expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-8)
})

test_that("runHeteroTests caches and reuses results", {
  test_obj <- setup_test_model(n = 120, seed = 777)
  clearAnalysisCache()
  first <- runHeteroTests(
    test_obj$model,
    test_obj$data,
    tests = c("white", "breusch_pagan"),
    use_cache = TRUE,
    progress = FALSE
  )
  second <- runHeteroTests(
    test_obj$model,
    test_obj$data,
    tests = c("white", "breusch_pagan"),
    use_cache = TRUE,
    progress = FALSE
  )
  expect_equal(first$white$statistic, second$white$statistic)
  expect_equal(first$breusch_pagan$statistic, second$breusch_pagan$statistic)
  clearAnalysisCache()
})

test_that("runHeteroTests switches to streaming when threshold is low", {
  test_obj <- setup_test_model(n = 90, seed = 555)
  results <- runHeteroTests(
    test_obj$model,
    test_obj$data,
    tests = c("white", "breusch_pagan", "koenker"),
    use_cache = FALSE,
    chunk_threshold_mb = 0.001,
    chunk_size = 20,
    progress = FALSE
  )
  expect_match(results$white$method, "streaming", ignore.case = TRUE)
  expect_match(results$breusch_pagan$method, "streaming", ignore.case = TRUE)
  expect_match(results$koenker$method, "streaming", ignore.case = TRUE)
})

test_that("bootstrap helper returns the requested number of replicates", {
  test_obj <- setup_test_model(n = 60, seed = 246)
  boot <- rbootstrap_test_statistic(
    performWhiteTest,
    test_obj$model,
    test_obj$data,
    B = 20,
    parallel = FALSE,
    progress = FALSE
  )
  expect_equal(length(boot$replicates), 20L)
  expect_true(sum(is.finite(boot$replicates)) > 0)
})

test_that("bootstrap helper supports parallel execution", {
  skip_on_cran()
  test_obj <- setup_test_model(n = 50, seed = 135)
  boot <- rbootstrap_test_statistic(
    performWhiteTest,
    test_obj$model,
    test_obj$data,
    B = 16,
    parallel = TRUE,
    n_cores = 1,
    progress = FALSE
  )
  expect_equal(length(boot$replicates), 16L)
})
