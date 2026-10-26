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

test_that("the streaming tests give the value of the exact tests in chunks of any size", {
  # Since 0.16.0 they read the design of the fit chunk by chunk, as the exact
  # tests read it whole (the model context); before, each chunk built its own
  # design from the formula, on its rows of `data`. Chunks of 7 leave a last
  # chunk of one observation, 119 one of one, 120 a single chunk.
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  d$w <- 1 / d$x1^2
  d$o <- 0.3 * d$x2
  fits <- list(
    ols = lm(y ~ x1 + x2, data = d),
    wls = lm(y ~ x1 + x2, data = d, weights = w),
    glm = glm(y ~ x1 + x2, data = d, weights = w),
    offset = lm(y ~ x1 + x2, data = d, offset = o),
    log = lm(y ~ log(x1) + x2, data = d),
    deficient = lm(y ~ x1 + x2 + I(2 * x2), data = d)
  )
  pairs <- list(
    list(performWhiteTestStreaming, performWhiteTest),
    list(performBPTestStreaming, performBPTest),
    list(performKoenkerTestStreaming, performKoenkerTest)
  )
  for (nm in names(fits)) {
    m <- fits[[nm]]
    for (pair in pairs) {
      exact <- pair[[2]](m, d)
      for (chunk in c(7, 25, 119, 120, 10000)) {
        for (data in list(d, NULL)) {
          # White's streaming test warns of the collinearity of the
          # rank-deficient fit.
          streamed <- suppressWarnings(pair[[1]](m, data, chunk_size = chunk, progress = FALSE))
          info <- paste(nm, exact$method, chunk, is.null(data))
          expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8,
                       info = info)
          expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-8, info = info)
          expect_equal(unname(streamed$parameter), unname(exact$parameter), info = info)
          expect_identical(streamed$chunks_processed, as.integer(ceiling(nrow(d) / chunk)),
                           info = info)
        }
      }
    }
  }
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

test_that("a finite chunk_threshold_mb asks the runner for the streaming tests", {
  test_obj <- setup_test_model(n = 90, seed = 555)
  # The data are above half the threshold: "Large dataset detected".
  results <- suppressWarnings(runHeteroTests(
    test_obj$model,
    test_obj$data,
    tests = c("white", "breusch_pagan", "koenker"),
    use_cache = FALSE,
    chunk_threshold_mb = 0.001,
    chunk_size = 20,
    progress = FALSE
  ))
  for (nm in c("white", "breusch_pagan", "koenker")) {
    expect_match(results[[nm]]$method, "streaming", ignore.case = TRUE, info = nm)
    expect_true(attr(results[[nm]], "extras")$streaming, info = nm)
    expect_identical(results[[nm]]$chunk_size, 20L, info = nm)
  }
})

# Data "above 100 MB" without allocating them: every measurement of the size
# of the data in the package, by check_memory_usage(), says `size_mb`.
local_data_size <- function(size_mb, env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_memory_usage = function(data, threshold_mb = 100) invisible(size_mb),
    .package = "heteroTests",
    .env = env
  )
}

test_that("by default the runner runs the exact tests, even on data above 100 MB", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")))
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  test_obj <- setup_test_model(n = 90, seed = 555)
  m <- test_obj$model
  d <- test_obj$data
  tests <- c("white", "breusch_pagan", "koenker")
  run <- function(...) {
    suppressMessages(runHeteroTests(m, d, tests = tests, use_cache = FALSE, progress = FALSE, ...))
  }
  exact <- list(
    performWhiteTest = performWhiteTest(m, d),
    performBPTest = performBPTest(m, d),
    performKoenkerTest = performKoenkerTest(m, d)
  )
  small <- run()
  diagnostics <- suppressMessages(runDiagnostics(m, d, tests = tests, use_cache = FALSE, progress = FALSE))

  # Up to 0.15.0 the runner, with its default of 100 MB, and the exact tests
  # themselves switched to the streaming tests above 100 MB.
  local_data_size(150)
  expect_identical(heteroTests:::check_memory_usage(d), 150)
  large <- run()
  for (nm in tests) {
    expect_false(attr(large[[nm]], "extras")$streaming, info = nm)
    expect_no_match(large[[nm]]$method, "streaming", ignore.case = TRUE)
  }
  expect_identical(large, small)
  expect_identical(
    suppressMessages(runDiagnostics(m, d, tests = tests, use_cache = FALSE, progress = FALSE)),
    diagnostics
  )
  expect_identical(performWhiteTest(m, d), exact$performWhiteTest)
  expect_identical(performBPTest(m, d), exact$performBPTest)
  expect_identical(performKoenkerTest(m, d), exact$performKoenkerTest)
  # The threshold of before, given, still streams above it, and not below.
  streamed <- run(chunk_threshold_mb = 100, chunk_size = 30)
  for (nm in tests) {
    expect_true(attr(streamed[[nm]], "extras")$streaming, info = nm)
    expect_match(streamed[[nm]]$method, "streaming", ignore.case = TRUE)
  }
  expect_identical(run(chunk_threshold_mb = 200), small)
})

test_that("a model with a factor gets White's test above 100 MB, not a fallback", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")))
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  m <- lm(y ~ x1 + group, data = d)
  white <- performWhiteTest(m, d)
  small <- runHeteroTests(m, d, use_cache = FALSE, progress = FALSE)
  # The squares of the dummies are collinear with them: the streaming test
  # stops, and with it asked for the runner falls back under the name "white".
  expect_error(
    performWhiteTestStreaming(m, d, progress = FALSE),
    "rank deficient"
  )
  asked <- suppressWarnings(
    runHeteroTests(m, d, use_cache = FALSE, chunk_threshold_mb = 0, progress = FALSE)
  )
  expect_identical(attr(asked$white, "extras")$status, "fallback")

  local_data_size(150)
  large <- runHeteroTests(m, d, use_cache = FALSE, progress = FALSE)
  expect_identical(large, small)
  expect_identical(attr(large$white, "diagnostic"), "white")
  expect_null(attr(large$white, "extras")$status)
  expect_identical(unname(large$white$statistic), unname(white$statistic))
  expect_identical(unname(large$white$parameter), unname(white$parameter))
  expect_identical(performWhiteTest(m, d), white)
  # With the streaming tests asked for, White's fallback without cross
  # products is the exact test on data of any size; up to 0.15.0 it switched
  # to the streaming test above 100 MB, stopped too, and the runner returned
  # the classical Breusch-Pagan test under the name "white".
  asked_large <- runHeteroTests(m, d, use_cache = FALSE, chunk_threshold_mb = 100, progress = FALSE)
  expect_identical(asked_large, asked)
  expect_identical(asked_large$white$method, performWhiteTest(m, d, cross_products = FALSE)$method)
})

test_that("bootstrap helper returns the requested number of replicates", {
  test_obj <- setup_test_model(n = 60, seed = 246)
  boot <- bootstrapTestStatistic(
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
  boot <- bootstrapTestStatistic(
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
