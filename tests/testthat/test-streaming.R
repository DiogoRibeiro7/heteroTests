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
  expect_identical_numbers(large, small)
  expect_identical_numbers(
    suppressMessages(runDiagnostics(m, d, tests = tests, use_cache = FALSE, progress = FALSE)),
    diagnostics
  )
  expect_identical_numbers(performWhiteTest(m, d), exact$performWhiteTest)
  expect_identical_numbers(performBPTest(m, d), exact$performBPTest)
  expect_identical_numbers(performKoenkerTest(m, d), exact$performKoenkerTest)
  # The threshold of before, given, still streams above it, and not below.
  streamed <- run(chunk_threshold_mb = 100, chunk_size = 30)
  for (nm in tests) {
    expect_true(attr(streamed[[nm]], "extras")$streaming, info = nm)
    expect_match(streamed[[nm]]$method, "streaming", ignore.case = TRUE)
  }
  expect_identical_numbers(run(chunk_threshold_mb = 200), small)
})

test_that("a model with a factor gets White's test above 100 MB, not a fallback", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")))
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  m <- lm(y ~ x1 + group, data = d)
  white <- performWhiteTest(m, d)
  small <- runHeteroTests(m, d, use_cache = FALSE, progress = FALSE)
  # The squares of the dummies are collinear with them: since 0.17.0 the
  # streaming test drops them, as performWhiteTest() does, and with it asked
  # for the runner runs it under the name "white". Up to 0.16.0 it stopped
  # ("Auxiliary regression became rank deficient", N3), and the runner fell
  # back on White's test without cross products.
  streamed <- performWhiteTestStreaming(m, d, progress = FALSE)
  expect_equal(unname(streamed$statistic), unname(white$statistic), tolerance = 1e-8)
  expect_identical(unname(streamed$parameter), unname(white$parameter))
  asked <- suppressWarnings(
    runHeteroTests(m, d, use_cache = FALSE, chunk_threshold_mb = 0, progress = FALSE)
  )
  expect_null(attr(asked$white, "extras")$status)
  expect_true(attr(asked$white, "extras")$streaming)
  expect_identical_numbers(unname(asked$white$statistic), unname(streamed$statistic))

  local_data_size(150)
  large <- runHeteroTests(m, d, use_cache = FALSE, progress = FALSE)
  expect_identical_numbers(large, small)
  expect_identical(attr(large$white, "diagnostic"), "white")
  expect_null(attr(large$white, "extras")$status)
  expect_identical_numbers(unname(large$white$statistic), unname(white$statistic))
  expect_identical(unname(large$white$parameter), unname(white$parameter))
  expect_identical_numbers(performWhiteTest(m, d), white)
  # With the streaming tests asked for, the runner streams White's test on
  # data of any size. Up to 0.15.0 it switched to the streaming test above
  # 100 MB, which stopped, and returned the classical Breusch-Pagan test under
  # the name "white"; in 0.16.0 it fell back on the exact test without cross
  # products.
  asked_large <- runHeteroTests(m, d, use_cache = FALSE, chunk_threshold_mb = 100, progress = FALSE)
  expect_identical_numbers(asked_large, asked)
  expect_identical(asked_large$white$method, streamed$method)
})

test_that("White's streaming test drops collinear terms, as performWhiteTest() does (N3)", {
  # Up to 0.16.0 it stopped with "Auxiliary regression became rank deficient"
  # whenever terms of its auxiliary regression were collinear: on any model
  # with a factor, whose dummies square to themselves, a poly() term, or x
  # beside I(x^2). It now counts them by the rank performWhiteTest() takes, on
  # the auxiliary design of the centred regressors, and drops them.
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  d$w <- 1 / d$x1^2
  fits <- list(
    factor = lm(y ~ x1 + group, data = d),
    factor_x2 = lm(y ~ x1 + x2 + group, data = d),
    interaction = lm(y ~ x1 * group, data = d),
    sum_contrasts = lm(y ~ x1 + group, data = d, contrasts = list(group = "contr.sum")),
    no_intercept = lm(y ~ 0 + group + x1, data = d),
    poly = lm(y ~ poly(x1, 2) + x2, data = d),
    square = lm(y ~ x1 + I(x1^2) + x2, data = d),
    weighted = lm(y ~ x1 + group, data = d, weights = w),
    glm = glm(y ~ x1 + group, data = d, weights = w)
  )
  for (nm in names(fits)) {
    m <- fits[[nm]]
    for (cross_products in c(TRUE, FALSE)) {
      exact <- performWhiteTest(m, d, cross_products = cross_products)
      for (chunk in c(7, 40, 10000)) {
        info <- paste(nm, cross_products, chunk)
        # The checks of the model warn of the collinearity of x1 and x1^2.
        streamed <- suppressWarnings(
          performWhiteTestStreaming(m, d, chunk_size = chunk, cross_products = cross_products,
                                    progress = FALSE)
        )
        expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-10,
                     info = info)
        expect_identical(unname(streamed$parameter), unname(exact$parameter), info = info)
        expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-10, info = info)
      }
    }
  }
  # The model with a factor: 17.830 on 8 degrees of freedom, where it stopped.
  streamed <- performWhiteTestStreaming(fits$factor, d, chunk_size = 7, progress = FALSE)
  expect_equal(unname(streamed$statistic), 17.830176, tolerance = 1e-6)
  expect_identical(unname(streamed$parameter), 8L)
})

test_that("the streaming tests do not depend on where the regressors are located", {
  # The regressors are centred at their means before the auxiliary design is
  # built, as performWhiteTest() centres them, and the chunks are reduced by a
  # QR decomposition, not cross-products. Up to 0.16.0 the cross-products of
  # a regressor 10,000 standard deviations from zero looked collinear: White's
  # test stopped on the squares of x1 + 1e4 sd, and the Breusch-Pagan and
  # Koenker tests on x1 itself beside the intercept.
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  pairs <- list(
    list(performWhiteTestStreaming, performWhiteTest),
    list(performBPTestStreaming, performBPTest),
    list(performKoenkerTestStreaming, performKoenkerTest)
  )
  at_zero <- lapply(pairs, function(pair) pair[[2]](lm(y ~ x1 + group, data = d), d))
  for (k in c(1e2, 1e3, 1e4)) {
    moved <- d
    moved$x1 <- d$x1 + k * stats::sd(d$x1)
    moved$x2 <- d$x2 + k * stats::sd(d$x2)
    for (f in list(y ~ x1 + group, y ~ x1 + x2)) {
      m <- lm(f, data = moved)
      for (i in seq_along(pairs)) {
        exact <- pairs[[i]][[2]](m, moved)
        for (chunk in c(7, 10000)) {
          info <- paste(k, deparse(f), exact$method, chunk)
          streamed <- pairs[[i]][[1]](m, moved, chunk_size = chunk, progress = FALSE)
          expect_identical(unname(streamed$parameter), unname(exact$parameter), info = info)
          expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8,
                       info = info)
        }
      }
    }
    # The value of the same model located at zero.
    m <- lm(y ~ x1 + group, data = moved)
    for (i in seq_along(pairs)) {
      streamed <- pairs[[i]][[1]](m, moved, chunk_size = 7, progress = FALSE)
      expect_equal(unname(streamed$statistic), unname(at_zero[[i]]$statistic), tolerance = 1e-8,
                   info = paste(k, i))
      expect_identical(unname(streamed$parameter), unname(at_zero[[i]]$parameter), info = paste(k, i))
    }
  }
})

test_that("the streaming tests take squared residuals of any scale", {
  # Up to 0.16.0 they refused squared residuals whose total sum of squares
  # about their mean was at most .Machine$double.eps, whatever their scale:
  # residuals of about 1e-5 were refused, "requires variation in squared
  # residuals", where the exact tests give 12.787, 12.997 and 10.289 and the
  # same data times 1e5 streamed to 12.787. The total is now compared with
  # the scale of the squared residuals.
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  set.seed(11)
  n <- 120
  d <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n))
  d$y <- 1e-5 * rnorm(n, sd = d$x1)
  m <- lm(y ~ x1 + x2, data = d)
  pairs <- list(
    list(performWhiteTestStreaming, performWhiteTest, 12.787139),
    list(performBPTestStreaming, performBPTest, 12.996911),
    list(performKoenkerTestStreaming, performKoenkerTest, 10.289153)
  )
  for (pair in pairs) {
    exact <- pair[[2]](m, d)
    expect_equal(unname(exact$statistic), pair[[3]], tolerance = 1e-6)
    # Residuals of 1e-7 and less have a variance of at most 1e-14, near the
    # absolute threshold of every test on the variance of the residuals,
    # .Machine$double.eps ("Residual variance is too small").
    for (scale in c(1e-2, 1, 1e5, 1e8)) {
      scaled <- d
      scaled$y <- d$y * scale
      for (chunk in c(7, 10000)) {
        info <- paste(exact$method, scale, chunk)
        streamed <- pair[[1]](lm(y ~ x1 + x2, data = scaled), scaled, chunk_size = chunk,
                              progress = FALSE)
        expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8,
                     info = info)
        expect_identical(unname(streamed$parameter), unname(exact$parameter), info = info)
      }
    }
  }
})

test_that("the streaming tests refuse squared residuals that do not vary", {
  # Residuals of +1 and -1, orthogonal to the regressors: the squared
  # residuals are 1 but for rounding, at any scale. The exact tests compute
  # from that rounding (Koenker's 18.36, White's 18.82 at scale 1); the
  # streaming tests refuse them, at every scale and in chunks of any size.
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  base <- data.frame(x = rep(1:10, each = 4), s = rep(c(1, -1, -1, 1), 10))
  tests <- c(White = "performWhiteTestStreaming", `Breusch-Pagan` = "performBPTestStreaming",
             Koenker = "performKoenkerTestStreaming")
  for (scale in c(1e-5, 1, 1e5)) {
    d <- base
    d$y <- base$s * scale
    m <- lm(y ~ x, data = d)
    e2 <- unname(residuals(m))^2
    expect_lt(max(abs(e2 / mean(e2) - 1)), 1e-12)
    for (label in names(tests)) {
      for (chunk in c(7, 10000)) {
        expect_error(
          suppressWarnings(get(tests[[label]])(m, d, chunk_size = chunk, progress = FALSE)),
          paste(label, "test requires variation in squared residuals"), fixed = TRUE,
          info = paste(label, scale, chunk)
        )
      }
    }
  }
  # Squared residuals exactly constant, through the regression itself.
  design <- cbind(`(Intercept)` = 1, x = rep(1:10, each = 4))
  residuals <- rep(c(2, -2, -2, 2), 10)
  for (chunk in c(7, 40)) {
    out <- heteroTests:::chunked_regression_r2(design, residuals, chunk,
                                               heteroTests:::bp_predictor_builder,
                                               heteroTests:::bp_response_builder)
    expect_true(out$zero_variation)
    expect_identical(out$r_squared, NA_real_)
  }
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
