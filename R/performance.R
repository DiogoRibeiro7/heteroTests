#' Memory-efficient White test for large datasets
#'
#' Splits the input data into manageable chunks and runs White's
#' test on each subset. The statistics are then averaged to produce
#' an overall result.
#'
#' @inheritParams performWhiteTest
#' @param chunk_size Number of observations per chunk.
#' @return An object of class `htest` summarizing the combined results.
#' @export
performWhiteTestStreaming <- function(model, data, chunk_size = 10000) {
  checkModelEnhanced(model, data)

  n <- nrow(data)
  if (n <= chunk_size) {
    return(performWhiteTest(model, data))
  }

  warning(sprintf("Using streaming implementation for large dataset (n=%d)", n),
    call. = FALSE
  )

  chunks <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))
  chunk_results <- lapply(chunks, function(idx) {
    chunk_data <- data[idx, , drop = FALSE]
    chunk_model <- lm(formula(model), data = chunk_data)
    performWhiteTest(chunk_model, chunk_data)
  })

  combined_stat <- mean(vapply(chunk_results, function(x) x$statistic, numeric(1)))
  combined_p <- mean(vapply(chunk_results, function(x) x$p.value, numeric(1)))

  structure(
    list(
      statistic = c("X-squared" = combined_stat),
      p.value = combined_p,
      method = "White's test (streaming implementation)",
      data.name = paste("chunked data, n =", n),
      chunks_processed = length(chunks)
    ),
    class = "htest"
  )
}

#' Parallel test execution for runHeteroTests
#'
#' Executes diagnostic tests in parallel if multiple cores are available.
#'
#' @param model Fitted model.
#' @param data Data used to fit the model.
#' @param tests Character vector of test names.
#' @param n_cores Number of cores to use. Defaults to available cores minus one.
#'
#' @return Named list of test results.
#' @export
runHeteroTestsParallel <- function(model, data, tests, n_cores = NULL) {
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1L, length(tests))
  }

  if (n_cores > 1L && requireNamespace("parallel", quietly = TRUE)) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, library(heteroTests))
    parallel::clusterExport(cl, c("model", "data"), envir = environment())

    results <- parallel::parLapply(cl, tests, function(t) {
      .test_factory$run_test(t, model, data)
    })
    names(results) <- tests
  } else {
    results <- lapply(tests, function(t) .test_factory$run_test(t, model, data))
    names(results) <- tests
  }

  results
}

#' Cache results of a heteroscedasticity test
#'
#' Uses a digest of the inputs to avoid re-running expensive diagnostics.
#'
#' @param test_name Name of the test registered in `test_factory`.
#' @param model Fitted model object.
#' @param data Data used to fit the model.
#' @param ... Additional arguments passed to the test function.
#' @param use_cache Logical; if `FALSE`, forces re-computation.
#'
#' @return Test result object.
#' @export
cachedTest <- function(test_name, model, data, ..., use_cache = TRUE) {
  if (!use_cache) {
    return(.test_factory$run_test(test_name, model, data, ...))
  }

  if (!requireNamespace("digest", quietly = TRUE)) {
    return(.test_factory$run_test(test_name, model, data, ...))
  }

  key <- digest::digest(list(test_name, model$coefficients, model$residuals, data, list(...)))

  if (exists(key, envir = .test_cache)) {
    return(get(key, envir = .test_cache))
  }

  result <- .test_factory$run_test(test_name, model, data, ...)
  assign(key, result, envir = .test_cache)
  result
}

#' Clear cached test results
#'
#' Removes all objects from the internal test cache.
#'
#' @export
clearTestCache <- function() {
  rm(list = ls(envir = .test_cache), envir = .test_cache)
  invisible(NULL)
}

.test_cache <- new.env(parent = emptyenv())
