chunk_progress_bar <- function(total, enabled = interactive(), label = "Processing chunks") {
  if (!enabled || total <= 1L) {
    update <- function(...) invisible(NULL)
    close_bar <- function(...) invisible(NULL)
    return(list(update = update, close = close_bar))
  }

  message(label)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  list(
    update = function(value) {
      utils::setTxtProgressBar(pb, value)
      invisible(NULL)
    },
    close = function() {
      close(pb)
      invisible(NULL)
    }
  )
}

chunk_indices <- function(n, chunk_size) {
  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }
  chunk_size <- max(1L, as.integer(chunk_size))
  split(seq_len(n), ((seq_len(n) - 1L) %/% chunk_size) + 1L)
}

should_use_sparse <- function(rows, cols) {
  requireNamespace("Matrix", quietly = TRUE) && rows * cols > 1e6
}

chunked_regression_r2 <- function(model, data, residuals, chunk_size,
                                  predictor_builder, response_builder,
                                  progress = FALSE, progress_label = "Processing chunks") {
  n <- length(residuals)
  if (n == 0L) {
    stop("No observations available after preprocessing.", call. = FALSE)
  }

  chunks <- chunk_indices(n, chunk_size)
  progress_bar <- chunk_progress_bar(length(chunks), progress, progress_label)
  on.exit(progress_bar$close(), add = TRUE)

  terms_obj <- stats::terms(model)
  contrasts <- model$contrasts
  xlevels <- model$xlevels

  XtX <- NULL
  XtY <- NULL
  y_sum <- 0
  y_sq_sum <- 0
  total_n <- 0
  design_names <- NULL

  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    chunk_data <- data[idx, , drop = FALSE]
    mm_chunk <- stats::model.matrix(
      terms_obj,
      data = chunk_data,
      contrasts.arg = contrasts,
      xlev = xlevels
    )

    predictors <- predictor_builder(mm_chunk, chunk_data, residuals[idx])
    predictors <- as.matrix(predictors)

    if (ncol(predictors) == 0) {
      stop("Predictor builder produced an empty matrix.", call. = FALSE)
    }

    if (is.null(colnames(predictors))) {
      colnames(predictors) <- paste0("V", seq_len(ncol(predictors)))
    }

    design_chunk <- cbind(`(Intercept)` = 1, predictors)
    if (is.null(design_names)) {
      design_names <- colnames(design_chunk)
      k <- ncol(design_chunk)
      XtX <- matrix(0, nrow = k, ncol = k)
      XtY <- numeric(k)
    } else {
      missing_names <- setdiff(design_names, colnames(design_chunk))
      if (length(missing_names) > 0L) {
        extras <- matrix(0, nrow = nrow(design_chunk), ncol = length(missing_names))
        colnames(extras) <- missing_names
        design_chunk <- cbind(design_chunk, extras)
      }
      design_chunk <- design_chunk[, design_names, drop = FALSE]
    }

    if (ncol(design_chunk) != nrow(XtX)) {
      stop("Inconsistent design matrix dimensions between chunks.", call. = FALSE)
    }

    response <- response_builder(residuals[idx], chunk_data, mm_chunk)
    response <- as.numeric(response)
    if (length(response) != nrow(design_chunk)) {
      stop("Response builder returned a vector of incorrect length.", call. = FALSE)
    }

    if (should_use_sparse(nrow(design_chunk), ncol(design_chunk))) {
      design_sparse <- Matrix::Matrix(design_chunk, sparse = TRUE)
      XtX <- XtX + as.matrix(Matrix::crossprod(design_sparse))
      XtY <- XtY + as.numeric(Matrix::crossprod(design_sparse, response))
    } else {
      XtX <- XtX + crossprod(design_chunk)
      XtY <- XtY + crossprod(design_chunk, response)
    }

    y_sum <- y_sum + sum(response)
    y_sq_sum <- y_sq_sum + sum(response^2)
    total_n <- total_n + length(response)

    progress_bar$update(i)
  }

  if (total_n == 0L) {
    stop("No data processed during chunked regression.", call. = FALSE)
  }

  qr_decomp <- qr(XtX)
  if (qr_decomp$rank < ncol(XtX)) {
    return(list(rank_deficient = TRUE, df = ncol(XtX) - 1L, chunks = length(chunks)))
  }

  beta <- tryCatch(
    solve(XtX, XtY),
    error = function(e) {
      qr.solve(XtX, XtY)
    }
  )

  sse <- y_sq_sum - sum(beta * XtY)
  sse <- max(sse, 0)
  mean_y <- y_sum / total_n
  sst <- y_sq_sum - total_n * mean_y^2

  if (sst <= .Machine$double.eps) {
    return(list(zero_variation = TRUE, df = ncol(XtX) - 1L, chunks = length(chunks)))
  }

  r_squared <- 1 - (sse / sst)
  r_squared <- min(max(r_squared, 0), 1)

  list(
    r_squared = r_squared,
    df = ncol(XtX) - 1L,
    n = total_n,
    response_mean = mean_y,
    response_tss = sst,
    chunks = length(chunks)
  )
}

white_predictor_builder <- function(cross_products = TRUE, max_interactions = 10) {
  function(model_matrix, data_chunk, residuals_chunk) {
    if (ncol(model_matrix) < 2L) {
      stop("White test requires at least one regressor beyond the intercept.", call. = FALSE)
    }

    if (colnames(model_matrix)[1] == "(Intercept)") {
      X <- model_matrix[, -1, drop = FALSE]
    } else {
      X <- model_matrix
    }

    aux_matrix <- X
    regressor_names <- colnames(X)
    aux_names <- regressor_names

    for (j in seq_len(ncol(X))) {
      aux_matrix <- cbind(aux_matrix, X[, j]^2)
      aux_names <- c(aux_names, paste0(regressor_names[j], "_sq"))
    }

    if (cross_products && ncol(X) > 1) {
      if (ncol(X) > max_interactions) {
        std_warning("cross_products_omitted")
      } else {
        for (i in seq_len(ncol(X) - 1L)) {
          for (j in seq.int(i + 1L, ncol(X))) {
            aux_matrix <- cbind(aux_matrix, X[, i] * X[, j])
            aux_names <- c(aux_names, paste0(regressor_names[i], "_x_", regressor_names[j]))
          }
        }
      }
    }

    colnames(aux_matrix) <- aux_names
    aux_matrix
  }
}

bp_predictor_builder <- function(model_matrix, data_chunk, residuals_chunk) {
  if (colnames(model_matrix)[1] == "(Intercept)") {
    predictors <- model_matrix[, -1, drop = FALSE]
  } else {
    predictors <- model_matrix
  }
  if (ncol(predictors) == 0) {
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }
  predictors
}

koenker_predictor_builder <- function(model_matrix, data_chunk, residuals_chunk) {
  bp_predictor_builder(model_matrix, data_chunk, residuals_chunk)
}

white_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  residuals_chunk^2
}

bp_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  residuals_chunk^2
}

koenker_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  # Koenker's studentized BP regresses the squared residuals on the regressors
  # (n R^2); it is the squared-residual auxiliary regression, not the absolute one.
  residuals_chunk^2
}

#' Memory-efficient White test for large datasets
#'
#' Computes White's statistic via chunked cross-products rather than fitting the
#' full auxiliary regression in memory. This streaming approach allows the test
#' to scale to datasets that would otherwise exhaust available RAM.
#'
#' @inheritParams performWhiteTest
#' @param chunk_size Positive integer specifying the number of observations per
#'   chunk. Smaller values reduce memory usage at the expense of additional
#'   iteration overhead.
#' @param progress Logical flag indicating whether a progress bar should be
#'   displayed while streaming the data. Defaults to `interactive()`.
#'
#' @return A \link[stats:htest]{htest} object containing the chi-squared statistic, p-value,
#'   and metadata about the chunked computation.
#'
#' @details
#' The streaming implementation iteratively builds the cross-product matrices
#' required for the auxiliary regression without materialising the full design
#' matrix. Each chunk contributes to \eqn{X'X}, \eqn{X'y}, and summary statistics
#' for the response. The final \eqn{n R^2} statistic is then computed exactly as
#' in the standard White test, ensuring numerical equivalence while dramatically
#' reducing peak memory usage. When `Matrix` is installed, sparse cross-products
#' are leveraged automatically for large chunks.
#'
#' @references
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroscedasticity. *Econometrica, 48*(4), 817–838.
#'
#' @examples
#' data(mtcars)
#' performWhiteTestStreaming(lm(mpg ~ wt + qsec, data = mtcars), mtcars, chunk_size = 16)
#'
#' @seealso
#' [performWhiteTest()] for the exact computation and \link[=performWhiteTestRobust]{performWhiteTestRobust()} for
#' enhanced reporting.
#' @export
performWhiteTestStreaming <- function(model, data, chunk_size = 10000,
                                      cross_products = TRUE, max_interactions = 10,
                                      progress = interactive()) {
  checkModelEnhanced(model, data)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "White test",
    min_obs_model = 20L,
    min_obs_data = 20L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  rprocessValidationResult(requirements)

  stats <- chunked_regression_r2(
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = white_predictor_builder(cross_products, max_interactions),
    response_builder = white_response_builder,
    progress = progress,
    progress_label = "Streaming White test"
  )

  if (isTRUE(stats$rank_deficient)) {
    std_error(
      "rassumption_violation",
      assumption = paste(
        "Auxiliary regression became rank deficient; consider disabling cross-product terms",
        "or removing collinear predictors"
      )
    )
  }

  if (isTRUE(stats$zero_variation)) {
    std_error(
      "rassumption_violation",
      assumption = "White test requires variation in squared residuals"
    )
  }

  test_statistic <- stats$n * stats$r_squared
  p_value <- stats::pchisq(test_statistic, stats$df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = stats$df),
      p.value = p_value,
      method = "White's test for heteroscedasticity (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Streaming Breusch–Pagan test for large datasets
#'
#' Computes the classical Breusch–Pagan statistic using chunked cross-products
#' to avoid materialising the full auxiliary regression when working with large
#' data frames. The streamed result is algebraically equivalent to the standard
#' implementation while substantially reducing peak memory usage.
#'
#' @inheritParams performBPTest
#' @param chunk_size Positive integer giving the number of observations per
#'   streaming chunk.
#' @param progress Logical flag indicating whether a textual progress bar should
#'   be displayed while processing the chunks. Defaults to `interactive()`.
#'
#' @return A \link[stats:htest]{htest} object mirroring [performBPTest()] with additional
#'   metadata describing the chunking configuration.
#'
#' @export
performBPTestStreaming <- function(model, data, chunk_size = 10000,
                                   progress = interactive()) {
  test_label <- "Breusch-Pagan"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("breusch_pagan", model = model, data = working_data)
  rprocessValidationResult(requirements)

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires residual variation greater than numerical precision"
    )
  }

  stats <- chunked_regression_r2(
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = bp_predictor_builder,
    response_builder = bp_response_builder,
    progress = progress,
    progress_label = "Streaming Breusch-Pagan test"
  )

  if (isTRUE(stats$rank_deficient)) {
    std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan auxiliary regression became rank deficient"
    )
  }

  if (isTRUE(stats$zero_variation)) {
    std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires variation in squared residuals"
    )
  }

  # Classical BP = 0.5 * ESS_f, with f = e^2/sigma^2 - 1. Scaling the auxiliary
  # response by a constant 1/sigma^2 scales the explained sum of squares by
  # 1/sigma^4, so 0.5 * ESS_f = 0.5 * (R^2 * TSS_{e^2}) / sigma^4. This is
  # algebraically identical to the exact performBPTest() computation.
  sigma2 <- stats$response_mean
  ess <- stats$r_squared * stats$response_tss
  test_statistic <- 0.5 * ess / sigma2^2
  df <- stats$df
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Streaming Koenker studentized Breusch–Pagan test
#'
#' Evaluates Koenker's studentized variant of the Breusch–Pagan test (the
#' \eqn{n R^2} statistic from regressing the squared residuals on the regressors)
#' using streamed cross-products so that large datasets can be processed without
#' allocating the full auxiliary regression matrix.
#'
#' @inheritParams performKoenkerTest
#' @param chunk_size Positive integer giving the number of observations processed
#'   per streaming chunk.
#' @param progress Logical indicating whether to display a textual progress bar.
#'
#' @return A \link[stats:htest]{htest} result equivalent to [performKoenkerTest()] with
#'   additional metadata describing the streaming configuration.
#'
#' @export
performKoenkerTestStreaming <- function(model, data, chunk_size = 10000,
                                        progress = interactive()) {
  test_label <- "Koenker"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("koenker", model = model, data = working_data)
  rprocessValidationResult(requirements)

  stats <- chunked_regression_r2(
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = koenker_predictor_builder,
    response_builder = koenker_response_builder,
    progress = progress,
    progress_label = "Streaming Koenker test"
  )

  if (isTRUE(stats$rank_deficient)) {
    std_error(
      "rassumption_violation",
      assumption = "Koenker auxiliary regression became rank deficient"
    )
  }

  if (isTRUE(stats$zero_variation)) {
    std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in squared residuals"
    )
  }

  test_statistic <- stats$n * stats$r_squared
  df <- stats$df
  p_value <- 1 - stats::pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Parallel test execution helper
#'
#' Executes a set of registered diagnostics in parallel when multiple CPU cores
#' are available, falling back to sequential execution otherwise.
#'
#' @param model Fitted model object passed to each diagnostic. Must be compatible
#'   with the internal `.test_factory` registry.
#' @param data Data frame used to fit `model` and supplied to each diagnostic.
#' @param tests Character vector naming the diagnostics to execute.
#' @param n_cores Optional positive integer giving the number of worker processes
#'   to spawn. Defaults to `parallel::detectCores() - 1`, with a lower bound of 1.
#'
#' @return Named list of test results in the order provided by `tests`.
#'
#' @details
#' When `n_cores` exceeds one and the `parallel` package is available, the helper
#' evaluates the diagnostics via [parallel::parLapply()] after exporting the
#' model and data to the worker environment. On systems without fork support the
#' function gracefully reverts to sequential execution.
#'
#' @seealso
#' [cachedTest()] for memoised execution of individual diagnostics.
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
      .run_registered_diagnostic(t, model, data)
    })
    names(results) <- tests
  } else {
    results <- lapply(tests, function(t) .run_registered_diagnostic(t, model, data))
    names(results) <- tests
  }

  results
}

#' Cache results of a heteroscedasticity test
#' 
#' Avoids recomputing expensive diagnostics by hashing their inputs and reusing
#' stored results.
#'
#' @param test_name Character scalar naming the diagnostic registered in
#'   `.diagnostic_registry` or `.test_factory`.
#' @param model Fitted model object supplied to the diagnostic.
#' @param data Data frame used to fit the model.
#' @param ... Additional arguments forwarded to the diagnostic.
#' @param use_cache Logical scalar indicating whether cached results may be used
#'   (defaults to `TRUE`).
#'
#' @return The diagnostic result, either retrieved from cache or freshly
#'   computed.
#'
#' @details
#' The cache key is created by hashing the test name, model coefficients,
#' residuals, data, and additional arguments. When the key already exists in the
#' cache environment the stored result is returned immediately; otherwise the
#' diagnostic is executed and its output stored for future calls. Set
#' `use_cache = FALSE` to force recomputation when the underlying model has
#' changed.
#'
#' @seealso
#' [clearTestCache()] resets the cache between analysis sessions.
#' @export
cachedTest <- function(test_name, model, data, ..., use_cache = TRUE) {
  if (!use_cache) {
    return(.run_registered_diagnostic(test_name, model, data, ...))
  }

  if (!requireNamespace("digest", quietly = TRUE)) {
    return(.run_registered_diagnostic(test_name, model, data, ...))
  }

  key <- digest::digest(list(test_name, model$coefficients, model$residuals, data, list(...)))

  if (exists(key, envir = .test_cache)) {
    return(get(key, envir = .test_cache))
  }

  result <- .run_registered_diagnostic(test_name, model, data, ...)
  assign(key, result, envir = .test_cache)
  result
}

.run_registered_diagnostic <- function(test_name, model, data, ...) {
  if (exists(test_name, envir = .diagnostic_registry, inherits = FALSE)) {
    fun <- get(test_name, envir = .diagnostic_registry, inherits = FALSE)
    result <- fun(model, data, ...)
    return(.ht_decorate_result(result, test_name, model, data))
  }

  available <- tryCatch(.test_factory$get_available(), error = function(e) character())
  if (test_name %in% available) {
    result <- .test_factory$run_test(test_name, model, data, ...)
    return(.ht_decorate_result(result, test_name, model, data))
  }

  stop("Unknown test: ", test_name)
}

#' Clear cached test results
#' 
#' Removes all objects from the internal cache environment so that subsequent
#' diagnostic calls recompute their statistics.
#'
#' @seealso
#' [cachedTest()] for invoking diagnostics with memoisation.
#'
#' @export
clearTestCache <- function() {
  rm(list = ls(envir = .test_cache), envir = .test_cache)
  invisible(NULL)
}

.test_cache <- new.env(parent = emptyenv())

.analysis_cache <- new.env(parent = emptyenv())

.analysis_cache_key <- function(model, data, tests, streaming, chunk_size) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NULL)
  }

  safe_sigma <- tryCatch(stats::sigma(model), error = function(e) NULL)

  tryCatch(
    digest::digest(
      list(
        tests = as.character(tests),
        streaming = streaming,
        chunk_size = chunk_size,
        coefficients = stats::coef(model),
        residual_hash = digest::digest(stats::residuals(model), algo = "xxhash64"),
        sigma = safe_sigma,
        data_hash = digest::digest(data, algo = "xxhash64")
      ),
      algo = "xxhash64"
    ),
    error = function(e) NULL
  )
}

.analysis_cache_get <- function(key) {
  if (is.null(key) || !exists(key, envir = .analysis_cache, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = .analysis_cache, inherits = FALSE)
}

.analysis_cache_set <- function(key, value) {
  if (!is.null(key)) {
    assign(key, value, envir = .analysis_cache)
  }
  value
}

#' Clear cached diagnostic run results
#'
#' Removes memoised outputs stored by [runHeteroTests()] when `use_cache = TRUE`.
#' This is useful for deterministic test runs or when the underlying data/model
#' have changed and cached summaries should be invalidated.
#'
#' @seealso [runHeteroTests()], [cachedTest()] for per-diagnostic caching.
#' @export
clearAnalysisCache <- function() {
  rm(list = ls(envir = .analysis_cache), envir = .analysis_cache)
  invisible(NULL)
}
