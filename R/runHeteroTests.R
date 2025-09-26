#' Run multiple heteroscedasticity tests
#'
#' This convenience wrapper runs diagnostic functions registered in the
#' package registry and returns their results as a list. By default it
#' executes White's test and the Breusch-Pagan test, but additional tests or
#' custom diagnostics registered via `registerDiagnostic()` can be requested.
#'
#' @param model A fitted model of class `lm`/`glm`, a model formula, a
#'   tidymodels [workflows::workflow] or a [parsnip::model_fit] object.
#' @param data Optional data source used to fit `model`. Accepts base data
#'   frames, tibbles, `data.table`s, `dtplyr_step`s and grouped data produced by
#'   [dplyr::group_by()]. When `model` is a formula the data must be supplied.
#' @param use_cache Logical, reuse cached diagnostic results when available.
#'   Requires the **digest** package for hashing inputs and defaults to `TRUE`.
#' @param chunk_threshold_mb Numeric threshold (in megabytes) above which
#'   streaming implementations are preferred for supported diagnostics.
#' @param chunk_size Integer number of observations processed per chunk when
#'   streaming tests on large datasets.
#' @param progress Logical flag controlling whether textual progress bars are
#'   displayed while running multiple diagnostics.
#'
#' @return A [`hetero_test_suite`] list of diagnostic results. When grouped data
#'   are supplied the return value is a [`hetero_grouped_suite`] containing a
#'   per-group `hetero_test_suite`.
#'
#' @seealso \code{\link{runDiagnostics}} for a broader suite that includes
#'   collinearity and influence measures, \code{\link{runPanelTests}} for panel
#'   data models and \code{\link{runTimeSeriesTests}} for time-series checks.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' runHeteroTests(m, mtcars)
#' registerDiagnostic("my_stat", function(model, data) list(statistic = 0))
#' runHeteroTests(m, mtcars, tests = c("white", "my_stat"))
#'
#' # Combine with advanced residual analysis
#' adv <- analyzeMLResiduals(m, mtcars)
runHeteroTests <- function(model, data = NULL,
                           tests = c("white", "breusch_pagan"),
                           use_cache = TRUE,
                           chunk_threshold_mb = 100,
                           chunk_size = 10000,
                           progress = interactive()) {
  prepared <- .ht_prepare_model(model, data = data, context = "runHeteroTests")

  if (prepared$grouped && length(prepared$group_splits) > 0) {
    if (is.null(prepared$fit_factory)) {
      stop(
        "Grouped diagnostics require a formula-based model specification so each group can be refitted.",
        call. = FALSE
      )
    }
    group_results <- vector("list", length(prepared$group_splits))
    for (i in seq_along(prepared$group_splits)) {
      group_data <- prepared$group_splits[[i]]
      group_model <- prepared$fit_factory(group_data)
      group_results[[i]] <- .run_hetero_core(
        group_model,
        group_data,
        tests = tests,
        use_cache = FALSE,
        chunk_threshold_mb = chunk_threshold_mb,
        chunk_size = chunk_size,
        progress = progress,
        recover_failures = TRUE
      )
    }
    structure(
      group_results,
      class = c("hetero_grouped_suite", "list"),
      group_keys = prepared$group_keys,
      tests = tests
    )
  } else {
    .run_hetero_core(
      prepared$model,
      prepared$data,
      tests = tests,
      use_cache = use_cache,
      chunk_threshold_mb = chunk_threshold_mb,
      chunk_size = chunk_size,
      progress = progress,
      recover_failures = FALSE
    )
  }
}

.run_hetero_core <- function(model, data, tests, use_cache, chunk_threshold_mb, chunk_size, progress, recover_failures = FALSE) {
  data_size <- if (is.null(data)) {
    NA_real_
  } else {
    tryCatch(
      check_memory_usage(data, threshold_mb = chunk_threshold_mb / 2),
      error = function(e) NA_real_
    )
  }
  use_streaming <- !is.na(data_size) && data_size > chunk_threshold_mb

  available <- as.list(.diagnostic_registry)
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop("Unknown tests: ", paste(invalid, collapse = ", "))
  }

  cache_key <- if (isTRUE(use_cache)) {
    analysis_key <- .analysis_cache_key(model, data, tests,
      streaming = use_streaming,
      chunk_size = chunk_size
    )
    if (!is.null(analysis_key)) {
      cached <- .analysis_cache_get(analysis_key)
      if (!is.null(cached)) {
        return(cached$results)
      }
    }
    analysis_key
  } else {
    NULL
  }

  streaming_map <- list(
    white = function(model, data) {
      performWhiteTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    },
    breusch_pagan = function(model, data) {
      performBPTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    },
    koenker = function(model, data) {
      performKoenkerTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    }
  )

  progress_bar <- chunk_progress_bar(length(tests), progress && length(tests) > 1L,
    label = "Running heteroscedasticity diagnostics"
  )
  on.exit(progress_bar$close(), add = TRUE)

  res <- vector("list", length(tests))
  names(res) <- tests

  for (i in seq_along(tests)) {
    test_name <- tests[i]
    runner <- available[[test_name]]

    if (use_streaming && test_name %in% names(streaming_map)) {
      runner <- streaming_map[[test_name]]
    }

    result <- tryCatch(
      {
        if (identical(runner, available[[test_name]]) && isTRUE(use_cache)) {
          cachedTest(test_name, model, data)
        } else {
          runner(model, data)
        }
      },
      error = function(e) {
        ht_log("WARN", sprintf("%s failed: %s", test_name, conditionMessage(e)))
        failure <- .ht_failure_result(test_name, model, data, conditionMessage(e))
        if (isTRUE(recover_failures)) {
          return(failure)
        }
        stop(conditionMessage(e), call. = FALSE)
      }
    )

    res[[i]] <- .ht_decorate_result(
      result,
      diagnostic_name = test_name,
      model = model,
      data = data,
      extras = list(streaming = use_streaming && test_name %in% names(streaming_map))
    )

    progress_bar$update(i)
  }

  output <- structure(
    res,
    class = c("hetero_test_suite", "list"),
    tests = tests,
    model = model,
    data_source = if (is.null(data)) NA_character_ else class(data)[1]
  )

  if (!is.null(cache_key)) {
    .analysis_cache_set(cache_key, list(
      results = output,
      timestamp = Sys.time(),
      parameters = list(
        streaming = use_streaming,
        chunk_size = chunk_size,
        tests = tests
      )
    ))
  }

  output
}
