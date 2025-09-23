#' Run multiple heteroscedasticity tests
#'
#' This convenience wrapper runs diagnostic functions registered in the
#' package registry and returns their results as a list. By default it
#' executes White's test and the Breusch-Pagan test, but additional tests or
#' custom diagnostics registered via `registerDiagnostic()` can be requested.
#'
#' @param model A fitted model of class `lm`.
#' @param data Optional data frame used to fit `model`. If not supplied,
#'   `model.frame(model)` is used.
#' @param use_cache Logical, reuse cached diagnostic results when available.
#'   Requires the **digest** package for hashing inputs and defaults to `TRUE`.
#' @param chunk_threshold_mb Numeric threshold (in megabytes) above which
#'   streaming implementations are preferred for supported diagnostics.
#' @param chunk_size Integer number of observations processed per chunk when
#'   streaming tests on large datasets.
#' @param progress Logical flag controlling whether textual progress bars are
#'   displayed while running multiple diagnostics.
#'
#' @return A named list of `htest` objects.
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
  checkModel(model)
  if (is.null(data)) {
    data <- model.frame(model)
  } else {
    checkData(data)
  }

  data_size <- tryCatch(
    check_memory_usage(data, threshold_mb = chunk_threshold_mb / 2),
    error = function(e) NA_real_
  )
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

    if (identical(runner, available[[test_name]]) && isTRUE(use_cache)) {
      res[[i]] <- cachedTest(test_name, model, data)
    } else {
      res[[i]] <- runner(model, data)
    }

    progress_bar$update(i)
  }

  if (!is.null(cache_key)) {
    .analysis_cache_set(cache_key, list(
      results = res,
      timestamp = Sys.time(),
      parameters = list(
        streaming = use_streaming,
        chunk_size = chunk_size,
        tests = tests
      )
    ))
  }

  res
}
