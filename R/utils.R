# Utility functions for input validation and logging

#' Validate model argument
#'
#' Checks that `model` is of class `lm` or `glm`.
#'
#' @param model An object to check.
#' @return Invisible `model` if valid, otherwise an error is thrown.
#' @export
checkModel <- function(model) {
  if (!inherits(model, c("lm", "glm"))) {
    std_error("invalid_model")
  }
  invisible(model)
}

# logging -----------------------------------------------------------------

.ht_log_levels <- c(INFO = 1L, WARN = 2L, ERROR = 3L)

.ht_log_state <- local({
  env <- new.env(parent = emptyenv())
  env$level <- "INFO"
  env$capture <- FALSE
  env$max_entries <- 1000L
  env$history <- data.frame(
    timestamp = as.POSIXct(character()),
    level = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
  env$sink <- NULL
  env
})

#' Log a formatted message
#'
#' This helper wraps [base::message()] but prepends a log level for
#' clearer diagnostics when running algorithms. Logs are filtered according
#' to [ht_set_log_level()] and can optionally be captured with
#' [ht_enable_log_capture()]. Intended for internal use.
#'
#' @param level One of "INFO", "WARN" or "ERROR".
#' @param msg Character string with the message to log.
#' @return `NULL`, invoked for its side effect.
#' @keywords internal
ht_log <- function(level = c("INFO", "WARN", "ERROR"), msg) {
  level <- match.arg(level)
  msg <- as.character(msg)
  state <- .ht_log_state
  severity <- .ht_log_levels[[level]]
  threshold <- state$level
  threshold_value <- if (identical(threshold, "SILENT")) {
    Inf
  } else {
    .ht_log_levels[[threshold]]
  }

  timestamp <- Sys.time()
  if (isTRUE(state$capture)) {
    state$history <- rbind(
      state$history,
      data.frame(
        timestamp = timestamp,
        level = level,
        message = msg,
        stringsAsFactors = FALSE
      )
    )
    n_rows <- nrow(state$history)
    if (!is.null(state$max_entries) && state$max_entries > 0L && n_rows > state$max_entries) {
      drop_n <- n_rows - state$max_entries
      state$history <- state$history[seq.int(drop_n + 1L, n_rows), , drop = FALSE]
    }
  }

  sink_target <- state$sink
  if (!is.null(sink_target)) {
    formatted <- sprintf("%s [%s] %s", format(timestamp, tz = "UTC"), level, msg)
    if (inherits(sink_target, "connection")) {
      tryCatch(writeLines(formatted, con = sink_target), error = function(e) invisible(NULL))
    } else if (is.character(sink_target) && length(sink_target) == 1L) {
      tryCatch(cat(formatted, "\n", file = sink_target, append = TRUE), error = function(e) invisible(NULL))
    }
  }

  if (!is.infinite(threshold_value) && severity >= threshold_value) {
    message(sprintf("[%s] %s", level, msg))
  }

  invisible(NULL)
}

#' Set log verbosity for heteroTests
#'
#' Controls which log messages emitted via [ht_log()] are printed. Messages are
#' always captured when log capture is enabled, even if suppressed by the
#' threshold.
#'
#' @param level Character string specifying the minimum level to emit. Accepted
#'   values are "INFO", "WARN", "ERROR" and "SILENT".
#' @return The previous log level (invisibly).
#' @export
ht_set_log_level <- function(level = c("INFO", "WARN", "ERROR", "SILENT")) {
  level <- match.arg(level)
  previous <- .ht_log_state$level
  .ht_log_state$level <- level
  invisible(previous)
}

#' Enable log capture for debugging complex diagnostics
#'
#' When enabled, log messages are recorded in memory (and optionally mirrored to
#' a file or connection) for later inspection via [ht_log_history()].
#'
#' @param enabled Logical flag, `TRUE` to enable capture and `FALSE` to disable.
#' @param max_entries Maximum number of entries to retain in memory. Older
#'   entries are discarded first. Use `Inf` to keep all entries.
#' @param sink Optional file path or connection to mirror log output.
#' @return Invisibly returns `NULL`.
#' @export
ht_enable_log_capture <- function(enabled = TRUE, max_entries = 1000L, sink = NULL) {
  .ht_log_state$capture <- isTRUE(enabled)
  if (is.finite(max_entries) && max_entries <= 0L) {
    max_entries <- 1000L
  }
  .ht_log_state$max_entries <- max_entries
  if (!is.null(sink) && !inherits(sink, "connection") && !is.character(sink)) {
    stop("`sink` must be NULL, a connection or a file path.", call. = FALSE)
  }
  .ht_log_state$sink <- sink
  invisible(NULL)
}

#' Retrieve captured log messages
#'
#' Returns a data frame containing the timestamp, level and message for recent
#' log entries captured via [ht_enable_log_capture()].
#'
#' @return A data frame with columns `timestamp`, `level` and `message`.
#' @export
ht_log_history <- function() {
  .ht_log_state$history
}

#' Clear captured log history
#'
#' Removes all stored log entries accumulated via [ht_enable_log_capture()].
#'
#' @return Invisibly returns `NULL`.
#' @export
ht_clear_log_history <- function() {
  .ht_log_state$history <- .ht_log_state$history[0, , drop = FALSE]
  invisible(NULL)
}

#' Safely fit a linear model
#'
#' Wraps [stats::lm()] in a `tryCatch` block that logs the error via
#' [ht_log()] before rethrowing. This helper is used internally by
#' diagnostic tests so failures are easier to debug.
#'
#' @param formula Model formula.
#' @param data Data frame to evaluate the formula in.
#' @param ... Additional arguments passed to [stats::lm()].
#' @return A fitted model object.
#' @keywords internal
safe_lm <- function(formula, data, ..., .recover = TRUE) {
  tryCatch(
    lm(formula, data = data, ...),
    error = function(e) {
      ht_log("WARN", paste("Auxiliary regression failed:", conditionMessage(e)))
      recovery <- list(suggestions = character())
      if (.recover) {
        recovery <- ht_recover_auxiliary_fit(formula, data, ..., error = e)
        if (!is.null(recovery$fit)) {
          ht_log("INFO", sprintf("Recovered auxiliary regression via %s", recovery$strategy))
          attr(recovery$fit, "ht_recovery") <- recovery
          return(recovery$fit)
        }
      }
      stop(ht_format_failure_message("auxiliary_regression", conditionMessage(e), recovery$suggestions), call. = FALSE)
    }
  )
}

#' Validate data argument
#'
#' Ensures that `data` is a data.frame. Used internally for input
#' validation across the package.
#'
#' @param data Object to check.
#' @return Invisible `data` if valid, otherwise an error is thrown.
#' @export
checkData <- function(data) {
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }
  invisible(data)
}

#' Validate numeric vector
#'
#' Ensures `x` is numeric and non-empty. Used internally by helpers
#' that expect numeric input.
#'
#' @param x Object to check.
#' @param name Optional variable name for error messages.
#' @return Invisible `x` if valid, otherwise an error is thrown.
#' @keywords internal
checkNumericVector <- function(x, name = "x") {
  if (!is.numeric(x)) {
    stop(sprintf("`%s` must be numeric", name), call. = FALSE)
  }
  if (length(x) == 0L) {
    stop(sprintf("`%s` must not be empty", name), call. = FALSE)
  }
  invisible(x)
}

#' Safe variance calculation
#'
#' Computes the variance of `x` while guarding against
#' near-zero values that could lead to division by zero
#' in subsequent calculations.
#'
#' @param x Numeric vector.
#' @return A variance value with a minimum of `.Machine$double.eps`.
#' @keywords internal
safe_var <- function(x) {
  checkNumericVector(x, "x")

  v <- var(x, na.rm = TRUE)
  if (is.na(v) || v < .Machine$double.eps) {
    warning("Near-zero variance detected", call. = FALSE)
    return(.Machine$double.eps)
  }
  v
}

#' Check memory usage and warn for large datasets
#'
#' @param data The dataset to check
#' @param threshold_mb Memory threshold in MB (default: 100MB)
#' @keywords internal
check_memory_usage <- function(data, threshold_mb = 100) {

  # Calculate object size in MB
  size_mb <- as.numeric(object.size(data)) / 1024^2

  if (size_mb > threshold_mb) {
    warning(
      "Large dataset detected (", round(size_mb, 1), " MB). ",
      "This may require significant memory and processing time. ",
      "Consider using a subset for initial analysis.",
      call. = FALSE
    )
  }

  # Check available memory if possible
  if (.Platform$OS.type == "unix") {
    tryCatch({
      mem_info <- system("free -m", intern = TRUE)
      # Parse and warn if memory is low
    }, error = function(e) {
      # Silently continue if can't check memory
    })
  }

  invisible(size_mb)
}
