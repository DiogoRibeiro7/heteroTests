# Utility functions for input validation and logging

#' Validate model argument
#'
#' Checks that `model` is of class `lm` or `glm`.
#'
#' @param model An object to check.
#' @return Invisible `model` if valid, otherwise an error is thrown.
#' @keywords internal
#' Log a formatted message
#'
#' This helper wraps [base::message()] but prepends a log level for
#' clearer diagnostics when running algorithms. Intended for internal use.
#'
#' @param level One of "INFO", "WARN" or "ERROR".
#' @param msg Character string with the message to log.
#' @return `NULL`, invoked for its side effect.
#' @keywords internal
ht_log <- function(level = c("INFO", "WARN", "ERROR"), msg) {
  level <- match.arg(level)
  message(sprintf("[%s] %s", level, msg))
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
safe_lm <- function(formula, data, ...) {
  tryCatch(
    lm(formula, data = data, ...),
    error = function(e) {
      ht_log("ERROR", paste("lm failed:", conditionMessage(e)))
      stop(e)
    }
  )
}

checkModel <- function(model) {
  if (!inherits(model, c("lm", "glm"))) {
    std_error("invalid_model")
  }
  invisible(model)
}

#' Validate data argument
#'
#' Ensures that `data` is a data.frame. Used internally for input
#' validation across the package.
#'
#' @param data Object to check.
#' @return Invisible `data` if valid, otherwise an error is thrown.
#' @keywords internal
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
    ht_log("WARN", "Near-zero variance detected")
    return(.Machine$double.eps)
  }
  v
}
