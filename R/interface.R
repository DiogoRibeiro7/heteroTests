# The calling convention of the tests, settled in 0.15.0: every test takes
# `(model, data, ...)`, and `data = NULL` stands for the model frame of
# `model`. performBoxMTest(data, group) is the documented exception: it tests
# the data, not the residuals of a model.

#' Check the `data` argument of a test computed from the model alone
#'
#' Tests whose statistic needs nothing beyond the residuals, fitted values and
#' model matrix of `model` accept `data` so that every test takes
#' `(model, data, ...)`. `NULL`, the default, stands for the model frame.
#'
#' Data that are given must still be the data the model was fitted to: they
#' must hold the variables of the model and the rows the fit used, which is
#' checked with the messages the tests that read `data` give.
#'
#' @param data The `data` argument of the calling test.
#' @param fn Character scalar naming the calling test.
#' @param model The fitted model, or `NULL` to check the type of `data` only.
#' @param test_label Character scalar naming the test in messages.
#' @return `data`, invisibly.
#' @keywords internal
#' @noRd
rcheck_data_argument <- function(data, fn, model = NULL, test_label = fn) {
  if (is.null(data)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(data)) {
    stop(sprintf("`data` in %s() must be a data frame or NULL.", fn), call. = FALSE)
  }
  if (!is.null(model)) {
    rcheck_data_matches_model(model, data, test_label)
  }
  invisible(data)
}

# Does `data` hold the variables of `model` and the rows its fit used? A model
# that is not an lm, or has no residuals, is left to the model checks that
# follow. The model frame itself, whose columns are the terms rather than the
# variables (`log(y)` rather than `y`), is accepted as it is.
rcheck_data_matches_model <- function(model, data, test_label) {
  if (!inherits(model, "lm")) {
    return(invisible(NULL))
  }
  residuals <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(residuals)) {
    return(invisible(NULL))
  }
  frame_columns <- tryCatch(names(stats::model.frame(model)), error = function(e) NULL)
  holds_frame <- !is.null(frame_columns) && all(frame_columns %in% names(data))
  if (!holds_frame) {
    required <- tryCatch(unique(all.vars(stats::terms(model))), error = function(e) NULL)
    rvalidateDataInputs(data, required_vars = required, min_obs = 0L)
  }
  resid_names <- names(residuals)
  rows_known <- length(resid_names) > 0L && !is.null(rownames(data))
  mismatch <- if (rows_known) {
    !all(resid_names %in% rownames(data))
  } else {
    nrow(data) != length(residuals)
  }
  if (mismatch) {
    rstop_data_rows(residuals, data, test_label)
  }
  invisible(NULL)
}

# The error for `data` whose rows are not those of the fit, in the words of
# prepare_model_data_for_test(): the rows it lacks when the rows are named,
# otherwise the counts.
rstop_data_rows <- function(residuals, data, test_label) {
  resid_names <- names(residuals)
  missing_rows <- if (length(resid_names) > 0L && !is.null(rownames(data))) {
    setdiff(resid_names, rownames(data))
  } else {
    character()
  }
  if (length(missing_rows) > 0L) {
    stop(
      sprintf(
        "%s requires `data` to contain the rows used to fit the model. Missing rows: %s",
        test_label,
        paste(utils::head(missing_rows, 3L), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  stop(
    sprintf(
      "%s requires `data` with %d observations to match the fitted model, got %d.",
      test_label, length(residuals), nrow(data)
    ),
    call. = FALSE
  )
}

#' Refuse a glm in a test that needs least-squares residuals
#'
#' Most tests accept a `glm` today; the ones that cannot use it refuse it with
#' this message, so that the refusal reads the same everywhere. Whether every
#' test should refuse a `glm` is a decision of its own (`ROADMAP.md`).
#'
#' @param model A fitted model.
#' @param fn Character scalar naming the calling test.
#' @param reason Optional character scalar saying why.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_glm <- function(model, fn, reason = NULL) {
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    return(invisible(NULL))
  }
  stop(
    sprintf("%s() needs a model fitted by stats::lm()", fn),
    if (is.null(reason)) "." else paste0(": ", reason),
    call. = FALSE
  )
}

#' Warn about a call that passes another argument where `data` now is
#'
#' Before 0.15.0 the second argument of some tests was not `data`:
#' `performArchLMTest(m, 3)` gave the number of lags. The caller recognises
#' such a call by the type of its second argument, which a data frame never
#' has, moves the value to the argument it was meant for, and calls this
#' function, which warns and shows the call to use instead. The positional
#' form works, with this warning, in 0.15.x and stops working in 1.0.0.
#'
#' @param fn Character scalar naming the calling test.
#' @param model_expr The unevaluated `model` argument, from `substitute()`.
#' @param moved Named list of the unevaluated arguments that were passed by
#'   position, in their old order, named by the argument each one was meant
#'   for.
#' @return `NULL`, invisibly; called for the warning.
#' @keywords internal
#' @noRd
rwarn_positional_call <- function(fn, model_expr, moved) {
  model_text <- rexpression_label(model_expr, "model")
  values <- vapply(
    names(moved),
    function(arg) rexpression_label(moved[[arg]], arg),
    character(1)
  )
  old <- sprintf("%s(%s)", fn, paste(c(model_text, values), collapse = ", "))
  new <- sprintf(
    "%s(%s)", fn,
    paste(c(model_text, paste(names(moved), "=", values)), collapse = ", ")
  )
  .Deprecated(
    old = fn,
    msg = sprintf(
      paste0(
        "%s is deprecated: since heteroTests 0.15.0 the second argument ",
        "of %s() is `data`, and this call stops working in 1.0.0.\n",
        "Use %s instead."
      ),
      old, fn, new
    )
  )
  invisible(NULL)
}

# Text for an unevaluated argument in a message: the expression as written
# when it is short, otherwise `fallback`. do.call() hands substitute() the
# evaluated object, whose deparsed form can be long.
rexpression_label <- function(expr, fallback) {
  if (is.name(expr) || is.call(expr) || (is.atomic(expr) && length(expr) == 1L)) {
    text <- paste(deparse(expr, width.cutoff = 60L), collapse = " ")
    if (nchar(text) <= 60L) {
      return(text)
    }
  }
  fallback
}
