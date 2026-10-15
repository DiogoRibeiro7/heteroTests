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
#' @param data The `data` argument of the calling test.
#' @param fn Character scalar naming the calling test.
#' @return `data`, invisibly.
#' @keywords internal
#' @noRd
rcheck_data_argument <- function(data, fn) {
  if (!is.null(data) && !is.data.frame(data)) {
    stop(sprintf("`data` in %s() must be a data frame or NULL.", fn), call. = FALSE)
  }
  invisible(data)
}

#' Warn about a call that passes another argument where `data` now is
#'
#' Before 0.15.0 the second argument of some tests was not `data`:
#' `performArchLMTest(m, 3)` gave the number of lags. The caller recognises
#' such a call by the type of its second argument, which a data frame never
#' has, moves the value to the argument it was meant for, and calls this
#' function, which warns and shows the call to use instead. The positional
#' form works, with this warning, for one release.
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
        "of %s() is `data`.\nUse %s instead."
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
