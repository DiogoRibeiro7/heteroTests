# The model context: one source, per call, for what a test reads from a fitted
# model and from its data.
#
# A test that is migrated to the context builds it first, with
# rmodel_context(), and reads from it the response, the design, the residuals
# and fitted values, the weights, the offset, the rank and the aliased
# columns, all taken from the stored fit; it never evaluates the call or the
# formula again, so `subset =`, `weights =`, `offset =`, `na.action` and
# variables of the workspace cannot be applied twice or found elsewhere, and a
# term computed from all the rows the fit saw, such as `I(x > median(x))`, keeps
# the values the fit used. `data`, when it is given, is matched to the
# observations of the fit by the guard of 0.15.0 (rdata_fit_rows() and
# rcheck_rows_at(), R/interface.R), so data it refuses are refused with the same
# message and class, and the context then reads from `data` only the rows the
# guard confirmed. Nothing is dropped or renumbered before the match.
#
# The context is an internal list of class `ht_model_context`. It is built for
# one call and dropped when the call returns: it is never stored, returned,
# exported or cached, and it holds no global or mutable state. It is not the
# validation cache of R/validation.R, whose checks the tests keep calling as
# before, with the same keys.
#
# docs/design/model-context.md sets out the contract, the migration of the
# tests in increments, and the decisions behind it. Since 0.16.0 (increment 1)
# performWhiteTest(), performBPTest() and performBreuschPaganTest(),
# performKoenkerTest(), performRankPermutationTest() and
# performHighDimensionalTest() are built on it; the other tests still prepare
# the model and data with rdata_or_model_frame() and
# prepare_model_data_for_test().

#' Build the model context of a test
#'
#' Runs the refusals every test makes first, in their order: a model that is
#' not an `lm` or a least-squares `glm`, a `survey::svyglm()` fit
#' ([rrefuse_non_gaussian_glm()]), a multivariate `lm` ([rcontext_kind()]),
#' and a model frame that `model = FALSE` rebuilt from data that changed after
#' the fit ([rcheck_model_frame()], with or without `data`). It then takes the
#' parts of the fit, and, when `data` is given, matches its rows to the
#' observations ([rcontext_align()]).
#'
#' @param model The fitted model of the calling test.
#' @param data The `data` argument of the calling test, or `NULL`.
#' @param fn Character scalar naming the calling test, for the refusals of the
#'   model. The refusals of `data` name the function the user called
#'   ([rtest_name()]), which is the test itself, a deprecated name such as
#'   `performBreuschPaganTest()`, or a wrapper such as
#'   `performWhiteTestRobust()`.
#' @param test_label Character scalar naming the test in the messages on rows
#'   that `data` lacks ([rstop_data_rows()]).
#' @param need Character vector of the variables the test reads besides the
#'   model, such as a grouping or ordering variable, named by the role they
#'   play; see [rcontext_variables()].
#' @return A list of class `ht_model_context`:
#'   \describe{
#'     \item{`model`}{the fit.}
#'     \item{`kind`}{`"lm"`, `"glm_gaussian"`, `"fitwls"` or `"rlm"`.}
#'     \item{`frame`}{the model frame, `stats::model.frame(model)`.}
#'     \item{`obs_id`}{the names of the observations, in the order of the fit.}
#'     \item{`na_action`}{`model$na.action`.}
#'     \item{`response`}{the response, on the scale of the fit (`log(y)` for
#'       `log(y) ~ x`).}
#'     \item{`design`}{the design of the fit, every column,
#'       `stats::model.matrix(model)`; [rcontext_design()] leaves out the
#'       aliased ones.}
#'     \item{`residuals`, `fitted`}{`model$residuals` and
#'       `model$fitted.values`, one per fitted observation, without the
#'       padding of `na.exclude`.}
#'     \item{`weights`}{the prior weights, [rleast_squares_weights()], or
#'       `NULL`.}
#'     \item{`offset`}{the total offset, the `offset` argument plus the
#'       `offset()` terms, or `NULL`.}
#'     \item{`rank`, `aliased`, `qr_tol`}{the rank of the fit, its aliased
#'       coefficients ([raliased_coefficients()]) and the tolerance its rank
#'       was decided with (`1e-7` for an `lm`, `1e-11` for a `glm`).}
#'     \item{`variance_function`}{the variance function of a [fitWLS()] fit,
#'       or `NULL`.}
#'     \item{`rows`}{for each observation, the row of `data` that holds it, or
#'       `NULL` without `data`.}
#'     \item{`alignment`}{how they were found: `"model_frame"` (no `data`),
#'       `"row_names"`, `"row_numbers"` or `"in_order"`.}
#'     \item{`variables`}{the variables named in `need`, one value per
#'       observation, in the order of the fit.}
#'   }
#' @keywords internal
#' @noRd
rmodel_context <- function(model, data = NULL, fn, test_label = fn, need = character()) {
  # The function the user called, named by the refusals of `data`. It is
  # looked for only when one is made, so a call that is not refused does not
  # search the stack (rtest_name()).
  caller_frame <- sys.parent()
  delayedAssign("caller", rtest_name(caller_frame, default = fn))
  kind <- rcontext_kind(model, fn)
  frame <- tryCatch(stats::model.frame(model), error = function(e) e)
  if (inherits(frame, "error") || !is.data.frame(frame)) {
    stop(
      sprintf(
        paste0(
          "%s() cannot use the model frame of `model`: it cannot be rebuilt (%s). ",
          "A model fitted with `model = FALSE` rebuilds it from the data it was ",
          "fitted on. Refit the model, or keep its model frame with `model = TRUE`, ",
          "the default."
        ),
        caller,
        if (inherits(frame, "error")) conditionMessage(frame) else "not a data frame"
      ),
      call. = FALSE
    )
  }
  # A frame that model = FALSE rebuilt from data that changed after the fit,
  # with or without `data` (#130).
  rcheck_model_frame(model, frame, caller)
  offset <- stats::model.offset(frame)
  ctx <- structure(
    list(
      model = model,
      kind = kind,
      frame = frame,
      obs_id = names(model$residuals),
      na_action = model$na.action,
      response = stats::model.response(frame, "numeric"),
      design = stats::model.matrix(model),
      residuals = model$residuals,
      fitted = model$fitted.values,
      weights = rleast_squares_weights(model),
      offset = if (!is.null(offset)) as.vector(offset),
      rank = model$rank,
      aliased = raliased_coefficients(model),
      qr_tol = model$qr$tol %||% 1e-7,
      variance_function = attr(model, "variance_function"),
      rows = NULL,
      alignment = "model_frame",
      variables = list()
    ),
    class = "ht_model_context"
  )
  if (!is.null(data)) {
    aligned <- rcontext_align(ctx, data, caller, test_label)
    ctx$rows <- aligned$rows
    ctx$alignment <- aligned$alignment
  }
  ctx$variables <- rcontext_variables(ctx, data, need)
  ctx
}

#' The kind of fit, after the refusals of the model
#'
#' Calls [rrefuse_non_gaussian_glm()] first, which refuses a
#' `survey::svyglm()` fit, a model that is neither an `lm` nor a `glm`, and a
#' `glm` that is not a least-squares fit, each with the message every test
#' gives since 0.15.0. A multivariate `lm`, `lm(cbind(y1, y2) ~ x)`, which
#' inherits from `lm`, is refused next: the tests take one response, and before
#' 0.16.0 they stopped on it with a coercion error.
#'
#' @param model The fitted model.
#' @param fn Character scalar naming the calling test.
#' @return `"glm_gaussian"`, `"fitwls"` (a [fitWLS()] fit), `"rlm"` (a
#'   `MASS::rlm()` fit) or `"lm"`.
#' @keywords internal
#' @noRd
rcontext_kind <- function(model, fn) {
  rrefuse_non_gaussian_glm(model, fn)
  if (inherits(model, "mlm") || is.matrix(model$residuals)) {
    responses <- NCOL(model$residuals)
    stop(
      sprintf("%s() needs a model with one response; `model` has %d.", fn, responses),
      call. = FALSE
    )
  }
  if (inherits(model, "glm")) {
    return("glm_gaussian")
  }
  if (!is.null(attr(model, "variance_function"))) {
    return("fitwls")
  }
  if (inherits(model, "rlm")) {
    return("rlm")
  }
  "lm"
}

#' Match the rows of `data` to the observations of the fit
#'
#' The alignment step is the guard of 0.15.0. `data` must be a data frame
#' holding the variables of the model, or the columns of its model frame
#' ([rvalidateDataInputs()], with today's messages). Its rows are then picked
#' by row name, and for data that number their rows (a tibble, a data.table, a
#' data frame whose row names were reset) by the original row numbers of the
#' fit ([rdata_fit_rows()]). Data that number their rows and have exactly one
#' row per observation, whose numbers are not those of the fit, as the
#' complete rows of a fit with `na.omit` renumbered, or the tibble in fit order
#' of a fit to data with character row names, are taken in the order of the
#' fit (decided on 2026-10-18). The rows picked either way must reproduce the
#' model frame ([rcheck_rows_at()]); data that lack a row of the fit are
#' refused with the message of [rstop_data_rows()]. Nothing is matched by
#' position without that comparison, and nothing is dropped before it.
#'
#' @param ctx The model context, before its rows are set.
#' @param data The `data` argument of the calling test, not `NULL`.
#' @param fn Character scalar naming the function the user called.
#' @param test_label Character scalar naming the test in the messages on
#'   rows that `data` lacks.
#' @return A list with `rows`, the row of `data` holding each observation, and
#'   `alignment`.
#' @keywords internal
#' @noRd
rcontext_align <- function(ctx, data, fn, test_label) {
  frame <- ctx$frame
  # The model frame itself, given as `data`, holds the terms (`log(x)`) rather
  # than the variables (`x`), and is accepted as it is.
  holds_frame <- is.data.frame(data) && all(names(frame) %in% names(data))
  required <- if (holds_frame) NULL else unique(all.vars(stats::terms(ctx$model)))
  rvalidateDataInputs(data, required_vars = required, min_obs = 0L)
  numbered <- .row_names_info(data) < 0L
  rows <- rdata_fit_rows(frame, data, fn)
  alignment <- if (numbered) "row_numbers" else "row_names"
  if (is.null(rows) && numbered && nrow(data) == length(ctx$residuals)) {
    rows <- seq_len(nrow(data))
    alignment <- "in_order"
  }
  if (is.null(rows)) {
    rstop_data_rows(ctx$residuals, data, test_label)
  }
  rcheck_rows_at(ctx$model, frame, data, rows, fn, numbered)
  list(rows = rows, alignment = alignment)
}

#' The variables a test reads besides the model
#'
#' Without `data`, a variable is read from the model frame when the frame
#' holds it under its own name and it is a variable of the formula; the frame
#' holds the terms of the model (`log(x)`), not the variables they are computed
#' from, so any other variable is refused with a message that says so. With
#' `data`, a variable is read from the rows the alignment confirmed, in the
#' order of the fit.
#'
#' @param ctx The model context.
#' @param data The `data` argument of the calling test, or `NULL`.
#' @param need Character vector of variable names. Its names, when given, are
#'   the names of the elements of the result.
#' @return A named list, one vector (or matrix) per variable, with one value
#'   (row) per observation.
#' @keywords internal
#' @noRd
rcontext_variables <- function(ctx, data, need) {
  if (length(need) == 0L) {
    return(list())
  }
  roles <- names(need)
  if (is.null(roles)) {
    roles <- need
  }
  roles[!nzchar(roles)] <- need[!nzchar(roles)]
  if (is.null(data)) {
    held <- need %in% names(ctx$frame) & need %in% all.vars(stats::formula(ctx$model))
    if (!all(held)) {
      terms_held <- setdiff(names(ctx$frame), c("(weights)", "(offset)"))
      stop(
        sprintf(
          paste(
            "The following variables are missing from the model frame of `model`: %s.",
            "It holds the terms of the model (%s), not the variables they are computed",
            "from; pass the data the model was fitted to as `data`."
          ),
          paste(need[!held], collapse = ", "),
          paste(terms_held, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(stats::setNames(lapply(need, function(v) ctx$frame[[v]]), roles))
  }
  absent <- setdiff(need, names(data))
  if (length(absent) > 0L) {
    rvalidateDataInputs(data, required_vars = absent, min_obs = 0L)
  }
  rows <- ctx$rows
  stats::setNames(
    lapply(need, function(v) {
      x <- data[[v]]
      if (is.matrix(x)) x[rows, , drop = FALSE] else x[rows]
    }),
    roles
  )
}

#' Check that the fit reproduces itself
#'
#' The parts of the context must have one value per fitted observation, the
#' rows of `data` must be distinct, and, but for a `MASS::rlm()` fit, whose
#' residuals are those of an M-estimate, the fit must be the least-squares fit
#' it says it is: its response must be its design times its coefficients, plus
#' its offset, plus its residuals, and its residuals must be orthogonal to the
#' columns of its design in the metric of its weights. A fit whose residuals,
#' coefficients or weights were changed after the fit is refused. The two are
#' checked relative to the size of their terms, so that a fit that is merely
#' ill-conditioned, or of a response near `1e15`, passes: least squares by a
#' QR decomposition makes errors of the order of the machine precision times
#' those sizes, far below the tolerances.
#'
#' The tests call it after [rvalidateModelInputs()], which refuses residuals
#' and coefficients that are not finite with its own messages.
#'
#' @param ctx The model context.
#' @param tol Relative tolerance of the first check; the second uses `1e-6`.
#' @return `ctx`, invisibly.
#' @keywords internal
#' @noRd
rvalidate_model_context <- function(ctx, tol = 1e-7) {
  n <- length(ctx$residuals)
  lengths_of <- c(
    length(ctx$obs_id), length(ctx$response), length(ctx$fitted), NROW(ctx$design),
    length(ctx$weights %||% ctx$residuals), length(ctx$offset %||% ctx$residuals),
    vapply(ctx$variables, NROW, integer(1)), length(ctx$rows %||% ctx$residuals)
  )
  if (any(lengths_of != n)) {
    stop("Internal error: the parts of the model context differ in length.", call. = FALSE)
  }
  if (anyDuplicated(ctx$rows) > 0L) {
    stop("Internal error: two observations of the fit were matched to one row of `data`.",
         call. = FALSE)
  }
  if (identical(ctx$kind, "rlm") || n == 0L) {
    return(invisible(ctx))
  }
  coefs <- stats::coef(ctx$model)
  keep <- !is.na(coefs)
  X <- ctx$design[, keep, drop = FALSE]
  b <- unname(coefs[keep])
  offset <- ctx$offset %||% 0
  e <- unname(ctx$residuals)
  y <- unname(ctx$response)
  linear <- drop(X %*% b) + offset
  size <- max(abs(y), drop(abs(X) %*% abs(b)) + abs(offset), abs(e))
  error <- max(abs(y - linear - e))
  if (!isTRUE(error <= tol * size)) {
    stop(
      "The fitted model does not reproduce its residuals from its design and coefficients: ",
      "it was changed after the fit. Refit the model.",
      call. = FALSE
    )
  }
  w <- ctx$weights %||% rep(1, n)
  # Norms scaled by the largest value, so that a residual near 1e-200 or a
  # design value near 1e200 does not underflow or overflow.
  norm_w <- function(v) {
    top <- max(abs(v))
    if (!is.finite(top) || top == 0) top else top * sqrt(sum(w * (v / top)^2))
  }
  scores <- abs(drop(crossprod(X, w * e)))
  bound <- 1e-6 * vapply(seq_len(ncol(X)), function(j) norm_w(X[, j]), numeric(1)) * norm_w(e)
  if (length(scores) > 0L && !isTRUE(all(scores <= bound))) {
    stop(
      "The weights of the model are not those its coefficients were fitted with: ",
      "it was changed after the fit. Refit the model.",
      call. = FALSE
    )
  }
  invisible(ctx)
}

#' A vector of the fit without the padding of `na.exclude`
#'
#' `residuals()` and `fitted()` of a fit with `na.action = na.exclude` hold
#' an `NA` for each row the fit dropped; the context has one value per fitted
#' observation.
#'
#' @param ctx The model context.
#' @param x A vector, padded or not.
#' @return `x` without the padded elements.
#' @keywords internal
#' @noRd
rcontext_unpad <- function(ctx, x) {
  if (inherits(ctx$na_action, "exclude") && length(x) > length(ctx$residuals)) {
    return(x[-ctx$na_action])
  }
  x
}

#' The residuals the tests read
#'
#' The Pearson residuals of [rpearson_residuals()], without the padding of
#' `na.exclude`: on an unweighted fit the residuals, on a weighted one
#' \eqn{\sqrt{w_i}\, e_i}. A fit with a zero weight is refused there, as
#' before.
#'
#' @param ctx The model context.
#' @return Numeric vector, one value per observation, named by observation.
#' @keywords internal
#' @noRd
rcontext_pearson <- function(ctx) {
  rcontext_unpad(ctx, rpearson_residuals(ctx$model))
}

#' The design of the fit without its aliased columns
#'
#' @param ctx The model context.
#' @return The design matrix of the fit, without the columns of a
#'   rank-deficient fit that it aliased ([rnon_aliased_columns()]).
#' @keywords internal
#' @noRd
rcontext_design <- function(ctx) {
  rnon_aliased_columns(ctx$design, ctx$model)
}

#' The rows of `data` that hold the observations, in the order of the fit
#'
#' What a test hands to its checks of the data, and to a test it calls in turn:
#' the rows of `data` the alignment confirmed, one per observation, in the
#' order of the fit and under the names of its observations
#' ([rdata_with_fit_rows()]), or the model frame without `data`.
#'
#' @param ctx The model context.
#' @param data The `data` argument of the calling test, or `NULL`.
#' @return A data frame with one row per observation.
#' @keywords internal
#' @noRd
rcontext_rows <- function(ctx, data) {
  if (is.null(data)) {
    return(ctx$frame)
  }
  rdata_with_fit_rows(data[ctx$rows, , drop = FALSE], ctx$obs_id)
}

#' Warn about the incomplete rows of `data`, dropping nothing
#'
#' The tests dropped the rows of `data` with a missing value in a variable of
#' the model before matching the rest, with the warning "Removed N
#' observations due to missing values in ...", which tests and users rely on.
#' The context matches the rows first and drops nothing, since the fit already
#' left those rows out; this gives the same warning on the same data.
#'
#' @param data The `data` argument of the calling test, or `NULL`.
#' @param model The fitted model.
#' @return `NULL`, invisibly; called for the warning.
#' @keywords internal
#' @noRd
rwarn_incomplete_rows <- function(data, model) {
  if (!is.data.frame(data)) {
    return(invisible(NULL))
  }
  variables <- intersect(unique(all.vars(stats::terms(model))), names(data))
  if (length(variables) == 0L) {
    return(invisible(NULL))
  }
  n_removed <- sum(!stats::complete.cases(data[, variables, drop = FALSE]))
  if (n_removed == 0L) {
    return(invisible(NULL))
  }
  with_missing <- variables[vapply(variables, function(v) any(is.na(data[[v]])), logical(1))]
  var_names <- paste(if (length(with_missing) > 0L) with_missing else variables, collapse = ", ")
  .std_warning("missing_values_removed", var_names = var_names, n_removed = n_removed)
  invisible(NULL)
}
