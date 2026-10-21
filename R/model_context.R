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
# before. Since 0.16.0 the caches of results (R/cache.R) hash its parts, built
# without `data`, into the key of a fit (rcache_fit_key()); the context itself
# is dropped there too.
#
# docs/design/model-context.md sets out the contract, the migration of the
# tests in increments, and the decisions behind it. Since 0.16.0 (increment 1)
# performWhiteTest(), performBPTest() and performBreuschPaganTest(),
# performKoenkerTest(), performRankPermutationTest() and
# performHighDimensionalTest() are built on it, (increment 2) the
# studentized Breusch-Pagan statistic (.studentized_bp_test()),
# performRESETTest(), performGQTest() and performQuantileRegressionTest(), and
# (increment 3) performGlejserTest(), performParkTest(), performSzroeterTest()
# with the registered "szroeter" (.szroeter_test()),
# performHarrisonMcCabeTest(), performHartleyFmaxTest() and
# performSpatialHeteroTest(), and (increment 4) the five group tests,
# performLeveneTest(), performBrownForsytheTest(), performFlignerKilleenTest(),
# performBartlettTest() and performOBrienTest() (rcontext_group_rows()); the
# other tests still prepare the model and data with rdata_or_model_frame() and
# prepare_model_data_for_test().

#' Build the model context of a test
#'
#' Runs the refusals every test makes first: a model that is not an `lm` or a
#' least-squares `glm`, a `survey::svyglm()` fit
#' ([rrefuse_non_gaussian_glm()]), a multivariate `lm` ([rcontext_kind()]),
#' and a model frame that `model = FALSE` rebuilt from data that changed after
#' the fit ([rcheck_model_frame()], with or without `data`). It then takes the
#' parts of the fit, and, when `data` is given, matches its rows to the
#' observations ([rcontext_align()]). The messages are those the tests gave;
#' on a call with more than one defect, the first one reported may differ.
#' The refusals of the context itself have class
#' `heteroTests_model_context_error`, and those of `data` the class of the
#' guard, `heteroTests_data_rows_error`: both are final in the runners.
#'
#' A fit with `model = FALSE` whose model frame can no longer be rebuilt from
#' the data it refers to is tested on `data`, when `data` is given and gives
#' the response and the fitted values of the fit ([rcontext_frame_from_data()]).
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
#' @param reads Character vector of the columns of `data` the test may read
#'   itself, from [rcontext_rows()], such as the `order_by` of
#'   [performRankPermutationTest()]. Those that are not columns of the model
#'   frame make the alignment stricter for data taken in the order of the fit
#'   ([rcontext_align()]).
#' @return A list of class `ht_model_context`:
#'   \describe{
#'     \item{`model`}{the fit.}
#'     \item{`kind`}{`"lm"`, `"glm_gaussian"`, `"gam"`, `"fitwls"` or `"rlm"`.}
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
rmodel_context <- function(model, data = NULL, fn, test_label = fn, need = character(),
                           reads = character()) {
  # The function the user called, named by the refusals of `data`. It is
  # looked for only when one is made, so a call that is not refused does not
  # search the stack (rtest_name()).
  caller_frame <- sys.parent()
  delayedAssign("caller", rtest_name(caller_frame, default = fn))
  kind <- rcontext_kind(model, fn)
  frame <- tryCatch(stats::model.frame(model), error = function(e) e)
  if (inherits(frame, "error") || !is.data.frame(frame)) {
    # A fit with `model = FALSE` whose data are gone: its frame is built from
    # `data`, and used only if it gives the response and the fitted values of
    # the fit (rcontext_frame_from_data()).
    rebuilt <- if (is.data.frame(data)) rcontext_frame_from_data(model, data)
    if (is.null(rebuilt)) {
      rstop_model_context(sprintf(
        paste0(
          "%s() cannot use the model frame of `model`: it cannot be rebuilt (%s)%s. ",
          "A model fitted with `model = FALSE` rebuilds it from the data it was ",
          "fitted on. Refit the model, or keep its model frame with `model = TRUE`, ",
          "the default."
        ),
        caller,
        if (inherits(frame, "error")) conditionMessage(frame) else "not a data frame",
        if (is.data.frame(data)) {
          ", and `data` does not give the response and the fitted values of the fit"
        } else {
          ""
        }
      ))
    }
    frame <- rebuilt
    design <- stats::model.matrix(attr(frame, "terms"), frame, contrasts.arg = model$contrasts)
  } else {
    # A frame that model = FALSE rebuilt from data that changed after the fit,
    # with or without `data` (#130).
    rcheck_model_frame(model, frame, caller)
    design <- stats::model.matrix(model)
  }
  offset <- stats::model.offset(frame)
  ctx <- structure(
    list(
      model = model,
      kind = kind,
      frame = frame,
      obs_id = rcontext_ids(model, frame),
      na_action = model$na.action,
      response = stats::model.response(frame, "numeric"),
      design = design,
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
    # Does the test read columns of `data` beyond the model frame, which the
    # comparison of the alignment cannot check?
    beyond <- length(need) > 0L ||
      (is.data.frame(data) && any(reads %in% setdiff(names(data), names(frame))))
    aligned <- rcontext_align(ctx, data, caller, test_label, beyond)
    ctx$rows <- aligned$rows
    ctx$alignment <- aligned$alignment
  }
  ctx$variables <- rcontext_variables(ctx, data, need)
  ctx
}

# The refusals of the context are final, as decided on 2026-10-16 (decision 6
# of the design): runHeteroTests() and runDiagnostics() run no other test
# under the name of one the context refused (ht_analyse_failure()).
rstop_model_context <- function(message) {
  stop(structure(
    class = c("heteroTests_model_context_error", "error", "condition"),
    list(message = message, call = NULL)
  ))
}

#' Refuse an `mgcv::gam()` fit with smooth terms
#'
#' A Gaussian `mgcv::gam()` fit without smooth terms is the least-squares fit
#' of its design, the fit of the Gaussian `glm`. A smooth term is penalised
#' unless it is fixed (`s(x, fx = TRUE)`), and a penalised fit is not the
#' least-squares fit of its design: its coefficients do not solve the
#' least-squares equations, and a test that fits that design again, its
#' segments or its quantiles, tests an unpenalised regression the model is not.
#' Whether the tests take such a fit is a question of the contract left to the
#' last step of the migration, with `MASS::rlm()` (increment 11 of the design
#' of the model context); until then the tests of increment 2 refuse every fit
#' with smooth terms, fixed ones included, decided on 2026-10-18, and those of
#' increment 1 test it as before 0.16.0.
#'
#' @param ctx The model context.
#' @param fn Character scalar naming the calling test.
#' @return `NULL`, invisibly; called for the error, of class
#'   `heteroTests_model_context_error`.
#' @keywords internal
#' @noRd
rrefuse_smooth_gam <- function(ctx, fn) {
  smooths <- ctx$model$smooth
  if (!identical(ctx$kind, "gam") || length(smooths) == 0L) {
    return(invisible(NULL))
  }
  labels <- vapply(smooths, function(s) as.character(s$label %||% "a smooth"), character(1))
  rstop_model_context(sprintf(
    paste0(
      "%s() cannot test an mgcv::gam() fit with smooth terms (%s): a smooth term is ",
      "usually penalised, and a penalised fit is not the least-squares fit of its design, ",
      "which the test fits again. Whether the tests take such a fit is left to a later ",
      "step of the package. Refit the model with lm() or glm(), with a parametric term ",
      "such as splines::ns() in place of each smooth."
    ),
    fn, paste(labels, collapse = ", ")
  ))
}

# The names of the observations, in the order of the fit: those of its
# residuals, or, for a fit whose residuals carry none (mgcv::gam()), the row
# names of its model frame, or their numbers.
rcontext_ids <- function(model, frame) {
  n <- length(model$residuals)
  ids <- names(model$residuals)
  if (length(ids) == n && !anyNA(ids)) {
    return(ids)
  }
  ids <- rownames(frame)
  if (length(ids) == n) {
    return(ids)
  }
  as.character(seq_len(n))
}

#' The model frame of a fit with `model = FALSE`, from `data`
#'
#' A fit with `model = FALSE` keeps no model frame, and `model.frame()`
#' rebuilds it from the data the fit refers to; when those are gone, it is
#' rebuilt here from `data`, through the call of the fit and the `predvars` of
#' its terms, and kept only if, on the rows of the fit, it gives the response
#' of the fit and, times the coefficients plus its offset, the fitted linear
#' predictor, the checks of [rcheck_model_frame()], to `1e-8`.
#'
#' @param model The fitted model.
#' @param data The `data` argument of the calling test.
#' @return The model frame, on the rows of the fit in its order, or `NULL`.
#' @keywords internal
#' @noRd
rcontext_frame_from_data <- function(model, data) {
  if (!inherits(model, "lm") || !is.null(model[["model"]])) {
    return(NULL)
  }
  frame <- tryCatch(stats::model.frame(model, data = data), error = function(e) NULL)
  ids <- names(model$residuals)
  if (!is.data.frame(frame) || is.null(ids)) {
    return(NULL)
  }
  rows <- match(ids, rownames(frame))
  if (anyNA(rows)) {
    return(NULL)
  }
  if (!identical(rows, seq_len(nrow(frame)))) {
    model_terms <- attr(frame, "terms")
    frame <- frame[rows, , drop = FALSE]
    attr(frame, "terms") <- model_terms
  }
  if (!isTRUE(rframe_holds_response(model, frame)) || !isTRUE(rframe_holds_fit(model, frame))) {
    return(NULL)
  }
  frame
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
#' A Gaussian `mgcv::gam()` fit, a `glm` by class, is accepted as before
#' 0.16.0; whether a penalised fit with smooth terms is an input the tests
#' justify is left to the last step of the migration, with `MASS::rlm()`.
#'
#' @param model The fitted model.
#' @param fn Character scalar naming the calling test.
#' @return `"gam"` (an `mgcv::gam()` fit), `"glm_gaussian"`, `"fitwls"` (a
#'   [fitWLS()] fit), `"rlm"` (a `MASS::rlm()` fit) or `"lm"`.
#' @keywords internal
#' @noRd
rcontext_kind <- function(model, fn) {
  rrefuse_non_gaussian_glm(model, fn)
  if (inherits(model, "mlm") || is.matrix(model$residuals)) {
    responses <- NCOL(model$residuals)
    rstop_model_context(
      sprintf("%s() needs a model with one response; `model` has %d.", fn, responses)
    )
  }
  if (inherits(model, "gam")) {
    return("gam")
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
#' Rows taken in the order of the fit are confirmed only up to rows that tie
#' on every column the comparison checked: two such rows could be exchanged
#' without a trace in the model frame. That changes nothing a test computes
#' from the model, but it changes the columns of `data` beyond the model frame,
#' such as the variable a test orders by. So when the test reads such columns
#' (`beyond`), data taken in the order of the fit are refused if any two rows
#' of the fit tie on every compared column.
#'
#' @param ctx The model context, before its rows are set.
#' @param data The `data` argument of the calling test, not `NULL`.
#' @param fn Character scalar naming the function the user called.
#' @param test_label Character scalar naming the test in the messages on
#'   rows that `data` lacks.
#' @param beyond Logical: does the test read columns of `data` beyond the
#'   model frame?
#' @return A list with `rows`, the row of `data` holding each observation, and
#'   `alignment`.
#' @keywords internal
#' @noRd
rcontext_align <- function(ctx, data, fn, test_label, beyond = FALSE) {
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
  checked <- rcheck_rows_at(ctx$model, frame, data, rows, fn, numbered)
  if (identical(alignment, "in_order") && isTRUE(beyond)) {
    compared <- attr(checked, "compared")
    if (rrows_repeat(frame, compared)) {
      rstop_data_rows_unmatched(fn, "in_order_ties", compared = compared)
    }
  }
  list(rows = rows, alignment = alignment)
}

# Do two rows of the model frame tie on every column in `compared`? Factors
# by their codes, numbers exactly.
rrows_repeat <- function(frame, compared) {
  if (nrow(frame) < 2L) {
    return(FALSE)
  }
  keys <- list()
  for (column in compared) {
    v <- frame[[column]]
    parts <- if (is.matrix(v)) lapply(seq_len(ncol(v)), function(j) v[, j]) else list(v)
    for (p in parts) {
      keys[[length(keys) + 1L]] <- if (is.factor(p)) as.integer(p) else p
    }
  }
  if (length(keys) == 0L) {
    return(TRUE)
  }
  names(keys) <- paste0("k", seq_along(keys))
  anyDuplicated(as.data.frame(keys, stringsAsFactors = FALSE)) > 0L
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
      rstop_model_context(sprintf(
        paste(
          "The following variables are missing from the model frame of `model`: %s.",
          "It holds the terms of the model (%s), not the variables they are computed",
          "from; pass the data the model was fitted to as `data`."
        ),
        paste(need[!held], collapse = ", "),
        paste(terms_held, collapse = ", ")
      ))
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
#' The orthogonality allows, besides `1e-6` of the norms of the column and of
#' the residuals, a rounding term that grows with the size of the response,
#' \eqn{\epsilon\, n\, \|X_j\|_W \|y\|_W}: a Gaussian `glm` computes its
#' residuals as \eqn{y - \hat\mu}, which shifts them all by about
#' \eqn{\epsilon |\bar y|}, so a response near `1e11` with a residual standard
#' deviation of 2 leaves scores that the first term alone refuses. In a sweep of
#' `lm` and Gaussian `glm` fits with 120 to 10,000 observations and responses
#' up to `1e14`, the largest score of a valid fit was 0.04 of that term. A
#' `mgcv::gam()` fit is penalised, so its residuals are not orthogonal to its
#' design, and only its first check is made. An edited coefficient leaves the
#' residuals unchanged, so only the first check sees it, at `1e-7` of the
#' largest term: an edit of 0.5 goes undetected once the response is above
#' about `1e7`. The checks guard against accidental edits; they are not a proof
#' that a fit is unedited.
#'
#' The tests call it after [rvalidateModelInputs()], which refuses residuals
#' and coefficients that are not finite with its own messages. Its refusals
#' are final in the runners, as every refusal of the context is.
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
    rstop_model_context("Internal error: the parts of the model context differ in length.")
  }
  if (anyDuplicated(ctx$rows) > 0L) {
    rstop_model_context(
      "Internal error: two observations of the fit were matched to one row of `data`."
    )
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
    rstop_model_context(paste0(
      "The fitted model does not reproduce its residuals from its design and coefficients: ",
      "it was changed after the fit. Refit the model."
    ))
  }
  if (identical(ctx$kind, "gam")) {
    return(invisible(ctx))
  }
  w <- ctx$weights %||% rep(1, n)
  # Norms scaled by the largest value, so that a residual near 1e-200 or a
  # design value near 1e200 does not underflow or overflow.
  norm_w <- function(v) {
    top <- max(abs(v))
    if (!is.finite(top) || top == 0) top else top * sqrt(sum(w * (v / top)^2))
  }
  scores <- abs(drop(crossprod(X, w * e)))
  columns <- vapply(seq_len(ncol(X)), function(j) norm_w(X[, j]), numeric(1))
  bound <- columns * (1e-6 * norm_w(e) + .Machine$double.eps * n * norm_w(y))
  if (length(scores) > 0L && !isTRUE(all(scores <= bound))) {
    rstop_model_context(paste0(
      "The weights of the model are not those its coefficients were fitted with: ",
      "it was changed after the fit. Refit the model."
    ))
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

#' The fitted mean, computed from the coefficients
#'
#' The fitted values of the fit, the linear predictor with the total offset,
#' one value per observation, in the order of the fit and without the padding
#' of `na.exclude`: for an `lm`, a `fitWLS()` fit, a `MASS::rlm()` fit and an
#' `mgcv::gam()` fit, the design times the coefficients that were not aliased,
#' in the order of the pivot of the fit, plus the offset, as
#' `stats::predict.lm()` computes it; for a Gaussian `glm`, its fitted values.
#' They are what `fitted()` holds, and so what `predict(type = "response")`
#' returns without new data, except on an `mgcv::gam()` fit with an offset
#' given as the `offset` argument, which `predict.gam()` leaves out (its
#' documentation says so) and `fitted()` includes. Unlike the fitted values an
#' `lm` stores, \eqn{y - e}, which equal them up to rounding, observations with
#' the same design row get the same value, so that their rounding errors,
#' which depend on the residuals, do not order them.
#'
#' @param ctx The model context.
#' @return Numeric vector, one value per observation.
#' @keywords internal
#' @noRd
rcontext_mean <- function(ctx) {
  if (identical(ctx$kind, "glm_gaussian")) {
    return(unname(ctx$fitted))
  }
  coefs <- stats::coef(ctx$model)
  pivot <- ctx$model$qr$pivot
  used <- if (!is.null(pivot) && length(ctx$rank) == 1L) {
    pivot[seq_len(ctx$rank)]
  } else {
    which(!is.na(coefs))
  }
  mean <- drop(ctx$design[, used, drop = FALSE] %*% coefs[used])
  if (!is.null(ctx$offset)) {
    mean <- mean + ctx$offset
  }
  unname(mean)
}

#' Refuse an extra variable that is missing at an observation of the fit
#'
#' A test that reads a variable of `data` besides the model, such as the
#' variable it orders by, groups by or regresses on, needs its value at every
#' observation of the fit. Before 0.16.0 the tests dropped the rows of `data`
#' with a missing value and then lacked those rows; they still refuse them,
#' with the message they gave, which now also names the variable (decided on
#' 2026-10-18).
#'
#' @param ctx The model context.
#' @param values The variable, one value per observation
#'   ([rcontext_variables()]).
#' @param variable Character scalar, the name of the variable.
#' @param argument Character scalar, the argument of the test that names it.
#' @param test_label Character scalar naming the test.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rstop_missing_variable <- function(ctx, values, variable, argument, test_label) {
  absent <- if (is.matrix(values) || is.data.frame(values)) {
    !stats::complete.cases(values)
  } else {
    is.na(values)
  }
  if (!any(absent)) {
    return(invisible(NULL))
  }
  stop(
    sprintf(
      paste0(
        "%s requires `data` to contain the rows used to fit the model. Missing rows: %s, ",
        "where %s, the `%s` variable, is missing."
      ),
      test_label, paste(utils::head(ctx$obs_id[absent], 3L), collapse = ", "), variable, argument
    ),
    call. = FALSE
  )
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

#' The residuals and the rows of a group test, without the observations
#' whose group is missing
#'
#' The five group tests, [performLeveneTest()], [performBrownForsytheTest()],
#' [performFlignerKilleenTest()], [performBartlettTest()] and
#' [performOBrienTest()], compare the Pearson residuals of the fit across the
#' groups of a variable of `data`. Before 0.16.0 each matched the rows of
#' `data` to the residuals itself, by row name, or by position when the
#' residuals carry no names, as those of an `mgcv::gam()` fit (N17), and then
#' left out the observations whose group, or a variable of the model, is
#' missing in `data`, with the warning "Removed N observations due to missing
#' values in ...". The rows are now those the alignment confirmed, in the
#' order of the fit, or the model frame without `data`; the observations left
#' out, and the warning, are those of before, computed on the same rows by
#' [rhandleMissingValues()].
#'
#' Without `data` the group is read from the model frame, which holds it only
#' when it is a variable of the model under its own name (M1 otherwise,
#' [rcontext_variables()]); `data` that lack it are refused with the message of
#' `data`. A fit with a zero weight is refused next, by [rcontext_pearson()],
#' as before.
#'
#' @param ctx The model context.
#' @param data The `data` argument of the calling test, or `NULL`.
#' @param group Character scalar, the name of the grouping variable.
#' @return A list with `residuals`, the Pearson residuals of the observations
#'   kept, and `data`, the rows that hold them (of `data`, or of the model
#'   frame without `data`), in the order of the fit.
#' @keywords internal
#' @noRd
rcontext_group_rows <- function(ctx, data, group) {
  rcontext_variables(ctx, data, c(group = group))
  residuals <- rcontext_pearson(ctx)
  rows <- if (is.null(data)) ctx$frame else data[ctx$rows, , drop = FALSE]
  # The variables of the model that the rows hold, and the group: all of the
  # variables of the model when `data` holds them, which the tests required
  # before 0.16.0, so the same rows are left out, with the same warning.
  variables <- intersect(unique(c(all.vars(stats::terms(ctx$model)), group)), names(rows))
  cleaned <- rhandleMissingValues(rows, variables = variables)
  if (cleaned$removed_count > 0) {
    residuals <- residuals[-cleaned$removed_cases]
  }
  list(residuals = residuals, data = cleaned$data)
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
#' @param also Character vector of the other variables the test reads from
#'   `data`, such as the `order_by` of [performGQTest()], which the tests
#'   checked for missing values with those of the model.
#' @return `NULL`, invisibly; called for the warning.
#' @keywords internal
#' @noRd
rwarn_incomplete_rows <- function(data, model, also = character()) {
  if (!is.data.frame(data)) {
    return(invisible(NULL))
  }
  variables <- intersect(unique(c(all.vars(stats::terms(model)), also)), names(data))
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
