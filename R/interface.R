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

#' The data a test reads when `data` is omitted
#'
#' `data = NULL` stands for the model frame of `model`, as it does in
#' runHeteroTests() and HeteroDiagnostic(), so that a test called as
#' `f(model)` reads what `f(model, stats::model.frame(model))` reads. The model
#' frame holds the variables of the model under the names of its terms
#' (`log(x)` rather than `x`), so a test that needs another column, or the
#' variables of a model that transforms them, still needs `data`. A model whose
#' frame cannot be rebuilt leaves `data` as `NULL`, and the test refuses it as
#' it did before 0.15.0.
#'
#' `data` that is given is checked by [rcheck_data_rows()] first, so every
#' test that starts here refuses data whose rows cannot be confirmed to hold
#' the observations of the fit.
#'
#' @param model The fitted model of the calling test.
#' @param data The `data` argument of the calling test.
#' @return `data`, or the model frame of `model` when `data` is `NULL`.
#' @keywords internal
#' @noRd
rdata_or_model_frame <- function(model, data) {
  if (!is.null(data)) {
    # sys.call(-1L) and sys.function(-1L) are the call and the function of the
    # test that called this one, also when the promise is forced later, in
    # the error.
    rcheck_data_rows(model, data, fn = rfunction_name(sys.call(-1L), sys.function(-1L)))
    return(data)
  }
  tryCatch(stats::model.frame(model), error = function(e) NULL)
}

#' Refuse `data` whose rows cannot be confirmed to hold the fit's observations
#'
#' The tests match the rows of `data` to the residuals by row name. A tibble,
#' a data.table, or a data frame whose row names were reset (as by
#' `dplyr::arrange()`) has row names that only number its rows, so for such
#' data the match is by position, and data in another order than the fit, or
#' with rows in front of those of the fit, were matched to other observations
#' and gave wrong values without an error. This guard refuses them, until the
#' model context planned for 0.16.0 replaces it.
#'
#' It takes the rows of `data` that the row names pick for the observations of
#' the fit, the rows the tests read, and compares them, row by row, with the
#' model frame of the fit: the response and every variable the frame holds as
#' itself (`y`, `x1`, `group`; not `log(x1)` or `poly(x1, 2)`). A response the
#' frame holds transformed, such as `log(y)`, is evaluated on all the rows of
#' `data`, through the `predvars` of the terms, as `model.frame()` evaluated it
#' before dropping rows, and the matched rows are then taken. A value that
#' cannot be rebuilt from `data` alone is not compared, and nothing is looked
#' up outside `data`.
#'
#' Data that lack a row of the fit, or a variable of the model, are left to
#' the calling test, which refuses them with its own message. Repeated row
#' names, which say nothing about which row holds which observation, are
#' refused. The error has class `heteroTests_data_rows_error`, which
#' [ht_analyse_failure()] treats as final: `runHeteroTests()` does not fall
#' back on another test for it.
#'
#' @param model The fitted model of the calling test. A model that is not an
#'   `lm`, or whose model frame cannot be rebuilt, is left to the checks of the
#'   test.
#' @param data The `data` argument of the calling test, not `NULL`.
#' @param fn Character scalar naming the calling test in the message, or
#'   `NULL`.
#' @return The rows of `data` that hold the observations of the fit,
#'   invisibly, or `NULL` when they were not checked.
#' @keywords internal
#' @noRd
rcheck_data_rows <- function(model, data, fn = NULL) {
  if (!inherits(model, "lm") || inherits(model, "svyglm") || !is.data.frame(data)) {
    return(invisible(NULL))
  }
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.data.frame(frame) || nrow(frame) == 0L) {
    return(invisible(NULL))
  }
  numbered <- .row_names_info(data) < 0L
  # The row names as stored, integer or character: rownames() would turn
  # integers into strings on every call, and two integer row names are the
  # same name exactly when they are the same integer.
  data_rows <- attr(data, "row.names")
  if (!numbered && anyDuplicated(data_rows) > 0L) {
    rstop_data_rows_unmatched(fn, "duplicated")
  }
  ids <- attr(frame, "row.names")
  rows <- if (identical(ids, data_rows)) {
    seq_along(ids)
  } else if (is.integer(ids) && is.integer(data_rows)) {
    match(ids, data_rows)
  } else {
    match(as.character(ids), as.character(data_rows))
  }
  if (anyNA(rows)) {
    return(invisible(NULL))
  }
  differ <- rdata_rows_differ(frame, data, rows)
  if (length(differ) > 0L) {
    rstop_data_rows_unmatched(fn, if (numbered) "numbered" else "named", differ)
  }
  invisible(rows)
}

# The columns of the model frame that the rows `rows` of `data` do not
# reproduce: the response, and each variable the frame holds as itself. A
# column that cannot be rebuilt from `data` is not compared.
rdata_rows_differ <- function(frame, data, rows) {
  model_terms <- attr(frame, "terms")
  plain <- intersect(names(frame), all.vars(model_terms))
  response <- if (isTRUE(attr(model_terms, "response") == 1L)) names(frame)[1L]
  differ <- character()
  for (column in unique(c(response, plain))) {
    values <- if (column %in% names(data)) {
      data[[column]]
    } else if (identical(column, response)) {
      rresponse_from_data(model_terms, data)
    }
    if (is.null(values)) {
      next
    }
    if (identical(rsame_rows(values, rows, frame[[column]]), FALSE)) {
      differ <- c(differ, column)
    }
  }
  differ
}

# The response of the model, such as log(y), evaluated on all the rows of
# `data` through the predvars of the terms, or NULL when `data` does not hold
# every variable it is computed from or the evaluation fails.
rresponse_from_data <- function(model_terms, data) {
  variables <- attr(model_terms, "predvars")
  if (is.null(variables)) {
    variables <- attr(model_terms, "variables")
  }
  expr <- variables[[2L]]
  needed <- all.vars(expr)
  if (length(needed) == 0L || !all(needed %in% names(data))) {
    return(NULL)
  }
  columns <- lapply(stats::setNames(needed, needed), function(v) data[[v]])
  env <- environment(model_terms)
  if (is.null(env)) {
    env <- baseenv()
  }
  values <- tryCatch(
    suppressWarnings(eval(expr, columns, env)),
    error = function(e) NULL
  )
  if (is.null(values) || NROW(values) != nrow(data)) {
    return(NULL)
  }
  values
}

# Do the rows `rows` of `values` equal `target`, a column of the model frame?
# Numbers to 1e-8 of the largest value of the column, factors and characters
# by their labels. NA when the two cannot be compared.
rsame_rows <- function(values, rows, target, tolerance = 1e-8) {
  tryCatch(
    {
      values <- if (is.matrix(target) || is.matrix(values)) {
        as.matrix(values)[rows, , drop = FALSE]
      } else {
        values[rows]
      }
      if (identical(values, target)) {
        return(TRUE)
      }
      if (NROW(values) != NROW(target) || NCOL(values) != NCOL(target)) {
        return(FALSE)
      }
      labelled <- function(v) is.factor(v) || is.character(v)
      if (labelled(values) || labelled(target)) {
        a <- as.character(values)
        b <- as.character(target)
      } else {
        a <- as.double(unclass(values))
        b <- as.double(unclass(target))
      }
      if (!identical(is.na(a), is.na(b))) {
        return(FALSE)
      }
      keep <- !is.na(a)
      a <- a[keep]
      b <- b[keep]
      same <- a == b
      if (all(same)) {
        return(TRUE)
      }
      if (is.character(a)) {
        return(FALSE)
      }
      finite <- abs(b[is.finite(b)])
      scale <- if (length(finite) > 0L && max(finite) > 0) max(finite) else 1
      all(same | (is.finite(a) & is.finite(b) & abs(a - b) <= tolerance * scale))
    },
    error = function(e) NA
  )
}

# rhandleMissingValues() drops the incomplete rows of `data` before some tests
# match the rest by row name, and a tibble numbers the rows that are left
# afresh, so their row names can then point to other rows. The rows the match
# picked, counted in `data` before the drop, must be the rows the row names of
# `data` picked before it, which rcheck_data_rows() compared with the fit.
rcheck_rows_after_cleaning <- function(data, cleaned, match_idx, ids, test_label) {
  if (cleaned$removed_count == 0L) {
    return(invisible(NULL))
  }
  kept <- seq_len(nrow(data))[-cleaned$removed_cases]
  if (!identical(kept[match_idx], match(ids, rownames(data)))) {
    rstop_data_rows_unmatched(NULL, "renumbered", label = test_label)
  }
  invisible(NULL)
}

# The error of rcheck_data_rows(), rcheck_data_in_fit_order() and
# rcheck_rows_after_cleaning(): what does not match, and what to pass instead.
rstop_data_rows_unmatched <- function(fn, reason, differ = character(), label = NULL) {
  who <- if (!is.null(fn)) sprintf("%s()", fn) else if (!is.null(label)) label else "The test"
  columns <- paste(utils::head(differ, 3L), collapse = ", ")
  verb <- if (length(differ) == 1L) "differs" else "differ"
  detail <- switch(reason,
    duplicated = paste0(
      "the row names of `data` are not unique, so they do not say which row ",
      "holds which observation."
    ),
    numbered = sprintf(
      paste0(
        "`data` has no row names of its own (a tibble, a data.table, or a ",
        "data frame whose row names were reset, as by dplyr::arrange()), so ",
        "its rows are matched to the observations by position, and in that ",
        "order its values are not those of the fit: %s %s from the model frame."
      ),
      columns, verb
    ),
    named = sprintf(
      paste0(
        "the rows of `data` that carry the row names of the fit do not hold ",
        "its values: %s %s from the model frame."
      ),
      columns, verb
    ),
    order = paste0(
      "it reads `data` by position, the i-th row with the i-th observation, ",
      "and the rows of `data` are not in the order of the fit."
    ),
    renumbered = paste0(
      "its rows with missing values were dropped, and `data`, which has no ",
      "row names of its own (a tibble), numbers the rows that are left ",
      "afresh, so their row names now point to other rows."
    )
  )
  advice <- switch(reason,
    order = "Pass the data the model was fitted on, in its order.",
    renumbered = paste0(
      "Pass it as a data frame, `as.data.frame(data)`, whose row numbers ",
      "survive the drop, or `data = NULL` to use the model frame."
    ),
    "Pass the data the model was fitted on, in its order, or `data = NULL` to use the model frame."
  )
  stop(structure(
    class = c("heteroTests_data_rows_error", "error", "condition"),
    list(
      message = paste0(
        who, " cannot match the rows of `data` to the observations of the model: ",
        detail, " ", advice
      ),
      call = NULL
    )
  ))
}

#' Refuse `data` that a test reads by position when it is not in fit order
#'
#' `performPesaranTest()`, `performBPRandomEffectsTest()` and
#' `performScatterDiagnostic()` read `data` by position, the i-th row with the
#' i-th residual, where the other tests match its rows by name. Data with its
#' own row names in another order than the fit, which the others match
#' correctly, were read against other residuals and gave wrong values. They
#' are refused, until 0.16.0 matches them by name.
#'
#' @param residuals The residuals the test reads, named by observation.
#' @param data The data the test reads, after [rcheck_data_rows()].
#' @param fn Character scalar naming the calling test.
#' @param test_label Character scalar naming the test in the message on the
#'   number of rows, which is that of [rstop_data_rows()].
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rcheck_data_in_fit_order <- function(residuals, data, fn, test_label) {
  if (nrow(data) != length(residuals)) {
    rstop_data_rows(residuals, data, test_label)
  }
  ids <- names(residuals)
  if (!is.null(ids) && !identical(match(ids, rownames(data)), seq_along(ids))) {
    rstop_data_rows_unmatched(fn, "order")
  }
  invisible(NULL)
}

# The exported name of the function `fun`, which `call` called, for messages:
# the name in the call when it is the package's name for `fun`, otherwise the
# export that is `fun` (a test passed on as `test_function`, or called through
# do.call()), or NULL. Called only for an error, so the search costs nothing
# on a call that is not refused.
rfunction_name <- function(call, fun) {
  call <- tryCatch(call, error = function(e) NULL)
  fun <- tryCatch(fun, error = function(e) NULL)
  if (!is.function(fun)) {
    return(NULL)
  }
  ns <- topenv(environment(rfunction_name))
  head <- if (is.call(call)) call[[1L]]
  if (is.call(head) && length(head) == 3L && as.character(head[[1L]]) %in% c("::", ":::")) {
    head <- head[[3L]]
  }
  candidates <- c(if (is.name(head)) as.character(head), getNamespaceExports(ns))
  for (name in candidates) {
    if (identical(get0(name, envir = ns, inherits = FALSE), fun)) {
      return(name)
    }
  }
  NULL
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
#' Three tests refuse every `glm` with this message. The others refuse a `glm`
#' that is not a least-squares fit with the same message, through
#' [rrefuse_non_gaussian_glm()], so that the refusal reads the same
#' everywhere. A `survey::svyglm()` fit is refused with the message of
#' [rrefuse_survey_fit()] instead, as by every other test.
#'
#' @param model A fitted model.
#' @param fn Character scalar naming the calling test.
#' @param reason Optional character scalar saying why.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_glm <- function(model, fn, reason = NULL) {
  rrefuse_survey_fit(model)
  if (inherits(model, "lm") && !inherits(model, "glm")) {
    return(invisible(NULL))
  }
  stop(
    sprintf("%s() needs a model fitted by stats::lm()", fn),
    if (is.null(reason)) "." else paste0(": ", reason),
    call. = FALSE
  )
}

#' Refuse a glm that is not a least-squares fit
#'
#' A `glm` with the Gaussian family and the identity link is a least-squares
#' fit: its coefficients, fitted values and residuals are those of `lm()` up to
#' rounding, and the tests return for it what they return for the `lm`
#' (`test-input-validation.R`). Any other `glm` has residuals whose variance
#' depends on the mean by construction, and before 0.15.0 most tests returned
#' a value for it that tested nothing. Such a fit is refused, decided on
#' 2026-10-15, with the message of [rrefuse_glm()].
#'
#' The tests that read the weights of an `lm`, to refit with them or to
#' refuse a weighted fit, read those of such a `glm` through
#' [rleast_squares_weights()], and give for a weighted Gaussian `glm` what they
#' give for the `lm` with the same weights.
#'
#' A `survey::svyglm()` fit is refused here too, with the message of
#' [rrefuse_survey_fit()], which says why its weights are not precision
#' weights. The tests call this first, so one that has no later check of its
#' own cannot return a value for such a fit, as `performRESETTest()` did
#' before 0.15.0.
#'
#' @param model A fitted model. Anything that is not a `glm` is left to the
#'   checks of the calling test.
#' @param fn Character scalar naming the calling test.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_non_gaussian_glm <- function(model, fn) {
  rrefuse_survey_fit(model)
  if (!inherits(model, "glm")) {
    return(invisible(NULL))
  }
  fam <- tryCatch(stats::family(model), error = function(e) NULL)
  least_squares <- !is.null(fam) && identical(fam$family, "gaussian") &&
    identical(fam$link, "identity")
  if (!least_squares) {
    described <- if (is.null(fam)) {
      "a glm whose family cannot be read"
    } else {
      sprintf("a glm with the %s family and the %s link", fam$family, fam$link)
    }
    rrefuse_glm(model, fn, paste0(
      described, " is not a least-squares fit. A glm with the gaussian family ",
      "and the identity link is accepted."
    ))
  }
  invisible(NULL)
}

#' Refuse a model with coefficients that are not finite
#'
#' An aliased term, such as `x2 = 2 * x1`, leaves an `NA` coefficient. The
#' tests that validate the model refuse such a fit with this message; the
#' five that did not, and returned a value, refuse it since 0.15.0.
#'
#' @param model A fitted model.
#' @param test_label Character scalar naming the test in the message.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_nonfinite_coefficients <- function(model, test_label) {
  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefs) || all(is.finite(coefs))) {
    return(invisible(NULL))
  }
  stop(
    sprintf(
      "Model coefficients must be finite before running %s. Check for NA or infinite estimates.",
      test_label
    ),
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
