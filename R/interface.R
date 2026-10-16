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
  # sys.parent() is the frame of the test that called this function, also
  # when the promise is forced later, in the error.
  if (!is.null(data)) {
    rcheck_data_rows(model, data, fn = rtest_name(sys.parent()))
    return(data)
  }
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (!is.null(frame)) {
    rcheck_model_frame(model, frame, fn = rtest_name(sys.parent()))
  }
  frame
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
#' the fit, the rows the tests read, and compares them, row by row, with every
#' column of the model frame of the fit. A column that `data` holds under its
#' own name (`y`, `x1`, `group`, or `log(x1)` when `data` is the model frame)
#' is compared as it is, value by value. Any other column is computed from
#' `data`: a term such as `log(y)`, `log(x1)`, `poly(x1, 2)`, `ns(x1, 3)` or
#' `offset(o)` through the `predvars` of the terms, and the `(weights)` and
#' `(offset)` columns from the `weights` and `offset` expressions of the call.
#' It is computed on all the rows of `data`, as `model.frame()` computed it
#' before dropping rows, and the matched rows are then taken; a term that
#' depends on the rows it is computed on, such as `I(x - mean(x))`, is also
#' tried on the matched rows alone, for data that hold more rows than the fit,
#' and passes when it equals the fit's column up to an increasing affine map,
#' for data that hold only the rows the fit kept.
#' A column whose expression names a variable that `data` does not hold, or
#' whose computation fails, is not compared, and nothing is looked up outside
#' `data`.
#'
#' A model fitted with `model = FALSE` has no model frame of its own, and
#' `model.frame()` rebuilds one from the data the fit refers to; if those data
#' changed after the fit, the rebuilt frame no longer holds the fit's response
#' and the comparison above would compare `data` with itself. Such a frame is
#' refused first (rcheck_model_frame()).
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
  rcheck_model_frame(model, frame, fn)
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
  differ <- rdata_rows_differ(model, frame, data, rows)
  if (length(differ) > 0L) {
    rstop_data_rows_unmatched(fn, if (numbered) "numbered" else "named", differ)
  }
  invisible(rows)
}

# Refuse a model frame that `model.frame()` rebuilt, for a fit without one of
# its own (`model = FALSE`), from data that changed after the fit: its response
# is no longer the response of the fit, `fitted + residuals` for an `lm` and
# `y` for a `glm`, both on the rows of the fit, without the padding of
# na.exclude. A frame the fit stored is its own and is not checked.
rcheck_model_frame <- function(model, frame, fn = NULL) {
  if (!is.null(model[["model"]]) || !inherits(model, "lm") || inherits(model, "svyglm")) {
    return(invisible(NULL))
  }
  response <- tryCatch(stats::model.response(frame), error = function(e) NULL)
  fitted_values <- model$fitted.values
  stored <- if (inherits(model, "glm")) model$y else fitted_values + model$residuals
  if (is.null(response) || is.null(stored) || !is.numeric(response) ||
        NCOL(response) != NCOL(stored)) {
    return(invisible(NULL))
  }
  response <- unname(as.matrix(response))
  stored <- unname(as.matrix(stored))
  same <- identical(dim(response), dim(stored)) && {
    scale <- pmax(abs(response), abs(stored), abs(unname(as.matrix(fitted_values))),
                  .Machine$double.xmin)
    isTRUE(all(abs(response - stored) <= 1e-8 * scale))
  }
  if (!same) {
    rstop_data_rows_unmatched(fn, "stale_frame")
  }
  invisible(NULL)
}

# The columns of the model frame that the rows `rows` of `data` do not
# reproduce. A column `data` holds under its own name is compared as it is; a
# term, or the `(weights)` and `(offset)` columns, is computed from `data`
# (rdata_column_same()). A column that cannot be computed from `data` alone is
# not compared.
rdata_rows_differ <- function(model, frame, data, rows) {
  model_terms <- attr(frame, "terms")
  variables <- as.list(attr(model_terms, "variables"))[-1L]
  predvars <- attr(model_terms, "predvars")
  predvars <- if (is.null(predvars)) variables else as.list(predvars)[-1L]
  if (length(predvars) != length(variables)) {
    predvars <- variables
  }
  call <- tryCatch(stats::getCall(model), error = function(e) NULL)
  env <- environment(model_terms)
  if (is.null(env)) {
    env <- baseenv()
  }
  columns <- names(frame)
  differ <- character()
  for (j in seq_along(columns)) {
    column <- columns[j]
    target <- frame[[j]]
    same <- if (column %in% names(data)) {
      rsame_rows(data[[column]], rows, target, recomputed = FALSE)
    } else {
      expr <- if (j <= length(predvars)) {
        predvars[[j]]
      } else if (identical(column, "(weights)") && !is.null(call)) {
        call$weights
      } else if (identical(column, "(offset)") && !is.null(call)) {
        call$offset
      }
      if (is.null(expr)) NA else rdata_column_same(expr, data, rows, target, env)
    }
    if (identical(same, FALSE)) {
      differ <- c(differ, column)
    }
  }
  differ
}

# Does the expression `expr` of a model-frame column, computed from `data`,
# give `target` on the rows `rows`? It is computed on all the rows of `data`,
# as model.frame() computed it, and, if that differs, on the rows `rows` alone,
# for a term that depends on the rows it is computed on and data with more rows
# than the fit. NA when `data` does not hold every variable it names or the
# computation fails.
rdata_column_same <- function(expr, data, rows, target, env) {
  needed <- all.vars(expr)
  if (length(needed) == 0L || !all(needed %in% names(data))) {
    return(NA)
  }
  columns <- lapply(stats::setNames(needed, needed), function(v) data[[v]])
  computed <- function(cols, n) {
    values <- tryCatch(
      suppressWarnings(eval(expr, cols, env)),
      error = function(e) NULL
    )
    if (is.null(values) || NROW(values) != n) NULL else values
  }
  all_rows <- computed(columns, nrow(data))
  same <- if (is.null(all_rows)) NA else rsame_rows(all_rows, rows, target, recomputed = TRUE)
  if (isTRUE(same)) {
    return(TRUE)
  }
  only_rows <- NULL
  if (!identical(rows, seq_len(nrow(data)))) {
    only_rows <- computed(
      lapply(columns, function(v) if (is.matrix(v)) v[rows, , drop = FALSE] else v[rows]),
      length(rows)
    )
    if (!is.null(only_rows)) {
      same <- rsame_rows(only_rows, seq_along(rows), target, recomputed = TRUE)
      if (isTRUE(same)) {
        return(TRUE)
      }
    }
  }
  # A term computed from the rows it is given, such as I(x - mean(x)), on data
  # that hold other rows than those the fit computed it on (the fit dropped
  # rows, or used a subset): the right rows give the fit's column up to an
  # increasing affine map, which a permutation of distinct values cannot.
  candidate <- if (!is.null(only_rows)) {
    only_rows
  } else if (!is.null(all_rows)) {
    if (is.matrix(all_rows)) all_rows[rows, , drop = FALSE] else all_rows[rows]
  }
  if (!is.null(candidate) && isTRUE(raffine_same(candidate, target))) {
    return(TRUE)
  }
  same
}

# Are the numbers `values` an increasing affine function of `target`, column
# by column, to 1e-8 of their size? A constant column of `target` must be
# matched by a constant one. NA when they are not numbers that can be
# compared.
raffine_same <- function(values, target, tolerance = 1e-8) {
  labelled <- function(z) {
    is.factor(z) || is.character(z) || inherits(z, c("POSIXt", "Date", "difftime"))
  }
  if (labelled(values) || labelled(target)) {
    return(FALSE)
  }
  tryCatch(
    {
      v <- unname(as.matrix(unclass(values)))
      x <- unname(as.matrix(unclass(target)))
      storage.mode(v) <- "double"
      storage.mode(x) <- "double"
      if (!identical(dim(v), dim(x)) || anyNA(v) || anyNA(x) ||
            !all(is.finite(v)) || !all(is.finite(x))) {
        return(FALSE)
      }
      for (j in seq_len(ncol(x))) {
        xj <- x[, j] - mean(x[, j])
        vj <- v[, j] - mean(v[, j])
        scale <- max(abs(v[, j]), abs(x[, j]), .Machine$double.xmin)
        sxx <- sum(xj^2)
        if (sxx <= (tolerance * scale)^2) {
          if (max(abs(vj)) > tolerance * scale) {
            return(FALSE)
          }
          next
        }
        slope <- sum(xj * vj) / sxx
        if (!(slope > 0) || max(abs(vj - slope * xj)) > tolerance * scale) {
          return(FALSE)
        }
      }
      TRUE
    },
    error = function(e) NA
  )
}

# Do the rows `rows` of `values` equal `target`, a column of the model frame?
# Factors and characters by their labels, dates and times exactly. Numbers
# copied from `data` value by value, each to 1e-8 of its own size; numbers
# computed again (`recomputed`), which can differ in their last bits (poly()
# through its predvars), to 1e-8 of the largest value of the column. NA when
# the two cannot be compared.
rsame_rows <- function(values, rows, target, recomputed = FALSE, tolerance = 1e-8) {
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
      exact <- function(v) inherits(v, c("POSIXt", "Date", "difftime"))
      if (is.factor(values) && is.factor(target) && identical(levels(values), levels(target))) {
        # The same levels: the codes say what the labels say.
        return(identical(as.integer(values), as.integer(target)))
      }
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
      if (is.character(a) || exact(values) || exact(target)) {
        return(FALSE)
      }
      both_finite <- is.finite(a) & is.finite(b)
      scale <- if (recomputed) {
        finite <- abs(b[is.finite(b)])
        if (length(finite) > 0L && max(finite) > 0) max(finite) else 1
      } else {
        pmax(abs(a), abs(b), .Machine$double.xmin)
      }
      all(same | (both_finite & abs(a - b) <= tolerance * scale))
    },
    error = function(e) NA
  )
}

# rhandleMissingValues() drops the incomplete rows of `data` before some tests
# match the rest by row name, and a tibble numbers the rows that are left
# afresh, so their row names can then point to other rows. The rows the match
# picked, counted in `data` before the drop, must be the rows the row names of
# `data` picked before it, which rcheck_data_rows() compared with the fit.
rcheck_rows_after_cleaning <- function(data, cleaned, match_idx, ids, fn = NULL,
                                       test_label = NULL) {
  if (cleaned$removed_count == 0L) {
    return(invisible(NULL))
  }
  kept <- seq_len(nrow(data))[-cleaned$removed_cases]
  if (!identical(kept[match_idx], match(ids, rownames(data)))) {
    rstop_data_rows_unmatched(fn, "renumbered", label = test_label)
  }
  invisible(NULL)
}

# The rows prepare_model_data_for_test() aligned to the fit, under the row
# names of the fit, for a test that calls another test on them: a tibble it
# subset numbers its rows afresh, and the inner test would then match them by
# name to other observations. Data whose row names are already those of the
# fit are returned as they are.
rdata_with_fit_rows <- function(data, ids) {
  if (is.null(ids) || identical(rownames(data), ids)) {
    return(data)
  }
  data <- as.data.frame(data)
  rownames(data) <- ids
  data
}

# The error of the checks above: what does not match, and what to pass
# instead. `data = NULL` is not offered, as many tests need columns, weights or
# offsets that the model frame does not hold under their own names.
rstop_data_rows_unmatched <- function(fn, reason, differ = character(), label = NULL) {
  who <- if (!is.null(fn)) sprintf("%s()", fn) else if (!is.null(label)) label else "The test"
  columns <- paste(utils::head(differ, 3L), collapse = ", ")
  if (length(differ) > 3L) {
    columns <- sprintf("%s and %d more", columns, length(differ) - 3L)
  }
  verb <- if (length(differ) == 1L) "differs" else "differ"
  in_order <- paste(
    "Pass the rows of the fit in the order of the fit,",
    "or a data frame with its row names."
  )
  detail <- switch(reason,
    duplicated = c(
      paste0(
        "the row names of `data` are not unique, so they do not say which row ",
        "holds which observation."
      ),
      in_order
    ),
    numbered = c(
      sprintf(
        paste0(
          "`data` has no row names of its own (a tibble, a data.table, or a ",
          "data frame whose row names were reset, as by dplyr::arrange()), so ",
          "its rows are matched to the observations by position, and in that ",
          "order its values are not those of the fit: %s %s from the model frame."
        ),
        columns, verb
      ),
      in_order
    ),
    named = c(
      sprintf(
        paste0(
          "the rows of `data` that carry the row names of the fit do not hold ",
          "its values: %s %s from the model frame."
        ),
        columns, verb
      ),
      "Pass the data the model was fitted on."
    ),
    order = c(
      paste0(
        "it reads `data` by position, the i-th row with the i-th observation, ",
        "and the rows of `data`, matched by name, are not in the order of the fit."
      ),
      "Pass the rows of the fit in the order of the fit."
    ),
    unnamed = c(
      paste0(
        "it reads `data` by position, the i-th row with the i-th observation, ",
        "and the row names of `data` are not those of the fit, so they cannot ",
        "show that its rows are in the order of the fit."
      ),
      "Pass a data frame with the row names of the fit, in the order of the fit."
    ),
    renumbered = c(
      paste0(
        "its rows with missing values were dropped, and `data`, which has no ",
        "row names of its own (a tibble), numbers the rows that are left ",
        "afresh, so their row names now point to other rows."
      ),
      "Pass it as a data frame, `as.data.frame(data)`, whose row numbers survive the drop."
    ),
    stale_frame = c(
      paste0(
        "the model was fitted with `model = FALSE`, so its model frame is ",
        "rebuilt from the data it was fitted on, and the rebuilt frame no ",
        "longer holds the response of the fit: those data changed after the ",
        "fit, reordered or with other values."
      ),
      "Refit the model, or keep its model frame with `model = TRUE`, the default."
    )
  )
  lead <- if (identical(reason, "stale_frame")) {
    " cannot use the model frame of `model`: "
  } else {
    " cannot match the rows of `data` to the observations of the model: "
  }
  stop(structure(
    class = c("heteroTests_data_rows_error", "error", "condition"),
    list(message = paste0(who, lead, detail[1L], " ", detail[2L]), call = NULL)
  ))
}

#' Refuse `data` that a test reads by position when it is not in fit order
#'
#' `performPesaranTest()`, `performBPRandomEffectsTest()` and
#' `performScatterDiagnostic()` read `data` by position, the i-th row with the
#' i-th residual, where the other tests match its rows by name. Data with its
#' own row names in another order than the fit, which the others match
#' correctly, were read against other residuals and gave wrong values. They
#' are refused, until 0.16.0 matches them by name, and so are data whose row
#' names are not those of the fit, which cannot show their order.
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
  if (is.null(ids)) {
    return(invisible(NULL))
  }
  rows <- match(ids, rownames(data))
  if (anyNA(rows)) {
    rstop_data_rows_unmatched(fn, "unnamed")
  }
  if (!identical(rows, seq_along(ids))) {
    rstop_data_rows_unmatched(fn, "order")
  }
  invisible(NULL)
}

# The name of the test the user called, for messages: starting at frame
# `frame`, the first enclosing function that is an exported perform*()
# function, then, while its caller is another one (a deprecated name such as
# performBreuschPaganTest(), or a wrapper such as performWhiteTestRobust()),
# that one; `default` when there is none. A test passed on as `test_function`
# or called through do.call() or `::` is found by the function itself, not by
# the name in the call. Called only for an error, so the search costs nothing
# on a call that is not refused.
rtest_name <- function(frame, default = NULL) {
  frame <- tryCatch(frame, error = function(e) 0L)
  ns <- topenv(environment(rtest_name))
  exports <- grep("^perform", getNamespaceExports(ns), value = TRUE)
  export_at <- function(k) {
    if (k < 1L) {
      return(NULL)
    }
    fun <- sys.function(k)
    for (name in exports) {
      if (identical(get0(name, envir = ns, inherits = FALSE), fun)) {
        return(name)
      }
    }
    NULL
  }
  parents <- sys.parents()
  name <- NULL
  k <- frame
  while (k >= 1L && is.null(name)) {
    name <- export_at(k)
    if (is.null(name)) {
      k <- parents[k]
    }
  }
  while (!is.null(name) && parents[k] >= 1L) {
    outer <- export_at(parents[k])
    if (is.null(outer)) {
      break
    }
    name <- outer
    k <- parents[k]
  }
  if (is.null(name)) default else name
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
