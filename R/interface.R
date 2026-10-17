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
#' before dropping rows, and, if that differs, on the matched rows alone.
#'
#' Some columns cannot be compared:
#' * a term that depends on the rows it is computed from, such as
#'   `I(x - mean(x))`, `I(x > median(x))` or `rank(x)`, differs on the rows a
#'   fit with `subset =` or `na.omit` kept even when they are the right ones.
#'   When such a column differs, it is computed again on half the rows; if
#'   that changes its values, it is not compared. `log()`, and `poly()`,
#'   `ns()` or `scale()` through their `predvars`, do not depend on the rows
#'   and are always compared;
#' * a column whose expression names a variable `data` does not hold, or whose
#'   computation fails (a function that is no longer found).
#'
#' When a column computed from variables of `data` is not compared (it
#' depends on the rows, or its computation failed), the columns that are must
#' tell the rows of the fit apart on their own: if two rows of the fit tie on
#' all of them but differ in such a column, the data are refused, with the
#' columns that could not be compared named. A column computed from variables
#' `data` does not hold (a vector of the workspace) is read by no test from
#' `data` and does not count. When no column at all can be compared, as with
#' `lm(d$y ~ d$x)`, the data are refused. When
#' every column is compared, two rows that tie on all of them have the same
#' design row, weight, offset and residual, and exchanging them changes no
#' statistic computed from the model.
#'
#' A model fitted with `model = FALSE` has no model frame of its own, and
#' `model.frame()` rebuilds one from the data the fit refers to; if those data
#' changed after the fit, the comparison above would compare `data` with
#' itself. Such a frame is refused first (rcheck_model_frame()).
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
  checked <- rdata_rows_checked(model, frame, data, rows)
  if (length(checked$differ) > 0L) {
    rstop_data_rows_unmatched(fn, if (numbered) "numbered" else "named", checked$differ)
  }
  # A column computed from variables `data` does not hold comes from elsewhere
  # (a vector of the workspace), so no test reads it from `data`: rows that tie
  # on every compared column can be exchanged without changing what a test
  # reads. Only the columns computed from variables of `data` (a term that
  # depends on the rows, or one whose computation failed) must be told apart.
  from_data <- names(checked$skipped)[checked$skipped %in% c("dependent", "failed")]
  unconfirmed <- length(checked$skipped) > 0L && (
    length(checked$compared) == 0L ||
      (length(from_data) > 0L && rrows_tied(frame, checked$compared, from_data))
  )
  if (unconfirmed) {
    rstop_data_rows_unmatched(
      fn, "unconfirmed",
      skipped = checked$skipped, compared = checked$compared
    )
  }
  invisible(rows)
}

# Refuse a model frame that `model.frame()` rebuilt, for a fit without one of
# its own (`model = FALSE`), from data that changed after the fit. Its response
# must be the response of the fit (`fitted + residuals` for an `lm`, `y` for a
# `glm`), and its design, times the coefficients, plus its offset, the fitted
# linear predictor, both on the rows of the fit, without the padding of
# na.exclude, and with the coefficients of aliased terms left out. A frame the
# fit stored is its own and is not checked.
rcheck_model_frame <- function(model, frame, fn = NULL) {
  if (!is.null(model[["model"]]) || !inherits(model, "lm") || inherits(model, "svyglm")) {
    return(invisible(NULL))
  }
  if (identical(rframe_holds_response(model, frame), FALSE) ||
        identical(rframe_holds_fit(model, frame), FALSE)) {
    rstop_data_rows_unmatched(fn, "stale_frame")
  }
  invisible(NULL)
}

# TRUE when the response of `frame` is that of the fit, FALSE when it is not,
# NA when it cannot be checked.
rframe_holds_response <- function(model, frame) {
  response <- tryCatch(stats::model.response(frame), error = function(e) NULL)
  fitted_values <- model$fitted.values
  stored <- if (inherits(model, "glm")) model$y else fitted_values + model$residuals
  if (is.null(response) || is.null(stored) || !is.numeric(response) ||
        NCOL(response) != NCOL(stored)) {
    return(NA)
  }
  response <- unname(as.matrix(response))
  stored <- unname(as.matrix(stored))
  if (!identical(dim(response), dim(stored))) {
    return(FALSE)
  }
  scale <- pmax(abs(response), abs(stored), abs(unname(as.matrix(fitted_values))),
                .Machine$double.xmin)
  isTRUE(all(abs(response - stored) <= 1e-8 * scale))
}

# TRUE when the design of `frame`, times the coefficients, plus its offset, is
# the fitted linear predictor, FALSE when it is not, NA when it cannot be
# checked.
rframe_holds_fit <- function(model, frame) {
  tryCatch(
    {
      coefs <- stats::coef(model)
      fitted_linear <- if (inherits(model, "glm")) model$linear.predictors else model$fitted.values
      if (is.null(coefs) || is.null(fitted_linear)) {
        return(NA)
      }
      b <- as.matrix(coefs)
      X <- stats::model.matrix(attr(frame, "terms"), frame, contrasts.arg = model$contrasts)
      if (nrow(b) != ncol(X)) {
        return(FALSE)
      }
      keep <- rowSums(is.na(b)) == 0L
      X <- X[, keep, drop = FALSE]
      b <- b[keep, , drop = FALSE]
      offset <- stats::model.offset(frame)
      if (is.null(offset)) {
        offset <- 0
      }
      linear <- unname(X %*% b + offset)
      target <- unname(as.matrix(fitted_linear))
      if (!identical(dim(linear), dim(target))) {
        return(FALSE)
      }
      scale <- abs(X) %*% abs(b) + abs(offset) + abs(target) + .Machine$double.xmin
      isTRUE(all(abs(linear - target) <= 1e-8 * scale))
    },
    error = function(e) NA
  )
}

# The columns of the model frame that the rows `rows` of `data` reproduce
# (`compared`), those they do not (`differ`), and those that could not be
# compared (`skipped`, named by column, the reason as the value). A column
# `data` holds under its own name is compared as it is; a term, or the
# `(weights)` and `(offset)` columns, is computed from `data`
# (rdata_column_status()).
rdata_rows_checked <- function(model, frame, data, rows) {
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
  plain <- all.vars(model_terms)
  expression_of <- function(j, column) {
    if (j <= length(predvars)) {
      predvars[[j]]
    } else if (column %in% c("(weights)", "(offset)") && !is.null(call)) {
      call[[if (column == "(weights)") "weights" else "offset"]]
    }
  }
  out <- list(compared = character(), differ = character(), skipped = character())
  for (j in seq_along(columns)) {
    column <- columns[j]
    target <- frame[[j]]
    expr <- expression_of(j, column)
    status <- if (column %in% names(data)) {
      same <- rsame_rows(data[[column]], rows, target, recomputed = FALSE)
      computed <- NULL
      if (!isTRUE(same) && !is.null(expr) && !column %in% plain) {
        # A term `data` holds under its own name, as the model frame of
        # another fit does (a bootstrap replicate refitted on one): computed
        # from the variables of `data`, it can still be the column of this fit.
        computed <- rdata_column_status(expr, data, rows, target, env)
      }
      if (isTRUE(same)) {
        "same"
      } else if (!is.null(computed) && computed %in% c("same", "dependent")) {
        computed
      } else if (identical(same, FALSE)) {
        "differ"
      } else {
        "failed"
      }
    } else if (j <= length(predvars) || column %in% c("(weights)", "(offset)")) {
      if (is.null(expr)) "missing" else rdata_column_status(expr, data, rows, target, env)
    } else {
      # Another column the fit's call added, such as (etastart): not data.
      "ignored"
    }
    if (identical(status, "same")) {
      out$compared <- c(out$compared, column)
    } else if (identical(status, "differ")) {
      out$differ <- c(out$differ, column)
    } else if (!identical(status, "ignored")) {
      out$skipped[[column]] <- status
    }
  }
  out
}

# Is the column computed by the expression `expr` from `data` the column
# `target` of the model frame on the rows `rows`? "same", "differ", or why it
# could not be compared: "missing" (it names a variable `data` does not
# hold), "failed" (its computation fails), "dependent" (it depends on the rows
# it is computed from, and differs).
rdata_column_status <- function(expr, data, rows, target, env) {
  needed <- all.vars(expr)
  if (length(needed) == 0L || !all(needed %in% names(data))) {
    return("missing")
  }
  columns <- lapply(stats::setNames(needed, needed), function(v) data[[v]])
  computed <- function(index = NULL) {
    cols <- if (is.null(index)) {
      columns
    } else {
      lapply(columns, function(v) if (is.matrix(v)) v[index, , drop = FALSE] else v[index])
    }
    n <- if (is.null(index)) nrow(data) else length(index)
    values <- tryCatch(suppressWarnings(eval(expr, cols, env)), error = function(e) NULL)
    if (is.null(values) || NROW(values) != n) NULL else values
  }
  all_rows <- computed()
  if (!is.null(all_rows) && isTRUE(rsame_rows(all_rows, rows, target, recomputed = TRUE))) {
    return("same")
  }
  only_rows <- NULL
  if (!identical(rows, seq_len(nrow(data)))) {
    only_rows <- computed(rows)
    if (!is.null(only_rows) &&
          isTRUE(rsame_rows(only_rows, seq_along(rows), target, recomputed = TRUE))) {
      return("same")
    }
  }
  if (is.null(all_rows) && is.null(only_rows)) {
    return("failed")
  }
  # The column differs. A term that depends on the rows it is computed from
  # differs on the rows a fit with subset = or na.omit kept even when they are
  # the right ones: it is not compared. Any other term differs because the
  # rows do.
  base_index <- if (!is.null(all_rows)) seq_len(nrow(data)) else rows
  base <- if (!is.null(all_rows)) all_rows else only_rows
  if (rdepends_on_rows(computed, base, base_index)) "dependent" else "differ"
}

# Does a column, `base` when computed on the rows `base_index` of `data`,
# depend on the rows it is computed from? It is computed again on the odd and
# on the even rows, on the first and on the second half, so that every row is
# left out of at least one of them (I(x - x[1]) depends on the first row
# only); if any changes its values on the rows it keeps, it does.
rdepends_on_rows <- function(computed, base, base_index) {
  n <- length(base_index)
  if (n < 4L) {
    return(FALSE)
  }
  half <- ceiling(n / 2)
  probes <- list(
    seq.int(1L, n, by = 2L), seq.int(2L, n, by = 2L),
    seq_len(half), seq.int(half + 1L, n)
  )
  for (positions in probes) {
    again <- computed(base_index[positions])
    if (is.null(again) || !isTRUE(rsame_rows(base, positions, again, recomputed = TRUE))) {
      return(TRUE)
    }
  }
  FALSE
}

# Can two rows of the fit pass for each other on the columns `compared` and
# still be different observations? They can when they tie on every compared
# column of the model frame and differ in one of the columns `skipped`, which
# the data could not be checked on. Rows that tie on every column, as the
# repeated rows of a pairs resample do, are the same observation and do not
# count.
rrows_tied <- function(frame, compared, skipped) {
  n <- nrow(frame)
  if (n < 2L) {
    return(FALSE)
  }
  keys_of <- function(columns) {
    keys <- list()
    for (column in columns) {
      v <- frame[[column]]
      parts <- if (is.matrix(v)) lapply(seq_len(ncol(v)), function(j) v[, j]) else list(v)
      for (p in parts) {
        keys[[length(keys) + 1L]] <- if (is.factor(p)) {
          as.integer(p)
        } else if (is.character(p)) {
          p
        } else {
          as.double(unclass(p))
        }
      }
    }
    keys
  }
  compared_keys <- keys_of(compared)
  skipped_keys <- keys_of(skipped)
  sorted <- do.call(order, unname(c(compared_keys, skipped_keys)))
  ties_on <- function(keys) {
    tied <- rep(TRUE, n - 1L)
    for (k in keys) {
      ks <- k[sorted]
      a <- ks[-1L]
      b <- ks[-n]
      same <- (a == b) | (is.na(a) & is.na(b))
      same[is.na(same)] <- FALSE
      tied <- tied & same
    }
    tied
  }
  any(ties_on(compared_keys) & !ties_on(skipped_keys))
}

# Do the rows `rows` of `values` equal `target`, a column of the model frame?
# Factors and characters by their labels, dates and times exactly, and numbers
# value by value, each to 1e-8 of its own size. Numbers computed again
# (`recomputed`), which can differ in their last bits (poly() through its
# predvars), may also differ by 1e-12 of the largest value of their column.
# NA when the two cannot be compared.
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
      floor <- 0
      if (recomputed && is.double(b)) {
        column_max <- matrix(abs(b), nrow = NROW(target))
        column_max[!is.finite(column_max)] <- 0
        floor <- 1e-12 * rep(apply(column_max, 2L, max), each = NROW(target))
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
      if (length(floor) > 1L) {
        floor <- floor[keep]
      }
      bound <- tolerance * pmax(abs(a), abs(b), .Machine$double.xmin) + floor
      all(same | (is.finite(a) & is.finite(b) & abs(a - b) <= bound))
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
rstop_data_rows_unmatched <- function(fn, reason, differ = character(), label = NULL,
                                      skipped = character(), compared = character()) {
  who <- if (!is.null(fn)) sprintf("%s()", fn) else if (!is.null(label)) label else "The test"
  listed <- function(names) {
    text <- paste(utils::head(names, 3L), collapse = ", ")
    if (length(names) > 3L) {
      text <- sprintf("%s and %d more", text, length(names) - 3L)
    }
    text
  }
  columns <- listed(differ)
  verb <- if (length(differ) == 1L) "differs" else "differ"
  why <- c(
    dependent = "depends on the rows it is computed from",
    missing = "names a variable `data` does not hold",
    failed = "cannot be computed from `data`"
  )
  not_compared <- paste(
    sprintf("%s %s", names(skipped), why[unname(skipped)]),
    collapse = "; "
  )
  compared_text <- if (length(compared) > 0L) {
    sprintf(
      "the columns that can be (%s) tie on rows of the fit that differ in those",
      listed(compared)
    )
  } else {
    "no column can be compared"
  }
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
    unconfirmed = c(
      sprintf(
        paste0(
          "some columns of the model frame cannot be compared (%s), and %s, ",
          "so the rows of `data` cannot be shown to be those of the fit."
        ),
        not_compared, compared_text
      ),
      paste(
        "Pass the data the model was fitted on, with all its rows and the variables",
        "the model names, in the order of the fit or with its row names."
      )
    ),
    stale_frame = c(
      paste0(
        "the model was fitted with `model = FALSE`, so its model frame is ",
        "rebuilt from the data it was fitted on, and the rebuilt frame no ",
        "longer holds the response or the fitted values of the fit: those ",
        "data changed after the fit, reordered or with other values."
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

#' Refuse a model that is neither an lm nor a glm
#'
#' The tests take a fitted `lm`, or a `glm`, which inherits from `lm`, and
#' refuse anything else first, with one message that names the class received
#' and what is accepted, and says how a panel is tested: as its pooled `lm()`,
#' with the units and the periods as columns of `data`. Before 0.16.0 a
#' `plm::plm()` fit was refused with four different messages, depending on the
#' test. [rrefuse_non_gaussian_glm()] and [rrefuse_glm()] call it, so every
#' test that starts with either does; the validation of the model gives the
#' same message ([rmodel_class_message()]).
#'
#' @param model The `model` argument of the calling test.
#' @param fn Character scalar naming the calling function.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_model_class <- function(model, fn) {
  if (inherits(model, "lm")) {
    return(invisible(NULL))
  }
  stop(rmodel_class_message(model, sprintf("%s()", fn)), call. = FALSE)
}

# The message for a model that is neither an lm nor a glm. It begins as the
# message of the validation of the model did before 0.16.0.
rmodel_class_message <- function(model, who) {
  sprintf(
    paste0(
      "Provide an object fitted with stats::lm() or stats::glm() before running %s ",
      "(received class: %s). The tests take an lm, or a glm with the gaussian family ",
      "and the identity link; a panel is tested as its pooled lm(), with the units and ",
      "the periods as columns of `data`, as performPesaranTest() and ",
      "performBPRandomEffectsTest() take them."
    ),
    who, paste(class(model), collapse = "/")
  )
}

#' Refuse an offset that a test does not account for yet
#'
#' Four tests leave the offset of a model out of a refit, so that on a model
#' with an offset they return a value that is not that of the model: RESET,
#' Goldfeld-Quandt (an offset given as an argument, and any offset on a
#' weighted fit), the quantile regression test and the studentized
#' Breusch-Pagan statistic. Until the offset fix planned for 0.16.0 they
#' refuse such a model, decided on 2026-10-17, rather than ignore its offset.
#' An offset that is zero everywhere changes nothing and is not refused. The
#' error has class `heteroTests_offset_error`, which [ht_analyse_failure()]
#' treats as final: `runHeteroTests()` does not fall back on another test for
#' it.
#'
#' @param model The fitted model.
#' @param fn Character scalar naming the calling test.
#' @param which `"any"` to refuse any offset, `"argument"` for an offset given
#'   as the `offset` argument only.
#' @param how Character scalar: what the test does with the offset.
#' @param subtract Logical: is the test on the model fitted to the response
#'   minus the offset the test the model asks for?
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_offset <- function(model, fn, which = c("any", "argument"), how, subtract = TRUE) {
  which <- match.arg(which)
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(frame)) {
    return(invisible(NULL))
  }
  offset <- if (identical(which, "argument")) {
    frame[["(offset)"]]
  } else {
    stats::model.offset(frame)
  }
  if (is.null(offset) || all(offset == 0, na.rm = TRUE)) {
    return(invisible(NULL))
  }
  instead <- paste0(
    "performKoenkerTest(), performWhiteTest(), performBPTest() and the wild and ",
    "White bootstraps account for an offset"
  )
  if (isTRUE(subtract)) {
    instead <- paste0(
      instead, "; so does ", fn, "() on the model fitted to the response minus ",
      "the offset, such as lm(I(y - o) ~ x), which is the same fit"
    )
  }
  stop(structure(
    class = c("heteroTests_offset_error", "error", "condition"),
    list(
      message = sprintf(
        paste0(
          "%s() does not account for an offset yet: %s, so on this model it would ",
          "return a wrong value, and it refuses the model rather than ignore its ",
          "offset. %s."
        ),
        fn, how, instead
      ),
      call = NULL
    )
  ))
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
  rrefuse_model_class(model, fn)
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
  rrefuse_model_class(model, fn)
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
#' An aliased term, such as `x2 = 2 * x1`, leaves an `NA` coefficient: the
#' fit is rank-deficient (see [raliased_coefficients()]). The tests that can
#' be computed on the estimable part of the design pass
#' `allow_aliased = TRUE` and accept such a fit; the others refuse it with
#' the message of [rstop_aliased()], which names the aliased coefficients. A
#' coefficient that is infinite, `NaN`, or `NA` without the fit being
#' rank-deficient is refused by every test with the message below.
#'
#' @param model A fitted model.
#' @param test_label Character scalar naming the test in the message.
#' @param allow_aliased Logical: does the calling test accept a rank-deficient
#'   fit?
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_nonfinite_coefficients <- function(model, test_label, allow_aliased = FALSE) {
  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefs) || all(is.finite(coefs))) {
    return(invisible(NULL))
  }
  aliased <- raliased_coefficients(model)
  if (length(aliased) > 0L && all(is.finite(coefs[!names(coefs) %in% aliased]))) {
    if (isTRUE(allow_aliased)) {
      return(invisible(NULL))
    }
    rstop_aliased(aliased, test_label)
  }
  stop(rnonfinite_coefficients_message(test_label), call. = FALSE)
}

# The message for a coefficient that is infinite, NaN, or NA without the fit
# being rank-deficient.
rnonfinite_coefficients_message <- function(test_label) {
  sprintf(
    "Model coefficients must be finite before running %s. Check for NA or infinite estimates.",
    test_label
  )
}

#' The coefficients a rank-deficient fit aliased
#'
#' `lm()` and `glm()` decompose the design by a pivoted QR decomposition, and
#' a column that is a linear combination of the columns before it, such as
#' `x2 = 2 * x1`, or the dummy of a cell of an interaction that no observation
#' falls in, is moved out of the decomposition: its coefficient is `NA` and
#' the rank of the fit, `model$rank`, is the number of the others. The
#' residuals and fitted values of such a fit are the projection on the column
#' space of the design, which the aliased columns do not change, so they are
#' those of the same model fitted without the aliased columns.
#'
#' @param model A fitted model.
#' @return The names of the aliased coefficients, or `character()` when the
#'   fit is not rank-deficient or its `NA` coefficients are not aliased ones
#'   (their number differs from the number of columns its rank leaves out).
#' @keywords internal
#' @noRd
raliased_coefficients <- function(model) {
  if (!inherits(model, "lm")) {
    return(character())
  }
  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  rank <- model$rank
  if (is.null(coefs) || is.matrix(coefs) || is.null(rank) || is.null(names(coefs))) {
    return(character())
  }
  # NaN is not aliasing; lm() and glm() set an aliased coefficient to NA.
  aliased <- is.na(coefs) & !is.nan(coefs)
  if (!any(aliased) || sum(!aliased) != rank) {
    return(character())
  }
  names(coefs)[aliased]
}

# Does the refit `refit` of a resampling replicate alias a coefficient that
# the fit estimated, whose names are `estimated`? A column the replicate does
# not have at all, such as the dummy of a level no row of the replicate holds,
# is not aliased.
rrefit_aliases_estimated <- function(refit, estimated) {
  coefs <- tryCatch(stats::coef(refit), error = function(e) NULL)
  if (is.null(coefs) || is.matrix(coefs)) {
    return(FALSE)
  }
  shared <- coefs[names(coefs) %in% estimated]
  anyNA(shared)
}

#' The columns of a design that a rank-deficient fit estimated
#'
#' The tests that accept a rank-deficient fit compute their statistic on the
#' estimable part of the design: they drop the aliased columns
#' ([raliased_coefficients()]) from the design they build, and then compute
#' what they compute for the same model fitted without those columns, which
#' has the same column space, the same residuals and fitted values, and as
#' many columns as the rank of the fit. A design of a fit that is not
#' rank-deficient is returned as it is.
#'
#' @param X A design matrix of `model`, from `model.matrix()`, whose columns
#'   are named as the coefficients.
#' @param model The fitted model.
#' @return `X` without its aliased columns.
#' @keywords internal
#' @noRd
rnon_aliased_columns <- function(X, model) {
  aliased <- raliased_coefficients(model)
  if (length(aliased) == 0L) {
    return(X)
  }
  keep <- !colnames(X) %in% aliased
  X[, keep, drop = FALSE]
}

#' Refuse a rank-deficient fit with the aliased coefficients named
#'
#' No test needs every coefficient, so no test refuses a rank-deficient fit
#' since 0.16.0; the checks that are made for any test, such as those of
#' `validateTestInputs()` and of a function a user registers with
#' `test_factory`, refuse it with this message.
#'
#' @param aliased The names of the aliased coefficients.
#' @param who The test, named in the message: `"performX()"` or a label.
#' @param reason Optional character scalar saying why the test needs every
#'   coefficient.
#' @return Does not return; called for the error.
#' @keywords internal
#' @noRd
rstop_aliased <- function(aliased, who, reason = NULL) {
  listed <- paste(utils::head(aliased, 5L), collapse = ", ")
  if (length(aliased) > 5L) {
    listed <- sprintf("%s and %d more", listed, length(aliased) - 5L)
  }
  what <- if (length(aliased) == 1L) {
    sprintf(
      paste(
        "the coefficient of %s is aliased (NA): its column of the design is a",
        "linear combination of the others."
      ),
      listed
    )
  } else {
    sprintf(
      paste(
        "the coefficients of %s are aliased (NA): their columns of the design are",
        "linear combinations of the others."
      ),
      listed
    )
  }
  stop(
    sprintf(
      "%s cannot use a rank-deficient model: %s%s Refit the model without the aliased columns.",
      who, what, if (is.null(reason)) "" else paste0(" ", reason)
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
