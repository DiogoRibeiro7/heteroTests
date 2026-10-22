#' Print and summarise the results of runHeteroTests()
#'
#' @description
#' `print()` shows a [`hetero_test_suite`] compactly: the model and the number
#' of observations, on one line when it fits the console, then one row per
#' test with its statistic, degrees of freedom, p-value and significance codes,
#' and a note for each test that did not run as asked. `summary()` returns an
#' object of class `summary.hetero_test_suite`, which prints as the summary of
#' an `lm` does ([summary.lm()]): the call, the number of observations, the
#' table of tests, the legend of the significance codes and the notes; and
#' `coef()` of it returns the table, as `coef(summary(fit))` does for an `lm`.
#'
#' The [`hetero_grouped_suite`] of a grouped run has the same methods, with
#' one table for all the groups, a row for each test in each group.
#'
#' @details
#' The table is printed by [printCoefmat()], so its format is that of
#' `summary.lm()`: `digits` sets the significant digits, the p-values are
#' formatted by [format.pval()], and the significance codes and their legend
#' are shown when `signif.stars` is `TRUE` (by default
#' `getOption("show.signif.stars")`) and a p-value is below 0.1. The print
#' methods pass `printCoefmat()` `cs.ind = integer()`, `tst.ind = 1`,
#' `zap.ind = integer()`, `P.values = TRUE`, `has.Pvalue = TRUE` and
#' `na.print = "NA"`; an argument of the same name given in `...` replaces
#' the value passed, so that `print(s, na.print = "-")` prints a failed test
#' with dashes and `print(s, P.values = FALSE)` leaves the p-values out.
#'
#' The rows are named as the suite is, by the names asked for in `tests`. The
#' `df` column holds the degrees of freedom of the statistic: the parameter of
#' the test named `df`, `df1` or `num df` (as [var.test()] and [anova()] name
#' it), or the first one when they have no names. The second degrees of
#' freedom of an F statistic, named `df2` or `denom df`, are a column of their
#' own, `df2`, when a test has them. A parameter of another name, such as the
#' number of replicates `B` of a resampling test or the number of
#' observations `n` of Szroeter's test, is not a degree of freedom, and the
#' notes give it.
#'
#' The notes say which test did not run as asked: one whose fallback ran
#' under its name, saying which test ran in its place and why; one that was
#' refused or failed, with its message and, for a failure of a particular
#' kind, that kind (`attr(, "extras")$issue`, such as `"rank_deficiency"`);
#' one computed by a streaming implementation; a parameter that is not a
#' degree of freedom; and a result that is not a test, as a diagnostic added
#' with [registerDiagnostic()] may return. A test "refused" its input when it
#' stopped before computing anything, with one of the refusals of the package:
#' of the model or of `data` by the model context (the issues
#' `"model_context"` and `"data_rows"`), of a `glm` that is not a
#' least-squares fit or a model of another class, of a `survey::svyglm()` fit,
#' of zero weights, of a weighted fit by a test that refits without its
#' weights, of an aliased coefficient, or of too few observations. These are
#' recognised by their messages, since the runner records the message and the
#' issue and not the class of the condition; a test that stopped otherwise
#' "failed". `print()` gives each note on one line, cut to the width of the
#' console; `summary()` gives it whole. Without groups, [runHeteroTests()]
#' stops when a test fails and no fallback runs, so a failed test is returned
#' only by a grouped run, which records the failure and goes on. Whether a
#' result was read from a cache is not shown: a result read from it is
#' `identical()` to the one computed (see [cachedTest()]).
#'
#' Several tests of one model are not independent evidence: they are computed
#' from the same residuals, most of them from regressions of their squares on
#' overlapping sets of regressors. A small p-value from one and a large one
#' from another describe the alternatives each test is built to detect; they
#' are not two votes, and `summary()` says so.
#'
#' Neither method changes the results: [generics::tidy()],
#' [generics::glance()], [generics::augment()] and `ggplot2::autoplot()` read
#' them as before.
#'
#' @param x A `hetero_test_suite` or `hetero_grouped_suite`, as
#'   [runHeteroTests()] returns, or its summary.
#' @param object A `hetero_test_suite` or `hetero_grouped_suite` for
#'   `summary()`, or its summary for `coef()`.
#' @param digits The number of significant digits to print, as in
#'   [print.summary.lm()].
#' @param signif.stars Logical: show the significance codes?
#' @param ... Passed on to [printCoefmat()] by the print methods, replacing
#'   the arguments they pass (Details); unused by the summary and `coef()`
#'   methods.
#'
#' @return
#' The print methods return `x`, invisibly. `coef()` of a summary returns its
#' `tests`.
#'
#' `summary()` of a `hetero_test_suite` returns a list of class
#' `summary.hetero_test_suite` with:
#' \describe{
#'   \item{`call`}{The call of the model, as [summary.lm()] keeps it, or
#'     `NULL` when its call is not recorded or is one [runHeteroTests()]
#'     made itself, which names the runner's variables rather than yours: the
#'     fit of a formula, its refit within a group, the refit of a fitted
#'     model within a group, and the fit to the data of a survey design. The
#'     runner puts no mark on a model it fits, since that would change what it
#'     returns, so these are recognised by the exact form of their calls,
#'     `stats::lm(formula = model, data = prep$data)`,
#'     `stats::lm(formula = model, data = df)`,
#'     `stats::lm(formula = formula, data = data)`, and any call with
#'     `data = .ht_refit_data`. A model of yours with such a call is shown by
#'     its formula too; one fitted with `lm(formula, data)` is shown by its
#'     call.}
#'   \item{`formula`}{The formula of the model, or `NULL`.}
#'   \item{`model_class`}{The class of the model, such as `"lm"`.}
#'   \item{`nobs`}{The number of observations, [stats::nobs()] of the model.}
#'   \item{`na.action`}{The `na.action` of the model, as [summary.lm()] keeps
#'     it: the observations dropped for missing values, or `NULL`.}
#'   \item{`weights`}{How the model is weighted, as text: the `weights`
#'     argument of its call (`"1/hp"`); the variance function of a [fitWLS()]
#'     fit; or, for prior weights its call does not record, their range;
#'     `NULL` for an unweighted model.}
#'   \item{`offset`}{The offset of the model, as text, or `NULL`.}
#'   \item{`family`}{The family and link of a `glm`, as text, or `NULL`.}
#'   \item{`tests`}{A numeric matrix with one row per test, named as the
#'     suite is, and the columns `Statistic`, `df`, `df2` (only when a test has
#'     second degrees of freedom) and `p-value`, as `coef(summary(fit))` holds
#'     those of the coefficients of an `lm`; `coef(summary(s))` returns it. A
#'     failed test has `NA` in every column.}
#'   \item{`method`}{A character vector, named as the suite is: the `method`
#'     of each result, the name of the test that ran.}
#'   \item{`diagnostic`}{A character vector: the registry name of the test
#'     that ran, `attr(, "diagnostic")`, which differs from the name asked for
#'     when a fallback ran in its place.}
#'   \item{`status`}{A character vector: `"ok"`; `"fallback"` when another
#'     test ran in its place; `"error"` when it failed; or `"other"` for a
#'     result that is not an `htest`.}
#'   \item{`notes`}{A character vector of the notes, as `summary()` prints
#'     them.}
#' }
#'
#' `summary()` of a `hetero_grouped_suite` returns a list of class
#' `summary.hetero_grouped_suite` with:
#' \describe{
#'   \item{`formula`, `model_class`}{As above, for the model of the first
#'     group.}
#'   \item{`keys`}{The grouping keys, one row per group,
#'     `attr(, "group_keys")` of the suite.}
#'   \item{`nobs`}{An integer vector: the number of observations of each
#'     group.}
#'   \item{`tests`}{A data frame with one row per test in each group: the
#'     grouping keys, `test` (the name asked for), `Statistic`, `df`, `df2`
#'     (only when a test has second degrees of freedom), `p-value` and
#'     `status`, as above; `coef(summary(g))` returns it.}
#'   \item{`groups`}{A list with the `summary.hetero_test_suite` of each
#'     group.}
#'   \item{`notes`}{A character vector of the notes of every group, each
#'     starting with the label of its group.}
#' }
#'
#' @section Earlier versions:
#' Up to 0.15.0 a suite had no print or summary method: `print()` showed the
#' list, each test and then the attributes of the suite, the fitted model among
#' them, and `summary()` gave the summary of a list.
#'
#' @seealso [runHeteroTests()]; [generics::tidy()] and [generics::glance()]
#'   for the results as data frames; [printCoefmat()].
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' s <- runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
#' s
#' summary(s)
#' coef(summary(s))
#' print(summary(s), signif.stars = FALSE)
#'
#' # White's test needs 20 observations: the runner falls back on the
#' # Breusch-Pagan test, and the note says so.
#' small <- mtcars[1:18, ]
#' runHeteroTests(lm(mpg ~ wt, data = small), small)
#'
#' if (requireNamespace("dplyr", quietly = TRUE)) {
#'   by_am <- dplyr::group_by(mtcars, am)
#'   g <- runHeteroTests(mpg ~ wt, by_am, tests = "koenker")
#'   print(g)
#'   print(summary(g)$tests)
#' }
#' @name summary.hetero_test_suite
NULL

#' @rdname summary.hetero_test_suite
#' @export
print.hetero_test_suite <- function(x, digits = max(3L, getOption("digits") - 3L),
                                    signif.stars = getOption("show.signif.stars"), ...) {
  s <- summary.hetero_test_suite(x)
  rsuite_cat_heading(rsuite_heading(s))
  if (nrow(s$tests) == 0L) {
    cat("\nNo tests.\n")
  } else {
    cat("\n")
    rsuite_print_table(s$tests, digits = digits, signif.stars = signif.stars, ...)
  }
  rsuite_cat_notes(attr(s, "short_notes"), short = TRUE)
  invisible(x)
}

#' @rdname summary.hetero_test_suite
#' @export
summary.hetero_test_suite <- function(object, ...) {
  model <- rsuite_model(object)
  rows <- rsuite_rows(object)
  out <- c(
    rsuite_model_info(model, object),
    list(
      tests = rows$tests,
      method = rows$method,
      diagnostic = rows$diagnostic,
      status = rows$status,
      notes = rows$notes
    )
  )
  structure(out, class = "summary.hetero_test_suite", short_notes = rows$short_notes)
}

#' @rdname summary.hetero_test_suite
#' @export
print.summary.hetero_test_suite <- function(x, digits = max(3L, getOption("digits") - 3L),
                                            signif.stars = getOption("show.signif.stars"),
                                            ...) {
  rsuite_cat_model(x)
  cat("\nTests:\n")
  if (nrow(x$tests) == 0L) {
    cat("No tests.\n")
  } else {
    rsuite_print_table(x$tests, digits = digits, signif.stars = signif.stars, ...)
  }
  rsuite_cat_notes(x$notes, short = FALSE)
  if (sum(!is.na(x$tests[, "p-value"])) >= 2L) {
    cat("\n", paste(strwrap(rsuite_independence, width = rsuite_width()), collapse = "\n"),
        "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname summary.hetero_test_suite
#' @export
print.hetero_grouped_suite <- function(x, digits = max(3L, getOption("digits") - 3L),
                                       signif.stars = getOption("show.signif.stars"), ...) {
  s <- summary.hetero_grouped_suite(x)
  rsuite_cat_heading(rsuite_grouped_heading(s))
  table <- rsuite_grouped_matrix(s)
  if (nrow(table) == 0L) {
    cat("\nNo tests.\n")
  } else {
    cat("\n")
    rsuite_print_table(table, digits = digits, signif.stars = signif.stars, ...)
  }
  rsuite_cat_notes(attr(s, "short_notes"), short = TRUE)
  invisible(x)
}

#' @rdname summary.hetero_test_suite
#' @export
summary.hetero_grouped_suite <- function(object, ...) {
  keys <- attr(object, "group_keys")
  labels <- rsuite_group_labels(keys, length(object))
  groups <- lapply(unclass(object), function(g) {
    if (inherits(g, "hetero_test_suite")) summary.hetero_test_suite(g) else NULL
  })
  names(groups) <- labels
  first <- Find(Negate(is.null), groups)
  key_frame <- if (is.null(keys)) NULL else as.data.frame(keys)
  has_df2 <- any(vapply(groups, function(s) "df2" %in% colnames(s$tests), logical(1)))
  pieces <- lapply(seq_along(groups), function(i) {
    s <- groups[[i]]
    if (is.null(s) || nrow(s$tests) == 0L) {
      return(NULL)
    }
    m <- rsuite_with_df2(s$tests, has_df2)
    numbers <- as.data.frame(m, stringsAsFactors = FALSE, optional = TRUE)
    rownames(numbers) <- NULL
    piece <- cbind(
      data.frame(test = rownames(m), stringsAsFactors = FALSE),
      numbers,
      data.frame(status = unname(s$status), stringsAsFactors = FALSE)
    )
    if (!is.null(key_frame)) {
      piece <- cbind(key_frame[rep(i, nrow(piece)), , drop = FALSE], piece)
    }
    piece
  })
  pieces <- Filter(Negate(is.null), pieces)
  tests <- if (length(pieces) == 0L) {
    data.frame()
  } else {
    do.call(rbind, pieces)
  }
  rownames(tests) <- NULL
  prefix <- function(field) {
    unlist(lapply(seq_along(groups), function(i) {
      notes <- if (identical(field, "notes")) groups[[i]]$notes else attr(groups[[i]], field)
      if (length(notes) == 0L) character() else paste0(labels[[i]], ": ", notes)
    }), use.names = FALSE) %||% character()
  }
  out <- list(
    formula = if (is.null(first)) NULL else first$formula,
    model_class = if (is.null(first)) NA_character_ else first$model_class,
    keys = keys,
    nobs = vapply(groups, function(s) {
      if (is.null(s)) NA_integer_ else as.integer(s$nobs)
    }, integer(1), USE.NAMES = FALSE),
    tests = tests,
    groups = groups,
    notes = prefix("notes")
  )
  structure(out, class = "summary.hetero_grouped_suite", short_notes = prefix("short_notes"))
}

#' @rdname summary.hetero_test_suite
#' @export
print.summary.hetero_grouped_suite <- function(x, digits = max(3L, getOption("digits") - 3L),
                                               signif.stars = getOption("show.signif.stars"),
                                               ...) {
  if (!is.null(x$formula)) {
    cat("\nFormula:\n", paste(deparse(x$formula), collapse = "\n"), "\n", sep = "")
  }
  labels <- names(x$groups) %||% character()
  cat("\nGroups: ", length(labels), rsuite_group_by(x$keys), "\n", sep = "")
  if (length(labels) > 0L) {
    n <- ifelse(is.na(x$nobs), "NA", format(x$nobs))
    lines <- sprintf("  %s  n = %s", format(labels), n)
    cat(lines, sep = "\n")
  }
  cat("\nTests:\n")
  table <- rsuite_grouped_matrix(x)
  if (nrow(table) == 0L) {
    cat("No tests.\n")
  } else {
    rsuite_print_table(table, digits = digits, signif.stars = signif.stars, ...)
  }
  rsuite_cat_notes(x$notes, short = FALSE)
  if (any(vapply(x$groups, function(s) {
    !is.null(s) && sum(!is.na(s$tests[, "p-value"])) >= 2L
  }, logical(1)))) {
    cat("\n", paste(strwrap(rsuite_independence, width = rsuite_width()), collapse = "\n"),
        "\n", sep = "")
  }
  cat("\n")
  invisible(x)
}

#' @rdname summary.hetero_test_suite
#' @export
coef.summary.hetero_test_suite <- function(object, ...) {
  object$tests
}

#' @rdname summary.hetero_test_suite
#' @export
coef.summary.hetero_grouped_suite <- function(object, ...) {
  object$tests
}

# The pieces --------------------------------------------------------------------

rsuite_independence <- paste(
  "Several tests of one model are not independent evidence: they are computed",
  "from the same residuals."
)

rsuite_width <- function() {
  max(20L, as.integer(getOption("width", 80L)) - 2L)
}

# The model of a suite: the attribute runHeteroTests() sets, or that of its
# first test.
rsuite_model <- function(x) {
  model <- attr(x, "model", exact = TRUE)
  if (!is.null(model)) {
    return(model)
  }
  for (r in unclass(x)) {
    model <- attr(r, "model", exact = TRUE)
    if (!is.null(model)) {
      return(model)
    }
  }
  NULL
}

# The call of the model, as summary.lm() prints it, or NULL when it is a call
# runHeteroTests() made itself, whose arguments name variables of the package
# rather than of the caller. The runner puts no mark on a model it fits: a
# mark would change what it returns, which must stay identical() to what it
# returned before. The calls it makes are recognised by their exact form
# instead: those of `rsuite_runner_calls`, which fit a formula
# (.ht_prepare_model()), refit it within a group, and fit the data of a survey
# design (.ht_survey_ols()); and any call with `data = .ht_refit_data`, the
# refit of a fitted model within a group (.ht_refit_model()). A model of the
# caller's whose call has one of these forms, such as
# `stats::lm(formula = model, data = df)`, is shown by its formula too; one
# fitted by `lm(formula, data)` is shown by its call.
rsuite_runner_calls <- list(
  quote(stats::lm(formula = model, data = prep$data)),
  quote(stats::lm(formula = model, data = df)),
  quote(stats::lm(formula = formula, data = data))
)

rsuite_call <- function(model) {
  call <- tryCatch(stats::getCall(model), error = function(e) NULL)
  if (!is.call(call)) {
    return(NULL)
  }
  made_here <- identical(call$data, quote(.ht_refit_data)) ||
    any(vapply(rsuite_runner_calls, identical, logical(1), y = call))
  if (made_here) NULL else call
}

rsuite_weights <- function(model) {
  w <- tryCatch(stats::weights(model), error = function(e) NULL)
  if (is.null(w)) {
    return(NULL)
  }
  variance <- attr(model, "variance_function", exact = TRUE)
  if (is.list(variance) && !is.null(variance$form)) {
    label <- variance$label %||% "model regressors"
    return(sprintf("estimated by fitWLS() (%s form, %s)", variance$form, label))
  }
  call <- tryCatch(stats::getCall(model), error = function(e) NULL)
  given <- if (is.call(call)) call$weights
  if (!is.null(given)) {
    return(deparse1(given))
  }
  if (all(w == 1, na.rm = TRUE)) {
    return(NULL)
  }
  # Weights the call does not record, as those of a fit made by lm.wfit():
  # their range.
  values <- vapply(signif(range(w, na.rm = TRUE), 3L), format, character(1))
  sprintf("prior weights not in the call, from %s to %s", values[[1L]], values[[2L]])
}

rsuite_offset <- function(model) {
  out <- character()
  tt <- tryCatch(stats::terms(model), error = function(e) NULL)
  at <- if (is.null(tt)) NULL else attr(tt, "offset")
  if (length(at) > 0L) {
    variables <- attr(tt, "variables")
    out <- vapply(at, function(i) deparse1(variables[[i + 1L]]), character(1))
  }
  call <- tryCatch(stats::getCall(model), error = function(e) NULL)
  given <- if (is.call(call)) call$offset
  if (!is.null(given)) {
    out <- c(out, deparse1(given))
  }
  if (length(out) == 0L) NULL else paste(out, collapse = " + ")
}

rsuite_family <- function(model) {
  if (!inherits(model, "glm")) {
    return(NULL)
  }
  fam <- tryCatch(stats::family(model), error = function(e) NULL)
  if (is.null(fam)) NULL else sprintf("%s (%s link)", fam$family, fam$link)
}

rsuite_model_info <- function(model, results) {
  nobs <- if (!is.null(model)) {
    tryCatch(as.integer(stats::nobs(model)), error = function(e) NULL)
  }
  if (length(nobs) != 1L) {
    nobs <- NULL
    for (r in unclass(results)) {
      nobs <- nobs %||% attr(r, "nobs", exact = TRUE)
    }
  }
  list(
    call = if (!is.null(model)) rsuite_call(model),
    formula = if (!is.null(model)) tryCatch(stats::formula(model), error = function(e) NULL),
    model_class = if (is.null(model)) NA_character_ else class(model)[1L],
    nobs = if (length(nobs) == 1L) as.integer(nobs) else NA_integer_,
    na.action = if (!is.null(model)) tryCatch(stats::na.action(model), error = function(e) NULL),
    weights = if (!is.null(model)) rsuite_weights(model),
    offset = if (!is.null(model)) rsuite_offset(model),
    family = rsuite_family(model)
  )
}

rsuite_and <- function(x) {
  if (length(x) <= 1L) {
    return(paste(x, collapse = ""))
  }
  paste(paste(x[-length(x)], collapse = ", "), "and", x[[length(x)]])
}

rsuite_field <- function(r, name) {
  if (is.list(r)) r[[name]] else NULL
}

rsuite_number <- function(value) {
  if (is.numeric(value) && length(value) >= 1L) as.double(unname(value[[1L]])) else NA_real_
}

# The degrees of freedom of a test, and the parameters that are not.
rsuite_parameters <- function(parameter) {
  df <- c(NA_real_, NA_real_)
  other <- character()
  if (!is.numeric(parameter) || length(parameter) == 0L) {
    return(list(df = df, other = other))
  }
  labels <- names(parameter) %||% rep("", length(parameter))
  labels[is.na(labels)] <- ""
  for (k in seq_along(parameter)) {
    value <- as.double(unname(parameter[[k]]))
    label <- labels[[k]]
    # The names of this package, and R's own for an F statistic ("num df" and
    # "denom df" of var.test(), oneway.test() and anova()). Unnamed parameters
    # are read as the degrees of freedom, in order.
    slot <- if (label %in% c("df", "df1", "num df") || (label == "" && k == 1L)) {
      1L
    } else if (label %in% c("df2", "denom df", "")) {
      2L
    } else {
      0L
    }
    if (slot > 0L && is.na(df[[slot]])) {
      df[[slot]] <- value
    } else {
      other <- c(other, sprintf("%s = %s", if (nzchar(label)) label else "parameter",
                                format(value)))
    }
  }
  list(df = df, other = other)
}

# A message of a failure, without the prefix and the suggestions that
# ht_format_failure_message() adds, on one line.
rsuite_failure_prefix <- paste0(
  "^(Auxiliary regression failed|Rank deficiency detected|Perfect separation detected|",
  "Data quality issue detected|Data do not match the model|Model cannot be tested|",
  "Diagnostic execution failed)( for [^:]+)?: "
)

rsuite_message <- function(message) {
  if (!is.character(message) || length(message) == 0L || all(is.na(message))) {
    return("no message was recorded")
  }
  message <- paste(message[!is.na(message)], collapse = " ")
  message <- sub("\n\nSuggested actions:[\\s\\S]*$", "", message, perl = TRUE)
  message <- sub(rsuite_failure_prefix, "", message)
  gsub("\\s+", " ", trimws(message))
}

# A test refuses an input before it computes anything, with the message of
# one of the refusals of the package. The runner records a test that stopped
# by its message and the kind of issue ht_analyse_failure() read in it: a
# refusal of the model context (R/model_context.R) is of the issue
# "model_context", one of `data` that cannot be matched to the fit
# "data_rows", and the others, whose conditions have no class of their own
# (that of rrefuse_non_gaussian_glm() is a simpleError), are of the issue
# "diagnostic_failure", as a failure of the computation is. They are told
# apart by these words of their messages: a glm that is not a least-squares
# fit and a model of another class (rrefuse_glm(), rrefuse_model_class()), a
# survey::svyglm() fit, zero weights, a weighted fit that a test refits
# without its weights, an aliased coefficient a test cannot drop, and too few
# observations (rvalidateModelInputs()).
rsuite_refusals <- c(
  "needs a model fitted by stats::lm()",
  "Provide an object fitted with stats::lm() or stats::glm()",
  "The model is a survey::svyglm() fit",
  "fitted with zero or non-finite weights",
  "does not support weighted fits",
  "aliased (NA)",
  "observations detected but"
)

rsuite_refused <- function(issue, message) {
  issue %in% c("model_context", "data_rows") ||
    any(vapply(rsuite_refusals, grepl, logical(1), x = message, fixed = TRUE))
}

# The notes of one result, as summary() prints them (`long`) and as print()
# does (`short`). Each starts with the name of the test, so that a grouped
# run can put the label of its group in front.
rsuite_note <- function(name, r) {
  if (!inherits(r, "htest")) {
    text <- sprintf("%s returned an object of class \"%s\", not a test result.",
                    name, class(r)[1L])
    return(list(long = text, short = text))
  }
  extras <- attr(r, "extras", exact = TRUE) %||% list()
  status <- extras$status %||% "ok"
  if (identical(status, "fallback")) {
    strategy <- extras$fallback_strategy
    code <- strategy %||% attr(r, "diagnostic", exact = TRUE) %||% "another test"
    ran <- if (identical(strategy, "white_simplified")) {
      "White's test without cross products"
    } else {
      r$method %||% "another test"
    }
    why <- rsuite_message(extras$original_error %||% extras$user_message)
    refused <- rsuite_refused(extras$issue %||% "", why)
    verb <- if (refused) "refused" else "failed"
    return(list(
      long = sprintf("%s %s, and \"%s\" ran in its place (%s). The %s: %s",
                     name, verb, code, ran, if (refused) "refusal" else "failure", why),
      short = sprintf("%s %s; \"%s\" ran in its place: %s", name, verb, code, why)
    ))
  }
  if (identical(status, "error")) {
    issue <- extras$issue %||% "diagnostic_failure"
    why <- rsuite_message(extras$message)
    verb <- if (rsuite_refused(issue, why)) "refused" else "failed"
    # The kind of issue, unless it is the generic one.
    kind <- if (identical(issue, "diagnostic_failure")) "" else sprintf(" (%s)", issue)
    return(list(
      long = sprintf("%s %s%s: %s", name, verb, kind, why),
      short = sprintf("%s %s: %s", name, verb, why)
    ))
  }
  NULL
}

rsuite_rows <- function(x) {
  results <- unclass(x)
  tests <- names(results) %||% rep("", length(results))
  tests[is.na(tests)] <- ""
  n <- length(results)
  statistic <- vapply(results, function(r) rsuite_number(rsuite_field(r, "statistic")), 0)
  p_value <- vapply(results, function(r) rsuite_number(rsuite_field(r, "p.value")), 0)
  parameters <- lapply(results, function(r) rsuite_parameters(rsuite_field(r, "parameter")))
  df <- vapply(parameters, function(p) p$df[[1L]], 0)
  df2 <- vapply(parameters, function(p) p$df[[2L]], 0)
  columns <- list(Statistic = statistic, df = df)
  if (any(!is.na(df2))) {
    columns$df2 <- df2
  }
  columns$`p-value` <- p_value
  table <- matrix(
    as.double(unlist(columns, use.names = FALSE)),
    nrow = n, ncol = length(columns),
    dimnames = list(tests, names(columns))
  )

  method <- vapply(results, function(r) {
    as.character(rsuite_field(r, "method") %||% NA_character_)[1L]
  }, "")
  diagnostic <- vapply(results, function(r) {
    as.character(attr(r, "diagnostic", exact = TRUE) %||% NA_character_)[1L]
  }, "")
  status <- vapply(results, function(r) {
    if (!inherits(r, "htest")) {
      return("other")
    }
    (attr(r, "extras", exact = TRUE) %||% list())$status %||% "ok"
  }, "")
  names(method) <- names(diagnostic) <- names(status) <- tests

  long <- character()
  short <- character()
  for (i in seq_len(n)) {
    note <- rsuite_note(tests[[i]], results[[i]])
    if (!is.null(note)) {
      long <- c(long, note$long)
      short <- c(short, note$short)
    }
  }
  streamed <- tests[vapply(results, function(r) {
    isTRUE((attr(r, "extras", exact = TRUE) %||% list())$streaming)
  }, logical(1))]
  for (name in streamed) {
    text <- sprintf("%s was computed in chunks, by its streaming implementation.", name)
    long <- c(long, text)
    short <- c(short, text)
  }
  for (i in seq_len(n)) {
    other <- parameters[[i]]$other
    if (length(other) == 0L) {
      next
    }
    text <- sprintf(
      "%s's %s %s.", tests[[i]], rsuite_and(other),
      if (length(other) == 1L) "is not a degree of freedom" else "are not degrees of freedom"
    )
    long <- c(long, text)
    short <- c(short, text)
  }
  list(
    tests = table, method = method, diagnostic = diagnostic, status = status,
    notes = long, short_notes = short
  )
}

rsuite_model_label <- function(s) {
  if (!is.null(s$call)) {
    return(deparse1(s$call))
  }
  if (!is.null(s$formula)) {
    return(deparse1(s$formula))
  }
  NULL
}

rsuite_heading <- function(s) {
  label <- rsuite_model_label(s)
  n <- if (is.na(s$nobs)) "" else sprintf(", n = %d", s$nobs)
  if (is.null(label)) {
    sprintf("Heteroscedasticity tests%s", n)
  } else {
    sprintf("Heteroscedasticity tests of %s%s", label, n)
  }
}

rsuite_cat_model <- function(x) {
  if (!is.null(x$call)) {
    cat("\nCall:\n", paste(deparse(x$call), collapse = "\n"), "\n", sep = "")
  } else if (!is.null(x$formula)) {
    cat("\nFormula:\n", paste(deparse(x$formula), collapse = "\n"), "\n", sep = "")
  }
  cat("\nNumber of observations: ", if (is.na(x$nobs)) "NA" else x$nobs, "\n", sep = "")
  mess <- if (is.null(x$na.action)) "" else stats::naprint(x$na.action)
  if (nzchar(mess)) {
    cat("  (", mess, ")\n", sep = "")
  }
  if (!is.null(x$weights)) cat("Weights: ", x$weights, "\n", sep = "")
  if (!is.null(x$offset)) cat("Offset: ", x$offset, "\n", sep = "")
  if (!is.null(x$family)) cat("Family: ", x$family, "\n", sep = "")
  invisible(NULL)
}

# The table, printed as summary.lm() prints its coefficients: the statistic
# formatted as a test statistic, the degrees of freedom as they are, and the
# p-values by format.pval(), with the significance codes. An argument of
# printCoefmat() given in `...` replaces the value passed here, so that
# print(s, na.print = "-") or print(s, P.values = FALSE) does what it says.
rsuite_print_table <- function(table, digits, signif.stars, ...) {
  args <- list(
    x = table, digits = digits, signif.stars = signif.stars,
    cs.ind = integer(), tst.ind = 1L, zap.ind = integer(),
    P.values = TRUE, has.Pvalue = TRUE, na.print = "NA"
  )
  given <- list(...)
  named <- names(given) %||% rep("", length(given))
  args[named[nzchar(named)]] <- given[nzchar(named)]
  do.call(stats::printCoefmat, c(args, given[!nzchar(named)]))
}

# A heading on one line when it fits the console, and wrapped otherwise.
rsuite_cat_heading <- function(text) {
  width <- max(20L, as.integer(getOption("width", 80L)))
  lines <- if (nchar(text) <= width) text else strwrap(text, width = width, exdent = 4L)
  cat(paste(lines, collapse = "\n"), "\n", sep = "")
}

rsuite_cut <- function(text, width) {
  if (nchar(text) <= width) text else paste0(substr(text, 1L, width - 3L), "...")
}

rsuite_cat_notes <- function(notes, short) {
  if (length(notes) == 0L) {
    return(invisible(NULL))
  }
  width <- rsuite_width()
  lines <- if (short) {
    vapply(notes, function(n) rsuite_cut(paste0("  ", n), width), "", USE.NAMES = FALSE)
  } else {
    unlist(lapply(notes, strwrap, width = width, indent = 2L, exdent = 4L))
  }
  cat("\nNotes:\n", paste(lines, collapse = "\n"), "\n", sep = "")
  invisible(NULL)
}

# Grouped runs ---------------------------------------------------------------

rsuite_group_labels <- function(keys, n) {
  if (is.null(keys) || NROW(keys) != n || NCOL(keys) == 0L) {
    return(sprintf("group %d", seq_len(n)))
  }
  keys <- as.data.frame(keys)
  values <- lapply(names(keys), function(k) {
    v <- as.character(keys[[k]])
    v[is.na(v)] <- "NA"
    paste0(k, "=", v)
  })
  do.call(paste, c(values, sep = ", "))
}

rsuite_group_by <- function(keys) {
  if (is.null(keys) || NCOL(keys) == 0L) {
    return("")
  }
  paste0(", by ", paste(names(keys), collapse = ", "))
}

rsuite_grouped_heading <- function(s) {
  groups <- length(s$groups)
  label <- if (is.null(s$formula)) "" else paste0(" of ", deparse1(s$formula))
  n <- s$nobs[!is.na(s$nobs)]
  sizes <- if (length(n) == 0L) {
    ""
  } else if (min(n) == max(n)) {
    sprintf(", n = %d each", n[[1L]])
  } else {
    sprintf(", n = %d to %d", min(n), max(n))
  }
  sprintf("Heteroscedasticity tests%s%s: %d group%s%s", label, rsuite_group_by(s$keys),
          groups, if (groups == 1L) "" else "s", sizes)
}

rsuite_with_df2 <- function(m, has_df2) {
  if (has_df2 && !"df2" %in% colnames(m)) {
    m <- cbind(m[, c("Statistic", "df"), drop = FALSE], df2 = NA_real_,
               m[, "p-value", drop = FALSE])
  }
  m
}

# One table for every group: a row for each test, named by its group and test.
rsuite_grouped_matrix <- function(s) {
  groups <- s$groups
  has_df2 <- any(vapply(groups, function(g) "df2" %in% colnames(g$tests), logical(1)))
  pieces <- lapply(seq_along(groups), function(i) {
    g <- groups[[i]]
    if (is.null(g) || nrow(g$tests) == 0L) {
      return(NULL)
    }
    m <- rsuite_with_df2(g$tests, has_df2)
    rownames(m) <- paste0(names(groups)[[i]], ": ", rownames(m))
    m
  })
  pieces <- Filter(Negate(is.null), pieces)
  if (length(pieces) == 0L) {
    return(matrix(numeric(), 0L, 3L, dimnames = list(NULL, c("Statistic", "df", "p-value"))))
  }
  do.call(rbind, pieces)
}
