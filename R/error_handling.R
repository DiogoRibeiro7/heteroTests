#' Internal helpers for robust error handling
#'
#' These functions orchestrate error analysis, recovery strategies and
#' suggestions that power the package-level resilience improvements.
#' They are not exported but are documented here for maintainability.
#' @keywords internal
NULL

ht_pretty_test_name <- function(test_name) {
  if (is.null(test_name) || length(test_name) == 0L) {
    return("Diagnostic")
  }
  pretty <- gsub("_", " ", test_name)
  tools::toTitleCase(pretty)
}

# The words of a failure's message that say what kind of failure it is, and
# so which advice it gets: "NA", "NaN" and "Inf" as R writes them, in
# capitals, with R's plurals "NAs" and "NaNs", and the other words in any
# case, each as a whole word, with "singularity" and "singularities". A word
# inside another names nothing, nor does the name of a rank test: "Rank
# permutation", "Rank-based", "Spearman rank" and "rank correlation" (the
# labels of performRankPermutationTest(), of the requirements of Szroeter's
# and the Fligner-Killeen tests and of performSpearmanTest()). Before 0.18.0
# they were matched anywhere and in any case, so that the "na" of
# performHighDimensionalTest() in a message made it a data-quality issue, and
# the "Rank" of performRankPermutationTest() a rank deficiency.
.ht_failure_words <- list(
  missing = list(cased = "\\b(NA|NaN)s?\\b", any_case = "\\bmissing\\b"),
  infinite = list(cased = "\\bInf\\b", any_case = "\\binfinite\\b"),
  rank = list(any_case = paste0(
    "(?<!spearman )\\b(rank|singular(ity|ities)?)\\b",
    "(?! permutation|-based| correlation)"
  )),
  separation = list(any_case = "\\bseparation\\b")
)

ht_message_mentions <- function(message, kind) {
  words <- .ht_failure_words[[kind]]
  (!is.null(words$cased) && any(grepl(words$cased, message, perl = TRUE))) ||
    any(grepl(words$any_case, message, ignore.case = TRUE, perl = TRUE))
}

# Whether every column of the design of `model` is constant, as in
# lm(y ~ 1) or lm(y ~ 0 + one): the model has no regressor that varies, so a
# test of the variance against the regressors has nothing to regress on, and
# removing a constant predictor leaves none. NA when the design cannot be read.
ht_constant_design <- function(model) {
  if (is.null(model)) {
    return(NA)
  }
  X <- tryCatch(stats::model.matrix(model), error = function(e) NULL)
  if (!is.matrix(X) || !is.numeric(X) || nrow(X) < 2L) {
    return(NA)
  }
  all(apply(X, 2L, function(column) all(column == column[1L])))
}

# The advice of a failure, from the words of its message and from `data`. On
# a model whose design is constant (ht_constant_design()) the constant
# columns of `data` are not advised away, which would leave no regressor, and
# a failure whose message names the regressors, the predictors or the
# intercept is advised a regressor that varies: until 0.18.0 the refusal of
# lm(y ~ 0 + one) by the Breusch-Pagan test advised "Remove constant
# predictor: one" (#183 review), as did that of lm(y ~ 1) when `data` held a
# constant column.
ht_data_cleaning_suggestions <- function(message = NULL, data = NULL, model = NULL) {
  suggestions <- character()
  constant_design <- isTRUE(ht_constant_design(model))
  names_regressors <- !is.null(message) && any(grepl(
    "\\b(regressors?|predictors?|intercept)\\b", message, ignore.case = TRUE, perl = TRUE
  ))
  if (constant_design && names_regressors) {
    suggestions <- c(
      suggestions,
      paste(
        "Fit a model with a regressor that varies: every column of this model's design is",
        "constant, so its regressors cannot explain a change in the error variance."
      )
    )
  }

  if (!is.null(message)) {
    if (ht_message_mentions(message, "missing")) {
      suggestions <- c(
        suggestions,
        "Remove or impute missing values before running diagnostics (e.g., tidyr::drop_na())."
      )
    }
    if (ht_message_mentions(message, "infinite")) {
      suggestions <- c(
        suggestions,
        "Replace infinite values with finite surrogates or remove the affected observations."
      )
    }
    if (ht_message_mentions(message, "rank")) {
      suggestions <- c(
        suggestions,
        "Remove collinear predictors or simplify interaction terms that induce rank deficiency."
      )
    }
    if (ht_message_mentions(message, "separation")) {
      suggestions <- c(
        suggestions,
        "Perfect separation detected; consider penalised regression or combining sparse factor levels."
      )
    }
  }

  if (!is.null(data) && is.data.frame(data)) {
    missing_rows <- sum(!stats::complete.cases(data))
    if (missing_rows > 0) {
      suggestions <- c(
        suggestions,
        sprintf(
          "Drop or impute %d row%s containing missing values prior to fitting.",
          missing_rows,
          ifelse(missing_rows == 1, "", "s")
        )
      )
    }

    has_infinite <- any(vapply(
      data,
      function(col) is.numeric(col) && any(is.infinite(col)),
      logical(1)
    ))
    if (has_infinite) {
      suggestions <- c(
        suggestions,
        "Replace or remove infinite values in the data before fitting."
      )
    }

    zero_var_cols <- names(Filter(
      function(col) is.numeric(col) && stats::sd(col, na.rm = TRUE) < .Machine$double.eps,
      data
    ))
    if (length(zero_var_cols) > 0 && !constant_design) {
      suggestions <- c(
        suggestions,
        sprintf(
          "Remove constant predictor%s: %s.",
          ifelse(length(zero_var_cols) == 1, "", "s"),
          paste(zero_var_cols, collapse = ", ")
        )
      )
    }
  }

  unique(suggestions)
}

ht_format_failure_message <- function(issue, raw_message, suggestions = character(), test_name = NULL) {
  prefix <- switch(
    issue,
    auxiliary_regression = "Auxiliary regression failed",
    rank_deficiency = "Rank deficiency detected",
    perfect_separation = "Perfect separation detected",
    data_quality = "Data quality issue detected",
    data_rows = "Data do not match the model",
    model_context = "Model cannot be tested",
    glm_family = "Model is not a least-squares fit",
    exact_fit = "Test cannot be computed",
    "Diagnostic execution failed"
  )
  if (!is.null(test_name)) {
    prefix <- sprintf("%s for %s", prefix, test_name)
  }
  message <- sprintf("%s: %s", prefix, raw_message)
  if (length(suggestions) > 0) {
    message <- paste0(
      message,
      "\n\nSuggested actions:\n- ",
      paste(unique(suggestions), collapse = "\n- ")
    )
  }
  message
}

ht_recover_auxiliary_fit <- function(formula, data, ..., error) {
  msg <- conditionMessage(error)
  suggestions <- ht_data_cleaning_suggestions(msg, data)
  mf <- tryCatch(
    stats::model.frame(formula, data = data, na.action = NULL),
    error = function(e) NULL
  )
  if (is.null(mf)) {
    return(list(fit = NULL, strategy = NULL, suggestions = suggestions))
  }
  numeric_cols <- vapply(mf, is.numeric, logical(1))
  finite_rows <- rep(TRUE, nrow(mf))
  if (any(numeric_cols)) {
    finite_rows <- apply(mf[, numeric_cols, drop = FALSE], 1L, function(x) all(is.finite(x)))
  }
  complete_rows <- stats::complete.cases(mf) & finite_rows
  if (!all(complete_rows)) {
    idx <- which(complete_rows)
    cleaned_data <- data[idx, , drop = FALSE]
    recovered <- tryCatch(
      lm(formula, data = cleaned_data, ...),
      error = function(e) NULL
    )
    if (!is.null(recovered)) {
      dropped <- length(complete_rows) - length(idx)
      suggestions <- unique(c(
        suggestions,
        sprintf(
          "Removed %d problematic row%s before fitting the auxiliary regression.",
          dropped,
          ifelse(dropped == 1, "", "s")
        )
      ))
      return(list(fit = recovered, strategy = "complete_cases", suggestions = suggestions))
    }
  }
  list(fit = NULL, strategy = NULL, suggestions = suggestions)
}

.ht_fallback_registry <- list(
  white = c("white_simplified", "breusch_pagan", "koenker", "ncv"),
  white_bootstrap = c("white", "breusch_pagan", "koenker"),
  breusch_pagan = c("koenker", "ncv"),
  student_bp = c("breusch_pagan", "koenker", "ncv"),
  koenker = c("ncv"),
  wild_bootstrap = c("white", "breusch_pagan"),
  quantile_regression = c("rank_permutation", "ncv"),
  rank_permutation = c("ncv"),
  high_dimensional = c("breusch_pagan", "koenker"),
  spatial_hetero = c("breusch_pagan", "white"),
  wild_bootstrap_quantile = c("quantile_regression", "rank_permutation")
)

.ht_custom_fallbacks <- list(
  white_simplified = function(model, data) {
    performWhiteTest(model, data, cross_products = FALSE)
  }
)

ht_select_fallbacks <- function(test_name, issue = NULL) {
  candidates <- .ht_fallback_registry[[test_name]]
  if (is.null(candidates)) {
    return(character())
  }
  unique(candidates)
}

ht_attempt_fallback <- function(test_name, model, data, available, context,
                                streaming_map, use_streaming, chunk_size, progress) {
  candidates <- context$fallbacks
  if (length(candidates) == 0L) {
    return(NULL)
  }

  for (candidate in candidates) {
    runner <- NULL
    diagnostic_name <- candidate
    extras <- list()
    final <- FALSE
    if (!is.null(.ht_custom_fallbacks[[candidate]])) {
      runner <- .ht_custom_fallbacks[[candidate]]
      diagnostic_name <- test_name
      extras$fallback_strategy <- candidate
    } else if (!is.null(available[[candidate]])) {
      runner <- available[[candidate]]
      if (use_streaming && candidate %in% names(streaming_map)) {
        runner <- streaming_map[[candidate]]
      }
    } else {
      next
    }

    fallback_result <- tryCatch(
      runner(model, data),
      error = function(fe) {
        ht_log(
          "WARN",
          sprintf(
            "Fallback %s for %s failed: %s",
            candidate,
            test_name,
            conditionMessage(fe)
          )
        )
        # A fallback that refuses because the regressors fit the squared
        # residuals exactly ends the search: the next ones would fit them
        # exactly too (ht_final_exact_fit()).
        final <<- ht_final_exact_fit(fe)
        NULL
      }
    )
    if (final) {
      return(NULL)
    }

    if (!is.null(fallback_result)) {
      extras <- utils::modifyList(
        extras,
        list(
          status = "fallback",
          fallback_for = test_name,
          reason = context$issue,
          suggestions = context$suggestions,
          original_error = context$raw_message,
          user_message = context$user_message,
          issue = context$issue
        )
      )
      return(.ht_decorate_result(
        fallback_result,
        diagnostic_name = diagnostic_name,
        model = model,
        data = data,
        extras = extras
      ))
    }
  }

  NULL
}

# A refusal of White's test because its auxiliary regression fits the squared
# residuals exactly, when the regressors alone fit them exactly too, or when
# that is not known (rwhite_refuse_exact_fit()): no test of those regressors
# can stand in for it.
ht_final_exact_fit <- function(error) {
  inherits(error, "heteroTests_white_exact_fit") && !identical(error$regressors_exact, FALSE)
}

# The analysis of a refusal that every substitute would make too: no fallback,
# and no cleaning to suggest.
ht_final_failure <- function(issue, raw_message, test_name) {
  list(
    raw_message = raw_message,
    user_message = ht_format_failure_message(
      issue,
      raw_message,
      test_name = ht_pretty_test_name(test_name)
    ),
    suggestions = character(),
    issue = issue,
    fallbacks = character()
  )
}

# A failure the package raises with a class of its own is told by its class;
# any other by the words of its message (ht_message_mentions()).
ht_analyse_failure <- function(test_name, error, model, data) {
  raw_message <- conditionMessage(error)
  if (inherits(error, "heteroTests_model_context_error")) {
    # The model context refused the model (R/model_context.R): a multivariate
    # lm, a model frame that cannot be rebuilt, a fit changed after it was
    # fitted. Any substitute would read the same model, or, as the
    # Cook-Weisberg test does, another one under this name: no fallback
    # (decision 6 of the design of the model context).
    return(ht_final_failure("model_context", raw_message, test_name))
  }
  if (ht_final_exact_fit(error)) {
    # White's auxiliary regression fits the squared residuals exactly, and so
    # do the regressors alone, or may: then so would the auxiliary regressions
    # of the fallbacks, the Breusch-Pagan and Koenker tests of those
    # regressors, whose statistics would mean nothing. No fallback. When the
    # regressors do not fit them exactly, as on a design with as many
    # auxiliary terms as observations, White's test without cross products
    # comes first, as on any other refusal of White's test.
    return(ht_final_failure("exact_fit", raw_message, test_name))
  }
  if (inherits(error, "heteroTests_data_rows_error")) {
    # `data` cannot be matched to the rows of the fit (rcheck_rows_at()), or
    # lacks some of them (rstop_data_rows(), with the class since 0.18.0).
    # Every substitute would read the same rows, or, as the Cook-Weisberg
    # test does, ignore `data` and return another test under this name, so
    # there is no fallback and no cleaning to suggest.
    return(ht_final_failure("data_rows", raw_message, test_name))
  }
  if (inherits(error, "heteroTests_glm_family_error")) {
    # A glm that is not a least-squares fit (rrefuse_glm()), which every test
    # of the package refuses, the substitutes included. Before 0.18.0 the
    # refusal had no class: the runner tried the fallbacks, and labelled it
    # by its words, a rank deficiency for "rank_permutation" and a
    # data-quality issue for "high_dimensional", with their advice.
    return(ht_final_failure("glm_family", raw_message, test_name))
  }
  suggestions <- ht_data_cleaning_suggestions(raw_message, data, model)
  issue <- "diagnostic_failure"
  if (ht_message_mentions(raw_message, "rank")) {
    issue <- "rank_deficiency"
  } else if (ht_message_mentions(raw_message, "separation")) {
    issue <- "perfect_separation"
  } else if (ht_message_mentions(raw_message, "missing") ||
               ht_message_mentions(raw_message, "infinite")) {
    issue <- "data_quality"
  }

  pretty_name <- ht_pretty_test_name(test_name)
  user_message <- ht_format_failure_message(
    issue,
    raw_message,
    suggestions = suggestions,
    test_name = pretty_name
  )

  list(
    raw_message = raw_message,
    user_message = user_message,
    suggestions = suggestions,
    issue = issue,
    fallbacks = ht_select_fallbacks(test_name, issue)
  )
}
