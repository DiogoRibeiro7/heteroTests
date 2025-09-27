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

ht_data_cleaning_suggestions <- function(message = NULL, data = NULL) {
  suggestions <- character()

  if (!is.null(message)) {
    if (grepl("NA|NaN|missing", message, ignore.case = TRUE)) {
      suggestions <- c(
        suggestions,
        "Remove or impute missing values before running diagnostics (e.g., tidyr::drop_na())."
      )
    }
    if (grepl("Inf", message, ignore.case = TRUE)) {
      suggestions <- c(
        suggestions,
        "Replace infinite values with finite surrogates or remove the affected observations."
      )
    }
    if (grepl("rank|singular", message, ignore.case = TRUE)) {
      suggestions <- c(
        suggestions,
        "Remove collinear predictors or simplify interaction terms that induce rank deficiency."
      )
    }
    if (grepl("perfect separation|separation", message, ignore.case = TRUE)) {
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

    zero_var_cols <- names(Filter(
      function(col) is.numeric(col) && stats::sd(col, na.rm = TRUE) < .Machine$double.eps,
      data
    ))
    if (length(zero_var_cols) > 0) {
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
  hc_covariance = c("hc_covariance_hc0", "breusch_pagan", "koenker"),
  quantile_regression = c("rank_permutation", "hc_covariance", "ncv"),
  rank_permutation = c("ncv"),
  high_dimensional = c("hc_covariance", "breusch_pagan", "koenker"),
  spatial_hetero = c("breusch_pagan", "white"),
  wild_bootstrap_quantile = c("quantile_regression", "rank_permutation")
)

.ht_custom_fallbacks <- list(
  white_simplified = function(model, data) {
    performWhiteTest(model, data, cross_products = FALSE)
  },
  hc_covariance_hc0 = function(model, data) {
    performHCCovarianceTest(model, data, type = "HC0")
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
        NULL
      }
    )

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

ht_analyse_failure <- function(test_name, error, model, data) {
  raw_message <- conditionMessage(error)
  suggestions <- ht_data_cleaning_suggestions(raw_message, data)
  issue <- "diagnostic_failure"
  if (grepl("rank|singular", raw_message, ignore.case = TRUE)) {
    issue <- "rank_deficiency"
  } else if (grepl("perfect separation|separation", raw_message, ignore.case = TRUE)) {
    issue <- "perfect_separation"
  } else if (grepl("NA|NaN|missing|Inf", raw_message, ignore.case = TRUE)) {
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
