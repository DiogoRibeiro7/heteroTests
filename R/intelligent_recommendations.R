#' Intelligent recommendation system for heteroscedasticity diagnostics
#'
#' The helpers in this module analyse dataset characteristics, interpret
#' diagnostic test results, and assemble remediation and reporting guidance for
#' users who need prescriptive recommendations. They build on the existing test
#' suite but expose higher-level summaries suitable for analysts who may not be
#' specialists in regression diagnostics.
#'
#' @keywords internal
NULL

#' Analyse dataset characteristics relevant to heteroscedasticity testing
#'
#' Provides a compact profile of the supplied dataset, capturing size,
#' distributional properties, and missingness metrics that influence diagnostic
#' selection.
#'
#' @param data A `data.frame` containing the modelling variables.
#' @param response Optional character scalar naming the response variable. Used
#'   only for labelling purposes.
#'
#' @return A list with summary statistics, including overall size, missingness
#'   rates, skewness diagnostics for numeric variables, and detected risk
#'   factors (e.g. high dimensionality, spatial columns).
#' @export
analyseDatasetCharacteristics <- function(data, response = NULL) {
  checkData(data)

  response <- if (is.null(response)) NA_character_ else response

  n_obs <- nrow(data)
  n_vars <- ncol(data)
  total_cells <- n_obs * max(n_vars, 1L)
  missing_counts <- vapply(data, function(col) sum(is.na(col)), integer(1))
  overall_missing <- if (total_cells == 0) 0 else sum(missing_counts) / total_cells
  missing_by_var <- if (n_obs == 0) rep(0, length(missing_counts)) else missing_counts / n_obs

  numeric_cols <- names(Filter(is.numeric, data))
  distribution_summary <- list()
  if (length(numeric_cols) > 0) {
    distribution_summary <- lapply(numeric_cols, function(var) {
      values <- data[[var]]
      values <- values[is.finite(values)]
      n <- length(values)
      if (n < 3L) {
        return(list(
          variable = var,
          mean = NA_real_,
          sd = NA_real_,
          skewness = NA_real_,
          kurtosis = NA_real_,
          shape = "insufficient_data"
        ))
      }

      m <- mean(values)
      s <- stats::sd(values)
      if (is.na(s) || s == 0) {
        return(list(
          variable = var,
          mean = m,
          sd = s,
          skewness = NA_real_,
          kurtosis = NA_real_,
          shape = "constant"
        ))
      }

      centred <- (values - m) / s
      skew <- mean(centred^3)
      kurt <- mean(centred^4) - 3
      shape <- if (is.na(skew)) {
        "undetermined"
      } else if (skew > 0.75) {
        "strong_right_skew"
      } else if (skew > 0.25) {
        "mild_right_skew"
      } else if (skew < -0.75) {
        "strong_left_skew"
      } else if (skew < -0.25) {
        "mild_left_skew"
      } else {
        "approximately_symmetric"
      }

      list(
        variable = var,
        mean = m,
        sd = s,
        skewness = skew,
        kurtosis = kurt,
        shape = shape
      )
    })
  }

  distribution_summary <- if (length(distribution_summary) == 0) {
    data.frame()
  } else {
    data.frame(
      variable = vapply(distribution_summary, `[[`, character(1), "variable"),
      mean = vapply(distribution_summary, `[[`, numeric(1), "mean"),
      sd = vapply(distribution_summary, `[[`, numeric(1), "sd"),
      skewness = vapply(distribution_summary, `[[`, numeric(1), "skewness"),
      kurtosis = vapply(distribution_summary, `[[`, numeric(1), "kurtosis"),
      shape = vapply(distribution_summary, `[[`, character(1), "shape"),
      stringsAsFactors = FALSE
    )
  }

  size_bucket <- if (n_obs < 100) {
    "small"
  } else if (n_obs < 1000) {
    "medium"
  } else if (n_obs < 10000) {
    "large"
  } else {
    "very_large"
  }

  missingness_level <- if (overall_missing == 0) {
    "none"
  } else if (overall_missing < 0.05) {
    "low"
  } else if (overall_missing < 0.15) {
    "moderate"
  } else {
    "high"
  }

  spatial_columns <- any(grepl("lat|lon|lng|long|geometry|x_coord|y_coord", names(data), ignore.case = TRUE))
  has_sf <- inherits(data, "sf") || any(vapply(data, inherits, logical(1), what = "sfc"))

  list(
    response = response,
    n_obs = n_obs,
    n_vars = n_vars,
    size_bucket = size_bucket,
    high_dimensional = n_vars > n_obs,
    missingness = list(
      overall_rate = overall_missing,
      level = missingness_level,
      by_variable = stats::setNames(as.numeric(missing_by_var), names(missing_counts))
    ),
    numeric_distribution = distribution_summary,
    detected_spatial_fields = spatial_columns || has_sf,
    categorical_variables = names(Filter(function(x) is.factor(x) || is.character(x), data)),
    notes = c(
      if (n_vars > n_obs) "More predictors than observations detected (p > n).",
      if (overall_missing > 0) sprintf("Missing data present in %d of %d variables.", sum(missing_counts > 0), length(missing_counts)),
      if (spatial_columns || has_sf) "Variables suggesting spatial structure detected."
    )
  )
}

#' Suggest diagnostics based on dataset profile
#'
#' Uses heuristics that consider sample size, dimensionality, missingness, and
#' spatial indicators to recommend a set of diagnostics from the package
#' registry.
#'
#' @param profile Output from [analyseDatasetCharacteristics()].
#'
#' @return A data frame with recommended diagnostics and the rationale for each
#'   suggestion.
#' @export
suggestDiagnosticsForProfile <- function(profile) {
  stopifnot(is.list(profile))

  recommendations <- list(
    list(test = "breusch_pagan", rationale = "Baseline regression-based heteroscedasticity check."),
    list(test = "white", rationale = "Detects general forms of non-constant variance.")
  )

  if (identical(profile$size_bucket, "small")) {
    recommendations <- c(recommendations, list(
      list(test = "wild_bootstrap", rationale = "Bootstrap inference performs well in small samples."),
      list(test = "rank_permutation", rationale = "Rank-based test is robust when asymptotics are unreliable.")
    ))
  }

  if (identical(profile$size_bucket, "very_large")) {
    recommendations <- c(recommendations, list(
      list(test = "hc_covariance", rationale = "HC3/HC4 covariance estimators stabilise inference for large datasets."),
      list(test = "quantile_regression", rationale = "Quantile-based check highlights distributional heterogeneity.")
    ))
  }

  if (isTRUE(profile$high_dimensional)) {
    recommendations <- c(recommendations, list(
      list(test = "high_dimensional", rationale = "Designed for scenarios where predictors outnumber observations.")
    ))
  }

  if (isTRUE(profile$detected_spatial_fields)) {
    recommendations <- c(recommendations, list(
      list(test = "spatial_hetero", rationale = "Spatial heteroscedasticity diagnostic for geographic data."),
      list(test = "wild_bootstrap", rationale = "Spatial dependence often benefits from bootstrap-based inference.")
    ))
  }

  if (!is.null(profile$missingness) && profile$missingness$overall_rate >= 0.05) {
    recommendations <- c(recommendations, list(
      list(test = "quantile_regression", rationale = "Quantile diagnostics remain informative with moderate missingness."),
      list(test = "wild_bootstrap", rationale = "Bootstrap procedures mitigate imbalance from incomplete cases.")
    ))
  }

  if (nrow(profile$numeric_distribution) > 0) {
    skewed <- profile$numeric_distribution$variable[profile$numeric_distribution$shape %in% c("strong_right_skew", "strong_left_skew")]
    if (length(skewed) > 0) {
      recommendations <- c(recommendations, list(
        list(test = "quantile_regression", rationale = sprintf("Variables %s show strong skewness; quantile checks capture tail effects.", paste(skewed, collapse = ", ")))
      ))
    }
  }

  unique_tests <- unique(vapply(recommendations, `[[`, character(1), "test"))
  df <- data.frame(
    test = unique_tests,
    rationale = vapply(unique_tests, function(tst) {
      reasons <- vapply(recommendations, function(rec) {
        if (rec$test == tst) rec$rationale else NA_character_
      }, character(1))
      paste(stats::na.omit(reasons), collapse = " ")
    }, character(1)),
    stringsAsFactors = FALSE
  )

  df[order(df$test), , drop = FALSE]
}

#' Interpret heteroscedasticity diagnostic results
#'
#' Generates human-readable interpretations of hypothesis test results along
#' with qualitative confidence levels derived from p-values.
#'
#' @param test_results Named list of objects inheriting from `htest`.
#' @param alpha Significance level for determining whether heteroscedasticity is
#'   detected.
#'
#' @return A list containing a tidy data frame of interpretations and a summary
#'   conclusion.
#' @export
interpretHeteroTestResults <- function(test_results, alpha = 0.05) {
  stopifnot(is.list(test_results))

  results <- lapply(names(test_results), function(name) {
    test <- test_results[[name]]
    if (!inherits(test, "htest")) {
      return(list(
        test = name,
        method = NA_character_,
        statistic = NA_real_,
        p_value = NA_real_,
        decision = "Unavailable",
        confidence = "Unknown",
        interpretation = sprintf("Result for %s is unavailable or malformed.", name)
      ))
    }

    p_val <- test$p.value
    decision <- if (is.na(p_val)) {
      "Inconclusive"
    } else if (p_val < alpha) {
      "Heteroscedasticity detected"
    } else {
      "No heteroscedasticity detected"
    }

    confidence <- if (is.na(p_val)) {
      "Unknown"
    } else if (p_val < 0.001) {
      "Very high"
    } else if (p_val < 0.01) {
      "High"
    } else if (p_val < 0.05) {
      "Moderate"
    } else if (p_val < 0.1) {
      "Low"
    } else {
      "Very low"
    }

    statistic <- test$statistic
    statistic <- if (is.null(statistic)) NA_real_ else unname(statistic)[1]

    interpretation <- if (decision == "Heteroscedasticity detected") {
      sprintf("Reject constant variance assumption based on %s (p = %.3f).", test$method %||% name, p_val)
    } else if (decision == "No heteroscedasticity detected") {
      sprintf("No evidence of heteroscedasticity from %s (p = %.3f).", test$method %||% name, p_val)
    } else {
      sprintf("%s did not return a valid p-value.", test$method %||% name)
    }

    list(
      test = name,
      method = test$method %||% name,
      statistic = statistic,
      p_value = p_val,
      decision = decision,
      confidence = confidence,
      interpretation = interpretation
    )
  })

  interpret_df <- data.frame(
    test = vapply(results, `[[`, character(1), "test"),
    method = vapply(results, `[[`, character(1), "method"),
    statistic = vapply(results, `[[`, numeric(1), "statistic"),
    p_value = vapply(results, `[[`, numeric(1), "p_value"),
    decision = vapply(results, `[[`, character(1), "decision"),
    confidence = vapply(results, `[[`, character(1), "confidence"),
    interpretation = vapply(results, `[[`, character(1), "interpretation"),
    stringsAsFactors = FALSE
  )

  hetero_detected <- sum(interpret_df$decision == "Heteroscedasticity detected", na.rm = TRUE)
  summary <- if (hetero_detected == 0) {
    "No strong evidence of heteroscedasticity across the evaluated diagnostics."
  } else if (hetero_detected == 1) {
    "One diagnostic indicates heteroscedasticity; consider corroborating with robust follow-up checks."
  } else {
    sprintf("%d diagnostics indicate heteroscedasticity; remediation is recommended.", hetero_detected)
  }

  structure(
    list(
      results = interpret_df,
      summary = summary,
      supporting_tests = hetero_detected
    ),
    class = "hetero_test_interpretation"
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    return(y)
  }
  if (is.atomic(x) && length(x) == 1L && is.na(x)) {
    return(y)
  }
  x
}

#' Detect heteroscedasticity patterns from fitted model
#'
#' @param model Fitted `lm`/`glm` object.
#' @param data Data used to fit the model.
#'
#' @return A list describing detected patterns and recommended remediation
#'   tactics.
#' @keywords internal
identifyVariancePatterns <- function(model, data) {
  res <- tryCatch(stats::residuals(model), error = function(e) NULL)
  fitted_vals <- tryCatch(stats::fitted(model), error = function(e) NULL)

  if (is.null(res) || is.null(fitted_vals)) {
    return(list(description = "Residual diagnostics unavailable.", suggestions = character()))
  }

  abs_res <- abs(res)
  corr_fit <- suppressWarnings(stats::cor(abs_res, fitted_vals, use = "complete.obs"))

  pattern <- "No dominant variance pattern detected."
  suggestions <- character()

  if (!is.na(corr_fit) && corr_fit > 0.4) {
    pattern <- "Variance appears to increase with fitted values."
    suggestions <- c(suggestions, "Consider log or Box-Cox transformations to stabilise variance.", "Fit a weighted least squares model using inverse variance weights.")
  } else if (!is.na(corr_fit) && corr_fit < -0.4) {
    pattern <- "Variance appears to decrease with fitted values."
    suggestions <- c(suggestions, "Inspect for floor effects or truncation.", "Model variance explicitly via variance functions (e.g., nlme::gls).")
  }

  numeric_predictors <- data[names(Filter(is.numeric, data))]
  if (ncol(numeric_predictors) > 0) {
    correlations <- vapply(numeric_predictors, function(col) {
      suppressWarnings(stats::cor(abs_res, col, use = "complete.obs"))
    }, numeric(1))
    correlations[is.na(correlations)] <- 0
    strongest <- which.max(abs(correlations))
    if (length(strongest) == 1 && abs(correlations[strongest]) > 0.4) {
      predictor <- names(correlations)[strongest]
      direction <- if (correlations[strongest] > 0) "higher" else "lower"
      pattern <- sprintf("Variance is strongly associated with %s %s values.", predictor, direction)
      suggestions <- c(suggestions, sprintf("Model variance as a function of %s (e.g., via varIdent in nlme::gls).", predictor))
    }
  }

  leverage <- tryCatch(stats::hatvalues(model), error = function(e) NULL)
  if (!is.null(leverage)) {
    high_leverage <- sum(leverage > (2 * mean(leverage, na.rm = TRUE)), na.rm = TRUE)
    if (high_leverage > 0) {
      suggestions <- c(suggestions, sprintf("%d observations show high leverage; robust regression or influence diagnostics recommended.", high_leverage))
    }
  }

  list(description = pattern, suggestions = unique(suggestions))
}

#' Recommend remediation strategies with contextual guidance
#'
#' @param test_results Named list of `htest` objects.
#' @param model Fitted model object.
#' @param data Dataset used for fitting.
#' @param profile Optional profile from [analyseDatasetCharacteristics()].
#'
#' @return Structured remediation guidance with severity assessments and
#'   recommended actions.
#' @export
recommendRemediationStrategies <- function(test_results, model, data, profile = NULL) {
  base <- suggestRemediation(test_results)
  pattern <- identifyVariancePatterns(model, data)

  severity <- base$severity %||% if (length(base) == 0 || !is.null(base$conclusion)) "None" else "Low"
  severity_label <- switch(severity,
    High = "Significant evidence of heteroscedasticity detected.",
    Medium = "Multiple diagnostics highlight variance instability.",
    Low = "Limited evidence of heteroscedasticity; monitor residuals.",
    None = "No remediation required.",
    "Severity undetermined."
  )

  adaptive_actions <- character()
  if (!is.null(profile)) {
    if (isTRUE(profile$high_dimensional)) {
      adaptive_actions <- c(adaptive_actions, "Reduce dimensionality or apply penalised regression to stabilise variance structure.")
    }
    if (!is.null(profile$missingness) && profile$missingness$overall_rate > 0.05) {
      adaptive_actions <- c(adaptive_actions, "Handle missing data via multiple imputation before re-running diagnostics.")
    }
    if (identical(profile$size_bucket, "very_large")) {
      adaptive_actions <- c(adaptive_actions, "Use heteroscedasticity-consistent covariance estimators (HC3/HC4) to avoid inflated Type I error.")
    }
  }

  structure(
    list(
      severity = severity,
      summary = severity_label,
      pattern = pattern$description,
      actions = unique(c(
        base$transformations %||% character(),
        base$variance_modeling %||% character(),
        base$action %||% character(),
        base$conclusion %||% character(),
        pattern$suggestions,
        adaptive_actions
      ))
    ),
    class = "hetero_remediation_guidance"
  )
}

#' Generate assumption warnings and alternative recommendations
#'
#' @param profile Dataset profile produced by [analyseDatasetCharacteristics()].
#' @param interpretations Output from [interpretHeteroTestResults()].
#' @param model Fitted model object.
#' @param data Dataset used to fit the model.
#' @param test_results Named list of diagnostics.
#'
#' @return Character vector of warnings and alternative strategies.
#' @export
warnDiagnosticAssumptions <- function(profile, interpretations, model, data, test_results) {
  warnings <- character()

  if (!is.null(profile$missingness) && profile$missingness$overall_rate > 0) {
    warnings <- c(warnings, sprintf("Missing data detected (%.1f%% of entries); ensure diagnostics use complete cases or impute.", profile$missingness$overall_rate * 100))
  }

  if (identical(profile$size_bucket, "small")) {
    warnings <- c(warnings, "Sample size is limited; rely on wild bootstrap or permutation-based tests for reliable inference.")
  }

  if (identical(profile$size_bucket, "very_large")) {
    warnings <- c(warnings, "Dataset is very large; asymptotic tests may flag trivial deviations. Prioritise effect sizes and HC estimators.")
  }

  if (isTRUE(profile$high_dimensional)) {
    warnings <- c(warnings, "High-dimensional design detected (p > n); classical tests may be invalid. Use high-dimensional diagnostics.")
  }

  if (isTRUE(profile$detected_spatial_fields)) {
    warnings <- c(warnings, "Spatial fields identified; ensure spatial weights (`listw`) are provided for spatial heteroscedasticity tests.")
  }

  if (length(profile$notes) > 0) {
    warnings <- unique(c(warnings, profile$notes))
  }

  inconclusive <- interpretations$results$decision == "Inconclusive"
  if (any(inconclusive)) {
    names <- interpretations$results$method[inconclusive]
    warnings <- c(warnings, sprintf("%s returned inconclusive results; consider rerunning with adjusted parameters or alternative tests.", paste(names, collapse = ", ")))
  }

  if (length(test_results) == 0) {
    warnings <- c(warnings, "No diagnostics executed; provide `test_results` or allow automatic execution.")
  }

  unique(warnings)
}

#' Build a decision tree for diagnostic selection
#'
#' @param profile Dataset profile from [analyseDatasetCharacteristics()].
#' @param recommendations Data frame produced by [suggestDiagnosticsForProfile()].
#'
#' @return A tidy data frame describing decision points and suggested actions.
#' @export
buildDiagnosticDecisionTree <- function(profile, recommendations) {
  stopifnot(is.list(profile))

  nodes <- list(
    list(
      step = 1L,
      condition = "Is the sample size < 100?",
      decision = if (identical(profile$size_bucket, "small")) "Yes" else "No",
      recommendation = if (identical(profile$size_bucket, "small")) "Emphasise wild bootstrap and rank permutation tests." else "Standard regression diagnostics appropriate."
    ),
    list(
      step = 2L,
      condition = "Are predictors more numerous than observations?",
      decision = if (isTRUE(profile$high_dimensional)) "Yes" else "No",
      recommendation = if (isTRUE(profile$high_dimensional)) "Run the high-dimensional variance diagnostic and consider dimensionality reduction." else "Proceed with conventional tests."
    ),
    list(
      step = 3L,
      condition = "Is spatial structure present?",
      decision = if (isTRUE(profile$detected_spatial_fields)) "Yes" else "No",
      recommendation = if (isTRUE(profile$detected_spatial_fields)) "Prepare spatial weights and run spatial heteroscedasticity checks." else "Spatial-specific diagnostics not required."
    ),
    list(
      step = 4L,
      condition = "Does missingness exceed 5%?",
      decision = if (!is.null(profile$missingness) && profile$missingness$overall_rate >= 0.05) "Yes" else "No",
      recommendation = if (!is.null(profile$missingness) && profile$missingness$overall_rate >= 0.05) "Impute or model missingness before rerunning quantile/robust diagnostics." else "Missingness unlikely to bias diagnostics."
    )
  )

  add_recommendations <- function(node_list, rec_df) {
    if (nrow(rec_df) == 0) {
      return(node_list)
    }
    node_list[[length(node_list) + 1]] <- list(
      step = length(node_list) + 1L,
      condition = "Recommended diagnostics",
      decision = "--",
      recommendation = paste(sprintf("%s: %s", rec_df$test, rec_df$rationale), collapse = "\n")
    )
    node_list
  }

  nodes <- add_recommendations(nodes, recommendations)

  data.frame(
    step = vapply(nodes, `[[`, integer(1), "step"),
    condition = vapply(nodes, `[[`, character(1), "condition"),
    decision = vapply(nodes, `[[`, character(1), "decision"),
    recommendation = vapply(nodes, `[[`, character(1), "recommendation"),
    stringsAsFactors = FALSE
  )
}

#' Generate an accessible diagnostic recommendation report
#'
#' @param profile Dataset profile.
#' @param interpretations Interpretation output.
#' @param remediation Remediation guidance.
#' @param assumption_warnings Character vector of warnings.
#' @param decision_tree Data frame produced by [buildDiagnosticDecisionTree()].
#'
#' @return Character vector containing a plain-language report aimed at
#'   non-statisticians.
#' @export
composeDiagnosticNarrative <- function(profile, interpretations, remediation, assumption_warnings, decision_tree) {
  lines <- c(
    "Heteroscedasticity Diagnostic Recommendation Report",
    paste0("Observations: ", profile$n_obs, "; Variables: ", profile$n_vars),
    paste0("Sample size category: ", profile$size_bucket),
    paste0("Missingness level: ", profile$missingness$level, sprintf(" (%.1f%% overall)", profile$missingness$overall_rate * 100)),
    if (!is.na(profile$response)) paste0("Response variable: ", profile$response) else NULL,
    "",
    "Test Interpretations:",
    interpretations$summary,
    apply(interpretations$results, 1, function(row) {
      sprintf("- %s: %s", row["method"], row["interpretation"])
    }),
    "",
    "Recommended Remediation:",
    remediation$summary,
    if (length(remediation$actions) > 0) sprintf("- %s", remediation$actions) else "- No specific actions required.",
    "",
    "Assumption Checks:",
    if (length(assumption_warnings) > 0) sprintf("- %s", assumption_warnings) else "- No assumption warnings triggered.",
    "",
    "Decision Path:",
    apply(decision_tree, 1, function(row) {
      sprintf("Step %s [%s]: %s", row["step"], row["decision"], row["recommendation"])
    })
  )

  unname(unlist(lines))
}

#' Generate comprehensive heteroscedasticity recommendations
#'
#' @param model Fitted `lm`/`glm` model or a formula.
#' @param data Optional data frame used when `model` is a formula or to
#'   override the data stored in the model object.
#' @param test_results Optional pre-computed diagnostics from
#'   [runHeteroTests()]. If `NULL`, diagnostics are executed automatically.
#' @param alpha Significance threshold used for interpretations.
#' @param include_report Logical; if `TRUE`, a plain-language report is
#'   generated and included in the output.
#'
#' @return An object of class `hetero_recommendation_report` containing the
#'   dataset profile, recommended diagnostics, interpretations, remediation
#'   guidance, assumption warnings, decision tree, and optional narrative.
#' @export
generateHeteroRecommendations <- function(model, data = NULL, test_results = NULL,
                                          alpha = 0.05, include_report = TRUE) {
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula", call. = FALSE)
    checkData(data)
    model <- stats::lm(model, data = data)
  } else {
    checkModel(model)
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      checkData(data)
    }
  }

  validateTestInputs(model, data, test_name = "recommendation_engine", min_obs = 10)

  response <- tryCatch(as.character(stats::formula(model))[2], error = function(e) NA_character_)
  profile <- analyseDatasetCharacteristics(data, response = response)
  recommendations <- suggestDiagnosticsForProfile(profile)

  if (is.null(test_results)) {
    test_results <- runHeteroTests(model, data)
  }

  interpretations <- interpretHeteroTestResults(test_results, alpha = alpha)
  remediation <- recommendRemediationStrategies(test_results, model, data, profile)
  assumption_warnings <- warnDiagnosticAssumptions(profile, interpretations, model, data, test_results)
  decision_tree <- buildDiagnosticDecisionTree(profile, recommendations)
  narrative <- if (isTRUE(include_report)) {
    composeDiagnosticNarrative(profile, interpretations, remediation, assumption_warnings, decision_tree)
  } else {
    NULL
  }

  structure(
    list(
      profile = profile,
      recommendations = recommendations,
      interpretations = interpretations,
      remediation = remediation,
      assumption_warnings = assumption_warnings,
      decision_tree = decision_tree,
      report = narrative
    ),
    class = "hetero_recommendation_report"
  )
}

#' Print method for hetero recommendation reports
#'
#' @param x Object produced by [generateHeteroRecommendations()].
#' @param ... Not used.
#' @export
print.hetero_recommendation_report <- function(x, ...) {
  cat("Heteroscedasticity Recommendation Overview\n")
  cat(rep("-", 50), "\n", sep = "")
  cat(sprintf("Observations: %s | Variables: %s\n", x$profile$n_obs, x$profile$n_vars))
  cat(sprintf("Sample size category: %s\n", x$profile$size_bucket))
  cat(sprintf("Missingness: %s (%.1f%%)\n", x$profile$missingness$level, x$profile$missingness$overall_rate * 100))
  cat("\nKey recommendations:\n")
  if (nrow(x$recommendations) > 0) {
    apply(x$recommendations, 1, function(row) {
      cat(sprintf("- %s: %s\n", row["test"], row["rationale"]))
    })
  }
  cat("\nSummary:\n")
  cat(x$interpretations$summary, "\n")
  cat(x$remediation$summary, "\n")
  if (!is.null(x$report)) {
    cat("\nNarrative excerpt:\n")
    cat(paste(head(x$report, 6), collapse = "\n"), "\n...")
  }
  invisible(x)
}
