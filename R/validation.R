#' Core validation utilities
#'
#' These helpers provide consistent validation of models, data, and
#' missing-value handling across the heteroscedasticity testing framework.
#' They centralize error messaging, reduce duplication, and make the
#' validation behaviour of new tests easier to reason about.
#'
#' @keywords internal
NULL

.fill_message_template <- function(template, ...) {
  args <- list(...)
  if (length(args) == 0) {
    return(template)
  }
  for (name in names(args)) {
    value <- args[[name]]
    if (length(value) > 1) {
      value <- paste(value, collapse = ", ")
    }
    template <- gsub(paste0("{", name, "}"), as.character(value), template, fixed = TRUE)
  }
  template
}

#' Validate regression model inputs
#'
#' Ensures that a supplied regression model satisfies the basic assumptions
#' required by the heteroscedasticity testing infrastructure. The checks
#' include class validation, successful fitting, availability of finite
#' residuals, and minimum sample size requirements.
#'
#' @param model A fitted model object produced by [stats::lm()] or
#'   [stats::glm()].
#' @param test_name A scalar character identifier used in error messages to
#'   reference the calling test.
#' @param min_obs Minimum number of observations required for the calling
#'   procedure. Defaults to `10`.
#' @return Invisibly returns `model` when validation passes.
#' @examples
#' mod <- stats::lm(mpg ~ wt, data = mtcars)
#' rvalidateModelInputs(mod, test_name = "Demo Test")
#' @keywords internal
rvalidateModelInputs <- function(model, test_name, min_obs = 10) {
  if (!inherits(model, c("lm", "glm"))) {
    model_class <- paste(class(model), collapse = "/")
    std_error("invalid_model_class", model_class = model_class)
  }

  if (!is.character(test_name) || length(test_name) != 1L || is.na(test_name) ||
    !nzchar(test_name)) {
    stop("`test_name` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.numeric(min_obs) || length(min_obs) != 1L || is.na(min_obs) ||
    min_obs < 1) {
    stop("`min_obs` must be a positive integer.", call. = FALSE)
  }
  min_obs <- as.integer(min_obs)

  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefs) || length(coefs) == 0L) {
    stop("Model coefficients are not available. Ensure the model was fitted successfully.", call. = FALSE)
  }
  if (!all(is.finite(coefs))) {
    stop("Model coefficients must be finite.", call. = FALSE)
  }

  resid <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(resid) || length(resid) == 0L) {
    stop("Model residuals are not available.", call. = FALSE)
  }
  if (!is.numeric(resid)) {
    stop("Model residuals must be numeric.", call. = FALSE)
  }
  if (anyNA(resid) || any(!is.finite(resid))) {
    stop("Model residuals must be finite.", call. = FALSE)
  }

  n_obs <- length(resid)
  if (n_obs < min_obs) {
    std_error(
      "rinsufficient_sample_size",
      test_name = test_name,
      min_obs = min_obs,
      n_obs = n_obs
    )
  }

  if (inherits(model, "lm")) {
    response <- tryCatch(
      stats::model.response(stats::model.frame(model)),
      error = function(e) NULL
    )
    if (!is.null(response) && is.numeric(response) && length(response) == n_obs) {
      total_var <- sum((response - mean(response))^2)
      if (total_var > 0) {
        r_squared <- 1 - sum(resid^2) / total_var
        if (isTRUE(all.equal(r_squared, 1, tolerance = sqrt(.Machine$double.eps)))) {
          std_error("perfect_fit_detected")
        }
      }
    }
  }

  invisible(model)
}

#' Validate data inputs for heteroscedasticity tests
#'
#' Performs standard checks on input data.frames prior to running
#' heteroscedasticity diagnostics. The routine verifies that required
#' variables are available and that the sample size meets minimum criteria.
#'
#' @param data A data.frame containing the variables required by the test.
#' @param required_vars Optional character vector of column names that must be
#'   present in `data`.
#' @param min_obs Minimum number of observations required. Defaults to `10`.
#' @return Invisibly returns the validated `data` object.
#' @examples
#' rvalidateDataInputs(mtcars, required_vars = c("mpg", "wt"))
#' @keywords internal
rvalidateDataInputs <- function(data, required_vars = NULL, min_obs = 10) {
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  if (!is.numeric(min_obs) || length(min_obs) != 1L || is.na(min_obs) || min_obs < 0) {
    stop("`min_obs` must be a non-negative integer.", call. = FALSE)
  }
  min_obs <- as.integer(min_obs)

  n_obs <- nrow(data)
  if (!is.null(n_obs) && n_obs < min_obs) {
    std_error(
      "rinsufficient_sample_size",
      test_name = "input data",
      min_obs = min_obs,
      n_obs = n_obs
    )
  }

  if (ncol(data) == 0) {
    stop("Data must contain at least one column.", call. = FALSE)
  }

  if (!is.null(required_vars)) {
    required_vars <- unique(as.character(required_vars))
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
      std_error("missing_variable", variable = paste(missing_vars, collapse = ", "))
    }
  }

  if (anyDuplicated(names(data)) > 0) {
    warning("Duplicate column names detected in data; results may be ambiguous.", call. = FALSE)
  }

  invisible(data)
}

#' Handle missing values according to a specified strategy
#'
#' Provides a centralized mechanism for dealing with missing values in
#' heteroscedasticity tests. The function can either drop incomplete cases,
#' emit warnings, or fail fast when missingness is not permitted.
#'
#' @param data A data.frame containing the data to be processed.
#' @param variables Character vector of variable names to inspect for missing
#'   values.
#' @param strategy Strategy describing how missing values should be handled.
#'   The options are:
#'   * `"complete_cases"` – remove incomplete rows and emit a warning that
#'     summarizes the data loss.
#'   * `"warn"` – behaves identically to `"complete_cases"` but is provided as a
#'     semantic alias when a calling test wants to emphasize the warning
#'     behaviour explicitly.
#'   * `"fail"` – abort when any missing values are detected.
#' @return A list with components `data` (the processed data frame),
#'   `removed_cases` (row indices removed), `removed_count` (number of removed
#'   observations), `removed_fraction` (proportion removed relative to the
#'   original data), `removed_variables` (variables with observed missingness),
#'   and `loss_message` (the formatted warning text).
#' @examples
#' cleaned <- rhandleMissingValues(mtcars, c("mpg", "wt"))
#' cleaned$removed_count
#' @keywords internal
rhandleMissingValues <- function(data, variables, strategy = "complete_cases") {
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  if (missing(variables) || is.null(variables) || length(variables) == 0) {
    stop("`variables` must contain at least one column name.", call. = FALSE)
  }
  variables <- as.character(variables)

  missing_vars <- setdiff(variables, names(data))
  if (length(missing_vars) > 0) {
    std_error("missing_variable", variable = paste(missing_vars, collapse = ", "))
  }

  strategy <- match.arg(strategy, c("complete_cases", "fail", "warn"))

  subset <- data[, variables, drop = FALSE]
  complete_mask <- stats::complete.cases(subset)
  missing_idx <- which(!complete_mask)
  n_removed <- length(missing_idx)

  missing_vars_present <- variables[vapply(variables, function(var) any(is.na(data[[var]])), logical(1))]
  if (n_removed == 0) {
    return(list(
      data = data,
      removed_cases = integer(0),
      removed_count = 0L,
      removed_fraction = 0,
      removed_variables = missing_vars_present,
      loss_message = NULL
    ))
  }

  var_names <- if (length(missing_vars_present) > 0) {
    paste(missing_vars_present, collapse = ", ")
  } else {
    paste(variables, collapse = ", ")
  }

  if (strategy == "fail") {
    std_error("missing_values_detected", var_names = var_names, n_removed = n_removed)
  }

  cleaned <- data[complete_mask, , drop = FALSE]
  original_n <- nrow(data)
  removed_fraction <- if (!is.null(original_n) && original_n > 0) {
    n_removed / original_n
  } else {
    NA_real_
  }

  loss_message <- .fill_message_template(
    error_messages[["missing_values_detected"]],
    var_names = var_names,
    n_removed = n_removed
  )

  std_warning("missing_values_removed", var_names = var_names, n_removed = n_removed)

  list(
    data = cleaned,
    removed_cases = missing_idx,
    removed_count = n_removed,
    removed_fraction = removed_fraction,
    removed_variables = missing_vars_present,
    loss_message = loss_message
  )
}

#' Validate distributional assumptions
#'
#' Assesses frequently used distributional assumptions before running
#' heteroscedasticity diagnostics. The checks cover approximate normality,
#' positivity for log-transformed quantities, sufficient variation, and the
#' presence of extreme outliers.
#'
#' @param data A data.frame containing the variables required for assessment.
#' @param assumptions A named list describing the desired checks. Recognised
#'   entries are:
#'   \itemize{
#'     \item `normality` – character vector or list with element `variables`
#'       specifying which columns should satisfy approximate normality. Optional
#'       list elements `alpha` (significance level) and `sample_limit` (maximum
#'       sample size for the Shapiro–Wilk test) can be supplied.
#'     \item `positive` – character vector or list identifying variables that
#'       must contain positive values. When provided as a list, `test_name`
#'       overrides the label used in error messages.
#'     \item `variation` – character vector or list (with `variables` and
#'       optional `tolerance`) describing variables that must exhibit
#'       non-negligible variance.
#'     \item `outliers` – character vector or list of variables to screen for
#'       extreme observations. Lists may include a numeric `threshold`
#'       specifying the acceptable number of robust standard deviations.
#'   }
#' @return A list containing `passed` (logical flag), `messages` (character
#'   vector of violations), `warnings` (character vector of recoverable issues),
#'   and `details` (named list with diagnostic information for each assumption).
#' @examples
#' res <- rvalidateDistributionalAssumptions(
#'   mtcars,
#'   assumptions = list(
#'     normality = list(variables = "mpg"),
#'     positive = list(variables = "disp", test_name = "Demo Test")
#'   )
#' )
#' res$passed
#' @keywords internal
rvalidateDistributionalAssumptions <- function(data, assumptions = list()) {
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  if (is.null(assumptions)) {
    assumptions <- list()
  }

  if (!is.list(assumptions)) {
    stop("`assumptions` must be provided as a list.", call. = FALSE)
  }

  result <- list(
    passed = TRUE,
    messages = character(0),
    warnings = character(0),
    details = list()
  )

  add_violation <- function(type, ...) {
    template <- error_messages[[type]]
    message <- .fill_message_template(template, ...)
    result$messages <<- c(result$messages, message)
    result$passed <<- FALSE
    invisible(message)
  }

  add_warning <- function(message) {
    result$warnings <<- c(result$warnings, message)
    invisible(message)
  }

  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]

  get_variables <- function(config, fallback = NULL, allow_empty = FALSE) {
    if (is.null(config)) {
      vars <- fallback
    } else if (is.character(config)) {
      vars <- config
    } else if (is.list(config)) {
      if (!is.null(config$variables)) {
        vars <- config$variables
      } else if (!is.null(config$vars)) {
        vars <- config$vars
      } else if (!is.null(fallback)) {
        vars <- fallback
      } else {
        vars <- character(0)
      }
    } else if (isTRUE(config) && !is.null(fallback)) {
      vars <- fallback
    } else {
      stop("Assumption configuration must supply variable names as a character vector.", call. = FALSE)
    }
    vars <- unique(as.character(vars))
    vars <- vars[!is.na(vars)]
    if (!allow_empty && length(vars) == 0) {
      stop("At least one variable must be supplied to evaluate the assumption.", call. = FALSE)
    }
    vars
  }

  # Normality checks --------------------------------------------------------
  if (!is.null(assumptions$normality)) {
    normality_cfg <- assumptions$normality
    normality_vars <- tryCatch(
      get_variables(normality_cfg, fallback = numeric_columns, allow_empty = TRUE),
      error = function(e) stop(e$message, call. = FALSE)
    )
    alpha <- if (is.list(normality_cfg) && !is.null(normality_cfg$alpha)) {
      normality_cfg$alpha
    } else {
      0.01
    }
    if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
      stop("Normality check `alpha` must be a number in (0, 1).", call. = FALSE)
    }
    sample_limit <- if (is.list(normality_cfg) && !is.null(normality_cfg$sample_limit)) {
      normality_cfg$sample_limit
    } else {
      5000L
    }
    if (!is.numeric(sample_limit) || length(sample_limit) != 1L || is.na(sample_limit) || sample_limit < 3) {
      stop("Normality check `sample_limit` must be an integer >= 3.", call. = FALSE)
    }
    sample_limit <- as.integer(sample_limit)

    normality_details <- list(alpha = alpha, variables = normality_vars)

    for (var in normality_vars) {
      if (!var %in% names(data)) {
        std_error("missing_variable", variable = var)
      }
      values <- data[[var]]
      if (!is.numeric(values)) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Variable '", var, "' must be numeric to assess normality")
        )
        next
      }
      finite_values <- values[is.finite(values)]
      n <- length(finite_values)
      if (n < 3) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("At least 3 finite observations required to assess normality of ", var)
        )
        normality_details[[var]] <- list(n = n, statistic = NA_real_, p_value = NA_real_)
        next
      }

      sample_values <- if (n > sample_limit) {
        finite_values[sample.int(n, sample_limit)]
      } else {
        finite_values
      }

      shapiro_result <- tryCatch(stats::shapiro.test(sample_values), error = function(e) e)
      if (inherits(shapiro_result, "error")) {
        add_warning(paste0("Failed to compute Shapiro-Wilk test for variable '", var, "': ", shapiro_result$message))
        next
      }
      statistic <- unname(shapiro_result$statistic)
      p_value <- shapiro_result$p.value

      normality_details[[var]] <- list(n = n, statistic = statistic, p_value = p_value)

      if (!is.na(p_value) && p_value < alpha) {
        add_violation("normality_assumption", variable = var)
      }
    }

    result$details$normality <- normality_details
  }

  # Positivity checks -------------------------------------------------------
  if (!is.null(assumptions$positive)) {
    positive_cfg <- assumptions$positive
    positive_vars <- get_variables(positive_cfg)
    positive_details <- list()
    test_label <- if (is.list(positive_cfg) && !is.null(positive_cfg$test_name)) {
      positive_cfg$test_name
    } else {
      "the selected test"
    }

    for (var in positive_vars) {
      if (!var %in% names(data)) {
        std_error("missing_variable", variable = var)
      }
      values <- data[[var]]
      if (!is.numeric(values)) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Variable '", var, "' must be numeric to check for positivity")
        )
        next
      }
      non_positive <- which(values <= 0 & !is.na(values))
      if (length(non_positive) > 0) {
        add_violation("positive_values_required", var_name = var, test_name = test_label)
        positive_details[[var]] <- list(
          offending_rows = non_positive,
          min_value = min(values, na.rm = TRUE)
        )
      }
    }

    if (length(positive_details) > 0) {
      result$details$positive <- positive_details
    }
  }

  # Variation checks --------------------------------------------------------
  if (!is.null(assumptions$variation)) {
    variation_cfg <- assumptions$variation
    variation_vars <- tryCatch(
      get_variables(variation_cfg, fallback = numeric_columns, allow_empty = TRUE),
      error = function(e) stop(e$message, call. = FALSE)
    )
    tolerance <- if (is.list(variation_cfg) && !is.null(variation_cfg$tolerance)) {
      variation_cfg$tolerance
    } else {
      sqrt(.Machine$double.eps)
    }
    if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0) {
      stop("Variation check `tolerance` must be a non-negative number.", call. = FALSE)
    }

    variation_details <- list(tolerance = tolerance, variables = variation_vars)

    for (var in variation_vars) {
      if (!var %in% names(data)) {
        std_error("missing_variable", variable = var)
      }
      values <- data[[var]]
      if (!is.numeric(values)) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Variable '", var, "' must be numeric to evaluate variation")
        )
        next
      }
      finite_values <- values[is.finite(values)]
      if (length(finite_values) < 2) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Insufficient observations to assess variation in ", var)
        )
        variation_details[[var]] <- list(variance = NA_real_, n = length(finite_values))
        next
      }
      var_value <- stats::var(finite_values)
      variation_details[[var]] <- list(variance = var_value, n = length(finite_values))
      if (is.na(var_value) || var_value <= tolerance) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Insufficient variation in variable '", var, "'")
        )
      }
    }

    result$details$variation <- variation_details
  }

  # Outlier checks ---------------------------------------------------------
  if (!is.null(assumptions$outliers)) {
    outlier_cfg <- assumptions$outliers
    outlier_vars <- tryCatch(
      get_variables(outlier_cfg, fallback = numeric_columns, allow_empty = TRUE),
      error = function(e) stop(e$message, call. = FALSE)
    )
    threshold <- if (is.list(outlier_cfg) && !is.null(outlier_cfg$threshold)) {
      outlier_cfg$threshold
    } else {
      4
    }
    if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) || threshold <= 0) {
      stop("Outlier `threshold` must be a positive number.", call. = FALSE)
    }

    outlier_details <- list(threshold = threshold, variables = outlier_vars)

    for (var in outlier_vars) {
      if (!var %in% names(data)) {
        std_error("missing_variable", variable = var)
      }
      values <- data[[var]]
      if (!is.numeric(values)) {
        add_violation(
          "rassumption_violation",
          assumption = paste0("Variable '", var, "' must be numeric to assess outliers")
        )
        next
      }
      finite_idx <- which(is.finite(values))
      if (length(finite_idx) < 3) {
        next
      }
      finite_values <- values[finite_idx]
      center <- stats::median(finite_values)
      mad_raw <- stats::mad(finite_values, center = center, constant = 1, na.rm = TRUE)
      scale <- if (is.finite(mad_raw) && mad_raw > 0) {
        mad_raw * 1.4826
      } else {
        stats::sd(finite_values)
      }
      if (!is.finite(scale) || scale == 0) {
        next
      }
      robust_z <- abs(finite_values - center) / scale
      extreme <- which(robust_z > threshold)
      if (length(extreme) > 0) {
        offending_rows <- finite_idx[extreme]
        add_violation(
          "rassumption_violation",
          assumption = paste0("Extreme outliers detected in variable '", var, "'")
        )
        outlier_details[[var]] <- list(
          rows = offending_rows,
          robust_z = robust_z[extreme]
        )
      }
    }

    if (length(outlier_details) > 0) {
      result$details$outliers <- outlier_details
    }
  }

  result
}

#' Validate grouping variable structure
#'
#' Ensures that grouping variables supplied to heteroscedasticity tests satisfy
#' minimum size and level requirements.
#'
#' @param data Data frame providing the grouping column.
#' @param group_var Name of the grouping variable to inspect.
#' @param min_group_size Minimum number of observations required in each group.
#'   Defaults to `3`.
#' @param min_groups Minimum number of groups that must be represented. Defaults
#'   to `2`.
#' @return A list mirroring the structure of
#'   [rvalidateDistributionalAssumptions()] with information about the evaluated
#'   grouping variable.
#' @examples
#' grp <- rvalidateGroupingVariable(mtcars, group_var = "cyl")
#' grp$details$n_groups
#' @keywords internal
rvalidateGroupingVariable <- function(data, group_var, min_group_size = 3, min_groups = 2) {
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  if (missing(group_var) || length(group_var) != 1L) {
    stop("`group_var` must be a single column name.", call. = FALSE)
  }

  group_var <- as.character(group_var)

  if (!group_var %in% names(data)) {
    std_error("missing_variable", variable = group_var)
  }

  if (!is.numeric(min_group_size) || length(min_group_size) != 1L || is.na(min_group_size) || min_group_size < 1) {
    stop("`min_group_size` must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(min_groups) || length(min_groups) != 1L || is.na(min_groups) || min_groups < 1) {
    stop("`min_groups` must be a positive integer.", call. = FALSE)
  }

  min_group_size <- as.integer(min_group_size)
  min_groups <- as.integer(min_groups)

  result <- list(
    passed = TRUE,
    messages = character(0),
    warnings = character(0),
    details = list(
      group_var = group_var,
      min_group_size = min_group_size,
      min_groups = min_groups
    )
  )

  add_violation <- function(type, ...) {
    template <- error_messages[[type]]
    message <- .fill_message_template(template, ...)
    result$messages <<- c(result$messages, message)
    result$passed <<- FALSE
    invisible(message)
  }

  column <- data[[group_var]]
  if (!(is.factor(column) || is.character(column))) {
    add_violation("invalid_group_variable", group_var = group_var, min_groups = min_groups)
    return(result)
  }

  group_factor <- if (is.factor(column)) {
    droplevels(column)
  } else {
    droplevels(factor(column))
  }

  valid_idx <- !is.na(group_factor)
  group_factor <- droplevels(group_factor[valid_idx])
  counts <- table(group_factor, useNA = "no")

  n_groups <- length(counts)
  result$details$n_groups <- n_groups
  result$details$group_counts <- counts

  if (n_groups < min_groups) {
    add_violation("invalid_group_variable", group_var = group_var, min_groups = min_groups)
  }

  if (n_groups > 0) {
    small_groups <- counts[counts < min_group_size]
    result$details$small_groups <- small_groups
    if (length(small_groups) > 0) {
      for (grp in names(small_groups)) {
        add_violation(
          "insufficient_group_size",
          group_name = grp,
          n_obs = small_groups[[grp]],
          min_required = min_group_size
        )
      }
    }
  } else {
    result$details$small_groups <- integer(0)
  }

  result
}

#' Validate test-specific sample size requirements
#'
#' Evaluates whether the supplied data meet the minimum observation counts
#' required by individual heteroscedasticity diagnostics. Requirements are
#' sourced from [rTEST_REQUIREMENTS] and can encode overall sample sizes,
#' group-level minima, or dynamically computed thresholds (e.g. ARCH LM tests
#' that depend on the lag order).
#'
#' @param test_name Character identifier of the diagnostic test.
#' @param model Optional fitted model object providing residual counts when the
#'   underlying data are unavailable.
#' @param data Optional data frame supplying the observations to be checked.
#' @param groups Optional grouping vector used for per-group requirements. If
#'   omitted, the function attempts to derive the grouping variable from
#'   `...` when `group_var` is supplied.
#' @param ... Additional arguments forwarded to dynamic requirement functions
#'   (for example the number of `lags` in an ARCH LM test). Unused entries are
#'   ignored when computing static requirements.
#' @return A list mirroring other validation helpers containing `passed`,
#'   `messages`, `warnings`, and `details` about the evaluation.
#' @examples
#' data <- data.frame(y = rnorm(25), x = rnorm(25))
#' rvalidateSampleSize("white", data = data)
#'
#' # Group-based requirement
#' grp_data <- data.frame(y = rnorm(15), g = rep(letters[1:3], each = 5))
#' rvalidateSampleSize("levene", data = grp_data, groups = grp_data$g)
#'
#' # Dynamic ARCH LM requirement (lags = 3 implies at least 11 observations)
#' arch_data <- data.frame(y = rnorm(20))
#' rvalidateSampleSize("arch_lm", data = arch_data, lags = 3)
#' @keywords internal
rvalidateSampleSize <- function(test_name, model = NULL, data = NULL, groups = NULL, ...) {
  if (!is.character(test_name) || length(test_name) != 1L || is.na(test_name) || !nzchar(test_name)) {
    stop("`test_name` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data.frame when supplied to sample size validation.", call. = FALSE)
  }

  extra <- list(...)

  test_key <- tolower(trimws(test_name))
  requirement <- rTEST_REQUIREMENTS[[test_key]]

  result <- list(
    passed = TRUE,
    messages = character(0),
    warnings = character(0),
    details = list(test_name = test_name)
  )

  add_violation <- function(message) {
    result$messages <<- c(result$messages, message)
    result$passed <<- FALSE
    invisible(message)
  }

  add_warning <- function(message) {
    result$warnings <<- c(result$warnings, message)
    invisible(message)
  }

  result$details$requirement_key <- test_key

  if (is.null(requirement)) {
    return(result)
  }

  group_vector <- groups
  if (is.null(group_vector) && !is.null(extra$group_var) && !is.null(data) && extra$group_var %in% names(data)) {
    group_vector <- data[[extra$group_var]]
  }

  observed_n <- NA_integer_
  observed_from <- NULL
  if (!is.null(data)) {
    observed_n <- nrow(data)
    observed_from <- "data"
  }
  if ((is.na(observed_n) || observed_n <= 0) && !is.null(model)) {
    resid <- tryCatch(stats::residuals(model), error = function(e) NULL)
    if (!is.null(resid)) {
      observed_n <- length(resid)
      observed_from <- "model"
    }
  }
  if ((is.na(observed_n) || observed_n <= 0) && !is.null(group_vector)) {
    observed_n <- length(group_vector)
    observed_from <- if (is.null(observed_from)) "groups" else observed_from
  }

  result$details$observed_n <- observed_n
  result$details$observed_from <- observed_from

  min_obs <- NULL
  min_obs_per_group <- NULL
  reason <- NULL

  format_reason <- function(text) {
    if (is.null(text) || !nzchar(text)) {
      return(NULL)
    }
    if (grepl("[.!?][[:space:]]*$", text)) {
      paste("Reason:", text)
    } else {
      paste0("Reason: ", text, ".")
    }
  }

  if (is.list(requirement)) {
    if (!is.null(requirement$min_obs)) {
      min_obs <- as.integer(requirement$min_obs)
    }
    if (!is.null(requirement$min_obs_per_group)) {
      min_obs_per_group <- as.integer(requirement$min_obs_per_group)
    }
    if (!is.null(requirement$reason)) {
      reason <- requirement$reason
    }
  } else if (is.function(requirement)) {
    removable <- c(
      "group_var", "min_group_size", "min_groups", "variables",
      "normality_vars", "normality_alpha", "alpha", "suspected_var",
      "assumptions", "threshold"
    )
    extra_args <- extra
    extra_args[intersect(names(extra_args), removable)] <- NULL
    req_formals <- names(formals(requirement))
    if (!is.null(req_formals) && !"..." %in% req_formals) {
      extra_args <- extra_args[intersect(names(extra_args), req_formals)]
    }
    computed <- tryCatch(do.call(requirement, extra_args), error = function(e) {
      stop(
        sprintf("Failed to evaluate sample size requirement for '%s': %s", test_name, e$message),
        call. = FALSE
      )
    })
    if (is.list(computed)) {
      if (!is.null(computed$min_obs)) {
        min_obs <- as.integer(computed$min_obs)
      }
      if (!is.null(computed$reason)) {
        reason <- computed$reason
      }
      if (!is.null(computed$min_obs_per_group)) {
        min_obs_per_group <- as.integer(computed$min_obs_per_group)
      }
    } else {
      min_obs <- as.integer(computed)
    }
    if (is.null(reason) && test_key == "arch_lm" && !is.null(extra$lags)) {
      reason <- sprintf("Lag order %s implies 2 * lags + 5 minimum observations", extra$lags)
    }
  } else {
    stop("Unsupported requirement entry type; must be list or function.", call. = FALSE)
  }

  result$details$min_obs <- min_obs
  result$details$min_obs_per_group <- min_obs_per_group
  result$details$reason <- reason

  reason_suffix <- format_reason(reason)

  if (!is.null(min_obs)) {
    if (is.na(observed_n)) {
      stop(
        sprintf("Unable to determine sample size for '%s'. Provide `data` or a fitted `model`.", test_name),
        call. = FALSE
      )
    }
    if (observed_n < min_obs) {
      base_message <- .fill_message_template(
        error_messages[["rinsufficient_sample_size"]],
        test_name = test_name,
        min_obs = min_obs,
        n_obs = observed_n
      )
      if (!is.null(reason_suffix)) {
        base_message <- paste(base_message, reason_suffix)
      }
      add_violation(base_message)
    }
  }

  if (!is.null(min_obs_per_group)) {
    if (is.null(group_vector)) {
      add_warning(sprintf("Grouping vector not supplied; cannot assess per-group sample sizes for '%s'.", test_name))
    } else {
      group_counts <- table(group_vector, useNA = "no")
      counts_vec <- as.integer(group_counts)
      names(counts_vec) <- names(group_counts)
      result$details$group_counts <- counts_vec
      small_groups <- group_counts[group_counts < min_obs_per_group]
      if (length(small_groups) > 0) {
        for (grp in names(small_groups)) {
          message <- .fill_message_template(
            error_messages[["insufficient_group_size"]],
            group_name = grp,
            n_obs = small_groups[[grp]],
            min_required = min_obs_per_group
          )
          if (!is.null(reason_suffix)) {
            message <- paste(message, reason_suffix)
          }
          add_violation(message)
        }
      }
    }
  }

  result
}

#' Validate test-specific requirements
#'
#' Aggregates validation logic for specific heteroscedasticity diagnostics.
#' The dispatcher applies assumption checks tailored to the supplied `test_name`
#' and returns a summary of any problems detected.
#'
#' @param test_name Name of the heteroscedasticity test whose requirements are
#'   being validated.
#' @param model Fitted model object used by the diagnostic. Required for checks
#'   that depend on model residuals (e.g. bootstrap procedures).
#' @param data Data frame containing the variables required by the test.
#' @param ... Additional arguments that refine the checks for particular tests.
#'   Supported options include:
#'   \itemize{
#'     \item `variables` / `normality_vars` – candidate variables for normality
#'       checks (Bartlett).
#'     \item `suspected_var` – name of the regressor expected to be positive
#'       (Park).
#'     \item `group_var`, `min_group_size`, `min_groups` – grouping requirements
#'       for tests that rely on group comparisons (e.g. Levene).
#'   }
#' @return List with `passed`, `messages`, `warnings`, and `details`
#'   describing the evaluation outcome.
#' @examples
#' bart <- rvalidateTestRequirements(
#'   "Bartlett",
#'   model = stats::lm(mpg ~ wt, data = mtcars),
#'   data = mtcars,
#'   variables = c("mpg", "wt")
#' )
#' bart$passed
#' @keywords internal
rvalidateTestRequirements <- function(test_name, model, data, ...) {
  if (!is.character(test_name) || length(test_name) != 1L || is.na(test_name) || !nzchar(test_name)) {
    stop("`test_name` must be a non-empty character string.", call. = FALSE)
  }

  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  extra <- list(...)

  result <- list(
    passed = TRUE,
    messages = character(0),
    warnings = character(0),
    details = list(test_name = test_name)
  )

  add_violation <- function(type, ...) {
    template <- error_messages[[type]]
    message <- .fill_message_template(template, ...)
    result$messages <<- c(result$messages, message)
    result$passed <<- FALSE
    invisible(message)
  }

  append_result <- function(new_result) {
    if (is.null(new_result)) {
      return(invisible(NULL))
    }
    if (!is.list(new_result) || is.null(new_result$passed)) {
      stop("Combined results must be produced by validation helpers.", call. = FALSE)
    }
    result$passed <<- result$passed && isTRUE(new_result$passed)
    result$messages <<- c(result$messages, new_result$messages)
    result$warnings <<- c(result$warnings, new_result$warnings)
    if (!is.null(new_result$details) && length(new_result$details) > 0) {
      for (nm in names(new_result$details)) {
        result$details[[nm]] <<- new_result$details[[nm]]
      }
    }
    invisible(NULL)
  }

  test_key <- tolower(trimws(test_name))

  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]

  sample_args <- extra
  sample_groups <- NULL
  if (is.null(sample_args$groups) && !is.null(sample_args$group_var) && !is.null(data) && sample_args$group_var %in% names(data)) {
    sample_groups <- data[[sample_args$group_var]]
  } else if (!is.null(sample_args$groups)) {
    sample_groups <- sample_args$groups
  }
  sample_args$groups <- NULL

  append_result(do.call(rvalidateSampleSize, c(
    list(
      test_name = test_name,
      model = model,
      data = data,
      groups = sample_groups
    ),
    sample_args
  )))

  group_test_keys <- c(
    "levene",
    "levene test",
    "brown-forsythe",
    "brown_forsythe",
    "fligner-killeen",
    "fligner killeen",
    "group",
    "bartlett",
    "bartlett's test",
    "bartlett test",
    "hartley_fmax",
    "hartley fmax",
    "hartley's fmax"
  )

  if (test_key %in% group_test_keys) {
    group_var <- extra$group_var
    if (is.null(group_var)) {
      stop("Group-based tests must provide `group_var`.", call. = FALSE)
    }
    min_group_size <- if (!is.null(extra$min_group_size)) extra$min_group_size else 3L
    min_groups <- if (!is.null(extra$min_groups)) extra$min_groups else 2L
    append_result(rvalidateGroupingVariable(
      data,
      group_var = group_var,
      min_group_size = min_group_size,
      min_groups = min_groups
    ))
  }

  if (test_key %in% c("bartlett", "bartlett's test", "bartlett test")) {
    vars <- if (!is.null(extra$variables)) {
      extra$variables
    } else if (!is.null(extra$normality_vars)) {
      extra$normality_vars
    } else {
      numeric_columns
    }
    alpha <- if (!is.null(extra$alpha)) extra$alpha else if (!is.null(extra$normality_alpha)) extra$normality_alpha else NULL
    assumption_cfg <- list(normality = list(variables = vars))
    if (!is.null(alpha)) {
      assumption_cfg$normality$alpha <- alpha
    }
    append_result(rvalidateDistributionalAssumptions(data, assumptions = assumption_cfg))
  } else if (test_key %in% c("park", "park test")) {
    suspected <- if (!is.null(extra$suspected_var)) {
      extra$suspected_var
    } else if (!is.null(extra$variables)) {
      extra$variables
    } else {
      stop("Park test requirements need `suspected_var` or `variables`.", call. = FALSE)
    }
    assumption_cfg <- list(positive = list(variables = suspected, test_name = test_name))
    append_result(rvalidateDistributionalAssumptions(data, assumptions = assumption_cfg))
  } else if (grepl("bootstrap", test_key, fixed = TRUE)) {
    residuals <- tryCatch(stats::residuals(model), error = function(e) NULL)
    if (is.null(residuals)) {
      add_violation(
        "rassumption_violation",
        assumption = "Bootstrap diagnostics require model residuals to be available"
      )
    } else {
      finite_res <- residuals[is.finite(residuals)]
      if (length(finite_res) < 2) {
        add_violation(
          "rassumption_violation",
          assumption = "Bootstrap diagnostics require residual variation"
        )
      } else {
        residual_variance <- stats::var(finite_res)
        result$details$residual_variance <- residual_variance
        if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
          add_violation(
            "rassumption_violation",
            assumption = "Bootstrap diagnostics require residual variance greater than numerical precision"
          )
        }
      }
    }
  } else {
    if (!is.null(extra$assumptions)) {
      append_result(rvalidateDistributionalAssumptions(data, assumptions = extra$assumptions))
    }
  }

  result
}

#' Process validation helper outputs
#'
#' Utility to standardise how validation helper results are handled across the
#' diagnostic implementations. Any accumulated warnings are emitted and
#' violations trigger an error composed of the recorded messages. The original
#' result is invisibly returned for callers that need access to the diagnostic
#' details.
#'
#' @param result A list produced by a validation helper such as
#'   [rvalidateTestRequirements()] or [rvalidateGroupingVariable()].
#' @return Invisibly returns `result` after issuing any warnings or errors.
#' @keywords internal
rprocessValidationResult <- function(result) {
  if (is.null(result)) {
    return(invisible(NULL))
  }

  if (!is.list(result) || is.null(result$passed)) {
    stop("Validation result must be a list with a `passed` element.", call. = FALSE)
  }

  warnings <- unique(result$warnings)
  if (length(warnings) > 0) {
    for (msg in warnings) {
      warning(msg, call. = FALSE)
    }
  }

  if (!isTRUE(result$passed)) {
    messages <- unique(result$messages)
    stop(paste(messages, collapse = "\n"), call. = FALSE)
  }

  invisible(result)
}
