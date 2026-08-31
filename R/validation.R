# Core validation utilities -------------------------------------------------
#
# These helpers provide consistent validation of models, data, and
# missing-value handling across the heteroscedasticity testing framework.
# They centralize error messaging, reduce duplication, and make the
# validation behaviour of new tests easier to reason about.

.validation_cache <- new.env(parent = emptyenv())

.validation_cache_enabled <- function() {
  requireNamespace("digest", quietly = TRUE)
}

.validation_cache_key <- function(scope, ...) {
  if (!.validation_cache_enabled()) {
    return(NULL)
  }
  digest::digest(list(scope = scope, ...), algo = "xxhash64")
}

.validation_cache_get <- function(key) {
  if (is.null(key) || !exists(key, envir = .validation_cache, inherits = FALSE)) {
    return(NULL)
  }
  get(key, envir = .validation_cache, inherits = FALSE)
}

.validation_cache_set <- function(key, value) {
  if (!is.null(key)) {
    assign(key, value, envir = .validation_cache)
  }
  invisible(value)
}

#' Reset cached validation results
#'
#' Clears the internal validation cache so that subsequent checks recompute
#' their diagnostics. This helper is primarily used in automated tests where
#' deterministic behaviour is required.
#'
#' @keywords internal
clearValidationCache <- function() {
  rm(list = ls(envir = .validation_cache), envir = .validation_cache)
  invisible(NULL)
}

new_validation_result <- function(scope, cache_key = NULL) {
  structure(
    list(
      scope = scope,
      cache_key = cache_key,
      passed = TRUE,
      messages = character(),
      warnings = character(),
      details = list()
    ),
    class = c("validation_result", "list")
  )
}

validation_add_error <- function(result, message, detail_name = NULL, detail_value = NULL) {
  result$passed <- FALSE
  result$messages <- c(result$messages, message)
  if (!is.null(detail_name)) {
    result$details[[detail_name]] <- detail_value
  }
  result
}

validation_add_warning <- function(result, message, detail_name = NULL, detail_value = NULL) {
  result$warnings <- c(result$warnings, message)
  if (!is.null(detail_name)) {
    result$details[[detail_name]] <- detail_value
  }
  result
}

validation_add_detail <- function(result, name, value) {
  result$details[[name]] <- value
  result
}

merge_details <- function(base_details, additional_details) {
  if (length(additional_details) == 0) {
    return(base_details)
  }
  for (nm in names(additional_details)) {
    base_details[[nm]] <- additional_details[[nm]]
  }
  base_details
}

validation_merge <- function(result, ...) {
  components <- list(...)
  for (component in components) {
    if (is.null(component)) {
      next
    }
    if (!inherits(component, "validation_result")) {
      stop("All merged components must be validation results.", call. = FALSE)
    }
    result$passed <- result$passed && isTRUE(component$passed)
    result$messages <- c(result$messages, component$messages)
    result$warnings <- c(result$warnings, component$warnings)
    result$details <- merge_details(result$details, component$details)
    if (is.null(result$details$components)) {
      result$details$components <- list()
    }
    result$details$components[[component$scope]] <- component$details
  }
  result
}

enforce_character_scalar <- function(value, arg_name, example = NULL) {
  ok <- is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
  if (ok) {
    return(value)
  }
  hint <- if (!is.null(example)) paste0(" (e.g., ", example, ")") else ""
  stop(sprintf("Argument `%s` must be a single non-empty string%s.", arg_name, hint), call. = FALSE)
}

enforce_integer_scalar <- function(value, arg_name, lower = 0L) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) || value < lower) {
    comparator <- if (lower <= 0L) "non-negative" else sprintf("\u2265 %d", lower)
    stop(sprintf("Argument `%s` must be a %s integer.", arg_name, comparator), call. = FALSE)
  }
  as.integer(value)
}

model_signature <- function(model) {
  list(
    class = class(model),
    coefficients = tryCatch(stats::coef(model), error = function(e) NULL),
    residuals = tryCatch(stats::residuals(model), error = function(e) NULL),
    df_residual = tryCatch(model$df.residual, error = function(e) NA_real_)
  )
}

#' Validate model and data inputs before running diagnostics
#'
#' Provides the compatibility wrapper used by the public testing interface to
#' ensure that fitted-model objects and their associated data satisfy minimal
#' quality requirements. The helper guards against the most common issues that
#' invalidate heteroscedasticity tests and produces actionable error messages
#' that reference the calling diagnostic.
#'
#' @param model A fitted model created by [stats::lm()] or [stats::glm()].
#' @param data A `data.frame` containing the variables used to fit `model`.
#' @param test_name Character scalar naming the diagnostic that is about to run;
#'   included in error messages for clarity.
#' @param min_obs Non-negative integer giving the minimum sample size accepted
#'   by the diagnostic. Defaults to `10`.
#'
#' @return Invisibly returns `TRUE` when validation succeeds. Execution stops
#'   with an informative error when any check fails.
#'
#' @details
#' The routine performs four layers of validation:
#' \enumerate{
#'   \item confirm that `model` inherits from `lm` or `glm` and that its
#'     coefficients and residuals are finite;
#'   \item verify that `data` is a `data.frame` with at least `min_obs` rows;
#'   \item ensure the residual vector is available, finite, and aligned with the
#'     supplied data;
#'   \item emit warnings when studentised residuals exceed five standard
#'     deviations in absolute value or when the dataset is very large (more
#'     than 10,000 observations).
#' }
#' Results are cached (when the **digest** package is installed) so repeated
#' calls with unchanged inputs return immediately.
#'
#' @references
#' Fox, J. (2015). *Applied Regression Analysis and Generalized Linear Models*
#' (3rd ed.). SAGE.
#'
#' Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression Diagnostics:
#' Identifying Influential Data and Sources of Collinearity*. Wiley.
#'
#' @examples
#' data(mtcars)
#' lm_fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
#' validateTestInputs(lm_fit, mtcars, "white")
#'
#' \donttest{
#' noisy <- mtcars
#' noisy$mpg[1] <- 100
#' outlier_fit <- stats::lm(mpg ~ wt + hp, data = noisy)
#' validateTestInputs(outlier_fit, noisy, "white")
#' }
#'
#' @seealso [checkModel()], [checkModelEnhanced()], [rvalidateModelInputs()],
#'   [rvalidateTestRequirements()]
#' @export
validateTestInputs <- function(model, data, test_name, min_obs = 10) {
  test_name <- enforce_character_scalar(test_name, "test_name", example = "\"white\"")
  min_obs <- enforce_integer_scalar(min_obs, "min_obs", lower = 0L)

  base_result <- new_validation_result(sprintf("validate_inputs:%s", test_name))
  data_result <- validate_data_inputs_internal(
    data,
    required_vars = NULL,
    min_obs = min_obs,
    context = sprintf("data supplied to %s", test_name)
  )
  model_result <- validate_model_inputs_internal(model, data, test_name, min_obs)

  combined <- validation_merge(base_result, data_result, model_result)

  if (!combined$passed) {
    messages <- unique(combined$messages)
    bullet_list <- paste(sprintf(" \u2022 %s", messages), collapse = "\n")
    stop(sprintf("Input validation for %s failed:\n%s", test_name, bullet_list), call. = FALSE)
  }

  warnings <- unique(combined$warnings)
  if (length(warnings) > 0) {
    for (msg in warnings) {
      warning(msg, call. = FALSE)
    }
  }

  invisible(TRUE)
}

#' Enhanced model diagnostics
#'
#' Extends [checkModel()] with additional soft checks that flag potential
#' numerical issues before computationally expensive diagnostics are executed.
#' The helper mirrors the warnings used throughout the package and retains the
#' original return semantics for backward compatibility.
#'
#' @param model Fitted model supplied to subsequent diagnostics.
#' @param data Optional `data.frame` used when fitting `model`. When provided it
#'   enables multicollinearity checks via [performVIFDiagnostic()].
#'
#' @return Invisibly returns `model` after issuing any relevant warnings.
#'
#' @details
#' The function warns when residual degrees of freedom fall below six, when a
#' near-perfect fit is detected (R\eqn{^2 > 0.999}), or when variance inflation
#' factors (VIFs) exceed 10. The latter threshold follows the regression
#' diagnostics literature (Belsley et al., 1980).
#'
#' @references
#' Belsley, D. A., Kuh, E., & Welsch, R. E. (1980). *Regression Diagnostics:
#' Identifying Influential Data and Sources of Collinearity*. Wiley.
#'
#' @examples
#' data(mtcars)
#' fit <- stats::lm(mpg ~ wt + hp, data = mtcars)
#' checkModelEnhanced(fit)
#'
#' @seealso [checkModel()], [performVIFDiagnostic()], [validateTestInputs()]
#' @export
checkModelEnhanced <- function(model, data = NULL) {
  checkModel(model)

  df_resid <- tryCatch(model$df.residual, error = function(e) NA_real_)
  if (is.finite(df_resid) && df_resid <= 5) {
    warning(
      sprintf("Very few residual degrees of freedom (%s)", format(df_resid)),
      call. = FALSE
    )
  }

  if (inherits(model, "lm")) {
    r_sq <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)
    if (is.finite(r_sq) && r_sq > 0.999) {
      warning(
        "Near-perfect fit detected - heteroscedasticity tests may be unreliable",
        call. = FALSE
      )
    }
  }

  if (!is.null(data)) {
    vif_vals <- tryCatch(performVIFDiagnostic(model), error = function(e) NULL)
    if (!is.null(vif_vals) && any(vif_vals > 10, na.rm = TRUE)) {
      warning("High multicollinearity detected (VIF > 10)", call. = FALSE)
    }
  }

  invisible(model)
}

validate_model_inputs_internal <- function(model, data, test_name, min_obs) {
  cache_key <- .validation_cache_key(
    "model_inputs",
    test = test_name,
    min_obs = min_obs,
    model = model_signature(model),
    data_rows = if (is.data.frame(data)) nrow(data) else NULL
  )
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  result <- new_validation_result("model_inputs", cache_key)
  result <- validation_add_detail(result, "test_name", test_name)
  result <- validation_add_detail(result, "min_obs", min_obs)

  if (!inherits(model, c("lm", "glm"))) {
    model_class <- paste(class(model), collapse = "/")
    message <- sprintf(
      "Provide an object fitted with stats::lm() or stats::glm() before running %s (received class: %s).",
      test_name,
      model_class
    )
    result <- validation_add_error(result, message, "model_class", model_class)
    return(.validation_cache_set(cache_key, result))
  }

  coefs <- tryCatch(stats::coef(model), error = function(e) NULL)
  if (is.null(coefs) || length(coefs) == 0L) {
    message <- sprintf(
      "Model coefficients are unavailable; refit the model before running %s.",
      test_name
    )
    result <- validation_add_error(result, message)
    return(.validation_cache_set(cache_key, result))
  }
  if (!all(is.finite(coefs))) {
    message <- sprintf(
      "Model coefficients must be finite before running %s. Check for NA or infinite estimates.",
      test_name
    )
    result <- validation_add_error(result, message)
    return(.validation_cache_set(cache_key, result))
  }
  result <- validation_add_detail(result, "coefficients", coefs)

  resid <- tryCatch(stats::residuals(model), error = function(e) NULL)
  if (is.null(resid) || length(resid) == 0L) {
    message <- sprintf("Model residuals are not available for %s.", test_name)
    result <- validation_add_error(result, message)
    return(.validation_cache_set(cache_key, result))
  }
  if (!is.numeric(resid)) {
    result <- validation_add_error(
      result,
      sprintf("Model residuals must be numeric before running %s.", test_name)
    )
    return(.validation_cache_set(cache_key, result))
  }
  if (anyNA(resid) || any(!is.finite(resid))) {
    result <- validation_add_error(
      result,
      sprintf("Model residuals must be finite before running %s.", test_name)
    )
    return(.validation_cache_set(cache_key, result))
  }
  result <- validation_add_detail(result, "residuals", resid)

  n_obs <- length(resid)
  result <- validation_add_detail(result, "n_obs", n_obs)
  if (n_obs < min_obs) {
    message <- sprintf(
      "Only %d observations detected but %s requires at least %d. Provide more data or choose a different diagnostic.",
      n_obs,
      test_name,
      min_obs
    )
    result <- validation_add_error(result, message)
  }

  if (is.data.frame(data)) {
    n_rows <- nrow(data)
    result <- validation_add_detail(result, "data_rows", n_rows)
    if (n_obs < n_rows) {
      message <- sprintf(
        "Model reports %d residuals but the supplied data has %d rows. Refit the model with the provided data before running %s.",
        n_obs,
        n_rows,
        test_name
      )
      result <- validation_add_error(result, message)
    }
    if (!is.null(n_rows) && n_rows > 10000) {
      warn <- sprintf(
        "Large dataset detected (%d rows); some diagnostics for %s may take longer to compute.",
        n_rows,
        test_name
      )
      result <- validation_add_warning(result, warn)
    }
  }

  resid_var <- stats::var(resid, na.rm = TRUE)
  result <- validation_add_detail(result, "residual_variance", resid_var)
  if (is.na(resid_var) || resid_var <= .Machine$double.eps) {
    message <- sprintf(
      "Residual variance is too small to evaluate %s. Introduce variability or reconsider the model specification.",
      test_name
    )
    result <- validation_add_error(result, message)
  }

  scaled <- suppressWarnings(scale(resid))
  scaled_vec <- as.numeric(scaled)
  extreme <- which(is.finite(scaled_vec) & abs(scaled_vec) > 5)
  if (length(extreme) > 0) {
    preview <- paste(utils::head(extreme, 5L), collapse = ", ")
    if (length(extreme) > 5L) {
      preview <- paste0(preview, sprintf(", ... (+%d)", length(extreme) - 5L))
    }
    warn <- sprintf(
      "Residual outliers detected at rows %s (|z| > 5). Inspect leverage before running %s.",
      preview,
      test_name
    )
    result <- validation_add_warning(result, warn, "extreme_residuals", list(rows = extreme, threshold = 5))
  }

  if (inherits(model, "lm")) {
    r_sq <- tryCatch(summary(model)$r.squared, error = function(e) NA_real_)
    if (isTRUE(is.finite(r_sq) && r_sq > 0.999)) {
      message <- sprintf(
        "Model appears perfectly explained (R\u00b2 = %.3f). Heteroscedasticity diagnostics are unreliable for a perfect fit.",
        r_sq
      )
      result <- validation_add_error(result, message, "r_squared", r_sq)
    } else {
      result <- validation_add_detail(result, "r_squared", r_sq)
    }
  }

  .validation_cache_set(cache_key, result)
}

validate_data_inputs_internal <- function(data, required_vars, min_obs, context) {
  data_is_df <- is.data.frame(data)
  cache_key <- if (data_is_df) {
    .validation_cache_key(
      "data_inputs",
      context = context,
      min_obs = min_obs,
      columns = names(data),
      n_rows = nrow(data),
      required = sort(unique(as.character(required_vars)))
    )
  } else {
    NULL
  }
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  result <- new_validation_result("data_inputs", cache_key)
  result <- validation_add_detail(result, "context", context)
  result <- validation_add_detail(result, "min_obs", min_obs)
  if (!is.null(required_vars)) {
    result <- validation_add_detail(result, "required_vars", unique(as.character(required_vars)))
  }

  if (!data_is_df) {
    message <- sprintf(
      "Expected a data.frame for %s; coerce your object with as.data.frame() before running diagnostics.",
      context
    )
    result <- validation_add_error(result, message)
    return(.validation_cache_set(cache_key, result))
  }

  if (ncol(data) == 0) {
    message <- sprintf(
      "The supplied %s contains no columns. Provide the predictors and response used to fit the model.",
      context
    )
    result <- validation_add_error(result, message)
    return(.validation_cache_set(cache_key, result))
  }

  n_obs <- nrow(data)
  result <- validation_add_detail(result, "n_obs", n_obs)
  if (!is.null(n_obs) && n_obs < min_obs) {
    message <- sprintf(
      "%s contains only %d observations but at least %d are required. Add more rows or reduce the minimum threshold.",
      tools::toTitleCase(context),
      n_obs,
      min_obs
    )
    result <- validation_add_error(result, message)
  }

  if (!is.null(required_vars) && length(required_vars) > 0) {
    required_vars <- unique(as.character(required_vars))
    missing_vars <- setdiff(required_vars, names(data))
    if (length(missing_vars) > 0) {
      message <- sprintf(
        "The following variables are missing from %s: %s. Verify column names with names(data).",
        context,
        paste(missing_vars, collapse = ", ")
      )
      result <- validation_add_error(result, message, "missing_vars", missing_vars)
    }
  }

  dup_names <- unique(names(data)[duplicated(names(data))])
  if (length(dup_names) > 0) {
    warn <- sprintf(
      "Duplicate column names detected (%s); results may be ambiguous.",
      paste(dup_names, collapse = ", ")
    )
    result <- validation_add_warning(result, warn, "duplicate_columns", dup_names)
  }

  .validation_cache_set(cache_key, result)
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
#' heteroTests:::rvalidateModelInputs(mod, test_name = "Demo Test")
#' @keywords internal
rvalidateModelInputs <- function(model, test_name, min_obs = 10) {
  test_name <- enforce_character_scalar(test_name, "test_name", example = "\"white\"")
  min_obs <- enforce_integer_scalar(min_obs, "min_obs", lower = 1L)

  result <- validate_model_inputs_internal(model, NULL, test_name, min_obs)
  rprocessValidationResult(result)
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
#' heteroTests:::rvalidateDataInputs(mtcars, required_vars = c("mpg", "wt"))
#' @keywords internal
rvalidateDataInputs <- function(data, required_vars = NULL, min_obs = 10) {
  min_obs <- enforce_integer_scalar(min_obs, "min_obs", lower = 0L)
  result <- validate_data_inputs_internal(data, required_vars, min_obs, context = "input data")
  rprocessValidationResult(result)
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
#'   * `"complete_cases"` - remove incomplete rows and emit a warning that
#'     summarizes the data loss.
#'   * `"warn"` - behaves identically to `"complete_cases"` but is provided as a
#'     semantic alias when a calling test wants to emphasize the warning
#'     behaviour explicitly.
#'   * `"fail"` - abort when any missing values are detected.
#' @return A list with components `data` (the processed data frame),
#'   `removed_cases` (row indices removed), `removed_count` (number of removed
#'   observations), `removed_fraction` (proportion removed relative to the
#'   original data), `removed_variables` (variables with observed missingness),
#'   and `loss_message` (the formatted warning text).
#' @examples
#' cleaned <- heteroTests:::rhandleMissingValues(mtcars, c("mpg", "wt"))
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
resolve_variable_config <- function(config, fallback = NULL, allow_empty = FALSE) {
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

check_normality_assumption <- function(data, config, numeric_columns) {
  normality_vars <- tryCatch(
    resolve_variable_config(config, fallback = numeric_columns, allow_empty = TRUE),
    error = function(e) stop(e$message, call. = FALSE)
  )
  alpha <- if (is.list(config) && !is.null(config$alpha)) config$alpha else 0.01
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("Normality check `alpha` must be a number in (0, 1).", call. = FALSE)
  }
  sample_limit <- if (is.list(config) && !is.null(config$sample_limit)) config$sample_limit else 5000L
  if (!is.numeric(sample_limit) || length(sample_limit) != 1L || is.na(sample_limit) || sample_limit < 3) {
    stop("Normality check `sample_limit` must be an integer >= 3.", call. = FALSE)
  }
  sample_limit <- as.integer(sample_limit)

  detail <- list(alpha = alpha, variables = normality_vars)
  result <- new_validation_result("assumption_normality")

  for (var in normality_vars) {
    if (!var %in% names(data)) {
      msg <- sprintf("Variable '%s' is not available to assess normality.", var)
      result <- validation_add_error(result, msg)
      next
    }
    values <- data[[var]]
    if (!is.numeric(values)) {
      result <- validation_add_error(
        result,
        sprintf("Variable '%s' must be numeric to assess normality.", var)
      )
      next
    }
    finite_values <- values[is.finite(values)]
    n <- length(finite_values)
    if (n < 3) {
      msg <- sprintf("At least 3 finite observations are required to assess normality of '%s'.", var)
      result <- validation_add_error(result, msg)
      detail[[var]] <- list(n = n, statistic = NA_real_, p_value = NA_real_)
      next
    }
    sample_values <- if (n > sample_limit) finite_values[sample.int(n, sample_limit)] else finite_values
    shapiro_result <- tryCatch(stats::shapiro.test(sample_values), error = function(e) e)
    if (inherits(shapiro_result, "error")) {
      warn <- sprintf("Failed to compute Shapiro-Wilk test for '%s': %s", var, shapiro_result$message)
      result <- validation_add_warning(result, warn)
      next
    }
    statistic <- unname(shapiro_result$statistic)
    p_value <- shapiro_result$p.value
    detail[[var]] <- list(n = n, statistic = statistic, p_value = p_value)
    if (!is.na(p_value) && p_value < alpha) {
      msg <- sprintf("Severe non-normality detected in '%s' (p = %.3f).", var, p_value)
      result <- validation_add_error(result, msg)
    }
  }

  result$details$normality <- detail
  result
}

check_positivity_assumption <- function(data, config) {
  positive_vars <- resolve_variable_config(config)
  detail <- list()
  test_label <- if (is.list(config) && !is.null(config$test_name)) config$test_name else "the selected test"
  result <- new_validation_result("assumption_positive")

  for (var in positive_vars) {
    if (!var %in% names(data)) {
      msg <- sprintf("Variable '%s' is not available to verify positivity.", var)
      result <- validation_add_error(result, msg)
      next
    }
    values <- data[[var]]
    if (!is.numeric(values)) {
      msg <- sprintf("Variable '%s' must be numeric to check for positivity.", var)
      result <- validation_add_error(result, msg)
      next
    }
    non_positive <- which(values <= 0 & !is.na(values))
    if (length(non_positive) > 0) {
      msg <- sprintf(
        "Variable '%s' contains %d non-positive value(s); %s requires strictly positive data.",
        var,
        length(non_positive),
        test_label
      )
      detail[[var]] <- list(offending_rows = non_positive, min_value = min(values, na.rm = TRUE))
      result <- validation_add_error(result, msg)
    }
  }

  if (length(detail) > 0) {
    result$details$positive <- detail
  }
  result
}

check_variation_assumption <- function(data, config, numeric_columns) {
  variation_vars <- tryCatch(
    resolve_variable_config(config, fallback = numeric_columns, allow_empty = TRUE),
    error = function(e) stop(e$message, call. = FALSE)
  )
  tolerance <- if (is.list(config) && !is.null(config$tolerance)) config$tolerance else sqrt(.Machine$double.eps)
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || tolerance < 0) {
    stop("Variation check `tolerance` must be a non-negative number.", call. = FALSE)
  }

  detail <- list(tolerance = tolerance, variables = variation_vars)
  result <- new_validation_result("assumption_variation")

  for (var in variation_vars) {
    if (!var %in% names(data)) {
      msg <- sprintf("Variable '%s' is not available to assess variation.", var)
      result <- validation_add_error(result, msg)
      next
    }
    values <- data[[var]]
    if (!is.numeric(values)) {
      msg <- sprintf("Variable '%s' must be numeric to evaluate variation.", var)
      result <- validation_add_error(result, msg)
      next
    }
    finite_values <- values[is.finite(values)]
    if (length(finite_values) < 2) {
      msg <- sprintf("Insufficient observations to assess variation in '%s'.", var)
      result <- validation_add_error(result, msg)
      detail[[var]] <- list(variance = NA_real_, n = length(finite_values))
      next
    }
    var_value <- stats::var(finite_values)
    detail[[var]] <- list(variance = var_value, n = length(finite_values))
    if (is.na(var_value) || var_value <= tolerance) {
      msg <- sprintf("Variable '%s' exhibits near-zero variance (%.3g).", var, var_value)
      result <- validation_add_error(result, msg)
    }
  }

  result$details$variation <- detail
  result
}

check_outlier_assumption <- function(data, config, numeric_columns) {
  outlier_vars <- tryCatch(
    resolve_variable_config(config, fallback = numeric_columns, allow_empty = TRUE),
    error = function(e) stop(e$message, call. = FALSE)
  )
  threshold <- if (is.list(config) && !is.null(config$threshold)) config$threshold else 4
  if (!is.numeric(threshold) || length(threshold) != 1L || is.na(threshold) || threshold <= 0) {
    stop("Outlier `threshold` must be a positive number.", call. = FALSE)
  }

  detail <- list(threshold = threshold, variables = outlier_vars)
  result <- new_validation_result("assumption_outliers")

  for (var in outlier_vars) {
    if (!var %in% names(data)) {
      msg <- sprintf("Variable '%s' is not available to screen for outliers.", var)
      result <- validation_add_error(result, msg)
      next
    }
    values <- data[[var]]
    if (!is.numeric(values)) {
      msg <- sprintf("Variable '%s' must be numeric to assess outliers.", var)
      result <- validation_add_error(result, msg)
      next
    }
    finite_idx <- which(is.finite(values))
    if (length(finite_idx) < 3) {
      next
    }
    finite_values <- values[finite_idx]
    center <- stats::median(finite_values)
    mad_raw <- stats::mad(finite_values, center = center, constant = 1, na.rm = TRUE)
    scale <- if (is.finite(mad_raw) && mad_raw > 0) mad_raw * 1.4826 else stats::sd(finite_values)
    if (!is.finite(scale) || scale == 0) {
      next
    }
    robust_z <- abs(finite_values - center) / scale
    extreme <- which(robust_z > threshold)
    if (length(extreme) > 0) {
      offending_rows <- finite_idx[extreme]
      msg <- sprintf("Extreme outliers detected in '%s' (threshold %.1f).", var, threshold)
      result <- validation_add_error(result, msg)
      detail[[var]] <- list(rows = offending_rows, robust_z = robust_z[extreme])
    }
  }

  if (length(detail) > 0) {
    result$details$outliers <- detail
  }
  result
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
#'     \item `normality` - character vector or list with element `variables`
#'       specifying which columns should satisfy approximate normality. Optional
#'       list elements `alpha` (significance level) and `sample_limit` (maximum
#'       sample size for the Shapiro-Wilk test) can be supplied.
#'     \item `positive` - character vector or list identifying variables that
#'       must contain positive values. When provided as a list, `test_name`
#'       overrides the label used in error messages.
#'     \item `variation` - character vector or list (with `variables` and
#'       optional `tolerance`) describing variables that must exhibit
#'       non-negligible variance.
#'     \item `outliers` - character vector or list of variables to screen for
#'       extreme observations. Lists may include a numeric `threshold`
#'       specifying the acceptable number of robust standard deviations.
#'   }
#' @return A list containing `passed` (logical flag), `messages` (character
#'   vector of violations), `warnings` (character vector of recoverable issues),
#'   and `details` (named list with diagnostic information for each assumption).
#' @examples
#' res <- heteroTests:::rvalidateDistributionalAssumptions(
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

  cache_key <- .validation_cache_key(
    "distributional_assumptions",
    assumptions = assumptions,
    columns = names(data),
    n_rows = nrow(data),
    data_snapshot = data
  )
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]
  result <- new_validation_result("distributional_assumptions", cache_key)

  if (!is.null(assumptions$normality)) {
    normality_result <- check_normality_assumption(data, assumptions$normality, numeric_columns)
    result <- validation_merge(result, normality_result)
  }

  if (!is.null(assumptions$positive)) {
    positive_result <- check_positivity_assumption(data, assumptions$positive)
    result <- validation_merge(result, positive_result)
  }

  if (!is.null(assumptions$variation)) {
    variation_result <- check_variation_assumption(data, assumptions$variation, numeric_columns)
    result <- validation_merge(result, variation_result)
  }

  if (!is.null(assumptions$outliers)) {
    outlier_result <- check_outlier_assumption(data, assumptions$outliers, numeric_columns)
    result <- validation_merge(result, outlier_result)
  }

  .validation_cache_set(cache_key, result)
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
#' grp <- heteroTests:::rvalidateGroupingVariable(mtcars, group_var = "cyl")
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

  min_group_size <- enforce_integer_scalar(min_group_size, "min_group_size", lower = 1L)
  min_groups <- enforce_integer_scalar(min_groups, "min_groups", lower = 1L)

  cache_key <- .validation_cache_key(
    "grouping_variable",
    column = group_var,
    min_group_size = min_group_size,
    min_groups = min_groups,
    data_snapshot = data[[group_var]]
  )
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  result <- new_validation_result("grouping_variable", cache_key)
  result <- validation_add_detail(result, "group_var", group_var)
  result <- validation_add_detail(result, "min_group_size", min_group_size)
  result <- validation_add_detail(result, "min_groups", min_groups)

  column <- data[[group_var]]
  if (!(is.factor(column) || is.character(column))) {
    msg <- sprintf(
      "Grouping variable '%s' must be a factor or character vector with at least %d levels.",
      group_var,
      min_groups
    )
    result <- validation_add_error(result, msg)
    return(.validation_cache_set(cache_key, result))
  }

  group_factor <- if (is.factor(column)) droplevels(column) else droplevels(factor(column))
  valid_idx <- !is.na(group_factor)
  group_factor <- droplevels(group_factor[valid_idx])
  counts <- table(group_factor, useNA = "no")

  n_groups <- length(counts)
  result <- validation_add_detail(result, "n_groups", n_groups)
  result <- validation_add_detail(result, "group_counts", counts)

  if (n_groups < min_groups) {
    msg <- sprintf(
      "Grouping variable '%s' has only %d level(s); at least %d are required.",
      group_var,
      n_groups,
      min_groups
    )
    result <- validation_add_error(result, msg)
  }

  if (n_groups > 0) {
    small_groups <- counts[counts < min_group_size]
    result <- validation_add_detail(result, "small_groups", small_groups)
    if (length(small_groups) > 0) {
      for (grp in names(small_groups)) {
        msg <- sprintf(
          "Group '%s' has %d observation(s); increase to %d for stable inference.",
          grp,
          small_groups[[grp]],
          min_group_size
        )
        result <- validation_add_error(result, msg)
      }
    }
  } else {
    result <- validation_add_detail(result, "small_groups", integer(0))
  }

  .validation_cache_set(cache_key, result)
}
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

#' Validate test-specific sample size requirements
#'
#' Evaluates whether the supplied data meet the minimum observation counts
#' required by individual heteroscedasticity diagnostics.
#'
#' @inheritParams performWhiteTest
#' @inheritParams rvalidateTestRequirements
#' @param groups Optional grouping vector used for per-group requirements. If
#'   omitted, the function attempts to derive the grouping variable from
#'   `...` when `group_var` is supplied.
#' @param ... Additional arguments forwarded to dynamic requirement functions
#'   (for example the number of `lags` in an ARCH LM test). Unused entries are
#'   ignored when computing static requirements.
#' @return A validation result describing whether the sample-size checks passed.
#' @keywords internal
rvalidateSampleSize <- function(test_name, model = NULL, data = NULL, groups = NULL, ...) {
  test_name <- enforce_character_scalar(test_name, "test_name", example = "\"white\"")
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be a data.frame when supplied to sample size validation.", call. = FALSE)
  }

  extra <- list(...)
  test_key <- tolower(trimws(test_name))
  requirement <- rTEST_REQUIREMENTS[[test_key]]

  cache_key <- .validation_cache_key(
    "sample_size",
    test = test_key,
    model = model_signature(model),
    data = if (!is.null(data)) data else NULL,
    groups = if (!is.null(groups)) groups else NULL,
    extra = extra
  )
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  result <- new_validation_result("sample_size", cache_key)
  result <- validation_add_detail(result, "test_name", test_name)
  result <- validation_add_detail(result, "requirement_key", test_key)

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
    if (is.null(observed_from)) {
      observed_from <- "groups"
    }
  }
  result <- validation_add_detail(result, "observed_n", observed_n)
  result <- validation_add_detail(result, "observed_from", observed_from)

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
  } else if (!is.null(requirement)) {
    stop("Unsupported requirement entry type; must be list or function.", call. = FALSE)
  }

  result <- validation_add_detail(result, "min_obs", min_obs)
  result <- validation_add_detail(result, "min_obs_per_group", min_obs_per_group)
  result <- validation_add_detail(result, "reason", reason)

  reason_suffix <- format_reason(reason)

  if (!is.null(min_obs)) {
    if (is.na(observed_n)) {
      stop(
        sprintf("Unable to determine sample size for '%s'. Provide `data` or a fitted `model`.", test_name),
        call. = FALSE
      )
    }
    if (observed_n < min_obs) {
      message <- sprintf(
        "%s requires at least %d observations but only %d were supplied.",
        test_name,
        min_obs,
        observed_n
      )
      if (!is.null(reason_suffix)) {
        message <- paste(message, reason_suffix)
      }
      result <- validation_add_error(result, message)
    }
  }

  if (!is.null(min_obs_per_group)) {
    if (is.null(group_vector)) {
      warn <- sprintf(
        "Grouping vector not supplied; cannot assess per-group sample sizes for '%s'.",
        test_name
      )
      result <- validation_add_warning(result, warn)
    } else {
      group_counts <- table(group_vector, useNA = "no")
      counts_vec <- as.integer(group_counts)
      names(counts_vec) <- names(group_counts)
      result <- validation_add_detail(result, "group_counts", counts_vec)
      small_groups <- group_counts[group_counts < min_obs_per_group]
      if (length(small_groups) > 0) {
        for (grp in names(small_groups)) {
          message <- sprintf(
            "Group '%s' has %d observation(s); at least %d are required.",
            grp,
            small_groups[[grp]],
            min_obs_per_group
          )
          if (!is.null(reason_suffix)) {
            message <- paste(message, reason_suffix)
          }
          result <- validation_add_error(result, message)
        }
      }
    }
  }

  .validation_cache_set(cache_key, result)
}

#' Validate test-specific requirements
#'
#' Aggregates validation logic tailored to the supplied `test_name` and returns
#' a summary of any problems detected.
#'
#' @param test_name Name of the heteroscedasticity test whose requirements are
#'   being validated.
#' @param model Fitted model object used by the diagnostic. Required for checks
#'   that depend on model residuals (e.g. bootstrap procedures).
#' @param data Data frame containing the variables required by the test.
#' @param ... Additional arguments that refine the checks for particular tests.
#' @return Validation result combining all relevant requirements.
#' @keywords internal
rvalidateTestRequirements <- function(test_name, model, data, ...) {
  test_name <- enforce_character_scalar(test_name, "test_name", example = "\"Bartlett\"")
  if (!is.data.frame(data)) {
    std_error("invalid_data")
  }

  extra <- list(...)
  cache_key <- .validation_cache_key(
    "test_requirements",
    test = tolower(trimws(test_name)),
    model = model_signature(model),
    data = data,
    extra = extra
  )
  cached <- .validation_cache_get(cache_key)
  if (!is.null(cached)) {
    return(cached)
  }

  result <- new_validation_result("test_requirements", cache_key)
  result <- validation_add_detail(result, "test_name", test_name)

  sample_args <- extra
  sample_groups <- NULL
  if (is.null(sample_args$groups) && !is.null(sample_args$group_var) && sample_args$group_var %in% names(data)) {
    sample_groups <- data[[sample_args$group_var]]
  } else if (!is.null(sample_args$groups)) {
    sample_groups <- sample_args$groups
  }
  sample_args$groups <- NULL

  sample_result <- do.call(rvalidateSampleSize, c(
    list(
      test_name = test_name,
      model = model,
      data = data,
      groups = sample_groups
    ),
    sample_args
  ))
  result <- validation_merge(result, sample_result)

  test_key <- tolower(trimws(test_name))
  numeric_columns <- names(data)[vapply(data, is.numeric, logical(1))]

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
    grouping_result <- rvalidateGroupingVariable(
      data,
      group_var = group_var,
      min_group_size = min_group_size,
      min_groups = min_groups
    )
    result <- validation_merge(result, grouping_result)
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
    assumptions_result <- rvalidateDistributionalAssumptions(data, assumptions = assumption_cfg)
    result <- validation_merge(result, assumptions_result)
  } else if (test_key %in% c("park", "park test")) {
    suspected <- if (!is.null(extra$suspected_var)) {
      extra$suspected_var
    } else if (!is.null(extra$variables)) {
      extra$variables
    } else {
      stop("Park test requirements need `suspected_var` or `variables`.", call. = FALSE)
    }
    assumption_cfg <- list(positive = list(variables = suspected, test_name = test_name))
    assumptions_result <- rvalidateDistributionalAssumptions(data, assumptions = assumption_cfg)
    result <- validation_merge(result, assumptions_result)
  } else if (grepl("bootstrap", test_key, fixed = TRUE)) {
    residuals <- tryCatch(stats::residuals(model), error = function(e) NULL)
    if (is.null(residuals)) {
      result <- validation_add_error(
        result,
        "Bootstrap diagnostics require model residuals to be available."
      )
    } else {
      finite_res <- residuals[is.finite(residuals)]
      if (length(finite_res) < 2) {
        result <- validation_add_error(
          result,
          "Bootstrap diagnostics require residual variation."
        )
      } else {
        residual_variance <- stats::var(finite_res)
        result <- validation_add_detail(result, "residual_variance", residual_variance)
        if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
          result <- validation_add_error(
            result,
            "Bootstrap diagnostics require residual variance greater than numerical precision."
          )
        }
      }
    }
  } else if (!is.null(extra$assumptions)) {
    assumptions_result <- rvalidateDistributionalAssumptions(data, assumptions = extra$assumptions)
    result <- validation_merge(result, assumptions_result)
  }

  .validation_cache_set(cache_key, result)
}

#' Process validation helper outputs
#'
#' Standardises how validation helper results are handled across the diagnostic
#' implementations.
#'
#' @param result A validation result (or compatible list) produced by the helper
#'   routines.
#' @return Invisibly returns `result` after issuing warnings or errors.
#' @keywords internal
rprocessValidationResult <- function(result) {
  if (is.null(result)) {
    return(invisible(NULL))
  }

  if (inherits(result, "validation_result")) {
    validated <- result
  } else if (is.list(result) && !is.null(result$passed)) {
    class(result) <- unique(c("validation_result", class(result)))
    validated <- result
  } else {
    stop("Validation result must be a list with a `passed` element.", call. = FALSE)
  }

  warnings <- unique(validated$warnings)
  if (length(warnings) > 0) {
    for (msg in warnings) {
      warning(msg, call. = FALSE)
    }
  }

  if (!isTRUE(validated$passed)) {
    messages <- unique(validated$messages)
    stop(paste(messages, collapse = "\n"), call. = FALSE)
  }

  invisible(validated)
}
