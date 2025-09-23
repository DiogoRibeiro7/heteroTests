#' Standardized error and warning messages
#'
#' Provides a central location for common error and warning strings used
#' across the package. Use `std_error()` and `std_warning()` to signal
#' problems consistently.
#'
#' @name messages
#' @keywords internal
NULL

error_messages <- list(
  invalid_model = "Provide a model fitted with stats::lm() or stats::glm().",
  invalid_data = "Input data must be a data.frame; call as.data.frame() before running the diagnostic.",
  missing_variable = "Variable '{variable}' not found in the supplied data. Check names(data).",
  negative_values = "Test requires positive values in variable '{variable}'.",
  invalid_logical = "'{arg}' must be a single logical value.",
  rinsufficient_sample_size = "Test '{test_name}' requires at least {min_obs} observations, but only {n_obs} were supplied.",
  missing_values_detected = "Missing values detected in {var_names}. {n_removed} observations removed.",
  invalid_model_class = "Expected an lm/glm object but received class {model_class}. Refit the model before proceeding.",
  perfect_fit_detected = "Model has perfect fit (R² = 1); heteroscedasticity tests are unreliable. Consider revising the specification.",
  rassumption_violation = "Test assumption violated: {assumption}. Results may be unreliable.",
  insufficient_group_size = "Group '{group_name}' has {n_obs} observation(s); at least {min_required} are needed.",
  invalid_group_variable = "Grouping variable '{group_var}' must be factor/character with \u2265 {min_groups} levels.",
  positive_values_required = "Variable '{var_name}' must contain only positive values for {test_name}.",
  normality_assumption = "Severe non-normality detected in {variable}. Consider robust alternatives."
)

#' Generate standardized error message
#'
#' @param type Error message type.
#' @param ... Named arguments to replace in the template.
#' @return Stops execution with a formatted message.
#' @export
std_error <- function(type, ...) {
  template <- error_messages[[type]]
  if (is.null(template)) {
    stop("Unknown error type: ", type, call. = FALSE)
  }
  args <- list(...)
  for (name in names(args)) {
    template <- gsub(paste0("{", name, "}"), args[[name]], template, fixed = TRUE)
  }
  stop(template, call. = FALSE)
}

warning_messages <- list(
  cross_products_omitted = "Cross-products omitted due to high dimensionality",
  missing_values_removed = "Removed {n_removed} observations due to missing values in {var_names}"
)

#' Generate standardized warning message
#'
#' @param type Warning message type.
#' @param ... Named arguments for the template.
#' @return Issues a warning with a formatted message.
#' @export
std_warning <- function(type, ...) {
  template <- warning_messages[[type]]
  if (is.null(template)) {
    stop("Unknown warning type: ", type, call. = FALSE)
  }
  args <- list(...)
  for (name in names(args)) {
    template <- gsub(paste0("{", name, "}"), args[[name]], template, fixed = TRUE)
  }
  warning(template, call. = FALSE)
}
