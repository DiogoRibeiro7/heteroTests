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
  invalid_model = "Model must be fitted with lm() or glm()",
  invalid_data = "Data must be a data.frame",
  missing_variable = "Variable '{variable}' not found in data",
  negative_values = "Test requires positive values in variable '{variable}'",
  invalid_logical = "'{arg}' must be a single logical value"
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
  cross_products_omitted = "Cross-products omitted due to high dimensionality"
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
