#' Scatter-plot diagnostics for heteroscedasticity
#'
#' Computes correlations between absolute residuals and specified variables
#' as a simple diagnostic for heteroscedastic patterns.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param vars Character vector of variable names to check.
#'
#' @return A named numeric vector of correlations.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
performScatterDiagnostic <- function(model, data, vars) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }

  res <- abs(residuals(model))
  sapply(vars, function(v) cor(res, data[[v]], method = "spearman"))
}
