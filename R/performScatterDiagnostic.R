#' Scatter-plot diagnostics for heteroscedasticity
#'
#' Computes Spearman correlations between the absolute residuals of a fitted
#' model and selected covariates, mirroring the slopes displayed in spread–level
#' plots.
#'
#' @param model A fitted [stats::lm] object (or compatible object providing
#'   residuals) whose residual dispersion is assessed.
#' @param data Data frame containing the variables referenced by `model` and the
#'   covariates listed in `vars`.
#' @param vars Character vector of column names to correlate with the absolute
#'   residuals. Variables must be present in `data` and coercible to numeric.
#'
#' @return A named numeric vector of Spearman correlation coefficients between
#'   `abs(residuals(model))` and each variable in `vars`.
#'
#' @details
#' The diagnostic reproduces the summary statistic underlying spread–level plots:
#' a strong positive correlation indicates that absolute residuals—and hence the
#' conditional variance—grow with the covariate, while negative correlations point
#' to diminishing variance. Because Spearman's \eqn{\rho} is rank-based, the
#' measure is robust to monotonic transformations of the covariate and provides a
#' quick complement to formal tests.
#'
#' @references
#' Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
#' (3rd ed.). Sage.
#'
#' Cleveland, W. S. (1993). *Visualizing Data*. Hobart Press. Chapter 5 introduces
#' spread–level plots for variance assessment.
#'
#' @seealso
#' [performSpearmanTest()] and [performNCVTest()] provide formal tests based on
#' the same intuition.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
#'
#' # Investigate variance trends in a simulated dataset with increasing spread
#' set.seed(707)
#' x <- runif(120)
#' y <- 1 + 2 * x + rnorm(120, sd = 0.3 + 0.8 * x)
#' sim_df <- data.frame(y, x)
#' fit <- lm(y ~ x, data = sim_df)
#' performScatterDiagnostic(fit, sim_df, "x")
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
