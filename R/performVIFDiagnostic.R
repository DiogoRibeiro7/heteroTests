#' Compute variance inflation factors
#'
#' Estimates the degree of multicollinearity among the predictors of a linear
#' model. The variance inflation factor for column \eqn{j} of the design matrix is
#' \eqn{1/(1-R_j^2)}, where \eqn{R_j^2} is from regressing that column on the
#' remaining columns; equivalently it is the \eqn{j}-th diagonal element of the
#' inverse correlation matrix of the design. The computation operates on the model
#' matrix, so it works correctly for models containing factors (each contrast
#' column receives its own VIF) and never references names that exist only after
#' contrast expansion.
#'
#' @param model A fitted model of class `lm`.
#' @return A named numeric vector of VIF values, one per design-matrix column
#'   (excluding the intercept). Values near 1 indicate little collinearity; values
#'   above ~5--10 are commonly treated as problematic. Perfectly collinear columns
#'   are reported as `Inf`.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performVIFDiagnostic(m)
performVIFDiagnostic <- function(model) {
  checkModel(model)
  mm <- model.matrix(model)
  assign <- attr(mm, "assign")
  X <- mm[, assign != 0, drop = FALSE]
  predictors <- colnames(X)

  if (ncol(X) == 0L) {
    return(stats::setNames(numeric(0), character(0)))
  }
  if (ncol(X) == 1L) {
    return(stats::setNames(1, predictors))
  }

  # VIF_j is the j-th diagonal of the inverse correlation matrix of the design.
  # Using the correlation matrix avoids rebuilding formulas from contrast-expanded
  # column names (which do not exist in the model frame, breaking factor models)
  # and is numerically equivalent to the auxiliary-regression definition.
  cor_mat <- stats::cor(X)
  vifs <- tryCatch(
    diag(solve(cor_mat)),
    error = function(e) stats::setNames(rep(Inf, ncol(X)), predictors)
  )
  stats::setNames(as.numeric(vifs), predictors)
}
