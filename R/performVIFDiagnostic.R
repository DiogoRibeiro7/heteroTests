#' Compute variance inflation factors
#'
#' This diagnostic estimates the degree of multicollinearity among predictors in a linear model.
#'
#' @param model A fitted model of class `lm`.
#' @return A named numeric vector of VIF values for each predictor.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performVIFDiagnostic(m)
performVIFDiagnostic <- function(model) {
  checkModel(model)
  mm <- model.matrix(model)
  assign <- attr(mm, "assign")
  predictors <- colnames(mm)[assign != 0]
  vifs <- setNames(numeric(length(predictors)), predictors)
  df <- as.data.frame(model.frame(model))
  for (p in predictors) {
    others <- setdiff(predictors, p)
    if (length(others) == 0) {
      vifs[p] <- 1
    } else {
      f <- as.formula(paste(p, "~", paste(others, collapse = " + ")))
      r2 <- summary(lm(f, data = df))$r.squared
      vifs[p] <- 1 / (1 - r2)
    }
  }
  vifs
}
