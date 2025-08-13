#' Weighted Least Squares wrapper
#'
#' Estimate heteroscedasticity-consistent weights from residuals and refit the model.
#'
#' The weights are computed as the inverse squared residuals from the initial fit.
#'
#' @param model A fitted model of class `lm`.
#' @return A new `lm` object fitted with weights.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' wls <- fitWLS(m)
#' summary(wls)
fitWLS <- function(model) {
  checkModel(model)
  res <- residuals(model)
  w <- 1 / (res^2)
  w[!is.finite(w)] <- max(w[is.finite(w)])
  df <- model.frame(model)
  df$weights <- w
  lm(formula(model), data = df, weights = weights)
}
