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
  mf <- stats::model.frame(model)
  response <- stats::model.response(mf)
  design <- stats::model.matrix(model, data = mf)

  fit <- stats::lm.wfit(design, response, w)
  fit$call <- model$call
  fit$terms <- stats::terms(model)
  fit$model <- mf
  fit$xlevels <- model$xlevels
  fit$contrasts <- attr(design, "contrasts")
  fit$na.action <- stats::na.action(model)
  fit$weights <- w
  class(fit) <- class(model)
  fit
}
