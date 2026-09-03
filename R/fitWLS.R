#' Weighted Least Squares wrapper
#'
#' Refit a model by feasible generalised least squares, weighting each
#' observation by the inverse of an estimated error variance.
#'
#' The variance is *modelled*, not read off the residuals directly. A single
#' squared residual is a one-degree-of-freedom estimate of \eqn{\sigma_i^2} and
#' far too noisy to invert: weighting by \eqn{1/e_i^2} hands almost all of the
#' weight to whichever observations the initial fit happened to reproduce most
#' closely. This function instead regresses \eqn{\log e_i^2} on the model's own
#' design matrix and takes \eqn{\hat\sigma_i^2 = \exp(\hat g_i)} from the
#' fitted values, the standard feasible-GLS recipe; weights are
#' \eqn{1/\hat\sigma_i^2}.
#'
#' The log scale keeps the fitted variances positive without constraining the
#' auxiliary regression, and residuals that are numerically zero are floored
#' before the logarithm, with a warning.
#'
#' Weights estimated this way are consistent under a correctly specified
#' variance model, so standard errors from the returned fit are usable. They
#' were not before 0.9.0, when the weights were the raw inverse squared
#' residuals: the weighted residual sum of squares then collapsed towards
#' \eqn{n} regardless of the data, and nominal 95% intervals covered the truth
#' about 10% of the time.
#'
#' If the variance model cannot be fitted, or yields no usable variation, the
#' function falls back to equal weights, which reduces the result to the
#' original OLS fit.
#'
#' @param model A fitted model of class `lm`.
#' @return A new `lm` object fitted with weights. The estimated variances are
#'   attached as the `"variance_model"` attribute.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' wls <- fitWLS(m)
#' summary(wls)
fitWLS <- function(model) {
  checkModel(model)
  mf <- stats::model.frame(model)
  response <- stats::model.response(mf)
  design <- stats::model.matrix(model, data = mf)

  res <- residuals(model)
  w <- rfgls_weights(res, design)

  fit <- stats::lm.wfit(design, response, w)
  fit$call <- model$call
  fit$terms <- stats::terms(model)
  fit$model <- mf
  fit$xlevels <- model$xlevels
  fit$contrasts <- attr(design, "contrasts")
  fit$na.action <- stats::na.action(model)
  fit$weights <- w
  class(fit) <- class(model)
  attr(fit, "variance_model") <- 1 / w
  fit
}

#' Feasible-GLS weights from a log-variance model
#'
#' Regresses the logged squared residuals on the design matrix and returns
#' `1 / exp(fitted)`. Falls back to equal weights when the auxiliary fit is
#' unusable, so the caller degrades to OLS rather than failing.
#'
#' @param res Residuals from the initial fit.
#' @param design Model matrix of the initial fit.
#' @return Numeric vector of weights, one per observation.
#' @keywords internal
#' @noRd
rfgls_weights <- function(res, design) {
  n <- length(res)
  equal <- rep(1, n)

  log_e2 <- tryCatch(
    rlog_squared_residuals(res, "fitWLS"),
    error = function(e) NULL
  )
  if (is.null(log_e2) || !all(is.finite(log_e2))) {
    return(equal)
  }

  aux <- tryCatch(stats::lm.fit(design, log_e2), error = function(e) NULL)
  if (is.null(aux)) {
    return(equal)
  }

  g_hat <- aux$fitted.values
  if (!all(is.finite(g_hat))) {
    return(equal)
  }

  # exp() of a large fitted value overflows to Inf and of a large negative one
  # underflows to 0; either would reintroduce the degenerate weighting this
  # function exists to avoid. Centre the log-variance so the weights are scale
  # free -- WLS is invariant to a common factor -- and then guard the range.
  g_hat <- g_hat - mean(g_hat)
  sigma2 <- exp(g_hat)
  if (!all(is.finite(sigma2)) || any(sigma2 <= 0)) {
    return(equal)
  }

  w <- 1 / sigma2
  if (!all(is.finite(w)) || any(w <= 0) ||
        diff(range(w)) < .Machine$double.eps) {
    return(equal)
  }
  w
}
