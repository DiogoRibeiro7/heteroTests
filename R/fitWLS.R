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

  # model$residuals, not residuals(model): under na.action = na.exclude the
  # latter is padded back to the original row count with NA placeholders, while
  # the model matrix and response cover only the fitted rows, and lm.wfit()
  # then fails with "incompatible dimensions".
  res <- model$residuals
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

  # Centre the log-variance so the weights are scale free: WLS is invariant to
  # a common factor.
  #
  # This exponentiation is checked rather than assumed safe. The tempting
  # argument that it cannot overflow -- log(e^2) is finite only while e^2 is,
  # so the logged squared residuals are bounded by log(double.xmax) = 709.8,
  # the same point at which exp() overflows -- does not hold, because fitted
  # values are not bounded by the range of the response. At a high-leverage
  # point the hat matrix carries negative weights and the fit extrapolates:
  # for an ill-conditioned design and logged squared residuals spanning the
  # representable range, the centred fitted values reach 1365 and exp()
  # underflows to a zero weight.
  #
  # rlog_squared_residuals() floors residuals below double.eps before taking
  # logarithms, which bounds log_e2 -- but not these fitted values, since the
  # auxiliary fit can extrapolate past the range of its own response. The
  # inputs tried here reached a centred value of 705, just under the limit;
  # that shows the guard is hard to reach through this caller, not that it
  # cannot be reached.
  w <- 1 / exp(g_hat - mean(g_hat))
  if (!all(is.finite(w)) || any(w <= 0)) {
    return(equal)
  }

  # A relative tolerance, not an absolute one: the auxiliary fit leaves
  # floating-point noise of order 1e-13 even when the fitted log-variance is
  # constant, which is far above .Machine$double.eps, so an absolute test here
  # would never fire.
  if (diff(range(w)) / mean(w) < 1e-8) {
    return(equal)
  }

  # Weights that span more than six orders of magnitude concentrate the fit on
  # a handful of observations, which is the failure this function was rewritten
  # to remove. Genuine heteroscedasticity does not reach that far: measured
  # weight ratios are about 14 for sd proportional to x, 236 for x^2 and 2.5e+03
  # for exp(x), so this bound only catches a pathological auxiliary fit. Warn
  # rather than fall back silently, since a caller who asked for WLS should know
  # it got OLS.
  if (max(w) / min(w) > 1e6) {
    warning(
      "fitWLS: the estimated variance model spans more than six orders of ",
      "magnitude, which would concentrate the fit on a few observations. ",
      "Falling back to equal weights, so the result is the unweighted fit.",
      call. = FALSE
    )
    return(equal)
  }
  w
}
