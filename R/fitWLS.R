#' Weighted Least Squares wrapper
#'
#' Refit a model by feasible generalised least squares, weighting each
#' observation by the inverse of an estimated error variance.
#'
#' The variance is *modelled*, not read off the residuals directly. A single
#' squared residual is a one-degree-of-freedom estimate of \eqn{\sigma_i^2} and
#' far too noisy to invert: weighting by \eqn{1/e_i^2} hands almost all of the
#' weight to whichever observations the initial fit happened to reproduce most
#' closely. This function instead fits a variance function to the squared
#' residuals and takes \eqn{\hat\sigma_i^2} from its fitted values, the
#' standard feasible-GLS recipe; weights are \eqn{1/\hat\sigma_i^2}.
#'
#' @section Variance functions:
#' `form` chooses the shape of the variance function and `var_formula` the
#' variables \eqn{z_i} it depends on. Both forms are linear in the logarithm
#' of the variance and carry their own constant.
#' \describe{
#'   \item{`"exponential"`}{\eqn{\log \sigma_i^2 = \gamma_0 + z_i^\top \gamma},
#'     the multiplicative model of Harvey (1976), estimated by regressing
#'     \eqn{\log e_i^2} on \eqn{z_i}. The log scale keeps the fitted variances
#'     positive without constraining the auxiliary regression. This is the
#'     default, and with `var_formula = NULL` it is what `fitWLS()` has
#'     computed since 0.9.0.}
#'   \item{`"power"`}{\eqn{\log \sigma_i^2 = \gamma_0 + \sum_j \delta_j \log
#'     z_{ij}}, that is \eqn{\sigma_i^2 \propto \prod_j z_{ij}^{\delta_j}}, the
#'     model of Park (1966). It is the exponential form in the logarithms of
#'     the regressors, which must be strictly positive. \eqn{\delta = 2} is the
#'     textbook case of a standard deviation proportional to \eqn{z}.}
#' }
#' Other log-linear shapes are written through `var_formula`, for example
#' `~ x + I(x^2)` or `~ sqrt(x)`. The additive model
#' \eqn{\sigma_i^2 = \alpha_0 + z_i^\top \alpha} is not offered: nothing keeps
#' its fitted variances positive, and in the validation study it was not
#' usable in a material share of samples drawn from an additive model itself
#' (`inst/validation/README.md`).
#'
#' Residuals that are numerically zero are floored before the logarithm is
#' taken, with a warning.
#'
#' Weights estimated this way are consistent under a correctly specified
#' variance function, so standard errors from the returned fit are usable. They
#' were not before 0.9.0, when the weights were the raw inverse squared
#' residuals: the weighted residual sum of squares then collapsed towards
#' \eqn{n} regardless of the data, and nominal 95% intervals covered the truth
#' about 10% of the time.
#'
#' Whether the variance function *is* correctly specified can be tested: pass
#' the result to [performVarianceFormTest()].
#'
#' If the variance function cannot be fitted, or yields no usable variation,
#' the function falls back to equal weights, which reduces the result to the
#' original OLS fit.
#'
#' @param model A fitted model of class `lm`.
#' @param data Optional [base::data.frame] holding the variables named in
#'   `var_formula`. It is needed only when they are not in the model frame, and
#'   is matched to the fitted rows by row name.
#' @param var_formula One-sided formula naming the variance regressors, for
#'   example `~ x`. `NULL`, the default, uses the regressors of `model`.
#' @param form Character scalar choosing the variance function:
#'   `"exponential"` (the default) or `"power"`. See the section on variance
#'   functions.
#' @return A new `lm` object fitted with weights. The estimated variances are
#'   attached as the `"variance_model"` attribute, and the fitted variance
#'   function (its form, regressors and coefficients) as the
#'   `"variance_function"` attribute, which is what
#'   [performVarianceFormTest()] reads.
#' @references
#' Harvey, A. C. (1976). Estimating regression models with multiplicative
#' heteroscedasticity. *Econometrica, 44*(3), 461--465.
#' \doi{10.2307/1913974}
#'
#' Park, R. E. (1966). Estimation with heteroscedastic error terms.
#' *Econometrica, 34*(4), 888. \doi{10.2307/1910108}
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' wls <- fitWLS(m)
#' summary(wls)
#'
#' # Variance proportional to a power of one regressor
#' wls_power <- fitWLS(m, var_formula = ~ wt, form = "power")
#' attr(wls_power, "variance_function")$coefficients
#' @seealso [performVarianceFormTest()] to test the variance function that was
#'   fitted.
fitWLS <- function(model, data = NULL, var_formula = NULL,
                   form = c("exponential", "power")) {
  checkModel(model)
  form <- match.arg(form)
  rweighted_refit(model, rfit_variance_function(model, data, var_formula, form))
}

#' Refit a model with the weights of a fitted variance function
#'
#' @param model A fitted `lm`.
#' @param variance The result of `rfit_variance_function()`.
#' @return The weighted fit [fitWLS()] returns.
#' @keywords internal
#' @noRd
rweighted_refit <- function(model, variance) {
  mf <- stats::model.frame(model)
  response <- stats::model.response(mf)
  design <- stats::model.matrix(model, data = mf)
  w <- variance$weights

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
  attr(fit, "variance_function") <- variance[names(variance) != "weights"]
  fit
}

#' Data frame aligned with the rows a model was fitted to
#'
#' With `data = NULL` this is the model frame, stripped of its terms so that a
#' second formula can be evaluated in it. Otherwise `data` is matched to the
#' fitted rows by row name, as the tests match it to the residuals.
#'
#' @param model A fitted `lm`.
#' @param data A data frame, or `NULL`.
#' @return A data frame with one row per fitted observation.
#' @keywords internal
#' @noRd
rvariance_frame <- function(model, data) {
  mf <- stats::model.frame(model)
  if (is.null(data)) {
    plain <- as.data.frame(mf)
    attr(plain, "terms") <- NULL
    return(plain)
  }
  checkData(data)
  idx <- match(rownames(mf), rownames(data))
  if (anyNA(idx)) {
    if (nrow(data) != nrow(mf)) {
      stop(
        sprintf(
          paste(
            "`data` must contain the %d rows the model was fitted to; it has",
            "%d rows and their names do not match."
          ),
          nrow(mf), nrow(data)
        ),
        call. = FALSE
      )
    }
    return(data)
  }
  data[idx, , drop = FALSE]
}

#' Design matrix of a one-sided formula on the fitted rows
#'
#' @param model A fitted `lm`.
#' @param data A data frame, or `NULL` for the model frame.
#' @param formula One-sided formula, or `NULL` for the regressors of `model`.
#' @param arg Name of the argument, for error messages.
#' @return A list with `matrix`, the regressors without an intercept column, and
#'   `label`, a description for printing.
#' @keywords internal
#' @noRd
rvariance_regressors <- function(model, data, formula, arg = "var_formula") {
  n <- nrow(stats::model.frame(model))
  if (is.null(formula)) {
    design <- stats::model.matrix(model)
    Z <- design[, colnames(design) != "(Intercept)", drop = FALSE]
    if (ncol(Z) == 0L) {
      stop(
        sprintf("The model has no regressors beyond the intercept; supply `%s`.", arg),
        call. = FALSE
      )
    }
    return(list(matrix = Z, label = "model regressors"))
  }
  if (!inherits(formula, "formula") || length(formula) != 2L) {
    stop(sprintf("`%s` must be a one-sided formula, e.g. ~ x1 + x2.", arg), call. = FALSE)
  }
  frame <- rvariance_frame(model, data)
  aux <- tryCatch(
    stats::model.frame(formula, data = frame, na.action = stats::na.pass),
    error = function(e) e
  )
  if (inherits(aux, "error")) {
    stop(
      sprintf(
        paste(
          "`%s` could not be evaluated (%s). Variables that are not part of",
          "the model must be supplied through `data`."
        ),
        arg, conditionMessage(aux)
      ),
      call. = FALSE
    )
  }
  Z <- stats::model.matrix(formula, data = aux)
  Z <- Z[, colnames(Z) != "(Intercept)", drop = FALSE]
  if (ncol(Z) == 0L) {
    stop(sprintf("`%s` must name at least one variable.", arg), call. = FALSE)
  }
  if (nrow(Z) != n) {
    stop(
      sprintf(
        "`%s` could not be aligned with the fitted rows (expected %d rows, got %d).",
        arg, n, nrow(Z)
      ),
      call. = FALSE
    )
  }
  if (any(!is.finite(Z))) {
    stop(
      sprintf("`%s` produced missing or non-finite values on the fitted rows.", arg),
      call. = FALSE
    )
  }
  list(matrix = Z, label = paste(deparse(formula[[2L]]), collapse = " "))
}

#' Fit a parametric variance function to the residuals of a model
#'
#' The engine behind [fitWLS()] and [performVarianceFormTest()]. Besides the
#' weights it returns what a specification test needs: the regressors on the
#' scale the form uses. For a variance function that is linear in the
#' logarithm of the variance they are its gradient divided by the fitted
#' variance, the direction in which estimating its parameters moves the
#' squared standardized residuals.
#'
#' @param model A fitted `lm`.
#' @param data,var_formula,form As in [fitWLS()].
#' @param warn Whether an unusable fit warns. [performVarianceFormTest()] turns
#'   the warning off because it raises an error instead.
#' @return A list: `weights` (equal weights when the fit is unusable), `form`,
#'   `var_formula`, `label`, `regressors` (on the scale the form uses),
#'   `coefficients`, `index` (the fitted log-variance), `usable` and `reason`.
#' @keywords internal
#' @noRd
rfit_variance_function <- function(model, data, var_formula, form, warn = TRUE) {
  # model$residuals, not residuals(model): under na.action = na.exclude the
  # latter is padded back to the original row count with NA placeholders, while
  # the model matrix and response cover only the fitted rows, and lm.wfit()
  # then fails with "incompatible dimensions".
  res <- model$residuals
  regressors <- rvariance_regressors(model, data, var_formula)
  Z <- regressors$matrix

  if (identical(form, "power")) {
    not_positive <- colnames(Z)[apply(Z, 2L, function(z) any(z <= 0))]
    if (length(not_positive) > 0L) {
      stop(
        "form = \"power\" takes logarithms of the variance regressors, so they ",
        "must be strictly positive. Not positive: ",
        paste(not_positive, collapse = ", "),
        ". Choose the regressors with `var_formula`, or use form = \"exponential\".",
        call. = FALSE
      )
    }
    Z <- log(Z)
    colnames(Z) <- paste0("log(", colnames(Z), ")")
  }

  # The variance function always carries its own constant, whether or not the
  # mean model has an intercept: without one, log sigma^2 = z'gamma pins the
  # variance at 1 where z = 0 and leaves the model no scale to fit.
  design <- cbind("(Intercept)" = 1, Z)

  estimated <- rfgls_fit(res, design, warn = warn)

  list(
    weights = estimated$weights,
    form = form,
    var_formula = var_formula,
    label = regressors$label,
    regressors = Z,
    coefficients = estimated$coefficients,
    index = estimated$index,
    usable = is.null(estimated$reason),
    reason = estimated$reason
  )
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
  rfgls_fit(res, design)$weights
}

#' Fit the log-variance model behind `rfgls_weights()`
#'
#' @param res Residuals from the initial fit.
#' @param design Auxiliary design matrix, including its constant.
#' @param warn Whether an unusable weight spread warns.
#' @return A list with `weights`, the fitted log-variance `index`, the
#'   `coefficients` of the auxiliary regression, and `reason`, which is `NULL`
#'   when the fit is usable and otherwise says why the weights are equal.
#' @keywords internal
#' @noRd
rfgls_fit <- function(res, design, warn = TRUE) {
  n <- length(res)
  fallback <- function(reason, index = rep(NA_real_, n), coefficients = NULL) {
    list(weights = rep(1, n), index = index, coefficients = coefficients, reason = reason)
  }

  log_e2 <- tryCatch(
    rlog_squared_residuals(res, "fitWLS"),
    error = function(e) NULL
  )
  if (is.null(log_e2) || !all(is.finite(log_e2))) {
    return(fallback("the residuals are missing or not finite"))
  }

  aux <- tryCatch(stats::lm.fit(design, log_e2), error = function(e) NULL)
  if (is.null(aux)) {
    return(fallback("the auxiliary regression of the log squared residuals failed"))
  }

  g_hat <- aux$fitted.values
  if (!all(is.finite(g_hat))) {
    return(fallback("the fitted log-variances are not finite"))
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
    return(fallback("the fitted variances overflow", g_hat, aux$coefficients))
  }

  reason <- rweight_spread_problem(w, warn = warn)
  if (!is.null(reason)) {
    return(fallback(reason, g_hat, aux$coefficients))
  }
  list(weights = w, index = g_hat, coefficients = aux$coefficients, reason = NULL)
}

#' Decide whether estimated weights are usable
#'
#' @param w Positive, finite weights.
#' @param warn Whether an excessive spread warns.
#' @return `NULL` when the weights are usable, otherwise a character scalar
#'   saying why they are not.
#' @keywords internal
#' @noRd
rweight_spread_problem <- function(w, warn = TRUE) {
  # A relative tolerance, not an absolute one: the auxiliary fit leaves
  # floating-point noise of order 1e-13 even when the fitted log-variance is
  # constant, which is far above .Machine$double.eps, so an absolute test here
  # would never fire.
  if (diff(range(w)) / mean(w) < 1e-8) {
    return("the fitted variance function is flat")
  }

  # Weights that span more than six orders of magnitude concentrate the fit on
  # a handful of observations, which is the failure this function was rewritten
  # to remove. Genuine heteroscedasticity does not reach that far: measured
  # weight ratios are about 14 for sd proportional to x, 236 for x^2 and 2.5e+03
  # for exp(x), so this bound only catches a pathological auxiliary fit. Warn
  # rather than fall back silently, since a caller who asked for WLS should know
  # it got OLS.
  if (max(w) / min(w) > 1e6) {
    if (warn) {
      warning(
        "fitWLS: the estimated variance model spans more than six orders of ",
        "magnitude, which would concentrate the fit on a few observations. ",
        "Falling back to equal weights, so the result is the unweighted fit.",
        call. = FALSE
      )
    }
    return("the estimated variances span more than six orders of magnitude")
  }
  NULL
}
