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
#' A test of constant variance on the result takes its weights as known,
#' although they were estimated from the same data, so its reference
#' distribution is only approximate: see the validation study in
#' `inst/validation/README.md`. The bootstrap
#' tests [performWildBootstrapTest()], [performWhiteTestBootstrap()] and
#' [bootstrapTestStatistic()] allow for the estimation. Since 0.15.0 they run
#' `fitWLS()` again on each replicate by default: they fit it as `model` was
#' fitted, fit the recorded variance function again to its residuals, on the
#' recorded regressors, and refit it with the weights that gives. Their
#' argument `reestimate_weights = FALSE` holds the weights fixed instead, as
#' they did before 0.15.0.
#'
#' If the variance function cannot be fitted, or yields no usable variation,
#' the function falls back to equal weights, which reduces the result to the
#' original OLS fit.
#'
#' The weighted fit keeps the design and the offset of `model`, whether the
#' offset was given as an argument, `lm(..., offset = o)`, or written in the
#' formula, `offset(o)`. Its coefficients, fitted values and residuals are
#' those `lm()` returns for the same call with the estimated weights added.
#' On a model fitted with weights, the refit uses the estimated weights in
#' their place, and `weights()` of the result returns them.
#'
#' A `glm` with the Gaussian family and the identity link is a least-squares
#' fit, with the design and residuals of the `lm`, and since 0.16.0 the result
#' for it is the `lm` returned for that `lm`, prior weights included; before,
#' it kept the class of the `glm` without its family, which `summary()` could
#' not read and every test refused. Any other `glm` is refused.
#'
#' @param model A fitted model of class `lm`, or a `glm` with the Gaussian
#'   family and the identity link.
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
#'   [performVarianceFormTest()] reads, and what the bootstrap tests read to
#'   estimate the weights again in each replicate. When `model` was fitted
#'   with weights, the attribute also holds them, as `initial_weights`.
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
  # A Gaussian glm with the identity link is a least-squares fit, and is
  # refitted as the lm it is. Before 0.16.0 any glm was refitted and kept the
  # class of a glm without its family, which summary() could not read and
  # every test refused.
  rrefuse_non_gaussian_glm(model, "fitWLS")
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
  # The design stored with the fit, and its offset, whether it was given as an
  # argument or written in the formula, so that the weighted fit is the model
  # supplied with weights added, as lm(weights = w) would fit it. The design
  # was computed by model.matrix(model, data = mf), which evaluates the call
  # of the fit again in the model frame: that frame holds log(x) and not x,
  # and weights and an offset given as arguments as "(weights)" and
  # "(offset)", so the call failed on a transformed regressor, on a weighted
  # model and on most offsets. Where it did not fail, lm.wfit() was called
  # without the offset, which it then dropped.
  design <- stats::model.matrix(model)
  offset <- stats::model.offset(mf)
  w <- variance$weights

  fit <- stats::lm.wfit(design, response, w, offset = offset)
  fit$offset <- offset
  fit$call <- model$call
  fit$terms <- stats::terms(model)
  fit$model <- mf
  fit$xlevels <- model$xlevels
  fit$contrasts <- attr(design, "contrasts")
  fit$na.action <- stats::na.action(model)
  fit$weights <- w
  # lm.wfit() fitted it, so it is an lm(..., weights = w), also when `model`
  # is a Gaussian glm with the identity link, the only glm fitWLS() and
  # performVarianceFormTest() accept. Before 0.16.0 such a fit kept the class
  # of the glm, without its family.
  class(fit) <- if (inherits(model, "glm")) "lm" else class(model)
  attr(fit, "variance_model") <- 1 / w
  record <- variance[names(variance) != "weights"]
  # The weights of the model refitted, if it had any. They decided its
  # residuals, so a bootstrap that estimates the variance function again
  # refits each replicate with them first, as the model was fitted. The model
  # frame keeps them as "(weights)" for now, but that is a defect to be fixed
  # in 0.16.0, so they are recorded here. The prior weights of a Gaussian glm
  # are read too (rleast_squares_weights()).
  initial_weights <- rleast_squares_weights(model)
  if (!is.null(initial_weights)) {
    record$initial_weights <- initial_weights
  }
  attr(fit, "variance_function") <- record
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
  .check_data(data)
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

  rvariance_function_on(res, Z, form, var_formula, regressors$label, warn = warn)
}

#' Fit a variance function on regressors already on the scale of its form
#'
#' The estimation step of [rfit_variance_function()], once the regressors have
#' been evaluated and, for the power form, logged. The bootstrap tests call it
#' on each replicate of a [fitWLS()] fit with the regressors the fit recorded,
#' so that a replicate is estimated as [fitWLS()] estimated the fit.
#'
#' @param res Residuals of the model the variance function is fitted to.
#' @param Z Variance regressors on the scale the form uses, without a constant.
#' @param form,var_formula,label As recorded by [rfit_variance_function()].
#' @param warn Whether an unusable weight spread warns.
#' @return The list [rfit_variance_function()] returns.
#' @keywords internal
#' @noRd
rvariance_function_on <- function(res, Z, form, var_formula, label, warn = TRUE) {
  # The variance function always carries its own constant, whether or not the
  # mean model has an intercept: without one, log sigma^2 = z'gamma pins the
  # variance at 1 where z = 0 and leaves the model no scale to fit.
  design <- cbind("(Intercept)" = 1, Z)

  estimated <- rfgls_fit(res, design, warn = warn)

  list(
    weights = estimated$weights,
    form = form,
    var_formula = var_formula,
    label = label,
    regressors = Z,
    coefficients = estimated$coefficients,
    index = estimated$index,
    usable = is.null(estimated$reason),
    reason = estimated$reason
  )
}

#' The variance function a bootstrap estimates again in each replicate
#'
#' Resolves the `reestimate_weights` argument of [performWildBootstrapTest()],
#' [performWhiteTestBootstrap()] and [bootstrapTestStatistic()]. `NULL`, the
#' default, estimates the weights again when `model` carries the variance
#' function [fitWLS()] estimated, and holds them fixed otherwise. `TRUE` on a
#' model without one is an error: weights the user supplied are known, and
#' there is no variance function to estimate them from.
#'
#' A fit on which [fitWLS()] fell back to equal weights estimated none, so it
#' is treated as a model without a variance function. A variance regressor
#' computed from the response (see [rregressors_from_response()]) cannot be
#' estimated again from the record, which holds the regressors as evaluated on
#' the data of the fit, while each replicate regenerates the response: `NULL`
#' then holds the weights fixed with a warning, and `TRUE` is an error. So is
#' a fit whose initial weights cannot be read (see
#' [rwls_initial_weights()]). The record returned carries the initial weights
#' as `initial_weights`, read from the model frame for a fit made before
#' 0.15.0.
#'
#' @param model The fitted model given to the bootstrap.
#' @param reestimate_weights The argument: `NULL`, `TRUE` or `FALSE`.
#' @param fn Name of the calling function, for messages.
#' @return `NULL` when the weights are held fixed, otherwise the record
#'   [fitWLS()] attached to `model` as its `"variance_function"` attribute.
#' @keywords internal
#' @noRd
rwls_reestimation <- function(model, reestimate_weights, fn) {
  if (!is.null(reestimate_weights) &&
        (!is.logical(reestimate_weights) || length(reestimate_weights) != 1L ||
           is.na(reestimate_weights))) {
    stop("`reestimate_weights` must be NULL, TRUE or FALSE.", call. = FALSE)
  }
  if (isFALSE(reestimate_weights)) {
    return(NULL)
  }
  record <- attr(model, "variance_function")
  requested <- isTRUE(reestimate_weights)
  if (is.null(record) || is.null(record$regressors)) {
    if (!requested) {
      return(NULL)
    }
    stop(
      fn, "(reestimate_weights = TRUE) needs a fit returned by fitWLS(), which ",
      "records the variance function its weights were estimated from. `model` ",
      "carries none: weights given to lm() are known, and they are held fixed ",
      "in each replicate.",
      call. = FALSE
    )
  }
  if (!isTRUE(record$usable)) {
    if (!requested) {
      return(NULL)
    }
    stop(
      fn, "(reestimate_weights = TRUE): fitWLS() could not estimate the ",
      "variance function of `model` (", record$reason, ") and fell back to ",
      "equal weights, so there are no estimated weights to estimate again.",
      call. = FALSE
    )
  }
  if (nrow(record$regressors) != length(model$residuals)) {
    stop(
      fn, "(): the variance function recorded on `model` does not have one row ",
      "per fitted observation, so it cannot be estimated again.",
      call. = FALSE
    )
  }
  response_regressors <- rregressors_from_response(model, record$regressors)
  if (length(response_regressors) > 0L) {
    what <- paste0(
      "the variance regressor", if (length(response_regressors) > 1L) "s", " ",
      paste(response_regressors, collapse = ", "), " of `model` ",
      if (length(response_regressors) > 1L) "are" else "is",
      " computed from the response, which each replicate regenerates, so ",
      "fitWLS() cannot be run again on a replicate from the regressors it recorded"
    )
    workaround <- paste0(
      "A variable of the response that the right-hand side of the model does ",
      "not use counts as regenerated; if it is not, give the response as a ",
      "column of the data, as in d$r <- d$y / d$x; lm(r ~ z, data = d), and ",
      "fit that."
    )
    if (requested) {
      stop(fn, "(reestimate_weights = TRUE): ", what, ". ", workaround, call. = FALSE)
    }
    warning(
      fn, "(): ", what, ". The weights are held fixed in each replicate, as ",
      "before 0.15.0. ", workaround,
      call. = FALSE
    )
    return(NULL)
  }
  initial <- rwls_initial_weights(model, record)
  if (identical(initial, FALSE)) {
    if (requested) {
      stop(
        fn, "(reestimate_weights = TRUE): the weights of the model fitWLS() ",
        "refitted cannot be read from `model`.",
        call. = FALSE
      )
    }
    warning(
      fn, "(): the weights of the model fitWLS() refitted cannot be read from ",
      "`model`, so the weights are held fixed in each replicate, as before 0.15.0.",
      call. = FALSE
    )
    return(NULL)
  }
  record$initial_weights <- initial
  record
}

#' Variance regressors computed from the response
#'
#' A replicate regenerates the response of the model and holds the design
#' fixed. A variable of the response that the right-hand side of the model
#' does not use, `y` in `log(y) ~ x` or both `y` and `x` in `I(y / x) ~ z`, is
#' therefore regenerated with it: `fitWLS()` on the replicate would see
#' `y* = exp(r*)` or `y* = r* x`. A variance regressor that uses such a
#' variable is computed from the response, and so is one that is the response
#' term itself, which is how `~ .` evaluated on the model frame names it,
#' backquoted. A variable that the right-hand side uses is held fixed with the
#' design, so `x` for `log(y / x) ~ log(x)` and the offset `o` for
#' `y ~ x + offset(o)` are not computed from the response, and neither is any
#' column of the design, whatever its name. The regressors are
#' read from the columns fitWLS() recorded, so `.` is expanded as fitWLS()
#' expanded it.
#'
#' The rule is conservative: `x` for `I(y / x) ~ z` counts as computed from
#' the response, since the replicate cannot tell which of `y` and `x`
#' changed. Giving the response as a column of the data, `r <- y / x` and
#' `lm(r ~ z)`, says that `x` stays.
#'
#' @param model A fitted `lm`.
#' @param Z The recorded variance regressors.
#' @return The names of the columns of `Z` computed from the response.
#' @keywords internal
#' @noRd
rregressors_from_response <- function(model, Z) {
  model_formula <- tryCatch(stats::formula(model), error = function(e) NULL)
  if (is.null(model_formula) || length(model_formula) != 3L || is.null(colnames(Z))) {
    return(character())
  }
  response <- model_formula[[2L]]
  # Every replicate holds the design fixed: a variable of the right-hand side
  # is not regenerated.
  response_vars <- setdiff(all.vars(response), all.vars(model_formula[[3L]]))
  squash <- function(x) gsub("[[:space:]`]", "", x)
  response_term <- squash(paste(deparse(response, width.cutoff = 500L), collapse = ""))
  # A column of the design is held fixed in every replicate whatever its
  # name, such as factor(g)y for a level "y" of g, which the fallback below
  # would read as the variable y.
  design_columns <- tryCatch(colnames(stats::model.matrix(model)),
                             error = function(e) character())
  uses_response <- vapply(colnames(Z), function(column) {
    if (column %in% design_columns) {
      return(FALSE)
    }
    # `~ .` on the model frame names the response column backquoted, as
    # `log(y)`; a column that is the response term, or holds it backquoted, is
    # computed from it whatever its variables.
    if (identical(squash(column), response_term) ||
          grepl(paste0("`", response_term, "`"), gsub("[[:space:]]", "", column), fixed = TRUE)) {
      return(TRUE)
    }
    if (length(response_vars) == 0L) {
      return(FALSE)
    }
    expr <- tryCatch(str2lang(gsub("`", "", column, fixed = TRUE)), error = function(e) NULL)
    used <- if (is.null(expr)) {
      # A column name that does not parse, such as factor(g)b: look for the
      # variables as whole words.
      response_vars[vapply(response_vars, function(v) {
        grepl(paste0("(^|[^[:alnum:]._])", gsub("([.])", "\\\\.", v), "($|[^[:alnum:]._])"),
              column)
      }, logical(1))]
    } else {
      all.vars(expr)
    }
    any(response_vars %in% used)
  }, logical(1))
  colnames(Z)[uses_response]
}

#' Weights of the model a fitWLS() fit refitted
#'
#' Since 0.15.0 [fitWLS()] records them as `initial_weights`. A fit made
#' before 0.15.0 does not, but its model frame is the frame of the model it
#' refitted, and holds them as `(weights)`. The frame of a fit made since
#' 0.15.0 of an unweighted model holds no weights, or, should it ever hold the
#' estimated weights, holds those of the fit itself, which are not the
#' weights of the model it refitted.
#'
#' @param model A [fitWLS()] fit.
#' @param record Its `"variance_function"` attribute.
#' @return The weights, `NULL` for an unweighted model, or `FALSE` when they
#'   cannot be read.
#' @keywords internal
#' @noRd
rwls_initial_weights <- function(model, record) {
  if (!is.null(record$initial_weights)) {
    return(record$initial_weights)
  }
  frame <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  if (is.null(frame)) {
    return(FALSE)
  }
  w <- stats::model.weights(frame)
  if (is.null(w)) {
    return(NULL)
  }
  w <- as.vector(w)
  if (identical(w, as.vector(model$weights))) {
    return(NULL)
  }
  if (length(w) != length(model$residuals)) {
    return(FALSE)
  }
  w
}

#' Estimate the weights of a replicate as fitWLS() estimated the fit
#'
#' `initial` is the model of a replicate fitted as the model [fitWLS()] was
#' given: without weights, or with the weights it had, recorded as
#' `initial_weights`. Its residuals are fitted with the variance function of
#' `record`, on the regressors `Z` of the rows of the replicate, and the model
#' is refitted with the estimated weights, as [fitWLS()] refits it.
#'
#' @param initial The initial fit of the replicate, an `lm`.
#' @param record The variance function [fitWLS()] recorded.
#' @param Z The variance regressors of the rows of the replicate.
#' @return The weighted fit, or `NULL` when the variance function cannot be
#'   estimated and [fitWLS()] would fall back to equal weights.
#' @keywords internal
#' @noRd
rwls_refit_replicate <- function(initial, record, Z) {
  variance <- rwithout_floor_warning(rvariance_function_on(
    initial$residuals, Z, record$form, record$var_formula, record$label,
    warn = FALSE
  ))
  if (!isTRUE(variance$usable)) {
    return(NULL)
  }
  rweighted_refit(initial, variance)
}

#' Pearson residuals of a replicate whose weights are estimated again
#'
#' The computation of [rwls_refit_replicate()] on the design matrix, for a
#' bootstrap that refits by least squares without building a model: the
#' initial fit, the variance function and the weighted refit are the calls
#' `lm()`, [rfit_variance_function()] and [rweighted_refit()] make.
#'
#' @param design The design matrix of the fit.
#' @param response The regenerated response, on the scale the model uses.
#' @param offset The offset of the fit, or `NULL`.
#' @param record The variance function [fitWLS()] recorded.
#' @return The Pearson residuals of the weighted refit, or `NULL` when the
#'   variance function cannot be estimated.
#' @keywords internal
#' @noRd
rwls_replicate_residuals <- function(design, response, offset, record) {
  initial <- if (is.null(record$initial_weights)) {
    stats::lm.fit(design, response, offset = offset)
  } else {
    stats::lm.wfit(design, response, record$initial_weights, offset = offset)
  }
  variance <- rwithout_floor_warning(rvariance_function_on(
    initial$residuals, record$regressors, record$form, record$var_formula,
    record$label, warn = FALSE
  ))
  if (!isTRUE(variance$usable)) {
    return(NULL)
  }
  refit <- stats::lm.wfit(design, response, variance$weights, offset = offset)
  refit$residuals * sqrt(variance$weights)
}

#' Evaluate without the warning of a residual floored before its logarithm
#'
#' [rlog_squared_residuals()] warns when it floors a residual that is
#' numerically zero. On the fit [fitWLS()] returns that is worth saying once.
#' In a replicate, such as one of pairs resampling that draws a level of a
#' factor once, it would say it once per replicate, so the replicate is
#' estimated as [fitWLS()] estimates it, floor included, without the warning.
#'
#' @param expr The expression to evaluate.
#' @return The value of `expr`.
#' @keywords internal
#' @noRd
rwithout_floor_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("numerically zero and were floored", conditionMessage(w), fixed = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Say once how many replicates whose weights were estimated again failed
#'
#' @param fn Name of the calling function.
#' @param failed Number of replicates that failed.
#' @param B Number of replicates.
#' @return `NULL`, invisibly; called for the warning, given when some but not
#'   all replicates failed. When all fail the caller says so itself.
#' @keywords internal
#' @noRd
rwarn_failed_replicates <- function(fn, failed, B) {
  if (failed > 0L && failed < B) {
    warning(
      sprintf(
        paste0(
          "%s(): %d of the %d replicates failed and were left out, among them ",
          "any whose variance function could not be fitted again; the results ",
          "are computed from the %d that remain."
        ),
        fn, failed, B, B - failed
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
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
