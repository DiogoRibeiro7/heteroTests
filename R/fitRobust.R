#' Robust regression wrapper
#'
#' Provides a simple interface to \code{MASS::rlm} for robust estimation.
#'
#' The result is an M-estimate, not a least-squares fit: its residuals are not
#' least-squares residuals, and the null distributions of the tests of the
#' package assume least squares. Since 0.17.0 every test refuses it, and so do
#' [fitWLS()], [runHeteroTests()] and [runDiagnostics()], with an error of
#' class `heteroTests_model_context_error`; until then 37 tests took it for an
#' `lm` and tested its residuals as least-squares ones. To test the variance
#' of the errors, test the [stats::lm()] fit of the same formula. The result is for
#' estimation, and for comparisons such as those of
#' [autoCompareRemediations()], which reads its AIC and residuals only.
#'
#' Since 0.17.0 the call the result records names what was given: the
#' formula, and the data as the caller named them, or, for an `lm`, the
#' `data`, `subset` and `na.action` of its call, so that `print()` and
#' `summary()` show `MASS::rlm(formula = mpg ~ wt + qsec, data = mtcars)`, where they
#' showed `rlm(formula = form, data = data)`, `update()` refits it, and the
#' refusal of the tests names the [stats::lm()] call to test instead,
#' `lm(mpg ~ wt + qsec, data = mtcars)`.
#'
#' @param model A fitted model of class `lm` or a formula.
#' @param data Optional data frame if `model` is a formula.
#' @param ... Additional arguments passed to \code{MASS::rlm}.
#' @return An object of class `rlm`.
#' @examples
#' data(mtcars)
#' m <- fitRobust(mpg ~ wt + qsec, mtcars)
#' summary(m)
#' # The tests refuse it; test the lm() fit of the same formula instead.
#' performKoenkerTest(lm(mpg ~ wt + qsec, mtcars))
fitRobust <- function(model, data = NULL, ...) {
  # The data as the caller named them, for the call of the result.
  data_expr <- substitute(data)
  if (inherits(model, "lm")) {
    checkModel(model)
    form <- formula(model)
    fit <- MASS::rlm(form, data = model.frame(model), ...)
    given <- if (is.call(model$call)) as.list(model$call) else list()
    named <- list(data = given$data, subset = given$subset, na.action = given$na.action)
  } else if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    form <- model
    fit <- MASS::rlm(form, data = data, ...)
    named <- list(data = data_expr)
  } else {
    stop("`model` must be an 'lm' object or a formula")
  }
  # The call names the formula and the data the caller gave, not the
  # variables of this function (from the review of increment 11: the refusal
  # of the tests suggested lm(mpg ~ wt + qsec, data = data), which stops at
  # top level, or fits whatever object `data` names there).
  # MASS::rlm(), which update() finds whether MASS is attached or not.
  fit$call[[1L]] <- quote(MASS::rlm)
  fit$call$formula <- form
  # The arguments passed on to MASS::rlm() as the caller wrote them, where
  # the call recorded `..1` for an expression such as MASS::psi.bisquare.
  dots <- as.list(substitute(list(...)))[-1L]
  for (arg in setdiff(names(dots), "")) {
    fit$call[[arg]] <- dots[[arg]]
  }
  for (arg in names(named)) {
    # `data` always (without data of its own, an lm leaves none, where the
    # call named the model frame of this function); the others when given.
    if (!is.null(named[[arg]]) || identical(arg, "data")) {
      fit$call[[arg]] <- named[[arg]]
    }
  }
  fit
}
