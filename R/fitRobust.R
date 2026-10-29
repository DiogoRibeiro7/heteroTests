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
#' of the errors, test the [lm()] fit of the same formula. The result is for
#' estimation, and for comparisons such as those of
#' [autoCompareRemediations()], which reads its AIC and residuals only.
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
  if (inherits(model, "lm")) {
    checkModel(model)
    form <- formula(model)
    data <- model.frame(model)
  } else if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    form <- model
  } else {
    stop("`model` must be an 'lm' object or a formula")
  }
  MASS::rlm(form, data = data, ...)
}
