#' Robust regression wrapper
#'
#' Provides a simple interface to \code{MASS::rlm} for robust estimation.
#'
#' @param model A fitted model of class `lm` or a formula.
#' @param data Optional data frame if `model` is a formula.
#' @param ... Additional arguments passed to \code{MASS::rlm}.
#' @return An object of class `rlm`.
#' @examples
#' data(mtcars)
#' m <- fitRobust(mpg ~ wt + qsec, mtcars)
#' summary(m)
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
