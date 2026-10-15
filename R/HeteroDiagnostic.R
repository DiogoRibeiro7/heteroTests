#' Diagnostic object
#'
#' Create a diagnostic object that can run tests and plots via a unified API.
#'
#' @param model A fitted `lm`/`glm` model or a formula.
#' @param data Data used to fit the model. Required when `model` is a formula.
#' @return An object of class `HeteroDiagnostic`.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' d <- HeteroDiagnostic(m, mtcars)
#' runDiagnostics(d$model, d$data)
#' plots <- plot(d)
#' summary(d)
#' @export
HeteroDiagnostic <- function(model, data = NULL) {
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    .check_data(data)
    model <- lm(model, data = data)
  } else {
    checkModel(model)
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      .check_data(data)
    }
  }
  structure(list(model = model, data = data), class = "HeteroDiagnostic")
}

#' @describeIn HeteroDiagnostic Run diagnostic tests
#' @param object A `HeteroDiagnostic` object.
#' @param tests Character vector of tests to run.
#' @export
#' @usage test(object, ...)
#' @export test
test <- function(object, ...) {
  .Deprecated(
    old = "test",
    msg = paste0(
      "'test()' is deprecated.\nUse 'runDiagnostics(x$model, x$data, tests)' instead.\n",
      "See help(\"Deprecated\")"
    )
  )
  UseMethod("test")
}

#' @export
test.HeteroDiagnostic <- function(object, tests = c("white", "koenker"), ...) {
  runDiagnostics(object$model, object$data, tests = tests, ...)
}

#' @export
plot.HeteroDiagnostic <- function(x,
                                  plots = c(
                                    "residuals_fitted", "spread_level",
                                    "density", "qq", "bubble_variance"
                                  ),
                                  ...) {
  runDiagnosticPlots(x$model, plots = plots)
}

#' @export
summary.HeteroDiagnostic <- function(object, tests = c("white", "koenker"), ...) {
  res <- runDiagnostics(object$model, object$data, tests = tests, ...)
  stats <- sapply(res, function(r) {
    if (is.list(r) && !is.null(r$statistic)) r$statistic else NA
  })
  class(stats) <- "summary.HeteroDiagnostic"
  stats
}
