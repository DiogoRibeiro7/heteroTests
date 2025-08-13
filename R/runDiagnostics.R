#' Run a broader suite of diagnostics
#'
#' This convenience wrapper extends `runHeteroTests()` by including checks for
#' multicollinearity, nonlinearity and influential observations.
#'
#' @param model A fitted model of class `lm` or a formula.
#' @param data Optional data frame if `model` is a formula.
#' @param tests Character vector of heteroscedasticity tests passed to
#'   `runHeteroTests`.
#' @param power Powers for `performRESETTest`.
#' @return A named list containing results of heteroscedasticity tests and other diagnostics.
#' @seealso \code{\link{runHeteroTests}}, \code{\link{analyzeMLResiduals}},
#'   \code{\link{runPanelTests}}, \code{\link{runTimeSeriesTests}}
#' @examples
#' data(mtcars)
#' runDiagnostics(mpg ~ wt + qsec, mtcars)
runDiagnostics <- function(model, data = NULL,
                           tests = c("white", "breusch_pagan"),
                           power = 2:3) {
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    checkData(data)
    model <- lm(model, data = data)
  } else {
    checkModel(model)
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      checkData(data)
    }
  }
  hetero <- runHeteroTests(model, data, tests)
  vif <- performVIFDiagnostic(model)
  reset <- performRESETTest(model, power = power)
  influence <- performInfluenceDiagnostics(model)
  c(hetero, list(vif = vif, reset = reset, influence = influence))
}
