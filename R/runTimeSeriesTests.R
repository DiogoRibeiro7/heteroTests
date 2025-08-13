#' Run time-series heteroscedasticity tests
#'
#' Convenience wrapper for Engle's ARCH LM and McLeod-Li tests.
#'
#' @param model A fitted model of class `lm`.
#' @param lags Number of lags for the ARCH LM and Ljung-Box tests.
#' @param tests Character vector of test names. Defaults to both tests.
#' @return A named list of `htest` objects.
#'
#' @seealso \code{\link{runPanelTests}} for panel-data diagnostics and
#'   \code{\link{runDiagnostics}} for the overall workflow.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, mtcars)
#' runTimeSeriesTests(m, lags = 2)
#' @export
runTimeSeriesTests <- function(model, lags = 1, tests = c("arch_lm", "mcleod_li")) {
  checkModel(model)
  available <- list(
    arch_lm = function(m) performArchLMTest(m, lags = lags),
    mcleod_li = function(m) performMcLeodLiTest(m, lags = lags)
  )
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop("Unknown tests: ", paste(invalid, collapse = ", "))
  }
  res <- lapply(tests, function(t) available[[t]](model))
  names(res) <- tests
  res
}
