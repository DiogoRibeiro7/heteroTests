#' Run multiple heteroscedasticity tests
#'
#' This convenience wrapper runs diagnostic functions registered in the
#' package registry and returns their results as a list. By default it
#' executes White's test and the Breusch-Pagan test, but additional tests or
#' custom diagnostics registered via `registerDiagnostic()` can be requested.
#'
#' @param model A fitted model of class `lm`.
#' @param data Optional data frame used to fit `model`. If not supplied,
#'   `model.frame(model)` is used.
#'
#' @return A named list of `htest` objects.
#'
#' @seealso \code{\link{runDiagnostics}} for a broader suite that includes
#'   collinearity and influence measures, \code{\link{runPanelTests}} for panel
#'   data models and \code{\link{runTimeSeriesTests}} for time-series checks.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' runHeteroTests(m, mtcars)
#' registerDiagnostic("my_stat", function(model, data) list(statistic = 0))
#' runHeteroTests(m, mtcars, tests = c("white", "my_stat"))
#'
#' # Combine with advanced residual analysis
#' adv <- analyzeMLResiduals(m, mtcars)
runHeteroTests <- function(model, data = NULL,
                           tests = c("white", "breusch_pagan")) {
  checkModel(model)
  if (is.null(data)) {
    data <- model.frame(model)
  } else {
    checkData(data)
  }
  available <- as.list(.diagnostic_registry)
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop("Unknown tests: ", paste(invalid, collapse = ", "))
  }
  res <- lapply(tests, function(t) available[[t]](model, data))
  names(res) <- tests
  res
}
