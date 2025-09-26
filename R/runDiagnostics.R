#' Run a broader suite of diagnostics
#'
#' This convenience wrapper extends `runHeteroTests()` by including checks for
#' multicollinearity, nonlinearity and influential observations.
#'
#' @param model A fitted `lm`/`glm`, a formula, tidymodels workflow or parsnip
#'   model fit compatible with [runHeteroTests()].
#' @param data Optional data associated with `model`. Required when the model is
#'   specified via a formula and used when additional preprocessing (e.g.
#'   grouped data or data.table objects) is desired.
#' @param tests Character vector of heteroscedasticity tests passed to
#'   `runHeteroTests`.
#' @param power Powers for `performRESETTest`.
#' @param use_cache Logical flag forwarded to [runHeteroTests()] controlling
#'   whether cached diagnostic results may be reused.
#' @param chunk_threshold_mb Numeric threshold passed to [runHeteroTests()] for
#'   enabling streaming diagnostics when the input data exceed this size.
#' @param chunk_size Integer size of each chunk when streaming diagnostics.
#' @param progress Logical controlling whether textual progress indicators are
#'   displayed during long-running operations.
#' @return A named list containing results of heteroscedasticity tests and other diagnostics.
#' @seealso \code{\link{runHeteroTests}}, \code{\link{analyzeMLResiduals}},
#'   \code{\link{runPanelTests}}, \code{\link{runTimeSeriesTests}}
#' @examples
#' data(mtcars)
#' runDiagnostics(mpg ~ wt + qsec, mtcars)
runDiagnostics <- function(model, data = NULL,
                           tests = c("white", "breusch_pagan"),
                           power = 2:3,
                           use_cache = TRUE,
                           chunk_threshold_mb = 100,
                           chunk_size = 10000,
                           progress = interactive()) {
  prepared <- .ht_prepare_model(model, data = data, context = "runDiagnostics", allow_grouped = FALSE)
  model <- prepared$model
  data <- prepared$data %||% tryCatch(model.frame(model), error = function(e) NULL)
  hetero <- runHeteroTests(
    model,
    data,
    tests,
    use_cache = use_cache,
    chunk_threshold_mb = chunk_threshold_mb,
    chunk_size = chunk_size,
    progress = progress
  )
  vif <- performVIFDiagnostic(model)
  reset <- performRESETTest(model, power = power)
  influence <- performInfluenceDiagnostics(model)
  c(hetero, list(vif = vif, reset = reset, influence = influence))
}
