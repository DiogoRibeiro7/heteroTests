#' Run panel-data heteroscedasticity tests
#'
#' Provides Breusch-Pagan LM and Pesaran CD tests for panel models.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param id Character. Column name identifying individuals.
#' @param time Character. Column name identifying time periods for the Pesaran test.
#' @param tests Character vector of test names. Defaults to both tests.
#' @return A named list of `htest` objects.
#'
#' @seealso \code{\link{runTimeSeriesTests}} for time-series models and
#'   \code{\link{runDiagnostics}} for additional diagnostics.
#'
#' @examples
#' df <- data.frame(
#'   id = rep(1:5, each = 4), time = rep(1:4, 5),
#'   x = runif(20), y = rnorm(20)
#' )
#' m <- lm(y ~ x, data = df)
#' runPanelTests(m, df, id = "id", time = "time")
#' @export
runPanelTests <- function(model, data, id, time = NULL,
                          tests = c("bp_random", "pesaran")) {
  checkModel(model)
  checkData(data)
  if (!id %in% names(data)) {
    stop("`id` must be a column in `data`.")
  }
  available <- list(
    bp_random = function(m) performBPRandomEffectsTest(m, data, id),
    pesaran = function(m) {
      if (is.null(time) || !time %in% names(data)) {
        stop("`time` must be provided for the Pesaran test")
      }
      performPesaranTest(m, data, id, time)
    }
  )
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop("Unknown tests: ", paste(invalid, collapse = ", "))
  }
  res <- lapply(tests, function(t) available[[t]](model))
  names(res) <- tests
  res
}
