#' Run panel-data heteroscedasticity tests
#'
#' Provides Breusch-Pagan LM and Pesaran CD tests for panel models.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`. The two tests read `id` and
#'   `time` from the rows of `data` that hold the observations of the fit:
#'   since 0.16.0, data that are given are matched to the observations of the
#'   fit, by row name in any order, or by the row numbers or in the order of
#'   the fit for rows that only number themselves, as described under `data`
#'   in [performPesaranTest()].
#' @param id Character. Column name identifying individuals.
#' @param time Character. Column name identifying time periods for the Pesaran
#'   test, whose pairs of `id` and `time` must each hold one observation of the
#'   fit.
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
  # Anything but an lm or a glm is refused with the message of the tests.
  rrefuse_model_class(model, "runPanelTests")
  checkModel(model)
  .check_data(data)
  if (!id %in% names(data)) {
    stop("`id` must be a column in `data`.")
  }
  # The two tests match the rows of `data` to the observations of the fit and
  # read `id` and `time` at the rows they confirmed (R/model_context.R); their
  # refusals stop the runner, which runs no other test in their place.
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
