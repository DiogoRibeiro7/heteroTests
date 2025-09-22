#' Perform McLeod-Li test for heteroscedasticity
#'
#' Applies the Ljung-Box test to squared residuals of a linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param lags Number of lags to use in the Ljung-Box test.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' Leverages the validation framework to ensure the fitted model provides enough
#' observations relative to the requested lag order and that the squared
#' residuals exhibit sufficient variation.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performMcLeodLiTest(m, lags = 10)
performMcLeodLiTest <- function(model, lags = 10) {
  if (!is.numeric(lags) || length(lags) != 1L || is.na(lags) || lags < 1) {
    stop("`lags` must be a positive integer.", call. = FALSE)
  }
  lags <- as.integer(lags)

  min_required <- rTEST_REQUIREMENTS$mcleod_li(lags)
  rvalidateModelInputs(model, test_name = "McLeod-Li", min_obs = min_required)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("mcleod_li", model = model, data = model_data, lags = lags)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running McLeod-Li test")

  res2 <- stats::residuals(model)^2
  if (stats::var(res2) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "McLeod-Li test requires variability in squared residuals"
    )
  }

  if (lags >= length(res2)) {
    stop("`lags` too large for the number of observations.", call. = FALSE)
  }

  lb <- stats::Box.test(res2, lag = lags, type = "Ljung-Box")
  if (anyNA(c(lb$statistic, lb$parameter, lb$p.value))) {
    std_error(
      "rassumption_violation",
      assumption = "McLeod-Li test could not compute the Ljung-Box statistic"
    )
  }

  structure(
    list(
      statistic = c("X-squared" = unname(lb$statistic)),
      parameter = unname(lb$parameter),
      p.value = lb$p.value,
      method = "McLeod-Li test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
