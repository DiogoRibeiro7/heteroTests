#' Perform McLeod-Li test for heteroscedasticity
#'
#' Applies the Ljung-Box test to squared residuals of a linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param lags Number of lags to use in the Ljung-Box test.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performMcLeodLiTest(m, lags = 10)
performMcLeodLiTest <- function(model, lags = 10) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.numeric(lags) || lags < 1) {
    stop("`lags` must be a positive integer.")
  }
  res2 <- residuals(model)^2
  lb <- Box.test(res2, lag = lags, type = "Ljung-Box")
  structure(
    list(
      statistic = c("X-squared" = unname(lb$statistic)),
      parameter = unname(lb$parameter),
      p.value = lb$p.value,
      method = "McLeod-Li test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
