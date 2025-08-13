#' Perform Engle's ARCH LM test
#'
#' Regresses squared residuals on their lags to detect ARCH effects.
#'
#' @param model A fitted model of class `lm`.
#' @param lags Number of lags to include in the auxiliary regression.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performArchLMTest(m, lags = 2)
performArchLMTest <- function(model, lags = 1) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.numeric(lags) || lags < 1) {
    stop("`lags` must be a positive integer.")
  }

  res2 <- residuals(model)^2
  n <- length(res2)
  if (lags >= n) {
    stop("`lags` too large for the number of observations.")
  }
  embed_mat <- stats::embed(res2, lags + 1)
  y <- embed_mat[, 1]
  X <- embed_mat[, -1, drop = FALSE]
  aux_model <- lm(y ~ X)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- length(y) * r2
  df <- lags
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Engle's ARCH LM test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
