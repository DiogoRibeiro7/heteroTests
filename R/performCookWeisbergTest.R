#' Perform Cook-Weisberg test for heteroscedasticity
#'
#' This is an implementation of the Breusch-Pagan test using the fitted values
#' as regressors (Stata's `estat hettest`).
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performCookWeisbergTest(m)
performCookWeisbergTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  e <- residuals(model)
  n <- length(e)
  X <- model.matrix(~ fitted(model))
  aux_model <- lm(e^2 ~ X[, -1])
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- 1
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Cook-Weisberg test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
