#' Perform Rice test for heteroscedasticity
#'
#' Uses finite differences of residuals to test for non-constant variance.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performRiceTest(m)
performRiceTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  res <- residuals(model)
  n <- length(res)
  diff_res <- diff(res)
  num <- sum(diff_res^2) / (n - 1)
  den <- 2 * mean(res^2)
  F_stat <- num / den
  df1 <- n - 1
  df2 <- n
  p_value <- 1 - pf(F_stat, df1, df2)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df1, df2 = df2),
      p.value = p_value,
      method = "Rice test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
