#' Perform Non-Constant Variance (NCV) test
#'
#' This function approximates the NCV test from the `car` package by regressing
#' the absolute residuals on the fitted values and testing the slope.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the t statistic and p-value for the slope.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performNCVTest(m)
performNCVTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  abs_res <- abs(residuals(model))
  fit <- fitted(model)
  reg <- lm(abs_res ~ fit)
  slope <- coef(summary(reg))[2, 1]
  se <- coef(summary(reg))[2, 2]
  t_value <- slope / se
  df <- df.residual(reg)
  p_value <- 2 * (1 - pt(abs(t_value), df))

  structure(
    list(
      statistic = c(t = t_value),
      parameter = df,
      p.value = p_value,
      method = "NCV test via absolute residual regression",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
