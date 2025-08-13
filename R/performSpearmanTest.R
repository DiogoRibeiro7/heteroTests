#' Perform Spearman rank correlation test for heteroscedasticity
#'
#' This test computes Spearman's rho between the absolute residuals of a
#' linear model and its fitted values. A significant correlation suggests
#' monotonic heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSpearmanTest(m)
performSpearmanTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  abs_res <- abs(residuals(model))
  fit <- fitted(model)
  rho <- cor(abs_res, fit, method = "spearman")
  n <- length(abs_res)
  t_statistic <- rho * sqrt((n - 2) / (1 - rho^2))
  p_value <- 2 * pt(-abs(t_statistic), df = n - 2)

  structure(
    list(
      statistic = c(t = t_statistic),
      parameter = n - 2,
      p.value = p_value,
      method = "Spearman rank correlation test for heteroscedasticity",
      data.name = deparse(formula(model)),
      estimate = c(rho = rho)
    ),
    class = "htest"
  )
}
