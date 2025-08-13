#' Perform Spread-Level test
#'
#' Fits a simple regression of log(|residuals|) on log(fitted) values.
#' A slope significantly different from zero indicates heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the t statistic for the slope.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSpreadLevelTest(m)
performSpreadLevelTest <- function(model) {
  checkModel(model)

  abs_res <- abs(residuals(model))
  abs_res <- pmax(abs_res, .Machine$double.eps)
  fit <- abs(fitted(model))
  fit <- pmax(fit, .Machine$double.eps)
  reg <- lm(log(abs_res) ~ log(fit))
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
      method = "Spread-Level test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
