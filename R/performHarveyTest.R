#' Perform Harvey test for heteroscedasticity
#'
#' The Harvey test regresses the log of squared residuals on the fitted values
#' and their squares. A significant regression indicates heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performHarveyTest(m)
performHarveyTest <- function(model) {
  checkModel(model)

  yhat <- fitted(model)
  e2 <- residuals(model)^2
  e2 <- pmax(e2, .Machine$double.eps)
  aux_model <- lm(log(e2) ~ yhat + I(yhat^2))
  R2 <- summary(aux_model)$r.squared
  df_num <- 2
  df_den <- aux_model$df.residual
  F_stat <- (R2 / df_num) / ((1 - R2) / df_den)
  p_value <- 1 - pf(F_stat, df_num, df_den)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Harvey test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
