#' Perform Cameron-Trivedi decomposition test
#'
#' Decomposes heteroscedasticity into linear and non-linear components using
#' successive regressions of squared residuals.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performCameronTrivediTest(m)
performCameronTrivediTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  yhat <- fitted(model)
  e2 <- residuals(model)^2
  aux_model <- lm(e2 ~ yhat + I(yhat^2))
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
      method = "Cameron-Trivedi decomposition test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
