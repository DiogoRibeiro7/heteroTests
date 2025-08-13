#' Perform Davidian-Carroll test for heteroscedasticity
#'
#' Fits a polynomial regression of log(residual^2) on fitted values and tests
#' whether the coefficients are all zero.
#'
#' @param model A fitted model of class `lm`.
#' @param degree Polynomial degree. Default is 2.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performDavidianCarrollTest(m)
performDavidianCarrollTest <- function(model, degree = 2) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.numeric(degree) || degree < 1) {
    stop("`degree` must be a positive integer.")
  }

  res <- residuals(model)
  fit <- fitted(model)
  formula_str <- paste0("log(res^2) ~ poly(fit, ", degree, ")")
  df <- data.frame(res = res, fit = fit)
  aux_model <- lm(as.formula(formula_str), data = df)
  aov_table <- anova(aux_model)
  F_stat <- aov_table$`F value`[1]
  df_num <- aov_table$Df[1]
  df_den <- aov_table$Df[2]
  p_value <- aov_table$`Pr(>F)`[1]

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Davidian-Carroll test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
