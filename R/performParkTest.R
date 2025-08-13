#' Perform Park test for heteroscedasticity
#'
#' The Park test regresses the log of squared residuals from a linear model
#' on the log of a suspected explanatory variable. A significant slope
#' indicates variance proportional to that variable.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param variable Character. Name of the variable suspected to drive the
#'   heteroscedasticity.
#'
#' @return An object of class \code{htest} with the t statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performParkTest(m, mtcars, "wt")
performParkTest <- function(model, data, variable) {
  checkModel(model)
  checkData(data)
  if (!variable %in% names(data)) {
    std_error("missing_variable", variable = variable)
  }
  e <- residuals(model)
  var_vals <- data[[variable]]
  if (any(var_vals <= 0)) {
    std_error("negative_values", variable = variable)
  }
  e2 <- pmax(e^2, .Machine$double.eps)
  dep <- log(e2)
  indep <- log(var_vals)
  aux_model <- lm(dep ~ indep)
  coef_summary <- summary(aux_model)$coefficients
  t_statistic <- coef_summary[2, 3]
  p_value <- coef_summary[2, 4]
  df <- aux_model$df.residual

  structure(
    list(
      statistic = c(t = t_statistic),
      parameter = df,
      p.value = p_value,
      method = "Park test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
