#' Perform Glejser test for heteroscedasticity
#'
#' This test regresses the absolute residuals of a fitted linear model on a
#' transformation of a suspected explanatory variable. A significant slope
#' indicates heteroscedasticity related to that variable.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param variable Character. Name of the suspected variable.
#' @param transformation Transformation to apply to `variable`. One of
#'   "abs", "sqrt", "inverse", "inverse_sqrt".
#'
#' @return An object of class \code{htest} with the t statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performGlejserTest(m, mtcars, "wt")
performGlejserTest <- function(model, data, variable,
                               transformation = c("abs", "sqrt", "inverse", "inverse_sqrt")) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!variable %in% names(data)) {
    stop("`variable` must exist in `data`.")
  }

  transformation <- match.arg(transformation)
  x <- data[[variable]]
  z <- switch(transformation,
    abs = abs(x),
    sqrt = sqrt(x),
    inverse = 1 / x,
    inverse_sqrt = 1 / sqrt(x)
  )

  aux_model <- lm(abs(residuals(model)) ~ z)
  coef_summary <- summary(aux_model)$coefficients
  t_stat <- coef_summary[2, 3]
  p_value <- coef_summary[2, 4]
  df <- aux_model$df.residual

  structure(
    list(
      statistic = c(t = t_stat),
      parameter = df,
      p.value = p_value,
      method = "Glejser test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
