#' Perform Ordered Lagrange Multiplier test
#'
#' Sorts observations by the suspected variable and runs a Breusch-Pagan
#' regression on the ordered data.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param order_by Character. Name of the variable to order the data by.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performOrderedLMTest(m, mtcars, order_by = "wt")
performOrderedLMTest <- function(model, data, order_by) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!order_by %in% names(data)) {
    stop("`order_by` must be a column in `data`.")
  }
  ordered_data <- data[order(data[[order_by]]), ]
  ordered_model <- lm(formula(model), data = ordered_data)
  e <- residuals(ordered_model)
  n <- length(e)
  X <- model.matrix(formula(ordered_model), data = ordered_data)
  aux_data <- data.frame(X[, -1, drop = FALSE])
  aux_model <- lm(e^2 ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - pchisq(test_statistic, df)
  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Ordered Lagrange Multiplier test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
