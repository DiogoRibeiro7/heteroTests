#' Perform Goldfeld-Quandt test for heteroscedasticity
#'
#' This function performs the Goldfeld-Quandt test on a fitted linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param order_by Character. Name of the variable to order the data by.
#' @param fraction Numeric. Fraction of observations to omit from the middle
#'   when splitting the ordered data. Defaults to 0.2.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performGQTest(m, mtcars, order_by = "wt")
performGQTest <- function(model, data, order_by, fraction = 0.2) {
  checkModel(model)
  checkData(data)
  ht_log("INFO", "Running Goldfeld-Quandt test")
  if (!order_by %in% names(data)) {
    stop("`order_by` must be a column in `data`.")
  }
  checkNumericVector(fraction, "fraction")
  if (fraction <= 0 || fraction >= 1) {
    stop("`fraction` must be a numeric value between 0 and 1.")
  }

  n <- nrow(data)
  ordered_data <- data[order(data[[order_by]]), ]
  omit_n <- floor(n * fraction)
  group_size <- floor((n - omit_n) / 2)
  if (group_size <= 0) {
    stop("`fraction` leaves no observations for testing.")
  }

  g1_index <- seq_len(group_size)
  g2_index <- seq(from = n - group_size + 1, to = n)

  model1 <- safe_lm(formula(model), data = ordered_data[g1_index, ])
  model2 <- safe_lm(formula(model), data = ordered_data[g2_index, ])

  sse1 <- sum(residuals(model1)^2)
  sse2 <- sum(residuals(model2)^2)
  df1 <- model1$df.residual
  df2 <- model2$df.residual

  if (sse1 > sse2) {
    F_stat <- (sse1 / df1) / (sse2 / df2)
    df_num <- df1
    df_den <- df2
  } else {
    F_stat <- (sse2 / df2) / (sse1 / df1)
    df_num <- df2
    df_den <- df1
  }
  p_value <- 1 - pf(F_stat, df_num, df_den)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Goldfeld-Quandt test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
