#' Perform O'Brien test for equality of variances
#'
#' This test modifies Levene's procedure to improve power when data are nearly normal.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performOBrienTest(m, mtcars, "cyl")
performOBrienTest <- function(model, data, group) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!group %in% names(data)) {
    stop("`group` must be a column in `data`.")
  }

  grp <- factor(data[[group]])
  res <- residuals(model)
  n_i <- tapply(res, grp, length)
  s_i2 <- tapply(res, grp, var)
  m_i <- tapply(res, grp, mean)
  z_i <- (n_i - 1.5) * s_i2 / ((n_i - 1) * (n_i - 1))
  z <- rep(z_i, times = n_i)
  aov_model <- lm(z ~ grp)
  aov_table <- anova(aov_model)
  F_stat <- aov_table$`F value`[1]
  df_num <- aov_table$Df[1]
  df_den <- aov_table$Df[2]
  p_value <- aov_table$`Pr(>F)`[1]

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "O'Brien test for equality of variances",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
