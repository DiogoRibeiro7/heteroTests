#' Perform Levene's test for equality of variances
#'
#' Levene's test assesses whether the variances across groups defined by a
#' categorical variable are equal. The test is performed on the absolute
#' deviations from the group means of the residuals from a linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @references
#' Levene, H. (1960). Robust tests for equality of variances. In
#' \emph{Contributions to Probability and Statistics} (pp. 278-292).
#' Stanford University Press.
#'
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality
#' of variances. \emph{Journal of the American Statistical Association},
#' 69(346), 364-367. \doi{10.1080/01621459.1974.10482955}
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performLeveneTest(m, mtcars, "cyl")
performLeveneTest <- function(model, data, group) {
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
  means <- tapply(res, grp, mean)
  z <- abs(res - means[grp])
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
      method = "Levene's test for equality of variances",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
