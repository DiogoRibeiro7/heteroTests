#' Perform O'Brien test for equality of variances
#'
#' Implements O'Brien's (1979) modification of Levene's test designed to retain
#' power under near-normal conditions by applying a variance-stabilising
#' transformation to group variances before the ANOVA step.
#'
#' @param model A fitted [stats::lm] object.
#' @param data A [base::data.frame] used to fit `model`.
#' @param group Character scalar specifying the grouping variable.
#'
#' @return An object of class \link[stats:htest]{htest} with the F statistic and p-value.
#'
#' @details
#' The procedure transforms each group's sample variance using O'Brien's
#' correction \eqn{Z_i = \frac{(n_i - 1.5)}{(n_i - 1)(n_i - 1)} s_i^2}, then applies an
#' ANOVA across the transformed values. This stabilises the variance of the test
#' statistic when the underlying errors are approximately normal. Because the
#' method assumes finite fourth moments, it is best suited to Gaussian or nearly
#' Gaussian data.
#'
#' @references
#' O'Brien, R. G. (1979). A general ANOVA method for robust tests of additive
#' models for variances. *Journal of the American Statistical Association, 74*(368),
#' 877–880. <https://doi.org/10.1080/01621459.1979.10481047>
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performOBrienTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performLeveneTest()] and [performBrownForsytheTest()] provide complementary
#' variance-equality diagnostics with different robustness properties.
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
