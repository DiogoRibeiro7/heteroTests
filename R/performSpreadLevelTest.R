#' Perform spread–level test
#'
#' Evaluates Tukey's spread–level plot diagnostic by regressing the log of the
#' absolute residuals on the log of the fitted values. A non-zero slope indicates
#' that the residual spread changes systematically with the mean.
#'
#' @param model A fitted [stats::lm] object whose residuals are to be assessed.
#'
#' @return An object of class \code{htest} containing the t statistic for the
#'   slope parameter and its two-sided p-value.
#'
#' @details
#' The spread–level transformation linearises power-of-the-mean variance models.
#' After computing \eqn{\log |e_i|} and \eqn{\log |\hat{y}_i|}, the function fits a
#' simple regression and tests whether the slope is zero. The helper
#' [checkModel()] ensures that residuals and fitted values are available and
#' finite. Because the test is essentially the score test from a power variance
#' function, it complements the parametric Harvey and Park diagnostics.
#'
#' @references
#' Tukey, J. W. (1977). *Exploratory Data Analysis*. Addison-Wesley. Chapter 8
#' describes the spread–level approach.
#'
#' Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
#' (3rd ed.). Sage. Section 3.4 discusses spread–level diagnostics.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSpreadLevelTest(mod)
#'
#' # Compare with the NCV test on the same model
#' performNCVTest(mod)
#'
#' @seealso
#' [performNCVTest()] and [performSpearmanTest()] target similar monotonic
#' heteroscedasticity patterns.
performSpreadLevelTest <- function(model) {
  checkModel(model)

  abs_res <- abs(residuals(model))
  abs_res <- pmax(abs_res, .Machine$double.eps)
  fit <- abs(fitted(model))
  fit <- pmax(fit, .Machine$double.eps)
  reg <- lm(log(abs_res) ~ log(fit))
  slope <- coef(summary(reg))[2, 1]
  se <- coef(summary(reg))[2, 2]
  t_value <- slope / se
  df <- df.residual(reg)
  p_value <- 2 * (1 - pt(abs(t_value), df))

  structure(
    list(
      statistic = c(t = t_value),
      parameter = df,
      p.value = p_value,
      method = "Spread-Level test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
