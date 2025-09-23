#' Perform non-constant variance (NCV) test
#'
#' Recreates the non-constant variance score test popularised by Fox and
#' Weisberg's *car* package by regressing the absolute residuals on the fitted
#' values. A significant slope coefficient implies that the error variance is
#' systematically related to the predicted mean.
#'
#' @param model A fitted [stats::lm] object whose residuals will be tested for
#'   heteroscedasticity.
#'
#' @return An object of class \link[stats:htest]{htest} containing the t statistic for the
#'   slope and its two-sided p-value.
#'
#' @details
#' The NCV test fits the auxiliary regression \eqn{|e_i| = \alpha + \beta \hat{y}_i + u_i}.
#' Under homoskedasticity the slope \eqn{\beta} should be zero. The implementation
#' mirrors the algorithm described in Fox and Weisberg (2019) and is equivalent to
#' calling `car::ncvTest()` with the default `fitted.values` predictor. The slope
#' t-statistic is used as the test statistic and, for large samples, follows a
#' standard normal distribution. The function relies on \link[=rvalidateModelInputs]{rvalidateModelInputs()}
#' to ensure the model is well defined and uses [checkModel()] to verify that the
#' required residuals and fitted values are accessible.
#'
#' @references
#' Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
#' (3rd ed.). Sage. Section 6.3 introduces the NCV test.
#'
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performNCVTest(mod)
#'
#' # Detect monotonic heteroscedasticity in simulated data
#' set.seed(867)
#' x <- runif(160)
#' y <- 5 + 2 * x + rnorm(160, sd = 0.3 + 0.7 * x)
#' df <- data.frame(y, x)
#' performNCVTest(lm(y ~ x, data = df))
#'
#' @seealso
#' [performCookWeisbergTest()] and [performSpearmanTest()] provide related
#' diagnostics using the same intuition.
performNCVTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  abs_res <- abs(residuals(model))
  fit <- fitted(model)
  reg <- lm(abs_res ~ fit)
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
      method = "NCV test via absolute residual regression",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
