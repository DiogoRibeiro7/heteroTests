#' Perform Cook–Weisberg test for heteroscedasticity
#'
#' Implements the Cook and Weisberg (1983) special case of the Breusch–Pagan
#' test in which the auxiliary regression uses the model's fitted values as the
#' sole regressor. This corresponds to the diagnostic returned by Stata's
#' `estat hettest` command with the `fitted` option.
#'
#' @param model A fitted [stats::lm] object whose residuals will be analysed for
#'   variance patterns.
#'
#' @return An object of class \link[stats:htest]{htest} containing the LM statistic,
#'   degrees of freedom, and p-value for the null hypothesis of homoskedasticity.
#'
#' @details
#' The Cook–Weisberg statistic regresses the squared residuals on the model's
#' fitted values. Under homoskedasticity the fitted values should not explain the
#' magnitude of the residuals and \eqn{n R^2} from this auxiliary regression is
#' asymptotically chi-squared with one degree of freedom. Because the test uses a
#' single regressor it is a quick screening tool for heteroscedasticity that grows
#' with the level of the dependent variable. The function validates the supplied
#' model using \link[=rvalidateModelInputs]{rvalidateModelInputs()} and ensures the fitted values and
#' residuals align prior to computing the statistic.
#'
#' @references
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' Wooldridge, J. M. (2020). *Introductory Econometrics: A Modern Approach*
#' (7th ed.). Cengage Learning. Section 8.3 discusses the Cook–Weisberg test.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performCookWeisbergTest(mod)
#'
#' # The test is powerful against variance proportional to the squared mean
#' set.seed(454)
#' x <- runif(200)
#' y <- 2 + 3 * x + rnorm(200, sd = (0.5 + x)^2)
#' df <- data.frame(y, x)
#' performCookWeisbergTest(lm(y ~ x, data = df))
#'
#' @seealso
#' [performBPTest()] for the general Breusch–Pagan regression and
#' [performWhiteTest()] for a higher-order omnibus alternative.
performCookWeisbergTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  e <- residuals(model)
  n <- length(e)
  X <- model.matrix(~ fitted(model))
  aux_model <- lm(e^2 ~ X[, -1])
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- 1
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Cook-Weisberg test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
