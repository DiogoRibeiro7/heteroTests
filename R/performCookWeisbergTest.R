#' Perform Cook–Weisberg test for heteroscedasticity
#'
#' Implements the Cook and Weisberg (1983) score test in which the variance model
#' contains the fitted values as its sole regressor. This is the diagnostic
#' returned by Stata's `estat hettest` command with the `fitted` option and by
#' `car::ncvTest()` with its default arguments.
#'
#' @param model A fitted [stats::lm] object whose residuals will be analysed for
#'   variance patterns.
#'
#' @return An object of class \code{htest} containing the score
#'   statistic, its single degree of freedom, and the p-value for the null
#'   hypothesis of homoskedasticity.
#'
#' @details
#' Writing \eqn{\hat{\sigma}^2 = \sum_i \hat{e}_i^2 / n} for the maximum-likelihood
#' error variance, the test regresses the scaled squared residuals
#' \eqn{\hat{e}_i^2 / \hat{\sigma}^2} on the fitted values and refers half the
#' explained sum of squares to a chi-squared distribution with one degree of
#' freedom. Because the variance model uses a single regressor the test is a quick
#' screening tool for heteroscedasticity that grows with the level of the
#' dependent variable, and it is exactly [performNCVTest()] with its default
#' variance model.
#'
#' The chi-squared reference distribution follows from the null variance of
#' \eqn{\hat{e}^2 / \sigma^2} being \eqn{2}, which holds under normal errors. When
#' that assumption is doubtful, prefer [performKoenkerTest()], whose studentized
#' statistic replaces the constant with a consistent estimate.
#'
#' @references
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' Wooldridge, J. M. (2020). *Introductory Econometrics: A Modern Approach*
#' (7th ed.). Cengage Learning. Section 8.3 discusses the Cook–Weisberg test.
#'
#' @section Validation:
#' Reproduces `car::ncvTest()` to within `1e-8`; see
#' `tests/testthat/test-pass-a-reference.R`. Releases before 0.7.0 returned
#' \eqn{n R^2} from regressing the raw squared residuals on the fitted values.
#' That is the studentized (Koenker) statistic — Stata's `estat hettest, iid
#' fitted` — not the Cook–Weisberg score test the documentation described; see
#' `NEWS.md`.
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
#' [performNCVTest()] for the same score test against an arbitrary variance
#' model, [performBPTest()] for the general Breusch–Pagan regression, and
#' [performWhiteTest()] for a higher-order omnibus alternative.
performCookWeisbergTest <- function(model) {
  rvalidateModelInputs(model, test_name = "Cook-Weisberg", min_obs = 10L)

  ht_log("INFO", "Running Cook-Weisberg test")

  # The Cook-Weisberg statistic is the score test of performNCVTest() with the
  # fitted values as the sole variance regressor; delegate so the two exports
  # cannot drift apart.
  result <- performNCVTest(model)
  result$method <- "Cook-Weisberg test for heteroscedasticity"
  result$data.name <- deparse(stats::formula(model))
  result$alternative <- "error variance depends on the fitted values"
  result
}
