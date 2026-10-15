#' Perform Cook–Weisberg test for heteroscedasticity
#'
#' Implements the Cook and Weisberg (1983) score test in which the variance model
#' contains the fitted values as its sole regressor. This is the diagnostic
#' returned by Stata's `estat hettest` command with the `fitted` option and by
#' `car::ncvTest()` with its default arguments.
#'
#' @param model A fitted [stats::lm] object whose residuals will be analysed for
#'   variance patterns.
#' @param data Optional data frame the model was fitted to, in which
#'   `var_formula` is evaluated; its rows are matched to the residuals by row
#'   name. When `NULL`, the default, the model frame of `model` is used.
#'   Without `var_formula` the statistic is computed from the residuals and
#'   fitted values of `model`, and `data` does not change the result.
#' @param var_formula Optional one-sided formula giving the variance model,
#'   evaluated in `data`, for example `~ x1 + x2`. When `NULL`, the default,
#'   the fitted values are the variance regressor and the result is what this
#'   function returned before 0.14.0; otherwise it is what the deprecated
#'   `performNCVTest(model, var_formula)` returned. In 0.14.0 `var_formula`
#'   was the second argument: a formula passed there by position is still read
#'   as `var_formula`, with a warning that shows the new call; that form stops
#'   working in 1.0.0.
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
#' # With the data, the variance model can use variables the mean model does not
#' performCookWeisbergTest(mod, mtcars, var_formula = ~ wt + hp)
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
performCookWeisbergTest <- function(model, data = NULL, var_formula = NULL) {
  if (inherits(data, "formula") && missing(var_formula)) {
    rwarn_positional_call(
      "performCookWeisbergTest", substitute(model), list(var_formula = substitute(data))
    )
    var_formula <- data
    data <- NULL
  }
  rcheck_data_argument(data, "performCookWeisbergTest", model, "Cook-Weisberg")
  rvalidateModelInputs(model, test_name = "Cook-Weisberg", min_obs = 10L)

  ht_log("INFO", "Running Cook-Weisberg test")

  # With a variance model of its own the test is the score test that the
  # deprecated performNCVTest() computed, returned as it returned it.
  if (!is.null(var_formula)) {
    return(.ncv_test(model, var_formula, data))
  }

  # The Cook-Weisberg statistic is the score test of performNCVTest() with the
  # fitted values as the sole variance regressor; delegate so the two cannot
  # drift apart.
  result <- .ncv_test(model)
  result$method <- "Cook-Weisberg test for heteroscedasticity"
  result$data.name <- deparse(stats::formula(model))
  result$alternative <- "error variance depends on the fitted values"
  result
}
