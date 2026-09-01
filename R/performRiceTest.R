#' Withdrawn Rice difference-based pseudo-test
#'
#' `performRiceTest()` previously compared Rice's (1984) difference-based
#' variance estimator with the mean squared residual and referred the ratio to
#' an F distribution. That construction cannot detect heteroscedasticity.
#'
#' The function is retained so existing callers receive an explicit migration
#' error rather than silently obtaining a test with no power.
#'
#' @param model A fitted [stats::lm] object. Retained only for backward-compatible
#'   argument matching.
#' @param ... Further arguments, ignored.
#'
#' @return This function does not return a test result. It signals an error with
#'   migration guidance.
#'
#' @details
#' For independent residuals with variances \eqn{\sigma_i^2}, the successive
#' difference satisfies
#' \deqn{E[(e_i - e_{i-1})^2] = \sigma_i^2 + \sigma_{i-1}^2,}
#' so Rice's numerator estimates the *mean* of \eqn{\sigma_i^2} -- and so does the
#' mean squared residual in the denominator. The ratio therefore sits at one
#' under any variance pattern, not just under homoscedasticity. In simulation its
#' mean is 1.00 under homoscedasticity, under \eqn{\sigma_i = 0.2 + 1.2 x_i},
#' under \eqn{\sigma_i = \exp(x_i)} (a fifty-fold spread), and under a twenty-fold
#' step change. The former implementation rejected in 0.0% of homoscedastic
#' samples, and adding an ordering argument does not repair it: the statistic is
#' insensitive to heteroscedasticity by construction.
#'
#' Rice's estimator is a tool for estimating the error variance in the presence
#' of a smooth mean function, not a variance-heterogeneity diagnostic.
#'
#' For variance that trends with an ordering variable, use
#' [performSzroeterTest()] or [performGQTest()], both of which are validated
#' against their references and carry documented power. For variance related to
#' the regressors, use [performBPTest()], [performKoenkerTest()] or
#' [performWhiteTest()].
#'
#' @references
#' Rice, J. (1984). Bandwidth choice for nonparametric regression.
#' *The Annals of Statistics, 12*(4), 1215--1230.
#' <https://doi.org/10.1214/aos/1176346788>
#'
#' @examples
#' \dontrun{
#' # Withdrawn: this signals an error with migration guidance.
#' performRiceTest(lm(mpg ~ wt, data = mtcars))
#' }
#'
#' @seealso
#' [performSzroeterTest()] and [performGQTest()] for ordered alternatives.
#'
#' @export
performRiceTest <- function(model, ...) {
  .Deprecated(msg = paste(
    "performRiceTest() has been withdrawn: the ratio of Rice's difference-based",
    "variance estimator to the mean squared residual estimates the same quantity",
    "in both numerator and denominator, so it cannot detect heteroscedasticity."
  ))
  stop(
    paste0(
      "No inferential result is returned. The statistic has no power against ",
      "heteroscedasticity: its expectation is 1 under any variance pattern. ",
      "For variance trending with an ordering variable use performSzroeterTest() ",
      "or performGQTest(); for variance related to the regressors use ",
      "performBPTest(), performKoenkerTest() or performWhiteTest()."
    ),
    call. = FALSE
  )
}
