#' Deprecated HC covariance pseudo-test
#'
#' `performHCCovarianceTest()` previously transformed HC0--HC4 leverage
#' adjustments into an auxiliary-regression statistic and assigned a chi-squared
#' reference distribution. HC0--HC4 are heteroscedasticity-consistent covariance
#' estimators; the cited literature does not justify that construction as a
#' heteroscedasticity hypothesis test.
#'
#' The function is retained temporarily so existing callers receive an explicit
#' migration error rather than silently obtaining invalid inferential output.
#'
#' @inheritParams performBPTest
#' @param type Character string naming an HC covariance estimator. Retained only
#'   for backward-compatible argument matching.
#'
#' @return This function does not return a test result. It signals an error with
#'   migration guidance.
#'
#' @details
#' For testing whether error variance depends on regressors, use established
#' diagnostics such as [performBPTest()], [performKoenkerTest()], or
#' [performWhiteTest()]. For heteroscedasticity-robust coefficient inference,
#' use a covariance estimator such as `sandwich::vcovHC()` directly.
#'
#' @references
#' MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent
#' covariance matrix estimators with improved finite sample properties.
#' *Journal of Econometrics, 29*(3), 305--325.
#'
#' Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
#' standard errors in the linear regression model. *The American Statistician,
#' 54*(3), 217--224.
#'
#' @export
performHCCovarianceTest <- function(model, data,
                                    type = c("HC0", "HC1", "HC2", "HC3", "HC4")) {
  type <- match.arg(type)
  .Deprecated(msg = paste(
    "performHCCovarianceTest() has been withdrawn because HC0--HC4 are",
    "covariance estimators, not a validated family of heteroscedasticity tests."
  ))
  stop(
    paste0(
      "No inferential result is returned. Use performBPTest(), ",
      "performKoenkerTest(), or performWhiteTest() for heteroscedasticity testing; ",
      "use sandwich::vcovHC(model, type = '", type,
      "') for heteroscedasticity-robust coefficient covariance estimation."
    ),
    call. = FALSE
  )
}
