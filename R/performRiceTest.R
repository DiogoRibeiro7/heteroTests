#' Perform Rice test for heteroscedasticity
#'
#' Implements the Rice (1980) test based on successive differences of residuals to
#' detect gradual changes in variance across an ordered sample.
#'
#' @param model A fitted [stats::lm] object (or compatible regression object)
#'   providing residuals whose ordering carries meaning, such as time-series or
#'   spatially ordered data.
#'
#' @return A \link[stats:htest]{htest} object containing the F statistic, degrees of freedom,
#'   and p-value for the null hypothesis of constant variance.
#'
#' @details
#' Let \eqn{\hat{e}_t} denote the residuals in their natural order. The Rice test
#' compares the variance of successive differences \eqn{\Delta \hat{e}_t = \hat{e}_t
#' - \hat{e}_{t-1}} to twice the residual variance. The statistic
#' \deqn{F = \frac{\sum_{t = 2}^n (\hat{e}_t - \hat{e}_{t-1})^2 / (n - 1)}{2\,
#'   \sum_{t = 1}^n \hat{e}_t^2 / n}}
#' follows, under homoskedasticity, an F distribution with \eqn{n - 1} and \eqn{n}
#' degrees of freedom. Large values suggest that neighbouring observations exhibit
#' different residual variances, a pattern typical of slowly evolving volatility.
#'
#' The diagnostic is particularly useful for ordered data where variance changes
#' smoothly rather than abruptly; practitioners often pair it with portmanteau
#' tests such as [performMcLeodLiTest()].
#'
#' @references
#' Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*. Cambridge
#' University Press. Section 5.5 discusses the Rice test.
#'
#' Harvey, A. C. (1990). *The Econometric Analysis of Time Series* (2nd ed.).
#' MIT Press. Chapter 4 reviews difference-based variance diagnostics.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performRiceTest(mod)
#'
#' # Simulated data with a variance break around the midpoint
#' set.seed(512)
#' x <- runif(160)
#' eps <- c(rnorm(80, sd = 0.4), rnorm(80, sd = 0.9))
#' y <- 1 + 0.5 * x + eps
#' performRiceTest(lm(y ~ x))
#'
#' @seealso
#' [performMcLeodLiTest()] for an alternative based on residual autocorrelations
#' and [performArchLMTest()] for LM diagnostics of conditional heteroscedasticity.
performRiceTest <- function(model) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }

  res <- residuals(model)
  n <- length(res)
  diff_res <- diff(res)
  num <- sum(diff_res^2) / (n - 1)
  den <- 2 * mean(res^2)
  F_stat <- num / den
  df1 <- n - 1
  df2 <- n
  p_value <- 1 - pf(F_stat, df1, df2)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df1, df2 = df2),
      p.value = p_value,
      method = "Rice test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
