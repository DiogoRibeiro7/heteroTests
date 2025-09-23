#' Perform McLeod–Li portmanteau test for conditional heteroscedasticity
#'
#' Applies the Ljung–Box portmanteau statistic to the squared residuals of a
#' fitted model, as advocated by McLeod and Li (1983), to detect lingering
#' autocorrelation in the second moment that signals ARCH-type behaviour.
#'
#' @param model A fitted [stats::lm] (or other object supported by
#'   [stats::residuals]) whose residuals are tested for conditional variance
#'   dependence.
#' @param lags A single positive integer giving the maximum lag order used in the
#'   portmanteau statistic. Values between 5 and 20 are common in practice.
#'
#' @return An object of class \link[stats:htest]{htest} reporting the chi-squared statistic,
#'   degrees of freedom, and p-value for the null hypothesis of no ARCH effects
#'   up to the requested lag.
#'
#' @details
#' Let \eqn{\hat{e}_t} denote the residual series and \eqn{r_k} the sample
#' autocorrelation of \eqn{\hat{e}_t^2} at lag \eqn{k}. The McLeod–Li test uses the
#' Ljung–Box statistic
#' \deqn{Q(m) = n (n + 2) \sum_{k = 1}^m \frac{r_k^2}{n - k},}
#' which is asymptotically chi-squared with \eqn{m} degrees of freedom under the
#' null of conditionally homoskedastic errors. Significant values indicate
#' remaining serial structure in the squared residuals that motivates ARCH/GARCH
#' modelling. Compared with the LM formulation of Engle's test, the portmanteau
#' statistic aggregates evidence across all lags up to \eqn{m} and is particularly
#' useful when the true order is unknown.
#'
#' The implementation uses the shared validation helpers to confirm that the
#' fitted model supplies enough observations relative to `lags`, that the squared
#' residuals retain variability, and that the Ljung–Box computation succeeds.
#'
#' @references
#' McLeod, A. I., & Li, W. K. (1983). Diagnostic checking ARMA time series models
#' using squared-residual autocorrelations. *Journal of Time Series Analysis, 4*(4),
#' 269–273. <https://doi.org/10.1111/j.1467-9892.1983.tb00373.x>
#'
#' Tsay, R. S. (2010). *Analysis of Financial Time Series* (3rd ed.). Wiley.
#' Section 2.4 details Ljung–Box diagnostics for squared residuals.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performMcLeodLiTest(mod, lags = 10)
#'
#' # Detect serial correlation in squared residuals from simulated ARCH data
#' set.seed(202)
#' innov <- rnorm(250)
#' eps <- numeric(250)
#' for (t in 2:250) {
#'   eps[t] <- sqrt(0.3 + 0.5 * eps[t - 1]^2) * innov[t]
#' }
#' arch_mod <- lm(eps ~ 1)
#' performMcLeodLiTest(arch_mod, lags = 5)
#'
#' # Increase the lag order to capture longer-memory volatility
#' performMcLeodLiTest(arch_mod, lags = 12)
#'
#' @seealso
#' [performArchLMTest()] for the related Lagrange Multiplier version and
#' [performRiceTest()] for a difference-based alternative on ordered data.
performMcLeodLiTest <- function(model, lags = 10) {
  if (!is.numeric(lags) || length(lags) != 1L || is.na(lags) || lags < 1) {
    stop("`lags` must be a positive integer.", call. = FALSE)
  }
  lags <- as.integer(lags)

  min_required <- rTEST_REQUIREMENTS$mcleod_li(lags)
  rvalidateModelInputs(model, test_name = "McLeod-Li", min_obs = min_required)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("mcleod_li", model = model, data = model_data, lags = lags)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running McLeod-Li test")

  res2 <- stats::residuals(model)^2
  if (stats::var(res2) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "McLeod-Li test requires variability in squared residuals"
    )
  }

  if (lags >= length(res2)) {
    stop("`lags` too large for the number of observations.", call. = FALSE)
  }

  lb <- stats::Box.test(res2, lag = lags, type = "Ljung-Box")
  if (anyNA(c(lb$statistic, lb$parameter, lb$p.value))) {
    std_error(
      "rassumption_violation",
      assumption = "McLeod-Li test could not compute the Ljung-Box statistic"
    )
  }

  structure(
    list(
      statistic = c("X-squared" = unname(lb$statistic)),
      parameter = unname(lb$parameter),
      p.value = lb$p.value,
      method = "McLeod-Li test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
