#' Perform Engle's ARCH Lagrange Multiplier test
#'
#' Implements Engle's (1982) score-type test for autoregressive conditional
#' heteroscedasticity (ARCH) by regressing squared residuals on their own lags and
#' evaluating the resulting \eqn{n R^2} statistic against a chi-squared
#' distribution.
#'
#' @param model A fitted [stats::lm] or [stats::arima] object providing the
#'   residual series \eqn{\hat{e}_t}. Objects must implement [stats::residuals]
#'   and optionally [stats::model.frame] so that validation checks can align the
#'   data with the stored residuals.
#' @param lags A single positive integer giving the number of lagged squared
#'   residuals \eqn{\hat{e}_{t-1}^2, \ldots, \hat{e}_{t-q}^2} included in the
#'   auxiliary regression. Typical choices range from 1 to 12 for monthly data.
#'
#' @return An object of class \link[stats:htest]{htest} containing the chi-squared statistic
#'   with \eqn{q} degrees of freedom, the associated p-value, and descriptive
#'   labels for the fitted model.
#'
#' @details
#' Let \eqn{\hat{e}_t} denote the residuals from the conditional mean model. The
#' ARCH LM test fits the auxiliary regression
#' \deqn{\hat{e}_t^2 = \alpha_0 + \alpha_1 \hat{e}_{t-1}^2 + \cdots + \alpha_q \hat{e}_{t-q}^2 + u_t}
#' and computes \eqn{\text{LM} = n R^2}, where \eqn{R^2} is the coefficient of
#' determination from this regression. Under the null hypothesis that the error
#' variance is conditionally homoskedastic (i.e. \eqn{\alpha_1 = \cdots =
#' \alpha_q = 0}) the LM statistic converges to a chi-squared distribution with
#' \eqn{q} degrees of freedom. Rejection indicates the presence of ARCH effects
#' that motivate GARCH-type volatility models.
#'
#' The function integrates the package's validation helpers to ensure:
#' \enumerate{
#'   \item the fitted model supplies at least \eqn{q + 15} observations and finite
#'     residuals via \link[=rvalidateModelInputs]{rvalidateModelInputs()};
#'   \item the optional model frame can be inspected for missingness and
#'     dimensionality using \link[=rvalidateTestRequirements]{rvalidateTestRequirements()}, which also enforces
#'     lag-specific minimum sample sizes; and
#'   \item the squared residuals retain sufficient variation once the lagged
#'     design matrix is constructed.
#' }
#'
#' @references
#' Engle, R. F. (1982). Autoregressive conditional heteroscedasticity with
#' estimates of the variance of United Kingdom inflation. *Econometrica, 50*(4),
#' 987–1007. <https://doi.org/10.2307/1912773>
#'
#' Tsay, R. S. (2010). *Analysis of Financial Time Series* (3rd ed.). Wiley.
#' Chapter 3 discusses LM diagnostics for conditional heteroskedasticity.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performArchLMTest(mod, lags = 2)
#'
#' # Detect ARCH effects in simulated data generated from an ARCH(1) process
#' set.seed(101)
#' innov <- rnorm(300)
#' eps <- numeric(300)
#' for (t in 2:300) {
#'   eps[t] <- sqrt(0.2 + 0.6 * eps[t - 1]^2) * innov[t]
#' }
#' arch_mod <- lm(eps ~ 1)
#' performArchLMTest(arch_mod, lags = 3)
#'
#' # Higher-order diagnostics for GARCH-type behaviour
#' performArchLMTest(arch_mod, lags = 6)
#'
#' @seealso
#' [performMcLeodLiTest()] for the portmanteau alternative based on squared
#' residual autocorrelations and [performPesaranTest()] when cross-sectional
#' dependence is of interest.
performArchLMTest <- function(model, lags = 1) {
  if (!is.numeric(lags) || length(lags) != 1L || is.na(lags) || lags < 1) {
    stop("`lags` must be a positive integer.", call. = FALSE)
  }
  lags <- as.integer(lags)

  min_required <- rTEST_REQUIREMENTS$arch_lm(lags)
  rvalidateModelInputs(model, test_name = "ARCH LM", min_obs = min_required)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("arch_lm", model = model, data = model_data, lags = lags)
  rprocessValidationResult(requirements)

  if (!is.null(model_data)) {
    check_memory_usage(model_data, threshold_mb = 50)
    if (nrow(model_data) > 10000) {
      message(
        "Large dataset (", nrow(model_data), " observations). ",
        "This may take some time to compute."
      )
    }
  }

  ht_log("INFO", "Running ARCH LM test")

  res2 <- stats::residuals(model)^2
  if (stats::var(res2) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "ARCH LM test requires variability in squared residuals"
    )
  }

  n <- length(res2)
  if (lags >= n) {
    stop("`lags` too large for the number of observations.", call. = FALSE)
  }

  embed_mat <- stats::embed(res2, lags + 1)
  y <- embed_mat[, 1]
  X <- embed_mat[, -1, drop = FALSE]

  aux_data <- data.frame(y = y, X)
  aux_model <- safe_lm(y ~ ., data = aux_data)

  r2 <- summary(aux_model)$r.squared
  test_statistic <- length(y) * r2
  df <- lags
  if (df <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "ARCH LM auxiliary regression requires positive degrees of freedom"
    )
  }

  p_value <- 1 - stats::pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Engle's ARCH LM test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
