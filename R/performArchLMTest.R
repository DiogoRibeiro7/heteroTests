#' Perform Engle's ARCH LM test
#'
#' Regresses squared residuals on their lags to detect ARCH effects.
#'
#' @param model A fitted model of class `lm`.
#' @param lags Number of lags to include in the auxiliary regression.
#' 
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' Incorporates the shared validation framework to check the fitted model,
#' enforce lag-dependent sample size requirements, and warn about large data
#' sets before constructing the auxiliary regression.
#' 
#' @references
#' Engle, R. F. (1982). Autoregressive conditional heteroscedasticity with
#' estimates of the variance of United Kingdom inflation. \emph{Econometrica},
#' 50(4), 987-1007. \doi{10.2307/1912773}
#' 
#' Hamilton, J. D. (1994). \emph{Time Series Analysis}. Princeton University Press.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performArchLMTest(m, lags = 2)
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
