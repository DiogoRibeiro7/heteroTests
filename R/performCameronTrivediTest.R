#' Perform Cameron-Trivedi decomposition test
#'
#' Decomposes heteroscedasticity into linear and non-linear components using
#' successive regressions of squared residuals.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @details
#' Validates the supplied model via [rvalidateModelInputs()] and enforces
#' Cameron-Trivedi-specific requirements through [rvalidateTestRequirements()].
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performCameronTrivediTest(m)
performCameronTrivediTest <- function(model) {
  rvalidateModelInputs(model, test_name = "Cameron-Trivedi", min_obs = 15L)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("cameron_trivedi", model = model, data = model_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Cameron-Trivedi test")

  yhat <- stats::fitted(model)
  if (stats::var(yhat) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi test requires variability in fitted values"
    )
  }

  e2 <- stats::residuals(model)^2
  if (stats::var(e2) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi test requires variability in squared residuals"
    )
  }

  aux_data <- data.frame(e2 = e2, yhat = yhat)
  aux_model <- safe_lm(e2 ~ yhat + I(yhat^2), data = aux_data)
  R2 <- summary(aux_model)$r.squared
  df_num <- 2
  df_den <- aux_model$df.residual

  if (df_den <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi auxiliary regression requires positive residual degrees of freedom"
    )
  }

  F_stat <- (R2 / df_num) / ((1 - R2) / df_den)
  p_value <- 1 - stats::pf(F_stat, df_num, df_den)
  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Cameron-Trivedi decomposition test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
