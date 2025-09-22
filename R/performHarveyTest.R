#' Perform Harvey test for heteroscedasticity
#'
#' The Harvey test regresses the log of squared residuals on the fitted values
#' and their squares. A significant regression indicates heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @details
#' Validates the fitted model with [rvalidateModelInputs()] and enforces
#' Harvey-specific requirements, including minimum sample size, via
#' [rvalidateTestRequirements()].
#' 
#' @references
#' Harvey, A. C. (1976). Estimating regression models with multiplicative
#' heteroscedasticity. \emph{Econometrica}, 44(3), 461-465.
#' \doi{10.2307/1913974}
#'
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity
#' in regression. \emph{Biometrika}, 70(1), 1-10. \doi{10.1093/biomet/70.1.1}
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performHarveyTest(m)
performHarveyTest <- function(model) {
  rvalidateModelInputs(model, test_name = "Harvey", min_obs = 15L)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("harvey", model = model, data = model_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Harvey test")

  yhat <- stats::fitted(model)
  if (stats::var(yhat) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey test requires variability in fitted values"
    )
  }

  e2 <- stats::residuals(model)^2
  e2 <- pmax(e2, .Machine$double.eps)
  if (any(!is.finite(e2))) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey test requires finite squared residuals"
    )
  }

  aux_data <- data.frame(e2 = e2, yhat = yhat)
  aux_model <- safe_lm(log(e2) ~ yhat + I(yhat^2), data = aux_data)
  R2 <- summary(aux_model)$r.squared
  df_num <- 2
  df_den <- aux_model$df.residual

  if (df_den <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey auxiliary regression requires positive residual degrees of freedom"
    )
  }

  F_stat <- (R2 / df_num) / ((1 - R2) / df_den)
  p_value <- 1 - stats::pf(F_stat, df_num, df_den)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Harvey test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
