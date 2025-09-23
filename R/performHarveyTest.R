#' Perform Harvey test for heteroscedasticity
#'
#' Applies Harvey's (1976) parametric test for multiplicative heteroscedasticity
#' by regressing the log of squared residuals on the fitted values and their
#' square. Departures from homoskedasticity manifest as a significant auxiliary
#' regression.
#'
#' @param model A fitted [stats::lm] object representing the mean equation under
#'   study.
#'
#' @return An object of class \link[stats:htest]{htest} with the F statistic and p-value for
#'   the null hypothesis that the variance is constant.
#'
#' @details
#' Harvey (1976) proposed modelling variance as \eqn{\sigma_i^2 = \sigma^2 
#' \exp(\gamma_1 \hat{y}_i + \gamma_2 \hat{y}_i^2)}. Taking logarithms yields the
#' auxiliary regression used here. The implementation validates the model via
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()}, aligns the data using \link[=rhandleMissingValues]{rhandleMissingValues()}, and
#' applies \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} to ensure sufficient sample size and
#' variability in the fitted values. The resulting \eqn{n R^2} is converted into an
#' F statistic with two and \eqn{n - p} degrees of freedom.
#' 
#' @references
#' Harvey, A. C. (1976). Estimating regression models with multiplicative
#' heteroscedasticity. *Econometrica, 44*(3), 461–465.
#' <https://doi.org/10.2307/1913974>
#'
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performHarveyTest(mod)
#'
#' # Compare with the Park test on the same model
#' performParkTest(mod, mtcars, "wt")
#'
#' @seealso
#' [performParkTest()] and [performGlejserTest()] implement related variance
#' function diagnostics.
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
