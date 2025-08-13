#' Automatically compare remedial models
#'
#' Fits several remedial models for heteroscedasticity and compares
#' their performance using AIC and residual RMSE. Currently evaluates
#' weighted least squares and robust regression against the original
#' model.
#'
#' @param model Fitted `lm` model or formula.
#' @param data Optional data frame if `model` is a formula.
#' @return A list with components `metrics`, `models`, and `best` indicating
#'   the recommended method.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, mtcars)
#' autoCompareRemediations(m)
#' @export
autoCompareRemediations <- function(model, data = NULL) {
  if (inherits(model, "formula")) {
    if (is.null(data)) {
      stop("`data` must be supplied when `model` is a formula")
    }
    checkData(data)
    lm_model <- lm(model, data = data)
  } else {
    checkModel(model)
    lm_model <- model
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      checkData(data)
    }
  }

  wls_model <- tryCatch(fitWLS(lm_model), error = function(e) NULL)
  robust_model <- tryCatch(fitRobust(lm_model), error = function(e) NULL)

  models <- list(OLS = lm_model, WLS = wls_model, Robust = robust_model)

  rmse <- function(x) sqrt(mean(x^2))
  metrics <- data.frame(
    Method = names(models),
    AIC = sapply(models, function(m) if (!is.null(m)) AIC(m) else NA_real_),
    RMSE = sapply(models, function(m) if (!is.null(m)) rmse(resid(m)) else NA_real_),
    stringsAsFactors = FALSE
  )

  best_idx <- which.min(metrics$AIC)
  metrics$Recommended <- FALSE
  if (length(best_idx) && is.finite(metrics$AIC[best_idx])) {
    metrics$Recommended[best_idx] <- TRUE
    best_method <- metrics$Method[best_idx]
  } else {
    best_method <- NA_character_
  }

  list(models = models, metrics = metrics, best = best_method)
}
