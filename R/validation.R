#' Comprehensive input validation for diagnostic tests
#'
#' Performs a series of checks on a fitted model and data
#' before running heteroscedasticity diagnostics. It reports
#' errors for invalid inputs and common data issues, and emits
#' warnings for potential problems such as outliers or large
#' datasets.
#'
#' @param model A fitted model of class `lm` or `glm`.
#' @param data Data frame used to fit the model.
#' @param test_name Name of the diagnostic test (for messages).
#' @param min_obs Minimum number of observations required.
#' @keywords internal
validateTestInputs <- function(model, data, test_name, min_obs = 10) {
  errors <- character()
  warnings <- character()

  if (!inherits(model, c("lm", "glm"))) {
    errors <- c(errors, "Model must be fitted with lm() or glm()")
  }
  if (!is.data.frame(data)) {
    errors <- c(errors, "Data must be a data.frame")
  }
  if (nrow(data) < min_obs) {
    errors <- c(errors, sprintf(
      "Insufficient observations: %d (minimum: %d)",
      nrow(data), min_obs
    ))
  }

  if (length(errors) == 0) {
    resid <- residuals(model)

    if (any(is.na(resid))) {
      errors <- c(errors, "Model contains NA residuals")
    }
    if (safe_var(resid) <= .Machine$double.eps) {
      errors <- c(errors, "Residual variance is essentially zero")
    }
    if (any(abs(scale(resid)) > 5)) {
      warnings <- c(warnings, "Extreme outliers detected (|z| > 5)")
    }
    if (nrow(data) > 10000) {
      warnings <- c(warnings, "Large dataset - consider computational implications")
    }
  }

  if (length(errors) > 0) {
    stop(sprintf(
      "Validation failed for %s:\n%s", test_name,
      paste("  -", errors, collapse = "\n")
    ), call. = FALSE)
  }

  if (length(warnings) > 0) {
    for (w in warnings) warning(w, call. = FALSE)
  }

  invisible(TRUE)
}

#' Enhanced model diagnostics
#'
#' Extends `checkModel()` with additional warnings about
#' degrees of freedom, perfect fits and multicollinearity.
#'
#' @inheritParams validateTestInputs
#' @keywords internal
checkModelEnhanced <- function(model, data = NULL) {
  checkModel(model)

  if (model$df.residual < 5) {
    warning("Very few residual degrees of freedom (", model$df.residual, ")",
      call. = FALSE
    )
  }

  if (inherits(model, "lm") && summary(model)$r.squared > 0.999) {
    warning("Near-perfect fit detected - heteroscedasticity tests may be unreliable",
      call. = FALSE
    )
  }

  if (!is.null(data)) {
    vif_vals <- tryCatch(performVIFDiagnostic(model), error = function(e) NULL)
    if (!is.null(vif_vals) && any(vif_vals > 10, na.rm = TRUE)) {
      warning("High multicollinearity detected (VIF > 10)", call. = FALSE)
    }
  }

  invisible(model)
}
