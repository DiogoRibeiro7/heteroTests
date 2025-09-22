#' Perform Glejser test for heteroscedasticity
#'
#' This test regresses the absolute residuals of a fitted linear model on a
#' transformation of a suspected explanatory variable. A significant slope
#' indicates heteroscedasticity related to that variable.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param variable Character. Name of the suspected variable.
#' @param transformation Transformation to apply to `variable`. One of
#'   "abs", "sqrt", "inverse", "inverse_sqrt".
#'
#' @return An object of class \code{htest} with the t statistic and p-value.
#'
#' @details
#' Integrates the shared validation utilities so that model and data integrity
#' are checked prior to fitting the auxiliary regression. Missing values in the
#' model variables or suspected regressor are dropped with an informative
#' warning. Transformation-specific requirements (e.g. positivity for the
#' inverse-square-root case) are enforced via [rvalidateTestRequirements()].
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performGlejserTest(m, mtcars, "wt")
performGlejserTest <- function(model, data, variable,
                               transformation = c("abs", "sqrt", "inverse", "inverse_sqrt")) {
  test_label <- "Glejser test"

  if (!is.character(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Glejser", min_obs = 12L)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), variable))

  transformation <- match.arg(transformation)

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 12L,
    min_obs_data = 12L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  assumption_cfg <- list()
  if (transformation == "inverse_sqrt") {
    assumption_cfg$positive <- list(variables = variable, test_name = test_label)
  }

  requirements <- rvalidateTestRequirements(
    "glejser",
    model = model,
    data = working_data,
    assumptions = assumption_cfg
  )
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Glejser test")

  x <- working_data[[variable]]
  if (!is.numeric(x)) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Variable '%s' must be numeric to evaluate the Glejser test", variable)
    )
  }

  if (transformation %in% c("sqrt", "inverse_sqrt") && any(x < 0, na.rm = TRUE)) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Transformation '%s' requires non-negative values in '%s'", transformation, variable)
    )
  }

  if (transformation %in% c("inverse", "inverse_sqrt") && any(abs(x) <= .Machine$double.eps, na.rm = TRUE)) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Transformation '%s' is undefined when '%s' is zero", transformation, variable)
    )
  }

  z <- switch(transformation,
    abs = abs(x),
    sqrt = sqrt(x),
    inverse = 1 / x,
    inverse_sqrt = 1 / sqrt(x)
  )

  if (any(!is.finite(z))) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Transformation '%s' produced non-finite values", transformation)
    )
  }

  abs_res <- abs(residuals)
  if (stats::var(abs_res) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Glejser test requires variability in absolute residuals"
    )
  }

  aux_data <- data.frame(abs_res = abs_res, z = z)
  aux_model <- safe_lm(abs_res ~ z, data = aux_data)
  coef_summary <- summary(aux_model)$coefficients
  t_stat <- coef_summary[2, 3]
  p_value <- coef_summary[2, 4]
  df <- aux_model$df.residual

  structure(
    list(
      statistic = c(t = t_stat),
      parameter = df,
      p.value = p_value,
      method = "Glejser test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
