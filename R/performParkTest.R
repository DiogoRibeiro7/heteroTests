#' Perform Park test for heteroscedasticity
#'
#' The Park test regresses the log of squared residuals from a linear model
#' on the log of a suspected explanatory variable. A significant slope
#' indicates variance proportional to that variable.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param variable Character. Name of the variable suspected to drive the
#'   heteroscedasticity.
#'
#' @return An object of class \code{htest} with the t statistic and p-value.
#'
#' @details
#' The Park test now leverages the shared validation framework: it checks the
#' supplied model with [rvalidateModelInputs()], verifies that `data` contains
#' the necessary variables via [rvalidateDataInputs()], removes incomplete cases
#' with [rhandleMissingValues()], and enforces the positive-value requirement
#' through [rvalidateTestRequirements()].
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performParkTest(m, mtcars, "wt")
performParkTest <- function(model, data, variable) {
  test_label <- "Park test"

  if (!is.character(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Park", min_obs = 10L)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), variable))

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 10L,
    min_obs_data = 10L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements(
    "park",
    model = model,
    data = working_data,
    suspected_var = variable
  )
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Park test")

  var_vals <- working_data[[variable]]
  if (!is.numeric(var_vals)) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Variable '%s' must be numeric to evaluate the Park test", variable)
    )
  }

  e2 <- pmax(residuals^2, .Machine$double.eps)
  dep <- log(e2)
  indep <- log(var_vals)

  if (any(!is.finite(indep))) {
    std_error(
      "rassumption_violation",
      assumption = sprintf("Log transformation of '%s' produced non-finite values", variable)
    )
  }

  aux_data <- data.frame(dep = dep, indep = indep)
  aux_model <- safe_lm(dep ~ indep, data = aux_data)
  coef_summary <- summary(aux_model)$coefficients
  t_statistic <- coef_summary[2, 3]
  p_value <- coef_summary[2, 4]
  df <- aux_model$df.residual

  structure(
    list(
      statistic = c(t = t_statistic),
      parameter = df,
      p.value = p_value,
      method = "Park test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
