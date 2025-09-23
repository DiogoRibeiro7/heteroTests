#' Perform Park test for heteroscedasticity
#'
#' Implements Park's (1966) log–log regression test for multiplicative
#' heteroscedasticity. The test regresses the log of the squared residuals on the
#' log of a suspected explanatory variable; a significant slope indicates that
#' the variance of the errors scales with that variable.
#'
#' @param model A fitted [stats::lm] object for which heteroscedasticity is to be
#'   assessed.
#' @param data A [base::data.frame] containing the variables used to fit `model`.
#' @param variable Character scalar naming the regressor suspected of driving the
#'   heteroscedasticity. The column must be numeric and strictly positive when
#'   using the log transformation.
#'
#' @return An object of class \link[stats:htest]{htest} with the t statistic for the slope
#'   coefficient and its two-sided p-value.
#'
#' @details
#' Park (1966) proposed modelling heteroscedastic variance as
#' \eqn{\sigma_i^2 = \sigma^2 z_i^{2\gamma}}. Taking logarithms yields the linear
#' regression \eqn{\log(\hat{e}_i^2) = \alpha + \gamma \log z_i + u_i}. The test
#' examines whether \eqn{\gamma = 0}. This implementation leverages the shared
#' validation framework: \link[=rvalidateModelInputs]{rvalidateModelInputs()} checks the fitted model,
#' \link[=rvalidateDataInputs]{rvalidateDataInputs()} and \link[=rhandleMissingValues]{rhandleMissingValues()} ensure the data contain
#' the necessary variables, and \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} enforces the
#' positivity constraints required for the logarithmic transformation.
#'
#' @references
#' Park, R. E. (1966). Estimation with heteroscedastic error terms. *Econometrica,
#' 34*(4), 888–898. <https://doi.org/10.2307/1909774>
#'
#' Wooldridge, J. M. (2020). *Introductory Econometrics: A Modern Approach*
#' (7th ed.). Cengage Learning. Section 8.4 reviews the Park test.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performParkTest(mod, mtcars, "wt")
#'
#' # Investigate variance tied to engine displacement
#' performParkTest(mod, mtcars, "disp")
#'
#' @seealso
#' [performHarveyTest()] and [performGlejserTest()] offer related parametric
#' tests based on alternative variance specifications.
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
