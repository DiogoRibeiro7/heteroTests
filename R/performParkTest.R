#' Perform Park test for heteroscedasticity
#'
#' Implements Park's (1966) log–log regression test for multiplicative
#' heteroscedasticity. The test regresses the log of the squared residuals on the
#' log of a suspected explanatory variable; a significant slope indicates that
#' the variance of the errors scales with that variable.
#'
#' @param model A fitted [stats::lm] object for which heteroscedasticity is to be
#'   assessed.
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `variable` from the
#'   rows of `data` that hold the observations of the fit. Without `data` it is
#'   read from the model frame of `model`, which holds it only when it is a
#'   variable of the model under its own name (`x`, not `log(x)`); a model that
#'   transforms the variable needs `data`, as does a variable the model does not
#'   use. Since 0.16.0, data that are given are matched to the observations of
#'   the fit: rows with row names of their own by name, in any order; rows that
#'   only number themselves (a tibble, a data.table, a data frame whose row
#'   names were reset) by the row numbers of the fit, or in the order of the fit
#'   when they hold one row per observation. The rows the fit dropped may be
#'   there or not. Data whose rows cannot be shown to hold the observations of
#'   the fit are refused; see [test_results].
#' @param variable Character scalar naming the regressor suspected of driving the
#'   heteroscedasticity. The column must be numeric and strictly positive when
#'   using the log transformation.
#'
#' @return An object of class \code{htest} with the t statistic for the slope
#'   coefficient and its two-sided p-value.
#'
#' @details
#' Park (1966) proposed modelling heteroscedastic variance as
#' \eqn{\sigma_i^2 = \sigma^2 z_i^{2\gamma}}. Taking logarithms yields the linear
#' regression \eqn{\log(\hat{e}_i^2) = \alpha + \gamma \log z_i + u_i}. The test
#' examines whether \eqn{\gamma = 0}. This implementation reads the residuals
#' and weights from the fit and `variable` from the rows of `data` that hold
#' its observations (the model context, since 0.16.0):
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()} checks the fitted model,
#' and \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} enforces the
#' positivity constraints required for the logarithmic transformation.
#'
#'
#' @section Interpretation and caveats:
#' Under the null the auxiliary error \eqn{u_i} behaves like a centred
#' \eqn{\log \chi^2_1} variate, which is strongly left-skewed. The reported t
#' statistic is therefore valid only asymptotically; in small samples the
#' reference distribution is approximate. The test also conditions on a single
#' user-chosen variable `variable`, so a non-significant result is evidence
#' against variance scaling with that variable, not against heteroscedasticity in
#' general. See [performBPTest()] or [performWhiteTest()] for omnibus
#' alternatives. Simulated size and power are recorded in
#' `inst/validation/pass-a-size-power.csv`.
#' @references
#' Park, R. E. (1966). Estimation with heteroscedastic error terms. *Econometrica,
#' 34*(4), 888. <https://doi.org/10.2307/1910108>
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
performParkTest <- function(model, data = NULL, variable) {
  test_label <- "Park test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `variable` is read
  # from the rows of `data` that hold the observations, so data taken in the
  # order of the fit must tell those rows apart when it is not a column of the
  # model frame.
  ctx <- rmodel_context(
    model, data, fn = "performParkTest", test_label = test_label,
    reads = if (!missing(variable) && is.character(variable)) variable else character()
  )

  if (!is.character(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Park", min_obs = 10L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The variable, one value per observation in the order of the fit: from the
  # confirmed rows of `data`, or without `data` from the model frame when it
  # holds it under its own name (rcontext_variables()).
  var_vals <- rcontext_variables(ctx, data, c(variable = variable))$variable
  rwarn_incomplete_rows(data, model, also = variable)
  # The Pearson residuals of the fit; a fit with a zero weight is refused
  # there, as before 0.16.0.
  residuals <- rcontext_pearson(ctx)
  rstop_missing_variable(ctx, var_vals, variable, "variable", test_label)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements(
    "park",
    model = model,
    data = working_data,
    suspected_var = variable
  )
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Park test")

  if (!is.numeric(var_vals)) {
    .std_error(
      "rassumption_violation",
      assumption = sprintf("Variable '%s' must be numeric to evaluate the Park test", variable)
    )
  }

  dep <- rlog_squared_residuals(residuals, "Park test")
  indep <- log(var_vals)

  if (any(!is.finite(indep))) {
    .std_error(
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
      parameter = c(df = df),
      p.value = p_value,
      method = "Park test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
