#' Perform Glejser test for heteroscedasticity
#'
#' Estimates Glejser's (1969) parametric test by regressing the absolute residuals
#' on transformations of a suspected regressor. Significant slopes indicate that
#' the variance of the error term depends on that regressor.
#'
#' @param model A fitted [stats::lm] object whose residuals are examined.
#' @param data [base::data.frame] used to fit `model`.
#' @param variable Character scalar giving the column suspected of driving the
#'   heteroscedasticity.
#' @param transformation Character scalar selecting the transformation applied to
#'   `variable` in the auxiliary regression. One of `"abs"`, `"sqrt"`, `"inverse"`,
#'   or `"inverse_sqrt"`.
#'
#' @return An object of class \code{htest} reporting the t statistic and
#'   p-value for the slope coefficient in the auxiliary regression.
#'
#' @details
#' Glejser suggested modelling heteroscedastic variance by regressing the
#' absolute residuals on transformations of the explanatory variable believed to
#' govern the error scale. The test explores several functional forms (absolute,
#' square root, inverse, inverse square root). This implementation validates the
#' fitted model and data through \link[=rvalidateModelInputs]{rvalidateModelInputs()}, \link[=rvalidateDataInputs]{rvalidateDataInputs()},
#' and \link[=rhandleMissingValues]{rhandleMissingValues()}, then enforces transformation-specific
#' requirements via \link[=rvalidateTestRequirements]{rvalidateTestRequirements()}. The resulting t statistic tests
#' the null hypothesis of no relationship between the transformed regressor and
#' the absolute residuals.
#'
#'
#' @section Interpretation and caveats:
#' The t statistic is valid under symmetric errors. Godfrey (1996) and Im (2000)
#' show that the Glejser test is not asymptotically valid when the errors are
#' asymmetric, because the mean of \eqn{|\hat{e}_i|} then depends on the
#' estimation error in the mean equation, and the test can over-reject. Under
#' skewed errors prefer [performKoenkerTest()] or [performWhiteTest()]. The test
#' also conditions on a single user-chosen variable and a single transformation,
#' so scanning several transformations and reporting only the smallest p-value
#' invalidates the nominal level. Simulated size and power are recorded in
#' `inst/validation/pass-a-size-power.csv`.
#' @references
#' Glejser, H. (1969). A new test for heteroscedasticity. *Journal of the American
#' Statistical Association, 64*(325), 316–323.
#' <https://doi.org/10.1080/01621459.1969.10500976>
#'
#' Gujarati, D. N., & Porter, D. C. (2009). *Basic Econometrics* (5th ed.).
#' McGraw-Hill. Chapter 11 discusses the Glejser procedure.
#'
#' Godfrey, L. G. (1996). Some results on the Glejser and Koenker tests for
#' heteroskedasticity. *Journal of Econometrics, 72*(1-2), 275-299.
#' <https://doi.org/10.1016/0304-4076(94)01722-0>
#'
#' Im, K. S. (2000). Robustifying Glejser test of heteroskedasticity.
#' *Journal of Econometrics, 97*(1), 179-188.
#' <https://doi.org/10.1016/S0304-4076(99)00061-5>
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performGlejserTest(mod, mtcars, "wt")
#'
#' # Examine alternative transformations
#' performGlejserTest(mod, mtcars, "wt", transformation = "inverse")
#'
#' @seealso
#' [performParkTest()] and [performHarveyTest()] provide related parametric tests
#' for specific forms of heteroscedasticity.
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
      parameter = c(df = df),
      p.value = p_value,
      method = "Glejser test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
