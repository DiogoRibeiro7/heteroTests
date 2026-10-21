#' Perform Glejser test for heteroscedasticity
#'
#' Estimates Glejser's (1969) parametric test by regressing the absolute residuals
#' on transformations of a suspected regressor. Significant slopes indicate that
#' the variance of the error term depends on that regressor. By default,
#' `robust = TRUE` since 0.15.0, the absolute residuals are first corrected
#' for the asymmetry of the errors: this is the version of Im (2000) and of
#' Machado and Santos Silva (2000), which keeps its level when the errors are
#' asymmetric, from about 150 observations on (see the section on asymmetric
#' errors for smaller samples). `robust = FALSE` gives Glejser's statistic,
#' the default up to 0.14.0.
#'
#' @param model A fitted [stats::lm] object whose residuals are examined.
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
#' @param variable Character scalar giving the column suspected of driving the
#'   heteroscedasticity.
#' @param transformation Character scalar selecting the transformation applied to
#'   `variable` in the auxiliary regression. One of `"abs"`, `"sqrt"`, `"inverse"`,
#'   or `"inverse_sqrt"`.
#' @param robust Logical. When `TRUE`, the default since 0.15.0, the absolute
#'   residuals are corrected for the asymmetry of the errors before the
#'   auxiliary regression, following Im (2000) and Machado and Santos Silva
#'   (2000). When `FALSE` the statistic is Glejser's (1969), which has its
#'   nominal level when positive and negative errors are equally likely, as
#'   they are for symmetric errors; it is what the function returned by
#'   default up to 0.14.0. See the sections on asymmetric errors and on the
#'   default.
#'
#' @return An object of class \code{htest} reporting the t statistic and
#'   p-value for the slope coefficient in the auxiliary regression. The `method`
#'   element says which of the two statistics was computed.
#'
#' @details
#' Glejser suggested modelling heteroscedastic variance by regressing the
#' absolute residuals on transformations of the explanatory variable believed to
#' govern the error scale. The test explores several functional forms (absolute,
#' square root, inverse, inverse square root). This implementation reads the
#' residuals and weights from the fit and `variable` from the rows of `data`
#' that hold its observations (the model context, since 0.16.0), validates the
#' fitted model through \link[=rvalidateModelInputs]{rvalidateModelInputs()},
#' then enforces transformation-specific
#' requirements via \link[=rvalidateTestRequirements]{rvalidateTestRequirements()}. The resulting t statistic tests
#' the null hypothesis of no relationship between the transformed regressor and
#' the absolute residuals, corrected for asymmetry by default.
#'
#'
#' @section Asymmetric errors:
#' Glejser's statistic takes the absolute residuals for the absolute errors.
#' The two differ by the estimation error of the mean equation,
#' \deqn{|\hat{e}_i| \approx |e_i| - \mathrm{sign}(e_i)\, x_i^\top (\hat{\beta} - \beta),}
#' and the second term averages out only when positive and negative errors are
#' equally likely (Godfrey 1996). When they are not, and the variable tested
#' is correlated with the regressors, the test does not in general have its
#' nominal level, and a larger sample does not help. In the designs of
#' `inst/validation/glejser-skewed-errors.R` Glejser's statistic,
#' `robust = FALSE`, rejects a true null hypothesis about 13% of the time at
#' the 5% level under exponential errors and about 17% under lognormal errors,
#' at every sample size from 50 to 1000. For other error distributions the
#' rate can also fall below the nominal level.
#'
#' `robust = TRUE` replaces the regressand \eqn{|\hat{e}_i|} by
#' \deqn{|\hat{e}_i| - \hat{m}\, \hat{e}_i, \qquad
#'   \hat{m} = \frac{1}{n} \sum_{i=1}^{n} \mathrm{sign}(\hat{e}_i),}
#' where \eqn{\hat{m}} is the proportion of positive residuals less the
#' proportion of negative ones. This is the correction of Im (2000). The
#' regressand of Machado and Santos Silva (2000),
#' \eqn{\hat{e}_i\,[I(\hat{e}_i \ge 0) - \hat{\eta}]} with \eqn{\hat{\eta}} the
#' proportion of non-negative residuals, is half of it when no residual is
#' exactly zero, and the two then give the same test. A residual that is zero
#' up to rounding error is counted as zero, so that the statistic does not
#' depend on the sign of a rounding error.
#'
#' The auxiliary regression is otherwise the same. Both references state the
#' statistic as \eqn{n R^2} of that regression, referred to a chi-squared
#' distribution with one degree of freedom. The t statistic reported here is
#' the signed form of it, \eqn{n R^2 = n t^2 / (t^2 + n - 2)}, and its p-value
#' is taken from the t distribution with \eqn{n - 2} degrees of freedom, as
#' for Glejser's statistic. The two p-values agree as the sample grows.
#'
#' In the same study, with 5000 replications in each cell, the corrected
#' statistic is within simulation error of the 5% level from 150 observations
#' on: all 27 of its cells with a true null hypothesis and 150 observations or
#' more are inside the 99% interval around 5%, from 4.21% to 5.79%, at 4.4%
#' to 5.7%. At 50 observations it rejects 5.7% to 6.6% of the time under
#' skewed errors, and four of those six cells are above that interval, at
#' 5.80% to 6.58%. Under symmetric errors \eqn{\hat{m}} is close to zero and
#' the two statistics differ little: the corrected one rejects 0.5 to 0.7
#' percentage points less often at 50 observations and gives up one or two
#' points of power there. For a weighted fit the residuals are the Pearson
#' residuals, and the correction is applied to those.
#'
#' @section The default:
#' Up to 0.14.0 the default was `robust = FALSE`. Since 0.15.0 it is
#' `robust = TRUE`, so a call that does not give `robust` returns a different
#' statistic and p-value from those it returned before, and its `method` ends
#' in "(robust to asymmetric errors)". `robust = FALSE` returns what such a
#' call returned up to 0.14.0. Glejser's statistic needs positive and
#' negative errors to be equally likely, which is seldom known: in the study
#' above it rejects a true null hypothesis 8.8% to 17.7% of the time at the 5%
#' level when the errors are skewed, at every sample size from 50 to 1000,
#' where the corrected statistic rejects 4.6% to 5.7% of the time from 150
#' observations on. The change costs some power when the errors are
#' symmetric, 1.1 and 1.9 percentage points at 50 observations under Gaussian
#' errors in that study, and the corrected statistic is above the 5% level at
#' 50 observations under skewed errors, as above.
#'
#' @section Interpretation and caveats:
#' Glejser's statistic, `robust = FALSE`, has its nominal level only when
#' positive and negative errors are equally likely; see the section on
#' asymmetric errors. The corrected statistic of the default does not need
#' that, and is the one to use when the residuals may be skewed or their shape
#' is unknown. Either form conditions on a single user-chosen variable and a
#' single transformation, so scanning several transformations and reporting
#' only the smallest p-value invalidates the nominal level. Simulated size and
#' power of both statistics are recorded in
#' `inst/validation/glejser-skewed-errors.csv`; those of Glejser's statistic in
#' `inst/validation/pass-a-size-power.csv`, and those of the default in
#' `inst/validation/full-sweep-size-power.csv` and
#' `inst/validation/weighted-fits-size.csv`.
#' @references
#' Glejser, H. (1969). A new test for heteroskedasticity. *Journal of the American
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
#' Machado, J. A. F., & Santos Silva, J. M. C. (2000). Glejser's test revisited.
#' *Journal of Econometrics, 97*(1), 189-202.
#' <https://doi.org/10.1016/S0304-4076(00)00016-6>
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' # The default, which keeps its level under asymmetric errors from about
#' # 150 observations
#' performGlejserTest(mod, mtcars, "wt")
#'
#' # Glejser's (1969) statistic, the default up to 0.14.0
#' performGlejserTest(mod, mtcars, "wt", robust = FALSE)
#'
#' # Examine alternative transformations
#' performGlejserTest(mod, mtcars, "wt", transformation = "inverse")
#'
#' @seealso
#' [performKoenkerTest()] for a test on the squared residuals that needs neither
#' normal nor symmetric errors; [performParkTest()] and [performHarveyTest()]
#' for related parametric tests of specific forms of heteroscedasticity.
performGlejserTest <- function(model, data = NULL, variable,
                               transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"),
                               robust = TRUE) {
  test_label <- "Glejser test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `variable` is read
  # from the rows of `data` that hold the observations, so data taken in the
  # order of the fit must tell those rows apart when it is not a column of the
  # model frame.
  ctx <- rmodel_context(
    model, data, fn = "performGlejserTest", test_label = test_label,
    reads = if (!missing(variable) && is.character(variable)) variable else character()
  )

  if (!is.character(variable) || length(variable) != 1L || is.na(variable) || !nzchar(variable)) {
    stop("`variable` must be supplied as a single column name.", call. = FALSE)
  }
  if (!is.logical(robust) || length(robust) != 1L || is.na(robust)) {
    stop("`robust` must be a single logical value.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Glejser", min_obs = 12L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  transformation <- match.arg(transformation)

  # The variable, one value per observation in the order of the fit: from the
  # confirmed rows of `data`, or without `data` from the model frame when it
  # holds it under its own name (rcontext_variables()).
  x <- rcontext_variables(ctx, data, c(variable = variable))$variable
  rwarn_incomplete_rows(data, model, also = variable)
  # The Pearson residuals of the fit; a fit with a zero weight is refused
  # there, as before 0.16.0.
  residuals <- rcontext_pearson(ctx)
  rstop_missing_variable(ctx, x, variable, "variable", test_label)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

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

  if (!is.numeric(x)) {
    .std_error(
      "rassumption_violation",
      assumption = sprintf("Variable '%s' must be numeric to evaluate the Glejser test", variable)
    )
  }

  if (transformation %in% c("sqrt", "inverse_sqrt") && any(x < 0, na.rm = TRUE)) {
    .std_error(
      "rassumption_violation",
      assumption = sprintf("Transformation '%s' requires non-negative values in '%s'", transformation, variable)
    )
  }

  if (transformation %in% c("inverse", "inverse_sqrt") && any(abs(x) <= .Machine$double.eps, na.rm = TRUE)) {
    .std_error(
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
    .std_error(
      "rassumption_violation",
      assumption = sprintf("Transformation '%s' produced non-finite values", transformation)
    )
  }

  abs_res <- abs(residuals)
  method <- "Glejser test for heteroscedasticity"
  if (robust) {
    # Im (2000). |e-hat| differs from |e| by sign(e) x'(beta-hat - beta), whose
    # mean is E[sign(e)] times the estimation error. Subtracting m-hat * e-hat,
    # with m-hat the mean sign of the residuals, removes it. The regressand of
    # Machado and Santos Silva (2000), e-hat * (I(e-hat >= 0) - eta-hat), is
    # half of this one when no residual is exactly zero.
    #
    # An observation the model fits exactly has a residual of the order of
    # 1e-15 whose sign is that of a rounding error and changes with the
    # parametrisation of the model. Such residuals count as zero, which is
    # what m-hat = (n_positive - n_negative) / n gives them when they are
    # exactly zero.
    signs <- sign(residuals)
    signs[abs_res <= sqrt(.Machine$double.eps) * max(abs_res)] <- 0
    abs_res <- abs_res - mean(signs) * residuals
    method <- paste(method, "(robust to asymmetric errors)")
  }
  if (stats::var(abs_res) <= .Machine$double.eps) {
    .std_error(
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
      method = method,
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
