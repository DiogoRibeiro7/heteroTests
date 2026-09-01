#' Quantile regression heteroscedasticity test
#'
#' Tests equality of regression slopes across two or more conditional quantiles
#' using the joint Wald-type test implemented by [quantreg::anova.rqs()]. This
#' avoids treating quantile-specific coefficient estimates from the same sample
#' as independent.
#'
#' Under a pure location-shift model with homoskedastic errors, slopes are equal
#' across quantiles. Systematic slope differences are therefore evidence against
#' that location-shift/homoskedastic specification. The test should not be read
#' as a universal test for every possible form of heteroscedasticity.
#'
#' @inheritParams performBPTest
#' @param taus Numeric vector containing at least two distinct quantiles in
#'   `(0, 1)`. Defaults to `c(0.25, 0.75)`.
#' @param se_type Standard-error method used by [quantreg::anova.rqs()]. Supported
#'   values are `"nid"` and `"ker"`.
#' @param iid Logical indicating whether the conditional densities are assumed
#'   identical when computing the joint test. Passed to
#'   [quantreg::anova.rqs()]. Defaults to `TRUE`.
#'
#' @return An object of class [stats::htest] containing the F-like joint test
#'   statistic, numerator and denominator degrees of freedom, p-value, fitted
#'   quantiles, and quantile-specific slope estimates.
#'
#' @references
#' Koenker, R., & Bassett, G. (1982). Robust tests for heteroscedasticity based
#' on regression quantiles. *Econometrica, 50*(1), 43-61.
#'
#' Koenker, R. (2005). *Quantile Regression*. Cambridge University Press.
#'
#' @examples
#' if (requireNamespace("quantreg", quietly = TRUE)) {
#'   # The test needs at least 40 observations, so mtcars (32) is too small.
#'   #'   model <- lm(stations ~ mag + depth, data = quakes)
#'   performQuantileRegressionTest(model, quakes)
#' }
#'
#' @export
performQuantileRegressionTest <- function(model, data,
                                          taus = c(0.25, 0.75),
                                          se_type = c("nid", "ker"),
                                          iid = TRUE) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for the quantile regression test.", call. = FALSE)
  }
  if (!is.numeric(taus) || length(taus) < 2L || anyNA(taus) ||
      any(!is.finite(taus)) || any(taus <= 0 | taus >= 1)) {
    stop("`taus` must contain at least two finite quantiles strictly between 0 and 1.", call. = FALSE)
  }
  taus <- sort(unique(as.numeric(taus)))
  if (length(taus) < 2L) {
    stop("`taus` must contain at least two distinct quantiles.", call. = FALSE)
  }
  se_type <- match.arg(se_type)
  if (!is.logical(iid) || length(iid) != 1L || is.na(iid)) {
    stop("`iid` must be a single non-missing logical value.", call. = FALSE)
  }

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "Quantile regression",
    min_obs_model = 30L,
    min_obs_data = 30L
  )
  working_data <- prepared$data

  requirements <- rvalidateTestRequirements(
    "quantile_regression",
    model = model,
    data = working_data
  )
  rprocessValidationResult(requirements)

  formula <- stats::formula(model)
  rq_fit <- quantreg::rq(formula, tau = taus, data = working_data)
  if (!inherits(rq_fit, "rqs")) {
    stop("Joint quantile regression fit did not return an 'rqs' object.", call. = FALSE)
  }

  joint <- stats::anova(rq_fit, se = se_type, iid = iid, joint = TRUE)
  table <- joint$table
  if (is.null(table) || nrow(table) != 1L || ncol(table) < 4L) {
    stop("quantreg joint slope test returned an unexpected result structure.", call. = FALSE)
  }

  ndf <- as.numeric(table[1L, 1L])
  ddf <- as.numeric(table[1L, 2L])
  statistic <- as.numeric(table[1L, 3L])
  p_value <- as.numeric(table[1L, 4L])
  if (any(!is.finite(c(ndf, ddf, statistic, p_value)))) {
    stop("quantreg joint slope test returned non-finite inference values.", call. = FALSE)
  }

  coef_matrix <- stats::coef(rq_fit)
  if (is.null(dim(coef_matrix))) {
    coef_matrix <- matrix(coef_matrix, ncol = length(taus))
  }
  slope_names <- setdiff(rownames(coef_matrix), "(Intercept)")
  slope_estimates <- if (length(slope_names) > 0L) {
    coef_matrix[slope_names, , drop = FALSE]
  } else {
    matrix(numeric(), nrow = 0L, ncol = length(taus))
  }
  colnames(slope_estimates) <- paste0("tau_", format(taus, trim = TRUE))

  structure(
    list(
      statistic = c(F = statistic),
      parameter = c(df1 = ndf, df2 = ddf),
      p.value = p_value,
      method = "Quantile regression joint test of equality of slopes",
      data.name = deparse(formula),
      alternative = "at least one slope differs across quantiles",
      quantiles = taus,
      estimate = slope_estimates,
      se_type = se_type,
      iid = iid
    ),
    class = "htest"
  )
}
