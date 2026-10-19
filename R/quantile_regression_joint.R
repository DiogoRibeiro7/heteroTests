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
#' Weighted fits are refused, although the resampling tests accept them. On a
#' fit with weights \eqn{w_i} the null hypothesis is that the Pearson residuals
#' \eqn{\sqrt{w_i}\, e_i}{sqrt(w_i) e_i} have constant variance, which is a
#' statement about the regression of \eqn{\sqrt{w_i}\, y_i}{sqrt(w_i) y_i} on
#' \eqn{\sqrt{w_i}\, x_i}{sqrt(w_i) x_i}. The quantile regressions this test
#' fits are of \eqn{y} on \eqn{x}, where the errors have variance
#' \eqn{\sigma^2 / w_i}{sigma^2 / w_i} by assumption, so their slopes differ across
#' quantiles even when the weights are right. Fitting them on the weighted scale
#' does not by itself repair that: there the conditional quantiles are the
#' regression plus a constant, and the constant is not a column of the weighted
#' regressors. Adding a column of ones would restore the location-shift form,
#' but that version of the test needs its own derivation and its own size
#' study, and has neither.
#'
#' @inheritParams performBPTest
#' @param data The data frame `model` was fitted to, or `NULL`, the default.
#'   The response, design and offset are read from the fit itself, so a model
#'   that transforms its variables, has an `offset()` term, or takes its
#'   `offset` from a column of the data needs no `data`, and the model frame of
#'   `model` is accepted as `data` (since 0.16.0; before, such models needed
#'   `data`). Data that are given are matched to the observations of the fit:
#'   rows with row names of their own by name, in any order; rows that only
#'   number themselves (a tibble, a data.table, a data frame whose row names
#'   were reset) by the row numbers of the fit, or in the order of the fit when
#'   they hold one row per observation. The rows the fit dropped may be there
#'   or not. Data whose rows cannot be shown to hold the observations of the
#'   fit are refused; see [test_results].
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
#' @section Supported models:
#' It accepts an unweighted `lm` and an unweighted `glm` with the Gaussian family
#' and the identity link and, since 0.16.0, a rank-deficient one: `quantreg::rq()`
#' refuses a singular design, so the quantile regressions are fitted to the columns
#' that were not aliased, which gives the value of the same model fitted without
#' them. Since 0.16.0 it also accepts a model with an offset: the quantile
#' regressions are fitted to the response of the fit minus its offset, on the
#' design of the fit, which is the model with the offset, since the offset
#' shifts every conditional quantile as it shifts the mean; before, they were
#' refitted from the formula, which left the offset out. It refuses a weighted
#' fit, a `fitWLS()` fit among them, as it refits the
#' model without the weights, a `glm` of another family or link, a
#' `survey::svyglm()` fit, a coefficient that is not finite for another reason, and
#' `data` that lack rows or variables of the fit.
#' It refuses a multivariate `lm` ("needs a model with one response"), and,
#' since 0.16.0, an `mgcv::gam()` fit with smooth terms: a smooth term is
#' usually penalised, and a penalised fit is not the least-squares fit of the
#' design the test fits again. A later step settles which tests take such a
#' fit; a `gam` without smooth terms is tested as the Gaussian `glm` it is.
#' Which model each test accepts,
#' and why, is set out in [test_results], under *Supported models*.
#'
#' @examples
#' if (requireNamespace("quantreg", quietly = TRUE)) {
#'   # The test needs at least 40 observations, so mtcars (32) is too small.
#'   model <- lm(stations ~ mag + depth, data = quakes)
#'   performQuantileRegressionTest(model, quakes)
#' }
#'
#' @export
performQuantileRegressionTest <- function(model, data = NULL,
                                          taus = c(0.25, 0.75),
                                          se_type = c("nid", "ker"),
                                          iid = TRUE) {
  test_label <- "Quantile regression"
  # The response, design and offset come from the fit; `data`, when given, is
  # matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performQuantileRegressionTest", test_label = test_label)
  rrefuse_smooth_gam(ctx, "performQuantileRegressionTest")
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for the quantile regression test.", call. = FALSE)
  }
  rrefuse_weighted_fit(model, "performQuantileRegressionTest()")
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

  rvalidateModelInputs(model, test_name = test_label, min_obs = 30L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements(
    "quantile_regression",
    model = model,
    data = working_data
  )
  rprocessValidationResult(requirements)

  # The test compares the slopes across quantiles, and a model without a
  # regressor beyond the intercept has none.
  if (all(colnames(rcontext_design(ctx)) == "(Intercept)")) {
    stop(
      "performQuantileRegressionTest() needs a regressor beyond the intercept: it tests ",
      "whether the slopes of the model differ across quantiles.",
      call. = FALSE
    )
  }
  formula <- stats::formula(model)
  rq_fit <- rrq_estimable(ctx, taus)
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

#' Quantile regressions on the stored design of the fit
#'
#' The quantile regressions are fitted to the response of the fit minus its
#' offset, on the design of the fit, both taken from the fit (the model
#' context), so that no term is computed again and an offset is accounted for:
#' the quantile regression of \eqn{y - o} on \eqn{X} is that of the model
#' with the offset, since the offset enters every quantile as it enters the
#' mean. Before 0.16.0 they were refitted from the formula, which left the
#' offset out, and a model with an offset was refused.
#'
#' `quantreg::rq()` refuses a singular design ("Singular design matrix"). The
#' joint test of equal slopes is then the test of the same model fitted
#' without the columns the least-squares fit aliased, which span the same
#' space: the quantile regressions are fitted on the remaining columns, so
#' the statistic is that of the model refitted without them, and the slopes
#' reported are those of the remaining columns, under their names.
#'
#' @param ctx The model context of the fit.
#' @param taus The quantiles.
#' @return The `rqs` fit.
#' @keywords internal
#' @noRd
rrq_estimable <- function(ctx, taus) {
  X <- rcontext_design(ctx)
  intercept <- colnames(X) == "(Intercept)"
  response <- ctx$response
  if (!is.null(ctx$offset)) {
    response <- response - ctx$offset
  }
  estimable <- data.frame(.response = response)
  estimable$.design <- X[, !intercept, drop = FALSE]
  fit_formula <- if (any(intercept)) .response ~ .design else .response ~ .design - 1
  fit <- quantreg::rq(fit_formula, tau = taus, data = estimable)
  rownames(fit$coefficients) <- sub("^\\.design", "", rownames(fit$coefficients))
  fit
}
