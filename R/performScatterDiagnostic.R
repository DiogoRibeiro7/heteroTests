#' Scatter-plot diagnostics for heteroscedasticity
#'
#' Computes Spearman correlations between the absolute residuals of a fitted
#' model and selected covariates, mirroring the slopes displayed in spread–level
#' plots.
#'
#' @param model A fitted [stats::lm] object (or compatible object providing
#'   residuals) whose residual dispersion is assessed.
#' @param data The data frame `model` was fitted to, holding the covariates
#'   listed in `vars`. The residuals and weights are read from the fit itself,
#'   and `vars` from the rows of `data` that hold the observations of the fit.
#'   Since 0.16.0 they are matched to the observations of the fit: rows with
#'   row names of their own by name, in any order; rows that only number
#'   themselves (a tibble, a data.table, a data frame whose row names were
#'   reset) by the row numbers of the fit, or in the order of the fit when they
#'   hold one row per observation. The rows the fit dropped may be there or
#'   not. Data whose rows cannot be shown to hold the observations of the fit
#'   are refused; see [test_results].
#' @param vars Character vector of column names to correlate with the absolute
#'   residuals. Variables must be present in `data` and coercible to numeric.
#'
#' @return A named numeric vector of Spearman correlation coefficients between
#'   `abs(residuals(model))` and each variable in `vars`.
#'
#' @details
#' The diagnostic reproduces the summary statistic underlying spread–level plots:
#' a strong positive correlation indicates that absolute residuals—and hence the
#' conditional variance—grow with the covariate, while negative correlations point
#' to diminishing variance. Because Spearman's \eqn{\rho} is rank-based, the
#' measure is robust to monotonic transformations of the covariate and provides a
#' quick complement to formal tests.
#'
#' @references
#' Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
#' (3rd ed.). Sage.
#'
#' Cleveland, W. S. (1993). *Visualizing Data*. Hobart Press. Chapter 5 introduces
#' spread–level plots for variance assessment.
#'
#' @seealso
#' [performSpearmanTest()] and [performNCVTest()] provide formal tests based on
#' the same intuition.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
#'
#' # Investigate variance trends in a simulated dataset with increasing spread
#' set.seed(707)
#' x <- runif(120)
#' y <- 1 + 2 * x + rnorm(120, sd = 0.3 + 0.8 * x)
#' sim_df <- data.frame(y, x)
#' fit <- lm(y ~ x, data = sim_df)
#' performScatterDiagnostic(fit, sim_df, "x")
performScatterDiagnostic <- function(model, data, vars) {
  # The absolute residuals of a glm that is not a least-squares fit vary with
  # the mean by construction, as the tests refuse it for; before 0.16.0 this
  # diagnostic returned their correlations.
  rrefuse_non_gaussian_glm(model, "performScatterDiagnostic")
  # `data` is required, and the covariates must be columns of it; then a fit
  # with a zero weight is refused, before `data` is matched, in the order of
  # the checks of before 0.16.0.
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop("Variables not found: ", paste(missing_vars, collapse = ", "))
  }
  rrefuse_zero_weights(rleast_squares_weights(model))
  # The residuals and weights come from the fit; `data` is matched to its
  # observations and checked (R/model_context.R), and `vars` are read from the
  # rows of `data` that hold them, so data taken in the order of the fit must
  # tell those rows apart when they are not columns of the model frame. Before
  # 0.16.0 the i-th row of `data` was read with the i-th residual, and data
  # with row names of their own in another order than the fit, with rows after
  # those of the fit, or of a fit that dropped rows, were refused. A fit with
  # na.exclude is read as before (rcontext_columns_at_fit()).
  ctx <- rmodel_context(
    model, data, fn = "performScatterDiagnostic", test_label = "Scatter diagnostic",
    reads = if (is.character(vars)) vars else character()
  )
  rvalidate_model_context(ctx)
  read <- rcontext_columns_at_fit(
    ctx, data, vars, "performScatterDiagnostic", "Scatter diagnostic"
  )
  res <- abs(read$residuals)
  sapply(vars, function(v) cor(res, read$columns[[v]], method = "spearman"))
}
