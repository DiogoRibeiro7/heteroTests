#' Perform Spearman rank correlation test for heteroscedasticity
#'
#' Assesses monotonic relationships between the magnitude of residuals and the
#' fitted values by computing Spearman's rank correlation. Significant
#' correlation indicates that the spread of the residuals increases or decreases
#' systematically with the mean of the response.
#'
#' @param model A fitted [stats::lm] object providing residuals and fitted values
#'   for the diagnostic.
#' @param data Optional data frame the model was fitted to. When `NULL`, the
#'   default, the model frame of `model` is used. The statistic is computed
#'   from the residuals and fitted values of `model`, so `data` does not change
#'   the result; it is accepted so that every test takes `(model, data, ...)`.
#'
#' @return An object of class \code{htest} containing the t statistic for the
#'   rank correlation and its associated p-value.
#'
#' @details
#' Spearman's rho is computed between the absolute residuals and fitted values.
#' Under homoskedasticity the correlation should be zero. The test uses the usual
#' t approximation for the correlation coefficient. The helper functions
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()} and \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} ensure minimum
#' sample sizes and guard against degenerate fitted values or residual magnitudes.
#'
#' @references
#' Spearman, C. (1904). The proof and measurement of association between two
#' things. *The American Journal of Psychology, 15*(1), 72–101.
#'
#' Gujarati, D. N., & Porter, D. C. (2009). *Basic Econometrics* (5th ed.).
#' McGraw-Hill. Section 11.5 discusses Spearman tests for heteroscedasticity.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSpearmanTest(mod)
#'
#' # Detect monotonic variance inflation
#' set.seed(717)
#' x <- runif(140)
#' y <- 1 + x + rnorm(140, sd = 0.3 + 0.4 * x)
#' df <- data.frame(y, x)
#' performSpearmanTest(lm(y ~ x, data = df))
#'
#' @seealso
#' [performNCVTest()] and [performSpreadLevelTest()] provide related tests based
#' on monotonic variance patterns.
performSpearmanTest <- function(model, data = NULL) {
  rrefuse_non_gaussian_glm(model, "performSpearmanTest")
  # The model context (R/model_context.R) refuses a model frame that
  # `model = FALSE` rebuilt from data changed after the fit. The test reads
  # nothing from `data`, which is matched to the observations of the fit as by
  # every test built on the context (decision 31).
  rcheck_data_argument(data, "performSpearmanTest")
  ctx <- rmodel_context(model, data, fn = "performSpearmanTest", test_label = "Spearman rank")
  rvalidateModelInputs(model, test_name = "Spearman rank", min_obs = 10L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  requirements <- rvalidateTestRequirements("spearman", model = model, data = ctx$frame)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Spearman rank correlation test")

  # The Pearson residuals and the fitted values, without the padding of
  # na.exclude (B13).
  abs_res <- abs(rcontext_pearson(ctx))
  fit <- rcontext_fitted(ctx)

  if (length(abs_res) != length(fit)) {
    stop("Fitted values and residuals must have matching lengths.", call. = FALSE)
  }

  if (stats::var(abs_res) <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Spearman test requires variability in absolute residuals"
    )
  }

  if (stats::var(fit) <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Spearman test requires variability in fitted values"
    )
  }

  rho <- stats::cor(abs_res, fit, method = "spearman")
  if (is.na(rho) || abs(rho) >= 1) {
    .std_error(
      "rassumption_violation",
      assumption = "Spearman correlation could not be computed due to degeneracy"
    )
  }

  n <- length(abs_res)
  t_statistic <- rho * sqrt((n - 2) / (1 - rho^2))
  p_value <- 2 * stats::pt(-abs(t_statistic), df = n - 2)

  structure(
    list(
      statistic = c(t = t_statistic),
      parameter = c(df = n - 2),
      p.value = p_value,
      method = "Spearman rank correlation test for heteroscedasticity",
      data.name = deparse(stats::formula(model)),
      estimate = c(rho = rho)
    ),
    class = "htest"
  )
}
