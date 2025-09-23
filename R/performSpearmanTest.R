#' Perform Spearman rank correlation test for heteroscedasticity
#'
#' Assesses monotonic relationships between the magnitude of residuals and the
#' fitted values by computing Spearman's rank correlation. Significant
#' correlation indicates that the spread of the residuals increases or decreases
#' systematically with the mean of the response.
#'
#' @param model A fitted [stats::lm] object providing residuals and fitted values
#'   for the diagnostic.
#'
#' @return An object of class \link[stats:htest]{htest} containing the t statistic for the
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
performSpearmanTest <- function(model) {
  rvalidateModelInputs(model, test_name = "Spearman rank", min_obs = 10L)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("spearman", model = model, data = model_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Spearman rank correlation test")

  abs_res <- abs(stats::residuals(model))
  fit <- stats::fitted(model)

  if (length(abs_res) != length(fit)) {
    stop("Fitted values and residuals must have matching lengths.", call. = FALSE)
  }

  if (stats::var(abs_res) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Spearman test requires variability in absolute residuals"
    )
  }

  if (stats::var(fit) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Spearman test requires variability in fitted values"
    )
  }

  rho <- stats::cor(abs_res, fit, method = "spearman")
  if (is.na(rho) || abs(rho) >= 1) {
    std_error(
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
      parameter = n - 2,
      p.value = p_value,
      method = "Spearman rank correlation test for heteroscedasticity",
      data.name = deparse(stats::formula(model)),
      estimate = c(rho = rho)
    ),
    class = "htest"
  )
}
