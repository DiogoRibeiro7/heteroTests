#' Perform Spearman rank correlation test for heteroscedasticity
#'
#' This test computes Spearman's rho between the absolute residuals of a
#' linear model and its fitted values. A significant correlation suggests
#' monotonic heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' [rvalidateModelInputs()] ensures the supplied model is compatible and enforces
#' the test-specific sample size via [rvalidateTestRequirements()].
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSpearmanTest(m)
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
