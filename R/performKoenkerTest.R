#' Perform Koenker studentized Breusch–Pagan test
#'
#' Evaluates Koenker's (1981) studentized variant of the Breusch–Pagan Lagrange
#' Multiplier test. The auxiliary regression of the squared residuals on the
#' regressors yields the \eqn{n R^2} statistic, which the studentization makes
#' robust to non-normal error distributions while still assessing whether the
#' error variance depends on the regressors.
#'
#' @param model A fitted [stats::lm] object supplying residuals and fitted values
#'   for the diagnostic.
#' @param data A [base::data.frame] containing the variables referenced in
#'   `model`. The data must align with the observations used to fit `model`.
#'
#' @return An object of class \code{htest} with the chi-squared statistic,
#'   degrees of freedom, and p-value for the null hypothesis of homoskedasticity.
#'
#' @details
#' Koenker's extension keeps the squared residuals of the Breusch–Pagan test but
#' replaces the normality-based scaling factor \eqn{2\hat{\sigma}^4} with the
#' empirical variance of the squared residuals. The resulting \eqn{n R^2}
#' statistic therefore retains validity under general forms of non-normality
#' (it is identical to [performStudentizedBPTest()]). This implementation
#' integrates the validation helpers
#' to ensure: (i) sufficient sample size and finite residuals via
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()}, (ii) complete data through \link[=rvalidateDataInputs]{rvalidateDataInputs()}
#' and \link[=rhandleMissingValues]{rhandleMissingValues()}, (iii) satisfaction of Koenker-specific
#' requirements using \link[=rvalidateTestRequirements]{rvalidateTestRequirements()}, and (iv) a full-rank design
#' matrix for the auxiliary regression.
#'
#' Rejecting the null indicates that the absolute residuals—and hence the error
#' variance—vary systematically with the predictors. The statistic simplifies to
#' \eqn{n R^2} from the auxiliary regression and is compared against a
#' chi-squared distribution with degrees of freedom equal to the number of
#' regressors.
#'
#' @references
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' <https://doi.org/10.1016/0304-4076(81)90062-2>
#'
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performKoenkerTest(mod, mtcars)
#'
#' # Simulated data with variance that scales with the regressor
#' set.seed(99)
#' x <- runif(120)
#' y <- 1 + 2 * x + rnorm(120, sd = 0.2 + 0.8 * x)
#' df <- data.frame(y, x)
#' performKoenkerTest(lm(y ~ x, data = df), df)
#'
#' @seealso
#' [performBPTest()] for the classical LM statistic using squared residuals,
#' [performStudentizedBPTest()] for the alternative studentized implementation,
#' and \link[=performBPTestRobust]{performBPTestRobust()} for bootstrap-enhanced reporting.
performKoenkerTest <- function(model, data) {
  test_label <- "Koenker studentized Breusch-Pagan test"

  rvalidateModelInputs(model, test_name = "Koenker", min_obs = 15L)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("koenker", model = model, data = working_data)
  rprocessValidationResult(requirements)

  size_mb <- tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NA_real_
  )
  if (!is.na(size_mb) && size_mb > 100) {
    ht_log("INFO", sprintf("Switching to streaming Koenker test for dataset of %.1f MB", size_mb))
    adaptive_chunk <- max(1000L, min(25000L, as.integer(nrow(working_data) / 5L)))
    adaptive_chunk <- min(adaptive_chunk, nrow(working_data))
    return(performKoenkerTestStreaming(
      model,
      working_data,
      chunk_size = adaptive_chunk,
      progress = FALSE
    ))
  }
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "Koenker test may take additional time."
    )
  }

  ht_log("INFO", "Running Koenker test")

  X_full <- stats::model.matrix(model, data = working_data)
  if (nrow(X_full) != length(residuals)) {
    stop(
      sprintf(
        "%s could not align the design matrix with model residuals (expected %d rows, got %d).",
        test_label,
        length(residuals),
        nrow(X_full)
      ),
      call. = FALSE
    )
  }

  if (colnames(X_full)[1] == "(Intercept)") {
    predictors <- X_full[, -1, drop = FALSE]
  } else {
    predictors <- X_full
  }

  if (ncol(predictors) == 0) {
    stop("Koenker test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  n <- length(residuals)
  if (n <= (ncol(predictors) + 1L)) {
    std_error(
      "rassumption_violation",
      assumption = "Koenker auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  e_squared <- residuals^2
  if (all(e_squared == e_squared[1])) {
    std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in squared residuals"
    )
  }

  aux_data <- as.data.frame(predictors)
  aux_model <- safe_lm(e_squared ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
