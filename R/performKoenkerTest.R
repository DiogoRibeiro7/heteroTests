#' Perform Koenker studentized Breusch-Pagan test
#'
#' This function performs the Koenker version of the Breusch-Pagan test,
#' which is more robust to non-normal errors. It regresses the absolute
#' residuals of a linear model on the regressors and uses the resulting
#' R-squared as the test statistic.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' The routine now relies on the shared validation utilities: model inputs are
#' checked with [rvalidateModelInputs()], `data` is verified via
#' [rvalidateDataInputs()], missing values are removed with
#' [rhandleMissingValues()], and Koenker-specific requirements are enforced via
#' [rvalidateTestRequirements()].
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performKoenkerTest(m, mtcars)
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

  abs_res <- abs(residuals)
  if (all(abs_res == abs_res[1])) {
    std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in absolute residuals"
    )
  }

  aux_data <- as.data.frame(predictors)
  aux_model <- safe_lm(abs_res ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - stats::pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
