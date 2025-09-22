#' Perform Ordered Lagrange Multiplier test
#'
#' Sorts observations by the suspected variable and runs a Breusch-Pagan
#' regression on the ordered data.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param order_by Character. Name of the variable to order the data by.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' Validates the inputs using the shared helpers before resorting the data and
#' fitting the auxiliary regression. Missing observations in the ordering
#' variable or model terms are removed with an explanatory warning.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performOrderedLMTest(m, mtcars, order_by = "wt")
performOrderedLMTest <- function(model, data, order_by) {
  test_label <- "Ordered LM test"

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  min_required <- rTEST_REQUIREMENTS$ordered_lm$min_obs

  rvalidateModelInputs(model, test_name = "Ordered LM", min_obs = min_required)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = min_required,
    min_obs_data = min_required
  )

  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("ordered_lm", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    std_error("missing_variable", variable = order_by)
  }

  ht_log("INFO", "Running Ordered LM test")

  ordered_data <- working_data[order(working_data[[order_by]]), , drop = FALSE]
  ordered_model <- safe_lm(stats::formula(model), data = ordered_data)
  e <- stats::residuals(ordered_model)
  n <- length(e)

  if (n < min_required) {
    std_error(
      "rinsufficient_sample_size",
      test_name = test_label,
      min_obs = min_required,
      n_obs = n
    )
  }

  X <- stats::model.matrix(stats::formula(ordered_model), data = ordered_data)
  if (nrow(X) != n) {
    stop("Ordered LM test could not align the design matrix with residuals.", call. = FALSE)
  }

  if (colnames(X)[1] == "(Intercept)") {
    aux_matrix <- X[, -1, drop = FALSE]
  } else {
    aux_matrix <- X
  }

  if (ncol(aux_matrix) == 0) {
    stop("Ordered LM test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  if (n <= (ncol(aux_matrix) + 1L)) {
    std_error(
      "rassumption_violation",
      assumption = "Ordered LM auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  aux_data <- data.frame(aux_matrix)
  aux_model <- safe_lm(e^2 ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - stats::pchisq(test_statistic, df)
  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Ordered Lagrange Multiplier test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
