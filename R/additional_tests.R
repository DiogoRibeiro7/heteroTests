#' Additional heteroscedasticity tests
#'
#' These functions extend the package with more specialized tests
#' for heteroscedasticity such as a studentized Breusch-Pagan test,
#' a bootstrap version of White's test and the Szroeter ordered test.
#'
#' @name additional_tests
NULL

# Internal helper --------------------------------------------------------------

prepare_model_data_for_test <- function(model, data, required_vars, test_label,
                                        min_obs_model = 10L, min_obs_data = 10L) {
  rvalidateModelInputs(model, test_name = test_label, min_obs = min_obs_model)
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = min_obs_data)

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  residuals <- stats::residuals(model)

  resid_names <- names(residuals)
  data_rows <- rownames(cleaned$data)

  if (!is.null(resid_names) && length(resid_names) > 0 && !is.null(data_rows)) {
    match_idx <- match(resid_names, data_rows)
    if (anyNA(match_idx)) {
      missing_rows <- resid_names[is.na(match_idx)]
      stop(
        sprintf(
          "%s requires `data` to contain the rows used to fit the model. Missing rows: %s",
          test_label,
          paste(utils::head(missing_rows, 3L), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    aligned_data <- cleaned$data[match_idx, , drop = FALSE]
  } else {
    if (nrow(cleaned$data) != length(residuals)) {
      stop(
        sprintf(
          "%s requires `data` with %d observations to match the fitted model, got %d.",
          test_label,
          length(residuals),
          nrow(cleaned$data)
        ),
        call. = FALSE
      )
    }
    aligned_data <- cleaned$data
  }

  list(data = aligned_data, residuals = residuals)
}

#' Studentized Breusch-Pagan test
#' 
#' Performs the Breusch-Pagan test using studentized residuals,
#' providing robustness to non-normality.
#' 
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @return An object of class `htest` with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performStudentizedBPTest(m, mtcars)
#' @export
performStudentizedBPTest <- function(model, data) {
  test_label <- "Studentized BP"

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

  requirements <- rvalidateTestRequirements("studentized_bp", model = model, data = working_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Studentized Breusch-Pagan test")

  resid_student <- stats::rstudent(model)
  if (length(resid_student) != nrow(working_data)) {
    stop("Studentized residuals could not be aligned with the working data.", call. = FALSE)
  }

  if (stats::var(resid_student) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires variability in studentized residuals"
    )
  }

  X_full <- stats::model.matrix(model, data = working_data)
  if (colnames(X_full)[1] == "(Intercept)") {
    predictors <- X_full[, -1, drop = FALSE]
  } else {
    predictors <- X_full
  }

  if (ncol(predictors) == 0) {
    stop("Studentized BP test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  n <- length(resid_student)
  if (n <= (ncol(predictors) + 1L)) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  aux_data <- as.data.frame(predictors)
  aux_model <- safe_lm(resid_student^2 ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Studentized Breusch-Pagan test",
      data.name = deparse(substitute(model)),
      alternative = "heteroscedasticity present"
    ),
    class = "htest"
  )
}

#' Bootstrap White test
#'
#' Uses bootstrap resampling of residuals to estimate the distribution
#' of White's test statistic. Useful for small samples.
#'
#' @inheritParams performWhiteTest
#' @param B Number of bootstrap replications.
#' @param parallel Logical, run in parallel using the `parallel` package?
#' @return An object of class `htest` with the bootstrap p-value.
#' @export
performWhiteTestBootstrap <- function(model, data, B = 1000, parallel = FALSE) {
  test_label <- "Bootstrap White"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 20L,
    min_obs_data = 20L
  )

  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("bootstrap_tests", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }

  ht_log("INFO", "Running Bootstrap White test")

  original_result <- performWhiteTest(model, working_data)
  original_stat <- unname(original_result$statistic[1])

  bootstrap_stat <- function() {
    fitted_vals <- stats::fitted(model)
    resid <- stats::residuals(model)
    boot_resid <- sample(resid, replace = TRUE)
    boot_y <- fitted_vals + boot_resid

    boot_data <- working_data
    response_name <- as.character(stats::formula(model))[2]
    boot_data[[response_name]] <- boot_y

    boot_model <- safe_lm(stats::formula(model), data = boot_data)
    unname(performWhiteTest(boot_model, boot_data)$statistic[1])
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    boot_stats <- parallel::mclapply(
      seq_len(B),
      function(i) bootstrap_stat(),
      mc.cores = max(1, parallel::detectCores() - 1)
    )
    boot_stats <- unlist(boot_stats, use.names = FALSE)
  } else {
    boot_stats <- replicate(B, bootstrap_stat())
  }

  p_value <- mean(boot_stats >= original_stat)

  structure(
    list(
      statistic = original_result$statistic,
      parameter = c(B = B),
      p.value = p_value,
      method = "Bootstrap White test",
      data.name = deparse(substitute(model)),
      boot_statistics = boot_stats
    ),
    class = "htest"
  )
}

#' Szroeter test for ordered alternatives
#'
#' Detects monotonic changes in variance when the observations can be
#' ordered by a known variable.
#'
#' @inheritParams performHarveyTest
#' @param data The data frame used to fit `model`.
#' @param order_by Variable name to order the observations by.
#' @return An object of class `htest`.
#' @export
performSzroeterTest <- function(model, data, order_by) {
  test_label <- "Szroeter test"

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))
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

  requirements <- rvalidateTestRequirements("szroeter", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    std_error("missing_variable", variable = order_by)
  }

  ht_log("INFO", "Running Szroeter test")

  ord <- order(working_data[[order_by]])
  e_ordered <- residuals[ord]
  n <- length(e_ordered)

  if (n <= 1) {
    std_error(
      "rinsufficient_sample_size",
      test_name = test_label,
      min_obs = 2L,
      n_obs = n
    )
  }

  if (stats::var(e_ordered) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Szroeter test requires variability in ordered residuals"
    )
  }

  ranks <- seq_len(n)
  numerator <- sum(ranks * e_ordered^2)
  denominator <- sum(e_ordered^2) * (n + 1) / 2

  test_statistic <- numerator / denominator

  var_stat <- (n + 1) * (2 * n + 1) / (12 * n)
  z_stat <- (test_statistic - 1) / sqrt(var_stat / n)
  p_value <- 2 * stats::pnorm(-abs(z_stat))

  structure(
    list(
      statistic = c(S = test_statistic),
      parameter = c(n = n),
      p.value = p_value,
      method = "Szroeter test for ordered heteroscedasticity",
      data.name = deparse(substitute(model))
    ),
    class = "htest"
  )
}
