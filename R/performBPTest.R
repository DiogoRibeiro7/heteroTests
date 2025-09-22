#' Perform Breusch-Pagan test for heteroscedasticity
#'
#' This function performs the Breusch-Pagan test on a fitted linear model. The
#' implementation integrates the shared validation utilities so that model
#' compatibility, data integrity, sample-size requirements, and missing values
#' are handled consistently before the auxiliary regression is estimated.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#'
#' @details
#' The routine validates inputs and test requirements in the following order:
#' \enumerate{
#'   \item [rvalidateModelInputs()] ensures `model` has at least 15 observations,
#'     finite residuals, and no perfect fit.
#'   \item [rvalidateDataInputs()] checks that `data` contains the variables
#'     referenced by the model formula.
#'   \item [rhandleMissingValues()] removes incomplete observations with a
#'     summary warning.
#'   \item [rvalidateTestRequirements()] enforces the Breusch-Pagan-specific
#'     sample-size threshold.
#'   \item Additional checks guard against vanishing residual variance and
#'     insufficient auxiliary degrees of freedom.
#' }
#'
#' @references
#' Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity
#' and random coefficient variation. \emph{Econometrica}, 47(5), 1287-1294.
#' \doi{10.2307/1911963}
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' \emph{Journal of Econometrics}, 17(1), 107-112. \doi{10.1016/0304-4076(81)90062-2}
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performBPTest(m, mtcars)
#'
#' # Validation examples
#' try(performBPTest(m, mtcars[1:10, ]))
performBPTest <- function(model, data) {
  test_label <- "Breusch-Pagan test"
  rvalidateModelInputs(model, test_name = "Breusch-Pagan", min_obs = 15L)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = 15L)

  handle_validation_result <- function(result) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      stop(paste(unique(result$messages), collapse = "\n"), call. = FALSE)
    }
    invisible(result)
  }

  align_to_model <- function(clean_data, residuals) {
    resid_names <- names(residuals)
    data_rows <- rownames(clean_data)

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
      aligned_data <- clean_data[match_idx, , drop = FALSE]
      list(data = aligned_data, residuals = residuals)
    } else {
      if (nrow(clean_data) != length(residuals)) {
        stop(
          sprintf(
            "%s requires `data` with %d observations to match the fitted model, got %d.",
            test_label,
            length(residuals),
            nrow(clean_data)
          ),
          call. = FALSE
        )
      }
      list(data = clean_data, residuals = residuals)
    }
  }

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  aligned <- align_to_model(cleaned$data, stats::residuals(model))
  working_data <- aligned$data
  residuals <- aligned$residuals

  requirements <- rvalidateTestRequirements("breusch_pagan", model = model, data = working_data)
  handle_validation_result(requirements)

  # Memory and performance warnings
  check_memory_usage(working_data, threshold_mb = 50)
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "This may take some time to compute."
    )
  }

  ht_log("INFO", "Running Breusch-Pagan test")

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires residual variation greater than numerical precision"
    )
  }

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
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  aux_data <- as.data.frame(predictors)
  n <- length(residuals)

  if (n <= (ncol(aux_data) + 1L)) {
    std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  e_squared <- residuals^2
  aux_model <- safe_lm(e_squared ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - stats::pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}

#' @rdname performBPTest
#' @export
performBreuschPaganTest <- performBPTest
