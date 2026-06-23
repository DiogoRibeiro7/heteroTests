#' Perform Breusch–Pagan test for heteroscedasticity
#'
#' Implements the Breusch and Pagan (1979) Lagrange Multiplier test for detecting
#' heteroscedasticity in linear regression models. The statistic is based on
#' regressing the squared residuals on the original regressors and assessing
#' whether the fitted values explain systematic variance in the errors.
#'
#' @param model A fitted [stats::lm] object describing the mean structure whose
#'   residual variance is to be assessed.
#' @param data A [base::data.frame] (or compatible object) containing the
#'   variables referenced in `model`. The data must include all observations used
#'   to fit `model` and should not contain unresolved missing values.
#'
#' @return An object of class \link[stats:htest]{htest} with the LM statistic, degrees of
#'   freedom, and p-value for the null hypothesis of constant error variance.
#'
#' @details
#' The routine follows the original Breusch–Pagan (1979) derivation. If
#' \eqn{\hat{e}} denotes the residual vector, \eqn{\hat{\sigma}^2 = \sum
#' \hat{e}_i^2 / n} the maximum-likelihood variance estimate, and \eqn{Z} the
#' design matrix without the intercept, the test regresses the scaled squared
#' residuals \eqn{\hat{e}_i^2 / \hat{\sigma}^2 - 1} on \eqn{Z} and forms half the
#' explained sum of squares. Under the null of homoskedasticity *and normal
#' disturbances*, this statistic converges to a chi-squared distribution with
#' degrees of freedom equal to the number of regressors in \eqn{Z}. This matches
#' `lmtest::bptest(..., studentize = FALSE)`. The
#' implementation integrates the validation helpers to:
#' \enumerate{
#'   \item ensure the fitted model has at least 15 observations and finite
#'     residuals via \link[=rvalidateModelInputs]{rvalidateModelInputs()};
#'   \item confirm `data` supplies all required variables and handle missing
#'     cases through \link[=rvalidateDataInputs]{rvalidateDataInputs()} and \link[=rhandleMissingValues]{rhandleMissingValues()};
#'   \item verify auxiliary regression conditions (sample size, rank, residual
#'     variation) with \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} and bespoke checks; and
#'   \item guard against alignment issues between residuals and the supplied data
#'     prior to fitting the auxiliary model.
#' }
#'
#' A significant statistic indicates that the error variance varies with the
#' regressors. Because the classical statistic assumes normal disturbances,
#' consider the studentized \eqn{n R^2} variant [performStudentizedBPTest()] or
#' the equivalent [performKoenkerTest()] when normality is in doubt.
#'
#' @references
#' Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity
#' and random coefficient variation. *Econometrica, 47*(5), 1287–1294.
#' <https://doi.org/10.2307/1911963>
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' <https://doi.org/10.1016/0304-4076(81)90062-2>
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performBPTest(mod, mtcars)
#'
#' # Compare behaviour on simulated homoskedastic and heteroskedastic samples
#' set.seed(42)
#' n <- 150
#' x <- runif(n)
#' y_homo <- 2 + 3 * x + rnorm(n, sd = 0.3)
#' y_hetero <- 2 + 3 * x + rnorm(n, sd = 0.3 + x)
#' df_h <- data.frame(y = y_homo, x = x)
#' df_het <- data.frame(y = y_hetero, x = x)
#' performBPTest(lm(y ~ x, data = df_h), df_h)
#' performBPTest(lm(y ~ x, data = df_het), df_het)
#'
#' @seealso
#' [performStudentizedBPTest()] and [performKoenkerTest()] for the studentized
#' \eqn{n R^2} LM statistic that drops the normality assumption, and
#' \link[=performBPTestRobust]{performBPTestRobust()} or [performWhiteTest()] for complementary diagnostics.
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
  size_mb <- tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NA_real_
  )
  if (!is.na(size_mb) && size_mb > 100) {
    ht_log("INFO", sprintf("Switching to streaming Breusch-Pagan test for dataset of %.1f MB", size_mb))
    adaptive_chunk <- max(1000L, min(25000L, as.integer(nrow(working_data) / 5L)))
    adaptive_chunk <- min(adaptive_chunk, nrow(working_data))
    return(performBPTestStreaming(
      model,
      working_data,
      chunk_size = adaptive_chunk,
      progress = FALSE
    ))
  }
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

  # Classical Breusch-Pagan (1979): regress the scaled squared residuals
  # g_i = e_i^2 / sigma^2 - 1 (with sigma^2 the ML variance estimate) on the
  # regressors and take half the explained sum of squares. The statistic is
  # chi-squared(df) under homoskedasticity *and* normal disturbances. For the
  # studentized form that drops the normality assumption use performKoenkerTest()
  # or performStudentizedBPTest().
  sigma2 <- sum(residuals^2) / n
  scaled_sq <- residuals^2 / sigma2 - 1
  aux_model <- safe_lm(scaled_sq ~ ., data = aux_data)
  fitted_aux <- stats::fitted(aux_model)
  test_statistic <- 0.5 * sum(fitted_aux^2)
  df <- ncol(aux_data)
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
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
