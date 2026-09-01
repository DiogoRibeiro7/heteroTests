#' Perform ordered Lagrange Multiplier test
#'
#' Reorders observations by a covariate suspected to drive heteroscedasticity and
#' applies the Breusch–Pagan auxiliary regression to the ordered sample so that
#' one-sided variance changes become more pronounced.
#'
#' @param model A fitted [stats::lm] object representing the conditional mean
#'   specification under scrutiny.
#' @param data A [base::data.frame] containing the variables used in `model` and
#'   the ordering variable.
#' @param order_by Character scalar naming the column in `data` that defines the
#'   ordering of observations prior to running the auxiliary regression.
#'
#' @return An object of class \code{htest} with the chi-squared statistic,
#'   degrees of freedom, and p-value for the null of homoskedasticity.
#'
#' @details
#' Ordering the data by a suspected driver of heteroscedasticity converts gradual
#' variance changes into more pronounced shifts at the tail of the sample. The
#' ordered LM test therefore applies the standard Breusch–Pagan regression to the
#' sorted data, yielding the statistic \eqn{n R^2} with degrees of freedom equal to
#' the number of regressors (excluding the intercept). It is particularly useful
#' when the variance is believed to increase with income, firm size, or another
#' monotonic covariate. The function validates the presence of the ordering
#' variable, ensures the model and data meet minimum sample-size requirements, and
#' checks that the auxiliary regression retains positive degrees of freedom.
#'
#' @references
#' Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*. Cambridge
#' University Press. Section 5.4 discusses ordered alternatives.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performOrderedLMTest(mod, mtcars, order_by = "wt")
#'
#' # Compare how the ordering variable influences the rejection decision
#' performOrderedLMTest(mod, mtcars, order_by = "qsec")
#'
#' @seealso
#' [performGQTest()] for split-sample alternatives and [performSzroeterTest()]
#' for rank-based ordered diagnostics.
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

  # Sorting the rows and refitting by OLS returns the same residuals in a
  # different order, so e^2 ~ X has the same R^2 and `order_by` cannot change
  # the statistic. The result equals performKoenkerTest() exactly. Say so
  # rather than letting the argument imply an effect it does not have.
  warning(
    paste0(
      "performOrderedLMTest() ignores `order_by`: reordering the rows and ",
      "refitting leaves the statistic unchanged, and the result is identical ",
      "to performKoenkerTest(). Use performSzroeterTest() or performGQTest() ",
      "for a genuinely ordered alternative."
    ),
    call. = FALSE
  )

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
