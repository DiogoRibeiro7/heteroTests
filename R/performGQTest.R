#' Perform Goldfeld–Quandt test for heteroscedasticity
#'
#' Executes the Goldfeld and Quandt (1965) two-sample test for heteroscedasticity
#' by ordering observations on a suspected variance-driving regressor and
#' comparing residual variances between the lower and upper portions of the
#' sample.
#'
#' @param model A fitted [stats::lm] object representing the regression of
#'   interest.
#' @param data A [base::data.frame] containing the variables used to estimate
#'   `model`. All rows used to fit the model must be present.
#' @param order_by Character scalar giving the name of the variable that orders
#'   the data prior to forming the two subsamples.
#' @param fraction Numeric scalar in \((0, 1)\) specifying the fraction of central
#'   observations to omit when splitting the ordered sample. Defaults to `0.2` as
#'   recommended by Goldfeld and Quandt.
#'
#' @return An object of class \link[stats:htest]{htest} with the F statistic and p-value for
#'   the null of constant variance.
#'
#' @details
#' Observations are sorted by `order_by`, the middle fraction `fraction` is
#' discarded, and separate models are estimated on the lower and upper regimes.
#' The ratio of residual sum of squares forms an F statistic with degrees of
#' freedom equal to the residual degrees from each regime. The function employs
#' the shared validation helpers: \link[=rvalidateModelInputs]{rvalidateModelInputs()} confirms the model is
#' eligible, \link[=rvalidateDataInputs]{rvalidateDataInputs()} and \link[=rhandleMissingValues]{rhandleMissingValues()} align the data
#' with the fitted values, and \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} enforces minimum
#' sample sizes and ordering assumptions specific to the test.
#'
#' @references
#' Goldfeld, S. M., & Quandt, R. E. (1965). Some tests for homoscedasticity.
#' *Journal of the American Statistical Association, 60*(310), 539–547.
#' <https://doi.org/10.1080/01621459.1965.10480811>
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performGQTest(mod, mtcars, order_by = "wt")
#'
#' # Sensitivity to the trimming fraction
#' performGQTest(mod, mtcars, order_by = "wt", fraction = 0.1)
#'
#' @seealso
#' [performHarveyTest()] and [performParkTest()] provide parametric
#' alternatives when the functional form of heteroscedasticity is specified.
performGQTest <- function(model, data, order_by, fraction = 0.2) {
  test_label <- "Goldfeld-Quandt test"

  rvalidateModelInputs(model, test_name = "Goldfeld-Quandt")

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  if (!is.numeric(fraction) || length(fraction) != 1L || is.na(fraction)) {
    stop("`fraction` must be a finite numeric value.", call. = FALSE)
  }
  if (fraction <= 0 || fraction >= 1) {
    stop("`fraction` must be strictly between 0 and 1.", call. = FALSE)
  }

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label
  )

  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("goldfeld_quandt", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    std_error("missing_variable", variable = order_by)
  }

  check_memory_usage(working_data, threshold_mb = 50)
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "This may take some time to compute."
    )
  }

  ht_log("INFO", "Running Goldfeld-Quandt test")

  n <- nrow(working_data)
  ordered_data <- working_data[order(working_data[[order_by]]), , drop = FALSE]

  omit_n <- floor(n * fraction)
  available <- n - omit_n
  group_size <- floor(available / 2)
  if (group_size < 2) {
    stop("`fraction` leaves insufficient observations for the two comparison groups.", call. = FALSE)
  }

  g1_index <- seq_len(group_size)
  g2_index <- seq(from = nrow(ordered_data) - group_size + 1L, to = nrow(ordered_data))

  model1 <- safe_lm(stats::formula(model), data = ordered_data[g1_index, , drop = FALSE])
  model2 <- safe_lm(stats::formula(model), data = ordered_data[g2_index, , drop = FALSE])

  sse1 <- sum(stats::residuals(model1)^2)
  sse2 <- sum(stats::residuals(model2)^2)
  df1 <- model1$df.residual
  df2 <- model2$df.residual

  if (df1 <= 0 || df2 <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Goldfeld-Quandt auxiliary regressions require positive residual degrees of freedom"
    )
  }

  if (sse1 > sse2) {
    F_stat <- (sse1 / df1) / (sse2 / df2)
    df_num <- df1
    df_den <- df2
  } else {
    F_stat <- (sse2 / df2) / (sse1 / df1)
    df_num <- df2
    df_den <- df1
  }

  p_value <- 1 - stats::pf(F_stat, df_num, df_den)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Goldfeld-Quandt test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
