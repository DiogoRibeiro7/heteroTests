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
#'   observations to omit when splitting the ordered sample. Defaults to `0.2`.
#' @param alternative Character scalar specifying the alternative hypothesis:
#'   `"greater"` tests whether variance increases from the lower to the upper
#'   segment, `"less"` tests whether it decreases, and `"two.sided"` tests for
#'   either direction. Defaults to `"greater"`, matching [lmtest::gqtest()].
#'
#' @return An object of class \code{htest} with the directional F
#'   statistic, degrees of freedom, p-value, and alternative hypothesis.
#'
#' @details
#' Observations are sorted by `order_by`. With the split point fixed at the
#' sample midpoint, a central fraction is omitted and the original model is
#' re-estimated on the lower and upper segments. The statistic is the residual
#' mean square of segment 2 divided by the residual mean square of segment 1.
#' It is therefore directional and is not forced to be greater than one.
#'
#' The split arithmetic and p-value conventions follow [lmtest::gqtest()] with
#' `point = 0.5`, which permits direct numerical validation against the reference
#' implementation.
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
#' performGQTest(mod, mtcars, order_by = "wt", alternative = "two.sided")
#'
#' @seealso
#' [lmtest::gqtest()] for the reference implementation, [performHarveyTest()]
#' and [performParkTest()] for parametric alternatives.
#' @export
performGQTest <- function(model, data, order_by, fraction = 0.2,
                          alternative = c("greater", "two.sided", "less")) {
  test_label <- "Goldfeld-Quandt test"
  alternative <- match.arg(alternative)

  rvalidateModelInputs(model, test_name = "Goldfeld-Quandt")

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  if (!is.numeric(fraction) || length(fraction) != 1L || is.na(fraction) || !is.finite(fraction)) {
    stop("`fraction` must be a finite numeric value.", call. = FALSE)
  }
  if (fraction <= 0 || fraction >= 1) {
    stop("`fraction` must be strictly between 0 and 1.", call. = FALSE)
  }

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))
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

  # Match lmtest::gqtest(point = 0.5, fraction = fraction) exactly. The +0.01
  # avoids an integer-boundary ambiguity before ceiling(), as in the reference.
  point1 <- floor((0.5 - fraction / 2) * n)
  point2 <- ceiling((0.5 + fraction / 2) * n + 0.01)

  # Number of fitted coefficients, including the intercept where present.
  k <- ncol(stats::model.matrix(model))
  if (point2 > n - k + 1L || point1 < k) {
    stop("`fraction` leaves insufficient observations for the two comparison groups.", call. = FALSE)
  }

  g1_index <- seq_len(point1)
  g2_index <- seq.int(point2, n)

  model1 <- safe_lm(stats::formula(model), data = ordered_data[g1_index, , drop = FALSE])
  model2 <- safe_lm(stats::formula(model), data = ordered_data[g2_index, , drop = FALSE])

  rss1 <- sum(stats::residuals(model1)^2)
  rss2 <- sum(stats::residuals(model2)^2)
  df_segment1 <- model1$df.residual
  df_segment2 <- model2$df.residual

  if (df_segment1 <= 0 || df_segment2 <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Goldfeld-Quandt auxiliary regressions require positive residual degrees of freedom"
    )
  }

  mse1 <- rss1 / df_segment1
  mse2 <- rss2 / df_segment2
  gq_stat <- mse2 / mse1
  df_num <- df_segment2
  df_den <- df_segment1

  lower_tail <- stats::pf(gq_stat, df_num, df_den)
  upper_tail <- stats::pf(gq_stat, df_num, df_den, lower.tail = FALSE)
  p_value <- switch(
    alternative,
    "two.sided" = min(1, 2 * min(lower_tail, upper_tail)),
    "less" = lower_tail,
    "greater" = upper_tail
  )

  alternative_label <- switch(
    alternative,
    "two.sided" = "variance changes from segment 1 to 2",
    "less" = "variance decreases from segment 1 to 2",
    "greater" = "variance increases from segment 1 to 2"
  )

  structure(
    list(
      statistic = c(GQ = gq_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Goldfeld-Quandt test for heteroscedasticity",
      data.name = deparse(stats::formula(model)),
      alternative = alternative_label
    ),
    class = "htest"
  )
}
