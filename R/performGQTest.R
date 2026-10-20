#' Perform Goldfeld–Quandt test for heteroscedasticity
#'
#' Executes the Goldfeld and Quandt (1965) two-sample test for heteroscedasticity
#' by ordering observations on a suspected variance-driving regressor and
#' comparing residual variances between the lower and upper portions of the
#' sample.
#'
#' @param model A fitted [stats::lm] object representing the regression of
#'   interest.
#' @param data The data frame `model` was fitted to, or `NULL`, the default.
#'   The response, design, weights and offset are read from the fit itself
#'   (since 0.16.0), and `order_by` from the rows of `data` that hold the
#'   observations of the fit. Without `data` it is read from the model frame of
#'   `model`, which holds it only when it is a variable of the model under its
#'   own name (`x`, not `log(x)`); a model that transforms the variable it is
#'   ordered by needs `data`. Data that are given are matched to the
#'   observations of the fit: rows with row names of their own by name, in any
#'   order; rows that only number themselves (a tibble, a data.table, a data
#'   frame whose row names were reset) by the row numbers of the fit, or in the
#'   order of the fit when they hold one row per observation. The rows the fit
#'   dropped may be there or not. Data whose rows cannot be shown to hold the
#'   observations of the fit are refused; see [test_results].
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
#' sample midpoint, a central fraction is omitted and the model is fitted again
#' on the lower and upper segments: each segment is fitted to its rows of the
#' design, the response and the offset of the fit, as [lmtest::gqtest()] fits
#' them to the rows of the design. The statistic is the residual mean square of
#' segment 2 divided by the residual mean square of segment 1. It is therefore
#' directional and is not forced to be greater than one.
#'
#' Before 0.16.0 the segments of an unweighted fit were refitted from the
#' formula, which computed again, on the rows of each segment, a term that
#' depends on the rows it is computed from: `ns(x)` and `bs(x)` placed their
#' knots at quantiles of the segment, and `I(x > median(x))` took its median,
#' so the segments were fitted to columns the fit never used; `poly(x)`
#' computed another basis of the same columns, which changed the last bits
#' only. The refit also left out an offset given as an argument, and a
#' weighted fit was refitted without its offset.
#'
#' The split arithmetic and p-value conventions follow [lmtest::gqtest()] with
#' `point = 0.5`, which permits direct numerical validation against the reference
#' implementation.
#'
#' @section Weighted fits:
#' On a fit with weights each segment is refitted with its own weights and the
#' segments are compared on the weighted residual sum of squares,
#' \eqn{\sum w_i e_i^2}. That is the Goldfeld-Quandt statistic of the equivalent
#' unweighted regression, so the test asks whether the weights are adequate.
#' Releases before 0.12.0 refitted the segments without the weights.
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity
#' link, unweighted or with positive weights, a `fitWLS()` fit and, since
#' 0.16.0, a rank-deficient fit, which it tests on the estimable part of its
#' design: it gives the value of the same model fitted without the aliased
#' columns. It accepts a model with an offset, given as an argument or as an
#' `offset()` term, weighted or not, and gives the value of the model fitted to
#' the response minus the offset (since 0.16.0). It refuses a `glm` of another
#' family or link, a `survey::svyglm()` fit, a fit with a zero weight, a
#' coefficient that is not finite for another reason, and `data` that lack rows
#' or variables of the fit.
#' It refuses a multivariate `lm` ("needs a model with one response"), and,
#' since 0.16.0, an `mgcv::gam()` fit with smooth terms: a smooth term is
#' usually penalised, and a penalised fit is not the least-squares fit of the
#' design the test fits again. A later step settles which tests take such a
#' fit; a `gam` without smooth terms is tested as the Gaussian `glm` it is.
#' Which model each test accepts, and why, is set out
#' in [test_results], under *Supported models*.
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
performGQTest <- function(model, data = NULL, order_by, fraction = 0.2,
                          alternative = c("greater", "two.sided", "less")) {
  test_label <- "Goldfeld-Quandt test"
  # The response, design, weights and offset come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  # `order_by` is read from the rows of `data` that hold the observations, so
  # data taken in the order of the fit must tell those rows apart when it is
  # not a column of the model frame.
  ctx <- rmodel_context(
    model, data, fn = "performGQTest", test_label = test_label,
    reads = if (!missing(order_by) && is.character(order_by)) order_by else character()
  )
  rrefuse_smooth_gam(ctx, "performGQTest")
  alternative <- match.arg(alternative)

  rvalidateModelInputs(model, test_name = "Goldfeld-Quandt", allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  if (!is.numeric(fraction) || length(fraction) != 1L || is.na(fraction) || !is.finite(fraction)) {
    stop("`fraction` must be a finite numeric value.", call. = FALSE)
  }
  if (fraction <= 0 || fraction >= 1) {
    stop("`fraction` must be strictly between 0 and 1.", call. = FALSE)
  }

  # The ordering variable, one value per observation in the order of the fit:
  # from the confirmed rows of `data`, or without `data` from the model frame
  # when it holds it under its own name (rcontext_variables()).
  ordering <- rcontext_variables(ctx, data, c(order_by = order_by))$order_by
  rwarn_incomplete_rows(data, model, also = order_by)
  # A fit with a zero weight is refused where it was before 0.16.0, by the
  # Pearson residuals of the fit (rpearson_residuals()).
  rcontext_pearson(ctx)
  # A missing value of `order_by` at an observation of the fit leaves the
  # observation without a place in the order. Before 0.16.0 the test dropped
  # the rows of `data` with a missing value and then lacked those rows; it
  # still refuses them, and says why.
  rstop_missing_variable(ctx, ordering, order_by, "order_by", test_label)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("goldfeld_quandt", model = model, data = working_data)
  rprocessValidationResult(requirements)

  check_memory_usage(working_data, threshold_mb = 50)
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "This may take some time to compute."
    )
  }

  ht_log("INFO", "Running Goldfeld-Quandt test")

  n <- length(ctx$residuals)
  ord <- order(ordering)

  # Match lmtest::gqtest(point = 0.5, fraction = fraction) exactly. The +0.01
  # avoids an integer-boundary ambiguity before ceiling(), as in the reference.
  point1 <- floor((0.5 - fraction / 2) * n)
  point2 <- ceiling((0.5 + fraction / 2) * n + 0.01)

  # The design of the fit without the columns a rank-deficient fit aliased;
  # its number of columns is the number of fitted coefficients, including the
  # intercept where present.
  design <- rcontext_design(ctx)
  k <- ncol(design)
  if (point2 > n - k + 1L || point1 < k) {
    stop("`fraction` leaves insufficient observations for the two comparison groups.", call. = FALSE)
  }

  g1_index <- seq_len(point1)
  g2_index <- seq.int(point2, n)

  # Each segment is fitted to its rows of the stored design, response and
  # offset of the fit (decided 2026-10-18). Before 0.16.0 an unweighted fit
  # refitted each segment from the formula, which computed again on the rows
  # of the segment a term that depends on the rows it is computed from, such
  # as ns(x), bs(x) or I(x > median(x)), and left out an offset given as an
  # argument; a weighted fit was refitted without its offset.
  #
  # On a weighted fit each segment is fitted with its own weights and compared
  # on the weighted residual sum of squares, sum(w * e^2). That is the
  # Goldfeld-Quandt statistic of the equivalent unweighted fit of sqrt(w) * y
  # on sqrt(w) * X, whose errors are homoscedastic when the weights are right.
  # Refitting without the weights, as releases before 0.12.0 did, compares raw
  # variances that differ by construction.
  response <- ctx$response
  offset <- ctx$offset
  prior_weights <- ctx$weights
  segment_fit <- function(index) {
    rows <- ord[index]
    if (is.null(prior_weights)) {
      fit <- stats::lm.fit(design[rows, , drop = FALSE], response[rows], offset = offset[rows])
      rss <- sum(fit$residuals^2)
    } else {
      fit <- stats::lm.wfit(
        design[rows, , drop = FALSE], response[rows], prior_weights[rows],
        offset = offset[rows]
      )
      rss <- sum(prior_weights[rows] * fit$residuals^2)
    }
    list(rss = rss, df = fit$df.residual)
  }
  segment1 <- segment_fit(g1_index)
  segment2 <- segment_fit(g2_index)
  rss1 <- segment1$rss
  rss2 <- segment2$rss
  df_segment1 <- segment1$df
  df_segment2 <- segment2$df

  if (df_segment1 <= 0 || df_segment2 <= 0) {
    .std_error(
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
