#' Perform Fligner–Killeen test for homogeneity of variances
#'
#' Conducts the median-centred Fligner–Killeen test of homogeneity of
#' variances on the Pearson residuals of the fit, with [stats::fligner.test()]:
#' it ranks the absolute deviations of the residuals from their group medians
#' and scores the ranks by quantiles of the normal distribution, which makes it
#' robust to outliers and to errors that are not normal.
#'
#' @param model A fitted [stats::lm] object.
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `group` from the
#'   rows of `data` that hold the observations of the fit; the observations
#'   whose group is missing are left out, with a warning. Without `data` it is
#'   read from the model frame of `model`, which holds it only when it is a
#'   variable of the model under its own name (`x`, not `log(x)`); a grouping
#'   variable the model does not use needs `data`. Since 0.16.0, data that are
#'   given are matched to the observations of the fit: rows with row names of
#'   their own by name, in any order; rows that only number themselves
#'   (a tibble, a data.table, a data frame whose row names were reset) by the
#'   row numbers of the fit, or in the order of the fit when they hold one row
#'   per observation. The rows the fit dropped may be there or not. Data whose
#'   rows cannot be shown to hold the observations of the fit are refused; see
#'   [test_results].
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-squared statistic and
#'   p-value.
#'
#' @details
#' The test is the median-centred version of the test of Fligner and Killeen
#' (1976), the "F-K:med \eqn{X^2}" of Conover, Johnson and Johnson (1981), whose
#' simulation study found it among the tests of homogeneity of variances most
#' robust to departures from normality; it is the test [stats::fligner.test()]
#' computes. The residuals of each group are centred at the median of the
#' group, the absolute values of the centred residuals are ranked over all
#' \eqn{n} observations, and the rank \eqn{r_i} is given the score
#' \eqn{a_i = \Phi^{-1}\{(1 + r_i / (n + 1)) / 2\}}. With \eqn{\bar a_j} the mean
#' score of group \eqn{j}, of size \eqn{n_j}, \eqn{\bar a} the mean of all the
#' scores and \eqn{s_a^2 = \sum_i (a_i - \bar a)^2 / (n - 1)}, the statistic
#' \deqn{X^2 = \sum_j n_j (\bar a_j - \bar a)^2 / s_a^2}
#' is referred to a chi-squared distribution with \eqn{k - 1} degrees of
#' freedom, for \eqn{k} groups. No logarithm is taken. The shared validation
#' helpers verify model compatibility and the minimum sample and group sizes,
#' and the observations whose group is missing are left out, before the
#' statistic is computed.
#'
#' @references
#' Conover, W. J., Johnson, M. E., & Johnson, M. M. (1981). A comparative study
#' of tests for homogeneity of variances, with applications to the outer
#' continental shelf bidding data. *Technometrics, 23*(4), 351–361.
#' <https://doi.org/10.1080/00401706.1981.10487680>
#'
#' Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample tests for
#' scale. *Journal of the American Statistical Association, 71*(353), 210–213.
#' <https://doi.org/10.1080/01621459.1976.10481517>
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performFlignerKilleenTest(mod, mtcars, "cyl")
#'
#' # Rank-based alternative to Bartlett's classical test
#' performBartlettTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performLeveneTest()] and [performBrownForsytheTest()] for other robust
#' variance-equality diagnostics.
performFlignerKilleenTest <- function(model, data = NULL, group) {
  test_label <- "Fligner-Killeen test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performFlignerKilleenTest", test_label = test_label,
    reads = if (!missing(group) && length(group) == 1L) as.character(group) else character()
  )

  rvalidateModelInputs(model, test_name = "Fligner-Killeen", min_obs = 6L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # An NA or empty name is refused here, before it reaches the checks of
  # `data` (rcontext_group_rows()), as by performOBrienTest().
  if (missing(group) || length(group) != 1L || is.na(group) || !nzchar(as.character(group))) {
    stop("`group` must be a single column name.", call. = FALSE)
  }

  group <- as.character(group)

  handle_validation_result <- function(result, warn_patterns = character()) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      messages <- unique(result$messages)
      warn_idx <- rep(FALSE, length(messages))
      if (length(warn_patterns) > 0 && length(messages) > 0) {
        warn_idx <- vapply(
          messages,
          function(msg) any(grepl(warn_patterns, msg, perl = TRUE)),
          logical(1)
        )
      }
      if (any(warn_idx)) {
        for (msg in messages[warn_idx]) {
          warning(msg, call. = FALSE)
        }
      }
      remaining <- messages[!warn_idx]
      if (length(remaining) > 0) {
        stop(paste(remaining, collapse = "\n"), call. = FALSE)
      }
    }
    invisible(result)
  }

  # The Pearson residuals of the fit and the rows of `data` that hold its
  # observations, in the order of the fit, or the model frame without `data`;
  # the observations whose group is missing are left out, with a warning, as
  # before 0.16.0 (rcontext_group_rows()).
  kept <- rcontext_group_rows(ctx, data, group)
  working_data <- kept$data
  residuals <- kept$residuals

  if (length(residuals) == 0 || nrow(working_data) == 0) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires observations after removing missing data"
    )
  }

  validation_frame <- working_data[, group, drop = FALSE]
  requirements <- rvalidateTestRequirements(
    "fligner_killeen",
    model = model,
    data = validation_frame,
    group_var = group,
    min_group_size = 3L
  )
  handle_validation_result(requirements)

  group_vector <- validation_frame[[group]]
  grp <- if (is.factor(group_vector)) {
    base::droplevels(group_vector)
  } else {
    base::droplevels(factor(group_vector))
  }

  if (length(grp) != length(residuals)) {
    stop(
      sprintf(
        "%s detected a mismatch between residuals and grouping variable after cleaning (residuals = %d, groups = %d).",
        test_label,
        length(residuals),
        length(grp)
      ),
      call. = FALSE
    )
  }

  if (length(levels(grp)) < 2L) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires at least two groups"
    )
  }

  finite_res <- residuals[is.finite(residuals)]
  residual_variance <- if (length(finite_res) > 1L) stats::var(finite_res) else NA_real_
  range_span <- if (length(finite_res) > 0L) diff(range(finite_res)) else NA_real_
  scale_reference <- if (length(finite_res) > 0L) {
    max(1, max(abs(finite_res), na.rm = TRUE))
  } else {
    1
  }

  insufficient_variation <- (
    length(finite_res) < 2L ||
      length(unique(finite_res)) < 2L ||
      is.na(residual_variance) ||
      residual_variance <= .Machine$double.eps ||
      (!is.na(range_span) && range_span <= sqrt(.Machine$double.eps) * scale_reference)
  )

  if (insufficient_variation) {
    .std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires variation in residuals for ranking"
    )
  }

  fl_res <- stats::fligner.test(residuals, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(fl_res$statistic)),
      parameter = c(df = unname(fl_res$parameter)),
      p.value = fl_res$p.value,
      method = "Fligner-Killeen test for homogeneity of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
