#' Perform Hartley's Fmax test
#'
#' Computes Hartley's (1950) maximum F-ratio statistic, the ratio of the largest
#' to the smallest group residual variance. Under normality and equal group sizes
#' the statistic follows the maximum-F-ratio distribution, not the ordinary
#' two-sample F distribution.
#'
#' @param model A fitted [stats::lm] object.
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `group` from the
#'   rows of `data` that hold the observations of the fit. Without `data` it is
#'   read from the model frame of `model`, which holds it only when it is a
#'   variable of the model under its own name (`x`, not `log(x)`); a grouping
#'   variable the model does not use needs `data`. Since 0.16.0, data that are
#'   given are matched to the observations of the fit: rows with row names of
#'   their own by name, in any order; rows that only number themselves (a
#'   tibble, a data.table, a data frame whose row names were reset) by the row
#'   numbers of the fit, or in the order of the fit when they hold one row per
#'   observation. The rows the fit dropped may be there or not. Data whose rows
#'   cannot be shown to hold the observations of the fit are refused; see
#'   [test_results].
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} with the Fmax statistic, the number of
#'   groups, the common (or approximate) variance degrees of freedom, and the
#'   upper-tail p-value.
#'
#' @details
#' Hartley's statistic is
#' \deqn{F_{\max} = \max_j s_j^2 / \min_j s_j^2.}
#' For \eqn{k} independent normal groups whose sample variances all have the same
#' degrees of freedom \eqn{\nu}, its null distribution is the maximum-F-ratio
#' distribution. The implementation evaluates that distribution with
#' [SuppDists::pmaxFratio()].
#'
#' The classical test assumes equal group sizes. When the supplied groups are
#' imbalanced, the function warns and rounds the mean group size to obtain an
#' approximate common integer degrees of freedom. Prefer Levene or
#' Brown-Forsythe when the design is materially unbalanced or normality is
#' doubtful.
#'
#' @references
#' Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for
#' heterogeneity of variance. *Biometrika, 37*(3/4), 308–312.
#' <https://doi.org/10.2307/2332383>
#'
#' @section Validation:
#' Pass B compares the p-value with `SuppDists::pmaxFratio()` and with
#' `vartest::hartley.test()` on balanced designs. Releases before 0.7.1 referred
#' \eqn{F_{\max}} to an ordinary two-sample F distribution, which is incorrect
#' whenever more than two variances are being compared and is one-tailed even in
#' the two-group special case.
#'
#' @examples
#' set.seed(1701)
#' n <- 20
#' d <- data.frame(
#'   g = factor(rep(letters[1:3], each = n)),
#'   x = rnorm(3 * n)
#' )
#' d$y <- 1 + d$x + rnorm(3 * n)
#' mod <- lm(y ~ x, data = d)
#' performHartleyFmaxTest(mod, d, "g")
#'
#' @seealso
#' [performBartlettTest()] for a likelihood-based normal-theory test and
#' [performLeveneTest()] and [performBrownForsytheTest()] for more robust
#' group-variance comparisons.
performHartleyFmaxTest <- function(model, data = NULL, group) {
  test_label <- "Hartley's Fmax test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performHartleyFmaxTest", test_label = test_label,
    reads = if (!missing(group) && is.character(group)) group else character()
  )

  if (!is.character(group) || length(group) != 1L || is.na(group) || !nzchar(group)) {
    stop("`group` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Hartley Fmax", min_obs = 10L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The groups, one per observation in the order of the fit: from the
  # confirmed rows of `data`, or without `data` from the model frame when it
  # holds the variable under its own name (rcontext_variables()).
  grp <- rcontext_variables(ctx, data, c(group = group))$group
  rwarn_incomplete_rows(data, model, also = group)
  # The Pearson residuals of the fit; a fit with a zero weight is refused
  # there, as before 0.16.0.
  residuals <- rcontext_pearson(ctx)
  rstop_missing_variable(ctx, grp, group, "group", test_label)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements(
    "hartley_fmax",
    model = model,
    data = working_data,
    group_var = group,
    min_group_size = 2L
  )
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Hartley's Fmax test")

  if (!is.factor(grp) && !is.character(grp)) {
    .std_error(
      "invalid_group_variable",
      group_var = group,
      min_groups = 2L
    )
  }
  grp <- base::droplevels(factor(grp))

  if (length(grp) != length(residuals)) {
    stop(
      sprintf(
        "%s detected a mismatch between residuals and groups (residuals = %d, groups = %d).",
        test_label,
        length(residuals),
        length(grp)
      ),
      call. = FALSE
    )
  }

  vars <- tapply(residuals, grp, stats::var)
  n_i <- tapply(residuals, grp, length)

  if (any(!is.finite(vars))) {
    .std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires finite group variances"
    )
  }
  if (min(vars) <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires positive variance within each group"
    )
  }

  k <- length(vars)
  if (k < 2L) {
    .std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires at least two groups"
    )
  }

  balanced <- length(unique(as.integer(n_i))) == 1L
  if (!balanced) {
    warning(
      paste0(
        "Hartley's Fmax test is exact only for equal group sizes; ",
        "using the rounded mean group size to approximate the common degrees of freedom."
      ),
      call. = FALSE
    )
  }

  df <- if (balanced) {
    as.integer(n_i[[1L]] - 1L)
  } else {
    max(1L, as.integer(round(mean(as.numeric(n_i)))) - 1L)
  }

  if (!is.finite(df) || df <= 0) {
    .std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires positive variance degrees of freedom"
    )
  }

  F_stat <- max(vars) / min(vars)
  p_value <- SuppDists::pmaxFratio(F_stat, df = df, k = k, lower.tail = FALSE)

  if (!is.finite(p_value) || p_value < 0 || p_value > 1) {
    .std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax reference distribution failed to produce a valid p-value"
    )
  }

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(groups = k, df = df),
      p.value = p_value,
      method = "Hartley's Fmax test",
      data.name = deparse(stats::formula(model)),
      alternative = "at least one group variance differs"
    ),
    class = "htest"
  )
}
