#' Perform Hartley's Fmax test
#'
#' Computes Hartley's (1950) Fmax statistic, the ratio of the largest to the
#' smallest group variance, to detect heterogeneity of variances under the
#' assumption of normality and equal sample sizes.
#'
#' @param model A fitted [stats::lm] object.
#' @param data A [base::data.frame] used to fit `model`.
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @details
#' The test compares \eqn{\max(s_i^2) / \min(s_i^2)} to critical values from the
#' F distribution. It is sensitive to departures from normality and unequal group
#' sizes, so the validation helpers confirm that each group has at least two
#' observations and that the model residuals can support the normal approximation
#' before the statistic is computed.
#'
#' @references
#' Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for
#' heterogeneity of variance. *Biometrika, 37*(3/4), 308–312.
#' <https://doi.org/10.2307/2332383>
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performHartleyFmaxTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performBartlettTest()] and [performLeveneTest()] for alternative variance
#' equality checks.
performHartleyFmaxTest <- function(model, data, group) {
  test_label <- "Hartley's Fmax test"

  if (!is.character(group) || length(group) != 1L || is.na(group) || !nzchar(group)) {
    stop("`group` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Hartley Fmax", min_obs = 10L)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), group))

  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 10L,
    min_obs_data = 10L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements(
    "hartley_fmax",
    model = model,
    data = working_data,
    group_var = group,
    min_group_size = 2L
  )
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Hartley's Fmax test")

  grp <- working_data[[group]]
  if (!is.factor(grp) && !is.character(grp)) {
    std_error(
      "invalid_group_variable",
      group_var = group,
      min_groups = 2L
    )
  }
  grp <- factor(grp)

  vars <- tapply(residuals, grp, stats::var)
  if (any(is.na(vars))) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test could not compute group variances"
    )
  }

  if (min(vars) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires positive variance within each group"
    )
  }

  F_stat <- max(vars) / min(vars)
  k <- length(vars)
  n <- tapply(residuals, grp, length)
  df <- min(n) - 1L
  if (df <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires at least two observations per group"
    )
  }

  p_value <- 1 - stats::pf(F_stat, df, df)

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(groups = k, df = df),
      p.value = p_value,
      method = "Hartley's Fmax test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
