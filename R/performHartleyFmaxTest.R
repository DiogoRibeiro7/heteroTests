#' Perform Hartley's Fmax test
#'
#' Computes Hartley's (1950) maximum F-ratio statistic, the ratio of the largest
#' to the smallest group residual variance. Under normality and equal group sizes
#' the statistic follows the maximum-F-ratio distribution, not the ordinary
#' two-sample F distribution.
#'
#' @param model A fitted [stats::lm] object.
#' @param data A [base::data.frame] used to fit `model`.
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
#' imbalanced, the function warns and uses the mean group size minus one as an
#' approximate common degree of freedom, matching the default approximation used
#' by established R implementations. Prefer Levene or Brown-Forsythe when the
#' design is materially unbalanced or normality is doubtful.
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
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires finite group variances"
    )
  }
  if (min(vars) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires positive variance within each group"
    )
  }

  k <- length(vars)
  if (k < 2L) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires at least two groups"
    )
  }

  balanced <- length(unique(as.integer(n_i))) == 1L
  if (!balanced) {
    warning(
      paste0(
        "Hartley's Fmax test is exact only for equal group sizes; ",
        "using the mean group size to approximate the common degrees of freedom."
      ),
      call. = FALSE
    )
  }

  # The maximum-F-ratio distribution assumes k independent mean squares with a
  # common number of degrees of freedom. For an unbalanced design this is an
  # explicit approximation rather than silently treating Fmax as an ordinary F.
  df <- if (balanced) as.numeric(n_i[[1L]] - 1L) else mean(as.numeric(n_i)) - 1
  if (!is.finite(df) || df <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax test requires positive variance degrees of freedom"
    )
  }

  F_stat <- max(vars) / min(vars)
  p_value <- SuppDists::pmaxFratio(F_stat, df = df, k = k, lower.tail = FALSE)

  if (!is.finite(p_value) || p_value < 0 || p_value > 1) {
    std_error(
      "rassumption_violation",
      assumption = "Hartley's Fmax reference distribution failed to produce a valid p-value"
    )
  }

  structure(
    list(
      statistic = c("F-max" = F_stat),
      parameter = c(groups = k, df = df),
      p.value = p_value,
      method = "Hartley's maximum F-ratio test",
      data.name = deparse(stats::formula(model)),
      alternative = "at least one group variance differs"
    ),
    class = "htest"
  )
}
