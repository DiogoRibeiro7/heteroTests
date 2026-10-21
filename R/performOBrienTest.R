#' Perform O'Brien test for equality of variances
#'
#' Implements O'Brien's observation-level transformation for testing equality of
#' variances across groups. The transformed residuals are compared with a
#' one-way ANOVA, yielding an approximate F test.
#'
#' @param model A fitted [stats::lm] object.
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself (since 0.16.0), and
#'   `group` from the rows of `data` that hold the observations of the fit;
#'   the observations whose group is missing are left out, with a warning.
#'   Without `data` it is read from the model frame of `model`, which holds it
#'   only when it is a variable of the model under its own name (`x`, not
#'   `log(x)`); a grouping variable the model does not use needs `data`. Data
#'   that are given are matched to the observations of the fit: rows with row
#'   names of their own by name, in any order; rows that only number themselves
#'   (a tibble, a data.table, a data frame whose row names were reset) by the
#'   row numbers of the fit, or in the order of the fit when they hold one row
#'   per observation. The rows the fit dropped may be there or not. Data whose
#'   rows cannot be shown to hold the observations of the fit are refused; see
#'   [test_results].
#' @param group Character scalar specifying the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic, numerator and
#'   denominator degrees of freedom, and p-value.
#'
#' @details
#' For residual \eqn{e_{ij}} in group \eqn{i}, with group size \eqn{n_i}, centre
#' \eqn{\bar e_i}, and sample variance \eqn{s_i^2}, O'Brien's transformation is
#' \deqn{
#' v_{ij} = \frac{(n_i-1.5)n_i(e_{ij}-\bar e_i)^2
#' -0.5(n_i-1)s_i^2}{(n_i-1)(n_i-2)}.
#' }
#' A one-way ANOVA of \eqn{v_{ij}} across the groups gives the test statistic,
#' referred to an \eqn{F_{k-1,N-k}} distribution. Because the denominator contains
#' \eqn{n_i-2}, every group must contain at least three observations.
#'
#' Releases before 0.7.1 instead computed one constant value from each group's
#' sample variance and repeated that value for every observation in the group.
#' That construction has no within-group transformed variation and is not
#' O'Brien's test.
#'
#' @references
#' O'Brien, R. G. (1981). A simple test for variance effects in experimental
#' designs. *Psychological Bulletin, 89*(3), 570-574.
#'
#' @section Validation:
#' Pass B reproduces `vartest::obrien.test()` on the same residuals and grouping
#' factor and independently reconstructs the transformation above.
#'
#' @examples
#' set.seed(1702)
#' n <- 25
#' d <- data.frame(
#'   g = factor(rep(letters[1:3], each = n)),
#'   x = rnorm(3 * n)
#' )
#' d$y <- 2 + d$x + rnorm(3 * n, sd = rep(c(1, 1, 1.8), each = n))
#' mod <- lm(y ~ x, data = d)
#' performOBrienTest(mod, d, "g")
#'
#' @seealso
#' [performLeveneTest()] and [performBrownForsytheTest()] for robust
#' deviation-based tests, and [performBartlettTest()] for the normal-theory
#' likelihood-ratio test.
performOBrienTest <- function(model, data = NULL, group) {
  test_label <- "O'Brien test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performOBrienTest", test_label = test_label,
    reads = if (!missing(group) && length(group) == 1L) as.character(group) else character()
  )

  if (!is.character(group) || length(group) != 1L || is.na(group) || !nzchar(group)) {
    stop("`group` must be supplied as a single column name.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "O'Brien", min_obs = 6L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The Pearson residuals of the fit and the rows of `data` that hold its
  # observations, in the order of the fit, or the model frame without `data`;
  # the observations whose group is missing are left out, with a warning, as
  # before 0.16.0 (rcontext_group_rows()).
  kept <- rcontext_group_rows(ctx, data, group)
  working_data <- kept$data
  residuals <- kept$residuals

  requirements <- rvalidateTestRequirements(
    "obrien",
    model = model,
    data = working_data,
    group_var = group
  )
  rprocessValidationResult(requirements)

  grp_raw <- working_data[[group]]
  if (!is.factor(grp_raw) && !is.character(grp_raw)) {
    .std_error("invalid_group_variable", group_var = group, min_groups = 2L)
  }
  grp <- base::droplevels(factor(grp_raw))

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

  levels_grp <- levels(grp)
  k <- length(levels_grp)
  n_total <- length(residuals)
  if (k < 2L) {
    .std_error(
      "rassumption_violation",
      assumption = "O'Brien test requires at least two groups"
    )
  }

  n_i <- tapply(residuals, grp, length)
  means_i <- tapply(residuals, grp, mean)
  vars_i <- tapply(residuals, grp, stats::var)

  if (any(n_i <= 2L)) {
    .std_error(
      "rassumption_violation",
      assumption = "O'Brien test requires at least three observations per group"
    )
  }
  if (any(!is.finite(vars_i))) {
    .std_error(
      "rassumption_violation",
      assumption = "O'Brien test requires finite variance within each group"
    )
  }

  transformed <- numeric(n_total)
  for (level in levels_grp) {
    idx <- which(grp == level)
    ni <- as.numeric(n_i[[level]])
    si2 <- as.numeric(vars_i[[level]])
    centre <- as.numeric(means_i[[level]])
    transformed[idx] <- (
      (ni - 1.5) * ni * (residuals[idx] - centre)^2 -
        0.5 * (ni - 1) * si2
    ) / ((ni - 1) * (ni - 2))
  }

  aux_data <- data.frame(transformed = transformed, grp = grp)
  aux <- safe_lm(transformed ~ grp, data = aux_data)
  tab <- stats::anova(aux)
  statistic <- unname(tab$`F value`[1L])
  df_num <- unname(tab$Df[1L])
  df_den <- unname(tab$Df[2L])
  p_value <- unname(tab$`Pr(>F)`[1L])

  if (!is.finite(statistic) || !is.finite(p_value)) {
    .std_error(
      "rassumption_violation",
      assumption = "O'Brien transformed-residual ANOVA failed to produce finite inference"
    )
  }

  structure(
    list(
      statistic = c(F = statistic),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "O'Brien test for equality of variances",
      data.name = deparse(stats::formula(model)),
      alternative = "at least one group variance differs"
    ),
    class = "htest"
  )
}
