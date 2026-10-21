#' Perform Brown–Forsythe test for equality of variances
#'
#' Implements the Brown and Forsythe (1974) modification of Levene's test that
#' replaces group means with medians when computing absolute deviations. Using
#' medians improves robustness to non-normality and outliers within each group.
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
#' @param group Character scalar naming the grouping variable in `data`.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @details
#' After validating the model and grouping structure with the shared helpers, the
#' function computes absolute deviations of residuals from the group medians and
#' fits an auxiliary regression on the grouping factor. The resulting ANOVA
#' statistic tests whether the medians of the absolute deviations differ across
#' groups, which signals heteroscedasticity.
#'
#' @references
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
#' variances. *Journal of the American Statistical Association, 69*(346),
#' 364–367. <https://doi.org/10.1080/01621459.1974.10482955>
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performBrownForsytheTest(mod, mtcars, "cyl")
#'
#' # Compare to Levene's mean-based variant
#' performLeveneTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performLeveneTest()] and [performFlignerKilleenTest()] for related
#' group-variance diagnostics.
performBrownForsytheTest <- function(model, data = NULL, group) {
  test_label <- "Brown-Forsythe test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performBrownForsytheTest", test_label = test_label,
    reads = if (!missing(group) && length(group) == 1L) as.character(group) else character()
  )

  rvalidateModelInputs(model, test_name = "Brown-Forsythe", min_obs = 10L, allow_aliased = TRUE)
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
      assumption = "Brown-Forsythe test requires observations after removing missing data"
    )
  }

  validation_frame <- working_data[, group, drop = FALSE]
  requirements <- rvalidateTestRequirements(
    "brown_forsythe",
    model = model,
    data = validation_frame,
    group_var = group,
    min_group_size = 5L
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

  medians <- tapply(residuals, grp, stats::median)
  if (any(!is.finite(medians))) {
    .std_error(
      "rassumption_violation",
      assumption = "Brown-Forsythe test requires finite group medians of residuals"
    )
  }

  centered <- medians[grp]
  if (any(!is.finite(centered))) {
    .std_error(
      "rassumption_violation",
      assumption = "Brown-Forsythe test could not evaluate group medians for all observations"
    )
  }

  z <- abs(residuals - centered)

  if (length(z) <= length(levels(grp))) {
    .std_error(
      "rassumption_violation",
      assumption = "Brown-Forsythe test requires observations to exceed the number of groups"
    )
  }

  aux_df <- data.frame(z = z, grp = grp)
  aov_model <- safe_lm(z ~ grp, data = aux_df)
  aov_table <- stats::anova(aov_model)

  F_stat <- aov_table$`F value`[1]
  df_num <- aov_table$Df[1]
  df_den <- aov_table$Df[2]
  p_value <- aov_table$`Pr(>F)`[1]

  if (!is.finite(F_stat) || !is.finite(df_num) || !is.finite(df_den)) {
    .std_error(
      "rassumption_violation",
      assumption = "Brown-Forsythe auxiliary regression failed to produce finite statistics"
    )
  }

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Brown-Forsythe test for equality of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
