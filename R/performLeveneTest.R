#' Perform Levene's test for equality of variances
#'
#' Implements Levene's (1960) robust test for equality of variances across
#' groups defined by a categorical factor. The test operates on the absolute
#' deviations of residuals from group means (or medians in Brown–Forsythe's
#' refinement) and uses an ANOVA on those deviations to detect variance
#' heterogeneity.
#'
#' @details
#' The function computes group-wise absolute deviations from the fitted values and
#' fits an auxiliary linear model with the grouping factor. Under homoskedasticity
#' the group effect has zero mean and the resulting F statistic follows the
#' standard ANOVA reference distribution. Prior to the analysis, the shared
#' validation helpers ensure that the model, data, and grouping variable satisfy
#' the minimum sample-size and structure requirements.
#'
#' @param model A fitted [stats::lm] object supplying residuals for the test.
#' @param data A [base::data.frame] containing the variables used in `model` and
#'   the grouping factor.
#' @param group Character scalar specifying the column in `data` that defines the
#'   groups whose variances are compared.
#'
#' @return An object of class \link[stats:htest]{htest} with the F statistic and p-value.
#'
#' @references
#' Levene, H. (1960). Robust tests for equality of variances. In
#' \emph{Contributions to Probability and Statistics} (pp. 278-292).
#' Stanford University Press.
#'
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality
#' of variances. \emph{Journal of the American Statistical Association},
#' 69(346), 364-367. \doi{10.1080/01621459.1974.10482955}
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performLeveneTest(mod, mtcars, "cyl")
#'
#' # Compare to the Brown–Forsythe median-based variant
#' performBrownForsytheTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performBrownForsytheTest()] for the median-based adaptation,
#' [performFlignerKilleenTest()] for a rank-based alternative, and
#' [performBartlettTest()] when normality can be assumed.
performLeveneTest <- function(model, data, group) {
  test_label <- "Levene's test"

  rvalidateModelInputs(model, test_name = "Levene", min_obs = 15L)

  if (missing(group) || length(group) != 1L) {
    stop("`group` must be a single column name.", call. = FALSE)
  }

  group <- as.character(group)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), group))
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = 15L)

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

  align_to_model <- function(raw_data, residuals) {
    resid_names <- names(residuals)
    data_rows <- rownames(raw_data)

    if (!is.null(resid_names) && length(resid_names) > 0 && !is.null(data_rows)) {
      match_idx <- match(resid_names, data_rows)
      if (anyNA(match_idx)) {
        missing_rows <- resid_names[is.na(match_idx)]
        stop(
          sprintf(
            "%s requires `data` to contain the rows used to fit the model. Missing rows: %s",
            test_label,
            paste(utils::head(missing_rows, 3L), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      aligned_data <- raw_data[match_idx, , drop = FALSE]
      list(data = aligned_data, residuals = residuals)
    } else {
      if (nrow(raw_data) != length(residuals)) {
        stop(
          sprintf(
            "%s requires `data` with %d observations to match the fitted model, got %d.",
            test_label,
            length(residuals),
            nrow(raw_data)
          ),
          call. = FALSE
        )
      }
      list(data = raw_data, residuals = residuals)
    }
  }

  aligned <- align_to_model(data, stats::residuals(model))
  working_data <- aligned$data
  residuals <- aligned$residuals

  cleaned <- rhandleMissingValues(working_data, variables = required_vars)
  if (cleaned$removed_count > 0) {
    residuals <- residuals[-cleaned$removed_cases]
  }
  working_data <- cleaned$data

  if (length(residuals) == 0 || nrow(working_data) == 0) {
    std_error(
      "rassumption_violation",
      assumption = "Levene test requires observations after removing missing data"
    )
  }

  if (length(residuals) != nrow(working_data)) {
    stop(
      sprintf(
        "%s could not align residuals with the cleaned data (expected %d rows, got %d).",
        test_label,
        length(residuals),
        nrow(working_data)
      ),
      call. = FALSE
    )
  }

  validation_frame <- working_data[, group, drop = FALSE]
  requirements <- rvalidateTestRequirements(
    "levene",
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

  means <- tapply(residuals, grp, mean)
  if (any(!is.finite(means))) {
    std_error(
      "rassumption_violation",
      assumption = "Levene test requires finite group means of residuals"
    )
  }

  centered <- means[grp]
  if (any(!is.finite(centered))) {
    std_error(
      "rassumption_violation",
      assumption = "Levene test could not evaluate group means for all observations"
    )
  }

  z <- abs(residuals - centered)

  if (length(z) <= length(levels(grp))) {
    std_error(
      "rassumption_violation",
      assumption = "Levene test requires observations to exceed the number of groups"
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
    std_error(
      "rassumption_violation",
      assumption = "Levene test auxiliary regression failed to produce finite statistics"
    )
  }

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Levene's test for equality of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
