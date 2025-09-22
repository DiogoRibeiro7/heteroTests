#' Perform Fligner-Killeen test for homogeneity of variances
#'
#' This non-parametric test compares group variances based on ranks. The
#' function integrates the validation framework so that model inputs, grouping
#' variables, and sample sizes are checked and missing values removed prior to
#' computing the rank-based statistic.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-squared statistic and p-value.
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performFlignerKilleenTest(m, mtcars, "cyl")
performFlignerKilleenTest <- function(model, data, group) {
  test_label <- "Fligner-Killeen test"

  rvalidateModelInputs(model, test_name = "Fligner-Killeen", min_obs = 6L)

  if (missing(group) || length(group) != 1L) {
    stop("`group` must be a single column name.", call. = FALSE)
  }

  group <- as.character(group)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), group))
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = 6L)

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
      assumption = "Fligner-Killeen test requires observations after removing missing data"
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
    std_error(
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
    std_error(
      "rassumption_violation",
      assumption = "Fligner-Killeen test requires variation in residuals for ranking"
    )
  }

  fl_res <- stats::fligner.test(residuals, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(fl_res$statistic)),
      parameter = unname(fl_res$parameter),
      p.value = fl_res$p.value,
      method = "Fligner-Killeen test for homogeneity of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
