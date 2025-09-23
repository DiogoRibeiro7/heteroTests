#' Perform Bartlett's test for equality of variances
#'
#' Bartlett's test compares the variances of residuals across groups under the
#' assumption of normality. The implementation now leverages the shared
#' validation utilities to ensure that models, data, grouping variables, and
#' normality diagnostics meet the documented requirements before computing the
#' chi-squared statistic.
#'
#' @details
#' Following Bartlett (1937), the test forms a log-likelihood ratio comparing the
#' pooled variance against group-specific variances. Under normality the statistic
#' follows a chi-squared distribution with \eqn{k - 1} degrees of freedom. Because
#' the test is sensitive to deviations from normality, the package validates
#' residual distribution assumptions and sample-size requirements through the
#' shared helper functions before computing the statistic.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-squared statistic and p-value.
#'
#' @references
#' Bartlett, M. S. (1937). Properties of sufficiency and statistical tests.
#' \emph{Proceedings of the Royal Society of London}, 160(901), 268-282.
#' \doi{10.1098/rspa.1937.0109}
#'
#' Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for
#' heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308-312.
#' \doi{10.2307/2332383}
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performBartlettTest(mod, mtcars, "cyl")
#'
#' # Compare with the robust Brown–Forsythe alternative
#' performBrownForsytheTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performLeveneTest()] and [performFlignerKilleenTest()] for robust
#' alternatives when normality is questionable.
performBartlettTest <- function(model, data, group) {
  test_label <- "Bartlett's test"

  rvalidateModelInputs(model, test_name = "Bartlett", min_obs = 6L)

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
      assumption = "Bartlett test requires observations after removing missing data"
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
  residual_col <- ".__residuals__"
  validation_frame[[residual_col]] <- residuals

  requirements <- rvalidateTestRequirements(
    "bartlett",
    model = model,
    data = validation_frame,
    group_var = group,
    min_group_size = 3L,
    variables = residual_col,
    normality_vars = residual_col
  )

  handle_validation_result(requirements, warn_patterns = "Severe non-normality detected")

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
      assumption = "Bartlett test requires at least two groups"
    )
  }

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Bartlett test requires residual variation greater than numerical precision"
    )
  }

  bt <- stats::bartlett.test(residuals, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(bt$statistic)),
      parameter = unname(bt$parameter),
      p.value = bt$p.value,
      method = "Bartlett's test for equality of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
