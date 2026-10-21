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
#' shared helper functions before computing the statistic. A sample variance is
#' defined with two observations, so the test permits groups of size two.
#'
#' @param model A fitted model of class `lm`.
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
performBartlettTest <- function(model, data = NULL, group) {
  test_label <- "Bartlett's test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `group` is read from
  # the rows of `data` that hold the observations, so data taken in the order
  # of the fit must tell those rows apart when it is not a column of the model
  # frame.
  ctx <- rmodel_context(
    model, data, fn = "performBartlettTest", test_label = test_label,
    reads = if (!missing(group) && length(group) == 1L) as.character(group) else character()
  )

  rvalidateModelInputs(model, test_name = "Bartlett", min_obs = 4L, allow_aliased = TRUE)
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
      assumption = "Bartlett test requires observations after removing missing data"
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
    min_group_size = 2L,
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
    .std_error(
      "rassumption_violation",
      assumption = "Bartlett test requires at least two groups"
    )
  }

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Bartlett test requires residual variation greater than numerical precision"
    )
  }

  bt <- stats::bartlett.test(residuals, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(bt$statistic)),
      parameter = c(df = unname(bt$parameter)),
      p.value = bt$p.value,
      method = "Bartlett's test for equality of variances",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
