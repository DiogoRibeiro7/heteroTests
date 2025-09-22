#' Perform Hartley's Fmax test
#'
#' Compares the maximum and minimum group variances.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#'
#' @details
#' Utilises the validation framework to verify the model and grouping structure
#' before computing the Fmax statistic. Missing values in the grouping variable
#' or model terms are removed with an accompanying warning.
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performHartleyFmaxTest(m, mtcars, "cyl")
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
