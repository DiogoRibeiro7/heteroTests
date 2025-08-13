#' Perform Hartley's Fmax test
#'
#' Compares the maximum and minimum group variances.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the F statistic and p-value.
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performHartleyFmaxTest(m, mtcars, "cyl")
performHartleyFmaxTest <- function(model, data, group) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!group %in% names(data)) {
    stop("`group` must be a column in `data`.")
  }
  res <- residuals(model)
  grp <- factor(data[[group]])
  vars <- tapply(res, grp, stats::var)
  F_stat <- max(vars) / min(vars)
  k <- length(vars)
  n <- tapply(res, grp, length)
  df <- min(n) - 1
  # Approximate p-value assuming normality
  p_value <- 1 - pf(F_stat, df, df)
  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(groups = k, df = df),
      p.value = p_value,
      method = "Hartley's Fmax test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
