#' Perform Bartlett's test for equality of variances
#'
#' Bartlett's test compares the variances of residuals across groups under the
#' assumption of normality.
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
#' performBartlettTest(m, mtcars, "cyl")
performBartlettTest <- function(model, data, group) {
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
  bt <- bartlett.test(res, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(bt$statistic)),
      parameter = unname(bt$parameter),
      p.value = bt$p.value,
      method = "Bartlett's test for equality of variances",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
