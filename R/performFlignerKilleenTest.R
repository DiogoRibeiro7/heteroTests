#' Perform Fligner-Killeen test for homogeneity of variances
#'
#' This non-parametric test compares group variances based on ranks. It is
#' robust against non-normality.
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
  fl_res <- fligner.test(res, grp)

  structure(
    list(
      statistic = c("X-squared" = unname(fl_res$statistic)),
      parameter = unname(fl_res$parameter),
      p.value = fl_res$p.value,
      method = "Fligner-Killeen test for homogeneity of variances",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
