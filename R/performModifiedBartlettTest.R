#' Perform modified Bartlett test for equality of variances
#'
#' Bartlett's test adjusted with a continuity correction for multiple groups.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param group Character. Name of the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-square statistic and p-value.
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' m <- lm(mpg ~ wt, data = mtcars)
#' performModifiedBartlettTest(m, mtcars, "cyl")
performModifiedBartlettTest <- function(model, data, group) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!group %in% names(data)) {
    stop("`group` must be a column in `data`.")
  }

  grp <- factor(data[[group]])
  res <- residuals(model)
  k <- nlevels(grp)
  n_i <- tapply(res, grp, length)
  s_i2 <- tapply(res, grp, var)
  N <- sum(n_i)
  num <- (N - k) * log(sum((n_i - 1) * s_i2) / (N - k)) - sum((n_i - 1) * log(s_i2))
  C <- 1 + (1 / (3 * (k - 1))) * (sum(1 / (n_i - 1)) - 1 / (N - k))
  chi_sq <- num / C
  df <- k - 1
  p_value <- 1 - pchisq(chi_sq, df)

  structure(
    list(
      statistic = c("X-squared" = chi_sq),
      parameter = df,
      p.value = p_value,
      method = "Modified Bartlett test for equality of variances",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
