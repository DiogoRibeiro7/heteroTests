#' Perform modified Bartlett test for equality of variances
#'
#' Computes Bartlett's test with the small-sample correction recommended by
#' Snedecor and Cochran to improve performance when group sizes differ.
#'
#' @param model A fitted [stats::lm] object.
#' @param data A [base::data.frame] used to fit `model`.
#' @param group Character scalar naming the grouping variable.
#'
#' @return An object of class \code{htest} with the chi-squared statistic and
#'   p-value.
#'
#' @details
#' The modified statistic divides Bartlett's log-likelihood ratio by a correction
#' factor \eqn{C} that accounts for unequal sample sizes. This reduces size
#' distortions relative to the classical test. The procedure assumes normality and
#' therefore should be paired with robust alternatives (e.g. Levene) when that
#' assumption is questionable.
#'
#' @references
#' Snedecor, G. W., & Cochran, W. G. (1989). *Statistical Methods* (8th ed.). Iowa
#' State University Press. Section 4.8 describes the corrected Bartlett statistic.
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mod <- lm(mpg ~ wt, data = mtcars)
#' performModifiedBartlettTest(mod, mtcars, "cyl")
#'
#' # Compare with the unmodified Bartlett test
#' performBartlettTest(mod, mtcars, "cyl")
#'
#' @seealso
#' [performBartlettTest()] for the classical version and [performLeveneTest()] for
#' a robust alternative.
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
