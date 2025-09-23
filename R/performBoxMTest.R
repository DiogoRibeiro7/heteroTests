#' Box's M test for equality of covariance matrices
#'
#' Applies Box's (1949) likelihood-ratio test to assess whether multiple groups
#' share the same covariance matrix under multivariate normality.
#'
#' @param data A [base::data.frame] or matrix containing numeric variables.
#' @param group A factor or grouping variable aligned with the rows of `data`.
#'
#' @return An object of class \link[stats:htest]{htest} containing the chi-squared statistic
#'   and p-value.
#'
#' @details
#' The test computes pooled and group-specific covariance matrices and forms the
#' Box M statistic, which is approximately chi-squared with \eqn{(g - 1) k (k + 1)/2}
#' degrees of freedom, where \eqn{g} is the number of groups and \eqn{k} the number
#' of variables. A correction factor is applied for small samples as described by
#' Box. The test is sensitive to non-normality, so results should be interpreted
#' alongside robust diagnostics when heavy tails are suspected.
#'
#' @references
#' Box, G. E. P. (1949). A general distribution theory for a class of likelihood
#' criteria. *Biometrika, 36*(3/4), 317–346. <https://doi.org/10.1093/biomet/36.3-4.317>
#'
#' @examples
#' data(iris)
#' performBoxMTest(iris[, 1:4], iris$Species)
#'
#' # Compare only two species
#' subset <- subset(iris, Species != "virginica")
#' performBoxMTest(subset[, 1:4], subset$Species)
#'
#' @seealso
#' [runMultivariateTests()] orchestrates a suite of multivariate diagnostics
#' including Box's M test.
#' @export
performBoxMTest <- function(data, group) {
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("data must be a data.frame or matrix")
  }
  group <- as.factor(group)
  k <- ncol(data)
  groups <- levels(group)
  g <- length(groups)
  n <- nrow(data)
  covmats <- lapply(groups, function(gr) {
    x <- as.matrix(data[group == gr, , drop = FALSE])
    cov(x)
  })
  ns <- as.numeric(table(group))
  pooled <- Reduce(`+`, Map(function(S, n) (n - 1) * S, covmats, ns)) / (n - g)
  M <- (n - g) * log(det(pooled)) - sum((ns - 1) * vapply(covmats, function(S) log(det(S)), 0))
  q <- (2 * k^2 + 3 * k - 1) * (g - 1) / (6 * (k + 1) * (g - 1)) *
    (sum(1 / (ns - 1)) - 1 / (n - g))
  stat <- M * (1 - q)
  df <- (g - 1) * k * (k + 1) / 2
  pval <- pchisq(stat, df, lower.tail = FALSE)
  structure(
    list(
      statistic = c(M = stat), parameter = c(df = df),
      p.value = pval, method = "Box's M Test for Equality of Covariance Matrices"
    ),
    class = "htest"
  )
}
