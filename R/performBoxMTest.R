#' Box's M Test for Equality of Covariance Matrices
#'
#' Perform Box's M test on multivariate data grouped by a factor.
#'
#' @param data A data.frame or matrix containing numeric variables.
#' @param group A factor or grouping variable of the same length as rows of `data`.
#'
#' @return A list with class `htest` containing the test statistic and p-value.
#' @examples
#' data(iris)
#' performBoxMTest(iris[, 1:4], iris$Species)
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
