#' Perform Curry–Walsh test for spatial heteroscedasticity
#'
#' Detects spatial clustering of residual variances by applying Moran's I to the
#' squared residuals using inverse-distance weights, following Curry and Walsh
#' (1990).
#'
#' @param model A fitted [stats::lm] object whose residuals correspond to spatial
#'   locations.
#' @param coords Numeric matrix or [base::data.frame] with two columns giving the
#'   planar coordinates associated with each residual. Rows must align with the
#'   observations used to fit `model`.
#'
#' @return An object of class \link[stats:htest]{htest} reporting Moran's I statistic for the
#'   squared residuals and a two-sided p-value based on the normal approximation.
#'
#' @details
#' The test computes the standard Moran's I statistic on \eqn{\hat{e}_i^2} with
#' inverse-distance weights \eqn{w_{ij} = 1 / d_{ij}} (setting \eqn{w_{ii} = 0} and
#' truncating non-finite distances). Significant positive values indicate spatial
#' clustering of large residual variances, while negative values suggest a spatial
#' checkerboard pattern. Because the reference distribution is only approximate for
#' small samples, practitioners may consider permutation resampling for more
#' precise p-values.
#'
#' @references
#' Curry, B., & Walsh, S. (1990). Tests for spatial heteroscedasticity. *Regional
#' Science and Urban Economics, 20*(3), 351–370.
#'
#' Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer Academic
#' Publishers. Chapter 9 discusses using Moran's I for residual diagnostics.
#'
#' @examples
#' set.seed(1)
#' coords <- cbind(x = runif(20), y = runif(20))
#' df <- data.frame(z = rnorm(20), x = coords[, 1])
#' mod <- lm(z ~ x, data = df)
#' performCurryWalshTest(mod, coords)
#'
#' # Spatial variance clustering yields large positive statistics
#' set.seed(123)
#' coords2 <- cbind(x = runif(30), y = runif(30))
#' region_effect <- rnorm(30, sd = 0.6)
#' df2 <- data.frame(y = region_effect + rnorm(30, sd = 0.2), x = coords2[, 1])
#' performCurryWalshTest(lm(y ~ x, data = df2), coords2)
#'
#' @seealso
#' [performPesaranTest()] for cross-sectional dependence diagnostics in panels and
#' [performGQTest()] when heteroscedasticity varies along a single ordering
#' variable.
performCurryWalshTest <- function(model, coords) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.matrix(coords) && !is.data.frame(coords)) {
    stop("`coords` must be a matrix or data frame with two columns.")
  }
  if (ncol(coords) != 2) {
    stop("`coords` must have two columns.")
  }

  res2 <- residuals(model)^2
  n <- length(res2)
  dist_mat <- as.matrix(dist(coords))
  W <- 1 / dist_mat
  diag(W) <- 0
  W[!is.finite(W)] <- 0
  S0 <- sum(W)
  res_c <- res2 - mean(res2)
  I <- (n / S0) * sum(res_c * (W %*% res_c)) / sum(res_c^2)
  p_value <- 2 * (1 - pnorm(abs(I)))

  structure(
    list(
      statistic = c(I = I),
      parameter = NULL,
      p.value = p_value,
      method = "Curry-Walsh test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
