#' Perform Curry-Walsh test for spatial heteroscedasticity
#'
#' Uses Moran's I statistic on squared residuals to detect spatial patterns of
#' heteroscedasticity.
#'
#' @param model A fitted model of class `lm`.
#' @param coords A matrix or data frame with coordinates (two columns).
#'
#' @return An object of class \code{htest} with the statistic and p-value.
#' @examples
#' set.seed(1)
#' coords <- cbind(x = runif(20), y = runif(20))
#' df <- data.frame(z = rnorm(20), x = coords[, 1])
#' m <- lm(z ~ x, data = df)
#' performCurryWalshTest(m, coords)
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
