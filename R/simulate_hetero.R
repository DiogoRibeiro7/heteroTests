#' Simulate linear regression data with heteroscedastic errors
#'
#' @param n Integer >= 1: number of observations
#' @param beta0 Numeric: intercept
#' @param beta1 Numeric: slope
#' @param sigma_func Function: takes numeric vector x and returns non-negative
#'   numeric vector of the same length
#' @param seed Integer or NULL: for reproducibility
#' @return A data.frame with columns x and y
#' @examples
#' sim_df <- simulate_hetero(
#'   n = 200, beta0 = 1, beta1 = 2,
#'   sigma_func = sigma_linear, seed = 42
#' )
simulate_hetero <- function(n, beta0, beta1, sigma_func, seed = NULL) {
  stopifnot(is.numeric(n), length(n) == 1, n >= 1, n == as.integer(n))
  stopifnot(is.numeric(beta0), length(beta0) == 1)
  stopifnot(is.numeric(beta1), length(beta1) == 1)
  stopifnot(is.function(sigma_func))
  if (!is.null(seed)) {
    stopifnot(is.numeric(seed), length(seed) == 1)
    set.seed(seed)
  }

  x <- runif(n, min = 0, max = 10)

  sigma <- sigma_func(x)
  stopifnot(is.numeric(sigma), length(sigma) == n, all(sigma >= 0))

  eps <- rnorm(n, mean = 0, sd = 1)
  y <- beta0 + beta1 * x + sigma * eps

  data.frame(x = x, y = y)
}

# --- Sigma functions for different heteroscedastic patterns ---

#' Linear increase: sigma(x) = 0.5 + 0.2 * x
#' @param x Numeric vector
#' @return Numeric vector
sigma_linear <- function(x) {
  stopifnot(is.numeric(x))
  0.5 + 0.2 * abs(x)
}

#' Exponential increase: sigma(x) = exp(0.1 * x)
sigma_exponential <- function(x) {
  stopifnot(is.numeric(x))
  exp(0.1 * x)
}

#' Group-wise: low variance when x < 5, high when x >= 5
sigma_group <- function(x) {
  stopifnot(is.numeric(x))
  ifelse(x < 5, 1.0, 3.0)
}

#' Piecewise: sigma(x) = 0.5 for x < 7, else 2.0
sigma_piecewise <- function(x) {
  stopifnot(is.numeric(x))
  ifelse(x < 7, 0.5, 2.0)
}

#' Polynomial variance: sigma(x) = a + b * x + c * x^2
#' @param x Numeric vector
#' @return Numeric vector
sigma_poly <- function(x, a = 0.5, b = 0.1, c = 0.02) {
  stopifnot(is.numeric(x), is.numeric(a), is.numeric(b), is.numeric(c))
  a + b * x + c * x^2
}

#' Sinusoidal variance: sigma(x) = A + B * sin(omega * x + phi)
#' @param x Numeric vector
#' @return Numeric vector
sigma_sin <- function(x, A = 1, B = 0.5, omega = 2 * pi / 10, phi = 0) {
  stopifnot(
    is.numeric(x), is.numeric(A), is.numeric(B),
    is.numeric(omega), is.numeric(phi)
  )
  A + B * sin(omega * x + phi)
}

#' Multiplicative: variance proportional to |mu(x)|^p where mu(x)=beta0+beta1*x
#' @param x Numeric vector
#' @param mu_func Function returning mean mu for given x
#' @param p Exponent controlling relationship
#' @return Numeric vector
sigma_multiplicative <- function(x, mu_func, p = 1) {
  stopifnot(is.numeric(x), is.function(mu_func), is.numeric(p))
  mu <- mu_func(x)
  abs(mu)^p
}

#' Simulate an ARCH(1) time series with heteroscedastic errors
#'
#' @param n Integer >= 2: length of series
#' @param mu Numeric: constant mean
#' @param alpha0 Numeric >= 0
#' @param alpha1 Numeric in [0, 1)
#' @param seed Integer or NULL
#' @return A data.frame with columns time, y and sigma
simulate_arch1 <- function(n, mu = 0, alpha0 = 0.5, alpha1 = 0.3, seed = NULL) {
  stopifnot(is.numeric(n), length(n) == 1, n >= 2, n == as.integer(n))
  stopifnot(is.numeric(mu), length(mu) == 1)
  stopifnot(is.numeric(alpha0), length(alpha0) == 1, alpha0 >= 0)
  stopifnot(is.numeric(alpha1), length(alpha1) == 1, alpha1 >= 0, alpha1 < 1)
  if (!is.null(seed)) set.seed(seed)

  eps <- numeric(n)
  sigma2 <- numeric(n)
  z <- rnorm(n)

  sigma2[1] <- alpha0 / (1 - alpha1)
  eps[1] <- sqrt(sigma2[1]) * z[1]

  for (t in 2:n) {
    sigma2[t] <- alpha0 + alpha1 * eps[t - 1]^2
    eps[t] <- sqrt(sigma2[t]) * z[t]
  }

  data.frame(time = seq_len(n), y = mu + eps, sigma = sqrt(sigma2))
}

#' Spatial heteroscedasticity: sigma(s) = gamma0 + gamma1 * distance from origin
#' @param coords Matrix or data.frame with columns x and y
#' @param gamma0 Numeric intercept
#' @param gamma1 Numeric slope
#' @return Numeric vector of sigma values
sigma_spatial <- function(coords, gamma0 = 0.5, gamma1 = 0.1) {
  if (!all(c("x", "y") %in% colnames(coords))) {
    stop("coords must have columns 'x' and 'y'.")
  }
  dist <- sqrt(coords$x^2 + coords$y^2)
  gamma0 + gamma1 * dist
}

#' Logistic variance: sigma(x) = L / (1 + exp(-k * (x - x0)))
#' @param x Numeric vector
#' @param L Upper asymptote
#' @param k Growth rate
#' @param x0 Midpoint
#' @return Numeric vector
sigma_logistic <- function(x, L = 2, k = 1, x0 = 5) {
  stopifnot(is.numeric(x), is.numeric(L), is.numeric(k), is.numeric(x0))
  L / (1 + exp(-k * (x - x0)))
}

#' Inverse relationship: sigma(x) = a / (b + x)
#' @param x Numeric vector
#' @param a Scale parameter
#' @param b Positive shift
#' @return Numeric vector
sigma_inverse <- function(x, a = 1, b = 1) {
  stopifnot(is.numeric(x), is.numeric(a), is.numeric(b))
  a / (b + abs(x))
}

#' Power variance: sigma(x) = a * |x|^p
#' @param x Numeric vector
#' @param a Scale factor
#' @param p Power exponent
#' @return Numeric vector
sigma_power <- function(x, a = 0.5, p = 0.5) {
  stopifnot(is.numeric(x), is.numeric(a), is.numeric(p))
  a * (abs(x)^p + 1e-08)
}

#' Threshold step: low variance below `thr`, high variance above
#' @param x Numeric vector
#' @param thr Threshold value
#' @param low Numeric variance below `thr`
#' @param high Numeric variance above `thr`
#' @return Numeric vector
sigma_step <- function(x, thr = 5, low = 0.5, high = 2) {
  stopifnot(is.numeric(x), is.numeric(thr), is.numeric(low), is.numeric(high))
  ifelse(x < thr, low, high)
}

#' U-shaped variance: sigma(x) = a + b * (x - center)^2
#'
#' @param x Numeric vector
#' @param a Base level
#' @param b Curvature
#' @param center Location of minimum variance
#' @return Numeric vector
sigma_u_shape <- function(x, a = 0.1, b = 0.05, center = 5) {
  stopifnot(is.numeric(x), is.numeric(a), is.numeric(b), is.numeric(center))
  a + b * (x - center)^2
}

#' Exponential decay: sigma(x) = a * exp(-b * x)
#'
#' @param x Numeric vector
#' @param a Scale factor
#' @param b Rate of decay
#' @return Numeric vector
sigma_exp_decay <- function(x, a = 2, b = 0.2) {
  stopifnot(is.numeric(x), is.numeric(a), is.numeric(b))
  a * exp(-b * x)
}

#' Gaussian peak variance: base + height * exp(-(x - mu)^2 / (2 * sd^2))
#'
#' @param x Numeric vector
#' @param base Baseline variance
#' @param height Peak height
#' @param mu Peak location
#' @param sd Peak spread
#' @return Numeric vector
sigma_gaussian_peak <- function(x, base = 0.5, height = 1, mu = 5, sd = 1) {
  stopifnot(
    is.numeric(x), is.numeric(base), is.numeric(height),
    is.numeric(mu), is.numeric(sd)
  )
  base + height * exp(-(x - mu)^2 / (2 * sd^2))
}

#' Piecewise linear variance with break at `thr`
#'
#' @param x Numeric vector
#' @param thr Break point
#' @param slope1 Slope below the break
#' @param slope2 Slope above the break
#' @return Numeric vector
sigma_piecewise_linear <- function(x, thr = 5, slope1 = 0.1, slope2 = 0.3) {
  stopifnot(is.numeric(x), is.numeric(thr), is.numeric(slope1), is.numeric(slope2))
  ifelse(x < thr, slope1 * (abs(x) + 1e-08), slope2 * x)
}

# --- End of heteroscedastic simulation utilities ---
