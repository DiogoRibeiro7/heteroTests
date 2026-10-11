# A prototype of the test of Dette, Neumeyer and Van Keilegom (2007) -----------
#
# Stage 1 of the work described in ROADMAP.md under "A consistent test of a
# variance function". Nothing here is exported and nothing in R/ is touched:
# this file only defines functions, in plain R, and is sourced by
#
#   inst/validation/variance-form-dnvk-reproduction.R  (a table of the paper)
#   inst/validation/variance-form-dnvk-size-power.R    (the package's designs)
#
# The published article (JRSS B 69(5), 903-917) could not be obtained. The
# definitions below follow the authors' submitted manuscript, Dette and Van
# Keilegom, "A new test for the parametric form of the variance function in
# nonparametric regression", technical report of 9 September 2005 (SFB 475,
# TU Dortmund, and IAP Statistics Network TR 0544), which is the version
# Unpaywall lists for the DOI 10.1111/j.1467-9868.2007.00616.x. Equation
# numbers are those of that manuscript. See variance-form-dnvk-design.md.
#
# The test, for a sample (X_i, Y_i) with one covariate X:
#
#   1. m-hat(x) and sigma-hat^2(x) are Nadaraya-Watson estimates with kernel K
#      and bandwidth h, sigma-hat^2 smoothing the squared residuals (2.8).
#   2. theta-hat minimises sum_i [(Y_i - m-hat(X_i))^2 - sigma_theta^2(X_i)]^2,
#      an unweighted least-squares fit of the variance function to the squared
#      residuals (2.11).
#   3. The parametric fit is smoothed with the same weights,
#      sigma-hat_0^2(x) = sum_i W_i(x, h) sigma_theta-hat^2(X_i).
#   4. eps-hat_i = (Y_i - m-hat(X_i)) / sigma-hat(X_i) and
#      eps-hat_i0 = (Y_i - m-hat(X_i)) / sigma-hat_0(X_i), with empirical
#      distribution functions F-hat and F-hat_0 (2.10, 2.12).
#   5. T_KS = sqrt(n) sup_y |F-hat(y) - F-hat_0(y)| and
#      T_CM = n int [F-hat(y) - F-hat_0(y)]^2 dF-hat(y).
#   6. Smooth residual bootstrap (4.1): eps*_i = eps~_i + v N_i, eps~_i drawn
#      with replacement from the eps-hat_j, N_i standard normal; then
#      Y*_i = m-hat(X_i) + sigma_theta-hat(X_i) eps*_i, and steps 1 to 5 are
#      repeated on (X_i, Y*_i).
#
# Steps 1 to 5 take the mean fit as an argument. The paper's is the
# Nadaraya-Watson smoother of step 1; the package's is a linear model fitted by
# least squares, whose residuals take the place of Y_i - m-hat(X_i). The
# bootstrap refits whichever mean the observed statistic used. The design note
# says what changes, and what the paper's theory does and does not cover.

# --- smoothing ------------------------------------------------------------------

#' Nadaraya-Watson weights at the design points
#'
#' Row i holds W_j(X_i, h), j = 1, ..., n, of (2.9). The weights depend only
#' on the covariate and the bandwidth, so one matrix serves the observed sample
#' and every bootstrap sample drawn on the same design.
dnvk_weights <- function(x, h, kernel = c("epanechnikov", "gaussian")) {
  kernel <- match.arg(kernel)
  u <- outer(x, x, "-") / h
  k <- switch(kernel,
    epanechnikov = 0.75 * (1 - u^2) * (abs(u) <= 1),
    gaussian = stats::dnorm(u)
  )
  k / rowSums(k)
}

# --- mean fits --------------------------------------------------------------------

#' The paper's mean: a Nadaraya-Watson smooth with the weights of the variance
dnvk_mean_nw <- function(W) {
  function(y) {
    fitted <- drop(W %*% y)
    list(fitted = fitted, residuals = y - fitted)
  }
}

#' The package's mean: least squares on a fixed design matrix
dnvk_mean_lm <- function(X) {
  qx <- qr(X)
  function(y) {
    fitted <- drop(qr.fitted(qx, y))
    list(fitted = fitted, residuals = y - fitted)
  }
}

# --- parametric variance functions ----------------------------------------------

# Each class gives sigma_theta^2(x) and the least-squares fit of (2.11). The
# exponential and power forms are those of fitWLS() and
# performVarianceFormTest() with one variance regressor: exp(a + b x) and
# a x^b = exp(log a + b log x). "constant" and "one_plus_theta_x2" are the
# hypotheses of the paper's Examples 4.1 and 4.3.
dnvk_forms <- list(
  constant = list(
    value = function(theta, x) rep(theta[1L], length(x)),
    fit = function(r, x) mean(r)
  ),
  one_plus_theta_x2 = list(
    value = function(theta, x) 1 + theta[1L] * x^2,
    fit = function(r, x) sum(x^2 * (r - 1)) / sum(x^4)
  ),
  exponential = list(
    value = function(theta, x) exp(theta[1L] + theta[2L] * x),
    fit = function(r, x) dnvk_fit_loglinear(r, x)
  ),
  power = list(
    value = function(theta, x) exp(theta[1L] + theta[2L] * log(x)),
    fit = function(r, x) dnvk_fit_loglinear(r, log(x))
  )
)

#' Least squares of r on exp(a + b g), by Gauss-Newton with step halving
#'
#' Starts from the regression of log(r) on g, with the constant moved so that
#' the fitted variances have the mean of r. Returns c(a, b), or NA when the
#' iteration does not settle within `max_iter` steps.
#'
#' The minimum need not exist. When one squared residual at an end of the range
#' of g is large enough, the sum of squares keeps falling as the fitted variance
#' turns into a spike at that point (b to infinity, a to minus infinity). The
#' paper assumes a compact parameter set, which rules this out; here the
#' iteration fails and the caller counts the sample as one the test could not
#' be computed on. How often that happens in the package's designs is in the
#' output of variance-form-dnvk-size-power.R.
dnvk_fit_loglinear <- function(r, g, tol = 1e-9, max_iter = 100L) {
  lr <- log(pmax(r, max(r) * 1e-12))
  b <- stats::cov(g, lr) / stats::var(g)
  a <- log(mean(r) / mean(exp(b * g)))
  theta <- c(a, b)
  objective <- function(th) sum((r - exp(th[1L] + th[2L] * g))^2)
  s <- objective(theta)
  for (iter in seq_len(max_iter)) {
    v <- exp(theta[1L] + theta[2L] * g)
    J <- cbind(v, v * g)
    step <- tryCatch(qr.solve(J, r - v), error = function(e) NULL)
    if (is.null(step) || !all(is.finite(step))) return(c(NA_real_, NA_real_))
    lambda <- 1
    repeat {
      candidate <- theta + lambda * step
      s_new <- objective(candidate)
      if (is.finite(s_new) && s_new <= s) break
      lambda <- lambda / 2
      if (lambda < 1e-10) return(theta)  # no direction of descent left
    }
    moved <- max(abs(candidate - theta))
    theta <- candidate
    s <- s_new
    if (moved <= tol * (1 + max(abs(theta)))) return(theta)
  }
  c(NA_real_, NA_real_)
}

# --- the statistic ----------------------------------------------------------------

#' T_CM and T_KS for one sample
#'
#' @param y Response.
#' @param x The covariate the variance function and the smoother use.
#' @param mean_fit A function of y returning `fitted` and `residuals`.
#' @param W Nadaraya-Watson weights from dnvk_weights(x, h).
#' @param form One element of dnvk_forms.
#' @return A list with `cm`, `ks`, `theta`, the mean `fitted` values and
#'   `residuals`, the standardized residuals `eps` and the fitted `sd_theta`,
#'   or NULL when the variance function could not be fitted.
dnvk_statistic <- function(y, x, mean_fit, W, form) {
  mf <- mean_fit(y)
  e <- mf$residuals
  r <- e^2
  s2 <- drop(W %*% r)
  theta <- form$fit(r, x)
  if (!all(is.finite(theta))) return(NULL)
  s2_theta <- form$value(theta, x)
  s2_0 <- drop(W %*% s2_theta)
  if (!all(is.finite(s2_theta)) || any(s2_theta <= 0) || any(s2_0 <= 0) || any(s2 <= 0)) {
    return(NULL)
  }
  eps <- e / sqrt(s2)
  eps0 <- e / sqrt(s2_0)
  n <- length(y)
  sorted <- sort(eps)
  sorted0 <- sort(eps0)
  # F-hat and F-hat_0 at the jumps of F-hat give the integral against dF-hat;
  # the supremum of the difference of two step functions is reached at a jump
  # of one of them.
  cm <- sum((findInterval(eps, sorted) / n - findInterval(eps, sorted0) / n)^2)
  pooled <- c(eps, eps0)
  ks <- sqrt(n) * max(abs(findInterval(pooled, sorted) - findInterval(pooled, sorted0))) / n
  list(
    cm = cm, ks = ks, theta = theta, fitted = mf$fitted, residuals = e, eps = eps,
    sd_theta = sqrt(s2_theta)
  )
}

#' The bootstrap test
#'
#' @param B Number of bootstrap samples.
#' @param v The smoothing constant of (4.1); the paper uses 0.2.
#' @param unit_variance FALSE, the default, resamples the standardized
#'   residuals as they are, as (4.1) describes. TRUE centres them and scales
#'   them so that eps* has mean 0 and variance 1. The two give the same test
#'   when the family of variance functions is closed under multiplication by a
#'   constant, as the exponential and power forms are; for a family that is not,
#'   such as 1 + theta x^2, only the second generates bootstrap data that
#'   satisfy the null hypothesis.
#' @return A list with the observed `cm` and `ks`, the bootstrap statistics in
#'   `boot` (a B by 2 matrix, NA where a bootstrap sample could not be
#'   fitted), and `failed`, the number of such samples. NULL when the observed
#'   statistic could not be computed.
dnvk_bootstrap <- function(y, x, mean_fit, W, form, B, v = 0.2, unit_variance = FALSE) {
  obs <- dnvk_statistic(y, x, mean_fit, W, form)
  if (is.null(obs)) return(NULL)
  n <- length(y)
  pool <- obs$eps
  if (unit_variance) {
    pool <- pool - mean(pool)
    pool <- pool / sqrt(mean(pool^2) * (1 + v^2))
    v <- v / sqrt(1 + v^2)
  }
  boot <- matrix(NA_real_, B, 2L, dimnames = list(NULL, c("cm", "ks")))
  for (b in seq_len(B)) {
    estar <- pool[sample.int(n, n, replace = TRUE)] + v * stats::rnorm(n)
    ystar <- obs$fitted + obs$sd_theta * estar
    sb <- dnvk_statistic(ystar, x, mean_fit, W, form)
    if (!is.null(sb)) boot[b, ] <- c(sb$cm, sb$ks)
  }
  list(cm = obs$cm, ks = obs$ks, boot = boot, failed = sum(is.na(boot[, 1L])))
}

#' P-value by the package's resampling convention, (1 + #{T* >= T}) / (B + 1)
dnvk_p_value <- function(observed, boot) {
  boot <- boot[!is.na(boot)]
  (1 + sum(boot >= observed)) / (length(boot) + 1)
}

#' The paper's rejection rule (4.2): T > T*_(floor(B (1 - alpha)))
dnvk_reject_paper <- function(observed, boot, alpha) {
  boot <- sort(boot[!is.na(boot)])
  observed > boot[floor(length(boot) * (1 - alpha) + 1e-9)]
}

#' The bandwidth used with the package's designs
#'
#' The paper's simulations use h = 0.3 and h = 0.5 with X uniform on [0, 1] and
#' give no rule. Its theory asks for n h^4 -> 0 and n h^(3 + 2 delta) /
#' log(1 / h) -> infinity, so h of order n^(-0.3) is inside that window. The
#' rule scales the paper's h = c to the range of the covariate and to the
#' sample size, so that c = 0.3 gives the paper's h at 100 observations on
#' [0, 1].
dnvk_bandwidth <- function(x, c = 0.3) {
  c * diff(range(x)) * (length(x) / 100)^(-0.3)
}
