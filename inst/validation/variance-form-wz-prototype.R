# A prototype of the test of Wang and Zhou (2007) --------------------------------
#
# A second stage 1 of the item "A consistent test of a variance function" in
# ROADMAP.md, after the test of Dette, Neumeyer and Van Keilegom did not hold
# its level (variance-form-dnvk-design.md). Nothing here is exported and
# nothing in R/ is touched: this file only defines functions, in plain R, and is
# sourced by
#
#   inst/validation/variance-form-wz-reproduction.R  (Table 1 of the paper)
#   inst/validation/variance-form-wz-size-power.R    (the package's designs)
#
# The published article (Biometrics 63(4), 1218-1225) could not be obtained.
# The definitions follow the authors' working paper, Wang and Zhou, "Assessing
# the adequacy of variance function in heteroscedastic regression models", UW
# Biostatistics Working Paper Series 299, 7 September 2006, a version revised
# after review. Equation numbers are the working paper's. See
# variance-form-wz-design.md.
#
# The model is Y_i = f(X_i, beta) + eps_i with Var(eps_i | Z_i) =
# g(Z_i, beta, theta) and a mean of known parametric form. The test:
#
#   1. (beta-hat, theta-hat) by pseudo-likelihood (Section 3.1): given beta,
#      theta maximises the normal log-likelihood -(1/2) sum log g - (1/2)
#      sum (Y - f)^2 / g, which (7) misprints with -sum log g (see
#      wz_fit_variance()); given theta, beta solves the generalized
#      least-squares equation (8); the two steps alternate until they settle.
#   2. r-hat_i = (Y_i - f(X_i, beta-hat))^2 - g(Z_i, beta-hat, theta-hat) (2).
#   3. T_n = sum_{i != j} K((Z_i - Z_j) / h) r-hat_i r-hat_j / (n (n - 1) h^q)
#      (3), a leave-one-out kernel estimate of E[r E(r | Z) p(Z)], which is zero
#      exactly under the null hypothesis.
#   4. The bootstrap (Section 3.2): standardize the residuals by the fitted
#      variance function, centre and scale them to mean 0 and variance 1, draw
#      eps*_i from them, set Y*_i = f(X_i, beta-hat) + sqrt(g(Z_i, beta-hat,
#      theta-hat)) eps*_i on the same design, and repeat steps 1 to 3.
#
# Here the mean is linear, f(X, beta) = X beta, and the variance function is
# log-linear in its regressors, g = exp(theta' w): w = (1, z) is the
# exponential form of performVarianceFormTest() and w = (1, log z) its power
# form. One smoothing variable, q = 1.

# --- kernel weights ---------------------------------------------------------------

#' Leave-one-out kernel weights K((Z_i - Z_j) / h) / h, with a zero diagonal
#'
#' The weights depend only on the smoothing variable and the bandwidth, so one
#' matrix serves the observed sample and every bootstrap sample on the same
#' design. The working paper does not name its kernel.
wz_kernel_matrix <- function(z, h, kernel = c("gaussian", "epanechnikov")) {
  kernel <- match.arg(kernel)
  u <- outer(z, z, "-") / h
  k <- switch(kernel,
    gaussian = stats::dnorm(u),
    epanechnikov = 0.75 * (1 - u^2) * (abs(u) <= 1)
  )
  diag(k) <- 0
  k / h
}

# --- pseudo-likelihood --------------------------------------------------------------

#' Pseudo-likelihood estimate of a log-linear variance function, given residuals
#'
#' Maximises -(1/2) sum log v - (1/2) sum e^2 / v over theta, v = exp(W theta),
#' by Fisher scoring with step halving: the score is W'(e^2 / v - 1) / 2 and the
#' expected information W'W / 2. The working paper prints (7) with -sum log g;
#' read literally, its maximiser is half the variance and r-hat would not have
#' mean zero under the null hypothesis, so this is the normal log-likelihood of
#' Carroll and Ruppert (1988), which the paper cites for the method. Starts from
#' `start` or from the regression of log(e^2) on W with its constant moved by
#' 1.27, minus the mean of the log of a chi-squared variable with one degree of
#' freedom. Returns NA when it does not settle within `max_iter` steps.
wz_fit_variance <- function(e2, W, start = NULL, tol = 1e-9, max_iter = 100L) {
  objective <- function(th) {
    lv <- drop(W %*% th)
    -0.5 * sum(lv) - 0.5 * sum(e2 * exp(-lv))
  }
  theta <- start
  if (is.null(theta) || !all(is.finite(theta))) {
    theta <- qr.coef(qr(W), log(pmax(e2, max(e2) * 1e-12)))
    theta[is.na(theta)] <- 0
    theta[1L] <- theta[1L] + 1.27
  }
  WtW <- crossprod(W)
  s <- objective(theta)
  for (iter in seq_len(max_iter)) {
    v <- exp(drop(W %*% theta))
    step <- tryCatch(solve(WtW, crossprod(W, e2 / v - 1)), error = function(err) NULL)
    if (is.null(step) || !all(is.finite(step))) return(rep(NA_real_, ncol(W)))
    lambda <- 1
    repeat {
      candidate <- theta + lambda * drop(step)
      s_new <- objective(candidate)
      if (is.finite(s_new) && s_new >= s) break
      lambda <- lambda / 2
      if (lambda < 1e-10) return(theta)
    }
    moved <- max(abs(candidate - theta))
    theta <- candidate
    s <- s_new
    if (moved <= tol * (1 + max(abs(theta)))) return(theta)
  }
  rep(NA_real_, ncol(W))
}

#' Pseudo-likelihood fit of a linear mean and a log-linear variance function
#'
#' The fixed point of (7) and (8): theta maximises the normal log-likelihood
#' given beta, and beta is the generalized least-squares fit given theta. The
#' working paper alternates the two to convergence. Because the expected
#' information of the normal log-likelihood is block diagonal in (beta, theta),
#' one scoring step for theta followed by the generalized least-squares step
#' for beta is a step of joint Fisher scoring, and the iteration below takes
#' those steps until both settle; it reaches the same fixed point in fewer
#' steps. The variance step starts from wz_fit_variance() on the residuals of
#' `start`, or of least squares.
#' @param y Response.
#' @param X Design matrix of the mean.
#' @param W Design matrix of the log-variance, with its constant column.
#' @param start Optional list with `beta` and `theta` to start from.
#' @return A list with `beta`, `theta`, `fitted`, `residuals` and `variance`, or
#'   NULL when the iteration does not settle.
wz_fit <- function(y, X, W, start = NULL, tol = 1e-9, max_iter = 200L) {
  beta <- if (is.null(start)) qr.coef(qr(X), y) else start$beta
  e <- y - drop(X %*% beta)
  theta <- wz_fit_variance(e^2, W, start = if (is.null(start)) NULL else start$theta)
  if (!all(is.finite(theta))) return(NULL)
  WtW <- crossprod(W)
  loglik <- function(b, th) {
    lv <- drop(W %*% th)
    r <- y - drop(X %*% b)
    -0.5 * sum(lv) - 0.5 * sum(r^2 * exp(-lv))
  }
  s <- loglik(beta, theta)
  for (iter in seq_len(max_iter)) {
    v <- exp(drop(W %*% theta))
    step <- tryCatch(solve(WtW, crossprod(W, e^2 / v - 1)), error = function(err) NULL)
    if (is.null(step) || !all(is.finite(step))) return(NULL)
    # Step halving on the variance step keeps the log-likelihood from falling.
    lambda <- 1
    repeat {
      theta_new <- theta + lambda * drop(step)
      sd_v <- sqrt(exp(drop(W %*% theta_new)))
      beta_new <- qr.coef(qr(X / sd_v), y / sd_v)
      s_new <- loglik(beta_new, theta_new)
      if (all(is.finite(beta_new)) && is.finite(s_new) && s_new >= s - 1e-12 * abs(s)) break
      lambda <- lambda / 2
      if (lambda < 1e-10) return(NULL)
    }
    moved <- max(abs(c(theta_new - theta, beta_new - beta)))
    theta <- theta_new
    beta <- beta_new
    s <- s_new
    e <- y - drop(X %*% beta)
    if (moved <= tol * (1 + max(abs(c(theta, beta))))) {
      return(list(
        beta = beta, theta = theta, fitted = drop(X %*% beta), residuals = e,
        variance = exp(drop(W %*% theta))
      ))
    }
  }
  NULL
}

# --- the statistic ------------------------------------------------------------------

#' T_n of (3) from the residuals r-hat and the kernel weights
wz_statistic <- function(r, Kmat) {
  n <- length(r)
  sum(r * drop(Kmat %*% r)) / (n * (n - 1))
}

#' Standardized T_n with a plug-in variance (not in the working paper)
#'
#' The working paper gives the limit n h^(q/2) T_n -> N(0, tau^2) (6) but no
#' estimate of tau^2. This uses the usual one for a degenerate U-statistic of
#' this kind, Var(T_n) = 2 sum_{i != j} K_ij^2 r_i^2 r_j^2 / (n (n - 1))^2,
#' with K_ij the weights of wz_kernel_matrix(). It is an addition, so that the
#' asymptotic test can be measured beside the bootstrap.
wz_standardized <- function(r, Kmat) {
  n <- length(r)
  t_n <- sum(r * drop(Kmat %*% r)) / (n * (n - 1))
  v <- 2 * sum(r^2 * drop(Kmat^2 %*% r^2)) / (n * (n - 1))^2
  t_n / sqrt(v)
}

#' The test, with its bootstrap
#'
#' The fits do not depend on the bandwidth, so one bootstrap serves several:
#' `Kmats` is a list of kernel weight matrices, one per bandwidth, and every
#' bootstrap sample is evaluated with each of them.
#' @param y,X,W As in wz_fit().
#' @param Kmats List of kernel weights from wz_kernel_matrix().
#' @param B Number of bootstrap samples.
#' @return A list with the observed `statistic` and the standardized `z`, one
#'   per bandwidth, the bootstrap statistics `boot` (a B by bandwidth matrix,
#'   NA where a bootstrap sample could not be fitted), `failed`, and the fit.
#'   NULL when the observed sample cannot be fitted.
wz_bootstrap <- function(y, X, W, Kmats, B) {
  fit <- wz_fit(y, X, W)
  if (is.null(fit)) return(NULL)
  r <- fit$residuals^2 - fit$variance
  observed <- vapply(Kmats, function(K) wz_statistic(r, K), numeric(1))
  z <- vapply(Kmats, function(K) wz_standardized(r, K), numeric(1))
  sd_v <- sqrt(fit$variance)
  # Step 2 of Section 3.2: standardized by the fitted variance function, then
  # centred and scaled to variance 1 (divisor n).
  pool <- fit$residuals / sd_v
  pool <- pool - mean(pool)
  pool <- pool / sqrt(mean(pool^2))
  n <- length(y)
  boot <- matrix(NA_real_, B, length(Kmats))
  for (b in seq_len(B)) {
    ystar <- fit$fitted + sd_v * pool[sample.int(n, n, replace = TRUE)]
    # The bootstrap world's parameters are (beta-hat, theta-hat); starting
    # there changes the number of steps, not the fixed point.
    fb <- wz_fit(ystar, X, W, start = fit)
    if (!is.null(fb)) {
      rb <- fb$residuals^2 - fb$variance
      boot[b, ] <- vapply(Kmats, function(K) wz_statistic(rb, K), numeric(1))
    }
  }
  list(statistic = observed, z = z, boot = boot, failed = sum(is.na(boot[, 1L])), fit = fit)
}

#' P-value by the package's resampling convention, (1 + #{T* >= T}) / (B + 1)
wz_p_value <- function(observed, boot) {
  boot <- boot[!is.na(boot)]
  (1 + sum(boot >= observed)) / (length(boot) + 1)
}

#' Wald test of theta_2 = 0 in exp(theta_0 + theta_1 x + theta_2 x^2)
#'
#' The comparison of the working paper's Table 1: the pseudo-likelihood fit of
#' the quadratic log-variance, which is the normal maximum-likelihood fit, and
#' the variance of theta-hat under normal errors, 2 (W'W)^(-1). Returns the
#' chi-squared statistic with one degree of freedom, or NA.
wz_wald_quadratic <- function(y, X, x) {
  W <- cbind(1, x, x^2)
  fit <- wz_fit(y, X, W)
  if (is.null(fit)) return(NA_real_)
  se2 <- 2 * solve(crossprod(W))[3L, 3L]
  fit$theta[3L]^2 / se2
}

#' The bandwidth used with the package's designs
#'
#' The working paper's simulations use h = 0.10 to 0.25 with X uniform on
#' (0, 1), the same h at 50 and 100 observations, and give no rule. Its theory
#' asks only for h -> 0 and n h^q -> infinity. The rule scales h = c to the
#' range of the variable and lets it shrink at the rate n^(-1/5), so that
#' c = 0.15 gives h = 0.15 at 100 observations on (0, 1).
wz_bandwidth <- function(z, c = 0.15) {
  c * diff(range(z)) * (length(z) / 100)^(-1 / 5)
}
