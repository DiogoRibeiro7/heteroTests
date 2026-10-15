#' Harrison-McCabe test for heteroscedasticity
#'
#' Orders the observations by a variable and tests whether the share of the
#' residual sum of squares that falls in the first part of the ordered sample
#' is too small, or too large, for errors of constant variance. The p-value is
#' exact under normal errors, given the regressors.
#'
#' @param model A fitted [stats::lm] object, with or without weights.
#' @param data A [base::data.frame] containing the variables used to fit
#'   `model` and the ordering variable named by `order_by`.
#'   When `NULL`, the default, the model frame of `model` is used, so `order_by`
#'   must then be a variable of the model. The model frame holds the terms of
#'   the model rather than its variables (`log(x)` rather than `x`), so a model
#'   that transforms a variable needs `data`, as does a
#'   model with an `offset()` term.
#' @param order_by Character scalar naming the column of `data` that orders the
#'   observations. Tied values keep the order the observations have in the
#'   fitted model.
#' @param point The size of the first part of the ordered sample. A number
#'   strictly between 0 and 1 is a fraction of the sample, and the first part
#'   then holds `floor(point * n)` observations, as in [lmtest::hmctest()]. A
#'   whole number of 1 or more is the number of observations. Each part must
#'   hold at least as many observations as the model has coefficients.
#' @param alternative Character scalar. `"less"` (the default) is the
#'   alternative that the variance increases with `order_by`, which makes the
#'   share small; `"greater"` that it decreases; `"two.sided"` either. The
#'   two-sided p-value is twice the smaller of the two one-sided ones, capped
#'   at 1, as in [performGQTest()] and in the default of
#'   `skedastic::harrison_mccabe()`.
#'
#' @return An object of class \code{htest} with the share `HMC` as the
#'   statistic, the number of observations in the first part `m` and the sample
#'   size `n` as parameters, the p-value, and the alternative in words.
#'
#' @details
#' With \eqn{\hat{e}_{(1)}, \dots, \hat{e}_{(n)}}{e(1), ..., e(n)} the
#' least-squares residuals in the order of `order_by`, the statistic of
#' Harrison and McCabe (1979) is
#' \deqn{b = \frac{\sum_{i=1}^{m} \hat{e}_{(i)}^2}{\sum_{i=1}^{n} \hat{e}_{(i)}^2},}{
#' b = sum(e[1:m]^2) / sum(e^2),}
#' the share of the residual sum of squares that falls in the first \eqn{m}
#' observations. A variance that increases along the ordering makes \eqn{b}
#' small, and one that decreases makes it large. It is the statistic of
#' [lmtest::hmctest()].
#'
#' The 1979 article was not consulted for this implementation. The definition
#' follows Harrison (1981), by its first author, which restates the statistic
#' with \eqn{m} the integer part of \eqn{n / 2} and rejects for small
#' \eqn{b}, and it agrees with [lmtest::hmctest()].
#'
#' Under normal errors of constant variance, \eqn{b} is a ratio of quadratic
#' forms in normal variables, and its distribution depends on the regressors.
#' With \eqn{M} the residual-maker of the design in the ordered sample and
#' \eqn{D} the diagonal matrix that selects the first \eqn{m} observations,
#' \deqn{P(b \le b_0) = P\left(\sum_j (\lambda_j - b_0) z_j^2 \le 0\right),}{
#' P(b <= b0) = P(sum_j (lambda_j - b0) z_j^2 <= 0),}
#' where the \eqn{z_j} are independent standard normal and the
#' \eqn{\lambda_j} are the \eqn{n - k} eigenvalues of \eqn{M D M} on the
#' residual space, \eqn{k} being the number of coefficients. They are found
#' from the \eqn{k} singular values of the first \eqn{m} rows of an
#' orthonormal basis of the design, and the probability is evaluated by
#' Imhof's (1961) formula with [stats::integrate()], as Harrison (1981) does
#' for the exact variant of the test. For a constant and a linear trend, the
#' 5% critical values this gives at 10, 20, 30 and 40 observations are those
#' of Harrison's (1981) Table 3 to the three decimals printed there. Under the
#' null hypothesis the mean of \eqn{b} is
#' \eqn{(m - \sum_{i \le m} h_{(i)}) / (n - k)}{(m - sum(h[1:m])) / (n - k)},
#' with \eqn{h_{(i)}}{h} the leverages in the ordered sample.
#'
#' The probability is computed to an absolute accuracy of about \code{2e-11}.
#' Smaller p-values are not resolved, and a statistic far in a tail can return
#' a p-value of 0.
#'
#' The p-value depends only on the space spanned by the regressors, so it does
#' not change with the parametrisation of the model.
#'
#' @section Direction of the alternative:
#' In [performGQTest()] and [performSzroeterTest()] the statistic grows when
#' the variance increases along the ordering, and in this test it shrinks. The
#' alternative names the tail of each statistic, so the same hypothesis, a
#' variance that increases with `order_by`, is `alternative = "less"` here and
#' `alternative = "greater"` in those two functions. In all three it is the
#' default, and the `alternative` element of the result states it in words.
#'
#' @section Weighted fits:
#' On a fit with weights the statistic is computed from the Pearson residuals
#' \eqn{\sqrt{w_i}\, \hat{e}_i}{sqrt(w) * e}, and the null distribution from
#' the weighted design \eqn{\sqrt{w_i}\, x_i}{sqrt(w) * x}. Both are those of
#' the equivalent unweighted regression, so the test asks whether the weights
#' are adequate.
#'
#' @section Heavy tails:
#' The null distribution assumes normal errors. In the designs of
#' `inst/validation/harrison-mccabe-size-power.R` the test rejects a true null
#' hypothesis 11.7% to 16.3% of the time at the 5% level when the errors
#' follow a \eqn{t_5}{t5} distribution, and more often the larger the sample;
#' [performGQTest()] and [performSzroeterTest()] do the same. When the tails
#' may be heavy, use [performGlejserTest()] with the ordering variable as
#' `variable` and `robust = TRUE`, which in the same study rejects 4.7% to
#' 4.9% of the time under those errors. Its default transformation takes the
#' absolute value of the variable, which preserves the ordering only when the
#' variable is positive.
#'
#' @section Reference implementations:
#' The statistic agrees with [lmtest::hmctest()] to \code{1e-8}. Its p-value
#' does not: with `simulate.p = TRUE` it is simulated from residuals of a
#' regression on a constant alone, whatever the model, and with
#' `simulate.p = FALSE` it is `NA`. `skedastic::harrison_mccabe()` reports a
#' different p-value as well; `inst/validation/README.md` says why.
#'
#' @references
#' Harrison, M. J. (1981). A comparison of the bounds, beta-approximate, and
#' exact variants of two tests for heteroscedasticity based on ordinary least
#' squares residuals. *The Economic and Social Review, 12*(4), 235--252.
#'
#' Harrison, M. J., & McCabe, B. P. M. (1979). A test for heteroscedasticity
#' based on ordinary least squares residuals. *Journal of the American
#' Statistical Association, 74*(366a), 494--499.
#' \doi{10.1080/01621459.1979.10482544}
#'
#' Imhof, J. P. (1961). Computing the distribution of quadratic forms in
#' normal variables. *Biometrika, 48*(3-4), 419--426.
#' \doi{10.1093/biomet/48.3-4.419}
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performHarrisonMcCabeTest(mod, mtcars, order_by = "wt")
#'
#' # Variance that increases with x
#' set.seed(7)
#' x <- runif(80, 1, 5)
#' d <- data.frame(x = x, y = 1 + 2 * x + rnorm(80, sd = exp(0.3 * x)))
#' performHarrisonMcCabeTest(lm(y ~ x, data = d), d, order_by = "x")
#'
#' # Either direction, with a third of the sample in the first part
#' performHarrisonMcCabeTest(lm(y ~ x, data = d), d,
#'   order_by = "x",
#'   point = 1 / 3, alternative = "two.sided"
#' )
#'
#' @seealso
#' [performGQTest()] and [performSzroeterTest()] for the other tests along an
#' ordering of the observations; [performGlejserTest()] when the errors may be
#' heavy-tailed.
#' @export
performHarrisonMcCabeTest <- function(model, data = NULL, order_by, point = 0.5,
                                      alternative = c("less", "two.sided", "greater")) {
  data <- rdata_or_model_frame(model, data)
  test_label <- "Harrison-McCabe test"

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }
  alternative <- match.arg(alternative)
  if (!is.numeric(point) || length(point) != 1L || !is.finite(point) || point <= 0) {
    stop(
      "`point` must be a single positive number: a fraction of the sample below 1, ",
      "or a number of observations.",
      call. = FALSE
    )
  }
  if (point >= 1 && point != floor(point)) {
    stop("`point` of 1 or more is a number of observations and must be a whole number.",
      call. = FALSE
    )
  }

  rvalidateModelInputs(model, test_name = "Harrison-McCabe")
  rrefuse_glm(
    model, "performHarrisonMcCabeTest",
    "its null distribution is that of least-squares residuals."
  )

  required_vars <- unique(c(all.vars(stats::terms(model)), order_by))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label
  )
  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("harrison_mccabe", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    .std_error("missing_variable", variable = order_by)
  }

  ht_log("INFO", "Running Harrison-McCabe test")

  # The residuals and the design of the equivalent unweighted regression: the
  # Pearson residuals sqrt(w) * e and the rows sqrt(w) * x on a weighted fit,
  # the ordinary ones otherwise.
  residuals <- unname(prepared$residuals)
  design <- stats::model.matrix(model)
  weights <- rprior_weights(model)
  if (!is.null(weights)) {
    design <- sqrt(weights) * design
  }
  n <- length(residuals)
  if (nrow(design) != n) {
    stop(
      sprintf(
        "%s could not align the model matrix with the residuals (%d rows, %d residuals).",
        test_label, nrow(design), n
      ),
      call. = FALSE
    )
  }

  # The residuals and the design are put in the same order: the null
  # distribution depends on which rows of the design fall in the first part.
  ord <- order(working_data[[order_by]])
  e <- residuals[ord]
  design <- design[ord, , drop = FALSE]
  basis <- qr(design)
  k <- basis$rank

  m <- if (point < 1) floor(point * n) else point
  if (m < k || m > n - k) {
    stop(
      sprintf(
        paste0(
          "`point` puts %d of the %d observations in the first part. Each part needs ",
          "at least %d, the number of coefficients."
        ),
        as.integer(m), n, k
      ),
      call. = FALSE
    )
  }

  # A perfect fit is refused by rvalidateModelInputs(); this is the last guard
  # against a zero denominator.
  total <- sum(e^2)
  if (!is.finite(total) || total <= 0) {
    .std_error(
      "rassumption_violation",
      assumption = "Harrison-McCabe test requires residuals that are not all zero"
    )
  }

  statistic <- sum(e[seq_len(m)]^2) / total
  tails <- rhmc_tails(statistic, rhmc_spectrum(basis, m))

  p_value <- switch(alternative,
    "less" = tails[["lower"]],
    "greater" = tails[["upper"]],
    "two.sided" = min(1, 2 * min(tails))
  )

  alternative_label <- switch(alternative,
    "less" = sprintf("variance increases with '%s'", order_by),
    "greater" = sprintf("variance decreases with '%s'", order_by),
    "two.sided" = sprintf("variance changes with '%s'", order_by)
  )

  structure(
    list(
      statistic = c(HMC = statistic),
      parameter = c(m = as.numeric(m), n = as.numeric(n)),
      p.value = p_value,
      method = "Harrison-McCabe test for heteroscedasticity",
      data.name = paste(deparse(stats::formula(model)), collapse = " "),
      alternative = alternative_label
    ),
    class = "htest"
  )
}

#' Eigenvalues of the Harrison-McCabe quadratic form
#'
#' With \eqn{M} the residual-maker of the ordered design and \eqn{D} the
#' selector of its first `m` rows, returns the eigenvalues of \eqn{M D M} on
#' the residual space, which has dimension \eqn{n - k}, as distinct values and
#' their multiplicities. If \eqn{Q_1} is an orthonormal basis of the design
#' and \eqn{s_i^2} are the squared singular values of its first `m` rows, the
#' eigenvalues are 1 with multiplicity \eqn{m - k}, the \eqn{k} values
#' \eqn{1 - s_i^2}, and 0 with multiplicity \eqn{n - m - k}.
#'
#' @param basis The QR decomposition of the design, rows in the order of the
#'   test.
#' @param m Number of observations in the first part, with `k <= m <= n - k`.
#' @return A list with `values` and `multiplicity`.
#' @keywords internal
#' @noRd
rhmc_spectrum <- function(basis, m) {
  n <- nrow(basis$qr)
  k <- basis$rank
  q1 <- qr.Q(basis)[seq_len(m), seq_len(k), drop = FALSE]
  s2 <- svd(q1, nu = 0L, nv = 0L)$d^2
  s2 <- pmin(pmax(s2, 0), 1)
  values <- c(1, 1 - s2, 0)
  multiplicity <- c(m - k, rep(1, length(s2)), n - m - k)
  present <- values[multiplicity > 0]
  if (diff(range(present)) <= sqrt(.Machine$double.eps)) {
    .std_error(
      "rassumption_violation",
      assumption = paste0(
        "Harrison-McCabe test needs a statistic that can vary: with this design ",
        "and `point` it takes the same value whatever the errors"
      )
    )
  }
  list(values = values, multiplicity = multiplicity)
}

#' Tail probabilities of the Harrison-McCabe statistic
#'
#' \eqn{P(b \le b_0)} and \eqn{P(b \ge b_0)} under normal errors of constant
#' variance, from Imhof's (1961) formula for \eqn{Q = \sum_j h_j \mu_j \chi^2_1}
#' with \eqn{\mu_j = \lambda_j - b_0}:
#' \eqn{P(Q > 0) = 1/2 + \pi^{-1} \int_0^\infty \sin\theta(u) / (u \rho(u))\,du},
#' \eqn{\theta(u) = \frac12 \sum_j h_j \arctan(\mu_j u)},
#' \eqn{\rho(u) = \prod_j (1 + \mu_j^2 u^2)^{h_j / 4}}.
#' The \eqn{\mu_j} are scaled to unit sum of squares first, which leaves the
#' probability unchanged, and the integral is taken over \eqn{t = \log u}.
#' When the statistic is far in a tail the \eqn{|\mu_j|} differ by orders of
#' magnitude, and the integrand has features at each scale \eqn{1 / |\mu_j|};
#' in \eqn{t} they all have the same width, and the integrand falls
#' exponentially at both ends. Against the closed form of a design in which
#' the statistic has a beta distribution, the absolute error is below
#' \code{2e-11} from 6 to 100000 observations and from \code{1e-15} to
#' \code{1 - 1e-9}, the largest at 7 observations. Smaller probabilities are
#' not resolved.
#'
#' @param statistic The observed share \eqn{b_0}.
#' @param spectrum The result of `rhmc_spectrum()`.
#' @return Named numeric vector with `lower` and `upper`.
#' @keywords internal
#' @noRd
rhmc_tails <- function(statistic, spectrum) {
  mu <- spectrum$values - statistic
  h <- spectrum$multiplicity
  keep <- h > 0 & mu != 0
  mu <- mu[keep]
  h <- h[keep]
  if (!any(mu < 0)) {
    return(c(lower = 0, upper = 1))
  }
  if (!any(mu > 0)) {
    return(c(lower = 1, upper = 0))
  }
  mu <- mu / sqrt(sum(h * mu^2))
  # sin(theta(u)) / (u rho(u)) du, with u = exp(t) and du = u dt.
  integrand <- function(t) {
    mu_u <- outer(mu, exp(t))
    theta <- 0.5 * colSums(h * atan(mu_u))
    log_rho <- 0.25 * colSums(h * log1p(mu_u^2))
    sin(theta) / exp(log_rho)
  }
  integral <- stats::integrate(
    integrand, -Inf, Inf,
    rel.tol = 1e-10, abs.tol = 1e-13, subdivisions = 2000L
  )$value / pi
  c(lower = min(1, max(0, 0.5 - integral)), upper = min(1, max(0, 0.5 + integral)))
}
