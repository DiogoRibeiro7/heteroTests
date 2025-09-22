#' Bootstrap utilities for heteroscedasticity diagnostics
#'
#' These helpers provide reusable bootstrap and power-estimation routines
#' for the heteroscedasticity testing framework. They support the robust
#' implementations introduced in \code{robust_implementations.R} but are
#' written generically so additional tests can reuse them.
#'
#' @name bootstrap_methods
NULL

#' Bootstrap a test statistic
#'
#' Provides a consistent wrapper around bootstrap resampling for test
#' statistics produced by heteroscedasticity diagnostics. The routine
#' refits the supplied model on bootstrap resamples of the working data
#' and evaluates `test_function` on each replicate. Percentile confidence
#' intervals and an empirical p-value are returned alongside the
#' bootstrap distribution.
#'
#' @param test_function A function that accepts `(model, data, ...)` and
#'   returns an object with a numeric `statistic` element (typically an
#'   [stats::htest] result).
#' @param model A fitted model of class `lm` or `glm`.
#' @param data Data frame used when fitting `model`.
#' @param B Integer, number of bootstrap replications. Defaults to 1000.
#' @param parallel Logical, compute replicates in parallel when possible?
#' @param ci_level Confidence level for the percentile interval.
#' @param resample Bootstrap strategy. Currently only "pairs" sampling is
#'   supported where rows of `data` are sampled with replacement.
#' @param ... Additional arguments forwarded to `test_function`.
#'
#' @return A list with components:
#'   \describe{
#'     \item{`original`}{Result returned by `test_function` on the
#'       original data.}
#'     \item{`original_statistic`}{Numeric value of the test statistic from
#'       the original sample.}
#'     \item{`replicates`}{Numeric vector of bootstrap statistics.}
#'     \item{`ci`}{Percentile confidence interval for the statistic.}
#'     \item{`p_value`}{Empirical bootstrap p-value.}
#'     \item{`B`}{Requested number of replications.}
#'     \item{`effective_samples`}{Number of non-missing bootstrap
#'       replications.}
#'     \item{`call`}{Matched call for reproducibility.}
#'   }
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#' if (interactive()) {
#'   set.seed(123)
#'   boot <- rbootstrap_test_statistic(performWhiteTest, model, mtcars, B = 100)
#'   boot$ci
#' }
#' @export
rbootstrap_test_statistic <- function(test_function, model, data, B = 1000,
                                      parallel = FALSE, ci_level = 0.95,
                                      resample = c("pairs"), ...) {
  if (!is.function(test_function)) {
    stop("`test_function` must be a function.", call. = FALSE)
  }

  resample <- match.arg(resample)

  rvalidateModelInputs(model, test_name = "Bootstrap helper", min_obs = 5L)
  rvalidateDataInputs(data, min_obs = 5L)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }

  if (!is.numeric(ci_level) || length(ci_level) != 1L || is.na(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be between 0 and 1.", call. = FALSE)
  }

  original_result <- test_function(model, data, ...)
  statistic <- original_result$statistic
  if (is.null(statistic) || length(statistic) == 0L) {
    stop("`test_function` must return an object with a `statistic` element.", call. = FALSE)
  }
  original_stat <- unname(statistic[[1L]])

  formula <- stats::formula(model)
  n <- nrow(data)
  if (n < 2L) {
    stop("Bootstrap requires at least two observations.", call. = FALSE)
  }

  compute_bootstrap <- function(index) {
    sampled_idx <- sample.int(n, replace = TRUE)
    boot_data <- data[sampled_idx, , drop = FALSE]
    boot_model <- tryCatch(
      safe_lm(formula, data = boot_data),
      error = function(e) NULL
    )
    if (is.null(boot_model)) {
      return(NA_real_)
    }
    boot_result <- tryCatch(
      test_function(boot_model, boot_data, ...),
      error = function(e) NA_real_
    )
    if (is.list(boot_result) && !is.null(boot_result$statistic)) {
      return(unname(boot_result$statistic[[1L]]))
    }
    NA_real_
  }

  replicate_indices <- seq_len(B)
  replicates <- rep(NA_real_, B)

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    if (.Platform$OS.type == "unix") {
      replicates <- unlist(parallel::mclapply(replicate_indices, compute_bootstrap,
        mc.preschedule = FALSE
      ), use.names = FALSE)
    } else {
      cl <- parallel::makeCluster(max(1L, parallel::detectCores() - 1L))
      on.exit(parallel::stopCluster(cl), add = TRUE)
      replicates <- unlist(parallel::parLapply(cl, replicate_indices, compute_bootstrap), use.names = FALSE)
    }
  } else {
    replicates <- vapply(replicate_indices, compute_bootstrap, numeric(1L))
  }

  effective <- sum(is.finite(replicates))
  if (effective == 0L) {
    warning("All bootstrap replicates failed; returning NA results.", call. = FALSE)
    ci <- c(lower = NA_real_, upper = NA_real_)
    p_value <- NA_real_
  } else {
    probs <- c((1 - ci_level) / 2, 1 - (1 - ci_level) / 2)
    ci_vals <- stats::quantile(replicates, probs = probs, na.rm = TRUE, names = FALSE)
    ci <- stats::setNames(ci_vals, c("lower", "upper"))
    p_value <- mean(replicates >= original_stat, na.rm = TRUE)
  }

  list(
    original = original_result,
    original_statistic = original_stat,
    replicates = replicates,
    ci = ci,
    ci_level = ci_level,
    p_value = p_value,
    B = B,
    effective_samples = effective,
    call = match.call()
  )
}

#' Estimate test power from observed effect size
#'
#' Uses the observed chi-squared statistic and effect size to approximate
#' the achieved power of a heteroscedasticity test and to suggest an
#' increased sample size required to attain the desired power threshold.
#'
#' @param test_result An object returned by `perform*Test()` functions with
#'   `statistic` and `parameter` components.
#' @param model Fitted model used in the diagnostic.
#' @param data Data frame supplied to the diagnostic.
#' @param alpha Significance level used in the test. Defaults to 0.05.
#' @param target_power Desired minimum power for the recommendation.
#' @param max_multiplier Upper bound multiplier relative to the observed
#'   sample size when searching for the recommended sample size.
#'
#' @return A list with elements `power`, `recommended_n`, and `details`
#'   containing auxiliary information (degrees of freedom, non-centrality
#'   parameter, and a power curve over a grid of effect sizes).
#'
#' @export
restimate_test_power <- function(test_result, model, data, alpha = 0.05,
                                 target_power = 0.8, max_multiplier = 5) {
  rvalidateModelInputs(model, test_name = "Power analysis", min_obs = 5L)
  rvalidateDataInputs(data, min_obs = 5L)

  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) ||
      alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(target_power) || length(target_power) != 1L || is.na(target_power) ||
      target_power <= 0 || target_power >= 1) {
    stop("`target_power` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.numeric(max_multiplier) || length(max_multiplier) != 1L ||
      is.na(max_multiplier) || max_multiplier <= 1) {
    stop("`max_multiplier` must be greater than 1.", call. = FALSE)
  }

  statistic <- test_result$statistic
  if (is.null(statistic) || length(statistic) == 0L) {
    stop("`test_result` must contain a `statistic` element.", call. = FALSE)
  }
  chi_sq <- max(unname(statistic[[1L]]), .Machine$double.eps)

  df <- NA_real_
  if (!is.null(test_result$parameter)) {
    df <- suppressWarnings(as.numeric(test_result$parameter[[1L]]))
  }
  if (is.na(df) || df <= 0) {
    df <- max(1, length(stats::coef(model)) - 1L)
  }

  n <- nrow(data)
  effect <- rcalculateEffectSize(test_result, model, data, type = "cramers_v")
  effect_size <- effect$effect_size

  if (!is.finite(effect_size) || effect_size <= 0) {
    return(list(
      power = NA_real_,
      recommended_n = NA_integer_,
      details = list(
        message = "Effect size not available or non-positive; cannot estimate power.",
        df = df,
        sample_size = n,
        call = match.call()
      )
    ))
  }

  critical <- stats::qchisq(1 - alpha, df)
  lambda_obs <- chi_sq
  observed_power <- 1 - stats::pchisq(critical, df, ncp = lambda_obs)

  power_for_n <- function(n_val) {
    lambda <- max(1e-8, effect_size^2) * max(1, df) * n_val
    1 - stats::pchisq(critical, df, ncp = lambda)
  }

  recommended_n <- n
  achieved_power <- power_for_n(recommended_n)

  if (!is.na(achieved_power) && achieved_power < target_power) {
    upper <- ceiling(n * max_multiplier)
    if (upper <= recommended_n) {
      upper <- recommended_n + 1L
    }
    while (power_for_n(upper) < target_power && upper < n * max_multiplier) {
      upper <- ceiling(upper * 1.2)
    }

    if (power_for_n(upper) < target_power) {
      recommended_n <- NA_integer_
    } else {
      root <- tryCatch(
        uniroot(function(x) power_for_n(x) - target_power,
          lower = recommended_n, upper = upper
        ),
        error = function(e) NULL
      )
      if (is.null(root)) {
        recommended_n <- NA_integer_
      } else {
        recommended_n <- ceiling(root$root)
      }
    }
  }

  curve_effect_sizes <- seq(0.05, max(0.5, effect_size * 1.5), by = 0.05)
  power_curve <- vapply(curve_effect_sizes, function(es) {
    lambda <- max(1e-8, es^2) * max(1, df) * n
    1 - stats::pchisq(critical, df, ncp = lambda)
  }, numeric(1L))

  list(
    power = observed_power,
    recommended_n = recommended_n,
    details = list(
      df = df,
      sample_size = n,
      lambda_observed = lambda_obs,
      effect_size = effect_size,
      power_curve = data.frame(
        effect_size = curve_effect_sizes,
        power = power_curve
      ),
      alpha = alpha,
      target_power = target_power,
      call = match.call()
    )
  )
}
