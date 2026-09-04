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
#' statistics produced by heteroscedasticity diagnostics. The routine refits
#' the supplied model on each bootstrap replicate and evaluates
#' `test_function` on it, returning the bootstrap distribution, a percentile
#' interval, and---for null-imposed resampling---a p-value.
#'
#' @details
#' The `resample` argument decides what the replicates mean.
#'
#' With `resample = "null"` (the default) the response is regenerated as
#' \eqn{\hat y_i + e^*_i}, where the \eqn{e^*_i} are drawn with replacement
#' from the model's residuals after dividing by \eqn{\sqrt{1 - h_i}} and
#' centring. Those data satisfy homoscedasticity by construction, so the
#' replicates estimate the null distribution and `p_value` is a test. The
#' leverage correction matters: OLS residuals have variance
#' \eqn{\sigma^2 (1 - h_i)}, and resampling them unscaled under-disperses the
#' regenerated errors, which inflates the rejection rate.
#'
#' With `resample = "pairs"` rows are sampled with replacement. The replicates
#' then follow the statistic's distribution under whatever variance structure
#' the data actually have, which is useful for describing its variability but
#' is not a null distribution: the replicates are centred on the observed
#' statistic, so comparing the two returns roughly 0.5 whatever the data.
#' Measured rejection under \eqn{\sigma_i = x_i^2} was 0\%. `p_value` is
#' therefore `NA` for pairs resampling, and was removed rather than kept as a
#' number that cannot be interpreted.
#'
#' Null-imposed resampling additionally requires the response to be a plain
#' variable: for `log(y) ~ x` the fitted values are on the log scale while the
#' data column holds `y`, so regenerating one from the other and refitting
#' would take the logarithm twice. That case raises an error;
#' `resample = "pairs"` resamples rows and is unaffected by it.
#'
#' @param test_function A function that accepts `(model, data, ...)` and
#'   returns an object with a numeric `statistic` element (typically an
#'   \code{htest} result).
#' @param model A fitted model of class `lm`. A `glm` is rejected: each
#'   replicate is refitted with least squares, which would discard its
#'   family and link.
#' @param data Data frame used when fitting `model`.
#' @param B Integer, number of bootstrap replications. Defaults to 1000.
#' @param parallel Logical, compute replicates in parallel when possible?
#' @param ci_level Confidence level for the percentile interval.
#' @param resample Bootstrap strategy. `"null"` (the default) regenerates the
#'   response under homoscedasticity and yields a testable `p_value`;
#'   `"pairs"` samples rows of `data` with replacement and yields replicates
#'   and an interval but no p-value. See Details.
#' @param n_cores Optional integer specifying the number of worker processes to
#'   use when `parallel = TRUE`. Defaults to `parallel::detectCores() - 1`.
#' @param progress Logical flag controlling whether a textual progress bar is
#'   displayed during bootstrap replication.
#' @param ... Additional arguments forwarded to `test_function`.
#'
#' @return A list with components:
#'   \describe{
#'     \item{`original`}{Result returned by `test_function` on the
#'       original data.}
#'     \item{`original_statistic`}{Numeric value of the test statistic from
#'       the original sample.}
#'     \item{`replicates`}{Numeric vector of bootstrap statistics.}
#'     \item{`ci`}{Percentile interval of the bootstrap distribution of the
#'       statistic, at `ci_level`. This summarises where the resampled
#'       statistic falls; it is not a confidence interval for a parameter and
#'       carries no coverage guarantee.}
#'     \item{`p_value`}{Empirical p-value from the null-imposed replicates,
#'       computed as \eqn{(1 + \#\{T^*_b \ge T\}) / (B_{\mathrm{eff}} + 1)};
#'       `NA` when `resample = "pairs"`.}
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
                                      resample = c("null", "pairs"), n_cores = NULL,
                                      progress = interactive(), ...) {
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

  if (!is.null(n_cores)) {
    if (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) || n_cores < 1) {
      stop("`n_cores` must be NULL or a positive integer.", call. = FALSE)
    }
    n_cores <- as.integer(n_cores)
  }

  # Both strategies refit each replicate with safe_lm(), which builds an lm and
  # drops a glm's family and link: on a Poisson fit of counts the coefficients
  # move from (0.457, 0.406) on the log link to (-0.931, 2.281) on the identity
  # scale, so the replicates describe a different model from the one supplied.
  # Neither strategy is usable for a glm, so both refuse rather than return
  # replicates of something the caller did not ask about.
  if (inherits(model, "glm")) {
    stop(
      "`rbootstrap_test_statistic()` supports Gaussian `lm` models only. Each ",
      "replicate is refitted with least squares, which would discard the ",
      "family and link of a `glm` and bootstrap a different model.",
      call. = FALSE
    )
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

  extra_args <- list(...)

  # Null-imposed resampling. Regenerating the response as
  # yhat + iid draws from the residuals gives data that satisfy
  # homoscedasticity by construction, so the replicates estimate the null
  # distribution of the statistic and the p-value below is a test.
  #
  # Residuals are divided by sqrt(1 - h_i) before resampling: OLS residuals
  # have variance sigma^2 (1 - h_i), so resampling them raw under-disperses the
  # regenerated errors and the test rejects too often. This is the construction
  # performWildBootstrapTest() uses, following Davidson and Flachaire (2008).
  response_col <- all.vars(formula)[1L]
  fitted_vals <- stats::fitted(model)
  null_residuals <- NULL
  if (identical(resample, "null")) {
    # Regenerating the response only makes sense where the fitted values and
    # the residuals live on the same scale as the column being overwritten.
    #
    # For a transformed response such as log(y) ~ x, fitted() is on the log
    # scale while the data column holds y, so writing one into the other and
    # refitting would take the logarithm a second time. For a glm, fitted() is
    # on the response scale, the residuals are deviance residuals, and the
    # refit below uses safe_lm(), which would silently drop the family and
    # link. Both cases previously ran and returned a plausible p-value from
    # meaningless replicates.
    if (!is.name(formula[[2L]])) {
      stop(
        "Null-imposed resampling needs a plain response variable, but the ",
        "model's response is `", deparse(formula[[2L]]), "`, which is on a ",
        "different scale from the data column. Fit the model on a ",
        "pre-transformed column, or use resample = \"pairs\".",
        call. = FALSE
      )
    }
    leverage <- stats::hatvalues(model)
    null_residuals <- stats::residuals(model) /
      sqrt(pmax(1 - leverage, .Machine$double.eps))
    null_residuals <- null_residuals - mean(null_residuals)
    if (length(fitted_vals) != n || length(null_residuals) != n) {
      stop(
        "Null-imposed resampling needs one fitted value and one residual per ",
        "row of `data`; the model was fitted on a different sample. Refit on ",
        "`data`, or pass resample = \"pairs\".",
        call. = FALSE
      )
    }
  }

  compute_bootstrap <- function(index) {
    if (identical(resample, "null")) {
      boot_data <- data
      boot_data[[response_col]] <- fitted_vals +
        sample(null_residuals, n, replace = TRUE)
    } else {
      sampled_idx <- sample.int(n, replace = TRUE)
      boot_data <- data[sampled_idx, , drop = FALSE]
    }
    boot_model <- tryCatch(
      safe_lm(formula, data = boot_data),
      error = function(e) NULL
    )
    if (is.null(boot_model)) {
      return(NA_real_)
    }
    boot_result <- tryCatch(
      do.call(test_function, c(list(model = boot_model, data = boot_data), extra_args)),
      error = function(e) NA_real_
    )
    if (is.list(boot_result) && !is.null(boot_result$statistic)) {
      return(unname(boot_result$statistic[[1L]]))
    }
    NA_real_
  }

  replicate_indices <- seq_len(B)
  replicates <- rep(NA_real_, B)

  progress_bar <- chunk_progress_bar(B, progress && B > 1L, "Bootstrapping diagnostic")
  on.exit(progress_bar$close(), add = TRUE)

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    available_cores <- tryCatch(parallel::detectCores(), error = function(e) 1L)
    worker_cores <- if (is.null(n_cores)) {
      max(1L, available_cores - 1L)
    } else {
      max(1L, min(n_cores, available_cores))
    }

    chunk_plan <- chunk_indices(B, max(10L, worker_cores * 5L))
    processed <- 0L

    if (.Platform$OS.type == "unix") {
      for (chunk in chunk_plan) {
        chunk_results <- parallel::mclapply(chunk, compute_bootstrap,
          mc.cores = worker_cores,
          mc.preschedule = FALSE
        )
        replicates[chunk] <- as.numeric(chunk_results)
        processed <- processed + length(chunk)
        progress_bar$update(processed)
      }
    } else {
      cl <- parallel::makeCluster(worker_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, library(heteroTests))
      parallel::clusterExport(cl,
        varlist = c("compute_bootstrap", "data", "formula", "n", "extra_args", "test_function",
                    "resample", "response_col", "fitted_vals", "null_residuals"),
        envir = environment()
      )
      for (chunk in chunk_plan) {
        chunk_results <- parallel::parLapply(cl, as.list(chunk), compute_bootstrap)
        replicates[chunk] <- unlist(chunk_results, use.names = FALSE)
        processed <- processed + length(chunk)
        progress_bar$update(processed)
      }
    }
  } else {
    for (i in replicate_indices) {
      replicates[i] <- compute_bootstrap(i)
      progress_bar$update(i)
    }
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
    if (identical(resample, "null")) {
      # Finite-simulation convention: (1 + #) / (B_eff + 1), where B_eff counts
      # the replicates that actually converged.
      p_value <- (1 + sum(replicates >= original_stat, na.rm = TRUE)) / (effective + 1)
    } else {
      # Pairs resampling regenerates the data as observed, so the replicates
      # follow the statistic's distribution under whatever variance structure
      # the data actually have -- centred on the observed statistic rather than
      # on the null. Comparing the two returns roughly 0.5 whatever the data:
      # measured rejection under sd = x^2 was 0%. The replicates and the
      # interval still describe the statistic's variability, so they are
      # returned; the p-value is not.
      p_value <- NA_real_
    }
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
