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
#' A weighted `lm` is resampled on the scale where its null hypothesis is
#' constant variance, the regression of \eqn{\sqrt{w_i}\, y_i}{sqrt(w_i) y_i}
#' on \eqn{\sqrt{w_i}\, x_i}{sqrt(w_i) x_i}. Null-imposed resampling draws the
#' \eqn{e^*_i}{e*_i} from the Pearson residuals
#' \eqn{\sqrt{w_i}\, e_i}{sqrt(w_i) e_i}, divided by
#' \eqn{\sqrt{1 - h_i}}{sqrt(1 - h_i)} with the leverages of that regression and
#' centred, and divides each draw by \eqn{\sqrt{w_i}}{sqrt(w_i)} before adding
#' it to \eqn{\hat y_i}{yhat_i}.
#' Pairs resampling draws each row with its weight. Either way each replicate
#' is refitted with the weights, so `test_function` sees a weighted fit, as it
#' did on the original data.
#'
#' Weights given to the fit are known, and they are held fixed. On a fit
#' returned by [fitWLS()] they were estimated from the same data, and since
#' 0.15.0 each replicate estimates them again by default
#' (`reestimate_weights`), so that `test_function` sees what `fitWLS()` would
#' have returned for the replicate. The replicate is fitted as the model
#' `fitWLS()` was given, without weights or with the weights that model had.
#' The variance function `fitWLS()` recorded is fitted again to its residuals,
#' with the same form and on the regressors `fitWLS()` evaluated, whose rows
#' follow the rows drawn under pairs resampling, and the replicate is refitted
#' with the weights it gives. A replicate whose variance function cannot be
#' fitted, where `fitWLS()` would fall back to equal weights, fails as a
#' replicate whose refit fails does, and the p-value and the interval are
#' computed from the replicates that remain; a warning gives the number that
#' failed. In
#' `inst/validation/weighted-fits-size.R`, with the variance function
#' correctly specified, normal errors, 150 observations, `B = 499` and
#' [performKoenkerTest()] as `test_function`, the rejection rate at the 5%
#' level was 0.083 with the weights held fixed, as every call did before
#' 0.15.0 and `reestimate_weights = FALSE` still does, 0.045 with them
#' estimated again, and 0.045 with the true weights. See the section on
#' weighted fits in [performWildBootstrapTest()].
#'
#' Weighted or not, each replicate is refitted with an offset the model was
#' given as an argument, `lm(..., offset = o)`; an offset written in the
#' formula, `offset(o)`, is evaluated again from the data of the replicate.
#' The rows of `data` are matched to those of the fit by row name, so that
#' each row is regenerated, or drawn, with its own fitted value, weight and
#' offset. Rows the model was not fitted to, such as those it dropped for
#' missing values, are left out, for the observed statistic as for the
#' replicates; the rows that stay keep the order of `data`. `data` that lacks
#' a row the model was fitted to is refused. Data without row names of its
#' own, such as a tibble or a data.table, that has as many rows as the fit is
#' taken in the order of the fit. The model is then refitted to the rows of
#' `data`, and `data` is refused when that refit does not give the residuals
#' of the fit to a relative `1e-8`: rows in another order taken by position,
#' values that differ from those the model was fitted to, or a variable the
#' formula finds outside `data`, which keeps the order of the fit. Pairs
#' resampling of a model with such a variable, for example `offset(o)` with
#' `o` in the environment of the formula, is refused too; give such an offset
#' as an argument instead.
#'
#' Null-imposed resampling additionally requires the response to be a plain
#' variable: for `log(y) ~ x` the fitted values are on the log scale while the
#' data column holds `y`, so regenerating one from the other and refitting
#' would take the logarithm twice. That case raises an error;
#' `resample = "pairs"` resamples rows and is unaffected by it.
#'
#' @section Parallel replicates:
#' With `parallel = TRUE` replicate `i` draws its random numbers from stream
#' `i` of a L'Ecuyer-CMRG generator ([parallel::nextRNGStream()]), seeded by
#' one number drawn from the session's generator. A call made after
#' `set.seed()` therefore returns the same result each time, whatever
#' `n_cores` and whether the workers are forked ([parallel::mclapply()]) or
#' run on a socket cluster ([parallel::makeCluster()], where the platform
#' cannot fork). It does not return the result of the same call with
#' `parallel = FALSE`, which draws every replicate from the session's
#' generator, as it did before 0.15.0: the two are bootstraps of the same data
#' with different draws, and each is valid. Before 0.15.0 a seeded call with
#' `parallel = TRUE` gave a different result each time.
#'
#' @param test_function A function that accepts `(model, data, ...)` and
#'   returns an object with a numeric `statistic` element (typically an
#'   \code{htest} result).
#' @param model A fitted model of class `lm`, with or without weights. A `glm`
#'   is rejected: each replicate is refitted with least squares, which would
#'   discard its family and link.
#' @param data Data frame used when fitting `model`, holding the rows it was
#'   fitted to under their row names. Other rows are left out. See Details.
#' @param B Integer, number of bootstrap replications. Defaults to 999. The
#'   p-value is `(1 + #) / (B_eff + 1)`, so a test at level `alpha` is exact
#'   only when `alpha * (B + 1)` is an integer: with 999 it is at the 5% and
#'   1% levels, and with 1000, the default before 0.15.0, at neither.
#' @param parallel Logical, compute replicates in parallel when possible? See
#'   *Parallel replicates*.
#' @param ci_level Confidence level for the percentile interval.
#' @param resample Bootstrap strategy. `"null"` (the default) regenerates the
#'   response under homoscedasticity and yields a testable `p_value`;
#'   `"pairs"` samples rows of `data` with replacement and yields replicates
#'   and an interval but no p-value. See Details.
#' @param n_cores Optional integer specifying the number of worker processes to
#'   use when `parallel = TRUE`. Defaults to `parallel::detectCores() - 1`.
#'   The result does not depend on it.
#' @param progress Logical flag controlling whether a textual progress bar is
#'   displayed during bootstrap replication.
#' @param ... Additional arguments forwarded to `test_function`.
#' @param reestimate_weights Whether each replicate estimates the weights of a
#'   [fitWLS()] fit again from the variance function `fitWLS()` recorded on it.
#'   `NULL`, the default, does so for a `fitWLS()` fit and holds the weights
#'   fixed for any other model. `FALSE` holds them fixed, as every call did
#'   before 0.15.0. `TRUE` is an error for a model that `fitWLS()` did not
#'   return, or on which it fell back to equal weights. A variance regressor
#'   computed from the response, which each replicate regenerates, cannot be
#'   estimated again: `NULL` then holds the weights fixed, with a warning, and
#'   `TRUE` is an error. It comes after `...`, so it has to be named in full.
#'   See Details.
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
#'   boot <- bootstrapTestStatistic(performWhiteTest, model, mtcars, B = 100)
#'   boot$ci
#' }
#' @export
bootstrapTestStatistic <- function(test_function, model, data, B = 999,
                                   parallel = FALSE, ci_level = 0.95,
                                   resample = c("null", "pairs"), n_cores = NULL,
                                   progress = interactive(), ...,
                                   reestimate_weights = NULL) {
  .bootstrap_test_statistic(
    test_function, model, data,
    B = B, parallel = parallel, ci_level = ci_level, resample = resample,
    n_cores = n_cores, progress = progress, ...,
    reestimate_weights = reestimate_weights, .call = match.call()
  )
}

#' @rdname bootstrapTestStatistic
#' @section Deprecated name:
#' `rbootstrap_test_statistic()` is deprecated since 0.14.0: use
#' `bootstrapTestStatistic()`, the same function under the package's naming
#' scheme. The old name still works, returns what it returned and warns.
#' @export
rbootstrap_test_statistic <- function(test_function, model, data, B = 999,
                                      parallel = FALSE, ci_level = 0.95,
                                      resample = c("null", "pairs"), n_cores = NULL,
                                      progress = interactive(), ...,
                                      reestimate_weights = NULL) {
  .Deprecated("bootstrapTestStatistic", old = "rbootstrap_test_statistic")
  .bootstrap_test_statistic(
    test_function, model, data,
    B = B, parallel = parallel, ci_level = ci_level, resample = resample,
    n_cores = n_cores, progress = progress, ...,
    reestimate_weights = reestimate_weights, .call = match.call()
  )
}

# The message of the warning an r-prefixed helper that leaves the exports gives.
.ht_r_prefixed_deprecation <- function(name) {
  sprintf(
    paste0(
      "'%s' is deprecated: it is an internal helper of heteroTests and ",
      "leaves the exports in 1.0.0.\nSee help(\"Deprecated\")"
    ),
    name
  )
}

# The implementation behind bootstrapTestStatistic() and its deprecated name.
# `.call` is the call each name records in its result, so that each reports
# the call it was given.
.bootstrap_test_statistic <- function(test_function, model, data, B = 999,
                                      parallel = FALSE, ci_level = 0.95,
                                      resample = c("null", "pairs"), n_cores = NULL,
                                      progress = interactive(), ...,
                                      reestimate_weights = NULL, .call = NULL) {
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
      "`bootstrapTestStatistic()` supports Gaussian `lm` models only. Each ",
      "replicate is refitted with least squares, which would discard the ",
      "family and link of a `glm` and bootstrap a different model.",
      call. = FALSE
    )
  }

  # A weighted fit is refitted with its weights, so each replicate is the model
  # that was supplied. NULL for an unweighted fit, which takes the path it
  # always took.
  prior_weights <- rprior_weights(model)

  # The variance function of a fitWLS() fit, estimated again in each
  # replicate, or NULL, which holds the weights fixed as before 0.15.0.
  variance_record <- rwls_reestimation(model, reestimate_weights, "bootstrapTestStatistic")

  # `data` is reduced to the rows the model was fitted to, matched by row
  # name: rows it does not hold, such as those the fit dropped for missing
  # values, are left out, and data that lacks a fitted row is refused. Pairs
  # resampling used to draw the rows the fit had dropped as well, so that each
  # replicate was refitted to a different number of rows. The rows that stay
  # keep the order of `data`, so data that holds exactly the fitted rows is
  # used as it is. row_index gives, for each row kept, the row of the fit it
  # holds; it is found before the rows are left out, because a tibble numbers
  # the rows of a subset afresh.
  #
  # Automatic row names, those of a tibble, a data.table or a data frame whose
  # names were reset, number the rows 1 to n whatever they hold. When such data
  # has as many rows as the fit, they are taken in the order of the fit, as
  # the complete cases of the data would be; the check of the residuals below
  # decides whether they are its rows.
  fit_rows <- names(model$residuals)
  automatic <- .row_names_info(data) < 0L && nrow(data) == length(model$residuals)
  if (is.null(fit_rows) || automatic) {
    row_index <- seq_len(nrow(data))
  } else {
    found <- match(rownames(data), fit_rows)
    missing_rows <- fit_rows[!seq_along(fit_rows) %in% found]
    if (length(missing_rows) > 0L) {
      stop(
        "Resampling needs `data` to hold the rows the model was fitted to, ",
        "under their row names; it lacks ", length(missing_rows), " of them (",
        paste(utils::head(missing_rows, 3L), collapse = ", "),
        if (length(missing_rows) > 3L) ", ..." else "", "). Refit on `data`, ",
        "or pass the rows of the fit.",
        call. = FALSE
      )
    }
    keep <- !is.na(found)
    if (!all(keep)) {
      data <- data[keep, , drop = FALSE]
    }
    row_index <- found[keep]
  }
  # The tests match `data` to the residuals of the model by row name. Rows
  # taken by position, or a subset that a tibble or a data.table has numbered
  # afresh, no longer carry the names of the rows they hold, so they are given
  # them, in a data frame. Data whose rows already carry them is left as it is.
  if (!is.null(fit_rows) && !identical(rownames(data), fit_rows[row_index])) {
    data <- as.data.frame(data)
    rownames(data) <- fit_rows[row_index]
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
  #
  # On a weighted fit the construction is carried out in the regression of
  # sqrt(w) y on sqrt(w) X, where the errors have constant variance under the
  # null hypothesis: its residuals are the Pearson residuals, and hatvalues()
  # of a weighted fit are its leverages. Each draw is returned to the scale of
  # the observation it is added to by dividing by sqrt(w), and the refit
  # carries the weights. Weighted or not, the refit carries an offset the
  # model was given as an argument, which the formula does not hold.
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
    null_residuals <- rpearson_residuals(model) /
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
  # The fitted values, the weights of a weighted fit and an offset given as an
  # argument have one value per fitted row. They are put in the row order of
  # `data`, matched by row name, so that each row is regenerated or drawn with
  # its own. Data in the order of the fit takes them as they are, so the same
  # seed gives the same result as before. Until the unweighted fit followed
  # this rule it took the fitted values by position: data holding the same
  # rows in another order added each row's draw to another row's fitted value,
  # and a refit dropped an offset given as an argument.
  #
  # `data` now holds the fitted rows once each, since the row names of a data
  # frame are unique, so row_index is a permutation. Data taken by position
  # needs as many rows as the fit.
  if (length(model$residuals) != n || anyNA(row_index)) {
    stop(
      "Resampling needs `data` to hold the rows the model was fitted to, once ",
      "each. Refit on `data`, or pass the rows of the fit.",
      call. = FALSE
    )
  }
  # Row names say which row is which only when `data` has names of its own,
  # and a variable the formula finds outside `data`, such as offset(o) with o
  # in the environment of the formula, keeps the order of the fit whatever the
  # order of `data`. Both are caught by refitting the model to the rows of
  # `data` and comparing its residuals with those of the fit. Pairs resampling
  # draws rows of `data`, so every variable has to follow them: the rows taken
  # in a fixed other order must give the residuals of the fit in that order.
  if (!rdata_reproduces_fit(model, data, row_index)) {
    stop(
      "`data` does not reproduce the fit: refitted to the rows of `data`, the ",
      "model does not give the residuals it was fitted with. Data without row ",
      "names of its own, such as a tibble or a data.table, is taken in the ",
      "order of the fit; values that differ from those of the fit are refused; ",
      "and a variable the formula finds outside `data` keeps the order of the ",
      "fit. Pass the rows of the fit, in its order when they have no row ",
      "names of their own, or add the variable to `data`.",
      call. = FALSE
    )
  }
  if (identical(resample, "pairs")) {
    # Even positions, then odd ones: fixed, so it draws no random number.
    shuffled <- c(seq(2L, n, by = 2L), seq(1L, n, by = 2L))
    if (!rdata_reproduces_fit(model, data[shuffled, , drop = FALSE], row_index[shuffled])) {
      stop(
        "Pairs resampling draws rows of `data`, but the model uses a variable ",
        "that does not come from `data` and would not follow them, such as an ",
        "offset in the environment of the formula. Add the variable to `data`, ",
        "or give an offset as an argument.",
        call. = FALSE
      )
    }
  }
  prior_weights <- prior_weights[row_index]
  fitted_vals <- fitted_vals[row_index]
  offset_argument <- roffset_argument(model)[row_index]
  # On a fitWLS() fit whose weights are estimated again, each replicate is
  # refitted as fitWLS() fitted the model: first as the model fitWLS() was
  # given, without weights or with the weights it had; then its variance
  # function is fitted again, on the regressors fitWLS() recorded, and it is
  # refitted with the weights that gives. The regressors and those weights
  # belong to rows, and are put in the row order of `data` as the rest is.
  if (!is.null(variance_record)) {
    refit_regressors <- variance_record$regressors[row_index, , drop = FALSE]
    refit_weights <- variance_record$initial_weights[row_index]
  }

  compute_bootstrap <- function(index) {
    boot_weights <- prior_weights
    boot_offset <- offset_argument
    if (!is.null(variance_record)) {
      boot_weights <- refit_weights
      boot_regressors <- refit_regressors
    }
    if (identical(resample, "null")) {
      boot_data <- data
      boot_errors <- sample(null_residuals, n, replace = TRUE)
      if (!is.null(prior_weights)) {
        boot_errors <- boot_errors / sqrt(prior_weights)
      }
      boot_data[[response_col]] <- fitted_vals + boot_errors
    } else {
      sampled_idx <- sample.int(n, replace = TRUE)
      boot_data <- data[sampled_idx, , drop = FALSE]
      boot_weights <- boot_weights[sampled_idx]
      boot_offset <- offset_argument[sampled_idx]
      if (!is.null(variance_record)) {
        boot_regressors <- boot_regressors[sampled_idx, , drop = FALSE]
      }
    }
    boot_model <- tryCatch(
      if (is.null(boot_weights) && is.null(boot_offset)) {
        safe_lm(formula, data = boot_data)
      } else {
        rrefit_lm(formula, boot_data, boot_weights, boot_offset)
      },
      error = function(e) NULL
    )
    # A replicate whose variance function cannot be fitted again, where
    # fitWLS() would fall back to equal weights, fails as a replicate whose
    # refit fails does, and the p-value counts the replicates that remain.
    if (!is.null(boot_model) && !is.null(variance_record)) {
      boot_model <- tryCatch(
        rwls_refit_replicate(boot_model, variance_record, boot_regressors),
        error = function(e) NULL
      )
    }
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

    # Replicate i draws from stream i of a L'Ecuyer-CMRG generator seeded by
    # one draw from the session's, so a seeded call gives the same replicates
    # each time, with any number of workers, forked or not. Before 0.15.0
    # each worker drew from a generator set.seed() did not reach.
    streams <- rtask_streams(B)
    cl <- NULL
    if (.Platform$OS.type != "unix") {
      cl <- parallel::makeCluster(worker_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      parallel::clusterEvalQ(cl, library(heteroTests))
    }
    for (chunk in chunk_plan) {
      chunk_results <- rparallel_tasks(chunk, compute_bootstrap, streams, worker_cores, cl)
      replicates[chunk] <- as.numeric(unlist(chunk_results, use.names = FALSE))
      processed <- processed + length(chunk)
      progress_bar$update(processed)
    }
  } else {
    for (i in replicate_indices) {
      replicates[i] <- compute_bootstrap(i)
      progress_bar$update(i)
    }
  }

  effective <- sum(is.finite(replicates))
  if (!is.null(variance_record)) {
    rwarn_failed_replicates("bootstrapTestStatistic", B - effective, B)
  }
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
    call = if (is.null(.call)) match.call() else .call
  )
}

#' Whether the rows of `data` reproduce a fit
#'
#' Refits the model, by least squares on the design rebuilt from `data`, with
#' the weights and an offset given as an argument taken from the fit in the
#' order `row_index` gives, and compares the residuals with those of the fit
#' in that order. The residuals do not depend on how the design is
#' parametrised, so a term computed from all the data, such as
#' `I(x - mean(x))`, does not count as a difference when `data` holds only
#' some of its rows; values that do not belong to the rows they are matched
#' to, or that differ from those of the fit, do.
#'
#' @param model A fitted `lm`.
#' @param data Data frame holding the fitted rows.
#' @param row_index For each row of `data`, the row of the fit it holds.
#' @return `TRUE` when the residuals agree to a relative `1e-8`, or when the
#'   model frame cannot be rebuilt from `data`, in which case the refits fail
#'   on their own; `FALSE` otherwise.
#' @keywords internal
#' @noRd
rdata_reproduces_fit <- function(model, data, row_index) {
  model_terms <- stats::terms(model)
  rebuilt <- tryCatch(
    stats::model.frame(model_terms, data = data, na.action = stats::na.pass,
                       xlev = model$xlevels),
    error = function(e) NULL
  )
  if (is.null(rebuilt)) {
    return(TRUE)
  }
  if (nrow(rebuilt) != length(row_index)) {
    return(FALSE)
  }
  y <- stats::model.response(rebuilt, "numeric")
  X <- tryCatch(
    stats::model.matrix(model_terms, rebuilt, contrasts.arg = model$contrasts),
    error = function(e) NULL
  )
  if (is.null(y) || is.null(X)) {
    return(TRUE)
  }
  offset <- stats::model.offset(rebuilt)
  argument <- roffset_argument(model)
  if (!is.null(argument)) {
    offset <- (if (is.null(offset)) 0 else offset) + argument[row_index]
  }
  weights <- rprior_weights(model)
  if (anyNA(y) || anyNA(X) || anyNA(offset)) {
    return(FALSE)
  }
  refit <- tryCatch(
    if (is.null(weights)) {
      stats::lm.fit(X, y, offset = offset)
    } else {
      stats::lm.wfit(X, y, weights[row_index], offset = offset)
    },
    error = function(e) NULL
  )
  if (is.null(refit)) {
    return(FALSE)
  }
  expected <- unname(model$residuals[row_index])
  scale <- max(abs(y - mean(y)), .Machine$double.eps)
  max(abs(unname(refit$residuals) - expected)) <= 1e-8 * scale
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
#' @section Deprecated:
#' Deprecated since 0.14.0: an internal helper of the package, which leaves the
#' exports in 1.0.0. It still works and warns.
#' @export
restimate_test_power <- function(test_result, model, data, alpha = 0.05,
                                 target_power = 0.8, max_multiplier = 5) {
  .Deprecated(
    old = "restimate_test_power",
    msg = .ht_r_prefixed_deprecation("restimate_test_power")
  )
  .estimate_test_power(
    test_result, model, data, alpha, target_power, max_multiplier,
    .call = match.call()
  )
}

# The implementation behind restimate_test_power(). The call it records keeps
# the exported name, as it did before 0.14.0.
.estimate_test_power <- function(test_result, model, data, alpha = 0.05,
                                 target_power = 0.8, max_multiplier = 5, .call = NULL) {
  if (is.null(.call)) {
    .call <- match.call()
    .call[[1L]] <- as.name("restimate_test_power")
  }
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
  effect <- .calculate_effect_size(test_result, model, data, type = "cramers_v")
  effect_size <- effect$effect_size

  if (!is.finite(effect_size) || effect_size <= 0) {
    return(list(
      power = NA_real_,
      recommended_n = NA_integer_,
      details = list(
        message = "Effect size not available or non-positive; cannot estimate power.",
        df = df,
        sample_size = n,
        call = .call
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
      call = .call
    )
  )
}
