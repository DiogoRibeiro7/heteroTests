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
#' Weighted or not, each replicate is refitted by least squares on the design
#' stored with the fit, in the rows of the replicate, with the weights and the
#' total offset of those rows, the `offset` argument and the `offset()` terms.
#' Nothing is evaluated again from the formula or from `data`: a term computed
#' from the rows it is given, such as `cut(x, quantile(x))`, the knots of
#' `splines::ns()` or `I(x > median(x))`, keeps in every replicate the values
#' the fit computed, and a variable the formula found outside `data`, such as
#' `offset(o)` with `o` in its environment, follows the rows drawn. Under pairs
#' resampling, a level of a factor or a value of a character regressor that no
#' row drawn holds is left out of the replicate, as a refit of the formula
#' leaves it out, and the contrasts of the fit are kept, but for a contrasts
#' matrix of such a factor, which gives way to the default contrasts.
#' `test_function` is given the replicate as an `lm`, whose model frame is that
#' of the fit in the rows of the replicate with the response of the
#' replicate, and the rows of `data`, regenerated or drawn. Up to 0.16.0 each
#' replicate was refitted from the formula on the rows of `data`, and a fit
#' whose model frame could not be rebuilt from them so was refused ("`data`
#' does not reproduce the fit"), as with `cut(x, quantile(x))`, a term
#' computed from all the rows on a fit that dropped some, or a model that
#' transforms a variable given its model frame as `data`; so was pairs
#' resampling of a model with a variable outside `data`. The random numbers
#' are drawn as before, in the same order, so a call made after `set.seed()`
#' returns what it returned wherever the stored design is the design the
#' formula rebuilt.
#'
#' The rows of `data` are matched to those of the fit by row name, so that
#' each row is regenerated, or drawn, with its own fitted value, weight and
#' offset. Rows the model was not fitted to, such as those it dropped for
#' missing values, are left out, for the observed statistic as for the
#' replicates; the rows that stay keep the order of `data`. `data` that lacks
#' a row the model was fitted to is refused. Data without row names of its
#' own, such as a tibble or a data.table, that has as many rows as the fit is
#' taken in the order of the fit. The rows are then matched to the
#' observations of the fit as the tests match them (see [test_results]), and
#' data whose rows cannot be shown to hold them are refused, as are values
#' that differ from those the model was fitted to. A model whose least-squares
#' refit of its design does not give its residuals, such as an `lm` fitted with
#' a smaller `tol` that keeps a column `lm()` would alias, is refused too
#' ("`data` does not reproduce the fit"): its replicates would be those of
#' another model. Since 0.17.0 a `MASS::rlm()` fit, an M-estimate, is refused
#' first, as by every test (see *Supported models*); it was refused here
#' before, with that message.
#'
#' Null-imposed resampling additionally requires the response to be a plain
#' variable: for `log(y) ~ x` the fitted values are on the log scale while the
#' data column holds `y`, so regenerating one from the other and refitting
#' would take the logarithm twice. That case raises an error;
#' `resample = "pairs"` resamples rows and is unaffected by it.
#'
#' The warning White's, Koenker's and the Breusch-Pagan tests give since 0.16.0
#' on a factor with more levels than observations per level is about their
#' asymptotic p-value (see *Fixed effects for small groups* in
#' [performKoenkerTest()]). The observed statistic and the replicates are
#' computed here without it, so that [performWhiteTestRobust()] and
#' [performBPTestRobust()] with `bootstrap = TRUE` give it once, for the
#' asymptotic p-value they report. The result's `original`, the test's own
#' result, carries that asymptotic p-value: on such a design it is
#' unreliable, and the bootstrap's, `p_value`, is the one to use.
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
#' @param model A fitted model of class `lm`, with or without weights, or,
#'   since 0.17.0, a `glm` with the Gaussian family and the identity link, the
#'   least-squares fit of `lm()` with its prior weights, whose replicates are
#'   those of that `lm`. Another `glm` is rejected: each replicate is refitted
#'   with least squares, which would discard its family and link. So is an
#'   `mgcv::gam()` fit, a `glm` by class.
#' @param data Data frame used when fitting `model`, holding the rows it was
#'   fitted to, or its model frame. Other rows are left out. See Details.
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
#'   return, or on which it fell back to equal weights. Each replicate
#'   regenerates the response and holds the design fixed, so a variance
#'   regressor that uses a variable of the response which the right-hand side
#'   of the model does not use, such as `y` or `x` for `I(y / x) ~ z`, or that
#'   is the response term itself, as `~ .` gives, cannot be estimated again:
#'   `NULL` then holds the weights fixed, with a warning, and `TRUE` is an
#'   error. Giving the response as a column of the data, as in `r ~ z`, says
#'   which variables stay. It comes after `...`, so it has to be named in
#'   full. See Details.
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
#' @section Supported models:
#' It accepts an `lm`, unweighted or with positive weights, a `fitWLS()` fit,
#' since 0.16.0 a rank-deficient `lm`, and since 0.17.0 a `glm` with the
#' Gaussian family and the identity link, weighted or not, bootstrapped as the
#' `lm` it is; a replicate whose refit aliases a coefficient the fit estimated,
#' as a pairs resample of an interaction can, fails. It refuses a `glm` of
#' another family or link, an `mgcv::gam()` fit and a `survey::svyglm()` fit,
#' as each replicate is refitted by least squares, a fit with a zero weight,
#' a `MASS::rlm()` fit, an M-estimate, with the message of every test since
#' 0.17.0 (it was refused before as a fit the least-squares refit does not
#' reproduce), a fit whose coefficients, residuals or weights were
#' changed after it was fitted (since 0.17.0), a `fitWLS()` fit of an
#' `mgcv::gam()` fit with smooth terms, whose terms do not rebuild the design of
#' its coefficients (since 0.17.0), and `data` whose rows cannot be
#' shown to hold the observations of the fit. Which model each test accepts,
#' and why, is set out in [test_results], under *Supported models*.
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
  fn <- "bootstrapTestStatistic"

  # A multivariate lm is refused with the message of the model context (M7),
  # as by the tests built on it, before the checks of the model, which
  # stopped on it with a coercion error. Anything else that is not a list,
  # such as a character string, is left to those checks, which name its class.
  if (inherits(model, "mlm") || (is.list(model) && is.matrix(model$residuals))) {
    rstop_model_context(sprintf("%s() needs a model with one response; `model` has %d.", fn,
                                NCOL(model$residuals)))
  }
  # A MASS::rlm() fit is refused by the model context too (rrefuse_rlm(),
  # increment 11): here first, before `test_function` is called on it, so that
  # the refusal names this function whatever `test_function` is. Before, the
  # observed statistic was computed and the fit was refused afterwards, its
  # least-squares refit not reproducing the M-estimate ("`data` does not
  # reproduce the fit").
  rrefuse_rlm(model, fn)
  rvalidateModelInputs(model, test_name = "Bootstrap helper", min_obs = 5L, allow_aliased = TRUE)
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

  # Both strategies refit each replicate by least squares. A glm with the
  # Gaussian family and the identity link is a least-squares fit, the fit of
  # lm(..., weights = w) with its prior weights, so its replicates are those of
  # that lm (N8 of the design of the model context, since increment 8; before,
  # every glm was refused). Any other glm is refused: on a Poisson fit of
  # counts the coefficients move from (0.457, 0.406) on the log link to
  # (-0.931, 2.281) on the identity scale, so the replicates would describe a
  # different model from the one supplied. So is a survey::svyglm() fit, whose
  # weights are not precision weights, and, as before, an mgcv::gam() fit: a
  # smooth term is usually penalised, and a penalised fit is not the
  # least-squares fit of its design, which each replicate fits again
  # (decision 23 of the design); a gam without one is the fit of glm().
  if (inherits(model, "glm")) {
    fam <- tryCatch(stats::family(model), error = function(e) NULL)
    least_squares <- !inherits(model, c("svyglm", "gam")) && !is.null(fam) &&
      identical(fam$family, "gaussian") && identical(fam$link, "identity")
    if (!least_squares) {
      stop(
        "`bootstrapTestStatistic()` supports Gaussian `lm` models only: an `lm`, or a ",
        "`glm` with the gaussian family and the identity link, which is a least-squares ",
        "fit. ",
        if (inherits(model, "gam")) {
          paste0(
            "An mgcv::gam() fit is not taken: a smooth term is usually penalised, and the ",
            "least-squares refit of each replicate is not the fit. Refit a gam without ",
            "smooth terms with glm()."
          )
        } else {
          paste0(
            "Each replicate is refitted with least squares, which would discard the ",
            "family and link of another `glm` and bootstrap a different model."
          )
        },
        call. = FALSE
      )
    }
  }

  # A weighted fit is refitted with its weights, so each replicate is the model
  # that was supplied: the weights of an lm, or the prior weights of a Gaussian
  # glm. NULL for an unweighted fit, which takes the path it always took.
  prior_weights <- rleast_squares_weights(model)

  # The variance function of a fitWLS() fit, estimated again in each
  # replicate, or NULL, which holds the weights fixed as before 0.15.0.
  variance_record <- rwls_reestimation(model, reestimate_weights, fn)

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
  # the complete cases of the data would be; the match of the model context
  # below decides whether they are its rows.
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

  # The warning of White's, Koenker's and the Breusch-Pagan tests on a factor
  # with more levels than observations per level is about their asymptotic
  # p-value (rwarn_small_groups(), R/model_context.R). The p-value here is
  # the bootstrap's, and performWhiteTestRobust() and performBPTestRobust(),
  # which call this function, give the warning of their asymptotic p-value
  # themselves: the observed statistic and the replicates are computed
  # without it, so that it is given once, not B + 2 times.
  original_result <- withCallingHandlers(
    test_function(model, data, ...),
    heteroTests_fixed_effects_warning = function(w) invokeRestart("muffleWarning")
  )
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
  # carries the weights. Weighted or not, the refit carries the offset of the
  # fit, the `offset` argument and the offset() terms.
  response_col <- all.vars(formula)[1L]
  # The fitted values, the leverages and the residuals of the observations of
  # the fit, without the padding of na.exclude (B13): padded, they had more
  # values than `data` has rows of the fit, and the fit was refused.
  fitted_vals <- rmodel_unpad(model, stats::fitted(model))
  null_residuals <- NULL
  if (identical(resample, "null")) {
    # Regenerating the response only makes sense where the fitted values and
    # the residuals live on the same scale as the column being overwritten.
    #
    # For a transformed response such as log(y) ~ x, fitted() is on the log
    # scale while the data column holds y, so the replicate, whose `data` the
    # test given as `test_function` matches to it, would hold a response the
    # fit never had. Before 0.15.0 the case ran, writing the fitted values on
    # the log scale into the column of y, and returned a plausible p-value from
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
    leverage <- rmodel_unpad(model, stats::hatvalues(model))
    null_residuals <- rmodel_unpad(model, rpearson_residuals(model)) /
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
  # The fitted values, the weights of a weighted fit and the offset have one
  # value per fitted row. They are put in the row order of `data`, matched by
  # row name, so that each row is regenerated or drawn with its own. Data in
  # the order of the fit takes them as they are, so the same seed gives the
  # same result as before. Until the unweighted fit followed this rule it took
  # the fitted values by position: data holding the same rows in another order
  # added each row's draw to another row's fitted value, and a refit dropped an
  # offset given as an argument.
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
  # The model context of the fit (R/model_context.R): its model frame, stored
  # design, response, weights and total offset, which the replicates are
  # built from, and `data`, matched to its observations by the guard of the
  # tests, which refuses rows that are not those of the fit. Each replicate is
  # refitted on the stored design, so a term computed from the rows it is
  # given, such as cut(x, quantile(x)), keeps the values of the fit, and a
  # model that transforms a variable, or takes its weights or its offset from a
  # column, is refitted without evaluating its formula again. Before increment
  # 8 of the model context each replicate was refitted from the formula on
  # the rows of `data`, and a fit whose model frame could not be rebuilt from
  # them so, as one with such a term or `data` that is its model frame, was
  # refused, since its replicates would have come from another design.
  ctx <- rmodel_context(model, data, fn = fn, test_label = "Resampling")
  rvalidate_model_context(ctx)
  # The replicates are least-squares refits of the stored design: a fit whose
  # least-squares refit of that design does not give its residuals, as an lm
  # fitted with a smaller `tol` that keeps a column lm() would alias, would
  # have replicates of another model, and is refused, as before. A
  # MASS::rlm() fit, which this refused too, is refused first since increment
  # 11 (rrefuse_rlm()).
  if (!identical(ctx$rows[row_index], seq_len(n)) || !rleast_squares_reproduces(ctx)) {
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
  prior_weights <- prior_weights[row_index]
  fitted_vals <- fitted_vals[row_index]
  # The total offset, which the refit subtracts, and the offset given as an
  # argument, which the call of the replicate records, as the model frame
  # holds it, "(offset)".
  model_offset <- ctx$offset[row_index]
  offset_argument <- if ("(offset)" %in% names(ctx$frame)) {
    as.vector(ctx$frame[["(offset)"]])[row_index]
  }
  # The model frame and the design of the fit in the row order of `data`.
  boot_frame_all <- rframe_rows(ctx$frame, row_index)
  boot_design_all <- rdesign_rows(ctx$design, row_index)
  # The call each replicate records: that of lm() refitting the formula on
  # the data of the replicate, with the weights and the offset argument it was
  # given, as before increment 8.
  replicate_call <- function(weights, offset) {
    call <- as.call(list(
      if (is.null(weights) && is.null(offset)) quote(lm) else quote(stats::lm),
      formula = quote(formula), data = quote(data)
    ))
    if (!is.null(weights)) {
      call$weights <- weights
    }
    if (!is.null(offset)) {
      call$offset <- offset
    }
    call
  }
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

  coefs <- stats::coef(model)
  estimated_coefficients <- names(coefs)[!is.na(coefs)]
  # Local copies travel with compute_bootstrap() to the workers of a socket
  # cluster, which load the installed package.
  aliases_estimated <- rrefit_aliases_estimated
  replicate_lm <- rreplicate_lm
  frame_rows <- rframe_rows
  pairs_design <- rpairs_design
  pairs_data <- rpairs_data
  design_rows <- rdesign_rows
  model_terms <- stats::terms(model)
  model_contrasts <- model$contrasts
  model_xlevels <- model$xlevels

  compute_bootstrap <- function(index) {
    boot_weights <- prior_weights
    boot_offset <- model_offset
    call_offset <- offset_argument
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
      boot_y <- fitted_vals + boot_errors
      boot_data[[response_col]] <- boot_y
      boot_frame <- boot_frame_all
      boot_design <- boot_design_all
    } else {
      sampled_idx <- sample.int(n, replace = TRUE)
      boot_data <- data[sampled_idx, , drop = FALSE]
      boot_weights <- boot_weights[sampled_idx]
      boot_offset <- model_offset[sampled_idx]
      call_offset <- offset_argument[sampled_idx]
      if (!is.null(variance_record)) {
        boot_regressors <- boot_regressors[sampled_idx, , drop = FALSE]
      }
      # The rows drawn of the model frame and of the stored design, under the
      # row names of the rows drawn of `data`, without the levels of a factor
      # or a character column that no row drawn holds and their dummies, as a
      # refit from the formula drops them. The design keeps its attributes,
      # its contrasts among them, which the replicate records. A draw whose
      # design cannot be built fails as a replicate whose refit fails does.
      drawn <- tryCatch(
        pairs_design(frame_rows(boot_frame_all, sampled_idx),
                     design_rows(boot_design_all, sampled_idx),
                     model_terms, model_contrasts, rownames(boot_data), model_xlevels),
        error = function(e) NULL
      )
      if (is.null(drawn)) {
        return(NA_real_)
      }
      boot_frame <- drawn$frame
      boot_design <- drawn$design
      boot_y <- boot_frame[[1L]]
      # The columns of the model frame that `data` does not hold under their
      # own names, such as I(x > median(x)) or (weights), are given with the
      # rows drawn: computed again from the rows drawn of `data`, a term that
      # depends on the rows it is given would not be the column of the
      # replicate, and the test given the replicate would refuse its rows.
      boot_data <- pairs_data(boot_data, boot_frame)
    }
    boot_model <- tryCatch(
      replicate_lm(boot_frame, boot_design, boot_y, boot_weights, boot_offset,
                   model_terms, replicate_call(boot_weights, call_offset)),
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
    # A replicate whose refit aliases a coefficient the fit estimated, as a
    # pairs resample can that leaves a cell of an interaction empty, has lost
    # a column of the design and is not a replicate of the model: it fails.
    # Before 0.16.0 every test of the package refused such a refit, so the
    # replicate failed then too; since then the tests accept a rank-deficient
    # fit, computed on the estimable part of its design.
    if (aliases_estimated(boot_model, estimated_coefficients)) {
      return(NA_real_)
    }
    # Without the warning on small groups, which the call gives once if at all
    # (see original_result above).
    boot_result <- tryCatch(
      withCallingHandlers(
        do.call(test_function, c(list(model = boot_model, data = boot_data), extra_args)),
        heteroTests_fixed_effects_warning = function(w) invokeRestart("muffleWarning")
      ),
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

#' Whether the least-squares refit of the stored design reproduces the fit
#'
#' Each replicate of [bootstrapTestStatistic()] is refitted by least squares
#' on the stored design of the fit, with its weights and its total offset.
#' That is a replicate of the model supplied only when the model is itself the
#' least-squares fit of that design: an `lm`, a Gaussian `glm` with the
#' identity link, a [fitWLS()] fit, an `mgcv::gam()` fit without smooth terms.
#' A `MASS::rlm()` fit, an M-estimate, whose residuals the refit does not
#' give, was refused here until increment 11, which refuses it before
#' ([rrefuse_rlm()]). The refit decides its rank at the tolerance of `lm()`,
#' `1e-7`, as the refits of the formula did before increment 8, so a fit whose rank was
#' decided at a smaller tolerance and differs, an `lm` fitted with a smaller
#' `tol` or a Gaussian `glm` (`1e-11`) with a nearly collinear column, is
#' refused, as such an `lm` was before. Refitting the replicates at the
#' tolerance of the fit would change the values of such an `lm`; it is left to
#' a later step (ROADMAP.md). Before increment 8 of the model context the refit was made from the
#' formula on the rows of `data` (`rdata_reproduces_fit()`), which also
#' checked those rows; the model context matches them now.
#'
#' @param ctx The model context of the fit.
#' @return `TRUE` when the residuals of the refit agree with those of the fit
#'   to a relative `1e-8` of the spread of the response, `FALSE` otherwise.
#' @keywords internal
#' @noRd
rleast_squares_reproduces <- function(ctx) {
  y <- unname(ctx$response)
  refit <- tryCatch(
    if (is.null(ctx$weights)) {
      stats::lm.fit(ctx$design, y, offset = ctx$offset)
    } else {
      stats::lm.wfit(ctx$design, y, ctx$weights, offset = ctx$offset)
    },
    error = function(e) NULL
  )
  if (is.null(refit) || anyNA(y)) {
    return(FALSE)
  }
  expected <- unname(rcontext_unpad(ctx, ctx$residuals))
  scale <- max(abs(y - mean(y)), .Machine$double.eps)
  isTRUE(max(abs(unname(refit$residuals) - expected)) <= 1e-8 * scale)
}

#' Rows of a model frame, with its terms
#'
#' `[.data.frame` keeps the columns of a model frame, matrices among them, and
#' makes the row names of repeated rows unique, as it does for the rows of
#' `data` drawn with them, but drops the `terms` attribute, which is put back.
#'
#' @param frame A model frame.
#' @param rows The rows to keep, in their order.
#' @return The model frame of those rows.
#' @keywords internal
#' @noRd
rframe_rows <- function(frame, rows) {
  model_terms <- attr(frame, "terms")
  frame <- frame[rows, , drop = FALSE]
  attr(frame, "terms") <- model_terms
  frame
}

#' Rows of a design matrix, with its attributes
#'
#' @param design A design matrix, from `model.matrix()`.
#' @param rows The rows to keep, in their order.
#' @return The rows of `design`, with its `assign` and `contrasts`
#'   attributes, which `[` drops and [stats::lm.fit()] reads.
#' @keywords internal
#' @noRd
rdesign_rows <- function(design, rows) {
  out <- design[rows, , drop = FALSE]
  attr(out, "assign") <- attr(design, "assign")
  attr(out, "contrasts") <- attr(design, "contrasts")
  out
}

#' The model frame and the design of a replicate of pairs resampling
#'
#' The rows drawn of the model frame and of the stored design, under the row
#' names of the rows drawn of `data`, as a refit from the formula on those
#' rows names them. A factor of the model frame that has a level no row drawn
#' holds loses that level, as `lm()` drops the levels no row of its data
#' holds (`model.frame(drop.unused.levels = TRUE)`), and with them a contrasts
#' matrix the factor carries; a character column, whose levels `model.matrix()`
#' takes from the values the rows hold, loses its absent values likewise. The
#' design is then built again from the rows drawn of the model frame, whose
#' columns are those of the fit, so that the dummy of that level is left out
#' rather than kept as a column of zeros, which the refit would alias; a
#' contrasts matrix of the fit for such a variable, which has a row for each of
#' its levels, gives way to the default contrasts, as in `lm()`, and a
#' contrasts function, such as `"contr.sum"`, is applied to the levels that
#' remain. The dummy of a cell of an interaction that no row drawn holds is
#' kept, as `lm()` keeps it.
#'
#' @param frame The rows drawn of the model frame of the fit.
#' @param design The rows drawn of the stored design, with its attributes.
#' @param model_terms,contrasts,xlevels The terms, contrasts and levels of the
#'   fit.
#' @param ids The row names of the rows drawn of `data`.
#' @return A list with the model frame, `frame`, and the design, `design`, of
#'   the replicate.
#' @keywords internal
#' @noRd
rpairs_design <- function(frame, design, model_terms, contrasts, ids, xlevels) {
  rownames(frame) <- ids
  rownames(design) <- ids
  dropped <- character()
  for (k in seq_along(frame)) {
    column <- frame[[k]]
    held <- length(unique(column[!is.na(column)]))
    name <- names(frame)[k]
    if (is.factor(column) && held < nlevels(column)) {
      frame[[k]] <- column[, drop = TRUE]
      dropped <- c(dropped, name)
    } else if (is.character(column) && held < length(xlevels[[name]])) {
      dropped <- c(dropped, name)
    }
  }
  if (length(dropped) > 0L) {
    matrices <- vapply(contrasts, is.matrix, logical(1))
    contrasts <- contrasts[!(names(contrasts) %in% dropped & matrices)]
    design <- stats::model.matrix(model_terms, frame,
                                  contrasts.arg = if (length(contrasts) > 0L) contrasts)
  }
  list(frame = frame, design = design)
}

#' The data of a replicate of pairs resampling
#'
#' The rows drawn of `data`, with the columns of the model frame of the
#' replicate that `data` does not hold under their own names, such as
#' `I(x > median(x))`, `log(x)` or `(weights)`, so that a test given the
#' replicate compares those columns as they are rather than computing them
#' again from the rows drawn: a term that depends on the rows it is given
#' would then take other values than in the replicate, which holds those of
#' the fit.
#'
#' @param data The rows drawn of `data`.
#' @param frame The model frame of the replicate, rows named as those of
#'   `data`.
#' @return `data` with those columns, a data frame.
#' @keywords internal
#' @noRd
rpairs_data <- function(data, frame) {
  extra <- setdiff(names(frame), names(data))
  if (length(extra) == 0L) {
    return(data)
  }
  out <- as.data.frame(data)
  rownames(out) <- rownames(frame)
  for (column in extra) {
    out[[column]] <- frame[[column]]
  }
  out
}

#' A least-squares fit of a resampling replicate on the stored design
#'
#' The `lm` that `lm()` returns for the replicate when the design it builds
#' from the formula is `design`: [stats::lm.fit()], or [stats::lm.wfit()] with
#' the weights, on `design` and the response of the replicate, with the
#' offset, and the parts `lm()` adds, the model frame of the replicate among
#' them, so that a test reads the replicate as it reads a fit. Nothing is
#' evaluated again: the design and the model frame are those of the fit, in
#' the rows of the replicate, its response the regenerated one. A Gaussian
#' `glm` gives an `lm` with its prior weights.
#'
#' @param frame The model frame of the replicate: the rows of that of the fit,
#'   with the response of the replicate in its first column.
#' @param design The design of the replicate.
#' @param response The response of the replicate, on the scale of the model.
#' @param weights,offset The weights and the total offset of its rows, or
#'   `NULL`.
#' @param model_terms The terms of the fit.
#' @param call The call to record.
#' @return The fit, of class `lm`.
#' @keywords internal
#' @noRd
rreplicate_lm <- function(frame, design, response, weights, offset, model_terms, call) {
  response <- as.vector(response, "double")
  names(response) <- rownames(frame)
  frame[[1L]] <- response
  fit <- if (is.null(weights)) {
    stats::lm.fit(design, response, offset = offset)
  } else {
    stats::lm.wfit(design, response, weights, offset = offset)
  }
  class(fit) <- "lm"
  fit$offset <- offset
  fit$contrasts <- attr(design, "contrasts")
  fit$xlevels <- stats::.getXlevels(model_terms, frame)
  fit$call <- call
  fit$terms <- model_terms
  fit$model <- frame
  fit
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
  rvalidateModelInputs(model, test_name = "Power analysis", min_obs = 5L, allow_aliased = TRUE)
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
    df <- max(1, sum(!is.na(stats::coef(model))) - 1L)
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
