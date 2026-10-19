#' Additional heteroscedasticity tests
#'
#' These functions extend the package with more specialized tests
#' for heteroscedasticity such as a studentized Breusch-Pagan test,
#' a bootstrap version of White's test and the Szroeter ordered test.
#'
#' @name additional_tests
NULL

# Internal helper --------------------------------------------------------------

# `allow_aliased = TRUE` for a test computed on the estimable part of the
# design, which accepts a rank-deficient fit (see rnon_aliased_columns()).
prepare_model_data_for_test <- function(model, data, required_vars, test_label,
                                        min_obs_model = 10L, min_obs_data = 10L,
                                        allow_aliased = FALSE) {
  rvalidateModelInputs(
    model, test_name = test_label, min_obs = min_obs_model, allow_aliased = allow_aliased
  )
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = min_obs_data)

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  # Pearson residuals: identical to residuals(model) for an unweighted fit, and
  # sqrt(w) * e for a weighted one. See rpearson_residuals().
  residuals <- rpearson_residuals(model)

  resid_names <- names(residuals)
  data_rows <- rownames(cleaned$data)

  if (!is.null(resid_names) && length(resid_names) > 0 && !is.null(data_rows)) {
    match_idx <- match(resid_names, data_rows)
    if (anyNA(match_idx)) {
      missing_rows <- resid_names[is.na(match_idx)]
      stop(
        sprintf(
          "%s requires `data` to contain the rows used to fit the model. Missing rows: %s",
          test_label,
          paste(utils::head(missing_rows, 3L), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    rcheck_rows_after_cleaning(
      data, cleaned, match_idx, resid_names,
      fn = rtest_name(sys.parent()), test_label = test_label
    )
    aligned_data <- cleaned$data[match_idx, , drop = FALSE]
  } else {
    if (nrow(cleaned$data) != length(residuals)) {
      stop(
        sprintf(
          "%s requires `data` with %d observations to match the fitted model, got %d.",
          test_label,
          length(residuals),
          nrow(cleaned$data)
        ),
        call. = FALSE
      )
    }
    aligned_data <- cleaned$data
  }

  list(data = aligned_data, residuals = residuals)
}

#' Studentized Breusch–Pagan test (deprecated)
#'
#' `performStudentizedBPTest()` is deprecated since 0.14.0: use
#' [performKoenkerTest()], which computes the same statistic. It still works
#' and warns, and returns what it returned before except on a fit returned by
#' [fitWLS()]. The model frame of that fit holds the weights of the model
#' `fitWLS()` refitted (none, for an unweighted model), not the estimated
#' weights, and before 0.15.0 `performStudentizedBPTest()` read them from
#' there, so it computed the statistic of the unweighted fit. Since 0.15.0 it
#' reads the weights of the fit, as [performKoenkerTest()] does, and the two
#' agree. On a model with an offset they agree since 0.16.0: before, the refit
#' of the design left the offset out, and `performStudentizedBPTest()`
#' returned the statistic of another model.
#'
#' Computes Koenker's (1981) studentized form of the Breusch–Pagan Lagrange
#' multiplier statistic, \eqn{n R^2} from the regression of the squared
#' residuals on the regressors. The classical Breusch–Pagan statistic assumes
#' normal errors and this one does not: in the simulations of
#' `inst/validation/` it holds its level under \eqn{t_5} errors, where the
#' classical statistic rejects far too often.
#'
#' @param model A fitted [stats::lm] object providing the residuals and design
#'   matrix for the auxiliary regression.
#' @param data The data frame `model` was fitted to, or `NULL`, the default.
#'   The response, design, weights and offset are read from the fit itself, so
#'   a model that transforms its variables, has an `offset()` term, or takes
#'   its `weights` or `offset` from a column of the data needs no `data`, and
#'   the model frame of `model` is accepted as `data` (since 0.16.0; before,
#'   such models needed `data`). Data that are given are matched to the
#'   observations of the fit: rows with row names of their own by name, in any
#'   order; rows that only number themselves (a tibble, a data.table, a data
#'   frame whose row names were reset) by the row numbers of the fit, or in the
#'   order of the fit when they hold one row per observation. The rows the fit
#'   dropped may be there or not. Data whose rows cannot be shown to hold the
#'   observations of the fit are refused; see [test_results].
#'
#' @return An object of class \code{htest} reporting the chi-squared statistic
#'   and p-value for the null hypothesis of constant error variance.
#'
#' @details
#' Following Koenker (1981) and the implementation in
#' \link[lmtest:bptest]{lmtest::bptest()}, the procedure fits an auxiliary
#' regression of \eqn{e_i^2 - \hat{\sigma}^2} on the regressors from the
#' original model (including the intercept), where \eqn{e_i} denotes the
#' residuals and \eqn{\hat{\sigma}^2 = \sum e_i^2 / n} the mean of their
#' squares. With \eqn{\hat{g}_i} the fitted values of that regression, the
#' statistic \eqn{T = n \sum \hat{g}_i^2 / \sum (e_i^2 - \hat{\sigma}^2)^2} is
#' its \eqn{n R^2}. Under homoskedasticity it is asymptotically chi-squared with
#' degrees of freedom equal to the number of regressors beyond the intercept,
#' whether or not the errors are normal. On a weighted fit \eqn{e_i} is the Pearson
#' residual \eqn{\sqrt{w_i}} times the raw residual and the auxiliary
#' regression is unweighted; see the section on weighted fits in
#' [performKoenkerTest()]. This departs from `lmtest::bptest()`, which keeps the
#' raw residuals and weights the auxiliary regression. The residuals are those
#' of the refit of the design of the fit to its response, with its weights and
#' its offset, so on a model with an offset the statistic is that of the model
#' fitted to the response minus the offset, Koenker's statistic of the model
#' (since 0.16.0). The implementation
#' shares the validation
#' helpers used across the package to ensure that: (i) the model satisfies
#' minimum sample-size thresholds via \link[=rvalidateModelInputs]{rvalidateModelInputs()}, (ii) the
#' response, design, weights and offset are those of the fit itself, with the
#' rows of `data` matched to its observations (the model context, since
#' 0.16.0), and (iii) the
#' requirements registered for this test in
#' \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} are met.
#'
#' @references
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' \doi{10.1016/0304-4076(81)90062-2}
#'
#' Davidson, R., & MacKinnon, J. G. (2004). *Econometric Theory and Methods*.
#' Oxford University Press. Section 7.5 derives the test as \eqn{n R^2} from
#' the regression of the squared residuals on the variance regressors.
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit. A model with an offset is accepted since
#' 0.16.0: the refit of the design carries the offset, as [performKoenkerTest()]
#' accounts for it.
#' It refuses a multivariate `lm` ("needs a model with one response"), and,
#' since 0.16.0, an `mgcv::gam()` fit with smooth terms: a smooth term is
#' usually penalised, and a penalised fit is not the least-squares fit of the
#' design the test fits again. A later step settles which tests take such a
#' fit; a `gam` without smooth terms is tested as the Gaussian `glm` it is.
#' Which model each test accepts, and why, is set out in
#' [test_results], under *Supported models*.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' # Deprecated: warns, and returns the statistic of performKoenkerTest()
#' suppressWarnings(performStudentizedBPTest(mod, mtcars))
#' performKoenkerTest(mod, mtcars)
#'
#' @seealso
#' [performBPTest()] for the classical LM statistic, which assumes normal
#' errors, and [performKoenkerTest()], which computes the same studentized
#' \eqn{n R^2} statistic on the squared residuals. The robust workflow in
#' \link[=performBPTestRobust]{performBPTestRobust()} augments the studentized statistic with bootstrap
#' diagnostics.
#' @export
performStudentizedBPTest <- function(model, data = NULL) {
  rrefuse_non_gaussian_glm(model, "performStudentizedBPTest")
  # The model and `data` are checked before the deprecation warning, as
  # before 0.16.0 (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performStudentizedBPTest", test_label = "Studentized BP")
  .Deprecated("performKoenkerTest", old = "performStudentizedBPTest")
  .studentized_bp_test(model, data, data_name = deparse(substitute(model)), ctx = ctx)
}

# The implementation behind the deprecated performStudentizedBPTest(), the
# "student_bp" registry name and performBPTestRobust(studentize = TRUE). Since
# 0.16.0 it is built on the model context (R/model_context.R): the response,
# design, weights and offset come from the fit, and the refit of the design
# carries the offset, so that on a model with an offset it gives Koenker's
# statistic, as it does without one. `ctx` is the context its caller built, or
# `NULL`.
.studentized_bp_test <- function(model, data, data_name = deparse(substitute(model)),
                                 ctx = NULL) {
  test_label <- "Studentized BP"
  # The registry name "student_bp", its test_factory entry and
  # rvalidate_against_reference() call this implementation directly, not
  # performStudentizedBPTest(), so the model and the rows of `data` are
  # checked here too.
  if (is.null(ctx)) {
    ctx <- rmodel_context(model, data, fn = "performStudentizedBPTest", test_label = test_label)
  }
  rrefuse_smooth_gam(ctx, rtest_name(sys.parent(), "performStudentizedBPTest"))
  rvalidateModelInputs(model, test_name = test_label, min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  # A fit with a zero weight is refused where it was before 0.16.0, by the
  # Pearson residuals of the fit (rpearson_residuals()).
  rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("studentized_bp", model = model, data = working_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Studentized Breusch-Pagan test")

  if (length(ctx$residuals) == 0L) {
    .std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires observations in the model frame"
    )
  }

  response <- ctx$response
  # The design of the fit, without the columns a rank-deficient fit aliased
  # (rcontext_design()).
  design_matrix <- rcontext_design(ctx)

  if (ncol(design_matrix) < 2L) {
    stop(
      "Studentized BP test requires at least an intercept and one regressor.",
      call. = FALSE
    )
  }

  # The weights of the fit, not those of its model frame. The frame of a
  # fitWLS() fit is that of the model fitWLS() refitted, so it holds that
  # model's weights, or none, and not the estimated weights: before 0.15.0 the
  # statistic of a fitWLS() fit was that of the unweighted fit. They are read
  # as the other tests read them, which for a Gaussian glm are its prior
  # weights (rleast_squares_weights()).
  weights <- ctx$weights
  if (is.null(weights)) {
    weights <- rep.int(1, nrow(design_matrix))
  }

  if (length(weights) != nrow(design_matrix)) {
    stop(
      "Studentized BP test detected a mismatch between weights and the model matrix.",
      call. = FALSE
    )
  }

  positive_weights <- weights > 0
  effective_n <- sum(positive_weights)
  if (effective_n <= ncol(design_matrix)) {
    .std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires more positive-weight observations than regressors"
    )
  }

  # Pearson residuals sqrt(w) * e: the residuals of the equivalent unweighted
  # fit, and the ones that are homoscedastic when the weights are right. With
  # no weights this is the ordinary residual vector. Before 0.12.0 the raw
  # residuals were used and the auxiliary regression was weighted instead, the
  # lmtest::bptest() treatment, which tests the raw residuals for constant
  # variance and so rejects a correctly weighted fit. The refit carries the
  # total offset of the fit; before 0.16.0 it left the offset out, and a model
  # with an offset was refused.
  base_fit <- stats::lm.wfit(design_matrix, response, weights, offset = ctx$offset)
  residuals <- sqrt(weights) * base_fit$residuals

  if (length(residuals) != nrow(working_data)) {
    stop("Studentized BP test could not align residuals with the working data.", call. = FALSE)
  }

  sigma2 <- sum(residuals^2) / effective_n
  if (!is.finite(sigma2) || sigma2 <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires residual variance to be positive"
    )
  }

  centered_sq <- residuals^2 - sigma2
  if (!any(abs(centered_sq) > .Machine$double.eps)) {
    .std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires variability in centred squared residuals"
    )
  }

  # The auxiliary regression is unweighted: the squared Pearson residuals are
  # already on the homoscedastic scale.
  aux_fit <- stats::lm.wfit(design_matrix, centered_sq, rep.int(1, nrow(design_matrix)))
  df <- aux_fit$rank - 1L
  if (df <= 0) {
    stop("Studentized BP auxiliary regression has insufficient rank.", call. = FALSE)
  }

  numerator <- sum(aux_fit$fitted.values^2)
  denominator <- sum(centered_sq^2)
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Studentized BP auxiliary regression produced non-finite values"
    )
  }

  test_statistic <- effective_n * numerator / denominator
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Studentized Breusch-Pagan test",
      data.name = data_name,
      alternative = "heteroscedasticity present"
    ),
    class = "htest"
  )
}

#' Bootstrap White test
#'
#' Approximates the finite-sample distribution of White's LM statistic by
#' resampling the fitted residuals, refitting the model, and recalculating the
#' test statistic across many bootstrap replications.
#'
#' @inheritParams performWhiteTest
#' @param data A [base::data.frame] (or object coercible to one) containing the
#'   variables referenced by `model`. It must include the observations used to
#'   fit `model` and will be checked for missing values.
#'   When `NULL`, the default, the model frame of `model` is used. It holds the
#'   terms of the model rather than its variables (`log(x)` rather than `x`), so
#'   a model that transforms a variable needs `data`, as does a
#'   model with an `offset()` term, and so may a fit whose `weights` or `offset`
#'   argument is a column of the data, which the model frame holds as
#'   `(weights)` or `(offset)`.
#' @param B Integer scalar giving the number of bootstrap replications.
#'   Defaults to 999. The p-value is `(1 + #) / (B_eff + 1)`, where `B_eff` is
#'   the number of replicates whose statistic is finite (`B` when every
#'   replicate succeeds), so a test at level `alpha` is exact only when
#'   `alpha * (B_eff + 1)` is an integer: with 999 it is at the 5% and 1%
#'   levels, and with 1000, the default before 0.15.0, at neither.
#' @param parallel Logical scalar; if `TRUE` the replicates are computed in
#'   parallel, by forked workers ([parallel::mclapply()]) where the platform
#'   can fork and on a socket cluster ([parallel::makeCluster()]) elsewhere.
#'   See *Parallel replicates*.
#' @param n_cores Optional integer number of worker processes when
#'   `parallel = TRUE`. Defaults to `parallel::detectCores() - 1`. The result
#'   does not depend on it.
#' @inheritParams performWildBootstrapTest
#'
#' @return A \code{htest} object reporting both the asymptotic White statistic
#'   and its bootstrap p-value. The returned object also stores the simulated test
#'   statistics in `boot_statistics` for further inspection.
#'
#' @details
#' For a fitted model \eqn{\hat{y} = X\hat{\beta}}, the algorithm proceeds as
#' follows:
#' \enumerate{
#'   \item Compute White's LM statistic \eqn{n R^2} from the auxiliary regression
#'     on squares and cross-products of the regressors.
#'   \item Generate \eqn{B} bootstrap samples by resampling the centred residuals
#'     with replacement, forming \eqn{y^{*(b)} = \hat{y} + \hat{e}^{*(b)}}.
#'   \item Refit the model to each bootstrap sample and recompute White's
#'     statistic \eqn{T^{*(b)}} using the same auxiliary specification.
#'   \item Estimate the bootstrap p-value as
#'     \eqn{\hat{p} = (1 + \sum_{b} I\{T^{*(b)} \ge T_{\text{obs}}\}) /
#'     (B_{\text{eff}} + 1)}{p = (1 + number of b with T*(b) >= T_obs) / (B_eff + 1)},
#'     where \eqn{b} runs over the \eqn{B_{\text{eff}}}{B_eff} replicates whose
#'     statistic is finite, so the p-value is never zero.
#' }
#'
#' The bootstrap distribution offers improved size control for moderate sample
#' sizes or high-dimensional designs where the chi-squared approximation may be
#' inaccurate. When `parallel = TRUE` the resampling step exploits available CPU
#' cores to reduce computation time.
#'
#' @section Parallel replicates:
#' With `parallel = TRUE` replicate `i` draws its random numbers from stream
#' `i` of a L'Ecuyer-CMRG generator ([parallel::nextRNGStream()]), seeded by
#' one number drawn from the session's generator. A call made after
#' `set.seed()` therefore returns the same result each time, whatever the
#' number of workers and whether they are forked or run on a socket cluster.
#' It does not return the result of the same call with `parallel = FALSE`,
#' which draws every replicate from the session's generator, as it did before
#' 0.15.0: the two are bootstraps of the same data with different draws, and
#' each is valid. Before 0.15.0 a seeded call with `parallel = TRUE` gave a
#' different result each time, and on Windows more than one worker was an
#' error.
#'
#' @section Weighted fits:
#' A fit with weights \eqn{w_i} is ordinary least squares on
#' \eqn{\sqrt{w_i}\, y_i}{sqrt(w_i) y_i} and \eqn{\sqrt{w_i}\, x_i}{sqrt(w_i) x_i},
#' whose errors have constant variance under the null hypothesis, and the
#' bootstrap is carried out in that regression. What is resampled is its
#' residuals, the Pearson residuals \eqn{\sqrt{w_i}\, e_i}{sqrt(w_i) e_i}.
#' When the model has an intercept they are centred, as the residuals of an
#' unweighted fit with an intercept are, because the weighted regression has
#' no constant column. Each draw is divided by \eqn{\sqrt{w_i}}{sqrt(w_i)} to
#' return it to the scale of the observation it is added to, and each
#' replicate is refitted with the weights, so [performWhiteTest()] computes
#' its statistic from the Pearson residuals as it does for the fit supplied.
#' With equal weights the result is that of the unweighted fit.
#'
#' Weighted or not, each replicate is refitted with an offset the model was
#' given as an argument, `lm(..., offset = o)`; an offset written in the
#' formula, `offset(o)`, is evaluated again from the data of the replicate. A
#' transformed response, such as `log(y)` in `log(y) ~ x`, is regenerated on
#' the scale the model uses, and the refit reads it in place of `log(y)`.
#'
#' Weights given to the fit are known, and they are held fixed in every
#' replicate. On a fit returned by [fitWLS()] they were estimated from the same
#' data, and since 0.15.0 they are estimated again in each replicate by
#' default (`reestimate_weights`): the replicate is fitted as the model
#' `fitWLS()` was given, the variance function `fitWLS()` recorded is fitted
#' again to its residuals, on the same regressors and with the same form, and
#' the replicate is refitted with the weights it gives, so that
#' [performWhiteTest()] sees a `fitWLS()` fit of the replicate. A replicate
#' whose variance function cannot be fitted, where `fitWLS()` would fall back
#' to equal weights, counts as a failed replicate, and the p-value is computed
#' from the replicates that remain; a warning gives the number that failed.
#'
#' In `inst/validation/weighted-fits-size.R`, the data have standard deviation
#' \eqn{\exp(0.3 x)}{exp(0.3 x)}, the variance function is correctly
#' specified, the errors are normal, there are 150 observations and
#' `B = 499`. At the 5% level the test then rejected 0.064 of the time when it
#' held the weights of `fitWLS()` fixed, as every call did before 0.15.0 and
#' `reestimate_weights = FALSE` still does. It rejected 0.0415 when it
#' estimated them again, just below the release gate's interval, 0.042 to
#' 0.058, and 0.054 with the true weights. The section on weighted fits
#' in [performWildBootstrapTest()] has the larger error of Koenker's
#' statistic.
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the Bootstrap*.
#' Chapman & Hall/CRC.
#'
#' Davidson, R., & MacKinnon, J. G. (2006). The power of bootstrap and asymptotic
#' tests. *Journal of Econometrics, 133*(2), 421–441.
#' \doi{10.1016/j.jeconom.2005.02.002}
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit. Which model each test accepts, and why, is
#' set out in [test_results], under *Supported models*.
#'
#' @examples
#' # The bootstrap needs at least 50 observations, so mtcars (32) is too small.
#' set.seed(42)
#' n <- 120
#' sim <- data.frame(x = runif(n, 1, 5))
#' sim$y <- 1 + 2 * sim$x + rnorm(n, sd = 0.3 + 0.6 * sim$x)
#' mod <- lm(y ~ x, data = sim)
#' performWhiteTestBootstrap(mod, sim, B = 199)
#'
#' \donttest{
#' # More replications give a smoother p-value estimate
#' performWhiteTestBootstrap(mod, sim, B = 499)
#'
#' # parallel = TRUE runs the replicates on n_cores workers, forked where the
#' # platform can fork and on a socket cluster elsewhere. Two is the most
#' # R CMD check --as-cran allows, and the result does not depend on it.
#' set.seed(42)
#' performWhiteTestBootstrap(mod, sim, B = 199, parallel = TRUE, n_cores = 2)
#' }
#'
#' @seealso
#' [performWhiteTest()] for the classical statistic and
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()} for enriched diagnostics.
#' @export
performWhiteTestBootstrap <- function(model, data = NULL, B = 999, parallel = FALSE,
                                      n_cores = NULL, reestimate_weights = NULL) {
  rrefuse_non_gaussian_glm(model, "performWhiteTestBootstrap")
  # The variance function of a fitWLS() fit, estimated again in each
  # replicate, or NULL, which holds the weights fixed as before 0.15.0.
  variance_record <- rwls_reestimation(model, reestimate_weights, "performWhiteTestBootstrap")
  data <- rdata_or_model_frame(model, data)
  test_label <- "Bootstrap White"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 20L,
    min_obs_data = 20L,
    allow_aliased = TRUE
  )

  # Under the row names of the fit, so that performWhiteTest() below matches
  # the rows to the same observations.
  working_data <- rdata_with_fit_rows(prepared$data, names(prepared$residuals))

  requirements <- rvalidateTestRequirements("bootstrap_tests", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }
  if (!is.null(n_cores) &&
        (!is.numeric(n_cores) || length(n_cores) != 1L || is.na(n_cores) || n_cores < 1)) {
    stop("`n_cores` must be NULL or a positive integer.", call. = FALSE)
  }

  ht_log("INFO", "Running Bootstrap White test")

  # The observed statistic and the replicates come from the same kind of
  # design, rebuilt from the rows of the fit as before 0.16.0, until increment
  # 8 of the model context builds the replicates on the stored design
  # (rwhite_test_rebuilt_design()).
  original_result <- rwhite_test_rebuilt_design(model, working_data)
  original_stat <- unname(original_result$statistic[1])

  # A weighted fit is ordinary least squares on sqrt(w) y and sqrt(w) X, whose
  # errors have constant variance under the null hypothesis and whose
  # residuals are the Pearson residuals. Those are resampled, and each draw is
  # returned to the scale of its observation by dividing by sqrt(w). When the
  # model has an intercept they are centred first: that regression has no
  # constant column, so its residuals need not average zero, while those of an
  # unweighted fit with an intercept do. Without an intercept neither path
  # centres, so equal weights resample exactly as no weights. The refit
  # carries the weights, so performWhiteTest() computes the statistic of each
  # replicate as it computed the observed one, from the Pearson residuals.
  #
  # Weighted or not, the refit carries an offset the model was given as an
  # argument, lm(..., offset = o), which the formula does not hold; one written
  # in the formula, offset(o), is evaluated again from the data. working_data
  # holds the fitted rows in the order of the fit, so the offset, the weights
  # and the fitted values are taken as they are.
  prior_weights <- rleast_squares_weights(model)
  offset_argument <- roffset_argument(model)
  if (!is.null(prior_weights)) {
    pool <- rpearson_residuals(model)
    if (attr(stats::terms(model), "intercept") == 1L) {
      pool <- pool - mean(pool)
    }
  }

  # The regenerated response is on the scale of the fitted values, which is
  # that of the response the model uses. A plain response is overwritten in
  # its column. A transformed one, such as log(y) in log(y) ~ x, is not a
  # column of the data: written into a column named "log(y)", it was ignored
  # by the refit, which computed log(y) from y again, so every replicate was
  # the data of the fit, its statistic the observed one, and the p-value 1.
  # It now goes into a column of its own, which the formula of the refit reads
  # in place of the transformed response.
  boot_formula <- stats::formula(model)
  response_name <- as.character(boot_formula)[2]
  if (!is.name(boot_formula[[2L]])) {
    response_name <- ".ht_bootstrap_response"
    while (response_name %in% names(working_data)) {
      response_name <- paste0(".", response_name)
    }
    boot_formula[[2L]] <- as.name(response_name)
  }

  bootstrap_stat <- function() {
    fitted_vals <- stats::fitted(model)
    if (is.null(prior_weights)) {
      resid <- stats::residuals(model)
      boot_resid <- sample(resid, replace = TRUE)
    } else {
      boot_resid <- sample(pool, replace = TRUE) / sqrt(prior_weights)
    }
    boot_y <- fitted_vals + boot_resid

    boot_data <- working_data
    boot_data[[response_name]] <- boot_y

    if (!is.null(variance_record)) {
      # fitWLS() run again on the replicate: the model is refitted as it was
      # before fitWLS() weighted it, its variance function is fitted again and
      # it is refitted with the weights that gives. A replicate whose variance
      # function cannot be fitted fails, and the p-value counts the replicates
      # that remain.
      initial_weights <- variance_record$initial_weights
      initial <- if (is.null(initial_weights) && is.null(offset_argument)) {
        safe_lm(boot_formula, data = boot_data)
      } else {
        rrefit_lm(boot_formula, boot_data, initial_weights, offset_argument)
      }
      boot_model <- rwls_refit_replicate(initial, variance_record, variance_record$regressors)
      if (is.null(boot_model)) {
        return(NA_real_)
      }
    } else if (is.null(prior_weights) && is.null(offset_argument)) {
      boot_model <- safe_lm(boot_formula, data = boot_data)
    } else {
      boot_model <- rrefit_lm(boot_formula, boot_data, prior_weights, offset_argument)
    }
    unname(rwhite_test_rebuilt_design(boot_model, boot_data)$statistic[1])
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Replicate i draws from stream i of a L'Ecuyer-CMRG generator seeded by
    # one draw from the session's, so a seeded call gives the same replicates
    # each time, with any number of workers. Before 0.15.0 the forked workers
    # drew from generators set.seed() did not reach, and on Windows, which
    # cannot fork, more than one worker was an error; there the replicates now
    # run on a socket cluster.
    workers <- if (is.null(n_cores)) {
      cores <- tryCatch(parallel::detectCores(), error = function(e) NA_integer_)
      if (is.na(cores)) 1L else max(1L, cores - 1L)
    } else {
      as.integer(n_cores)
    }
    boot_stats <- rparallel_tasks(
      seq_len(B), function(i) bootstrap_stat(), rtask_streams(B), workers
    )
    boot_stats <- as.numeric(unlist(boot_stats, use.names = FALSE))
  } else {
    boot_stats <- replicate(B, bootstrap_stat())
  }

  # Finite-simulation convention (Davison & Hinkley 1997, sec. 4.2):
  # p = (1 + #{T_b >= T_obs}) / (B_eff + 1). The raw proportion can return
  # exactly zero, which is not an attainable p-value from a finite number of
  # replicates, and it is anti-conservative in the tail.
  finite_stats <- boot_stats[is.finite(boot_stats)]
  effective <- length(finite_stats)
  if (!is.null(variance_record)) {
    rwarn_failed_replicates("performWhiteTestBootstrap", B - effective, B)
  }
  if (effective == 0L) {
    .std_error(
      "rassumption_violation",
      assumption = "Bootstrap White test produced no usable replicates"
    )
  }
  p_value <- (1 + sum(finite_stats >= original_stat)) / (effective + 1)

  structure(
    list(
      statistic = original_result$statistic,
      parameter = c(B = B),
      p.value = p_value,
      method = "Bootstrap White test",
      data.name = deparse(substitute(model)),
      boot_statistics = boot_stats
    ),
    class = "htest"
  )
}

#' Szroeter test for ordered alternatives
#'
#' Detects monotonic changes in variance when observations can be meaningfully
#' ordered—typically by time or another covariate—using the cumulative weighting
#' scheme proposed by Szroeter (1978).
#'
#' @inheritParams performHarveyTest
#' @param data A [base::data.frame] (or compatible tibble) containing the
#'   variables used to fit `model` and the ordering variable supplied via
#'   `order_by`.
#'   When `NULL`, the default, the model frame of `model` is used, so `order_by`
#'   must then be a variable of the model. The model frame holds the terms of
#'   the model rather than its variables (`log(x)` rather than `x`), so a model
#'   that transforms a variable needs `data`, as does a
#'   model with an `offset()` term.
#' @param order_by Character scalar naming the column that defines the ordering of
#'   observations prior to computing the statistic. The column must be numeric or
#'   coercible to an orderable vector.
#' @param alternative Character scalar specifying the alternative hypothesis:
#'   `"greater"` (the default) tests for variance increasing with `order_by`,
#'   `"less"` for variance decreasing, and `"two.sided"` for a change in either
#'   direction. Szroeter's test targets monotone alternatives, so the one-sided
#'   form is the usual choice.
#'
#' @return An object of class \code{htest} reporting Szroeter's
#'   standardised statistic `Q`, the sample size, the underlying rank-weighted
#'   statistic `h` in `estimate`, and the p-value from the asymptotic normal
#'   reference distribution.
#'
#' @details
#' After ordering the residuals \eqn{\hat{e}_{(i)}} by `order_by`, the test forms
#' the rank-weighted average of the squared residuals
#' \deqn{h = \frac{\sum_{i = 1}^n i \, \hat{e}_{(i)}^2}{\sum_{i = 1}^n \hat{e}_{(i)}^2},}
#' which is Szroeter's (1978) class of statistics evaluated at the canonical
#' weights \eqn{h_i = i}. Under homoskedasticity \eqn{h} is centred on the mean
#' rank \eqn{(n + 1) / 2} with variance \eqn{(n^2 - 1) / (6n)}, so
#' \deqn{Q = \frac{h - (n + 1) / 2}{\sqrt{(n^2 - 1) / (6n)}}}
#' is asymptotically standard normal. Variance that grows with `order_by` shifts
#' weight onto the high ranks and drives \eqn{Q} upwards.
#'
#' The implementation relies on the package validation helpers to align the
#' supplied data with the model residuals, check that the ordering variable is
#' present, and ensure sufficient sample size and variability in the squared
#' residuals.
#'
#' Ties in `order_by` keep the order of the rows the model was fitted to.
#' Unless the rows were sorted by the response, that order does not depend on
#' the residuals, so the test keeps its level under the null hypothesis; but
#' with a discrete ordering variable the statistic depends on the order of the
#' rows.
#'
#' `runHeteroTests(tests = "szroeter")`, and anything else that runs the
#' registered diagnostic, orders the observations by the fitted values of
#' `model`, so its alternative is variance that rises with the mean. With a
#' single regressor that is the order of the regressor when its slope is
#' positive and the reverse order when it is negative. The fitted values are
#' computed from the coefficients, so observations with the same regressors
#' tie exactly, as they do under a factor or a binary regressor, and the ties
#' are broken as above.
#'
#' @section Validation:
#' The statistic and its null variance are checked against an independent
#' reconstruction of Szroeter (1978) in
#' `tests/testthat/test-pass-a-reference.R`. Releases before 0.7.0 divided
#' the centred statistic by a further \eqn{\sqrt{n}}, shrinking \eqn{Q} by a
#' factor of roughly \eqn{2 / \sqrt{n}} and leaving the test with essentially no
#' power against any alternative; see `NEWS.md`.
#'
#' @references
#' Szroeter, J. (1978). A class of parametric tests for heteroscedasticity in
#' linear econometric models. *Econometrica, 46*(6), 1311–1327.
#' \doi{10.2307/1913833}
#'
#' Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*. Cambridge
#' University Press. Section 5.4 outlines the Szroeter test.
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit. Which model each test accepts, and why, is
#' set out in [test_results], under *Supported models*.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSzroeterTest(mod, mtcars, order_by = "wt")
#'
#' # Detect ordered heteroscedasticity in simulated data with variance increasing
#' # in an index variable
#' set.seed(404)
#' n <- 150
#' x <- sort(runif(n))
#' y <- 2 + 0.5 * x + rnorm(n, sd = 0.4 + 0.8 * x)
#' df <- data.frame(y, x)
#' performSzroeterTest(lm(y ~ x, data = df), df, order_by = "x")
#'
#' # Two-sided version when the direction of the change is not known in advance
#' performSzroeterTest(mod, mtcars, order_by = "wt", alternative = "two.sided")
#'
#' @seealso
#' [performKoenkerTest()] for the regressor-based alternative and
#' [performGQTest()] for split-sample diagnostics on ordered data.
#' @export
performSzroeterTest <- function(model, data = NULL, order_by,
                                alternative = c("greater", "two.sided", "less")) {
  rrefuse_non_gaussian_glm(model, "performSzroeterTest")
  data <- rdata_or_model_frame(model, data)
  test_label <- "Szroeter test"

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  alternative <- match.arg(alternative)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L,
    allow_aliased = TRUE
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("szroeter", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    .std_error("missing_variable", variable = order_by)
  }

  ht_log("INFO", "Running Szroeter test")

  ord <- order(working_data[[order_by]])
  e_ordered <- residuals[ord]
  n <- length(e_ordered)

  if (n <= 1) {
    .std_error(
      "rinsufficient_sample_size",
      test_name = test_label,
      min_obs = 2L,
      n_obs = n
    )
  }

  if (stats::var(e_ordered) <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Szroeter test requires variability in ordered residuals"
    )
  }

  # Szroeter's (1978) h statistic with the canonical weights h_i = i: a
  # rank-weighted average of the squared residuals in the supplied ordering.
  ranks <- seq_len(n)
  h_stat <- sum(ranks * e_ordered^2) / sum(e_ordered^2)

  # Under homoskedasticity h has mean (n + 1) / 2 (the mean rank) and variance
  # (n^2 - 1) / (6 n), giving an asymptotically standard normal Q. Note the
  # sqrt(n) in the scale factor: h is a weighted mean of the ranks, so its
  # dispersion grows with n and the centred statistic must NOT be divided by a
  # further sqrt(n).
  q_stat <- (h_stat - (n + 1) / 2) / sqrt((n^2 - 1) / (6 * n))

  p_value <- switch(
    alternative,
    "greater" = stats::pnorm(q_stat, lower.tail = FALSE),
    "less" = stats::pnorm(q_stat),
    "two.sided" = 2 * stats::pnorm(-abs(q_stat))
  )

  alternative_label <- switch(
    alternative,
    "greater" = sprintf("variance increases with '%s'", order_by),
    "less" = sprintf("variance decreases with '%s'", order_by),
    "two.sided" = sprintf("variance changes monotonically with '%s'", order_by)
  )

  structure(
    list(
      statistic = c(Q = q_stat),
      parameter = c(n = n),
      estimate = c(h = h_stat),
      p.value = p_value,
      method = "Szroeter test for ordered heteroscedasticity",
      data.name = deparse(substitute(model)),
      alternative = alternative_label
    ),
    class = "htest"
  )
}
