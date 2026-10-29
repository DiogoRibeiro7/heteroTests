#' Additional heteroscedasticity tests
#'
#' These functions extend the package with more specialized tests
#' for heteroscedasticity such as a studentized Breusch-Pagan test,
#' a bootstrap version of White's test and the Szroeter ordered test.
#'
#' @name additional_tests
NULL

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

  # Koenker's statistic, with Koenker's test's warning on a factor with more
  # levels than observations per level (rwarn_small_groups(),
  # R/model_context.R).
  rwarn_small_groups(ctx$model, ctx$frame, "the studentized Breusch-Pagan test",
                     "performKoenkerTest")

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
#' @param data The data frame `model` was fitted to, or `NULL`, the default.
#'   The response, design, weights and offset are read from the fit itself, so
#'   a model that transforms its variables, has an `offset()` term, or takes
#'   its `weights` or `offset` from a column of the data needs no `data`, and
#'   the model frame of `model` is accepted as `data` (since 0.17.0; before,
#'   such models needed `data`). Data that are given are matched to the
#'   observations of the fit: rows with row names of their own by name, in any
#'   order; rows that only number themselves (a tibble, a data.table, a data
#'   frame whose row names were reset) by the row numbers of the fit, or in the
#'   order of the fit when they hold one row per observation. The rows the fit
#'   dropped may be there or not. Data whose rows cannot be shown to hold the
#'   observations of the fit are refused; see [test_results].
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
#' Weighted or not, each replicate is refitted on the stored design of the
#' fit with its total offset, the `offset` argument and the `offset()` terms
#' (see *The design of the replicates*). A transformed response, such as
#' `log(y)` in `log(y) ~ x`, is regenerated on the scale the model uses.
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
#' @section The design of the replicates:
#' Since 0.17.0 the observed statistic and every replicate are computed on the
#' design stored with the fit, the design `performWhiteTest()` reads: each
#' replicate, the fitted values plus the errors drawn, is refitted by least
#' squares on that design, with the weights and the total offset of the fit,
#' and its statistic is White's on the same design, without the columns its
#' refit aliased. Nothing is evaluated again from `data` or the formula. Up to
#' 0.16.0 both were computed on a design rebuilt from the rows of `data`, each
#' replicate refitted from the formula: `cut(x, quantile(x))` computed other
#' breaks on the rows of the fit and stopped the test ("factor ... has new
#' levels"); a term computed from all the rows of the data, such as
#' `I(x > median(x))`, took other values on a fit that dropped rows; a fit
#' whose weights or offset are a column of the data, or with an `offset()` term
#' or a transformed variable, stopped without `data`; and an `mgcv::gam()` fit,
#' whose residuals carry no names, given its data in another order, had each
#' draw added to the fitted value of another row. The random numbers are
#' drawn as before, in the same order, so a call made after `set.seed()`
#' returns what it returned wherever the stored design is the design rebuilt
#' from `data`.
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
#' lack rows or variables of the fit. Since 0.17.0 it refuses an `mgcv::gam()`
#' fit with smooth terms: a smooth term is usually penalised, and a penalised
#' fit is not the least-squares fit of its design, which each replicate fits
#' again (before, the refit from the formula stopped on the smooth term); a
#' `gam` without smooth terms is tested as the Gaussian `glm` it is. It also
#' refuses a fit whose coefficients, residuals or weights were changed after it
#' was fitted, from which it computed the value of the fit before the edit, and
#' a `fitWLS()` fit of a `gam` with smooth terms, whose terms do not rebuild the
#' design of its coefficients, on which it computed a value on the variables of
#' the formula.
#' Which model each test accepts, and why, is set out in [test_results], under
#' *Supported models*.
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
  fn <- "performWhiteTestBootstrap"
  test_label <- "Bootstrap White"
  rrefuse_non_gaussian_glm(model, fn)
  # The variance function of a fitWLS() fit, estimated again in each
  # replicate, or NULL, which holds the weights fixed as before 0.15.0.
  variance_record <- rwls_reestimation(model, reestimate_weights, fn)
  # The response, design, weights, offset and residuals come from the fit;
  # `data`, when given, is matched to its observations and checked
  # (R/model_context.R). Since increment 8 of the model context the observed
  # statistic and the replicates are both computed on the stored design;
  # before, both were computed on a design rebuilt from the rows of `data`
  # (rwhite_test_rebuilt_design(), gone).
  ctx <- rmodel_context(model, data, fn = fn, test_label = test_label)
  # Each replicate refits the design by least squares, which is not the fit of
  # a penalised smooth term (decision 23 of the design of the model context).
  rrefuse_smooth_gam(ctx, fn)
  rvalidateModelInputs(model, test_name = test_label, min_obs = 20L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  # The Pearson residuals; a zero weight is refused here, as before.
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

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

  # The observed statistic: White's test on the stored design, with the checks
  # performWhiteTest() makes, without its warning on a factor with more levels
  # than observations per level, which concerns the asymptotic p-value the
  # bootstrap does not report.
  rvalidateModelInputs(model, test_name = "White", min_obs = 20L, allow_aliased = TRUE)
  rprocessValidationResult(
    rvalidateTestRequirements("white", model = model, data = working_data)
  )
  tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NULL
  )
  if (nrow(working_data) > 10000) {
    message("Large dataset (", nrow(working_data), " observations). ",
            "This may take some time to compute.")
  }
  original_result <- rwhite_statistic(residuals, rcontext_design(ctx), TRUE, 10, "White test",
                                      data_name = "model")
  original_stat <- unname(original_result$statistic[1])

  # A weighted fit is ordinary least squares on sqrt(w) y and sqrt(w) X, whose
  # errors have constant variance under the null hypothesis and whose
  # residuals are the Pearson residuals. Those are resampled, and each draw is
  # returned to the scale of its observation by dividing by sqrt(w). When the
  # model has an intercept they are centred first: that regression has no
  # constant column, so its residuals need not average zero, while those of an
  # unweighted fit with an intercept do. Without an intercept neither path
  # centres, so equal weights resample exactly as no weights. The refit
  # carries the weights, so the statistic of each replicate is computed as the
  # observed one is, from the Pearson residuals.
  prior_weights <- ctx$weights
  if (!is.null(prior_weights)) {
    pool <- residuals
    if (attr(stats::terms(model), "intercept") == 1L) {
      pool <- pool - mean(pool)
    }
  }

  # Each replicate is the fitted values plus the regenerated errors, on the
  # scale of the response the model uses (log(y) for log(y) ~ x), refitted by
  # least squares on the stored design, every column of it, with the weights
  # and the total offset of the fit, the `offset` argument and the offset()
  # terms: what lm() computes from the formula when the design it rebuilds is
  # the stored one, without evaluating the formula again. Before, each
  # replicate was refitted from the formula on the rows of `data`, so a term
  # computed from the rows it is given, such as cut(x, quantile(x)), took other
  # values than in the fit, and a model that transforms a variable, or takes
  # its weights or its offset from a column, could not be refitted without
  # `data`. The statistic of a replicate is White's on the stored design,
  # without the columns its refit aliased. The draws are those of before, in
  # the same order, so a seeded call returns what it returned wherever the
  # stored design is the design rebuilt from `data`.
  design <- ctx$design
  model_offset <- ctx$offset
  fitted_vals <- ctx$fitted
  # Without the padding of na.exclude (B13), whose NA the replicates drew.
  raw_residuals <- if (is.null(prior_weights)) rcontext_unpad(ctx, stats::residuals(model))
  # Local copies travel with bootstrap_stat() to the workers of a socket
  # cluster, which load the installed package.
  statistic_of <- rwhite_statistic
  replicate_residuals <- rwls_replicate_residuals
  estimated_columns <- rreplicate_estimated_columns
  bootstrap_stat <- function() {
    if (is.null(prior_weights)) {
      boot_resid <- sample(raw_residuals, replace = TRUE)
    } else {
      boot_resid <- sample(pool, replace = TRUE) / sqrt(prior_weights)
    }
    boot_y <- fitted_vals + boot_resid

    if (!is.null(variance_record)) {
      # fitWLS() run again on the replicate: the design is refitted as the
      # model was before fitWLS() weighted it, its variance function is fitted
      # again and it is refitted with the weights that gives. A replicate whose
      # variance function cannot be fitted fails, and the p-value counts the
      # replicates that remain.
      refit <- replicate_residuals(design, boot_y, model_offset, variance_record,
                                   with_fit = TRUE)
      if (is.null(refit)) {
        return(NA_real_)
      }
      boot_pearson <- refit$pearson
      boot_fit <- refit$fit
    } else {
      boot_fit <- if (is.null(prior_weights)) {
        stats::lm.fit(design, boot_y, offset = model_offset)
      } else {
        stats::lm.wfit(design, boot_y, prior_weights, offset = model_offset)
      }
      boot_pearson <- if (is.null(prior_weights)) {
        boot_fit$residuals
      } else {
        boot_fit$residuals * sqrt(prior_weights)
      }
    }
    boot_design <- estimated_columns(design, boot_fit$coefficients, boot_fit$rank)
    unname(statistic_of(boot_pearson, boot_design, TRUE, 10, "White test",
                        data_name = "boot_model")$statistic[1])
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
#' @param data The data frame `model` was fitted to, or `NULL`, the default. The
#'   residuals and weights are read from the fit itself, and `order_by` from the
#'   rows of `data` that hold the observations of the fit. Without `data` it is
#'   read from the model frame of `model`, which holds it only when it is a
#'   variable of the model under its own name (`x`, not `log(x)`); a model that
#'   transforms the variable it is ordered by needs `data`, as does an ordering
#'   variable the model does not use. Since 0.16.0, data that are given are
#'   matched to the observations of the fit: rows with row names of their own by
#'   name, in any order; rows that only number themselves (a tibble, a
#'   data.table, a data frame whose row names were reset) by the row numbers of
#'   the fit, or in the order of the fit when they hold one row per observation.
#'   The rows the fit dropped may be there or not. Data whose rows cannot be
#'   shown to hold the observations of the fit are refused; see [test_results].
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
#'   statistic `h` in `estimate`, and the p-value from the normal reference
#'   distribution, with the null mean and variance of `h` given the design.
#'
#' @details
#' After ordering the residuals \eqn{\hat{e}_{(i)}} by `order_by`, the test forms
#' the rank-weighted average of the squared residuals
#' \deqn{h = \frac{\sum_{i = 1}^n i \, \hat{e}_{(i)}^2}{\sum_{i = 1}^n \hat{e}_{(i)}^2},}
#' which is Szroeter's (1978) class of statistics evaluated at the canonical
#' weights \eqn{h_i = i}. Variance that grows with `order_by` shifts weight onto
#' the high ranks and drives \eqn{h} upwards.
#'
#' Since 0.16.0 the null mean and variance of \eqn{h} are those of the design.
#' Let \eqn{M} be the residual-maker of the design in the order of the test
#' (the rows \eqn{\sqrt{w_i}\, x_i} on a weighted fit), \eqn{A = \mathrm{diag}(1,
#' \ldots, n)}, \eqn{k} the rank of the design and \eqn{\nu = n - k}. Under normal
#' errors of constant variance (given the weights) the residuals make \eqn{h} a
#' ratio of quadratic forms, \eqn{h = \sum_j \lambda_j z_j^2 / \sum_j z_j^2},
#' with \eqn{z_j} independent standard normal and \eqn{\lambda_j} the
#' \eqn{\nu} eigenvalues of \eqn{MAM} on the residual space. The ratio is
#' independent of its denominator, so its moments are ratios of moments:
#' \deqn{E\,h = \frac{\mathrm{tr}(MA)}{\nu} = \frac{n (n + 1) / 2 - \sum_i i\, l_{(i)}}{\nu},
#'   \qquad \mathrm{Var}\,h = \frac{2\, (\nu\, \mathrm{tr}(MAMA) - \mathrm{tr}(MA)^2)}{\nu^2 (\nu + 2)},}
#' with \eqn{l_{(i)}} the leverages in the order of the test and
#' \eqn{\mathrm{tr}(MAMA) = \sum_i i^2 - 2 \sum_i i^2 l_{(i)} + \|Q_1' A Q_1\|_F^2},
#' \eqn{Q_1} an orthonormal basis of the design, so that no eigenvalue is
#' computed and the cost is \eqn{O(n k^2)}. The test refers
#' \deqn{Q = \frac{h - E\,h}{\sqrt{\mathrm{Var}\,h}}}
#' to the standard normal distribution. The moments are derived here, by the
#' argument that gives those of the Durbin-Watson statistic, and are not taken
#' from Szroeter (1978). Without regressors they are \eqn{(n + 1) / 2} and
#' \eqn{(n^2 - 1) / (6 (n + 2))}. Before 0.16.0 the test used \eqn{(n + 1) / 2}
#' and \eqn{(n^2 - 1) / (6n)}, Szroeter's asymptotic moments, whatever the
#' design: on a weighted fit whose leverage falls along the ordering the mean
#' of \eqn{h} is above \eqn{(n + 1) / 2} (by about 0.44 in the design of the
#' validation below, at every \eqn{n}), and the test rejected too often. The
#' statistic \eqn{h} is unchanged; \eqn{Q} and the p-value change for every
#' fit, on `lm(mpg ~ wt + qsec, mtcars)` ordered by `wt` from -0.6494 and
#' 0.7420 to -0.6626 and 0.7462.
#'
#' The test needs at least two residual degrees of freedom, \eqn{\nu \ge 2}.
#' With one the residuals lie on a single line, \eqn{h} takes the same value
#' whatever the errors and its null variance is 0: since 0.16.0 the test
#' refuses such a fit, for which it returned a value before.
#'
#' The residuals and weights are those of the fit, and `order_by` is read from
#' the rows of `data` that hold its observations (the model context, since
#' 0.16.0); the package validation helpers check that the ordering variable is
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
#' are broken as above. Since 0.16.0 they are read from the fit, in its order,
#' and `data` is matched to the observations of the fit as this function
#' matches it: without `data` the registered test reads the model frame, and
#' data in the order of the fit whose row numbers are not those of the fit are
#' taken. Before, it matched the fitted values to the row names of `data`, and
#' refused both ("Missing rows").
#'
#' @section Validation:
#' The statistic and its null moments are checked against an independent
#' reconstruction from the full residual-maker in
#' `tests/testthat/test-pass-a-reference.R`, and the moments against the
#' eigenvalues of \eqn{MAM} to `1e-13`. Releases before 0.7.0 divided
#' the centred statistic by a further \eqn{\sqrt{n}}, shrinking \eqn{Q} by a
#' factor of roughly \eqn{2 / \sqrt{n}} and leaving the test with essentially no
#' power against any alternative; see `NEWS.md`.
#'
#' `inst/validation/szroeter-design-null-size.R` tested the samples of
#' `inst/validation/harrison-mccabe-size-power.R`, `lm(y ~ x1 + x2)` ordered by
#' `x1`, at 5000 replications per cell. With Gaussian errors the test rejected
#' a true null hypothesis at the 5% level 0.048, 0.050 and 0.049 of the time at
#' 40, 100 and 400 observations, and 0.049, 0.047 and 0.050 on a correctly
#' weighted fit (weights \eqn{1 / x_1^2}), all inside the release gate's
#' interval, 0.042 to 0.058; with the moments of 0.15.0 it rejected 0.044, 0.048
#' and 0.049, and on the weighted fit 0.067, 0.056 and 0.055. Its moments
#' agreed with those of 100,000 simulated samples of one design of each size,
#' variances within 1%. The exact null distribution of \eqn{h} given the design,
#' from the eigenvalues by Imhof's (1961) method, gave the same rates within
#' 0.0006, at a cost of \eqn{O(n^3)} per call. Under \eqn{t_5} errors no version
#' holds the level (0.10 to 0.15): the null assumes normal errors, and
#' [performGlejserTest()] with `robust = TRUE` is the test to use there.
#'
#' @references
#' Szroeter, J. (1978). A class of parametric tests for heteroscedasticity in
#' linear econometric models. *Econometrica, 46*(6), 1311–1327.
#' \doi{10.2307/1913833}
#'
#' Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*. Cambridge
#' University Press. Section 5.4 outlines the Szroeter test.
#'
#' Imhof, J. P. (1961). Computing the distribution of quadratic forms in
#' normal variables. *Biometrika, 48*(3-4), 419--426.
#' \doi{10.1093/biomet/48.3-4.419}
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit.
#' It refuses a multivariate `lm` ("needs a model with one response"), since
#' 0.16.0; before, it stopped with a coercion error. An `mgcv::gam()` fit is
#' tested as before 0.16.0, with smooth terms too, until a later step settles
#' which tests take such a fit; since 0.16.0 its observations are matched to the
#' rows of `data` by the row names of its model frame. Before, they were matched
#' by position, since the residuals of a `gam` carry no names, and `data` with
#' the rows of the fit in another order gave a wrong value. The null moments of
#' a `gam` with smooth terms are those of its design as an unpenalised
#' least-squares fit, with the rank of that design, as
#' [performHarrisonMcCabeTest()] takes its null distribution.
#' Which model each test accepts, and why, is set out in [test_results], under
#' *Supported models*.
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
  test_label <- "Szroeter test"
  # The residuals and weights come from the fit; `data`, when given, is matched
  # to its observations and checked (R/model_context.R). `order_by` is read
  # from the rows of `data` that hold the observations, so data taken in the
  # order of the fit must tell those rows apart when it is not a column of the
  # model frame.
  ctx <- rmodel_context(
    model, data, fn = "performSzroeterTest", test_label = test_label,
    reads = if (!missing(order_by) && is.character(order_by)) order_by else character()
  )

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  alternative <- match.arg(alternative)
  .szroeter_test(ctx, data, order_by, alternative, data_name = deparse(substitute(model)))
}

# The test behind performSzroeterTest() and the registered "szroeter", given
# the model context of the call. The registered test orders the observations
# by the fitted values of the model (`by_mean = TRUE`, with `order_by` the
# name the result gives them), which are computed from the fit
# (rcontext_mean()) and read nothing from `data`.
.szroeter_test <- function(ctx, data, order_by, alternative, data_name, by_mean = FALSE) {
  test_label <- "Szroeter test"
  model <- ctx$model
  rvalidateModelInputs(model, test_name = test_label, min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  # The ordering, one value per observation in the order of the fit: the
  # fitted values, or the variable read from the confirmed rows of `data`,
  # or without `data` from the model frame when it holds it under its own
  # name (rcontext_variables()). The registered test read the fitted values
  # with predict(), and restricted `data` to the rows of the fit before the
  # checks below, so it warned of no row the fit left out.
  if (by_mean) {
    ordering <- rcontext_mean(ctx)
  } else {
    ordering <- rcontext_variables(ctx, data, c(order_by = order_by))$order_by
    rwarn_incomplete_rows(data, model, also = order_by)
  }
  # The Pearson residuals of the fit; a fit with a zero weight is refused
  # there, as before 0.16.0.
  residuals <- rcontext_pearson(ctx)
  rstop_missing_variable(ctx, ordering, order_by, "order_by", test_label)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("szroeter", model = model, data = working_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Szroeter test")

  ord <- order(ordering)
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

  # The design of the equivalent unweighted regression, taken as
  # performHarrisonMcCabeTest() takes it: the design of the fit without the
  # columns a rank-deficient fit aliased, times sqrt(w) on a weighted fit
  # (the prior weights of an lm or a Gaussian glm), in the order of the test,
  # with the rank of the fit (rhmc_rank()). Its residual-maker gives the null
  # moments of h.
  design <- rcontext_design(ctx)
  if (!is.null(ctx$weights)) {
    design <- sqrt(ctx$weights) * design
  }
  if (nrow(design) != n) {
    stop(
      sprintf(
        "%s could not align the model matrix with the residuals (%d rows, %d residuals).",
        test_label, nrow(design), n
      ),
      call. = FALSE
    )
  }
  basis <- qr(design[ord, , drop = FALSE], tol = ctx$qr_tol)
  moments <- rszroeter_moments(basis, rhmc_rank(ctx, basis))

  # Q standardises h with its mean and variance under normal errors of
  # constant variance, given the design (rszroeter_moments()). Before 0.16.0
  # they were (n + 1) / 2 and (n^2 - 1) / (6 n), whatever the design, which on
  # a weighted fit whose leverage falls along the ordering made the test
  # liberal (0.067 at the 5% level with 40 observations). The reference is
  # still the standard normal distribution.
  q_stat <- (h_stat - moments[["mean"]]) / sqrt(moments[["var"]])

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
      data.name = data_name,
      alternative = alternative_label
    ),
    class = "htest"
  )
}

#' The null mean and variance of Szroeter's h, given the design
#'
#' With \eqn{M} the residual-maker of the design in the order of the test,
#' \eqn{A = \mathrm{diag}(1, \ldots, n)}, \eqn{k} the rank of the design and
#' \eqn{\nu = n - k}, the residuals under normal errors of constant variance
#' make \eqn{h = \sum_j \lambda_j z_j^2 / \sum_j z_j^2}, with \eqn{z_j}
#' independent standard normal and \eqn{\lambda_j} the eigenvalues of
#' \eqn{M A M} on the residual space. The ratio is independent of its
#' denominator, so its moments are ratios of moments:
#' \eqn{E h = \mathrm{tr}(MA) / \nu} and
#' \eqn{\mathrm{Var}\, h = 2 (\nu\, \mathrm{tr}(MAMA) - \mathrm{tr}(MA)^2) /
#' (\nu^2 (\nu + 2))}. With \eqn{Q_1} an orthonormal basis of the design and
#' \eqn{h_{(i)}} the leverages in the order of the test,
#' \eqn{\mathrm{tr}(MA) = n (n + 1) / 2 - \sum_i i\, h_{(i)}} and
#' \eqn{\mathrm{tr}(MAMA) = \sum_i i^2 - 2 \sum_i i^2 h_{(i)} +
#' \|Q_1' A Q_1\|_F^2}, so no eigenvalue is computed and the cost is
#' \eqn{O(n k^2)}. Without regressors the moments are \eqn{(n + 1) / 2} and
#' \eqn{(n^2 - 1) / (6 (n + 2))}.
#'
#' @param basis The QR decomposition of the design, rows in the order of the
#'   test.
#' @param k The rank of the design: the columns of `qr.Q(basis)` that span it.
#' @return A named numeric vector, `mean` and `var`.
#' @keywords internal
#' @noRd
rszroeter_moments <- function(basis, k) {
  n <- nrow(basis$qr)
  nu <- n - k
  ranks <- seq_len(n)
  q1 <- qr.Q(basis)[, seq_len(k), drop = FALSE]
  leverage <- rowSums(q1^2)
  tr_ma <- sum(ranks) - sum(ranks * leverage)
  tr_mama <- sum(ranks^2) - 2 * sum(ranks^2 * leverage) + sum(crossprod(q1, q1 * ranks)^2)
  moments <- c(mean = tr_ma / nu, var = 2 * (nu * tr_mama - tr_ma^2) / (nu^2 * (nu + 2)))
  # With one residual degree of freedom the residuals lie on one line, h is
  # the same whatever the errors and its null variance is 0. Before 0.16.0 the
  # test standardised it with the fixed moments and returned a value.
  if (nu < 2L) {
    stop(
      sprintf(
        paste0(
          "Szroeter test needs at least two residual degrees of freedom: the fit has %d ",
          "observations and %d estimable coefficients, so h takes the same value whatever ",
          "the errors and there is nothing to test."
        ),
        as.integer(n), as.integer(k)
      ),
      call. = FALSE
    )
  }
  if (!all(is.finite(moments)) || moments[["var"]] <= 0) {
    stop(
      paste0(
        "Szroeter test cannot test this fit: with its design and this ordering, h ",
        "takes the same value whatever the errors (its null variance is 0)."
      ),
      call. = FALSE
    )
  }
  moments
}
