#' Test the functional form of the error variance
#'
#' Tests whether the error variance follows a stated parametric function of
#' stated variables, \eqn{\sigma_i^2 = h(z_i; \gamma)}. Every other test in the
#' package has constant variance as its null hypothesis, so it can say that the
#' variance moves with some variables but not whether an exponential or a power
#' function of them describes it. Here the variance function is the null
#' hypothesis, and rejecting it means that the chosen shape is inadequate.
#'
#' @param model A fitted [stats::lm] object for the mean equation, or the result
#'   of [fitWLS()]. In the second case the variance function that was fitted is
#'   the one tested, and `var_formula` and `form` must be left at their
#'   defaults.
#' @param data Optional [base::data.frame] holding the variables named in
#'   `var_formula` or `against`. It is needed only when they are not in the
#'   model frame, and is matched to the fitted rows by row name.
#' @param var_formula One-sided formula naming the variance regressors
#'   \eqn{z_i}, for example `~ x`. `NULL`, the default, uses the regressors of
#'   `model`.
#' @param form Character scalar giving the variance function under the null:
#'   `"exponential"` (the default) or `"power"`. They are defined in the section
#'   on variance functions of [fitWLS()].
#' @param against The terms the variance function is tested against. The
#'   default, `"squares"`, adds the squares and pairwise products of the
#'   variance regressors, on the scale the form uses (logarithms for the power
#'   form). A one-sided formula such as `~ w` or `~ I(x^3)` names the added
#'   terms instead, which tests the variance function against a variable it
#'   leaves out or against a particular departure.
#'
#' @return An object of class \code{htest} with the F statistic, its numerator
#'   and denominator degrees of freedom, and the p-value for the null hypothesis
#'   that the variance function is correctly specified. `estimate` holds the
#'   fitted coefficients of the variance function: slopes of the log-variance
#'   for the exponential form and exponents for the power form. The element
#'   `variance_function` records the form, the variance regressors and the
#'   added terms that entered the test.
#'
#' @details
#' The procedure has three steps.
#' \enumerate{
#'   \item The variance function is fitted to the residuals of `model` and the
#'     model is refitted by weighted least squares, exactly as [fitWLS()] does.
#'   \item The squared standardized residuals
#'     \eqn{r_i = \tilde{e}_i^2 / \hat\sigma_i^2} of the weighted fit are
#'     formed. If the variance function is right their mean does not depend on
#'     anything.
#'   \item \eqn{r_i} is regressed on a constant, on
#'     \eqn{g_i = \hat\sigma_i^{-2}\, \partial \sigma_i^2 / \partial \gamma},
#'     and on the added terms \eqn{a_i}. The statistic is the F test that the
#'     coefficients on \eqn{a_i} are zero.
#' }
#' For the exponential form \eqn{g_i} is \eqn{z_i}, and for the power form it
#' is \eqn{\log z_i}. The regression is the score direction of the variance
#' function extended by the added terms, so the test is a studentized Lagrange
#' multiplier test of the stated form against that extension.
#'
#' Step 3 keeps \eqn{g_i} in the regression for a reason. Estimating
#' \eqn{\gamma} fits the variance along \eqn{g_i}, so those directions say
#' little about the form: a Breusch--Pagan or Koenker test of the weighted fit
#' on the same variance regressors rejects a wrong form about as often as the
#' right one. The information is in the terms outside the fitted variance
#' function. Conditioning on \eqn{g_i} is also what makes the null
#' distribution free of the error in \eqn{\hat\gamma} to first order, which is
#' the regression-based construction of Wooldridge (1990, 1991).
#'
#' @section Interpretation and caveats:
#' A rejection says that the added terms move the variance beyond what the
#' stated function allows. A non-rejection says only that those terms do not;
#' it does not establish the form. Two functions that are close over the
#' observed range of \eqn{z_i} cannot be told apart, and the test will then
#' accept both: a variance that is linear in a regressor is accepted as
#' exponential or as a power when the regressor varies over a short range.
#' Running the test for several forms is a comparison of fits, and choosing
#' the form with the largest p-value is not a test.
#'
#' The test judges the shape, not the fitted coefficients. Because it
#' conditions on \eqn{g_i}, it does not respond to weights that are off along
#' the variance regressors themselves. A test of constant variance on the
#' weighted fit is sensitive to that, with the reservations about its level
#' described under weighted fits in [performKoenkerTest()].
#'
#' The F reference distribution assumes that the standardized errors have a
#' constant fourth moment, the assumption behind the Koenker test. It does not
#' assume normality.
#'
#' With \eqn{q} variance regressors the default adds up to
#' \eqn{q (q + 1) / 2} terms. When that is large relative to the sample, name
#' the variance regressors with `var_formula` or the added terms with
#' `against`.
#'
#' @section Validation:
#' No reference implementation exists, so the statistic is checked against a
#' reconstruction from its definition to `1e-8`
#' (`tests/testthat/test-variance-form.R`) and by simulation. Over 24 designs
#' in which the form under test was right (both forms, constant and
#' heteroscedastic variances, 50 to 400 observations, Gaussian and
#' \eqn{t_5} errors, 5000 replications each) the rejection rate at the 5%
#' level ran from 4.2% to 5.7%. Power against a log-variance that is
#' quadratic in a regressor was 98% at 150 observations. Against the other
#' log-linear form it was 22% at 150 observations and 74% at 400. See
#' `inst/validation/README.md`.
#'
#' @references
#' Wooldridge, J. M. (1990). A unified approach to robust, regression-based
#' specification tests. *Econometric Theory, 6*(1), 17--43.
#' \doi{10.1017/S0266466600004898}
#'
#' Wooldridge, J. M. (1991). On the application of robust, regression-based
#' diagnostics to models of conditional means and conditional variances.
#' *Journal of Econometrics, 47*(1), 5--46.
#' \doi{10.1016/0304-4076(91)90076-P}
#'
#' Harvey, A. C. (1976). Estimating regression models with multiplicative
#' heteroscedasticity. *Econometrica, 44*(3), 461--465.
#' \doi{10.2307/1913974}
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107--112.
#' \doi{10.1016/0304-4076(81)90062-2}
#'
#' Carroll, R. J., & Ruppert, D. (1988). *Transformation and Weighting in
#' Regression*. Chapman & Hall. Chapter 3 treats the estimation and checking of
#' variance functions.
#'
#' @examples
#' # Standard deviation proportional to x, so the variance is a power of x
#' set.seed(2026)
#' n <- 300
#' sim <- data.frame(x = runif(n, 1, 5))
#' sim$y <- 1 + 2 * sim$x + sim$x * rnorm(n)
#' mod <- lm(y ~ x, data = sim)
#'
#' # The power form is the true one; estimate is the fitted exponent
#' performVarianceFormTest(mod, form = "power")
#'
#' # The exponential form in x is not
#' performVarianceFormTest(mod, form = "exponential")
#'
#' # Test the variance function of a weighted fit directly
#' wls <- fitWLS(mod, form = "power")
#' performVarianceFormTest(wls)
#'
#' # Test a variance function against one particular added term
#' performVarianceFormTest(mod, form = "exponential", against = ~ log(x))
#'
#' @seealso
#' [fitWLS()] fits the variance functions tested here. [performHarveyTest()]
#' and [performParkTest()] test constant variance against the exponential and
#' the power form respectively.
#' @export
performVarianceFormTest <- function(model, data = NULL, var_formula = NULL,
                                    form = c("exponential", "power"),
                                    against = "squares") {
  if (!inherits(model, "lm") || inherits(model, "glm")) {
    stop("`model` must be a fitted `lm` object.", call. = FALSE)
  }
  rvalidateModelInputs(model, test_name = "Variance form test", min_obs = 30L)
  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("variance_form", model = model, data = model_data)
  rprocessValidationResult(requirements)

  resolved <- if (missing(form)) {
    rresolve_variance_fit(model, data, var_formula)
  } else {
    rresolve_variance_fit(model, data, var_formula, match.arg(form))
  }
  variance <- resolved$variance
  weighted <- resolved$weighted

  ht_log("INFO", sprintf("Running variance-function specification test (%s form)", variance$form))

  # Squared standardized residuals of the weighted fit. The weights are the
  # inverse fitted variances up to a common factor, which the F statistic is
  # invariant to.
  r <- weighted$weights * weighted$residuals^2
  if (!all(is.finite(r)) || stats::var(r) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Variance form test requires variation in the squared standardized residuals"
    )
  }

  # The variance regressors are the score direction of a log-linear variance
  # function, d sigma^2 / d gamma divided by sigma^2. They stay in the
  # regression and the added terms are tested beyond them.
  added <- rvariance_added_terms(weighted, data, against, variance)
  restricted <- cbind("(Intercept)" = 1, variance$regressors)
  test <- radded_terms_f_test(r, restricted, added$matrix)

  # The constant of a log-variance regression is shifted by E[log(e^2)], so it
  # does not estimate the constant of the variance function and is left out.
  estimate <- variance$coefficients
  estimate <- estimate[!is.na(estimate) & names(estimate) != "(Intercept)"]

  structure(
    list(
      statistic = c(F = test$statistic),
      parameter = c(df1 = test$df1, df2 = test$df2),
      p.value = stats::pf(test$statistic, test$df1, test$df2, lower.tail = FALSE),
      estimate = estimate,
      method = sprintf(
        "Specification test of the variance function (%s form)", variance$form
      ),
      data.name = paste0(
        paste(deparse(stats::formula(model)), collapse = " "),
        "; variance regressors: ", variance$label,
        "; against: ", added$label
      ),
      alternative = "the variance function is misspecified",
      variance_function = list(
        form = variance$form,
        regressors = colnames(variance$regressors),
        added_terms = test$retained
      )
    ),
    class = "htest"
  )
}

#' Find the variance function to test and the weighted fit that used it
#'
#' A [fitWLS()] result carries its variance function, which is then the one
#' tested. Any other model is refitted with the variance function the caller
#' described.
#'
#' @param model A fitted `lm`.
#' @param data,var_formula As in [performVarianceFormTest()].
#' @param form The form the caller asked for, or `NULL` when it was left at its
#'   default.
#' @return A list with `variance`, the fitted variance function, and
#'   `weighted`, the weighted fit.
#' @keywords internal
#' @noRd
rresolve_variance_fit <- function(model, data, var_formula, form = NULL) {
  variance <- attr(model, "variance_function")
  if (!is.null(variance)) {
    if (!is.null(var_formula) || !is.null(form)) {
      stop(
        "`model` is a fitWLS() fit and carries the variance function to test. ",
        "Leave `var_formula` and `form` unset, or pass the unweighted model ",
        "to test a different variance function.",
        call. = FALSE
      )
    }
    weighted <- model
  } else {
    if (!is.null(rprior_weights(model))) {
      stop(
        "`model` was fitted with weights that did not come from fitWLS(), so ",
        "there is no variance function to re-estimate. Pass the unweighted ",
        "model, or check the weights with a test of constant variance such as ",
        "performKoenkerTest(): on a weighted fit it uses the Pearson residuals.",
        call. = FALSE
      )
    }
    if (is.null(form)) {
      form <- "exponential"
    }
    # warn = FALSE: an unusable fit is an error below, not a fallback.
    variance <- rfit_variance_function(model, data, var_formula, form, warn = FALSE)
    weighted <- rweighted_refit(model, variance)
  }

  if (!isTRUE(variance$usable)) {
    stop(
      sprintf(
        "The %s variance function could not be tested: %s.",
        variance$form, variance$reason
      ),
      call. = FALSE
    )
  }
  list(variance = variance, weighted = weighted)
}

#' Terms a variance function is tested against
#'
#' @param weighted The weighted fit.
#' @param data,against As in [performVarianceFormTest()].
#' @param variance The fitted variance function.
#' @return A list with `matrix`, the added terms, and `label`.
#' @keywords internal
#' @noRd
rvariance_added_terms <- function(weighted, data, against, variance) {
  if (is.character(against) && length(against) == 1L && identical(against, "squares")) {
    added <- rsquares_and_products(variance$regressors)
    label <- "squares and products of the variance regressors"
  } else if (inherits(against, "formula")) {
    named <- rvariance_regressors(weighted, data, against, arg = "against")
    added <- named$matrix
    label <- named$label
  } else {
    stop("`against` must be \"squares\" or a one-sided formula.", call. = FALSE)
  }
  list(matrix = added, label = label)
}

#' F test of terms added to a regression
#'
#' @param r Response.
#' @param restricted Design matrix under the null, including its constant.
#' @param added Matrix of added terms.
#' @return A list with `statistic`, `df1`, `df2` and `retained`, the names of
#'   the added terms that were not aliased.
#' @keywords internal
#' @noRd
radded_terms_f_test <- function(r, restricted, added) {
  fit0 <- stats::lm.fit(restricted, r)
  fit1 <- stats::lm.fit(cbind(restricted, added), r)

  # Degrees of freedom follow the realised ranks, so an added term that the
  # variance function already spans (the square of a dummy variable, say) is
  # not counted.
  df1 <- fit1$rank - fit0$rank
  df2 <- length(r) - fit1$rank
  if (df1 < 1L) {
    stop(
      "The added terms are collinear with the variance function, so there is ",
      "nothing to test it against. Name other terms with `against`.",
      call. = FALSE
    )
  }
  if (df2 < 1L) {
    std_error(
      "rassumption_violation",
      assumption = paste(
        "Variance form test requires more observations than terms; name fewer",
        "variance regressors with `var_formula` or fewer added terms with `against`"
      )
    )
  }

  rss0 <- sum(fit0$residuals^2)
  rss1 <- sum(fit1$residuals^2)
  if (!is.finite(rss1) || rss1 <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Variance form test requires residual variation in its auxiliary regression"
    )
  }

  retained <- names(fit1$coefficients)[!is.na(fit1$coefficients)]
  list(
    statistic = ((rss0 - rss1) / df1) / (rss1 / df2),
    df1 = df1,
    df2 = df2,
    retained = setdiff(retained, colnames(restricted))
  )
}

#' Squares and pairwise products of the columns of a matrix
#'
#' @param Z Numeric matrix with column names.
#' @return A matrix with one column per square and per pairwise product.
#' @keywords internal
#' @noRd
rsquares_and_products <- function(Z) {
  q <- ncol(Z)
  nm <- colnames(Z)
  if (is.null(nm)) {
    nm <- paste0("z", seq_len(q))
  }
  pairs <- which(upper.tri(diag(q), diag = TRUE), arr.ind = TRUE)
  out <- Z[, pairs[, 1L], drop = FALSE] * Z[, pairs[, 2L], drop = FALSE]
  colnames(out) <- ifelse(
    pairs[, 1L] == pairs[, 2L],
    paste0(nm[pairs[, 1L]], "^2"),
    paste0(nm[pairs[, 1L]], ":", nm[pairs[, 2L]])
  )
  out
}
