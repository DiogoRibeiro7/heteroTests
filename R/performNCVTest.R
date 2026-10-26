#' Perform the non-constant variance (NCV) score test
#'
#' Implements the Cook–Weisberg score test for non-constant error variance, the
#' diagnostic returned by `car::ncvTest()`. The squared residuals, scaled by the
#' maximum-likelihood error variance, are regressed on a variance model — by
#' default the fitted values — and half the explained sum of squares is referred
#' to a chi-squared distribution.
#'
#' @param model A fitted [stats::lm] object whose residuals will be tested for
#'   heteroscedasticity.
#' @param data Optional data frame the model was fitted to, in which
#'   `var_formula` is evaluated, as in [performCookWeisbergTest()]. When
#'   `NULL`, the default, the model frame of `model` is used.
#' @param var_formula Optional one-sided [stats::formula] giving the variance
#'   model, evaluated in `data` (for example `~ x1 + x2`).
#'   When `NULL` (the default) the fitted values are used as the single variance
#'   regressor, matching the default behaviour of `car::ncvTest()`. Before
#'   0.15.0 `var_formula` was the second argument: a formula passed there by
#'   position is still read as `var_formula`, with a warning; that form stops
#'   working in 1.0.0.
#'
#' @return An object of class \code{htest} containing the chi-squared
#'   score statistic, its degrees of freedom, and the upper-tail p-value.
#'
#' @details
#' Let \eqn{\hat{e}_i} denote the OLS residuals and
#' \eqn{\hat{\sigma}^2 = \sum_i \hat{e}_i^2 / n} the maximum-likelihood variance
#' estimate. The test regresses the scaled squared residuals
#' \eqn{u_i = \hat{e}_i^2 / \hat{\sigma}^2} on the variance regressors \eqn{Z} and
#' computes
#' \deqn{S = \tfrac{1}{2} \sum_i (\hat{u}_i - \bar{u})^2,}
#' the explained sum of squares of that auxiliary regression divided by two.
#' Under homoskedasticity and normal errors \eqn{S} is asymptotically chi-squared
#' with degrees of freedom equal to the number of variance regressors. This is the
#' score (Lagrange multiplier) form of the Breusch–Pagan statistic specialised by
#' Cook and Weisberg (1983); with `var_formula = NULL` it is the statistic
#' reported by `car::ncvTest()` and by Stata's `estat hettest`.
#'
#' Because the divisor \eqn{2} is the null variance of \eqn{\hat{e}^2 / \sigma^2}
#' under normality, the test is sensitive to non-normal errors. Use
#' [performKoenkerTest()] for the studentized variant, which replaces that
#' constant with a consistent estimate and is robust to non-normal kurtosis.
#'
#' @section Weighted fits:
#' On a fit with weights the test is computed from the Pearson residuals
#' \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see
#' the section on weighted fits in [performKoenkerTest()], which also covers
#' weights estimated by [fitWLS()].
#'
#' @references
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. <https://doi.org/10.1093/biomet/70.1.1>
#'
#' Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
#' (3rd ed.). Sage. Section 8.5 introduces the NCV test.
#'
#' @section Validation:
#' Reproduces `car::ncvTest()` to within `1e-8` for both the default and the
#' `var_formula` forms; see `tests/testthat/test-pass-a-reference.R`.
#' Releases before 0.7.0 regressed the *absolute* residuals on the fitted values
#' and reported a t statistic, which is a Glejser-type test rather than the
#' Cook–Weisberg score test named in the documentation; see `NEWS.md`.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performNCVTest(mod)
#'
#' # Score test against a specific variance model rather than the fitted values
#' performNCVTest(mod, var_formula = ~wt)
#'
#' # Detect monotonic heteroscedasticity in simulated data
#' set.seed(867)
#' x <- runif(160)
#' y <- 5 + 2 * x + rnorm(160, sd = 0.3 + 0.7 * x)
#' df <- data.frame(y, x)
#' performNCVTest(lm(y ~ x, data = df))
#'
#' @seealso
#' [performCookWeisbergTest()] for the fitted-value special case,
#' [performKoenkerTest()] for the studentized variant, and [performBPTest()] for
#' the Breusch–Pagan test against the full regressor set.
#' @section Deprecated:
#' `performNCVTest()` is deprecated since 0.14.0: use
#' [performCookWeisbergTest()], which gained `var_formula` and is the one entry
#' point for the Cook–Weisberg score test. `performNCVTest(model, var_formula)`
#' with a variance formula returns what `performCookWeisbergTest(model,
#' var_formula)` returns; without one the two return the same statistic,
#' degrees of freedom and p-value, and `performCookWeisbergTest()` labels the
#' result as the Cook–Weisberg test. The old name still works, returns what it
#' returned and warns.
performNCVTest <- function(model, data = NULL, var_formula = NULL) {
  rrefuse_non_gaussian_glm(model, "performNCVTest")
  .Deprecated("performCookWeisbergTest", old = "performNCVTest")
  if (inherits(data, "formula") && missing(var_formula)) {
    rwarn_positional_call(
      "performNCVTest", substitute(model), list(var_formula = substitute(data))
    )
    var_formula <- data
    data <- NULL
  }
  ctx <- rcontext_second_formula(model, data, list(var_formula), "performNCVTest", "NCV")
  .ncv_test(model, var_formula, data, ctx)
}

# The implementation behind the deprecated performNCVTest(), the "ncv"
# registry name and performCookWeisbergTest(); unchanged in 0.14.0. Since
# 0.15.0 `var_formula` can be evaluated in `data` rather than in the model
# frame; with `data = NULL` the computation is the one of 0.14.0.
#
# Since 0.16.0 the callers build the model context (`ctx`) first, and the fit
# must reproduce itself (rvalidate_model_context()). The variables of
# `var_formula` are read at the rows of `data` the alignment confirmed, in the
# order of the fit, or without `data` from the model frame, where it holds
# them under their own names, and never from the workspace
# (rcontext_formula_rows()): before, with `data = NULL`, a variable that
# neither held was found in the environment of the formula (R3, decision 9).
# Without a context, as from the internal benchmarks, the residuals and
# fitted values are read from `model`, which is all the test without
# `var_formula` reads.
.ncv_test <- function(model, var_formula = NULL, data = NULL, ctx = NULL) {
  rvalidateModelInputs(model, test_name = "NCV", min_obs = 10L, allow_aliased = TRUE)
  if (!is.null(ctx)) {
    rvalidate_model_context(ctx)
  }

  ht_log("INFO", "Running NCV score test")

  e <- rpearson_residuals(model)
  n <- length(e)

  # Cook and Weisberg scale by the ML variance estimate, i.e. RSS / n rather
  # than RSS / (n - p); car::ncvTest() does the same.
  sigma2_ml <- sum(e^2) / n
  if (!is.finite(sigma2_ml) || sigma2_ml <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "NCV test requires a positive residual variance"
    )
  }
  u <- e^2 / sigma2_ml

  if (is.null(var_formula)) {
    Z <- cbind(fitted.values = stats::fitted(model))
    var_label <- "fitted values"
  } else {
    if (!inherits(var_formula, "formula") || length(var_formula) != 2L) {
      stop("`var_formula` must be a one-sided formula, e.g. ~ x1 + x2.", call. = FALSE)
    }
    if (is.null(ctx)) {
      ctx <- rcontext_second_formula(model, data, list(var_formula), "performNCVTest", "NCV")
    }
    # The model frame without `data`, stripped of the terms of the mean model,
    # which model.matrix() would otherwise check the variance formula against;
    # with `data`, the rows the alignment confirmed, in the order of the fit.
    plain <- rcontext_formula_rows(ctx, data, var_formula)
    if (!is.null(data)) {
      # As before 0.16.0: the checks of the model for "Cook-Weisberg test",
      # whose warnings name it, the warning on the rows of `data` with a
      # missing value in a variable of `var_formula`, and the refusal of such a
      # value at an observation of the fit, which now names the variable
      # (decision 8). Nothing is dropped before the rows are matched.
      rvalidateModelInputs(model, test_name = "Cook-Weisberg test", min_obs = 10L,
                           allow_aliased = TRUE)
      read <- intersect(setdiff(all.vars(var_formula), "."), names(data))
      rwarn_incomplete_rows(data, NULL, also = read)
      for (v in read) {
        rstop_missing_variable(ctx, plain[[v]], v, "var_formula", "Cook-Weisberg test")
      }
    }
    aux_frame <- stats::model.frame(var_formula, data = plain)
    Z <- stats::model.matrix(var_formula, data = aux_frame)
    Z <- Z[, colnames(Z) != "(Intercept)", drop = FALSE]
    if (ncol(Z) == 0L) {
      stop("`var_formula` must supply at least one variance regressor.", call. = FALSE)
    }
    if (nrow(Z) != n) {
      stop(
        sprintf(
          "NCV test could not align `var_formula` with the model residuals (expected %d rows, got %d).",
          n, nrow(Z)
        ),
        call. = FALSE
      )
    }
    var_label <- paste(all.vars(var_formula), collapse = ", ")
  }

  if (all(apply(Z, 2, stats::var) <= .Machine$double.eps)) {
    .std_error(
      "rassumption_violation",
      assumption = "NCV test requires variability in the variance regressors"
    )
  }

  aux_data <- data.frame(y_aux = u, Z)
  names(aux_data) <- c("y_aux", paste0("z", seq_len(ncol(Z))))
  aux_model <- safe_lm(y_aux ~ ., data = aux_data)

  aux_fit <- stats::fitted(aux_model)
  test_statistic <- sum((aux_fit - mean(aux_fit))^2) / 2

  # Take the degrees of freedom from the realised rank of the auxiliary design
  # (minus the intercept) so a rank-deficient variance model is not credited
  # with degrees of freedom for columns that lm() aliased away.
  df <- aux_model$rank - 1L
  if (df < 1L) {
    .std_error(
      "rassumption_violation",
      assumption = "NCV auxiliary regression retained no variance regressors after rank reduction"
    )
  }
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Cook-Weisberg score test for non-constant variance",
      data.name = paste0(
        deparse(stats::formula(model)), "; variance model: ", var_label
      ),
      alternative = "error variance depends on the variance model"
    ),
    class = "htest"
  )
}
