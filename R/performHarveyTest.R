#' Perform Harvey test for multiplicative heteroscedasticity
#'
#' Applies Harvey's (1976) Lagrange multiplier test for multiplicative
#' heteroscedasticity by regressing the log of the squared residuals on a set of
#' variance regressors. Departures from homoskedasticity manifest as a
#' significant auxiliary regression.
#'
#' @param model A fitted [stats::lm] object representing the mean equation under
#'   study.
#' @param auxiliary Character scalar choosing the variance regressors \eqn{Z}.
#'   `"regressors"` (the default) uses the model's own explanatory variables,
#'   which is the specification in Harvey (1976) and the usual textbook form.
#'   `"fitted"` uses the fitted values and their square, a Cook–Weisberg-style
#'   variance model in the conditional mean; this was the behaviour of releases
#'   before 0.7.0.
#' @param studentize Logical. When `FALSE` (the default) the classical statistic
#'   \eqn{\mathrm{ESS} / (\pi^2 / 2)} is referred to a chi-squared distribution.
#'   When `TRUE` the overall F statistic of the auxiliary regression is used
#'   instead, which estimates the null variance of \eqn{\log \hat{e}^2} from the
#'   data rather than assuming the normal-error value \eqn{\pi^2 / 2}.
#'
#' @return An object of class \code{htest} with the test statistic,
#'   its degrees of freedom, and the p-value for the null hypothesis that the
#'   variance is constant.
#'
#' @details
#' Harvey (1976) models the variance as \eqn{\sigma_i^2 = \exp(z_i^\top \gamma)}
#' and tests \eqn{\gamma = 0}. Taking logarithms of the squared residuals gives
#' the auxiliary regression
#' \deqn{\log \hat{e}_i^2 = \alpha + z_i^\top \gamma + v_i.}
#' Under the null \eqn{v_i} behaves like a centred \eqn{\log \chi^2_1} variate,
#' whose variance is \eqn{\pi^2 / 2 \approx 4.9348}. The classical statistic is
#' therefore
#' \deqn{\mathrm{LM} = \frac{\mathrm{ESS}}{\pi^2 / 2} \sim \chi^2_q,}
#' with \eqn{q} the number of variance regressors and ESS the explained sum of
#' squares of the auxiliary regression.
#'
#' The `studentize = TRUE` variant replaces the fixed constant \eqn{\pi^2 / 2}
#' with the auxiliary residual mean square. Both forms are asymptotically
#' equivalent under normal errors, but the studentized form is more reliable when
#' normality is doubtful, because \eqn{\pi^2 / 2} is the null variance of
#' \eqn{\log \hat{e}^2} only when the errors are Gaussian. See the Validation
#' section for simulated size under both.
#'
#' The implementation validates the model via
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()} and applies
#' \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} to ensure a
#' sufficient sample size and variability in the variance regressors.
#'
#' @references
#' Harvey, A. C. (1976). Estimating regression models with multiplicative
#' heteroscedasticity. *Econometrica, 44*(3), 461–465.
#' <https://doi.org/10.2307/1913974>
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson. Section 9.5
#' derives the \eqn{\mathrm{ESS} / 4.9348} form of the statistic.
#'
#' @section Validation:
#' The default statistic reproduces an independent reconstruction of Harvey
#' (1976) to within `1e-8`; see `tests/testthat/test-pass-a-reference.R`.
#' Releases before 0.7.0 always used the studentized F form against the fitted
#' values and their square, a variance model that appears in neither Harvey
#' (1976) nor the standard reference implementations; see `NEWS.md`.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performHarveyTest(mod)
#'
#' # Studentized variant, which does not assume normal errors
#' performHarveyTest(mod, studentize = TRUE)
#'
#' # Variance driven by the conditional mean rather than by the regressors
#' performHarveyTest(mod, auxiliary = "fitted")
#'
#' # Compare with the Park test on the same model
#' performParkTest(mod, mtcars, "wt")
#'
#' @seealso
#' [performParkTest()] and [performGlejserTest()] implement related variance
#' function diagnostics; [performBPTest()] tests the same regressor set under an
#' additive rather than a multiplicative variance model.
performHarveyTest <- function(model,
                              auxiliary = c("regressors", "fitted"),
                              studentize = FALSE) {
  auxiliary <- match.arg(auxiliary)
  if (!is.logical(studentize) || length(studentize) != 1L || is.na(studentize)) {
    stop("`studentize` must be a single logical value.", call. = FALSE)
  }

  rvalidateModelInputs(model, test_name = "Harvey", min_obs = 15L)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("harvey", model = model, data = model_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Harvey test")

  if (auxiliary == "fitted") {
    yhat <- stats::fitted(model)
    if (stats::var(yhat) <= .Machine$double.eps) {
      std_error(
        "rassumption_violation",
        assumption = "Harvey test requires variability in fitted values"
      )
    }
    Z <- cbind(yhat = yhat, yhat_sq = yhat^2)
  } else {
    X <- stats::model.matrix(model)
    Z <- X[, colnames(X) != "(Intercept)", drop = FALSE]
    if (ncol(Z) == 0L) {
      std_error(
        "rassumption_violation",
        assumption = "Harvey test requires at least one regressor beyond the intercept"
      )
    }
    if (all(apply(Z, 2, stats::var) <= .Machine$double.eps)) {
      std_error(
        "rassumption_violation",
        assumption = "Harvey test requires variability in the variance regressors"
      )
    }
  }

  e <- stats::residuals(model)
  if (any(!is.finite(e))) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey test requires finite residuals"
    )
  }
  log_e2 <- rlog_squared_residuals(e, "Harvey test")

  aux_data <- data.frame(y_aux = log_e2, Z)
  names(aux_data) <- c("y_aux", paste0("z", seq_len(ncol(Z))))
  aux_model <- safe_lm(y_aux ~ ., data = aux_data)

  # Degrees of freedom follow the realised rank of the auxiliary design so that
  # a rank-deficient Z is not credited with the columns lm() aliased away.
  df_num <- aux_model$rank - 1L
  df_den <- aux_model$df.residual

  if (df_num < 1L) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey auxiliary regression retained no variance regressors after rank reduction"
    )
  }

  if (df_den <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Harvey auxiliary regression requires positive residual degrees of freedom"
    )
  }

  if (studentize) {
    # Estimate the null variance of log(e^2) from the auxiliary residuals rather
    # than assuming the Gaussian value pi^2 / 2.
    fit_summary <- summary(aux_model)
    R2 <- fit_summary$r.squared
    statistic <- (R2 / df_num) / ((1 - R2) / df_den)
    p_value <- stats::pf(statistic, df_num, df_den, lower.tail = FALSE)
    stat_vec <- c(F = statistic)
    param_vec <- c(df1 = df_num, df2 = df_den)
    method <- "Harvey test for multiplicative heteroscedasticity (studentized)"
  } else {
    # Harvey (1976): LM = ESS / Var(log chi^2_1), with Var(log chi^2_1) = pi^2 / 2.
    aux_fit <- stats::fitted(aux_model)
    ess <- sum((aux_fit - mean(aux_fit))^2)
    statistic <- ess / (pi^2 / 2)
    p_value <- stats::pchisq(statistic, df_num, lower.tail = FALSE)
    stat_vec <- c("X-squared" = statistic)
    param_vec <- c(df = df_num)
    method <- "Harvey test for multiplicative heteroscedasticity"
  }

  structure(
    list(
      statistic = stat_vec,
      parameter = param_vec,
      p.value = p_value,
      method = method,
      data.name = paste0(
        deparse(stats::formula(model)),
        "; variance regressors: ",
        if (auxiliary == "fitted") "fitted values and their square" else "model regressors"
      ),
      alternative = "error variance is a multiplicative function of the variance regressors"
    ),
    class = "htest"
  )
}
