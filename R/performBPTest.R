#' Perform Breusch–Pagan test for heteroscedasticity
#'
#' Implements the Breusch and Pagan (1979) Lagrange Multiplier test for detecting
#' heteroscedasticity in linear regression models. The statistic is based on
#' regressing the squared residuals on the original regressors and assessing
#' whether the fitted values explain systematic variance in the errors.
#'
#' @param model A fitted [stats::lm] object describing the mean structure whose
#'   residual variance is to be assessed.
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
#' @return An object of class \code{htest} with the LM statistic, degrees of
#'   freedom, and p-value for the null hypothesis of constant error variance.
#'
#' @details
#' The routine follows the original Breusch–Pagan (1979) derivation. If
#' \eqn{\hat{e}} denotes the residual vector, \eqn{\hat{\sigma}^2 = \sum
#' \hat{e}_i^2 / n} the maximum-likelihood variance estimate, and \eqn{Z} the
#' design matrix without the intercept, the test regresses the scaled squared
#' residuals \eqn{\hat{e}_i^2 / \hat{\sigma}^2 - 1} on \eqn{Z} and forms half the
#' explained sum of squares. Under the null of homoskedasticity *and normal
#' disturbances*, this statistic converges to a chi-squared distribution with
#' degrees of freedom equal to the number of regressors in \eqn{Z}. This matches
#' `lmtest::bptest(..., studentize = FALSE)`. The
#' implementation integrates the validation helpers to:
#' \enumerate{
#'   \item ensure the fitted model has at least 15 observations and finite
#'     residuals via \link[=rvalidateModelInputs]{rvalidateModelInputs()};
#'   \item read the response, design, residuals and weights from the fit (the
#'     model context, since 0.16.0), and match the rows of `data`, when it is
#'     given, to the observations of the fit;
#'   \item verify auxiliary regression conditions (sample size, rank, residual
#'     variation) with \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} and bespoke checks; and
#'   \item guard against alignment issues between residuals and the supplied data
#'     prior to fitting the auxiliary model.
#' }
#'
#' A significant statistic indicates that the error variance varies with the
#' regressors. Because the classical statistic assumes normal disturbances,
#' consider the studentized \eqn{n R^2} variant [performKoenkerTest()] when
#' normality is in doubt.
#'
#' Like White's test, it is not valid with fixed effects for small groups, a
#' factor with few observations per level, and over-rejects on homoscedastic
#' data, the more the smaller the groups and the more groups there are: in
#' 2,000 draws of `lm(y ~ x + subject)` with normal errors and 30 subjects it
#' rejected at the 5\% level in 38\% with three observations of each, 26\%
#' with four and 15\% with eight, and with 100 subjects of eight in 24\% (see
#' *Too few observations* in [performWhiteTest()]).
#'
#' The test is computed in memory whatever the size of the data. Up to 0.15.0
#' it switched to [performBPTestStreaming()] on data above 100 MB; call that
#' function to stream.
#'
#' @section Weighted fits:
#' On a fit with weights the test is computed from the Pearson residuals
#' \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see
#' the section on weighted fits in [performKoenkerTest()], which also covers
#' weights estimated by [fitWLS()].
#'
#' @references
#' Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity
#' and random coefficient variation. *Econometrica, 47*(5), 1287–1294.
#' <https://doi.org/10.2307/1911963>
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' \doi{10.1016/0304-4076(81)90062-2}
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performBPTest(mod, mtcars)
#'
#' # Compare behaviour on simulated homoskedastic and heteroskedastic samples
#' set.seed(42)
#' n <- 150
#' x <- runif(n)
#' y_homo <- 2 + 3 * x + rnorm(n, sd = 0.3)
#' y_hetero <- 2 + 3 * x + rnorm(n, sd = 0.3 + x)
#' df_h <- data.frame(y = y_homo, x = x)
#' df_het <- data.frame(y = y_hetero, x = x)
#' performBPTest(lm(y ~ x, data = df_h), df_h)
#' performBPTest(lm(y ~ x, data = df_het), df_het)
#'
#' @seealso
#' [performKoenkerTest()] for the studentized
#' \eqn{n R^2} LM statistic that drops the normality assumption, and
#' \link[=performBPTestRobust]{performBPTestRobust()} or [performWhiteTest()] for complementary diagnostics.
performBPTest <- function(model, data = NULL) {
  test_label <- "Breusch-Pagan test"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performBPTest", test_label = test_label)
  rvalidateModelInputs(model, test_name = "Breusch-Pagan", min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  handle_validation_result <- function(result) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      stop(paste(unique(result$messages), collapse = "\n"), call. = FALSE)
    }
    invisible(result)
  }

  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("breusch_pagan", model = model, data = working_data)
  handle_validation_result(requirements)

  # A warning above 50 MB. Up to 0.15.0 the test switched to
  # performBPTestStreaming() above 100 MB; the streaming tests run only when
  # they are called, or asked for in runHeteroTests() (?runHeteroTests).
  tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NULL
  )
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "This may take some time to compute."
    )
  }

  ht_log("INFO", "Running Breusch-Pagan test")

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires residual variation greater than numerical precision"
    )
  }

  # The design of the fit without the columns a rank-deficient fit aliased, so
  # that the degrees of freedom are those of the estimable part of the design.
  X_full <- rcontext_design(ctx)
  if (nrow(X_full) != length(residuals)) {
    stop(
      sprintf(
        "%s could not align the design matrix with model residuals (expected %d rows, got %d).",
        test_label,
        length(residuals),
        nrow(X_full)
      ),
      call. = FALSE
    )
  }

  if (colnames(X_full)[1] == "(Intercept)") {
    predictors <- X_full[, -1, drop = FALSE]
  } else {
    predictors <- X_full
  }

  if (ncol(predictors) == 0) {
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  aux_data <- as.data.frame(predictors)
  n <- length(residuals)

  if (n <= (ncol(aux_data) + 1L)) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  # Classical Breusch-Pagan (1979): regress the scaled squared residuals
  # g_i = e_i^2 / sigma^2 - 1 (with sigma^2 the ML variance estimate) on the
  # regressors and take half the explained sum of squares. The statistic is
  # chi-squared(df) under homoskedasticity *and* normal disturbances. For the
  # studentized form that drops the normality assumption use performKoenkerTest().
  sigma2 <- sum(residuals^2) / n
  scaled_sq <- residuals^2 / sigma2 - 1
  aux_model <- safe_lm(scaled_sq ~ ., data = aux_data)
  fitted_aux <- stats::fitted(aux_model)
  test_statistic <- 0.5 * sum(fitted_aux^2)
  df <- ncol(aux_data)
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}

#' @section Deprecated name: `performBreuschPaganTest()` is deprecated since
#'   0.14.0. It was the same function as `performBPTest()` under a second name;
#'   it still works, returns what `performBPTest()` returns and warns. Use
#'   `performBPTest()`.
#' @rdname performBPTest
#' @export
performBreuschPaganTest <- function(model, data = NULL) {
  .Deprecated("performBPTest", old = "performBreuschPaganTest")
  performBPTest(model, data)
}
