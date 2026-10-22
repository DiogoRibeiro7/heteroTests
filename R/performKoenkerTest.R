#' Perform Koenker studentized Breusch–Pagan test
#'
#' Evaluates Koenker's (1981) studentized variant of the Breusch–Pagan Lagrange
#' Multiplier test. The auxiliary regression of the squared residuals on the
#' regressors yields the \eqn{n R^2} statistic, which the studentization makes
#' robust to non-normal error distributions while still assessing whether the
#' error variance depends on the regressors.
#'
#' @param model A fitted [stats::lm] object supplying residuals and fitted values
#'   for the diagnostic.
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
#' @return An object of class \code{htest} with the chi-squared statistic,
#'   degrees of freedom, and p-value for the null hypothesis of homoskedasticity.
#'
#' @details
#' Koenker's extension keeps the squared residuals of the Breusch–Pagan test but
#' replaces the normality-based scaling factor \eqn{2\hat{\sigma}^4} with the
#' empirical variance of the squared residuals. The resulting \eqn{n R^2}
#' statistic therefore retains validity under general forms of non-normality
#' (the deprecated [performStudentizedBPTest()] computes the same statistic,
#' except on a fit returned by [fitWLS()]). This implementation
#' integrates the validation helpers
#' to ensure: (i) sufficient sample size and finite residuals via
#' \link[=rvalidateModelInputs]{rvalidateModelInputs()}, (ii) the response, design, residuals and
#' weights of the fit itself, with the rows of `data` matched to its observations (the model
#' context, since 0.16.0), (iii) satisfaction of Koenker-specific
#' requirements using \link[=rvalidateTestRequirements]{rvalidateTestRequirements()}, and (iv) a full-rank design
#' matrix for the auxiliary regression.
#'
#' Rejecting the null indicates that the squared residuals—and hence the error
#' variance—vary systematically with the predictors. The statistic simplifies to
#' \eqn{n R^2} from the auxiliary regression and is compared against a
#' chi-squared distribution with degrees of freedom equal to the number of
#' regressors.
#'
#' Like White's test, it is not valid with fixed effects for small groups, a
#' factor with few observations per level, and over-rejects on homoscedastic
#' data, the more the smaller the groups and the more groups there are: in
#' 2,000 draws of `lm(y ~ x + subject)` with 30 subjects it rejected at the
#' 5\% level in 54\% with three observations of each, 22\% with four and 12\%
#' with eight, and with 100 subjects of eight in 22\% (see *Too few
#' observations* in [performWhiteTest()]).
#'
#' @section Weighted fits:
#' `lm(..., weights = w)` states that the error variance is
#' \eqn{\sigma^2 / w_i}. On such a fit the test is computed from the Pearson
#' residuals \eqn{\sqrt{w_i}\, e_i}, the residuals of the equivalent unweighted
#' regression, which have constant variance when the weights are right. The
#' null hypothesis is then that the weights are adequate; the variance
#' regressors stay on their original scale. Releases before 0.12.0 used the raw
#' residuals, whose variance differs across observations by assumption, so a
#' correctly weighted fit was rejected as often as an unweighted one. A fit
#' with a zero weight is refused.
#'
#' The reference distribution takes the weights as known. When they were
#' estimated from the same data, as by [fitWLS()], it is only approximate, and
#' not more accurate in larger samples. With a correctly specified variance
#' function and normal errors, the Koenker test rejected 8% of the time at the
#' 5% level with 150 observations and 11% with 600, the White test 6% and 8%,
#' and the Harvey test never, because it repeats the regression the weights
#' were estimated from (`inst/validation/weighted-fits-size.csv`).
#' [performVarianceFormTest()] accounts for the estimation, held its level in
#' the same designs, and is the test to use on a `fitWLS()` fit.
#'
#' @references
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' \doi{10.1016/0304-4076(81)90062-2}
#'
#' Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in
#' regression. *Biometrika, 70*(1), 1–10. \doi{10.1093/biomet/70.1.1}
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performKoenkerTest(mod, mtcars)
#'
#' # Simulated data with variance that scales with the regressor
#' set.seed(99)
#' x <- runif(120)
#' y <- 1 + 2 * x + rnorm(120, sd = 0.2 + 0.8 * x)
#' df <- data.frame(y, x)
#' performKoenkerTest(lm(y ~ x, data = df), df)
#'
#' @seealso
#' [performBPTest()] for the classical LM statistic using squared residuals,
#' and \link[=performBPTestRobust]{performBPTestRobust()} for bootstrap-enhanced reporting.
performKoenkerTest <- function(model, data = NULL) {
  test_label <- "Koenker studentized Breusch-Pagan test"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performKoenkerTest", test_label = test_label)

  rvalidateModelInputs(model, test_name = "Koenker", min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("koenker", model = model, data = working_data)
  rprocessValidationResult(requirements)

  size_mb <- tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NA_real_
  )
  if (!is.na(size_mb) && size_mb > 100) {
    ht_log("INFO", sprintf("Switching to streaming Koenker test for dataset of %.1f MB", size_mb))
    adaptive_chunk <- max(1000L, min(25000L, as.integer(nrow(working_data) / 5L)))
    adaptive_chunk <- min(adaptive_chunk, nrow(working_data))
    return(performKoenkerTestStreaming(
      model,
      working_data,
      chunk_size = adaptive_chunk,
      progress = FALSE
    ))
  }
  if (nrow(working_data) > 10000) {
    message(
      "Large dataset (", nrow(working_data), " observations). ",
      "Koenker test may take additional time."
    )
  }

  ht_log("INFO", "Running Koenker test")

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
    stop("Koenker test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  n <- length(residuals)
  if (n <= (ncol(predictors) + 1L)) {
    .std_error(
      "rassumption_violation",
      assumption = "Koenker auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  e_squared <- residuals^2
  if (all(e_squared == e_squared[1])) {
    .std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in squared residuals"
    )
  }

  aux_data <- as.data.frame(predictors)
  aux_model <- safe_lm(e_squared ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
