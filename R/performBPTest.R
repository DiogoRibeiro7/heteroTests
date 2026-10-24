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
#' The test is computed in memory whatever the size of the data. Up to 0.15.0
#' it switched to [performBPTestStreaming()] on data above 100 MB; call that
#' function to stream.
#'
#' @section Fixed effects for small groups:
#' The Breusch-Pagan test, like White's and Koenker's tests, is not valid with
#' fixed effects for small groups: a factor with many levels and few
#' observations of each, such as `subject` in `lm(y ~ x + subject)` with each
#' subject observed a few times. On homoscedastic data, with normal errors, it
#' rejects too often, and the more often the more levels the factor has and
#' the fewer observations each level has. The excess comes from the dummies of the
#' factor in the auxiliary regression. The residuals of a level are taken
#' from its own mean, so under normal errors the squared residuals of a level
#' of \eqn{T} observations vary between levels \eqn{T / (T - 1)} times as much
#' as the auxiliary regression assumes, and with \eqn{G} levels the statistic
#' exceeds its degrees of freedom by about \eqn{(G - 1) / (T - 1)}, as
#' Koenker's did in the study below. Regressed on `x` alone, without the
#' dummies, the squared residuals were rejected by this test in 4.3\% to
#' 5.6\% of the draws (`inst/validation/fixed-effects-dummies.R`). The excess
#' grows with \eqn{G} at a fixed \eqn{T}, while the spread
#' of the chi-squared distribution grows as \eqn{\sqrt{2G}}{sqrt(2G)}, so with
#' a fixed number of observations per level the size goes to 1 as the number
#' of levels grows.
#'
#' Since 0.16.0 the test warns when a term of the model that holds a factor
#' or a character variable has more levels than observations per level:
#' \eqn{L > n / L}, that is \eqn{L^2 > n}, with \eqn{L} the number of
#' distinct combinations of the factors of the term among the \eqn{n}
#' observations of the fit. The term is a factor, such as `subject`; an
#' interaction of factors, counted by its cells, such as `a:b` of a crossed
#' `a * b` or `school:class` of a nested `school / class`; or a slope for each
#' level, `x:subject`. A variable left out of the formula, as in `y ~ . - id`,
#' does not count. The warning names the term and gives \eqn{L}, \eqn{n} and
#' the mean number of observations per level; the statistic, the degrees of
#' freedom and the p-value are those the test computes without it. It has class
#' `heteroTests_fixed_effects_warning`, which
#' `suppressWarnings(expr, classes = "heteroTests_fixed_effects_warning")`
#' muffles, and no other warning. It is given on every call, also when
#' [cachedTest()] or [runHeteroTests()] answers from its cache, and
#' [runHeteroTests()] and [runDiagnostics()] give it once for each test that
#' gives it. [performBPTestStreaming()],
#' \link[=performBPTestRobust]{performBPTestRobust()} with
#' `studentize = FALSE`, the deprecated `performBreuschPaganTest()` and the
#' registered `"breusch_pagan"` give it too.
#'
#' On a balanced design of \eqn{G} subjects of \eqn{T} observations the rule
#' is \eqn{G \le T}{G <= T}. In the study it kept the three tests within
#' 2.5\% to 7.5\% at the 5\% level where it holds, and put at least one of
#' them outside where it does not, in 68 of the 69 designs where they
#' computed; the exception is this test on 20 subjects of 20, at 8.0\%.
#' Unbalanced groups, a second regressor and a regressor correlated with the
#' subject effects followed the same pattern, with the mean number of
#' observations per level, \eqn{n / G}, in place of \eqn{T}. The rule is a
#' warning, not a correction, and many panels fail it: on 100 firms observed
#' over 10 years the three tests reject in 18\% to 21\% of the draws.
#' Rejection at the 5\% level, in percent, in 2,000 homoscedastic draws of
#' `lm(y ~ x + subject)`, with `x`, the subject effects and the errors
#' standard normal:
#'
#' | Subjects | Observations of each | White | Koenker | Breusch-Pagan | Warns |
#' | ---: | ---: | ---: | ---: | ---: | --- |
#' | 10 | 10 | 6.4 | 6.2 | 7.3 | no |
#' | 20 | 20 | 6.0 | 6.4 | 8.0 | no |
#' | 5 | 4 | 1.0 | 5.4 | 7.0 | yes |
#' | 30 | 3 | 50.7 | 57.3 | 39.6 | yes |
#' | 30 | 4 | 22.2 | 23.9 | 28.7 | yes |
#' | 100 | 8 | 23.8 | 23.8 | 25.2 | yes |
#' | 100 | 10 | 18.1 | 18.4 | 21.3 | yes |
#' | 200 | 4 | 97.8 | 97.6 | 82.5 | yes |
#'
#' A slope for each level counts too, but there the error goes the other way:
#' with a slope for each subject, `lm(y ~ x + x:subject)`, on 30 subjects of 3
#' and of 4 and on 100 of 8, the three tests rejected too rarely, White's in
#' 0.7\% to 2.2\% of the draws, Koenker's in 0.1\% to 0.8\% and the
#' Breusch-Pagan test in 0.4\% to 2.8\%
#' (`inst/validation/fixed-effects-variants-size.R`). The warning therefore
#' says that the p-value is unreliable: in the simulations usually far too
#' small, and too large with a slope per level.
#'
#' The study is `inst/validation/fixed-effects-size.R`, with its results in
#' `fixed-effects-size.csv` (`system.file("validation", package =
#' "heteroTests")`). Juhl and Sosa-Escudero (2014) derive a test of
#' heteroskedasticity for this setting, after fixed effects estimation of a
#' panel of many units observed a fixed number of times; it needs to be told
#' which factor is the unit, and the package does not implement it.
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
#' Juhl, T., & Sosa-Escudero, W. (2014). Testing for heteroskedasticity in
#' fixed effects models. *Journal of Econometrics, 178*(Part 3), 484–494.
#' \doi{10.1016/j.jeconom.2013.07.005}
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

  # A factor with more levels than observations per level, among the
  # observations of the fit: the value stands, with a warning
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(ctx$model, ctx$frame, "the Breusch-Pagan test", "performBPTest")

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
