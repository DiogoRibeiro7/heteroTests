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
#' The test is computed in memory whatever the size of the data. Up to 0.15.0
#' it switched to [performKoenkerTestStreaming()] on data above 100 MB; call
#' that function to stream.
#'
#' @section Fixed effects for small groups:
#' Koenker's test, like White's and the Breusch-Pagan tests, is not valid with
#' fixed effects for small groups: a factor with many levels and few
#' observations of each, such as `subject` in `lm(y ~ x + subject)` with each
#' subject observed a few times. On homoscedastic data it rejects too often,
#' and the more often the more levels the factor has and the fewer
#' observations each level has. The excess comes from the dummies of the factor in the auxiliary
#' regression. The residuals of a level are taken from its own mean, so under
#' normal errors the squared residuals of a level of \eqn{T} observations vary
#' between levels \eqn{T / (T - 1)} times as much as the auxiliary regression
#' assumes, and with \eqn{G} levels the statistic has mean its degrees of
#' freedom plus \eqn{(G - 1) / (T - 1)}, as it had in the study below, within
#' 2.3\%. Regressed on `x` alone, without the dummies, the squared residuals
#' were rejected in 4.6\% to 5.5\% of the draws
#' (`inst/validation/fixed-effects-dummies.R`). The excess grows with
#' \eqn{G} at a fixed \eqn{T}, while the spread of the chi-squared
#' distribution grows as \eqn{\sqrt{2G}}{sqrt(2G)}, so with a fixed number of
#' observations per level the size goes to 1 as the number of levels grows.
#'
#' Since 0.16.0 the test warns when a factor of the model, a main effect or a
#' factor that enters only through an interaction, has more levels than
#' observations per level: \eqn{L > n / L}, that is \eqn{L^2 > n}, for
#' \eqn{L} levels among the \eqn{n} observations of the fit. The warning
#' names the factor and gives \eqn{L}, \eqn{n} and the mean number of
#' observations per level; the statistic, the degrees of freedom and the
#' p-value are those the test computes without it. It has class
#' `heteroTests_fixed_effects_warning`, which
#' `suppressWarnings(expr, classes = "heteroTests_fixed_effects_warning")`
#' muffles, and no other warning. It is given on every call, also when
#' [cachedTest()] or [runHeteroTests()] answers from its cache, and
#' [runHeteroTests()] and [runDiagnostics()] give it once for each test that
#' gives it. [performKoenkerTestStreaming()],
#' \link[=performBPTestRobust]{performBPTestRobust()} with
#' `studentize = TRUE`, the deprecated [performStudentizedBPTest()] and the
#' registered `"koenker"` and `"student_bp"` give it too.
#' [performWildBootstrapTest()], which bootstraps the same statistic, held its
#' level on these designs and does not give it.
#'
#' On a balanced design of \eqn{G} subjects of \eqn{T} observations the rule
#' is \eqn{G \le T}{G <= T}. In the study it kept the three tests within
#' 2.5\% to 7.5\% at the 5\% level where it holds, and put at least one of
#' them outside where it does not, in 68 of the 69 designs where they
#' computed; the exception is the Breusch-Pagan test on 20 subjects of 20, at
#' 8.0\%. Unbalanced groups, a second regressor and a regressor correlated
#' with the subject effects followed the same pattern, with the mean number of
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
#' A factor that enters only through an interaction is counted too, but there
#' the error goes the other way: with a slope for each subject,
#' `lm(y ~ x + x:subject)` on 30 subjects of 3 and of 4 and on 100 of 8, the
#' three tests rejected too rarely, White's in 0.7\% to 2.2\% of the draws,
#' Koenker's in 0.1\% to 0.8\% and the Breusch-Pagan test in 0.4\% to 2.8\%
#' (`inst/validation/fixed-effects-variants-size.R`). The p-value is then too
#' large, and the warning says so when every factor it names enters only
#' through interactions.
#'
#' The study is `inst/validation/fixed-effects-size.R`, with its results in
#' `fixed-effects-size.csv` (`system.file("validation", package =
#' "heteroTests")`). Juhl and Sosa-Escudero (2014) derive a test of
#' heteroskedasticity for this setting, after fixed effects estimation of a
#' panel of many units observed a fixed number of times; it needs to be told
#' which factor is the unit, and the package does not implement it.
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
#' Juhl, T., & Sosa-Escudero, W. (2014). Testing for heteroskedasticity in
#' fixed effects models. *Journal of Econometrics, 178*(Part 3), 484–494.
#' \doi{10.1016/j.jeconom.2013.07.005}
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

  # A warning above 50 MB. Up to 0.15.0 the test switched to
  # performKoenkerTestStreaming() above 100 MB; the streaming tests run only
  # when they are called, or asked for in runHeteroTests() (?runHeteroTests).
  tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NULL
  )
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

  # A factor with more levels than observations per level, among the
  # observations of the fit: the value stands, with a warning
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(ctx$model, ctx$frame, "Koenker's test", "performKoenkerTest")

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
