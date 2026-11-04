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
#' disturbances*, this statistic converges to a chi-squared distribution whose
#' degrees of freedom are the rank of the auxiliary regression less its
#' intercept, the number of regressors on a model with an intercept (see
#' *Degrees of freedom*). This matches
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
#' @section Degrees of freedom:
#' The auxiliary regression of the scaled squared residuals on the regressors
#' \eqn{Z} has an intercept, and the degrees of freedom are the rank of \eqn{[1, Z]} minus
#' one, the rank of the auxiliary regression less its intercept, as White's
#' test counts them (see *Degrees of freedom* in [performWhiteTest()]) and as
#' `lmtest::bptest()` does. A column of \eqn{Z} that is a linear combination of
#' the intercept and the columns before it is dropped. So a model with an
#' intercept counts its regressors, and a model without an intercept whose
#' columns span the constant, such as the dummies of every level of a factor
#' in `lm(y ~ 0 + group + x1)`, counts one fewer: the auxiliary regression adds
#' an intercept that those columns already span. The rank is decided as in
#' White's test, on the regressors centred one by one, at the default
#' tolerance of [qr()]. A model whose only regressors are constant has none
#' beyond the intercept, and is refused, as a model with an intercept alone is.
#'
#' Up to 0.17.0 the degrees of freedom were the number of columns of
#' \eqn{Z}, one more on a model without an intercept whose columns span the
#' constant, with the same statistic and a larger p-value: on
#' `lm(y ~ 0 + group + x1)`, with a factor of four levels and 120
#' observations, 8.653 on 5 degrees of freedom (p = 0.124), where
#' `lm(y ~ x1 + group)`, the same column space, and
#' `lmtest::bptest(..., studentize = FALSE)` give 8.653 on 4 (p = 0.070), as
#' the test now does. [performBPTestStreaming()] gives these degrees of freedom
#' too, on such a model, which it refused up to 0.17.0 ("Breusch-Pagan
#' auxiliary regression became rank deficient"), and so do
#' \link[=performBPTestRobust]{performBPTestRobust()} with `studentize = FALSE`,
#' whose interval follows them, and the registered `"breusch_pagan"` of
#' [runHeteroTests()]. A model with an intercept, or without one whose columns
#' do not span the constant, such as `lm(y ~ 0 + x1 + x2)`, keeps its degrees
#' of freedom. On the latter `lmtest::bptest()` given the model regresses the
#' squared residuals on its columns without an intercept, and gives another
#' statistic, as before; given the regressors as `varformula = ~ x1 + x2` it
#' gives the value of this test.
#'
#' Up to 0.17.0 the auxiliary regression was also fitted by `lm()` from a
#' formula, on the columns of \eqn{Z} as they are, and on three kinds of
#' design of full rank its statistic was wrong. It is now the value of
#' [performBPTestStreaming()], which had it already, and of `lmtest::bptest()`
#' but on the Gaussian `glm`, whose design `lmtest::bptest()` refits by
#' `lm.fit()`, dropping the regressor too;
#' \link[=performBPTestRobust]{performBPTestRobust()} with `studentize = FALSE`
#' and the registered `"breusch_pagan"` change with it. On a model without an
#' intercept with a regressor whose spread is less than about \eqn{10^{-7}} of
#' its distance from zero, that fit dropped the regressor as a combination of
#' its intercept, and the degrees of freedom still counted it: on the same 120
#' observations, `lm(y ~ 0 + I(x1 + 1e8) + x2)` gave 2.850 on 2 degrees of
#' freedom (p = 0.241), where `lmtest::bptest()` given `varformula = ~ x1 + x2`
#' and the test now give 16.72 (p = 0.0002). A Gaussian `glm`, fitted at a
#' tolerance of \eqn{10^{-11}}, keeps a regressor that `lm()` would alias:
#' `glm(y ~ I(1e6 + x1 / 10) + x2)` gave 2.005 (p = 0.367) for 12.45
#' (p = 0.002), the value of `lm(y ~ x1 + x2)`, the same column space. And a
#' regressor named `scaled_sq` was taken for the response of the formula:
#' `lm(y ~ scaled_sq + x2)` gave 102.0 for 12.45, the value of the same model
#' under another name. On other designs the statistic is the one it was.
#'
#' @section Exact fit:
#' An auxiliary regression that fits the scaled squared residuals exactly has
#' an \eqn{R^2} of 1, and the statistic, half its explained sum of squares, is
#' half their total sum of squares, a function of the spread of the squared
#' residuals alone, whatever their pattern. On a paired design, such as
#' `lm(y ~ treat + subject)` with two observations of each subject, the two
#' residuals of a subject sum to zero, so their squares are equal and a
#' combination of the subject dummies: on 30 subjects the test gave 63.25 on
#' 30 degrees of freedom (p = 0.0004) on homoscedastic data, the value of
#' `lmtest::bptest(..., studentize = FALSE)`, and on `lm(y ~ x1 + f)` with a
#' factor of 10 levels of two observations, 28.03 on 10 (p = 0.002). Since
#' 0.18.0 it refuses such a regression when the design forces it, where it
#' returned that statistic, as White's test refuses it (see *Too few
#' observations* in [performWhiteTest()]). The refusal is an error of class
#' `heteroTests_exact_fit_error`, whose message says why and asks for more
#' observations per level: the design forces the exact fit, and more levels
#' of two observations do not help. A fit counts as exact by the rule of
#' White's test, when its residual sum of squares is at most
#' `sqrt(.Machine$double.eps)`, about \eqn{1.5 \times 10^{-8}}, of the total
#' sum of squares of the squared residuals.
#' The design forces it when the regression would fit the squares of the
#' residuals of any response: the test pushes a fixed pseudo-response,
#' standard normal draws from a fixed seed that leave the random numbers of
#' the session as they were, through the QR decomposition of the fit, with
#' its weights, and refuses only when the same regressors fit the squares of
#' its residuals exactly too. A fit the
#' data make exact is tested, as up to 0.17.0: in a one-way layout of nine
#' groups of ten observations and one of two, with a value of the group of two
#' typed 100 times too large, the dummies fit the squared residuals to within
#' \eqn{1.1 \times 10^{-9}} of their total sum of squares, an exact fit by the
#' rule, and the test gives 2069.35 on 9 degrees of freedom. A fit without a QR
#' decomposition, such as an `mgcv::gam()` fit tested on its design, is
#' refused, with a message that says only that the regression fits the squared
#' residuals exactly.
#'
#' [performBPTestStreaming()], the registered `"breusch_pagan"`,
#' `performBreuschPaganTest()`, [performBPTestRobust()] with
#' `studentize = FALSE` and [bootstrapTestStatistic()] of this test refuse it
#' too. [runHeteroTests()] takes no fallback, and stops with the refusal: of
#' its fallbacks, Koenker's test fits the same squared residuals exactly, and
#' the Cook-Weisberg test tests the fitted values, another hypothesis, and
#' would return its value under this name. The other tests whose auxiliary
#' regression can fit the squared residuals, or a function of them, exactly
#' refuse such a fit by the same rule (see *Exact fit* in
#' [performKoenkerTest()]). [autoTransform()], which chooses a transformation
#' by the p-value of this test, leaves out a candidate fit it refuses and stops
#' with the refusal when every candidate is refused, as on such a design; on
#' the one-way layout above it chooses `boxcox`, as up to 0.17.0.
#'
#' The refusal covers the exact fit only. One step away from it, with one of
#' the 30 subjects observed a third time, the test gives 64.74 on 30
#' (p = 0.0002), and on any fit with fixed effects for small groups it rejects
#' too often, and warns (see *Fixed effects for small groups*).
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

  n <- length(residuals)

  if (n <= (ncol(predictors) + 1L)) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan auxiliary regression requires observations to exceed predictors plus intercept"
    )
  }

  # The columns of the auxiliary regression and its rank, decided on the
  # regressors centred one by one, as White's test decides them
  # (rauxiliary_columns()): a model without an intercept whose columns span
  # the constant, as the dummies of every level of a factor do, counts the
  # intercept of the auxiliary regression once. Until 0.18.0 the degrees of
  # freedom were the number of regressors, one more on such a model.
  centred <- predictors - rep(colMeans(predictors), each = n)
  columns <- rauxiliary_columns(centred)
  if (length(columns$keep) == 0L) {
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  # Classical Breusch-Pagan (1979): regress the scaled squared residuals
  # g_i = e_i^2 / sigma^2 - 1 (with sigma^2 the ML variance estimate) on the
  # regressors and take half the explained sum of squares. The statistic is
  # chi-squared(df) under homoskedasticity *and* normal disturbances, with df
  # the rank of the auxiliary regression less its intercept. For the
  # studentized form that drops the normality assumption use performKoenkerTest().
  sigma2 <- sum(residuals^2) / n
  scaled_sq <- residuals^2 / sigma2 - 1
  aux_fit <- rauxiliary_fit(scaled_sq, predictors, centred, columns, test_label)
  # An auxiliary regression that fits the squared residuals exactly, as the
  # dummies of a factor with two observations per level do, whose residuals
  # are equal and opposite in each pair: the statistic, half the explained sum
  # of squares, is then half the total, a function of the spread of the
  # squared residuals alone. Refused since 0.18.0 at the threshold of White's
  # test (rauxiliary_fit_exact()), with its message, when the design forces it
  # (rforced_exact_fit()); on the paired design of White's exact fit the test
  # gave 63.25 on 30 (p = 0.0004). A fit the data make exact, as an outlier in
  # a group of two observations, is tested, as up to 0.17.0. The scaled squared
  # residuals are an affine function of the squared residuals, with the same
  # fit, so the pseudo-residuals are only squared.
  if (rauxiliary_fit_exact(aux_fit)) {
    forced <- rforced_exact_fit(rcontext_qr(ctx), function(e) {
      rauxiliary_fit_exact(rauxiliary_fit(e^2, predictors, centred, columns, test_label))
    })
    if (!identical(forced, FALSE)) {
      rrefuse_auxiliary_exact_fit("the Breusch-Pagan test", aux_fit$rank, n,
                                  rexact_fit_statistic$spread, forced = forced)
    }
  }
  test_statistic <- 0.5 * sum(aux_fit$fitted.values^2)
  df <- aux_fit$rank - 1L
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
  .Deprecated(
    "performBPTest", old = "performBreuschPaganTest",
    msg = .ht_renamed_deprecation("performBreuschPaganTest", "performBPTest")
  )
  performBPTest(model, data)
}
