#' Perform White's test for heteroscedasticity (corrected implementation)
#'
#' Conducts the classic White (1980) Lagrange Multiplier test for unknown forms
#' of heteroscedasticity in linear regression models. The implementation builds
#' the full auxiliary regression with squares and cross-products of the
#' regressors and integrates the package's validation helpers to guarantee that
#' the expanded design matrix is well-posed before fitting the test equation.
#'
#' @param model A fitted [stats::lm] object representing the mean specification
#'   to be diagnosed.
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
#' @param cross_products Logical scalar indicating whether to include all
#'   pairwise cross-products of the regressors in the auxiliary regression.
#'   Defaults to `TRUE` and should remain enabled unless dimensionality makes
#'   the regression unstable.
#' @param max_interactions Single positive integer giving the maximum number of
#'   original predictors for which cross-products are generated. When the number
#'   of regressors exceeds this threshold, cross-products are dropped to avoid
#'   explosive growth in columns. Defaults to `10`.
#'
#' @return An object of class \code{htest} containing the chi-squared test
#'   statistic, associated degrees of freedom, and p-value.
#'
#' @details
#' Let \eqn{\hat{e}} be the residuals from the fitted model. White's test fits an
#' auxiliary regression of \eqn{\hat{e}^2} on the original regressors, their
#' squares, and (optionally) their cross-products. Under the null hypothesis of
#' homoskedasticity and mild regularity conditions, the statistic
#' \eqn{n \times R^2} from this auxiliary model converges to a chi-squared
#' distribution whose degrees of freedom are the rank of the auxiliary
#' regression less its intercept (see *Degrees of freedom*). The function:
#' \enumerate{
#'   \item validates the model object with \link[=rvalidateModelInputs]{rvalidateModelInputs()} to ensure at
#'     least 20 observations and finite residuals;
#'   \item reads the response, design, residuals and weights from the fit (the
#'     model context, since 0.16.0), matches the rows of `data` to the
#'     observations of the fit, and warns about rows of `data` with missing
#'     values, which the fit left out;
#'   \item drops the columns of the auxiliary design that are linearly
#'     dependent on the intercept and the other columns; and
#'   \item applies \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} so that sample-size and
#'     conditioning rules specific to the White test are satisfied.
#' }
#'
#' Rejecting the null indicates that the variance of the errors depends on one
#' or more functions of the regressors. While the statistic is asymptotically
#' valid under heteroskedasticity of unknown form, finite-sample performance can
#' deteriorate when the number of regressors is large relative to the sample
#' size. In such situations consider disabling `cross_products` or switching to
#' the robust variants provided by the package.
#'
#' The test is computed in memory whatever the size of the data. Up to 0.15.0
#' it switched to [performWhiteTestStreaming()] on data above 100 MB, which
#' stops on any model with a factor or a `poly()` term; call that function to
#' stream.
#'
#' @section Degrees of freedom:
#' Let \eqn{Z} hold the regressors, their squares and their cross-products.
#' The auxiliary regression of \eqn{\hat{e}^2} on \eqn{Z} has an intercept, and
#' the degrees of freedom are the rank of \eqn{[1, Z]} minus one, as
#' `lmtest::bptest()` counts them given the same terms as `varformula`. A column
#' of \eqn{Z} that is a linear combination of the intercept and the columns
#' before it is dropped: the square of a dummy, which equals the dummy, and,
#' when the columns of \eqn{Z} span the constant, one column more. They span it
#' with the squares of the dummies of `contr.sum`, `contr.helmert` or
#' `contr.poly` contrasts, the last the default for an ordered factor, the
#' dummies of every level of a factor in a model without an intercept, an
#' interaction with an empty cell such as that of the baseline levels, or the
#' squares of an orthogonal `poly()` basis. In 0.15.0 the degrees of freedom
#' were the rank of \eqn{Z} alone, one more on such a design, with the same
#' statistic and a larger p-value: on
#' `lm(y ~ x1 + f, contrasts = list(f = "contr.sum"))`, with a factor of two
#' levels, 5 where `lmtest::bptest()` gives 4, and on `lm(y ~ x1 + o)`, with an
#' ordered factor of four levels, 9 for 8.
#'
#' The rank is decided, at the default tolerance of [qr()], on the auxiliary
#' design of the centred regressors, which spans the same columns with the
#' intercept, so that it does not depend on where the regressors are located;
#' the regression is fitted on \eqn{Z}, or on that design when the fit of
#' \eqn{Z} would lose one of the columns kept. Before, a regressor far from
#' zero, such as \eqn{x = 10^4 + u} or a date written as `YYYYMMDD`, lost its
#' square, which the rank of \eqn{[1, Z]} took for a combination of the
#' intercept and the regressor: on `lm(y ~ x1 + x2)` with \eqn{10^4} added to
#' `x1`, 10.34 on 4 degrees of freedom, where the model of `x1` as it was gives
#' 11.14 on 5, as both now do. That holds for regressors that enter the model
#' linearly, which are centred one by one. A product or a power of a variable
#' located far from zero inside the model stays ill-conditioned once its
#' columns are centred, and can still lose a column; how far is far depends
#' on the spread of the variable. On 200 observations of two standard normal
#' regressors, `lm(y ~ I(x1 + 2e3) * I(x2 + 2e2))`, with `x1` about 2,000
#' standard deviations from zero, keeps every coefficient, and White's test
#' gives 46.83 on 7 degrees of freedom where `lm(y ~ x1 * x2)`, the same
#' model, gives 51.90 on 8; `lm(y ~ I(x1 + 2e3) + I((x1 + 2e3)^2))` gives 8.05
#' on 3 for 11.29 on 4. In ten such data sets, of normal and of uniform
#' regressors, the degrees of freedom held, and the statistic moved by at most
#' 3e-5 relative, while the shift was at most 700 standard deviations of the
#' regressor; the column was lost from 800 to 2,000 standard deviations on,
#' depending on the term and the data. From about 3,000 to 11,000 standard
#' deviations on, `lm()` itself aliases the interaction or the square, before
#' White's test sees it, and the test is that of the model without the term:
#' with `I(x1 + 1e4) * I(x2 + 1e3)`, 35.02 on 5, the value of
#' `lm(y ~ x1 + x2)`. A year from 1990 to 2020, about 220 standard deviations
#' from zero, and its square, `year * z` or `income * age` are far from that,
#' and give the value of the centred model to seven digits or more.
#'
#' On a design whose auxiliary columns do not span the constant and whose
#' regressors are not located far from zero, such as continuous regressors or
#' factors with the default treatment contrasts, nothing changed. The same
#' holds with `cross_products = FALSE` or more regressors than
#' `max_interactions`, and for the tests computed through this function:
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()}, whose interval,
#' effect size and power follow the degrees of freedom, and the registered
#' `"white"` of [runHeteroTests()]. The p-value of
#' [performWhiteTestBootstrap()] depends on the statistic alone and does not
#' change on a design that spans the constant. [performWhiteTestStreaming()]
#' stops on such a design, whose auxiliary columns are collinear with its
#' intercept, and where it gives a value it gives these degrees of freedom,
#' but on an `mgcv::gam()` fit with smooth terms, whose smooth terms it does
#' not test. On an auxiliary design whose columns are nearly collinear, as the
#' squares and products of the basis of a smooth term can be, the columns
#' kept can differ from those kept in 0.15.0, so the statistic can change too:
#' there the auxiliary regression of 0.15.0 could silently lose a column to
#' its intercept while its degrees of freedom still counted it, as on a
#' `mgcv::gam(y ~ s(x1) + x2)` fit whose statistic moves by 0.28\%, to the
#' value of `lmtest::bptest()` with the same terms.
#'
#' @section Too few observations:
#' An auxiliary regression that fits the squared residuals exactly has an
#' \eqn{R^2} of 1, and the statistic is the number of observations, whatever
#' the data. It fits them exactly with as many independent terms as
#' observations, and with fewer when the squared residuals are a combination
#' of its terms: on a paired design, such as `lm(y ~ treat + subject)` with two
#' observations of each subject, the two residuals of a subject sum to zero,
#' so their squares are equal and a combination of the subject dummies. On 30
#' subjects the test gave 60 on 30 degrees of freedom (p = 0.0009) on
#' homoscedastic data. Since 0.16.0 it refuses such a regression, where it
#' returned that statistic, and says what to use, as the call allows:
#' `cross_products = FALSE` when the products were in and the regressors alone
#' do not fit the squared residuals exactly, Koenker's test when they do not,
#' and more data when the regression has as many terms as observations. Six
#' continuous regressors have 27 squares and products, 28 terms with the
#' intercept, so they need at least 29 observations. With fewer terms than
#' observations the design forces the exact fit, and more data of the same
#' design do not help: 500 subjects of two observations are refused as 30
#' are, and the refusal asks for more observations per level.
#'
#' A fit counts as exact when its residual sum of squares is at most
#' `sqrt(.Machine$double.eps)`, about \eqn{1.5 \times 10^{-8}}, of the total
#' sum of squares of the squared residuals. An exact fit leaves rounding
#' error, about \eqn{10^{-30}} of the total on the paired design; squared
#' residuals that depart from the fit by \eqn{2 \times 10^{-4}} of their size
#' leave about \eqn{2 \times 10^{-7}}, and are tested.
#'
#' The robust form, the registered `"white"`, [performWhiteTestBootstrap()]
#' and [performWhiteTestStreaming()] refuse both kinds of exact fit too.
#' [runHeteroTests()] falls back on White's test without cross products, as on
#' any refusal of White's test, unless the regressors alone fit the squared
#' residuals exactly, as on the paired design, or that is not known, as in the
#' streaming test: the Breusch-Pagan and Koenker tests, its other fallbacks,
#' regress the squared residuals on the regressors and would return the
#' number of observations too, so the runner stops with the refusal.
#'
#' The refusal covers the exact fit only, and one step away from it the test
#' still rejects almost always: in 2,000 homoscedastic draws of
#' `lm(y ~ x + subject)`, with normal `x` and errors, White's test rejected at
#' the 5\% level in 99\% of them with 30 subjects of two observations and one
#' of them observed a third time, and in all but one with 30 pairs whose
#' weights vary within pairs. Those are fixed effects for small groups, on
#' which the test warns (see *Fixed effects for small groups*).
#'
#' @section Fixed effects for small groups:
#' White's, Koenker's and the Breusch-Pagan tests are not valid with fixed
#' effects for small groups: a factor with many levels and few observations
#' of each, such as `subject` in `lm(y ~ x + subject)` with each subject
#' observed a few times. On homoscedastic data they reject too often, and the
#' more often the more levels the factor has and the fewer observations each
#' level has.
#' The excess comes from the dummies of the factor in the auxiliary
#' regression. The residuals of a level are taken from its own mean, so under
#' normal errors the squared residuals of a level of \eqn{T} observations vary
#' between levels \eqn{T / (T - 1)} times as much as the auxiliary regression
#' assumes, and with \eqn{G} levels the statistic exceeds its degrees of
#' freedom by about \eqn{(G - 1) / (T - 1)}. In the study below Koenker's
#' statistic had that mean, and the squared residuals regressed on `x` alone,
#' without the dummies, were rejected in 4.6\% to 5.5\% of the draws by
#' Koenker's statistic and in 4.3\% to 5.6\% by the Breusch-Pagan statistic
#' (`inst/validation/fixed-effects-dummies.R`). The
#' excess grows with \eqn{G} at a fixed \eqn{T}, while the spread of the
#' chi-squared distribution grows as \eqn{\sqrt{2G}}{sqrt(2G)}, so with a fixed number
#' of observations per level the size goes to 1 as the number of levels
#' grows.
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
#' gives it. \link[=performWhiteTestRobust]{performWhiteTestRobust()} and the
#' registered `"white"` give it too, and so does [performWhiteTestStreaming()]
#' where it computes: it refuses a factor that is a main effect, whose dummies
#' and their squares are collinear, and computes White's statistic on one that
#' enters only through an interaction. [performWhiteTestBootstrap()], whose
#' p-value is a bootstrap's, held its level on these designs and does not
#' give it.
#'
#' On a balanced design of \eqn{G} subjects of \eqn{T} observations the rule
#' is \eqn{G \le T}{G <= T}. In the study it kept the three tests within 2.5\% to
#' 7.5\% at the 5\% level where it holds, and put at least one of them outside
#' where it does not, in 68 of the 69 designs where they computed; the
#' exception is the Breusch-Pagan test on 20 subjects of 20, at 8.0\%.
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
#' A factor that enters only through an interaction is counted too, but there
#' the error goes the other way: with a slope for each subject,
#' `lm(y ~ x + x:subject)` on 30 subjects of 3 and of 4 and on 100 of 8, the
#' three tests rejected too rarely, White's in 0.7\% to 2.2\% of the draws,
#' Koenker's in 0.1\% to 0.8\% and the Breusch-Pagan test in 0.4\% to 2.8\%
#' (`inst/validation/fixed-effects-variants-size.R`). The p-value is then too
#' large, and the warning says so when every factor it names enters only
#' through interactions.
#'
#' With at most `max_interactions` regressors White's auxiliary regression
#' holds the products of `x` with the dummies, a slope for each subject, and
#' on 5 subjects of 4 it rejected too rarely, in 1.0\%. The study is
#' `inst/validation/fixed-effects-size.R`, with its results in
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
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroskedasticity. *Econometrica, 48*(4), 817–838.
#' <https://doi.org/10.2307/1912934>
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson. Chapter 11
#' provides an accessible overview of Lagrange Multiplier diagnostics such as
#' the White test.
#'
#' Juhl, T., & Sosa-Escudero, W. (2014). Testing for heteroskedasticity in
#' fixed effects models. *Journal of Econometrics, 178*(Part 3), 484–494.
#' \doi{10.1016/j.jeconom.2013.07.005}
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTest(mod, mtcars)
#'
#' # Disable cross-products when dimensionality is modest but collinearity is a concern
#' performWhiteTest(mod, mtcars, cross_products = FALSE)
#'
#' # Detect heteroscedasticity in simulated data with variance increasing in x
#' set.seed(123)
#' n <- 200
#' x <- runif(n)
#' y <- 1 + 2 * x + rnorm(n, sd = 0.5 + x)
#' df <- data.frame(y, x)
#' hetero_mod <- lm(y ~ x, data = df)
#' performWhiteTest(hetero_mod, df)
#'
#' @seealso
#' [performWhiteTestBootstrap()] for a resampled version suited to small samples,
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()} for enriched reporting, and [performBPTest()] for a
#' lower-dimensional Lagrange Multiplier alternative.
#' @export
performWhiteTest <- function(model, data = NULL, cross_products = TRUE, max_interactions = 10) {
  test_label <- "White test"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performWhiteTest", test_label = test_label)
  # A rank-deficient fit is tested on the estimable part of its design: as the
  # same model fitted without its aliased columns (rnon_aliased_columns()).
  rvalidateModelInputs(model, test_name = "White", min_obs = 20L, allow_aliased = TRUE)
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

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  handle_validation_result(requirements)

  if (!is.logical(cross_products) || length(cross_products) != 1) {
    .std_error("invalid_logical", arg = "cross_products")
  }

  # A warning above 50 MB. Up to 0.15.0 the test switched to
  # performWhiteTestStreaming() above 100 MB; the streaming tests run only when
  # they are called, or asked for in runHeteroTests() (?runHeteroTests).
  tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NULL
  )
  if (nrow(working_data) > 10000) {
    message("Large dataset (", nrow(working_data), " observations). ",
            "This may take some time to compute.")
  }

  ht_log("INFO", "Running White test")

  # The design of the fit (with its intercept), without the columns of a
  # rank-deficient fit that it aliased
  X_full <- rcontext_design(ctx)
  result <- rwhite_statistic(residuals, X_full, cross_products, max_interactions, test_label,
                             data_name = deparse(substitute(model)))
  # A factor with more levels than observations per level, among the
  # observations of the fit: the value stands, with a warning, given once the
  # test has computed, so that a refusal followed by the fallback of the
  # runner warns once (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(ctx$model, ctx$frame, "White's test", "performWhiteTest")
  result
}

# White's auxiliary regression fits the squared residuals exactly when its
# residual sum of squares is at most this fraction of their total: then R^2 is
# 1 and the statistic n R^2 the number of observations, whatever the data. An
# exact fit leaves only rounding error, at about the square of the machine
# epsilon relative to the total (1e-30 on the paired designs of the tests). The
# square root of the epsilon, 1.5e-8, the usual tolerance of a computation
# that should give zero, is far above that and refuses no fit whose residuals
# are more than about 1.2e-4 of the spread of the squared residuals, at which
# n R^2 equals n to eight digits: squared residuals that depart from an exact
# fit by 2e-4 of their size leave 1.9e-7 of the total and are tested, by 2e-5
# 1.9e-9 and are refused (test-performWhiteTest.R).
rwhite_exact_tolerance <- sqrt(.Machine$double.eps)

# The refusal of an auxiliary regression that fits the squared residuals
# exactly: with as many independent terms as observations, or more
# (`saturated`), or because the squared residuals are a combination of fewer
# terms, as when each level of a factor holds two observations, whose
# residuals then sum to zero in each pair. The advice depends on the call:
# `cross_products = FALSE` only when the products were in and the regressors
# alone may not fit the squared residuals exactly, Koenker's test only when
# they do not (`regressors_exact`), since its auxiliary regression is on them;
# NA when that is not known. More data help a
# saturated regression; an exact fit with fewer terms than observations is
# forced by the design, as by a factor with two observations per level, whose
# terms grow with its levels, so there only more observations per level help.
# Its class tells runHeteroTests() whether a test of the regressors alone can
# stand in for it (ht_analyse_failure()). Shared by rwhite_statistic() and
# performWhiteTestStreaming().
rwhite_refuse_exact_fit <- function(terms, n, saturated, products, regressors_exact = NA,
                                    what = "independent terms") {
  cause <- if (saturated) {
    sprintf(
      "it has %d %s with its intercept for %d observations, at least as many as there are",
      as.integer(terms), what, as.integer(n)
    )
  } else {
    sprintf(
      paste(
        "the squared residuals are a combination of its %d %s with its intercept, for %d",
        "observations"
      ),
      as.integer(terms), what, as.integer(n)
    )
  }
  # Without the products the regression keeps the regressors and their
  # squares, which fit the squared residuals exactly when the regressors do.
  advice <- c(
    if (isTRUE(products) && !isTRUE(regressors_exact)) "`cross_products = FALSE`",
    if (identical(regressors_exact, FALSE)) "Koenker's test (performKoenkerTest())",
    if (saturated) "more data" else "more observations per level"
  )
  advice <- if (length(advice) > 1L) {
    paste(paste(advice[-length(advice)], collapse = ", "), "or", advice[length(advice)])
  } else {
    advice
  }
  reason <- if (saturated) {
    "There are too few observations for White's test with these regressors"
  } else {
    paste(
      "The design forces the exact fit, as a factor with two observations per level does,",
      "however many levels it has"
    )
  }
  message <- sprintf(
    paste(
      "White's auxiliary regression fits the squared residuals exactly: %s. Its R-squared",
      "is 1 and its statistic the number of observations, whatever the data. %s; use %s."
    ),
    cause, reason, advice
  )
  stop(structure(
    class = c("heteroTests_white_exact_fit", "error", "condition"),
    list(message = message, call = NULL, regressors_exact = regressors_exact)
  ))
}

# Whether the regressors with an intercept fit the squared residuals exactly,
# at rwhite_exact_tolerance: then so does the auxiliary regression of any
# test of the regressors alone, Koenker's and Breusch-Pagan's.
rwhite_regressors_fit_exactly <- function(X, e_squared) {
  centred <- X - rep(colMeans(X), each = nrow(X))
  fit <- stats::lm.fit(cbind(1, centred), e_squared)
  total <- sum((e_squared - mean(e_squared))^2)
  total > 0 && sum(fit$residuals^2) <= rwhite_exact_tolerance * total
}

# White's statistic from the residuals the test reads and the design of the
# auxiliary regression, with its intercept. performWhiteTest() gives it the
# design of the fit (the model context); rwhite_test_rebuilt_design(), behind
# the White bootstrap until increment 8 of the model context, the design
# rebuilt from `data` as before 0.16.0.
rwhite_statistic <- function(residuals, X_full, cross_products, max_interactions, test_label,
                             data_name) {
  # Extract model components
  e_squared <- residuals^2
  n <- length(e_squared)

  if (nrow(X_full) != n) {
    stop(
      sprintf(
        "%s could not align the design matrix with model residuals (expected %d rows, got %d).",
        test_label,
        n,
        nrow(X_full)
      ),
      call. = FALSE
    )
  }

  # Remove intercept column for auxiliary regression
  if (colnames(X_full)[1] == "(Intercept)") {
    X <- X_full[, -1, drop = FALSE]
  } else {
    X <- X_full
  }

  p <- ncol(X)  # Number of original regressors (excluding intercept)

  if (p == 0) {
    stop("No regressors found in model (intercept-only model)")
  }

  # The auxiliary design Z: the regressors, their squares and, unless they are
  # left out, their pairwise products X_i * X_j for i < j.
  products <- cross_products && p > 1
  if (products && p > max_interactions) {
    .std_warning("cross_products_omitted")
    message("Cross-products omitted due to high dimensionality (p = ", p,
            " > max_interactions = ", max_interactions, ")")
    products <- FALSE
  }
  auxiliary_design <- function(M) {
    Z <- cbind(M, M^2)
    if (products) {
      for (i in seq_len(p - 1L)) {
        for (j in seq.int(i + 1L, p)) {
          Z <- cbind(Z, M[, i] * M[, j])
        }
      }
    }
    Z
  }
  aux_matrix <- auxiliary_design(X)

  # Drop linearly dependent auxiliary columns rather than aborting. A 0/1
  # indicator squared equals itself, so its square is collinear with it. The
  # auxiliary regression has an intercept, so a column is dependent when it is
  # a combination of the intercept and the columns before it: the columns of Z
  # can span the constant, as the squares of contr.sum, Helmert or contr.poly
  # dummies, the dummies of every level of a factor without an intercept, or
  # the squares of an orthogonal poly() basis do, and a column of Z that adds
  # only the constant adds nothing to the regression. The degrees of freedom
  # are the rank of [1, Z] minus one, the rank of the auxiliary regression less
  # its intercept, as in lmtest::bptest().
  #
  # The rank is decided on the design built from the centred regressors, which
  # spans the same columns as Z with the intercept, prefix by prefix, so that
  # it does not depend on where the regressors are located: on x = 1e4 + u,
  # the square of x is a combination of the intercept and x to within the
  # tolerance of qr() relative to its size, while the square of the centred x
  # is not. Each regressor is centred on its own, so a product or a power of a
  # variable located far from zero inside the model, I(x1 + 1e4) * I(x2 + 1e3),
  # stays ill-conditioned (ROADMAP). The columns kept are those QR pivoting of
  # [1, Z_c] keeps, at the default tolerance of qr(). Up to 0.15.0 the rank was
  # that of Z alone, one too many when Z spans the constant; in the development
  # version before this change, that of [1, Z], which lost the square of a
  # regressor located far from zero.
  centred <- auxiliary_design(X - rep(colMeans(X), each = n))
  qr_aux <- qr(cbind(1, centred))
  rank_with_intercept <- qr_aux$rank
  # The intercept is the first column and is never pivoted out.
  keep <- setdiff(sort(qr_aux$pivot[seq_len(rank_with_intercept)]), 1L) - 1L
  dropped <- ncol(aux_matrix) - length(keep)
  if (dropped > 0L) {
    ht_log("INFO", sprintf(
      paste(
        "White test: dropped %d auxiliary term(s) collinear with the intercept",
        "and the other terms; using df = %d."
      ),
      dropped, length(keep)
    ))
  }
  if (length(keep) == 0L) {
    .std_error(
      "rassumption_violation",
      assumption = "Auxiliary regression has no independent regressors after removing collinear terms"
    )
  }
  # As many independent terms as observations: the auxiliary regression fits
  # the squared residuals exactly, R^2 is 1 and the statistic n whatever the
  # data.
  if (rank_with_intercept >= n) {
    rwhite_refuse_exact_fit(rank_with_intercept, n, saturated = TRUE, products = products,
                            regressors_exact = rwhite_regressors_fit_exactly(X, e_squared))
  }

  # The auxiliary regression, e^2 on the intercept and the columns kept, fitted
  # with lm.fit() at its default tolerance: on the columns of Z as they are,
  # which keeps the value of a design whose regressors are not located far from
  # zero, or on the centred ones, which span the same space, when the fit of
  # those as they are aliases one of them. Its rank less one is the degrees of
  # freedom. Fitted from the matrix rather than a formula, it does not depend on
  # the names of the regressors: a regressor `a` beside a regressor `a_sq` is
  # tested, where the formula stopped on the duplicated name.
  aux_fit <- stats::lm.fit(cbind(1, aux_matrix[, keep, drop = FALSE]), e_squared)
  if (aux_fit$rank < rank_with_intercept) {
    aux_fit <- stats::lm.fit(cbind(1, centred[, keep, drop = FALSE]), e_squared)
  }
  if (aux_fit$rank != rank_with_intercept) {
    stop(sprintf(
      "%s: the auxiliary regression has rank %d where its columns have rank %d.",
      test_label, aux_fit$rank, rank_with_intercept
    ), call. = FALSE)
  }
  aux_rank <- aux_fit$rank - 1L

  # R^2 as summary.lm() computes it for a model with an intercept.
  fitted_squares <- aux_fit$fitted.values
  explained <- sum((fitted_squares - mean(fitted_squares))^2)
  residual <- sum(aux_fit$residuals^2)
  r_squared <- explained / (explained + residual)
  # Fewer independent terms than observations, and still an exact fit: the
  # squared residuals are a combination of the terms, and n R^2 is n.
  total <- explained + residual
  if (total > 0 && residual <= rwhite_exact_tolerance * total) {
    rwhite_refuse_exact_fit(aux_fit$rank, n, saturated = FALSE, products = products,
                            regressors_exact = rwhite_regressors_fit_exactly(X, e_squared))
  }

  # Handle numerical edge cases
  if (is.na(r_squared) || r_squared < 0) {
    warning("Invalid R-squared from auxiliary regression. Setting to 0.")
    r_squared <- 0
  }
  if (r_squared > 1) {
    warning("R-squared > 1 from auxiliary regression. Setting to 1.")
    r_squared <- 1
  }

  test_statistic <- n * r_squared
  # The rank of the auxiliary regression less its intercept: the number of
  # auxiliary regressors kept.
  df <- aux_rank
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)

  # Create result object
  result <- structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "White's test for heteroscedasticity",
      data.name = data_name,
      alternative = "heteroscedasticity present"
    ),
    class = "htest"
  )

  # Add diagnostic information
  attr(result, "auxiliary_regressors") <- length(keep)
  attr(result, "original_regressors") <- p
  attr(result, "cross_products_included") <- cross_products && p > 1 && p <= max_interactions
  attr(result, "r_squared_auxiliary") <- r_squared

  ht_log("INFO", paste("White test completed: statistic =", round(test_statistic, 4),
                       "df =", df, "p =", round(p_value, 4)))

  return(result)
}

# White's test as it was computed before the model context: `data`, or the
# model frame, prepared by rdata_or_model_frame(), rhandleMissingValues() and
# a match by row name, and the design rebuilt from it with
# model.matrix(model, data = working_data). performWhiteTestBootstrap() uses
# it for its observed statistic and for its replicates, which it refits from
# the formula, so that both come from the same kind of design and its results
# are those of 0.15.0. Increment 8 of the model context, which builds the
# replicates on the stored design, removes this function (docs/design/
# model-context.md, "Migration plan").
rwhite_test_rebuilt_design <- function(model, data = NULL, cross_products = TRUE,
                                       max_interactions = 10) {
  rrefuse_non_gaussian_glm(model, "performWhiteTest")
  data <- rdata_or_model_frame(model, data)
  test_label <- "White test"
  rvalidateModelInputs(model, test_name = "White", min_obs = 20L, allow_aliased = TRUE)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = 20L)

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

  align_to_model <- function(clean_data, residuals) {
    resid_names <- names(residuals)
    data_rows <- rownames(clean_data)

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
      # `data` and `cleaned` are those of the enclosing call: the rows before
      # and after rhandleMissingValues().
      rcheck_rows_after_cleaning(
        data, cleaned, match_idx, resid_names,
        fn = rtest_name(sys.parent()), test_label = test_label
      )
      aligned_data <- clean_data[match_idx, , drop = FALSE]
      list(data = aligned_data, residuals = residuals)
    } else {
      if (nrow(clean_data) != length(residuals)) {
        stop(
          sprintf(
            "%s requires `data` with %d observations to match the fitted model, got %d.",
            test_label,
            length(residuals),
            nrow(clean_data)
          ),
          call. = FALSE
        )
      }
      list(data = clean_data, residuals = residuals)
    }
  }

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  aligned <- align_to_model(cleaned$data, rpearson_residuals(model))
  working_data <- aligned$data
  residuals <- aligned$residuals

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  handle_validation_result(requirements)

  if (!is.logical(cross_products) || length(cross_products) != 1) {
    .std_error("invalid_logical", arg = "cross_products")
  }

  # A warning above 50 MB; up to 0.15.0 the observed statistic and the
  # replicates of the White bootstrap switched to the streaming test above
  # 100 MB, as performWhiteTest() did.
  tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NULL
  )
  if (nrow(working_data) > 10000) {
    message("Large dataset (", nrow(working_data), " observations). ",
            "This may take some time to compute.")
  }

  ht_log("INFO", "Running White test")

  X_full <- rnon_aliased_columns(stats::model.matrix(model, data = working_data), model)
  rwhite_statistic(residuals, X_full, cross_products, max_interactions, test_label,
                   data_name = deparse(substitute(model)))
}

performWhiteTestEnhanced <- function(model, data, cross_products = TRUE, 
                                   bootstrap = FALSE, B = 999, 
                                   parallel = FALSE, alpha = 0.05) {
  
  # Run standard White test
  result <- performWhiteTest(model, data, cross_products = cross_products)
  
  # Add bootstrap p-value if requested
  if (bootstrap) {
    ht_log("INFO", paste("Computing bootstrap p-value with", B, "replications"))
    
    bootstrap_stats <- bootstrap_white_test(model, data, B, parallel, cross_products)
    
    # Bootstrap p-value
    # Finite-simulation convention: (1 + #) / (B_eff + 1).
    finite_stats <- bootstrap_stats[is.finite(bootstrap_stats)]
    boot_effective <- length(finite_stats)
    boot_p_value <- if (boot_effective == 0L) {
      NA_real_
    } else {
      (1 + sum(finite_stats >= unname(result$statistic[1]))) / (boot_effective + 1)
    }
    
    result$p.value.bootstrap <- boot_p_value
    result$method <- paste(result$method, "(with bootstrap)")
    result$bootstrap_statistics <- bootstrap_stats
    
    # Bootstrap confidence interval for the test statistic
    result$bootstrap_ci <- quantile(bootstrap_stats, c(alpha/2, 1-alpha/2))
  }
  
  # Add interpretation
  result$interpretation <- interpret_white_test(result, alpha)
  
  return(result)
}

#' Bootstrap implementation for White test
#' @keywords internal
bootstrap_white_test <- function(model, data, B, parallel, cross_products) {
  rrefuse_weighted_fit(model, "The bootstrap White test")
  
  fitted_vals <- fitted(model)
  original_residuals <- residuals(model)
  response_var <- all.vars(formula(model))[1]
  
  # Bootstrap function
  bootstrap_stat <- function(i) {
    # Resample residuals under null hypothesis (homoscedasticity)
    boot_residuals <- sample(original_residuals, replace = TRUE)
    boot_y <- fitted_vals + boot_residuals
    
    # Create bootstrap dataset
    boot_data <- data
    boot_data[[response_var]] <- boot_y
    
    # Fit bootstrap model
    boot_model <- tryCatch({
      lm(formula(model), data = boot_data)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(boot_model)) return(NA)
    
    # Calculate White test statistic for bootstrap sample
    boot_result <- tryCatch({
      performWhiteTest(boot_model, boot_data, cross_products = cross_products)
    }, error = function(e) {
      return(list(statistic = NA))
    })
    
    return(as.numeric(boot_result$statistic))
  }
  
  # Run bootstrap
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    n_cores <- min(parallel::detectCores() - 1, 4)  # Use at most 4 cores
    boot_stats <- parallel::mclapply(1:B, bootstrap_stat, mc.cores = n_cores)
    boot_stats <- unlist(boot_stats)
  } else {
    boot_stats <- replicate(B, bootstrap_stat(1))
  }
  
  # Remove any NAs from failed bootstrap samples
  boot_stats <- boot_stats[!is.na(boot_stats)]
  
  if (length(boot_stats) < B * 0.8) {
    warning("More than 20% of bootstrap samples failed. Results may be unreliable.")
  }
  
  return(boot_stats)
}

#' Interpret White test results
#' @keywords internal
interpret_white_test <- function(result, alpha = 0.05) {
  p_val <- result$p.value
  statistic <- result$statistic
  
  interpretation <- list()
  
  # Significance level
  if (p_val < 0.001) {
    interpretation$significance <- "Very strong evidence of heteroscedasticity (p < 0.001)"
  } else if (p_val < 0.01) {
    interpretation$significance <- "Strong evidence of heteroscedasticity (p < 0.01)"
  } else if (p_val < alpha) {
    interpretation$significance <- paste0("Evidence of heteroscedasticity (p < ", alpha, ")")
  } else if (p_val < 0.1) {
    interpretation$significance <- "Weak evidence of heteroscedasticity (p < 0.10)"
  } else {
    interpretation$significance <- "No evidence of heteroscedasticity"
  }
  
  # Effect size (rough approximation)
  n_obs <- attr(result, "original_regressors") # This needs to be fixed - should be n
  if (!is.null(attr(result, "r_squared_auxiliary"))) {
    r2_aux <- attr(result, "r_squared_auxiliary") 
    interpretation$effect_size <- ifelse(r2_aux < 0.02, "small",
                                       ifelse(r2_aux < 0.15, "medium", "large"))
  }
  
  # Recommendations
  if (p_val < alpha) {
    interpretation$recommendation <- c(
      "Consider heteroscedasticity-robust standard errors",
      "Examine residual plots for patterns",
      "Consider variance-stabilizing transformations",
      "Try weighted least squares if variance pattern is identifiable"
    )
  } else {
    interpretation$recommendation <- "No remedial action needed for heteroscedasticity"
  }
  
  return(interpretation)
}

#' Print method for enhanced White test results
#' @export
print.htest_enhanced <- function(x, ...) {
  # Use standard htest print method first
  NextMethod()
  
  # Add enhanced information
  if (!is.null(x$interpretation)) {
    cat("\nInterpretation:\n")
    cat("  ", x$interpretation$significance, "\n")
    
    if (!is.null(x$interpretation$effect_size)) {
      cat("   Effect size:", x$interpretation$effect_size, "\n")
    }
    
    if (!is.null(x$interpretation$recommendation)) {
      cat("\nRecommendations:\n")
      for (rec in x$interpretation$recommendation) {
        cat("  -", rec, "\n")
      }
    }
  }
  
  if (!is.null(x$p.value.bootstrap)) {
    cat("\nBootstrap p-value:", round(x$p.value.bootstrap, 4), "\n")
  }
}
