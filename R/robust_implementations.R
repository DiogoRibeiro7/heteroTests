#' Robust heteroscedasticity diagnostics
#'
#' Provides enhanced implementations of core heteroscedasticity diagnostics
#' with bootstrap support, effect size calculations, and statistical power
#' summaries. The module also exposes orchestration helpers for running the
#' enhanced diagnostics in batch and utilities for validating results against
#' reference implementations from other packages.
#'
#' @name robust_implementations
NULL

.extract_df <- function(test_result, model) {
  if (!is.null(test_result$parameter)) {
    df <- suppressWarnings(as.numeric(test_result$parameter[[1L]]))
    if (is.finite(df) && df > 0) {
      return(df)
    }
  }
  max(1, sum(!is.na(stats::coef(model))) - 1L)
}

.asymptotic_ci <- function(df, level) {
  lower_tail <- (1 - level) / 2
  stats::setNames(
    c(
      stats::qchisq(lower_tail, df),
      stats::qchisq(1 - lower_tail, df)
    ),
    c("lower", "upper")
  )
}

#' Robust White test with bootstrap and effect sizes
#'
#' Extends [performWhiteTest()] with optional bootstrap resampling, confidence
#' intervals, effect size reporting, and power analysis.
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
#' @param method Character string selecting the auxiliary specification.
#'   "standard" retains squares and cross-products, while "reduced"
#'   excludes cross-products for high-dimensional designs.
#' @param bootstrap Logical, compute bootstrap diagnostics for the
#'   statistic and p-value? The bootstrap is [bootstrapTestStatistic()], so on
#'   a fit returned by [fitWLS()] each replicate estimates the weights again,
#'   its default since 0.15.0. There is no argument to hold them fixed here;
#'   [bootstrapTestStatistic()] with `reestimate_weights = FALSE` does that.
#' @param B Number of bootstrap replications when `bootstrap = TRUE`.
#'   Defaults to 999, at which a bootstrap p-value of the form
#'   `(1 + #) / (B + 1)` gives an exact test at the 5% and 1% levels; it was
#'   1000 before 0.15.0. The value is recorded in `robust_details$bootstrap$B`
#'   whether or not the bootstrap runs.
#' @param ci_level Confidence level for reported intervals.
#' @param parallel Logical, allow parallel bootstrap evaluation when the
#'   `parallel` package is available. A seeded call gives the same result
#'   each time; see *Parallel replicates* in [bootstrapTestStatistic()].
#'
#' @return An object of class \code{htest} augmented with a
#'   `robust_details` list containing bootstrap, effect size, and power
#'   information.
#'
#' @details
#' Provides enriched inference around the White test by combining bootstrap
#' resampling (Efron & Tibshirani, 1993) with asymptotic approximations. The
#' `robust_details` element summarises interval estimates, effect sizes, and power
#' calculations to aid decision-making.
#'
#' @references
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroscedasticity. *Econometrica, 48*(4), 817–838.
#'
#' Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the Bootstrap*.
#' Chapman & Hall.
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit. An `mgcv::gam()` fit with penalised
#' smooth terms is tested on its design, the basis of its smooths, as by
#' [performWhiteTest()], and refused with `bootstrap = TRUE` (since 0.17.0;
#' before, the bootstrap refused every `gam` with an error of its own): each
#' replicate fits that design again by least squares, which a penalised fit is
#' not. A `gam` that is the least-squares fit of its design, as one whose
#' smooth terms are all fixed (`s(x, fx = TRUE)`) or one without smooth terms,
#' is bootstrapped too, as the `lm()` of that design (decisions 37 and 38 of
#' the design of the model context). Which model each test accepts, and why,
#' is set out in [test_results], under *Supported models*.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTestRobust(mod, mtcars, bootstrap = TRUE, B = 200)
#'
#' @seealso
#' [performWhiteTest()] for the base statistic and
#' [performWhiteTestBootstrap()] for a lighter-weight resampling option.
#'
#' @export
performWhiteTestRobust <- function(model, data = NULL, method = c("standard", "reduced"),
                                   bootstrap = FALSE, B = 999, ci_level = 0.95,
                                   parallel = FALSE) {
  fn <- "performWhiteTestRobust"
  test_label <- "White (robust)"
  rrefuse_non_gaussian_glm(model, fn)
  # The response, design, weights, offset and residuals come from the fit;
  # `data`, when given, is matched to its observations and checked
  # (R/model_context.R). Before increment 8 of the model context the test
  # prepared `data`, or the model frame, itself, and a model that transforms
  # a variable, or has an offset() term, needed `data`.
  ctx <- rmodel_context(model, data, fn = fn, test_label = test_label)
  method <- match.arg(method)

  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap)) {
    stop("`bootstrap` must be a single logical value.", call. = FALSE)
  }

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.numeric(ci_level) || length(ci_level) != 1L || is.na(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }

  # The bootstrap refits each replicate by least squares: a penalised
  # mgcv::gam() fit is refused here, under the name of this test, as by
  # bootstrapTestStatistic() (decision 37 of the design of the model context).
  if (isTRUE(bootstrap)) {
    rrefuse_penalised_gam(ctx, fn, "replicate")
  }
  rvalidateModelInputs(model, test_name = test_label, min_obs = 20L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  # A zero weight is refused here, as before.
  rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit
  # and under the names of its observations, or the model frame: what the
  # checks below read, and what the base statistic and the bootstrap are
  # given.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  rprocessValidationResult(requirements)

  cross_products <- identical(method, "standard")
  base_result <- performWhiteTest(model, working_data, cross_products = cross_products)

  df <- .extract_df(base_result, model)
  ci_info <- list(
    estimate = .asymptotic_ci(df, ci_level),
    level = ci_level,
    source = "asymptotic"
  )

  bootstrap_details <- list(
    enabled = FALSE,
    B = B,
    p_value = NA_real_,
    ci = NULL,
    replicates = NULL,
    effective_samples = NA_integer_,
    ci_level = ci_level
  )

  if (isTRUE(bootstrap)) {
    boot <- bootstrapTestStatistic(
      performWhiteTest,
      model,
      working_data,
      B = B,
      parallel = parallel,
      ci_level = ci_level,
      cross_products = cross_products
    )
    base_result$p.value_bootstrap <- boot$p_value
    ci_info$estimate <- boot$ci
    ci_info$source <- "bootstrap"
    bootstrap_details <- list(
      enabled = TRUE,
      B = B,
      p_value = boot$p_value,
      ci = boot$ci,
      replicates = boot$replicates,
      effective_samples = boot$effective_samples,
      ci_level = boot$ci_level
    )
  }

  effect <- .calculate_effect_size(base_result, model, working_data)
  power <- .estimate_test_power(base_result, model, working_data, alpha = 1 - ci_level)

  base_result$method <- paste(base_result$method, "(robust)")
  base_result$robust_details <- list(
    method = method,
    confidence_interval = ci_info,
    effect_size = effect,
    bootstrap = bootstrap_details,
    power = power,
    sample_size = nrow(working_data)
  )
  base_result
}

#' Robust Breusch–Pagan test with bootstrap and effect sizes
#'
#' Enhances [performBPTest()] by optionally studentising residuals, resampling the
#' test statistic via bootstrap, and reporting effect sizes together with power
#' diagnostics.
#'
#' @inheritParams performBPTest
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
#' @param studentize Logical, use studentized residuals for the auxiliary
#'   regression (Koenker variant)? Defaults to `TRUE`. The name matches
#'   `lmtest::bptest()` and [performHarveyTest()].
#' @inheritParams performWhiteTestRobust
#' @param studentized Deprecated since 0.15.0: use `studentize`. It is still
#'   accepted, with a warning, and does what `studentize` does; it stops
#'   working in 1.0.0.
#'
#' @return An augmented \code{htest} object containing a
#'   `robust_details` element describing the additional diagnostics. Its
#'   `studentized` field records the value of `studentize`.
#'
#' @details
#' Offers expanded reporting for the Breusch–Pagan family of tests including
#' bootstrap p-values and asymptotic confidence intervals. The optional
#' `studentize` argument toggles between the classic and Koenker versions.
#'
#' @references
#' Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity
#' and random coefficient variation. *Econometrica, 47*(5), 1287–1294.
#'
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#'
#' @section Supported models:
#' It accepts an `lm` and a `glm` with the Gaussian family and the identity link,
#' unweighted or with positive weights, a `fitWLS()` fit and, since 0.16.0, a
#' rank-deficient fit, which it tests on the estimable part of its design: it gives
#' the value of the same model fitted without the aliased columns. It refuses a
#' `glm` of another family or link, a `survey::svyglm()` fit, a fit with a zero
#' weight, a coefficient that is not finite for another reason, and `data` that
#' lack rows or variables of the fit. It accounts for an offset with
#' `studentize = FALSE`, and since 0.16.0 with `studentize = TRUE`, the default,
#' whose refit of the design carries the offset of the fit.
#' It refuses a multivariate `lm` ("needs a model with one response"), and,
#' since 0.16.0, with `studentize = TRUE`, the default, an `mgcv::gam()` fit
#' with penalised smooth terms, which is not the least-squares fit of its
#' design, the basis of its smooths, that the studentized statistic fits again,
#' and, since 0.17.0, such a fit with `bootstrap = TRUE`, whose replicates are
#' fitted again. With `studentize = FALSE` and no bootstrap it tests such a fit
#' on its design, as `performBPTest()` does. Since 0.17.0 a `gam` that is the
#' least-squares fit of its design, as one whose smooth terms are all fixed
#' (`s(x, fx = TRUE)`), is tested on that design and gives the value of the
#' `lm()` of it; before, it was refused as a penalised one. A `gam` without
#' smooth terms is tested as the Gaussian `glm` it is (decisions 37 and 38 of
#' the design of the model context).
#' Which model each
#' test accepts, and why, is set out in [test_results], under *Supported models*.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performBPTestRobust(mod, mtcars, bootstrap = TRUE, B = 200)
#'
#' @seealso
#' [performBPTest()] for the baseline test and [performKoenkerTest()] for the
#' standalone studentised variant.
#'
#' @export
performBPTestRobust <- function(model, data = NULL, studentize = TRUE,
                                bootstrap = FALSE, B = 999, ci_level = 0.95,
                                parallel = FALSE, studentized) {
  fn <- "performBPTestRobust"
  test_label <- "Breusch-Pagan (robust)"
  rrefuse_non_gaussian_glm(model, fn)
  # The response, design, weights, offset and residuals come from the fit;
  # `data`, when given, is matched to its observations and checked
  # (R/model_context.R). Before increment 8 of the model context the test
  # prepared `data`, or the model frame, itself, and a model that transforms
  # a variable, or has an offset() term, needed `data`.
  ctx <- rmodel_context(model, data, fn = fn, test_label = test_label)
  if (!missing(studentized)) {
    if (!missing(studentize)) {
      stop("Supply `studentize` only; `studentized` is its deprecated name.", call. = FALSE)
    }
    .Deprecated(
      old = "performBPTestRobust",
      msg = paste0(
        "The argument `studentized` of performBPTestRobust() is deprecated ",
        "and stops working in heteroTests 1.0.0.\n",
        "Use `studentize` instead, as in lmtest::bptest()."
      )
    )
    studentize <- studentized
  }
  if (!is.logical(studentize) || length(studentize) != 1L || is.na(studentize)) {
    stop("`studentize` must be a single logical value.", call. = FALSE)
  }

  if (!is.logical(bootstrap) || length(bootstrap) != 1L || is.na(bootstrap)) {
    stop("`bootstrap` must be a single logical value.", call. = FALSE)
  }

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.numeric(ci_level) || length(ci_level) != 1L || is.na(ci_level) ||
      ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be between 0 and 1.", call. = FALSE)
  }

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }

  # The bootstrap refits each replicate by least squares: a penalised
  # mgcv::gam() fit is refused here, under the name of this test, as by
  # bootstrapTestStatistic() (decision 37 of the design of the model context).
  if (isTRUE(bootstrap)) {
    rrefuse_penalised_gam(ctx, fn, "replicate")
  }
  rvalidateModelInputs(model, test_name = test_label, min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  # A zero weight is refused here, as before.
  rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit
  # and under the names of its observations, or the model frame: what the
  # checks below read, and what the base statistic and the bootstrap are
  # given.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("breusch_pagan", model = model, data = working_data)
  rprocessValidationResult(requirements)

  base_fun <- if (studentize) .studentized_bp_test else performBPTest
  base_result <- base_fun(model, working_data)

  df <- .extract_df(base_result, model)
  ci_info <- list(
    estimate = .asymptotic_ci(df, ci_level),
    level = ci_level,
    source = "asymptotic"
  )

  bootstrap_details <- list(
    enabled = FALSE,
    B = B,
    p_value = NA_real_,
    ci = NULL,
    replicates = NULL,
    effective_samples = NA_integer_,
    ci_level = ci_level
  )

  if (isTRUE(bootstrap)) {
    boot <- bootstrapTestStatistic(
      base_fun,
      model,
      working_data,
      B = B,
      parallel = parallel,
      ci_level = ci_level
    )
    base_result$p.value_bootstrap <- boot$p_value
    ci_info$estimate <- boot$ci
    ci_info$source <- "bootstrap"
    bootstrap_details <- list(
      enabled = TRUE,
      B = B,
      p_value = boot$p_value,
      ci = boot$ci,
      replicates = boot$replicates,
      effective_samples = boot$effective_samples,
      ci_level = boot$ci_level
    )
  }

  effect <- .calculate_effect_size(base_result, model, working_data)
  power <- .estimate_test_power(base_result, model, working_data, alpha = 1 - ci_level)

  descriptor <- if (studentize) "Studentized" else "Classical"
  base_result$method <- paste(base_result$method, "(robust", descriptor, ")")
  # The field keeps its name, so that the returned object does not change.
  base_result$robust_details <- list(
    studentized = studentize,
    confidence_interval = ci_info,
    effect_size = effect,
    bootstrap = bootstrap_details,
    power = power,
    sample_size = nrow(working_data)
  )
  base_result
}

#' Run enhanced heteroscedasticity diagnostics
#'
#' Executes one or more robust diagnostics, with an automatic bootstrap for
#' small samples. The Breusch-Pagan entry is Koenker's studentized statistic,
#' `performBPTestRobust(studentize = TRUE)`.
#'
#' @inheritParams performWhiteTestRobust
#' @param data A [base::data.frame] (or object coercible to one) containing the
#'   variables referenced by `model`. It must include the observations used to
#'   fit `model` and will be checked for missing values.
#' @param tests Character vector of tests to run. Use "all" (default) to
#'   run the White and Breusch-Pagan diagnostics, or supply a subset such
#'   as `c("white", "bp")`: `"bp"` is another name for `"breusch_pagan"`, and
#'   the result is named `breusch_pagan` either way. Before 0.16.0 `"bp"` was
#'   refused ("Unsupported test requested: bp").
#' @param auto_enhance Logical, enable the automatic bootstrap for small
#'   samples (n < 50). It does nothing else: the Breusch-Pagan entry is the
#'   studentized statistic either way. Before 0.16.0 `auto_enhance = FALSE`
#'   also turned the studentization off and ran the classical Breusch-Pagan
#'   test, which assumes normal errors; [performBPTestRobust()] with
#'   `studentize = FALSE` still runs it.
#' @param bootstrap_B Number of bootstrap replications used when automatic
#'   enhancement triggers bootstrap. Defaults to 999, the default of every
#'   resampling test in the package since 0.15.0; it was 500 before. On a fit
#'   returned by [fitWLS()] the bootstrap of [performWhiteTestRobust()] and
#'   [performBPTestRobust()] estimates the weights again in each replicate, the
#'   default of [bootstrapTestStatistic()] since 0.15.0, and there is no
#'   argument to hold them fixed here.
#'
#' @return A list with the executed results and metadata describing the
#'   enhancements that were applied.
#'
#' @export
runAdvancedDiagnostics <- function(model, data, tests = "all",
                                   auto_enhance = TRUE, bootstrap_B = 999,
                                   parallel = FALSE, ci_level = 0.95) {
  rvalidateModelInputs(
    model, test_name = "Advanced diagnostics", min_obs = 10L, allow_aliased = TRUE
  )
  rvalidateDataInputs(data, min_obs = 10L)

  if (!is.logical(auto_enhance) || length(auto_enhance) != 1L || is.na(auto_enhance)) {
    stop("`auto_enhance` must be a single logical value.", call. = FALSE)
  }

  if (!is.numeric(bootstrap_B) || length(bootstrap_B) != 1L || is.na(bootstrap_B) || bootstrap_B < 1) {
    stop("`bootstrap_B` must be a positive integer.", call. = FALSE)
  }
  bootstrap_B <- as.integer(bootstrap_B)

  # The names accepted, each with the test it runs. The names asked for are
  # matched to the names of the table, since 0.16.0; they were matched to its
  # values, so "bp", which is not one, was refused.
  available <- c(white = "white", bp = "breusch_pagan", breusch_pagan = "breusch_pagan")
  if (identical(tests, "all")) {
    tests <- c("white", "breusch_pagan")
  }
  tests <- unique(tolower(tests))
  if (!all(tests %in% names(available))) {
    stop(
      "Unsupported test requested: ", paste(setdiff(tests, names(available)), collapse = ", "),
      call. = FALSE
    )
  }
  tests <- unique(unname(available[tests]))

  n <- nrow(data)
  results <- list()

  if ("white" %in% tests) {
    use_boot <- auto_enhance && n < 50
    results$white <- performWhiteTestRobust(
      model,
      data,
      bootstrap = use_boot,
      B = bootstrap_B,
      ci_level = ci_level,
      parallel = parallel
    )
  }

  if ("breusch_pagan" %in% tests) {
    use_boot <- auto_enhance && n < 50
    # Koenker's studentized statistic whatever `auto_enhance` is, since
    # 0.16.0: `auto_enhance` turns the small-sample bootstrap on and off and
    # nothing else. It also set `studentize` before, so that turning the
    # bootstrap off ran the classical test, which assumes normal errors.
    results$breusch_pagan <- performBPTestRobust(
      model,
      data,
      studentize = TRUE,
      bootstrap = use_boot,
      B = bootstrap_B,
      ci_level = ci_level,
      parallel = parallel
    )
  }

  list(
    results = results,
    metadata = list(
      auto_enhance = auto_enhance,
      sample_size = n,
      tests = names(results),
      bootstrap_B = bootstrap_B
    )
  )
}

#' @rdname runAdvancedDiagnostics
#' @section Deprecated name:
#' `rrunAdvancedDiagnostics()` is deprecated since 0.15.0: use
#' `runAdvancedDiagnostics()`, the same function under the package's naming
#' scheme. The old name still works, returns what it returned and warns; it
#' leaves the exports in 1.0.0.
#' @export
rrunAdvancedDiagnostics <- function(model, data, tests = "all",
                                    auto_enhance = TRUE, bootstrap_B = 999,
                                    parallel = FALSE, ci_level = 0.95) {
  .Deprecated(
    old = "rrunAdvancedDiagnostics",
    msg = paste0(
      "'rrunAdvancedDiagnostics' is deprecated and leaves the exports in ",
      "heteroTests 1.0.0.\nUse 'runAdvancedDiagnostics' instead.\n",
      "See help(\"Deprecated\")"
    )
  )
  runAdvancedDiagnostics(
    model, data,
    tests = tests, auto_enhance = auto_enhance, bootstrap_B = bootstrap_B,
    parallel = parallel, ci_level = ci_level
  )
}

#' Validate diagnostics against reference implementations
#'
#' Compares the package's test results with established implementations
#' from other packages (currently `lmtest` and `car`). Differences exceeding
#' the supplied tolerance are flagged.
#'
#' @param test_name Character scalar naming the diagnostic: "breusch_pagan",
#'   "studentized_bp", or "white".
#' @inheritParams performWhiteTestRobust
#' @param data A [base::data.frame] (or object coercible to one) containing the
#'   variables referenced by `model`. It must include the observations used to
#'   fit `model` and will be checked for missing values.
#' @param tolerance Acceptable absolute difference between statistics and
#'   p-values when comparing to the reference implementation.
#'
#' @return A list describing the comparison, including a status field with
#'   values `"ok"`, `"mismatch"`, or `"skipped"` when the reference package
#'   is unavailable.
#'
#' @section Deprecated:
#' Deprecated since 0.14.0: an internal helper of the package, which leaves the
#' exports in 1.0.0. It still works and warns.
#' @export
rvalidate_against_reference <- function(test_name, model, data, tolerance = 1e-6) {
  .Deprecated(
    old = "rvalidate_against_reference",
    msg = .ht_r_prefixed_deprecation("rvalidate_against_reference")
  )
  .validate_against_reference(test_name, model, data, tolerance)
}

.validate_against_reference <- function(test_name, model, data, tolerance = 1e-6) {
  if (missing(test_name) || length(test_name) != 1L || !is.character(test_name) || is.na(test_name)) {
    stop("`test_name` must be a single character string.", call. = FALSE)
  }

  rvalidateModelInputs(
    model, test_name = "Reference validation", min_obs = 5L, allow_aliased = TRUE
  )
  rvalidateDataInputs(data, min_obs = 5L)

  key <- gsub("[^a-z]", "_", tolower(test_name))
  if (key %in% c("bp", "breusch_pagan_test")) {
    key <- "breusch_pagan"
  }
  supported <- c("breusch_pagan", "studentized_bp", "white")
  if (!key %in% supported) {
    stop("Unsupported `test_name`: ", test_name, call. = FALSE)
  }

  tolerance <- abs(as.numeric(tolerance))
  if (is.na(tolerance)) {
    tolerance <- 1e-6
  }

  # The reference implementations read residuals() and weights(), which a fit
  # with na.exclude pads: they are given the same fit read with na.omit, which
  # drops the same rows (rmodel_as_na_omit(), B13); lmtest::bptest() stopped
  # on a weighted such fit with "incompatible dimensions".
  reference_model <- rmodel_as_na_omit(model)

  compare_results <- function(ours, reference_stat, reference_p) {
    stat_diff <- abs(unname(ours$statistic[[1L]]) - reference_stat)
    p_diff <- abs(ours$p.value - reference_p)
    status <- if (stat_diff <= tolerance && p_diff <= tolerance) "ok" else "mismatch"
    list(
      status = status,
      statistic_difference = stat_diff,
      p_value_difference = p_diff,
      ours = ours,
      reference = list(statistic = reference_stat, p.value = reference_p)
    )
  }

  if (key == "breusch_pagan") {
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(list(status = "skipped", message = "Package 'lmtest' not installed."))
    }
    reference <- lmtest::bptest(reference_model, studentize = FALSE)
    ours <- performBPTest(model, data)
    return(compare_results(ours, unname(reference$statistic), reference$p.value))
  }

  if (key == "studentized_bp") {
    if (!requireNamespace("lmtest", quietly = TRUE)) {
      return(list(status = "skipped", message = "Package 'lmtest' not installed."))
    }
    reference <- lmtest::bptest(reference_model, studentize = TRUE)
    # The other two keys call the exported test, which refuses a glm that is
    # not a least-squares fit; this one calls the implementation behind
    # performStudentizedBPTest() and refuses it as that function does, since
    # 0.15.0.
    rrefuse_non_gaussian_glm(model, "performStudentizedBPTest")
    ours <- .studentized_bp_test(model, data)
    return(compare_results(ours, unname(reference$statistic), reference$p.value))
  }

  if (!requireNamespace("car", quietly = TRUE)) {
    return(list(status = "skipped", message = "Package 'car' not installed."))
  }
  reference <- car::ncvTest(reference_model)
  ours <- performWhiteTest(model, data)
  reference_stat <- unname(reference$ChiSq)
  reference_p <- reference$p
  compare_results(ours, reference_stat, reference_p)
}
