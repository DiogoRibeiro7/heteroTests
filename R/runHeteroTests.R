#' Run multiple heteroscedasticity tests
#'
#' This convenience wrapper runs diagnostic functions registered in the
#' package registry and returns their results as a list. By default it
#' executes White's test and Koenker's studentized Breusch-Pagan test, but
#' additional tests or custom diagnostics registered via `registerDiagnostic()`
#' can be requested.
#'
#' Up to 0.14.0 the default was `c("white", "breusch_pagan")`, whose second
#' test is the classical Breusch-Pagan statistic of [performBPTest()]. That
#' statistic divides by `2 sigma^4`, the variance of the squared errors only
#' when they are normal. In the full sweep of `inst/validation/`, with 150
#' observations and 400 replications, it rejected a true null hypothesis
#' 28.75% of the time at the 5% level when the errors had a `t5`
#' distribution, against 4.75% for Koenker's statistic. The classical test is
#' still run by name, `tests = "breusch_pagan"`.
#'
#' The runner runs the exact tests unless `chunk_threshold_mb` asks for the
#' streaming ones, [performWhiteTestStreaming()], [performBPTestStreaming()]
#' and [performKoenkerTestStreaming()]. Up to 0.15.0 it switched to them by
#' itself on data above 100 MB, and [performWhiteTest()], [performBPTest()]
#' and [performKoenkerTest()] did so on their own. Streaming saves memory
#' mainly in White's test, whose auxiliary design is several times wider than
#' the model matrix; the auxiliary design of the Breusch-Pagan and Koenker
#' tests is the model matrix itself. And until 0.17.0 the streaming White test
#' stopped on any model with a factor or a `poly()` term, whose squares are
#' collinear with the other terms of its auxiliary regression, so that above
#' 100 MB such a model got a fallback under the name `"white"` where smaller
#' data got White's test; since 0.17.0 it drops those terms, as
#' [performWhiteTest()] does.
#'
#' @param model A fitted `lm`, or a `glm` with the Gaussian family and the
#'   identity link, a model formula, a tidymodels [workflows::workflow] or a
#'   [parsnip::model_fit] object. The tests refuse a `glm` of any other
#'   family or link since 0.15.0.
#' @param data Optional data source used to fit `model`. Accepts base data
#'   frames, tibbles, `data.table`s, `dtplyr_step`s and grouped data produced by
#'   [dplyr::group_by()]. When `model` is a formula the data must be supplied.
#'   With grouped data the model is refitted within each group by evaluating
#'   its own call on that group, so every variable it uses, including weights,
#'   offset and subset, must be a column of `data`. A fit returned by
#'   [fitWLS()] is refused with grouped data since 0.17.0: its call does not
#'   hold the weights it estimated, so each group was tested as the unweighted
#'   fit of its rows; so is a `MASS::rlm()` fit, which every test refuses. A
#'   test that refuses the model of a group as the model context refuses a
#'   model, such as a multivariate `lm` or a penalised `mgcv::gam()` fit given
#'   a test that needs least squares, stops the grouped run with that refusal
#'   and its class since 0.17.0, as the run on ungrouped data does; before,
#'   the group recorded it as a failed test, whose statistic was `NA`. The
#'   failures of a test for another reason are recorded in its group, as
#'   before. The data a tidymodels workflow was fitted to are mapped
#'   to the variables of its engine by the workflow's preprocessing, and the
#'   model frame of the engine's fit is tested, as without `data` (since
#'   0.17.0; they were refused). A recipe step skipped on new data that
#'   changed the outcome or a predictor at the fit, such as
#'   `step_log(y, skip = TRUE)`, is named in the refusal of such data, and the
#'   data of a [parsnip::fit_xy()] fit with case weights, whose column the
#'   fit does not record, are refused. Each result keeps what
#'   [generics::augment()] needs to map the data in the same way, as
#'   `attr(, "tidymodels")`, the blueprint as the workflow keeps it: a recipe
#'   step keeps the environment it was written in, so that a saved result
#'   carries what that environment holds, as saving the workflow does.
#' @param tests Character vector of test names to run: any of the names
#'   [listDiagnostics()] returns, which include those added with
#'   `registerDiagnostic()`. On a fit returned by [fitWLS()] the resampling
#'   tests `"wild_bootstrap"` and `"white_bootstrap"` estimate the weights
#'   again in each replicate, as [performWildBootstrapTest()] and
#'   [performWhiteTestBootstrap()] do by default since 0.15.0; to hold them
#'   fixed, call those functions with `reestimate_weights = FALSE`.
#' @param use_cache Logical, reuse the results of an earlier call on the same
#'   inputs? Requires the **digest** package for hashing inputs and defaults
#'   to `TRUE`. Only deterministic tests are cached, under a key that holds the
#'   test and the parts of the model and the data it reads (see
#'   [cachedTest()], which also says what the key leaves out): the resampling
#'   tests `"wild_bootstrap"`, `"white_bootstrap"`, `"rank_permutation"` and
#'   `"spatial_hetero"`, and diagnostics added with `registerDiagnostic()`, run
#'   on every call, and a call that runs one, or falls back on one, is not
#'   stored as a whole. A seeded call returns what it returns with
#'   `use_cache = FALSE`, `identical()`, and leaves the random number generator
#'   where it leaves it. Each test is kept in the cache of [cachedTest()], and
#'   each call whose tests are all deterministic in a second cache
#'   ([clearAnalysisCache()]); each holds at most
#'   `getOption("heteroTests.cache_size", 100)` results and drops the one used
#'   least recently, and `options(heteroTests.cache_size = 0)` turns both off.
#'   The checks the tests make first have a cache of their own, bounded by
#'   `getOption("heteroTests.validation_cache_size", 1000)`. Before 0.16.0 the
#'   resampling tests were cached too, and two fits with the same coefficients
#'   and residuals shared their results.
#' @param chunk_threshold_mb Size of `data` in megabytes above which
#'   `"white"`, `"breusch_pagan"` and `"koenker"` are run by their streaming
#'   forms. The default, `Inf`, never streams: give a finite value to ask for
#'   them, such as `100`, or `0` for data of any size. The result of a
#'   streaming test has `attr(, "extras")$streaming` set to `TRUE`. Up to
#'   0.15.0 the default was `100`. A warning about the size of the data is
#'   given above half the threshold, or above 50 MB with the default.
#' @param chunk_size Integer number of observations processed per chunk when
#'   the streaming tests run (`chunk_threshold_mb`).
#' @param progress Logical flag controlling whether textual progress bars are
#'   displayed while running multiple diagnostics.
#'
#' @return A [`hetero_test_suite`] list of diagnostic results, which prints as
#'   a table of the tests and whose `summary()` is laid out as that of an `lm`
#'   ([summary.hetero_test_suite()]). When grouped data are supplied the return
#'   value is a [`hetero_grouped_suite`] containing a per-group
#'   `hetero_test_suite`. A suite whose model the runner fitted itself, from a
#'   formula, to the data of a survey design or within a group, says so in
#'   `attr(, "model_origin")` (`"formula"`, `"survey"` or `"group"`). A result
#'   whose `data.name` is `"model"` reports the
#'   formula of the model instead: up to 0.15.0 White's test, its bootstrap
#'   and its form without cross products, the studentized Breusch-Pagan
#'   statistic and Szroeter's test reported `"model"` here.
#'
#' @seealso \code{\link{runDiagnostics}} for a broader suite that includes
#'   collinearity and influence measures, \code{\link{runPanelTests}} for panel
#'   data models and \code{\link{runTimeSeriesTests}} for time-series checks.
#'
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' runHeteroTests(m, mtcars)
#' summary(runHeteroTests(m, mtcars))
#' registerDiagnostic("my_stat", function(model, data) list(statistic = 0))
#' runHeteroTests(m, mtcars, tests = c("white", "my_stat"))
#'
#' # Combine with advanced residual analysis
#' adv <- analyzeMLResiduals(m, mtcars)
runHeteroTests <- function(model, data = NULL,
                           tests = c("white", "koenker"),
                           use_cache = TRUE,
                           chunk_threshold_mb = Inf,
                           chunk_size = 10000,
                           progress = interactive()) {
  rrefuse_survey_fit(model)
  for (old in intersect(names(.ht_deprecated_tests), tests)) {
    .Deprecated(old = old, msg = .ht_deprecated_test_message(old))
  }
  prepared <- .ht_prepare_model(model, data = data, context = "runHeteroTests()")

  if (prepared$grouped && length(prepared$group_splits) > 0) {
    # A fitWLS() fit is refused before any group is refitted (B11, decision 10
    # of the design of the model context), and so is a MASS::rlm() fit, which
    # every test refuses (R6, decision 4): each group was refitted by its call
    # and its refusals recorded as failures of the group, or, with MASS
    # attached, the rlm() of each group tested.
    rrefuse_grouped_fitwls(prepared$model)
    rrefuse_rlm(prepared$model, "runHeteroTests")
    if (is.null(prepared$fit_factory)) {
      stop(
        "Grouped diagnostics require a formula-based model specification so each group can be refitted.",
        call. = FALSE
      )
    }
    group_results <- vector("list", length(prepared$group_splits))
    # The warnings about fixed effects for small groups of every group, given
    # once all the groups have run (rcache_capture_warnings()).
    small_groups <- list()
    for (i in seq_along(prepared$group_splits)) {
      group_data <- prepared$group_splits[[i]]
      group_model <- prepared$fit_factory(group_data)
      run <- rcache_capture_warnings(.run_hetero_core(
        group_model,
        group_data,
        tests = tests,
        use_cache = FALSE,
        chunk_threshold_mb = chunk_threshold_mb,
        chunk_size = chunk_size,
        progress = progress,
        recover_failures = TRUE
      ))
      group_results[[i]] <- rsuite_mark_origin(run$value, "group")
      small_groups <- c(small_groups, run$warnings)
    }
    grouped <- structure(
      group_results,
      class = c("hetero_grouped_suite", "list"),
      group_keys = prepared$group_keys,
      tests = tests
    )
    rcache_resignal(list(warnings = small_groups))
    grouped
  } else {
    output <- .run_hetero_core(
      prepared$model,
      prepared$data,
      tests = tests,
      use_cache = use_cache,
      chunk_threshold_mb = chunk_threshold_mb,
      chunk_size = chunk_size,
      progress = progress,
      recover_failures = FALSE
    )
    if (prepared$source %in% c("formula", "survey")) {
      output <- rsuite_mark_origin(output, prepared$source)
    }
    # What augment() needs of a workflow or a parsnip fit is kept on each result,
    # so that it maps their data as the runner maps them (rsuite_mark_tidymodels()).
    rsuite_mark_tidymodels(output, model)
  }
}

#' Refuse a grouped run of a fitWLS() fit
#'
#' A grouped run refits the model within each group by evaluating its own
#' call on the rows of the group (`.ht_refit_group()`). The call of a
#' [fitWLS()] fit is that of the model it refitted, without the weights it
#' estimated, so each group was refitted by ordinary least squares, or with
#' the prior weights of the model, and tested as such a fit: a test of
#' another model than the one given, without an error (B11 of the design of
#' the model context). Estimating the variance function again within each
#' group is a model of its own, which the caller can fit. Such a run is refused
#' (decision 10), with the class of the refusals of the model context, which
#' the runners keep (decision 6).
#'
#' @param model The model of the run, after `.ht_prepare_model()`.
#' @return `NULL`, invisibly; called for the error.
#' @keywords internal
#' @noRd
rrefuse_grouped_fitwls <- function(model) {
  if (is.null(attr(model, "variance_function"))) {
    return(invisible(NULL))
  }
  rstop_model_context(paste0(
    "runHeteroTests() cannot run a grouped analysis of a fitWLS() fit: each group is refitted ",
    "by evaluating the call of the model on its rows, and that call does not hold the weights ",
    "fitWLS() estimated, so each group would be tested as another fit, without them. Fit ",
    "fitWLS() within each group and test each fit, or pass the data ungrouped ",
    "(dplyr::ungroup()) to test the fit as a whole."
  ))
}

#' Record that runHeteroTests() fitted the model of a suite itself
#'
#' The call of a model the runner fits names its own variables, such as
#' `stats::lm(formula = model, data = prep$data)`, and `summary()` shows the
#' formula of such a model rather than its call (R/suite_summary.R). The
#' runner says so in the suite it builds, as `attr(, "model_origin")`:
#' `"formula"` for the fit of a formula, `"group"` for a refit within a group,
#' `"survey"` for the fit to the data of a survey design. It is set on the
#' suite after the run, outside the cache of whole calls, and never on the
#' model, whose call the caches key on. Before, `summary()` recognised such a
#' call by its form, which R-devel's `lm()` (2026-10) no longer records.
#'
#' @param suite A `hetero_test_suite`.
#' @param origin `"formula"`, `"group"` or `"survey"`.
#' @return `suite`, with the attribute.
#' @keywords internal
#' @noRd
rsuite_mark_origin <- function(suite, origin) {
  if (inherits(suite, "hetero_test_suite")) {
    attr(suite, "model_origin") <- origin
  }
  suite
}

.run_hetero_core <- function(model, data, tests, use_cache, chunk_threshold_mb, chunk_size, progress, recover_failures = FALSE) {
  # The streaming tests run only when the caller asks for them with a finite
  # threshold; the default, Inf, never streams. Up to 0.15.0 the default was
  # 100 MB, and the runner switched by itself (?runHeteroTests). The warning
  # about the size of the data stays where it was with that default, at 50 MB.
  warn_mb <- if (isTRUE(is.finite(chunk_threshold_mb))) chunk_threshold_mb / 2 else 50
  data_size <- if (is.null(data)) {
    NA_real_
  } else {
    tryCatch(
      check_memory_usage(data, threshold_mb = warn_mb),
      error = function(e) NA_real_
    )
  }
  use_streaming <- isTRUE(data_size > chunk_threshold_mb)

  available <- as.list(.diagnostic_registry)
  invalid <- setdiff(tests, names(available))
  if (length(invalid) > 0) {
    stop(
      "Unknown tests: ", paste(invalid, collapse = ", "),
      ". listDiagnostics() returns the registered names.",
      call. = FALSE
    )
  }

  # The caches hold deterministic tests only (R/cache.R): a resampling test,
  # or a diagnostic a user registered, runs on every call, and a call that
  # runs one is not stored as a whole. The keys of the model and the data are
  # computed once for the call, and those of its tests from them.
  cacheable <- vapply(tests, rcache_cacheable, logical(1))
  inputs <- if (isTRUE(use_cache) && any(cacheable)) rcache_inputs(model, data)
  cache_key <- if (all(cacheable)) {
    rcache_analysis_key(tests, use_streaming, chunk_size, inputs)
  }
  cached <- rcache_get(.analysis_cache, cache_key)
  if (!is.null(cached)) {
    # The warnings about fixed effects for small groups that the tests gave
    # when the call was computed, once each, in their order (R/cache.R).
    rcache_resignal(cached)
    return(rcache_attach(cached, model))
  }
  rng_before <- rrng_state()
  # The warnings about fixed effects for small groups the tests give, muffled
  # while each test runs, kept to be stored with the call, and given to the
  # caller at the end: a test, or its fallback, gives one once it has
  # computed, and the cache of cachedTest() gives it again when it answers
  # (rcached_diagnostic()).
  small_groups <- list()

  streaming_map <- list(
    white = function(model, data) {
      performWhiteTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    },
    breusch_pagan = function(model, data) {
      performBPTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    },
    koenker = function(model, data) {
      performKoenkerTestStreaming(model, data,
        chunk_size = chunk_size,
        progress = progress
      )
    }
  )

  progress_bar <- chunk_progress_bar(length(tests), progress && length(tests) > 1L,
    label = "Running heteroscedasticity diagnostics"
  )
  on.exit(progress_bar$close(), add = TRUE)

  res <- vector("list", length(tests))
  names(res) <- tests

  for (i in seq_along(tests)) {
    test_name <- tests[i]
    runner <- available[[test_name]]

    if (use_streaming && test_name %in% names(streaming_map)) {
      runner <- streaming_map[[test_name]]
    }

    failure_context <- NULL
    cached_route <- identical(runner, available[[test_name]]) && isTRUE(use_cache)
    caught <- rcache_capture_warnings(tryCatch(
      {
        if (cached_route) {
          rcached_diagnostic(test_name, model, data, if (cacheable[[i]]) inputs)
        } else {
          runner(model, data)
        }
      },
      error = function(e) {
        failure_context <<- ht_analyse_failure(test_name, e, model, data)
        ht_log("WARN", sprintf("%s failed: %s", test_name, failure_context$raw_message))
        fallback <- ht_attempt_fallback(
          test_name,
          model,
          data,
          available = available,
          context = failure_context,
          streaming_map = streaming_map,
          use_streaming = use_streaming,
          chunk_size = chunk_size,
          progress = progress
        )
        if (!is.null(fallback)) {
          # A substitute the caches may not hold, such as a diagnostic a user
          # registered under the name of a fallback, or a resampling test,
          # keeps the call out of the cache of whole calls (#144 review).
          if (!rcache_fallback_cacheable(fallback)) {
            cache_key <<- NULL
          }
          return(fallback)
        }

        failure <- .ht_failure_result(
          test_name,
          model,
          data,
          failure_context$user_message,
          suggestions = failure_context$suggestions,
          issue = failure_context$issue
        )
        # A grouped run records the failure of a test in a group and goes on,
        # but a refusal of the model by the model context is final there too
        # (decision 6): the run stops with it, its class and its message,
        # where each group recorded it as a failed test of NA (from the review
        # of increment 11: a penalised gam that a test refuses, a
        # multivariate lm).
        if (isTRUE(recover_failures) && !identical(failure_context$issue, "model_context")) {
          return(failure)
        }
        if (failure_context$issue %in% c("data_rows", "model_context")) {
          # Keep the class of the refusal of `data` (rcheck_rows_at()) or of
          # the model by the model context (R/model_context.R), so that a
          # caller can tell it from a failure of the test.
          stop(structure(
            class = class(e),
            list(message = failure_context$user_message, call = NULL)
          ))
        }
        stop(failure_context$user_message, call. = FALSE)
      }
    ))
    result <- caught$value
    small_groups <- c(small_groups, caught$warnings)

    if (inherits(result, "hetero_test")) {
      if (cached_route && is.null(failure_context)) {
        # The result of the route through the cache, decorated by
        # .run_registered_diagnostic(), is marked as the route without it marks
        # it, so that use_cache = TRUE and FALSE return the same suite. Before
        # 0.16.0 it lacked `extras$streaming`.
        attr(result, "extras") <- utils::modifyList(
          attr(result, "extras") %||% list(),
          list(streaming = FALSE)
        )
      }
      res[[i]] <- result
    } else {
      extras <- list(streaming = use_streaming && test_name %in% names(streaming_map))
      if (!is.null(failure_context)) {
        extras$suggestions <- failure_context$suggestions
        extras$issue <- failure_context$issue
      }
      res[[i]] <- .ht_decorate_result(
        result,
        diagnostic_name = test_name,
        model = model,
        data = data,
        extras = extras
      )
    }

    progress_bar$update(i)
  }

  # A test that named the argument "model" names the model's formula instead
  # (rsuite_data_name()).
  formula_text <- tryCatch(deparse(stats::formula(model)), error = function(e) NULL)
  res <- lapply(res, rsuite_data_name, formula_text = formula_text)

  output <- structure(
    res,
    class = c("hetero_test_suite", "list"),
    tests = tests,
    model = model,
    data_source = if (is.null(data)) NA_character_ else class(data)[1]
  )

  # Not stored if a test drew random numbers after all, as a fallback could.
  if (!is.null(cache_key) && identical(rng_before, rrng_state())) {
    rcache_put(.analysis_cache, cache_key, rcache_detach(output, model, small_groups),
               rcache_result_limit())
  }

  # The warnings about fixed effects for small groups, muffled while the tests
  # ran, are given once the suite is complete and stored: under
  # options(warn = 2) an error then, where inside the tryCatch() above it
  # would have made the runner take a fallback (rcache_capture_warnings()).
  rcache_resignal(list(warnings = small_groups))
  output
}

#' The data.name of a test run by runHeteroTests()
#'
#' A test reached through the registry names the argument of the registry's
#' function, so before 0.16.0 five tests of the package reported `"model"`
#' through the runner, whatever the model was called: White's test, its
#' bootstrap, its form without cross products (a fallback), the studentized
#' Breusch-Pagan statistic and Szroeter's test. A `data.name` that is exactly
#' `"model"` becomes the formula of the model, as Koenker's test and the
#' others report it; any other is kept, so that a diagnostic added with
#' registerDiagnostic() keeps what it says it tested, such as the two halves
#' of the residuals that a `var.test()` compares. Called directly, a test still
#' names the object it was given: `performWhiteTest(m, d)$data.name` is `"m"`.
#'
#' @param result A result of the suite.
#' @param formula_text `deparse(stats::formula(model))`, or `NULL` when the
#'   model has no formula, and nothing changes.
#' @return `result`, with that `data.name`.
#' @keywords internal
#' @noRd
rsuite_data_name <- function(result, formula_text) {
  if (is.null(formula_text) || !inherits(result, "htest") ||
        !identical(result[["data.name"]], "model")) {
    return(result)
  }
  result[["data.name"]] <- formula_text
  result
}
