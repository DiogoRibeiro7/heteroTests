#' Diagnostics registry
#'
#' Maintains a registry of diagnostic functions that can be called by
#' \code{runHeteroTests()} and \code{runDiagnostics()}. Users can register
#' custom diagnostics via \code{registerDiagnostic()}.
".diagnostic_registry" <- new.env(parent = emptyenv())
".plot_registry" <- new.env(parent = emptyenv())
# What each test the package registers does with random numbers, by name:
# "deterministic" (draws none, and its result is a function of the model, the
# data and its arguments), "random" (draws them: a bootstrap or a
# permutation test), or "data" (tests data rather than a model). Only the
# deterministic ones are cached (rcache_cacheable(), R/cache.R). A name has
# no flag once registerDiagnostic() registers it, whoever does.
".diagnostic_kind" <- new.env(parent = emptyenv())
#' Register a diagnostic function
#'
#' @param name Name of the diagnostic.
#' @param fun Function taking a model and data and returning a result.
#' @details A diagnostic registered this way is never cached by
#'   [cachedTest()] or [runHeteroTests()], nor is a test of the package
#'   registered again under its name: whether it draws random numbers, or
#'   reads anything besides the model and the data, cannot be known.
#' @return Invisibly returns \code{NULL}.
#' @seealso [listDiagnostics()] for the names already registered.
#' @examples
#' custom <- function(model, data) list(dummy = TRUE)
#' registerDiagnostic("custom", custom)
registerDiagnostic <- function(name, fun) {
  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single string")
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function")
  }
  assign(name, fun, envir = .diagnostic_registry)
  # A diagnostic a user registers, or registers again under a name of the
  # package, is never cached: whether it draws random numbers, or reads
  # anything besides the model and the data, cannot be known.
  if (exists(name, envir = .diagnostic_kind, inherits = FALSE)) {
    rm(list = name, envir = .diagnostic_kind)
  }
  invisible(NULL)
}

# Register a test of the package, with what it does with random numbers
# ("deterministic", "random" or "data"; see .diagnostic_kind above).
rregister_package_diagnostic <- function(name, fun, kind = c("deterministic", "random", "data")) {
  kind <- match.arg(kind)
  registerDiagnostic(name, fun)
  assign(name, kind, envir = .diagnostic_kind)
  invisible(NULL)
}

#' List the registered diagnostics
#'
#' Returns the names [runHeteroTests()] accepts in `tests`: the diagnostics
#' the package registers and any added with [registerDiagnostic()].
#'
#' `"box_m"` is registered for [runMultivariateTests()], which calls it with
#' data and a grouping factor rather than with a model.
#'
#' @return A character vector of the registered names, sorted.
#' @seealso [registerDiagnostic()], [runHeteroTests()]
#' @examples
#' listDiagnostics()
listDiagnostics <- function() {
  sort(ls(.diagnostic_registry))
}

#' Register a diagnostic plot
#'
#' @param name Name of the plot
#' @param fun Function taking a model and returning a ggplot object
#' @return Invisibly returns `NULL`.
#' @examples
#' registerPlot("custom_plot", function(model) ggplot2::ggplot())
registerPlot <- function(name, fun) {
  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single string")
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function")
  }
  assign(name, fun, envir = .plot_registry)
  invisible(NULL)
}

# Registry names deprecated in 0.14.0, each with the name its warning points
# to. A deprecated name keeps calling what it called before, so its results do
# not change, and runHeteroTests() warns once per call, not once per group.
# Since 0.15.0 it refuses first what the deprecated function it computes
# refuses: a glm that is not a least-squares fit, with that function's
# message. Before, the two names returned a value for such a fit, and
# runHeteroTests() fell back on "ncv" and returned it under the name of the
# test that had refused it.
.ht_deprecated_tests <- c(student_bp = "koenker", ncv = "cook_weisberg")

.ht_deprecated_test_message <- function(old) {
  sprintf(
    "Test name \"%s\" is deprecated.\nUse \"%s\" instead.\nSee help(\"Deprecated\")",
    old, .ht_deprecated_tests[[old]]
  )
}

# preset built-in diagnostics. The kind of each was read from its code and
# is checked by tests/testthat/test-cache-contract.R, which runs every one
# and compares .Random.seed before and after: the resampling tests
# ("white_bootstrap", "wild_bootstrap", "rank_permutation", and
# "spatial_hetero", whose p-value is the permutation test of
# spdep::moran.mc()) draw random numbers, the others none.
rregister_package_diagnostic("white", function(model, data) performWhiteTest(model, data))
rregister_package_diagnostic("breusch_pagan", function(model, data) performBPTest(model, data))
rregister_package_diagnostic("koenker", function(model, data) performKoenkerTest(model, data))
rregister_package_diagnostic(
  "cook_weisberg", function(model, data) performCookWeisbergTest(model, data)
)
rregister_package_diagnostic("ncv", function(model, data) {
  rrefuse_non_gaussian_glm(model, "performNCVTest")
  # The data it is given are checked against the model, as performNCVTest()
  # checks them, since 0.16.0. Before, data that lacked rows or variables of
  # the fit were ignored, and because "ncv" is the last fallback of "white",
  # "koenker" and "breusch_pagan", runHeteroTests() on such data returned the
  # Cook-Weisberg statistic under their names.
  rcheck_data_argument(data, "performNCVTest", model, "NCV")
  .ncv_test(model)
})
rregister_package_diagnostic(
  "spread_level", function(model, data) performSpreadLevelTest(model, data)
)
# performBoxMTest() tests the data, not a model, and is the documented
# exception to the (model, data, ...) convention.
rregister_package_diagnostic("box_m", function(data, group) performBoxMTest(data, group), "data")
rregister_package_diagnostic("student_bp", function(model, data) {
  rrefuse_non_gaussian_glm(model, "performStudentizedBPTest")
  .studentized_bp_test(model, data)
})
rregister_package_diagnostic(
  "white_bootstrap", function(model, data) performWhiteTestBootstrap(model, data), "random"
)
# performSzroeterTest() needs an ordering of the observations and a registry
# entry is called with the model and the data only. The ordering comes from
# the model: the fitted values, so the alternative is that the variance rises
# with the mean. With a single regressor that is the order of the regressor
# when its slope is positive and the reverse order when it is negative.
#
# The fitted values are the linear predictor of the fit with its total
# offset, X b + o, read from the model context (rcontext_mean()): what
# fitted() holds, and so what predict(type = "response") returns, except on
# an mgcv::gam() fit with an `offset` argument, which predict.gam() leaves
# out. Before 0.16.0 they came from predict(), so on such a gam the test
# ordered by X b alone. They are computed from the coefficients, not taken
# from those an lm stores, y - e: rows with the same design row would then
# differ in their last bits by an amount that depends on the residual, and
# those rounding errors would decide the order within the ties. Computed from
# the coefficients, such rows tie exactly and keep the order of the rows the
# model was fitted to.
#
# Since 0.16.0 they are in the order of the fit, and `data` is matched to the
# observations of the fit as performSzroeterTest() matches it, so nothing is
# matched by row name here. Before, they were matched to the row names of
# `data`, and a tibble, or data whose row names were reset, that held the
# rows of the fit in its order but under other numbers was refused with
# "Missing rows"; and `data = NULL` was refused, where the other registered
# tests read the model frame. A row of the fit that `data` lacks is an error,
# with the message performSzroeterTest() gives for it. The result calls the
# ordering "fitted values", or, when `data` has a column of that name, that
# name with dots in front.
rregister_package_diagnostic(
  "szroeter",
  function(model, data) {
    ctx <- rmodel_context(model, data, fn = "performSzroeterTest", test_label = "Szroeter test")
    column <- "fitted values"
    while (column %in% names(data)) {
      column <- paste0(".", column)
    }
    .szroeter_test(ctx, data, column, "greater", data_name = "model", by_mean = TRUE)
  }
)
rregister_package_diagnostic(
  "wild_bootstrap", function(model, data) performWildBootstrapTest(model, data), "random"
)
rregister_package_diagnostic(
  "quantile_regression", function(model, data) performQuantileRegressionTest(model, data)
)
rregister_package_diagnostic(
  "rank_permutation", function(model, data) performRankPermutationTest(model, data), "random"
)
rregister_package_diagnostic(
  "high_dimensional", function(model, data) performHighDimensionalTest(model, data)
)
rregister_package_diagnostic(
  "variance_form", function(model, data) performVarianceFormTest(model, data)
)
rregister_package_diagnostic(
  "spatial_hetero",
  function(model, data) {
    listw <- attr(data, "listw")
    if (is.null(listw)) {
      stop(
        "Spatial heteroscedasticity diagnostic requires `listw` to be provided via `attr(data, 'listw')`.",
        call. = FALSE
      )
    }
    performSpatialHeteroTest(model, data, listw = listw)
  },
  "random"
)

# preset built-in plots
registerPlot("residuals_fitted", function(model) plotResidualsFitted(model))
registerPlot("spread_level", function(model) plotSpreadLevel(model))
registerPlot("density", function(model) plotResidualDensity(model))
registerPlot("qq", function(model) plotResidualQQ(model))
registerPlot("bubble_variance", function(model) plotBubbleVariance(model))

#' Run registered diagnostic plots
#'
#' @param model A fitted `lm` or `glm` object
#' @param plots Character vector of plot names to generate
#' @return Named list of ggplot objects
#' @examples
#' runDiagnosticPlots(lm(mpg ~ wt, mtcars))
runDiagnosticPlots <- function(model,
                               plots = c(
                                 "residuals_fitted", "spread_level",
                                 "density", "qq", "bubble_variance"
                               )) {
  checkModel(model)
  available <- as.list(.plot_registry)
  invalid <- setdiff(plots, names(available))
  if (length(invalid) > 0) {
    stop("Unknown plots: ", paste(invalid, collapse = ", "))
  }
  res <- lapply(plots, function(p) available[[p]](model))
  names(res) <- plots
  res
}
