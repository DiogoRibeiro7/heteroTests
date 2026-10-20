#' Diagnostics registry
#'
#' Maintains a registry of diagnostic functions that can be called by
#' \code{runHeteroTests()} and \code{runDiagnostics()}. Users can register
#' custom diagnostics via \code{registerDiagnostic()}.
".diagnostic_registry" <- new.env(parent = emptyenv())
".plot_registry" <- new.env(parent = emptyenv())
#' Register a diagnostic function
#'
#' @param name Name of the diagnostic.
#' @param fun Function taking a model and data and returning a result.
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

# preset built-in diagnostics
registerDiagnostic("white", function(model, data) performWhiteTest(model, data))
registerDiagnostic("breusch_pagan", function(model, data) performBPTest(model, data))
registerDiagnostic("koenker", function(model, data) performKoenkerTest(model, data))
registerDiagnostic("cook_weisberg", function(model, data) performCookWeisbergTest(model, data))
registerDiagnostic("ncv", function(model, data) {
  rrefuse_non_gaussian_glm(model, "performNCVTest")
  # The data it is given are checked against the model, as performNCVTest()
  # checks them, since 0.16.0. Before, data that lacked rows or variables of
  # the fit were ignored, and because "ncv" is the last fallback of "white",
  # "koenker" and "breusch_pagan", runHeteroTests() on such data returned the
  # Cook-Weisberg statistic under their names.
  rcheck_data_argument(data, "performNCVTest", model, "NCV")
  .ncv_test(model)
})
registerDiagnostic("spread_level", function(model, data) performSpreadLevelTest(model, data))
# performBoxMTest() tests the data, not a model, and is the documented
# exception to the (model, data, ...) convention.
registerDiagnostic("box_m", function(data, group) performBoxMTest(data, group))
registerDiagnostic("student_bp", function(model, data) {
  rrefuse_non_gaussian_glm(model, "performStudentizedBPTest")
  .studentized_bp_test(model, data)
})
registerDiagnostic("white_bootstrap", function(model, data) performWhiteTestBootstrap(model, data))
# performSzroeterTest() needs an ordering of the observations and a registry
# entry is called with the model and the data only. The ordering comes from
# the model: the fitted values, so the alternative is that the variance rises
# with the mean. With a single regressor that is the order of the regressor
# when its slope is positive and the reverse order when it is negative.
#
# The fitted values are those predict(type = "response") computes, X b (plus
# any offset), read from the model context (rcontext_mean()). They are not
# those an lm stores, y - e: rows with the same design row would then differ
# in their last bits by an amount that depends on the residual, and those
# rounding errors would decide the order within the ties. Computed from the
# coefficients, such rows tie exactly and keep the order of the rows the model
# was fitted to.
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
registerDiagnostic(
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
registerDiagnostic("wild_bootstrap", function(model, data) performWildBootstrapTest(model, data))
registerDiagnostic("quantile_regression", function(model, data) performQuantileRegressionTest(model, data))
registerDiagnostic("rank_permutation", function(model, data) performRankPermutationTest(model, data))
registerDiagnostic("high_dimensional", function(model, data) performHighDimensionalTest(model, data))
registerDiagnostic("variance_form", function(model, data) performVarianceFormTest(model, data))
registerDiagnostic(
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
  }
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
