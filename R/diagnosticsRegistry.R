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

# preset built-in diagnostics
registerDiagnostic("white", function(model, data) performWhiteTest(model, data))
registerDiagnostic("breusch_pagan", function(model, data) performBPTest(model, data))
registerDiagnostic("koenker", function(model, data) performKoenkerTest(model, data))
registerDiagnostic("cook_weisberg", function(model, data) performCookWeisbergTest(model))
registerDiagnostic("ncv", function(model, data) performNCVTest(model))
registerDiagnostic("spread_level", function(model, data) performSpreadLevelTest(model))
registerDiagnostic("box_m", function(data, group) performBoxMTest(data, group))
registerDiagnostic("student_bp", function(model, data) performStudentizedBPTest(model, data))
registerDiagnostic("white_bootstrap", function(model, data) performWhiteTestBootstrap(model, data))
registerDiagnostic(
  "szroeter",
  function(model, data) {
    performSzroeterTest(model, data, order_by = names(data)[2])
  }
)
registerDiagnostic("wild_bootstrap", function(model, data) performWildBootstrapTest(model, data))
registerDiagnostic("hc_covariance", function(model, data) performHCCovarianceTest(model, data))
registerDiagnostic("quantile_regression", function(model, data) performQuantileRegressionTest(model, data))
registerDiagnostic("rank_permutation", function(model, data) performRankPermutationTest(model, data))
registerDiagnostic("high_dimensional", function(model, data) performHighDimensionalTest(model, data))
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
