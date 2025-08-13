#' Heteroscedasticity Test Factory
#'
#' Provides a registry of heteroscedasticity tests with metadata and
#' convenience methods to run them with input validation. This is an
#' alternative to the older environment-based registry used by
#' `runHeteroTests()`.
#'
#' @section Usage:
#' The shared instance `test_factory` can be used to register new tests
#' and execute them. See \code{register} and \code{run_test} methods
#' for details.
#'
#' @export
TestFactory <- R6::R6Class(
  "TestFactory",
  private = list(
    .tests = list(),
    .metadata = list()
  ),
  public = list(
    #' Register a heteroscedasticity test
    #' @param name Name of the test
    #' @param func Function with arguments `model` and `data`
    #' @param metadata Optional list with metadata fields
    #' @return Invisibly returns the factory
    register = function(name, func, metadata = list()) {
      if (!is.character(name) || length(name) != 1) {
        stop("name must be a single string")
      }
      if (!is.function(func)) {
        stop("func must be a function")
      }
      required <- c("model", "data")
      if (!all(required %in% names(formals(func)))) {
        stop("function must have arguments: ", paste(required, collapse = ", "))
      }
      default_meta <- list(
        description = paste("Heteroscedasticity test:", name),
        data_types = "cross_sectional",
        min_observations = 10
      )
      meta <- modifyList(default_meta, metadata)
      private$.tests[[name]] <- func
      private$.metadata[[name]] <- meta
      invisible(self)
    },

    #' Get available tests
    #' @param data_type Filter by supported data type
    #' @param min_n Filter by minimum observations
    #' @return Character vector of test names
    get_available = function(data_type = NULL, min_n = NULL) {
      tests <- names(private$.tests)
      if (!is.null(data_type)) {
        tests <- Filter(
          function(n) data_type %in% private$.metadata[[n]]$data_types,
          tests
        )
      }
      if (!is.null(min_n)) {
        tests <- Filter(
          function(n) min_n >= private$.metadata[[n]]$min_observations,
          tests
        )
      }
      tests
    },

    #' Run a registered test
    #' @param test_name Name of the test
    #' @param model Fitted model
    #' @param data Data frame used to fit the model
    #' @param ... Additional arguments passed to the test
    #' @return Result of the test
    run_test = function(test_name, model, data, ...) {
      if (!test_name %in% names(private$.tests)) {
        stop("Unknown test: ", test_name)
      }
      meta <- private$.metadata[[test_name]]
      validateTestInputs(model, data, test_name, meta$min_observations)
      result <- private$.tests[[test_name]](model, data, ...)
      if (inherits(result, "htest")) {
        result$test_metadata <- meta
      }
      result
    }
  )
)

#' Shared instance of the test factory
#'
#' Users can register new tests via this object. Built-in tests are
#' pre-registered at package load time.
#' @export
#' @name test_factory
NULL

test_factory <- TestFactory$new()

# Register a few core tests
.test_factory_register_defaults <- function() {
  test_factory$register("white", performWhiteTest)
  test_factory$register("breusch_pagan", performBPTest)
  test_factory$register("koenker", performKoenkerTest)
  test_factory$register("student_bp", performStudentizedBPTest)
  test_factory$register("white_bootstrap", performWhiteTestBootstrap)
  test_factory$register("szroeter", performSzroeterTest)
}

.test_factory_register_defaults()

# Internal alias for use by package functions
.test_factory <- test_factory
