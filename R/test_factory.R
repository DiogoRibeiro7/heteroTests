#' Heteroscedasticity Test Factory
#'
#' Provides a registry of heteroscedasticity tests with metadata and
#' convenience methods to run them with input validation. This is an
#' alternative to the older environment-based registry used by
#' `runHeteroTests()`.
#'
#' @section Deprecated:
#' `TestFactory` and its shared instance `test_factory` are deprecated since
#' 0.14.0: they are the package's internal machinery and leave the exports in
#' 1.0.0. They are objects rather than functions, so they cannot warn when
#' used; this page and `NEWS.md` are the notice. Use [registerDiagnostic()] to
#' add a test and [runHeteroTests()] to run one.
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
          function(n) private$.metadata[[n]]$min_observations >= min_n,
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
      test <- private$.tests[[test_name]]
      # A rank-deficient fit is left to a test of the package, which accepts
      # it, computed on the estimable part of the design, or refuses it with
      # its own message; it is refused here for any other function.
      .validate_test_inputs(
        model, data, test_name, meta$min_observations,
        allow_aliased = rfactory_test_validates(test)
      )
      result <- test(model, data, ...)
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
#'
#' Deprecated since 0.14.0, with [TestFactory]: it leaves the exports in
#' 1.0.0, and as an object it cannot warn when used. Use
#' [registerDiagnostic()] and [runHeteroTests()] instead.
#'
#' Its `"szroeter"` entry is [performSzroeterTest()], which needs the column
#' that orders the observations, given as `order_by`:
#' `test_factory$run_test("szroeter", model, data, order_by = "x")`. Without
#' `order_by` it refuses the call and asks for it, since 0.16.0; before, it
#' stopped with "argument "order_by" is missing, with no default".
#' @export
#' @name test_factory
NULL

test_factory <- TestFactory$new()

# The tests the package registers, by name. Each validates the model it is
# given itself, and accepts a rank-deficient fit, computed on the estimable
# part of its design, or refuses it with its own message, so run_test() leaves
# such a fit to them, and to any function of the package
# (rfactory_test_validates()). For a function a user writes, the checks
# run_test() makes first still refuse such a fit, as before 0.16.0.
.ht_factory_tests <- list(
  white = performWhiteTest,
  breusch_pagan = performBPTest,
  koenker = performKoenkerTest,
  # The implementation behind performStudentizedBPTest(), after the refusal
  # of a glm that is not a least-squares fit that the function makes, since
  # 0.15.0; it returned a value for such a fit before.
  student_bp = function(model, data, ...) {
    rrefuse_non_gaussian_glm(model, "performStudentizedBPTest")
    .studentized_bp_test(model, data, ...)
  },
  white_bootstrap = performWhiteTestBootstrap,
  # performSzroeterTest() needs the ordering of the observations, which
  # run_test() passes on from its `...` as `order_by`. Without it the entry
  # refuses the call and asks for it, as "spatial_hetero" asks for its
  # neighbour list, since 0.16.0; before, it stopped with "argument
  # "order_by" is missing, with no default". It does not order by anything
  # of its own: the registered "szroeter" of runHeteroTests() orders by the
  # fitted values, an ordering this entry never had.
  szroeter = function(model, data, order_by, ...) {
    if (missing(order_by)) {
      stop(
        "The Szroeter test needs `order_by`, the column of `data` that orders the ",
        "observations: test_factory$run_test(\"szroeter\", model, data, order_by = \"x\").",
        call. = FALSE
      )
    }
    performSzroeterTest(model, data, order_by = order_by, ...)
  },
  wild_bootstrap = performWildBootstrapTest,
  quantile_regression = performQuantileRegressionTest,
  rank_permutation = performRankPermutationTest,
  high_dimensional = performHighDimensionalTest,
  spatial_hetero = function(model, data) {
    listw <- attr(data, "listw")
    if (is.null(listw)) {
      stop("Spatial test requires `listw` attribute on data.", call. = FALSE)
    }
    performSpatialHeteroTest(model, data, listw = listw)
  }
)

# Is `fun` a function of the package, such as one of the tests it registers?
# Those are defined in its namespace. A function a user writes, even one that
# calls a test of the package, is defined elsewhere. The environment is
# compared rather than the function itself: an installed package lazy-loads
# the list above apart from the functions, and covr rewrites the functions
# of the namespace but not their copies in the list.
rfactory_test_validates <- function(fun) {
  is.function(fun) && identical(environment(fun), environment(rfactory_test_validates))
}

# Register core and validated tests
.test_factory_register_defaults <- function() {
  for (name in names(.ht_factory_tests)) {
    test_factory$register(name, .ht_factory_tests[[name]])
  }
}

.test_factory_register_defaults()

# Internal alias for use by package functions
.test_factory <- test_factory
