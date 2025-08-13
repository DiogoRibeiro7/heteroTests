# =============================================================================
# Test Helper Functions and Setup
# tests/testthat/helper-setup.R
# =============================================================================

library(testthat)
library(heteroTests)

# Test data generators
# =============================================================================

#' Generate test data for heteroscedasticity testing
#' @param n Number of observations
#' @param heteroscedastic Whether to create heteroscedastic errors
#' @param seed Random seed for reproducibility
#' @return Data frame with x1, x2, y columns
generate_test_data <- function(n = 50, heteroscedastic = FALSE, seed = 123) {
  set.seed(seed)
  x1 <- rnorm(n)
  x2 <- rnorm(n)

  if (heteroscedastic) {
    # Create heteroscedastic errors proportional to |x1|
    sigma <- 0.5 + 0.3 * abs(x1)
    y <- 1 + 2 * x1 + 3 * x2 + sigma * rnorm(n)
  } else {
    # Homoscedastic errors
    y <- 1 + 2 * x1 + 3 * x2 + rnorm(n)
  }

  data.frame(x1 = x1, x2 = x2, y = y)
}

#' Generate panel data for panel tests
#' @param n_individuals Number of cross-sectional units
#' @param n_periods Number of time periods
#' @param seed Random seed
#' @return Panel data frame
generate_panel_data <- function(n_individuals = 5, n_periods = 4, seed = 123) {
  set.seed(seed)
  n_total <- n_individuals * n_periods

  data.frame(
    id = rep(1:n_individuals, each = n_periods),
    time = rep(1:n_periods, n_individuals),
    x = rnorm(n_total),
    y = rnorm(n_total)
  )
}

#' Generate data with grouping variables
#' @param n Total number of observations
#' @param n_groups Number of groups
#' @param seed Random seed
#' @return Data frame with grouping variable
generate_grouped_data <- function(n = 60, n_groups = 3, seed = 123) {
  set.seed(seed)
  group_size <- n %/% n_groups

  data.frame(
    x = rnorm(n),
    y = rnorm(n),
    group = rep(factor(1:n_groups), each = group_size)[1:n]
  )
}

#' Create a standard test model with data
#' @param heteroscedastic Whether to use heteroscedastic errors
#' @param n Sample size
#' @param seed Random seed
#' @return List with model and data components
create_test_model <- function(heteroscedastic = FALSE, n = 50, seed = 123) {
  data <- generate_test_data(n = n, heteroscedastic = heteroscedastic, seed = seed)
  list(
    model = lm(y ~ x1 + x2, data = data),
    data = data
  )
}

#' Create data with positive values only (for Park test etc.)
#' @param n Sample size
#' @param seed Random seed
#' @return List with model and data
create_positive_data_model <- function(n = 50, seed = 123) {
  set.seed(seed)
  data <- data.frame(
    x1 = runif(n, 1, 10),  # Ensure positive values
    x2 = runif(n, 1, 10),
    y = rnorm(n)
  )
  data$y <- 1 + 2 * log(data$x1) + 3 * log(data$x2) + rnorm(n)

  list(
    model = lm(y ~ x1 + x2, data = data),
    data = data
  )
}

# Custom expectations
# =============================================================================

#' Expect object to be a valid htest
#' @param object Object to test
expect_htest <- function(object) {
  expect_s3_class(object, "htest")
  expect_true("statistic" %in% names(object))
  expect_true("p.value" %in% names(object))
  expect_true("method" %in% names(object))
  expect_true(is.numeric(object$statistic))
  expect_true(is.numeric(object$p.value))
  expect_true(object$p.value >= 0 && object$p.value <= 1)
  expect_false(is.na(object$p.value))
}

#' Expect valid diagnostic results
#' @param result List of diagnostic results
#' @param expected_tests Vector of expected test names
expect_valid_diagnostic_result <- function(result, expected_tests) {
  expect_type(result, "list")
  expect_named(result, expected_tests, ignore.order = TRUE)

  for (test_name in expected_tests) {
    if (inherits(result[[test_name]], "htest")) {
      expect_htest(result[[test_name]])
    }
  }
}

#' Expect valid ggplot object
#' @param object Plot object to test
expect_ggplot <- function(object) {
  expect_s3_class(object, "ggplot")
  expect_true("data" %in% names(object))
  expect_true("layers" %in% names(object))
}

#' Expect valid remediation suggestions
#' @param object Remediation suggestions object
expect_remediation_suggestions <- function(object) {
  expect_s3_class(object, "remediation_suggestions")
  expect_true(is.list(object))
}

# Test utilities
# =============================================================================

#' Skip test if package not available
#' @param pkg Package name
skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not available"))
  }
}

#' Skip long-running tests on CRAN
skip_on_cran <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    return(invisible(TRUE))
  }
  testthat::skip("On CRAN")
}

#' Create temporary test environment
with_temp_test_env <- function(code) {
  old_seed <- .Random.seed
  on.exit({
    if (exists("old_seed")) {
      .Random.seed <<- old_seed
    }
  })

  set.seed(123)
  eval(substitute(code), envir = parent.frame())
}

# Constants for testing
# =============================================================================

# Standard test configurations
BASIC_TESTS <- c("white", "breusch_pagan", "koenker")
GROUP_TESTS <- c("levene", "bartlett", "brown_forsythe")
TIME_SERIES_TESTS <- c("arch_lm", "mcleod_li")
PANEL_TESTS <- c("bp_random", "pesaran")

# Test data sizes
SMALL_N <- 20
MEDIUM_N <- 50
LARGE_N <- 200

# Statistical test parameters
ALPHA_LEVEL <- 0.05
SIMULATION_REPS <- 100  # For property-based tests
PERFORMANCE_TIMEOUT <- 10  # seconds
