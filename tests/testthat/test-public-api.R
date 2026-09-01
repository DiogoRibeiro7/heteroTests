library(testthat)

# The public surface after the API review. Six exports were removed in 0.8.0:
# three returned nothing but a migration error, and three were exact duplicates
# of tests that remain. These checks record that decision so the names cannot
# quietly come back, and so the canonical replacements stay covered.

removed_exports <- c(
  # Withdrawn: the statistic could not detect heteroscedasticity.
  "performRiceTest",
  "performCurryWalshTest",
  # Withdrawn: HC0-HC4 are covariance estimators, not a test.
  "performHCCovarianceTest",
  # Duplicates of tests that remain.
  "performOrderedLMTest",
  "performCameronTrivediTest",
  "performModifiedBartlettTest"
)

test_that("the removed diagnostics are no longer exported", {
  exports <- getNamespaceExports("heteroTests")
  for (nm in removed_exports) {
    expect_false(
      nm %in% exports,
      info = paste(nm, "was removed in 0.8.0 and must not be re-exported")
    )
  }
})

test_that("the removed diagnostics are gone from the namespace entirely", {
  ns <- asNamespace("heteroTests")
  for (nm in removed_exports) {
    expect_false(
      exists(nm, envir = ns, inherits = FALSE),
      info = paste(nm, "should have been deleted, not merely unexported")
    )
  }
})

test_that("the replacement for each removed diagnostic is exported", {
  # Every removal has somewhere to go; if one of these disappears the
  # migration advice in NEWS stops being true.
  replacements <- c(
    "performSzroeterTest",      # for performRiceTest
    "performGQTest",            # for performRiceTest
    "performSpatialHeteroTest", # for performCurryWalshTest
    "performBPTest",            # for performHCCovarianceTest
    "performKoenkerTest",       # for performOrderedLMTest
    "performWhiteTest",         # for performCameronTrivediTest
    "performNCVTest",           # for performCameronTrivediTest
    "performBartlettTest"       # for performModifiedBartlettTest
  )
  exports <- getNamespaceExports("heteroTests")
  for (nm in replacements) {
    expect_true(nm %in% exports, info = paste(nm, "is a documented replacement"))
  }
})

test_that("no help page or registry still points at a removed diagnostic", {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  root <- if (file.exists(desc)) normalizePath(dirname(desc)) else ""
  is_source <- nzchar(root) &&
    length(list.files(file.path(root, "R"), pattern = "[.][Rr]$")) > 0L
  skip_if_not(is_source, "source-tree check only")

  files <- c(
    list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE),
    list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  )
  pattern <- paste(removed_exports, collapse = "|")
  offenders <- Filter(function(f) {
    any(grepl(pattern, readLines(f, warn = FALSE)))
  }, files)

  expect_equal(
    length(offenders), 0L,
    info = paste("still reference a removed diagnostic:",
                 paste(basename(offenders), collapse = ", "))
  )
})
