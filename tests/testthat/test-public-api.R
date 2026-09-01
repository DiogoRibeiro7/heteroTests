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

test_that("nothing shipped still points at a removed diagnostic", {
  # inst/ is scanned as well as R/ and man/: the tutorial notebooks and the
  # validation scripts are shipped and executable, so a call left behind there
  # fails in a user's hands just as surely as one in the package code.
  #
  # Markdown under inst/ is exempt. inst/validation/README.md has to name
  # performModifiedBartlettTest() in order to record why it was withdrawn, and
  # prose that documents a removal is not a caller. Nothing in .md executes.
  # skip_if_not_source_tree() comes from helper-source-tree.R.
  root <- skip_if_not_source_tree()

  inst_files <- list.files(file.path(root, "inst"), recursive = TRUE,
                           full.names = TRUE)
  inst_files <- inst_files[!grepl("[.]md$", inst_files, ignore.case = TRUE)]

  files <- c(
    list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE),
    list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE),
    inst_files
  )
  pattern <- paste(removed_exports, collapse = "|")
  offenders <- Filter(function(f) {
    any(grepl(pattern, readLines(f, warn = FALSE)))
  }, files)

  expect_equal(
    length(offenders), 0L,
    info = paste("still reference a removed diagnostic:",
                 paste(sub(root, "", offenders, fixed = TRUE), collapse = ", "))
  )
})
