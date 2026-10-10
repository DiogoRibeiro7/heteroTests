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

test_that("every diagnostic a recommendation can name is registered", {
  # The scan above looks for function names. A recommendation names a
  # registry entry instead, and runHeteroTests(tests = ) stops on the first
  # name it does not know, so a stale one fails the whole run.
  #
  # suggestDiagnosticsForProfile() is the only recommendation function that
  # returns test names: generateHeteroRecommendations() returns its output,
  # and buildDiagnosticDecisionTree() repeats the names it is given. It
  # branches on the size bucket, high dimension, spatial fields, the
  # missingness rate (at 0.05) and the shape of each numeric variable, and
  # every combination of the values analyseDatasetCharacteristics() produces
  # is tried here.
  registered <- ls(heteroTests:::.diagnostic_registry)
  shapes <- c(
    "none", "insufficient_data", "constant", "undetermined",
    "approximately_symmetric", "mild_right_skew", "mild_left_skew",
    "strong_right_skew", "strong_left_skew"
  )
  grid <- expand.grid(
    size_bucket = c("small", "medium", "large", "very_large"),
    high_dimensional = c(FALSE, TRUE),
    detected_spatial_fields = c(FALSE, TRUE),
    missing_rate = c(NA, 0, 0.049, 0.05, 0.2),
    shape = shapes,
    stringsAsFactors = FALSE
  )

  named <- character()
  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    profile <- list(
      size_bucket = g$size_bucket,
      high_dimensional = g$high_dimensional,
      detected_spatial_fields = g$detected_spatial_fields,
      missingness = if (is.na(g$missing_rate)) NULL else list(overall_rate = g$missing_rate),
      numeric_distribution = if (g$shape == "none") {
        data.frame()
      } else {
        data.frame(variable = "x", shape = g$shape, stringsAsFactors = FALSE)
      }
    )
    named <- union(named, suggestDiagnosticsForProfile(profile)$test)
  }

  expect_true(length(named) > 0L)
  expect_identical(setdiff(named, registered), character(),
                   info = "recommended but not registered")
})

test_that("no object in R/ has two different definitions", {
  # Without a Collate field R sources R/ in alphabetical order, so when two
  # files assign the same name the later one wins and the earlier one is code
  # that never runs and that no test can reach. Until this check was added,
  # suggestDiagnosticsForProfile() was defined in
  # R/intelligent_recommendations.R and again in
  # R/zz_profile_diagnostic_selection.R; the copy that never ran still
  # recommended "hc_covariance", a diagnostic removed in 0.8.0. Identical
  # copies, such as the two of `%||%`, cannot change behaviour and are allowed.
  root <- skip_if_not_source_tree()
  files <- list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE)

  definitions <- list()
  for (f in files) {
    for (e in parse(f, keep.source = FALSE)) {
      is_assignment <- is.call(e) &&
        (identical(e[[1]], as.name("<-")) || identical(e[[1]], as.name("="))) &&
        (is.name(e[[2]]) || is.character(e[[2]]))
      if (is_assignment) {
        nm <- as.character(e[[2]])
        definitions[[nm]] <- c(definitions[[nm]], paste(deparse(e[[3]]), collapse = "\n"))
      }
    }
  }
  differing <- names(definitions)[
    vapply(definitions, function(d) length(unique(d)) > 1L, logical(1))
  ]

  expect_identical(differing, character(),
                   info = "defined more than once, differently")
})
