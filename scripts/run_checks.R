#!/usr/bin/env Rscript

#' Run all project checks
#'
#' This script formats the R code, lints it, runs the unit tests and
#' generates a coverage report. It assumes all required packages are
#' already installed via `renv::restore()` or `./setup.sh`.
#'
#' The suite runs once, inside `covr::package_coverage()`, which stops with an
#' error when a test fails. covr runs it against an installed copy of the
#' package, where the checks that read the source tree skip
#' (tests/testthat/helper-source-tree.R), as they do under R CMD check, so the
#' files that hold them run from the source first. The coverage is saved in
#' coverage/coverage.rds, which the Codecov upload in run-checks.yml reads
#' rather than running the suite again.

required <- c("styler", "lintr", "testthat", "covr", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop(
    "Missing packages: ", paste(missing, collapse = ", "),
    ". Run ./setup.sh to restore the environment."
  )
}

message("Styling R code...")
styler::style_dir("R/")

message("Linting R code...")
lintr::lint_dir("R/")

# The test files that read the source tree: those that call the helpers of
# helper-source-tree.R or reach the package root through test_path().
source_tree_markers <- c("skip_if_not_source_tree(", "hetero_source_root(", 'test_path("..", "..",')
test_files <- list.files("tests/testthat", pattern = "^test.*[.][Rr]$")
reads_source <- vapply(test_files, function(file) {
  text <- readLines(file.path("tests/testthat", file), warn = FALSE)
  any(vapply(source_tree_markers, function(m) any(grepl(m, text, fixed = TRUE)), logical(1)))
}, logical(1))
if (!any(reads_source)) {
  stop("No test file reads the source tree; update source_tree_markers in scripts/run_checks.R.")
}
# test_local() matches `filter` against the file names without "test-" and
# ".R".
source_tree_names <- sub("^test[-_]", "", sub("[.][Rr]$", "", test_files[reads_source]))
source_tree_filter <- paste0(
  "^(", paste(gsub(".", "\\.", source_tree_names, fixed = TRUE), collapse = "|"), ")$"
)

message("Running the tests that read the source tree: ",
        paste(test_files[reads_source], collapse = ", "))
set.seed(123)
testthat::test_local("tests/testthat", filter = source_tree_filter)

message("Running the tests with coverage...")
# clean = FALSE keeps the library covr installs the package into, under
# tempdir(), so that the results of the tests can be shown from its
# testthat.Rout; tempdir() goes when the session ends.
cov <- covr::package_coverage(clean = FALSE)
rout <- file.path(attr(cov, "library"), "heteroTests", "heteroTests-tests", "testthat.Rout")
if (file.exists(rout)) {
  lines <- readLines(rout, warn = FALSE)
  # The list of skips, which ends at a blank line, and the counts.
  skipped <- grep("Skipped tests", lines, fixed = TRUE)
  if (length(skipped)) {
    blank <- which(!nzchar(trimws(lines)))
    end <- c(blank[blank > skipped[1]], length(lines) + 1L)[1] - 1L
    writeLines(lines[skipped[1]:end])
  }
  writeLines(utils::tail(grep("^\\[ FAIL", lines, value = TRUE), 1))
}
attr(cov, "library") <- NULL
message(sprintf("Coverage: %.2f%%", covr::percent_coverage(cov)))
out_dir <- "coverage"
if (!dir.exists(out_dir)) dir.create(out_dir)
saveRDS(cov, file.path(out_dir, "coverage.rds"))
covr::report(cov, file = file.path(out_dir, "index.html"), browse = FALSE)
message(sprintf("Coverage report written to %s/index.html", out_dir))
