#!/usr/bin/env Rscript

#' Run all project checks
#'
#' This script formats the R code, lints it, runs the unit tests and
#' generates a coverage report. It assumes all required packages are
#' already installed via `renv::restore()` or `./setup.sh`.

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

message("Running tests...")
set.seed(123)
testthat::test_local("tests/testthat")

message("Generating coverage report...")
cov <- covr::package_coverage()
message(sprintf("Coverage: %.2f%%", covr::percent_coverage(cov)))
out_dir <- "coverage"
if (!dir.exists(out_dir)) dir.create(out_dir)
covr::report(cov, file = file.path(out_dir, "index.html"), browse = FALSE)
message(sprintf("Coverage report written to %s/index.html", out_dir))
