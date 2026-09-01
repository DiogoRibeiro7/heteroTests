# Shared source-tree detection for the checks that read files from the package
# root rather than the loaded namespace.
#
# An installed copy of heteroTests also carries a DESCRIPTION naming the
# package, and covr re-runs the suite against exactly such a copy, so matching
# on the package name alone is not enough: a source checkout is the one whose
# R/ holds .R sources rather than a compiled .rdb.

hetero_source_root <- function() {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  if (!file.exists(desc)) return(NULL)
  if (!any(grepl("Package: heteroTests", readLines(desc, warn = FALSE), fixed = TRUE))) {
    return(NULL)
  }
  root <- normalizePath(dirname(desc))
  if (length(list.files(file.path(root, "R"), pattern = "[.][Rr]$")) == 0L) {
    return(NULL)
  }
  root
}

skip_if_not_source_tree <- function() {
  root <- hetero_source_root()
  testthat::skip_if(
    is.null(root),
    "this check only runs from a source checkout"
  )
  root
}
