#!/usr/bin/env Rscript

# Cross-platform setup helper for heteroTests
# This script provides an alternative to setup.sh for Windows and macOS users.
# It ensures renv is installed and restores the locked package library.

is_online <- function(url = "https://cran.r-project.org") {
  con <- try(url(url, "rb"), silent = TRUE)
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  on.exit(close(con), add = TRUE)
  TRUE
}

online <- is_online()

if (!requireNamespace("renv", quietly = TRUE)) {
  if (online) {
    install.packages("renv", repos = "https://cran.r-project.org")
  } else {
    stop("renv package is required but not installed and no internet connection.")
  }
}

if (online) {
  tryCatch(
    renv::restore(prompt = FALSE),
    error = function(e) message("renv restore failed: ", e$message)
  )
} else {
  message("No internet connection; skipping renv restore")
}

required <- c("styler", "lintr", "testthat", "covr", "quickcheck", "ggplot2")
for (pkg in required) {
  if (!requireNamespace(pkg, quietly = TRUE) && online) {
    try(install.packages(pkg, repos = "https://cran.r-project.org"), silent = TRUE)
  }
}

message("Setup complete.")
