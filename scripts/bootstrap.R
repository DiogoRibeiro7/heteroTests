#!/usr/bin/env Rscript

#' Bootstrap project dependencies
#'
#' Installs renv if necessary, restores the locked library, and ensures
#' development packages are installed. Only attempts installations when
#' internet access is available.
#'
#' Usage:
#'   Rscript scripts/bootstrap.R

is_online <- function(url = "https://cran.r-project.org") {
  con <- try(url(url, "rb"), silent = TRUE)
  if (inherits(con, "try-error")) {
    message("[WARN] Network check failed: ", conditionMessage(attr(con, "condition")))
    return(FALSE)
  }
  on.exit(close(con), add = TRUE)
  TRUE
}

online <- is_online()

apt_install <- function(pkg) {
  apt_pkg <- paste0("r-cran-", tolower(pkg))
  args <- c("install", "-y", "-qq", apt_pkg)
  status <- system2("apt-get", args)
  if (status != 0) {
    message("[WARN] apt-get install for ", pkg, " failed with status ", status)
    return(FALSE)
  }
  message("[INFO] Installed ", pkg, " via apt-get")
  TRUE
}

if (!requireNamespace("renv", quietly = TRUE)) {
  if (online) {
    message("[INFO] Installing renv package")
      install.packages("renv", repos = "https://cran.r-project.org", quiet = TRUE)
  } else {
    stop("renv not installed and no internet connection")
  }
}

if (!dir.exists("renv")) {
  message("[INFO] Initializing renv library")
  renv::init(bare = TRUE, restart = FALSE)
}

restored <- TRUE
if (online) {
  restored <- tryCatch(
    {
      renv::restore(prompt = FALSE)
      message("[INFO] renv restore completed")
      TRUE
    },
    error = function(e) {
      message("[WARN] renv restore failed: ", e$message)
      FALSE
    }
  )
} else {
  message("No internet connection; skipping renv restore")
}

required <- c("styler", "lintr", "testthat", "covr", "ggplot2")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  if (restored && online) {
      for (pkg in missing) {
        try(install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE), silent = TRUE)
      }
  }

  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0 && nzchar(Sys.which("apt-get"))) {
    for (pkg in still_missing) {
      apt_install(pkg)
    }
  }

  message("[INFO] Hydrating installed packages")
  renv::hydrate(packages = missing)
}
