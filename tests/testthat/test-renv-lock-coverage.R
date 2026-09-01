library(testthat)

# The Dockerfile builds the image with
#   renv::restore(); remotes::install_local()
# so renv.lock is the pinned environment the package is meant to be
# reproducible in. It had drifted badly: 9 packages recorded against 40
# declared in DESCRIPTION, with five hard Imports absent entirely, which makes
# the pinning guarantee illusory even though the build still succeeds (
# install_local() silently resolves the gaps from CRAN).
#
# Regenerating the lockfile requires renv::snapshot() in the target
# environment. This check does not attempt that; it fails when the lockfile
# stops covering the package's hard dependencies, so the drift is visible at
# PR time rather than at image-build time.

source_root <- function() {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  if (!file.exists(desc)) return(NULL)
  if (!any(grepl("Package: heteroTests", readLines(desc, warn = FALSE), fixed = TRUE))) {
    return(NULL)
  }
  root <- normalizePath(dirname(desc))
  # An installed package also has a matching DESCRIPTION; a source checkout
  # is the one whose R/ contains sources rather than a compiled .rdb.
  if (length(list.files(file.path(root, "R"), pattern = "[.][Rr]$")) == 0L) {
    return(NULL)
  }
  root
}

declared_field <- function(desc, field) {
  if (!field %in% colnames(desc)) return(character())
  x <- gsub("\\([^)]*\\)", "", desc[1, field])
  x <- trimws(unlist(strsplit(x, ",")))
  sort(unique(x[nzchar(x) & x != "R"]))
}

test_that("renv.lock covers every hard dependency", {
  root <- source_root()
  skip_if(is.null(root), "renv.lock checks only run from the source tree")
  lock_path <- file.path(root, "renv.lock")
  skip_if_not(file.exists(lock_path), "no renv.lock in this layout")
  skip_if_not_installed("jsonlite")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  imports <- declared_field(desc, "Imports")
  base_pkgs <- rownames(installed.packages(priority = "base"))
  hard <- setdiff(imports, base_pkgs)

  locked <- names(jsonlite::fromJSON(lock_path)$Packages)
  missing <- setdiff(hard, locked)

  expect_equal(
    length(missing), 0L,
    info = paste0(
      "Imports absent from renv.lock: ", paste(missing, collapse = ", "),
      ". renv::restore() would not install them, so the pinned environment is ",
      "incomplete. Regenerate with renv::snapshot()."
    )
  )
})

test_that("renv.lock records an R version consistent with DESCRIPTION", {
  root <- source_root()
  skip_if(is.null(root), "renv.lock checks only run from the source tree")
  lock_path <- file.path(root, "renv.lock")
  skip_if_not(file.exists(lock_path), "no renv.lock in this layout")
  skip_if_not_installed("jsonlite")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  depends <- if ("Depends" %in% colnames(desc)) desc[1, "Depends"] else ""
  floor_txt <- sub(".*R \\(>=\\s*([0-9.]+)\\).*", "\\1", depends)
  skip_if(identical(floor_txt, depends), "DESCRIPTION declares no R floor")

  lock_r <- jsonlite::fromJSON(lock_path)$R$Version
  skip_if(is.null(lock_r), "renv.lock records no R version")

  # expect_gte() subtracts, which numeric_version does not support.
  expect_true(
    package_version(lock_r) >= package_version(floor_txt),
    info = paste0("renv.lock pins R ", lock_r,
                  " but DESCRIPTION requires R >= ", floor_txt)
  )
})
