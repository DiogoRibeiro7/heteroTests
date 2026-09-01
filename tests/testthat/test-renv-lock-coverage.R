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

# hetero_source_root() comes from helper-source-tree.R.

# renv.lock is JSON, but parsing it with an external JSON package would make
# these checks conditional on a package that is neither declared in DESCRIPTION
# nor pinned in the lockfile: skip_if_not_installed() would then silently
# disable the very drift check this file exists to run. renv writes one
# "Package" field per record and puts the R version ahead of the "Packages"
# object, so both values can be read with base R and the checks always execute.
lock_packages <- function(lock_path) {
  ln <- readLines(lock_path, warn = FALSE)
  hits <- grep('"Package"[[:space:]]*:', ln, value = TRUE)
  sub('^[[:space:]]*"Package"[[:space:]]*:[[:space:]]*"([^"]+)".*$', "\\1", hits)
}

lock_r_version <- function(lock_path) {
  ln <- readLines(lock_path, warn = FALSE)
  start <- grep('^[[:space:]]*"Packages"[[:space:]]*:', ln)
  head_ln <- if (length(start)) ln[seq_len(start[1] - 1L)] else ln
  hit <- grep('"Version"[[:space:]]*:', head_ln, value = TRUE)
  if (!length(hit)) return(NULL)
  sub('^[[:space:]]*"Version"[[:space:]]*:[[:space:]]*"([^"]+)".*$', "\\1", hit[1])
}

declared_field <- function(desc, field) {
  if (!field %in% colnames(desc)) return(character())
  x <- gsub("\\([^)]*\\)", "", desc[1, field])
  x <- trimws(unlist(strsplit(x, ",")))
  sort(unique(x[nzchar(x) & x != "R"]))
}

test_that("renv.lock covers every hard dependency", {
  root <- hetero_source_root()
  skip_if(is.null(root), "renv.lock checks only run from the source tree")
  lock_path <- file.path(root, "renv.lock")
  skip_if_not(file.exists(lock_path), "no renv.lock in this layout")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  imports <- declared_field(desc, "Imports")
  base_pkgs <- rownames(installed.packages(priority = "base"))
  hard <- setdiff(imports, base_pkgs)

  locked <- lock_packages(lock_path)
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
  root <- hetero_source_root()
  skip_if(is.null(root), "renv.lock checks only run from the source tree")
  lock_path <- file.path(root, "renv.lock")
  skip_if_not(file.exists(lock_path), "no renv.lock in this layout")

  desc <- read.dcf(file.path(root, "DESCRIPTION"))
  depends <- if ("Depends" %in% colnames(desc)) desc[1, "Depends"] else ""
  floor_txt <- sub(".*R \\(>=\\s*([0-9.]+)\\).*", "\\1", depends)
  skip_if(identical(floor_txt, depends), "DESCRIPTION declares no R floor")

  lock_r <- lock_r_version(lock_path)
  skip_if(is.null(lock_r), "renv.lock records no R version")

  # expect_gte() subtracts, which numeric_version does not support.
  expect_true(
    package_version(lock_r) >= package_version(floor_txt),
    info = paste0("renv.lock pins R ", lock_r,
                  " but DESCRIPTION requires R >= ", floor_txt)
  )
})
