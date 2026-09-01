library(testthat)

# DESCRIPTION, CITATION.cff and .zenodo.json each carry the release version by
# hand, with nothing keeping them in step. They drifted to three different
# values before 0.7.1 -- DESCRIPTION at 0.7.1, CITATION.cff at 0.7.0 and
# .zenodo.json at 0.6.5, the last with a publication date three months stale.
# These checks fail at PR time instead of at release.

metadata_path <- function(file) {
  # Installed packages do not ship these, so resolve from the source tree.
  path <- testthat::test_path("..", "..", file)
  if (file.exists(path)) path else ""
}

test_that("DESCRIPTION, CITATION.cff and .zenodo.json agree on the version", {
  desc_path <- metadata_path("DESCRIPTION")
  cff_path <- metadata_path("CITATION.cff")
  zenodo_path <- metadata_path(".zenodo.json")
  skip_if_not(
    all(nzchar(c(desc_path, cff_path, zenodo_path))),
    "release metadata files are only present in the source tree"
  )

  desc_version <- unname(read.dcf(desc_path, fields = "Version")[1, 1])

  cff <- readLines(cff_path, warn = FALSE)
  cff_versions <- unique(trimws(gsub('"', "", sub("^\\s*version:\\s*", "", grep("^\\s*version:", cff, value = TRUE)))))

  zen <- readLines(zenodo_path, warn = FALSE)
  zen_version <- trimws(gsub('[",]', "", sub('.*"version"\\s*:\\s*', "", grep('"version"', zen, value = TRUE)[1])))

  expect_length(cff_versions, 1L)
  expect_identical(
    cff_versions, desc_version,
    info = paste0("CITATION.cff says ", paste(cff_versions, collapse = "/"),
                  ", DESCRIPTION says ", desc_version)
  )
  expect_identical(
    zen_version, desc_version,
    info = paste0(".zenodo.json says ", zen_version,
                  ", DESCRIPTION says ", desc_version)
  )
})

test_that("release dates are well formed and not in the future", {
  cff_path <- metadata_path("CITATION.cff")
  zenodo_path <- metadata_path(".zenodo.json")
  skip_if_not(
    all(nzchar(c(cff_path, zenodo_path))),
    "release metadata files are only present in the source tree"
  )

  cff <- readLines(cff_path, warn = FALSE)
  cff_date <- trimws(gsub('"', "", sub("^\\s*date-released:\\s*", "",
                                       grep("^\\s*date-released:", cff, value = TRUE)[1])))

  zen <- readLines(zenodo_path, warn = FALSE)
  zen_date <- trimws(gsub('[",]', "", sub('.*"publication_date"\\s*:\\s*', "",
                                          grep('"publication_date"', zen, value = TRUE)[1])))

  # Named so a failure says which file carries the bad date, and compared
  # against today with no slack: the previous `Sys.Date() + 1` let a date one
  # day in the future through, which is exactly what this test exists to catch.
  dates <- c(CITATION.cff = cff_date, .zenodo.json = zen_date)
  for (nm in names(dates)) {
    parsed <- as.Date(dates[[nm]], format = "%Y-%m-%d")
    expect_false(
      is.na(parsed),
      info = paste0(nm, " has an unparseable release date: ", dates[[nm]])
    )
    expect_lte(
      parsed, Sys.Date(),
      label = paste0(nm, " release date (", dates[[nm]], ")")
    )
  }
})
