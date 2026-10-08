#!/usr/bin/env Rscript

# Render and validate the R Journal manuscript locally.
# Run this script from the repository root.

article_dir <- file.path("paper")
article_file <- file.path(article_dir, "heteroTests.Rmd")

required <- c("rmarkdown", "rjtools")
missing <- required[
  !vapply(required, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0L) {
  stop(
    "Missing package(s): ",
    paste(missing, collapse = ", "),
    ". Install development dependencies before rendering.",
    call. = FALSE
  )
}

if (!file.exists(article_file)) {
  stop("Manuscript not found: ", article_file, call. = FALSE)
}

run_spelling_check <- function(tex_file) {
  tex <- readLines(tex_file, warn = FALSE)

  # rjtools::check_spelling() currently expects the generated TeX to contain
  # a literal \\bibliography{} command. The current R Journal/citeproc output
  # may instead render references without that marker, leaving its internal
  # bib_loc empty. Add a boundary only to a temporary copy used by the checker.
  reference_boundary <- grep(
    "\\\\section\\*?\\{References\\}|\\\\begin\\{CSLReferences\\}",
    tex
  )

  insert_after <- if (length(reference_boundary) > 0L) {
    max(0L, reference_boundary[[1L]] - 1L)
  } else {
    length(tex)
  }

  tex <- append(
    tex,
    "\\bibliography{heteroTests}",
    after = insert_after
  )

  tmp <- tempfile("heteroTests-rjtools-spelling-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines(tex, file.path(tmp, "heteroTests.tex"))
  rjtools::check_spelling(tmp)
}

run_rjtools_checks <- function() {
  cat("\n--- R Journal checks ---\n")

  # This is the same check sequence used by initial_check_article(), except
  # spelling uses the parser-compatible temporary TeX copy above.
  rjtools::check_filenames(".")
  rjtools::check_structure(".")
  rjtools::check_folder_structure(".")
  rjtools::check_unnecessary_files(".")
  rjtools::check_cover_letter(".")

  rjtools::check_title(".", ignore = "heteroTests")
  rjtools::check_section(".")
  rjtools::check_abstract(".")
  run_spelling_check("heteroTests.tex")

  rjtools::check_proposed_pkg("heteroTests", ask = FALSE)
  rjtools::check_pkg_label(".")
  rjtools::check_packages_available(".")
  rjtools::check_bib_doi(".")
  rjtools::check_csl(".")
  rjtools::check_date(".", "heteroTests.Rmd")
}

old_wd <- setwd(article_dir)
on.exit(setwd(old_wd), add = TRUE)

rmarkdown::render(
  input = basename(article_file),
  output_format = rjtools::rjournal_article(),
  envir = new.env(parent = globalenv()),
  clean = TRUE
)

pdf_file <- "heteroTests.pdf"
tex_file <- "heteroTests.tex"

if (!file.exists(pdf_file)) {
  stop("R Journal PDF was not generated: ", pdf_file, call. = FALSE)
}
if (!file.exists(tex_file)) {
  stop("R Journal TeX source was not generated: ", tex_file, call. = FALSE)
}

run_rjtools_checks()

message("R Journal article rendered and checked: ", normalizePath(pdf_file))
