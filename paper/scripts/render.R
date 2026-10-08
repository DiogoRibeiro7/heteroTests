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

run_spelling_workaround <- function(tex_file) {
  tex <- readLines(tex_file, warn = FALSE)

  if (!any(grepl("\\\\bibliography\\{", tex))) {
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
  }

  tmp <- tempfile("heteroTests-rjtools-spelling-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE, force = TRUE), add = TRUE)

  writeLines(tex, file.path(tmp, "heteroTests.tex"))
  rjtools::check_spelling(tmp)
}

run_remaining_rjtools_checks <- function() {
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

check_error <- tryCatch(
  {
    rjtools::initial_check_article(
      path = ".",
      pkg = "heteroTests",
      ask = FALSE
    )
    NULL
  },
  error = identity
)

if (inherits(check_error, "error")) {
  tex <- readLines(tex_file, warn = FALSE)
  missing_bibliography_boundary <- !any(
    grepl("\\\\bibliography\\{", tex)
  )
  spelling_parser_failure <- identical(
    conditionMessage(check_error),
    "argument of length 0"
  )

  if (!missing_bibliography_boundary || !spelling_parser_failure) {
    stop(check_error)
  }

  message(
    "rjtools::check_spelling() could not locate a \\bibliography{} ",
    "boundary in the generated TeX. Running the spelling check on a ",
    "temporary parser-compatible copy, then continuing with the remaining ",
    "official rjtools checks."
  )

  run_spelling_workaround(tex_file)
  run_remaining_rjtools_checks()
}

message("R Journal article rendered and checked: ", normalizePath(pdf_file))
