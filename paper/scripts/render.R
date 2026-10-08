#!/usr/bin/env Rscript

# Render and validate the R Journal manuscript using the canonical rjtools
# workflow. Run this script from the repository root.

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

old_wd <- setwd(article_dir)
on.exit(setwd(old_wd), add = TRUE)

rmarkdown::render(
  input = basename(article_file),
  output_format = rjtools::rjournal_article(),
  envir = new.env(parent = globalenv()),
  clean = TRUE
)

pdf_file <- "heteroTests.pdf"
if (!file.exists(pdf_file)) {
  stop("R Journal PDF was not generated: ", pdf_file, call. = FALSE)
}

rjtools::initial_check_article(
  path = ".",
  pkg = "heteroTests",
  ask = FALSE
)

message("R Journal article rendered and checked: ", normalizePath(pdf_file))
