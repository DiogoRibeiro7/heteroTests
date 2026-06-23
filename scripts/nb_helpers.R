# Shared helpers for building the tutorial notebooks (sourced by build_nb_*.R).
# Each builder assembles a list of cells and calls write_nb(); execution and
# output population are done afterwards with:
#   jupyter nbconvert --to notebook --execute --inplace \
#     --ExecutePreprocessor.kernel_name=ir inst/tutorials/<file>.ipynb

suppressWarnings(suppressMessages(library(jsonlite)))

# markdown cell from one or more lines (each becomes its own source line)
md <- function(...) {
  lines <- c(...)
  list(cell_type = "markdown", metadata = setNames(list(), character()),
       source = as.list(paste0(lines, "\n")))
}

# code cell from one or more lines
code <- function(...) {
  lines <- c(...)
  list(cell_type = "code", metadata = setNames(list(), character()),
       execution_count = NULL, outputs = list(),
       source = as.list(paste0(lines, "\n")))
}

# A consistent "series navigation" markdown cell, with the current notebook bold.
series_nav <- function(current) {
  items <- list(
    c("01", "Detecting heteroscedasticity",   "01-detecting-heteroscedasticity.ipynb"),
    c("02", "Remediation",                     "02-remediating-heteroscedasticity.ipynb"),
    c("03", "Modern & scalable diagnostics",   "03-modern-and-scalable.ipynb"),
    c("04", "Group-wise variance tests",       "04-group-wise-variance.ipynb"),
    c("05", "Time series & ARCH effects",      "05-time-series-arch.ipynb"),
    c("06", "Choosing a test: a power study",  "06-choosing-a-test.ipynb")
  )
  links <- vapply(items, function(it) {
    label <- paste0(it[1], ". ", it[2])
    if (it[1] == current) paste0("**", label, "**") else paste0("[", label, "](", it[3], ")")
  }, character(1))
  md(paste0("*heteroTests tutorial series — ", paste(links, collapse = " · "), "*"))
}

notebook <- function(cells) {
  list(
    cells = cells,
    metadata = list(
      kernelspec = list(display_name = "R", language = "R", name = "ir"),
      language_info = list(name = "R", codemirror_mode = "r", file_extension = ".r",
                           mimetype = "text/x-r-source", pygments_lexer = "r",
                           version = "4.5.1")
    ),
    nbformat = 4L, nbformat_minor = 5L
  )
}

write_nb <- function(nb, path) {
  json <- toJSON(nb, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json, path)
  cat("wrote", path, "\n")
}
