library(testthat)

# The reference index of the documentation site lists the help pages in the
# sections of tools/reference-index.R, from which tools/build-docs.R writes
# docs/reference/index.md and the nav of mkdocs.yml. When pkgdown built the
# site, a topic missing from the index in _pkgdown.yml failed the build. These
# checks keep that guarantee for the sections: every exported object and every
# help page is in one, so a new one cannot be left out of the reference.
#
# tools/ is not in the built package, so they run from a source checkout only.
# skip_if_not_source_tree() comes from helper-source-tree.R.

reference_sections <- function(root) {
  path <- file.path(root, "tools", "reference-index.R")
  skip_if_not(file.exists(path), "tools/reference-index.R is not in this tree")
  source(path, local = new.env())$value
}

# The names (\name and \alias) and keywords of each help page, by file.
help_pages <- function(root) {
  files <- list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE)
  pages <- lapply(files, function(f) {
    rd <- tools::parse_Rd(f, encoding = "UTF-8")
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
    text <- function(tag) {
      vapply(rd[tags == tag], function(x) paste(unlist(x), collapse = ""), character(1))
    }
    list(names = unique(c(text("\\name"), text("\\alias"))), keywords = text("\\keyword"))
  })
  stats::setNames(pages, basename(files))
}

# The help files a section lists: by name or alias, and by keyword.
section_files <- function(entry, pages) {
  by_name <- names(Filter(function(p) any(entry$contents %in% p$names), pages))
  by_keyword <- if (is.null(entry$keyword)) {
    character()
  } else {
    names(Filter(function(p) entry$keyword %in% p$keywords, pages))
  }
  c(by_name, by_keyword)
}

test_that("every name the reference index lists has a help page", {
  root <- skip_if_not_source_tree()
  sections <- reference_sections(root)
  pages <- help_pages(root)
  documented <- unlist(lapply(pages, `[[`, "names"), use.names = FALSE)
  listed <- unlist(lapply(sections, `[[`, "contents"), use.names = FALSE)
  expect_identical(
    setdiff(listed, documented), character(),
    info = "listed in tools/reference-index.R but documented by no help page"
  )
})

test_that("every exported object is in a section of the reference index", {
  root <- skip_if_not_source_tree()
  sections <- reference_sections(root)
  pages <- help_pages(root)
  listed <- unique(unlist(lapply(sections, section_files, pages = pages)))
  indexed_names <- unlist(lapply(pages[listed], `[[`, "names"), use.names = FALSE)
  missing <- setdiff(getNamespaceExports("heteroTests"), indexed_names)
  expect_identical(
    sort(missing), character(),
    info = paste(
      "exported but in no section of tools/reference-index.R:",
      paste(sort(missing), collapse = ", ")
    )
  )
})

test_that("every help page is in a section of the reference index", {
  root <- skip_if_not_source_tree()
  sections <- reference_sections(root)
  pages <- help_pages(root)
  listed <- unique(unlist(lapply(sections, section_files, pages = pages)))
  missing <- setdiff(names(pages), listed)
  expect_identical(
    sort(missing), character(),
    info = paste(
      "help pages in no section of tools/reference-index.R:",
      paste(sort(missing), collapse = ", ")
    )
  )
})
