library(testthat)

# R CMD check verifies that \usage sections agree with function signatures, but
# the local testthat run does not, so signature changes can pass here and fail
# on CI. That happened three times while the validation passes were landing.
# These checks close the gap by running R's own documentation tools directly.
#
# They also matter because man/ is maintained by hand for a large share of the
# package: roxygen2 generated 78 of the 130 pages and the other 52 were written
# directly, so nothing regenerates them when a signature changes.

# skip_if_not_source_tree() comes from helper-source-tree.R.

test_that("no code/documentation mismatches", {
  root <- skip_if_not_source_tree()
  mismatches <- tools::codoc(dir = root)
  expect_equal(
    length(mismatches), 0L,
    info = paste(
      "\\usage disagrees with the function signature for:",
      paste(names(mismatches), collapse = ", "),
      "- man/ is largely hand-written, so update the .Rd alongside the code."
    )
  )
})

test_that("every documented argument exists and every argument is documented", {
  root <- skip_if_not_source_tree()
  problems <- tools::checkDocFiles(dir = root)
  expect_equal(
    length(problems), 0L,
    info = paste("checkDocFiles reported:", paste(capture.output(print(problems)),
                                                  collapse = " "))
  )
})

test_that("no exported object is left undocumented", {
  root <- skip_if_not_source_tree()
  undocumented <- tools::undoc(dir = root)
  # undoc() always returns the same four categories (code objects, data sets,
  # S4 classes, S4 methods); what matters is whether any of them has entries.
  found <- unlist(lapply(undocumented, as.character), use.names = FALSE)
  expect_equal(
    length(found), 0L,
    info = paste("undocumented objects:", paste(found, collapse = ", "))
  )
})
