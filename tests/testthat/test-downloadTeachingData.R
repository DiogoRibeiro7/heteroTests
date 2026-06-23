library(testthat)
library(heteroTests)


test_that("downloadTeachingData retrieves tips dataset", {
  skip_if_not_installed("curl")
  skip_if_not(curl::has_internet(), "No internet connection available")
  tmp <- tempfile(fileext = ".csv")
  path <- downloadTeachingData("tips", destfile = tmp, quiet = TRUE)
  expect_true(file.exists(path))
  df <- read.csv(path)
  expect_true(nrow(df) > 0)
})

test_that("downloadTeachingData fails on invalid name", {
  expect_error(downloadTeachingData("invalid"), "Unknown dataset name")
})

test_that("downloadTeachingData honors destfile", {
  skip_if_not_installed("curl")
  skip_if_not(curl::has_internet(), "No internet connection available")
  dest <- tempfile(fileext = ".csv")
  path <- downloadTeachingData("tips", destfile = dest, quiet = TRUE)
  expect_equal(normalizePath(path), normalizePath(dest))
  expect_true(file.exists(dest))
})
