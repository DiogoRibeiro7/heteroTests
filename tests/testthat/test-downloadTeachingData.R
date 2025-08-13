library(testthat)
library(heteroTests)

context("Teaching data download")

test_that("downloadTeachingData retrieves tips dataset", {
  tmp <- tempfile(fileext = ".csv")
  path <- downloadTeachingData("tips", destfile = tmp)
  expect_true(file.exists(path))
  df <- read.csv(path)
  expect_true(nrow(df) > 0)
})

test_that("downloadTeachingData fails on invalid name", {
  expect_error(downloadTeachingData("invalid"), "Unknown dataset name")
})

test_that("downloadTeachingData honors destfile", {
  dest <- tempfile(fileext = ".csv")
  path <- downloadTeachingData("tips", destfile = dest)
  expect_equal(normalizePath(path), normalizePath(dest))
  expect_true(file.exists(dest))
})
