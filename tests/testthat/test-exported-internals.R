# 0.14.0 deprecates the exported internal helpers. Each keeps working, warns
# with .Deprecated() and returns or signals what its internal version does, and
# the package's own code calls the internal version, so it never warns itself.

internal_message <- "is deprecated: it is an internal helper of heteroTests"

test_that("std_error() and std_warning() warn and signal what the internal versions signal", {
  expect_warning(
    expect_error(std_error("invalid_data"), "data.frame"),
    internal_message,
    class = "deprecatedWarning"
  )
  expect_error(.std_error("invalid_data"), "data.frame")
  expect_warning(
    expect_warning(std_warning("cross_products_omitted"), "Cross-products omitted"),
    internal_message,
    class = "deprecatedWarning"
  )
})

test_that("checkData() and validateTestInputs() warn and return what the internal versions return", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(out <- checkData(mtcars), internal_message, class = "deprecatedWarning")
  expect_identical(out, .check_data(mtcars))
  expect_warning(
    out <- validateTestInputs(m, mtcars, "white"),
    internal_message,
    class = "deprecatedWarning"
  )
  expect_identical(out, .validate_test_inputs(m, mtcars, "white"))
})

test_that("the log-capture functions warn and keep working", {
  old_level <- ht_set_log_level("INFO")
  on.exit(ht_set_log_level(old_level), add = TRUE)
  expect_warning(ht_enable_log_capture(TRUE), internal_message, class = "deprecatedWarning")
  on.exit(suppressWarnings(ht_enable_log_capture(FALSE)), add = TRUE)
  expect_warning(ht_clear_log_history(), internal_message, class = "deprecatedWarning")
  utils::capture.output(heteroTests:::ht_log("INFO", "captured line"), type = "message")
  expect_warning(history <- ht_log_history(), internal_message, class = "deprecatedWarning")
  expect_true(any(grepl("captured line", history$message)))
})

test_that("the package does not call its own deprecated helpers", {
  calls <- 0L
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  withCallingHandlers(
    {
      suppressMessages(runHeteroTests(m, mtcars, tests = c("white", "koenker"), use_cache = FALSE))
      try(performWhiteTest(m, "not data"), silent = TRUE)
    },
    deprecatedWarning = function(w) {
      calls <<- calls + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(calls, 0L)
})
