library(testthat)

test_that("memory warnings work correctly", {
  large_data <- data.frame(
    x = rnorm(50000),
    y = rnorm(50000),
    z = rnorm(50000)
  )

  expect_warning(
    heteroTests:::check_memory_usage(large_data, threshold_mb = 1),
    "Large dataset detected"
  )

  expect_silent(
    heteroTests:::check_memory_usage(mtcars, threshold_mb = 100)
  )
})

test_that("functions handle large datasets without crashing", {
  set.seed(123)
  n <- 5000
  large_data <- data.frame(
    y = rnorm(n, 10 + 0.5 * (1:n), sqrt(1:n)),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )

  model <- lm(y ~ x1 + x2, data = large_data)

  expect_no_error(suppressWarnings(performWhiteTest(model, large_data)))
  expect_no_error(suppressWarnings(performBreuschPaganTest(model, large_data)))
})

test_that("memory usage is reasonable for typical datasets", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  gc_before <- gc()
  result <- performWhiteTest(model, mtcars)
  gc_after <- gc()

  memory_used <- (gc_after[2, 2] - gc_before[2, 2])
  expect_lt(memory_used, 10)
})
