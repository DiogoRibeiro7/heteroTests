library(testthat)
library(heteroTests)


test_that("Studentized BP detects heteroscedasticity", {
  set.seed(123)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 3 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performStudentizedBPTest(model, df)
  expect_lt(res$p.value, 0.05)
})

test_that("Bootstrap White test returns htest", {
  set.seed(123)
  n <- 50
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 2 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performWhiteTestBootstrap(model, df, B = 50, parallel = FALSE)
  expect_s3_class(res, "htest")
  expect_true(res$p.value >= 0 && res$p.value <= 1)
})

test_that("Szroeter test works", {
  set.seed(321)
  n <- 150
  x <- runif(n)
  z <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = z)
  df <- data.frame(x = x, y = y, z = z)
  model <- lm(y ~ x, data = df)
  res <- performSzroeterTest(model, df, "z")
  expect_s3_class(res, "htest")
})

test_that("prepare_model_data_for_test aligns residuals and data", {
  df <- mtcars
  model <- lm(mpg ~ wt + cyl, data = df)
  result <- heteroTests:::prepare_model_data_for_test(
    model,
    df,
    required_vars = c("mpg", "wt", "cyl"),
    test_label = "alignment",
    min_obs_model = 5L,
    min_obs_data = 5L
  )
  expect_equal(nrow(result$data), nrow(df))
  expect_equal(length(result$residuals), nrow(df))
})

test_that("prepare_model_data_for_test detects missing model rows", {
  df <- mtcars
  model <- lm(mpg ~ wt + cyl, data = df)
  trimmed <- df[-1, ]
  expect_error(
    heteroTests:::prepare_model_data_for_test(
      model,
      trimmed,
      required_vars = c("mpg", "wt", "cyl"),
      test_label = "alignment"
    ),
    "requires `data` to contain the rows used to fit the model",
    fixed = TRUE
  )
})
