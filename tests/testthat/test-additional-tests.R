library(testthat)
library(heteroTests)


test_that("Studentized BP detects heteroscedasticity", {
  set.seed(123)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 3 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- without_deprecation(performStudentizedBPTest(model, df))
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

# Until increment 10 of the model context these two assertions were made on
# prepare_model_data_for_test(), the preparation of `data` the tests used
# before the context, which no test called since increment 8; they are made
# on the tests, which match `data` through the context.
test_that("the additional tests take the rows of the fit from its data", {
  df <- mtcars
  model <- lm(mpg ~ wt + cyl, data = df)
  set.seed(4)
  shuffled <- df[sample(nrow(df)), ]
  expect_identical(
    performSzroeterTest(model, shuffled, "wt")$statistic,
    performSzroeterTest(model, df, "wt")$statistic
  )
})

test_that("the additional tests detect missing model rows", {
  df <- mtcars
  model <- lm(mpg ~ wt + cyl, data = df)
  trimmed <- df[-1, ]
  expect_error(
    performSzroeterTest(model, trimmed, "wt"),
    "requires `data` to contain the rows used to fit the model. Missing rows: Mazda RX4",
    fixed = TRUE
  )
})
