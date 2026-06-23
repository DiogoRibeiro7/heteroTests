library(testthat)
library(heteroTests)


test_that("White test matches published results", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  result <- performWhiteTest(model, mtcars)

  expected_statistic <- 6.543086
  expected_p_value <- 0.2568981

  expect_equal(as.numeric(result$statistic), expected_statistic, tolerance = 1e-4)
  expect_equal(result$p.value, expected_p_value, tolerance = 1e-4)
})

test_that("Breusch-Pagan test matches R's bptest", {
  skip_if_not_installed("lmtest")

  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)

  our_result <- performBreuschPaganTest(model, mtcars)
  reference_result <- lmtest::bptest(model, studentize = FALSE)

  expect_equal(as.numeric(our_result$statistic),
    as.numeric(reference_result$statistic),
    tolerance = 1e-6
  )
  expect_equal(unname(our_result$p.value),
    unname(reference_result$p.value),
    tolerance = 1e-6
  )
})

test_that("ARCH LM test matches known results", {
  set.seed(123)
  n <- 100
  e <- rnorm(n)
  y <- numeric(n)
  y[1] <- e[1]
  for (i in 2:n) {
    y[i] <- e[i] * sqrt(0.1 + 0.5 * y[i - 1]^2)
  }

  model <- lm(y ~ 1)
  result <- performArchLMTest(model, lags = 1)
  expect_lt(result$p.value, 0.05)
})
