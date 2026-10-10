library(testthat)

# The registered "szroeter" diagnostic, which runHeteroTests(tests = "szroeter")
# and every other caller of the registry runs. performSzroeterTest() needs an
# ordering and the registry passes only the model and the data, so the entry
# orders the observations by the fitted values. Up to 0.12.0 it ordered them by
# names(data)[2], so the result depended on the order of the columns.

szroeter_entry <- function() {
  get("szroeter", envir = heteroTests:::.diagnostic_registry)
}

run_szroeter <- function(model, data) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  runHeteroTests(model, data, tests = "szroeter", use_cache = FALSE)$szroeter
}

test_that("the registered Szroeter test orders the residuals by the fitted values", {
  # Two regressors, so the order of the fitted values is the order of neither.
  set.seed(31)
  n <- 90
  d <- data.frame(x1 = runif(n, 1, 4), x2 = rnorm(n))
  d$y <- 1 + d$x1 - 0.8 * d$x2 + rnorm(n, sd = 0.3 * d$x1)
  m <- lm(y ~ x1 + x2, data = d)
  expect_false(identical(order(fitted(m)), order(d$x1)))
  expect_false(identical(order(fitted(m)), order(d$x2)))

  # Szroeter (1978) with h_i = i, as in test-pass-a-reference.R.
  e_ordered <- residuals(m)[order(fitted(m))]
  h <- sum(seq_len(n) * e_ordered^2) / sum(e_ordered^2)
  q <- (h - (n + 1) / 2) / sqrt((n^2 - 1) / (6 * n))

  res <- run_szroeter(m, d)
  expect_equal(unname(res$statistic), q, tolerance = 1e-10)
  expect_equal(unname(res$estimate), h, tolerance = 1e-10)
  expect_equal(unname(res$parameter), n, tolerance = 0)
  expect_equal(res$p.value, pnorm(q, lower.tail = FALSE), tolerance = 1e-12)
  expect_match(res$alternative, "fitted values", fixed = TRUE)
})

test_that("the registered Szroeter test does not depend on the order of the columns", {
  # Before the correction, mtcars as given ordered by cyl, which the model does
  # not use; the other orders below put a regressor or the response second.
  m <- lm(mpg ~ wt + hp, data = mtcars)
  rest <- setdiff(names(mtcars), c("mpg", "wt", "hp"))
  orders <- list(
    names(mtcars),
    c("mpg", "wt", "hp", rest),
    c("mpg", "hp", "wt", rest),
    c("wt", "mpg", "hp", rest),
    rev(names(mtcars))
  )

  results <- lapply(orders, function(cols) run_szroeter(m, mtcars[, cols]))
  stats <- vapply(results, function(r) unname(r$statistic), numeric(1))
  p_values <- vapply(results, `[[`, numeric(1), "p.value")

  expect_equal(stats, rep(stats[1], length(stats)), tolerance = 0)
  expect_equal(p_values, rep(p_values[1], length(p_values)), tolerance = 0)

  # Without data, runHeteroTests() uses the model frame.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  no_data <- runHeteroTests(m, tests = "szroeter", use_cache = FALSE)$szroeter
  expect_equal(unname(no_data$statistic), stats[1], tolerance = 0)
})

test_that("with one regressor the order follows the sign of its slope", {
  # Ordering by the fitted values is ordering by x when the slope is positive
  # and by -x when it is negative, so the alternative is always variance that
  # rises with the mean. The variance here rises with x in both designs.
  set.seed(57)
  n <- 80
  d <- data.frame(x = runif(n, 1, 4))
  e <- rnorm(n, sd = d$x)

  d$y <- 1 + 2 * d$x + e
  up <- lm(y ~ x, data = d)
  res_up <- run_szroeter(up, d)
  by_x <- performSzroeterTest(up, d, order_by = "x")
  expect_equal(unname(res_up$statistic), unname(by_x$statistic), tolerance = 1e-12)
  expect_equal(res_up$p.value, by_x$p.value, tolerance = 1e-12)
  expect_lt(res_up$p.value, 0.05)

  d$y <- 1 - 2 * d$x + e
  down <- lm(y ~ x, data = d)
  res_down <- run_szroeter(down, d)
  by_x <- performSzroeterTest(down, d, order_by = "x")
  by_x_less <- performSzroeterTest(down, d, order_by = "x", alternative = "less")
  expect_equal(unname(res_down$statistic), -unname(by_x$statistic), tolerance = 1e-12)
  expect_equal(res_down$p.value, by_x_less$p.value, tolerance = 1e-12)
  expect_gt(res_down$p.value, 0.95)
})

test_that("the registered Szroeter test uses the rows the model used", {
  set.seed(83)
  n <- 70
  d <- data.frame(x1 = runif(n, 1, 4), x2 = rnorm(n))
  d$y <- 1 + d$x1 + d$x2 + rnorm(n, sd = d$x1)

  # Rows outside `subset` have no fitted value and take no part.
  m <- lm(y ~ x1 + x2, data = d, subset = x1 > 1.5)
  used <- d[names(fitted(m)), ]
  used$fv <- fitted(m)
  expected <- performSzroeterTest(m, used, order_by = "fv")
  res <- suppressWarnings(run_szroeter(m, d))
  expect_equal(unname(res$statistic), unname(expected$statistic), tolerance = 1e-12)
  expect_equal(unname(res$parameter), nrow(used), tolerance = 0)

  # Weighted fit: the Pearson residuals, ordered by the fitted values.
  w <- runif(n, 0.5, 2)
  mw <- lm(y ~ x1 + x2, data = cbind(d, w = w), weights = w)
  dw <- cbind(d, w = w, fv = fitted(mw))
  expected_w <- performSzroeterTest(mw, dw, order_by = "fv")
  res_w <- run_szroeter(mw, cbind(d, w = w))
  expect_equal(unname(res_w$statistic), unname(expected_w$statistic), tolerance = 1e-12)
})

test_that("the registered Szroeter test holds its level and has power", {
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  szroeter <- szroeter_entry()

  draw <- function(n, sd_fun) {
    d <- data.frame(x1 = runif(n, 1, 4), x2 = rnorm(n))
    mu <- 1 + d$x1 + 0.5 * d$x2
    d$y <- mu + rnorm(n, sd = sd_fun(mu))
    d
  }

  # Under homoscedastic Gaussian errors the fitted values are independent of
  # the residuals, so ordering by them keeps the level. 2000 replications:
  # Monte Carlo standard error 0.005 at 5%; the band is three of them.
  set.seed(20261010)
  p_null <- replicate(2000, {
    d <- draw(50, function(mu) 1)
    szroeter(lm(y ~ x1 + x2, data = d), d)$p.value
  })
  expect_gt(mean(p_null < 0.05), 0.035)
  expect_lt(mean(p_null < 0.05), 0.065)

  # Standard deviation proportional to the mean. Ordering the other way gives
  # no rejections in this design, so a wrong direction fails here.
  set.seed(20261011)
  p_alt <- replicate(300, {
    d <- draw(50, function(mu) 0.25 * mu)
    szroeter(lm(y ~ x1 + x2, data = d), d)$p.value
  })
  expect_gt(mean(p_alt < 0.05), 0.6)
})
