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

# Szroeter (1978) with h_i = i, as in test-pass-a-reference.R, with the
# residuals of `model` ordered by `key`. order() is stable, so ties keep the
# order of the rows.
szroeter_q <- function(model, key) {
  e_ordered <- residuals(model)[order(key)]
  n <- length(e_ordered)
  h <- sum(seq_len(n) * e_ordered^2) / sum(e_ordered^2)
  c(h = h, q = (h - (n + 1) / 2) / sqrt((n^2 - 1) / (6 * n)))
}

test_that("the registered Szroeter test orders the residuals by the fitted values", {
  # Two regressors, so the order of the fitted values is the order of neither.
  set.seed(31)
  n <- 90
  d <- data.frame(x1 = runif(n, 1, 4), x2 = rnorm(n))
  d$y <- 1 + d$x1 - 0.8 * d$x2 + rnorm(n, sd = 0.3 * d$x1)
  m <- lm(y ~ x1 + x2, data = d)
  mean_values <- drop(model.matrix(m) %*% coef(m))
  expect_false(identical(order(mean_values), order(d$x1)))
  expect_false(identical(order(mean_values), order(d$x2)))

  ref <- szroeter_q(m, mean_values)
  res <- run_szroeter(m, d)
  expect_equal(unname(res$statistic), unname(ref["q"]), tolerance = 1e-10)
  expect_equal(unname(res$estimate), unname(ref["h"]), tolerance = 1e-10)
  expect_equal(unname(res$parameter), n, tolerance = 0)
  expect_equal(res$p.value, pnorm(ref["q"], lower.tail = FALSE), tolerance = 1e-12,
               ignore_attr = TRUE)
  expect_match(res$alternative, "fitted values", fixed = TRUE)

  # The fitted values are matched to the data by row name, not by position:
  # the same model with the rows of the data shuffled gives the same result.
  shuffled <- run_szroeter(m, d[sample(n), ])
  expect_equal(unname(shuffled$statistic), unname(ref["q"]), tolerance = 1e-10)
})

test_that("the registered Szroeter test does not depend on the order of the columns", {
  # Before the correction, mtcars as given ordered by cyl, which the model does
  # not use. The other orders below put wt, hp, the response mpg and gear second.
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

test_that("equal fitted values tie exactly, however the model is written", {
  # lm() stores its fitted values as y - e, so two observations with the same
  # design row can differ in the last bits by an amount that depends on the
  # residual. Ordered by those values, the statistic changed when y was
  # rescaled or a factor was coded differently. The reference orders by the
  # group means, which tie exactly, and leaves the rows of a group in order.
  szroeter <- szroeter_entry()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)

  set.seed(3)
  n <- 50
  d <- data.frame(t = rep(0:1, each = n / 2))
  d$y <- 1 + d$t + rnorm(n, sd = 1 + d$t)
  m <- lm(y ~ t, data = d)
  ref <- unname(szroeter_q(m, ave(d$y, d$t))["q"])

  d10 <- transform(d, y = 10 * y)
  binary <- list(
    y = szroeter(m, d),
    y_times_10 = szroeter(lm(y ~ t, data = d10), d10),
    factor_t = szroeter(lm(y ~ factor(t), data = d), d)
  )
  for (nm in names(binary)) {
    expect_equal(unname(binary[[nm]]$statistic), ref, tolerance = 1e-10, info = nm)
  }

  set.seed(11)
  n <- 60
  g <- factor(sample(c("a", "b", "c"), n, TRUE))
  sds <- c(a = 1, b = 2, c = 3)[as.character(g)]
  d3 <- data.frame(g = g, y = sds + rnorm(n, sd = sds))
  ref3 <- unname(szroeter_q(lm(y ~ g, data = d3), ave(d3$y, d3$g))["q"])

  d3_sum <- d3
  contrasts(d3_sum$g) <- contr.sum(3)
  d3_helmert <- d3
  contrasts(d3_helmert$g) <- contr.helmert(3)
  d3_rev <- transform(d3, g = factor(g, levels = c("c", "b", "a")))
  groups <- list(
    treatment = szroeter(lm(y ~ g, data = d3), d3),
    sum = szroeter(lm(y ~ g, data = d3_sum), d3_sum),
    helmert = szroeter(lm(y ~ g, data = d3_helmert), d3_helmert),
    cell_means = szroeter(lm(y ~ 0 + g, data = d3), d3),
    levels_reversed = szroeter(lm(y ~ g, data = d3_rev), d3_rev)
  )
  for (nm in names(groups)) {
    expect_equal(unname(groups[[nm]]$statistic), ref3, tolerance = 1e-10, info = nm)
  }
})

test_that("the registered Szroeter test uses the rows the model used, and says nothing", {
  set.seed(83)
  n <- 70
  d <- data.frame(x1 = runif(n, 1, 4), x2 = rnorm(n))
  d$y <- 1 + d$x1 + d$x2 + rnorm(n, sd = d$x1)
  key <- function(m) drop(model.matrix(m) %*% coef(m))

  # Rows outside `subset` have no fitted value and take no part. Before the
  # data were restricted to the rows of the fit, they were reported as
  # "missing values in fitted values".
  m <- lm(y ~ x1 + x2, data = d, subset = x1 > 1.5)
  expect_warning(res <- run_szroeter(m, d), NA)
  expect_equal(unname(res$statistic), unname(szroeter_q(m, key(m))["q"]), tolerance = 1e-10)
  expect_equal(unname(res$parameter), sum(d$x1 > 1.5), tolerance = 0)

  # Data with more rows than the fit.
  m50 <- lm(y ~ x1 + x2, data = d[1:50, ])
  expect_warning(res50 <- run_szroeter(m50, d), NA)
  expect_equal(unname(res50$statistic), unname(szroeter_q(m50, key(m50))["q"]),
               tolerance = 1e-10)

  # Weighted fit: the Pearson residuals, ordered by the fitted values.
  w <- runif(n, 0.5, 2)
  mw <- lm(y ~ x1 + x2, data = cbind(d, w = w), weights = w)
  dw <- cbind(d, w = w, fv = key(mw))
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

  # A binary regressor: two distinct fitted values, so the order within each
  # group decides the statistic. Ordered by lm()'s stored fitted values, whose
  # rounding errors follow the residuals, the test rejected 7.6% of the time
  # with this seed.
  set.seed(20261012)
  p_binary <- replicate(2000, {
    d <- data.frame(t = rep(0:1, each = 25))
    d$y <- 1 + d$t + rnorm(50)
    szroeter(lm(y ~ t, data = d), d)$p.value
  })
  expect_gt(mean(p_binary < 0.05), 0.035)
  expect_lt(mean(p_binary < 0.05), 0.065)

  # Standard deviation proportional to the mean. Ordering the other way gives
  # no rejections in this design, so a wrong direction fails here.
  set.seed(20261011)
  p_alt <- replicate(300, {
    d <- draw(50, function(mu) 0.25 * mu)
    szroeter(lm(y ~ x1 + x2, data = d), d)$p.value
  })
  expect_gt(mean(p_alt < 0.05), 0.6)
})
