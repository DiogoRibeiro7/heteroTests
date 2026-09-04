library(testthat)

# Both statistics here were wrong against their published definitions, in ways
# that a reference comparison would not have caught because neither has a
# reference implementation among the packages we compare against. Simulated
# size is what caught them, so simulated size is what guards them.

skip_unless_slow <- function() {
  skip_on_cran()
}

test_that("performBPRandomEffectsTest holds its nominal level", {
  skip_unless_slow()
  # Before 0.11.0 the statistic omitted the "- 1" and the square from Breusch
  # and Pagan's equation 5, and scaled by T^2 rather than nT. The result sat at
  # T^2 / (2(T-1)) = 3.6 for T = 6, just under the chi-square(1) critical value
  # of 3.841, so it rejected about a third of the time under the null.
  n_i <- 30
  n_t <- 6
  reps <- 200

  set.seed(2024)
  p <- vapply(seq_len(reps), function(i) {
    id <- rep(seq_len(n_i), each = n_t)
    x <- runif(n_i * n_t, 1, 5)
    d <- data.frame(id = id, x = x, y = 1 + 2 * x + rnorm(n_i * n_t))
    suppressWarnings(
      performBPRandomEffectsTest(lm(y ~ x, data = d), d, "id")$p.value)
  }, numeric(1))

  # 200 replications put the Monte Carlo standard error at 1.5%, so a test at
  # the nominal level lands well inside this band and the old 33% does not.
  expect_lt(mean(p < 0.05), 0.12)
})

test_that("performBPRandomEffectsTest detects an individual effect", {
  skip_unless_slow()
  set.seed(11)
  n_i <- 30
  n_t <- 6
  id <- rep(seq_len(n_i), each = n_t)
  x <- runif(n_i * n_t, 1, 5)
  u <- rep(rnorm(n_i, sd = 2), each = n_t)
  d <- data.frame(id = id, x = x, y = 1 + 2 * x + u + rnorm(n_i * n_t))

  r <- suppressWarnings(performBPRandomEffectsTest(lm(y ~ x, data = d), d, "id"))
  expect_lt(r$p.value, 0.01)
})

test_that("performPesaranTest is standard normal under independence", {
  skip_unless_slow()
  # Before 0.11.0 the scaling read sqrt(N(N-1)/(2T)) times the mean pairwise
  # correlation, which is Pesaran's statistic divided by T: it came out T times
  # too small and the test never rejected. The statistic is asymptotically
  # N(0, 1), so its spread is the thing to check.
  n_i <- 25
  n_t <- 8
  reps <- 200

  set.seed(31)
  stats <- vapply(seq_len(reps), function(i) {
    id <- rep(seq_len(n_i), each = n_t)
    tt <- rep(seq_len(n_t), times = n_i)
    x <- runif(n_i * n_t, 1, 5)
    d <- data.frame(id = id, time = tt, x = x,
                    y = 1 + 2 * x + rnorm(n_i * n_t))
    as.numeric(suppressWarnings(
      performPesaranTest(lm(y ~ x, data = d), d, "id", "time"))$statistic)
  }, numeric(1))

  # The old statistic had standard deviation about 0.13 here; the published one
  # is close to 1.
  expect_gt(stats::sd(stats), 0.7)
  expect_lt(stats::sd(stats), 1.4)
})

test_that("performPesaranTest detects a common factor", {
  skip_unless_slow()
  set.seed(41)
  n_i <- 25
  n_t <- 8
  id <- rep(seq_len(n_i), each = n_t)
  tt <- rep(seq_len(n_t), times = n_i)
  x <- runif(n_i * n_t, 1, 5)
  f <- rnorm(n_t)
  d <- data.frame(id = id, time = tt, x = x,
                  y = 1 + 2 * x + 2 * f[tt] + rnorm(n_i * n_t))

  r <- suppressWarnings(performPesaranTest(lm(y ~ x, data = d), d, "id", "time"))
  expect_lt(r$p.value, 0.01)
})
