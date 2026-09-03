library(testthat)

# Bootstrap and permutation p-values must use the finite-simulation convention
#
#     p = (1 + #{T_b >= T_obs}) / (B_eff + 1)
#
# rather than the raw proportion #/B. The raw proportion can return exactly
# zero, which is not an attainable p-value from a finite number of replicates,
# and it is anti-conservative in the tail.

test_that("performWhiteTestBootstrap uses (1 + #) / (B + 1)", {
  set.seed(99)
  n <- 60
  df <- data.frame(x = runif(n, 1, 4))
  df$y <- 1 + 2 * df$x + rnorm(n, sd = 0.1 + 2 * df$x)
  model <- lm(y ~ x, data = df)

  B <- 50
  res <- performWhiteTestBootstrap(model, df, B = B, parallel = FALSE)

  boot <- res$boot_statistics
  boot <- boot[is.finite(boot)]
  expected <- (1 + sum(boot >= unname(res$statistic[1]))) / (length(boot) + 1)

  expect_equal(unname(res$p.value), expected, tolerance = 1e-12)
  # Never exactly zero, and never above one.
  expect_gt(res$p.value, 0)
  expect_lte(res$p.value, 1)
  expect_gte(res$p.value, 1 / (length(boot) + 1))
})

test_that("rbootstrap_test_statistic uses (1 + #) / (B_eff + 1)", {
  set.seed(7)
  n <- 60
  df <- data.frame(x = runif(n, 1, 4))
  df$y <- 1 + 2 * df$x + rnorm(n, sd = 0.1 + 2 * df$x)
  model <- lm(y ~ x, data = df)

  out <- heteroTests:::rbootstrap_test_statistic(
    performBPTest, model, df,
    B = 40, progress = FALSE
  )

  reps <- out$replicates
  expected <- (1 + sum(reps >= out$original_statistic, na.rm = TRUE)) /
    (out$effective_samples + 1)

  expect_equal(out$p_value, expected, tolerance = 1e-12)
  expect_gt(out$p_value, 0)
})

test_that("wild bootstrap and rank permutation already use the convention", {
  set.seed(11)
  n <- 80
  df <- data.frame(x = runif(n, 1, 4))
  df$y <- 1 + 2 * df$x + rnorm(n, sd = 0.1 + 2 * df$x)
  model <- lm(y ~ x, data = df)

  wb <- performWildBootstrapTest(model, df, B = 60)
  expect_gt(wb$p.value, 0)
  expect_gte(wb$p.value, 1 / (wb$bootstrap$effective_samples + 1))

  rp <- performRankPermutationTest(model, df, B = 60, progress = FALSE)
  expect_gt(rp$p.value, 0)
})

test_that("null-imposed resampling gives the bootstrap p-value power", {
  skip_on_cran()
  # Before 0.10.0 this p-value came from pairs resampling, so the replicates
  # were centred on the observed statistic and the p-value sat near 0.5
  # whatever the data: measured rejection under sd = x^2 was 0%. A single
  # strongly heteroscedastic sample is enough to catch a regression to that.
  set.seed(4)
  n <- 120
  x <- runif(n, 1, 5)
  d <- data.frame(x = x, y = 1 + 2 * x + rnorm(n, sd = x^2))
  m <- lm(y ~ x, data = d)

  out <- suppressWarnings(rbootstrap_test_statistic(
    performKoenkerTest, m, d, B = 199, progress = FALSE))

  expect_true(is.finite(out$p_value))
  expect_lt(out$p_value, 0.05)
})

test_that("pairs resampling returns replicates but no p-value", {
  skip_on_cran()
  # Pairs replicates describe the statistic's variability under the data as
  # they are, which is not a null distribution, so no p-value is reported.
  set.seed(4)
  n <- 60
  x <- runif(n, 1, 5)
  d <- data.frame(x = x, y = 1 + 2 * x + rnorm(n, sd = x))
  m <- lm(y ~ x, data = d)

  out <- suppressWarnings(rbootstrap_test_statistic(
    performKoenkerTest, m, d, B = 50, resample = "pairs", progress = FALSE))

  expect_true(is.na(out$p_value))
  expect_length(out$replicates, 50L)
  expect_true(all(is.finite(out$ci)))
})

test_that("null residuals are leverage-corrected before resampling", {
  skip_on_cran()
  # Resampling raw residuals under-disperses the regenerated errors, because
  # OLS residuals have variance sigma^2 (1 - h_i). An early version without the
  # correction rejected about 13% of the time under the null instead of 5%.
  set.seed(7)
  n <- 80
  x <- c(runif(n - 1, 1, 5), 40)          # one high-leverage point
  d <- data.frame(x = x, y = 1 + 2 * x + rnorm(n))
  m <- lm(y ~ x, data = d)

  e <- residuals(m)
  h <- hatvalues(m)
  corrected <- e / sqrt(pmax(1 - h, .Machine$double.eps))

  # The correction inflates the residual at the high-leverage point.
  expect_gt(var(corrected), var(e))
  expect_gt(abs(corrected[n]) / abs(e[n]), 1)
})

test_that("null resampling refuses models it cannot regenerate", {
  set.seed(3)
  n <- 60
  d <- data.frame(x = runif(n, 1, 5))
  d$y <- exp(1 + 0.5 * d$x + rnorm(n, sd = 0.3))

  # A transformed response: fitted() is on the log scale while the data column
  # holds y, so writing one into the other and refitting would take the
  # logarithm twice. This used to run and return p = 1.
  m_log <- lm(log(y) ~ x, data = d)
  expect_error(
    rbootstrap_test_statistic(performKoenkerTest, m_log, d, B = 5,
                              progress = FALSE),
    "plain response variable"
  )

  # A glm: fitted() is on the response scale, the residuals are deviance
  # residuals, and the refit would drop the family and link. This used to run
  # and return p = 0.952.
  d2 <- data.frame(x = runif(n, 1, 5))
  d2$cnt <- rpois(n, lambda = exp(0.5 + 0.4 * d2$x))
  g <- glm(cnt ~ x, data = d2, family = poisson)
  expect_error(
    rbootstrap_test_statistic(performKoenkerTest, g, d2, B = 5,
                              progress = FALSE),
    "Gaussian `lm` models only"
  )


  # Pairs resampling is scale-agnostic, so it remains available for the
  # transformed response. `replicates` is preallocated to B and filled with NA
  # when a replicate fails, so its length alone would pass even if every
  # replicate had failed; assert the values instead.
  out <- suppressWarnings(rbootstrap_test_statistic(
    performKoenkerTest, m_log, d, B = 10, resample = "pairs", progress = FALSE))
  expect_length(out$replicates, 10L)
  expect_true(all(is.finite(out$replicates)))
})

test_that("a glm is refused by both resampling strategies", {
  # Each replicate is refitted with safe_lm(), which drops a glm's family and
  # link: on a Poisson fit of counts the coefficients move from (0.457, 0.406)
  # on the log link to (-0.931, 2.281) on the identity scale, so the replicates
  # would describe a different model from the one supplied.
  set.seed(3)
  n <- 80
  d <- data.frame(x = runif(n, 1, 5))
  d$cnt <- rpois(n, lambda = exp(0.5 + 0.4 * d$x))
  g <- glm(cnt ~ x, data = d, family = poisson)

  for (strategy in c("null", "pairs")) {
    expect_error(
      rbootstrap_test_statistic(performKoenkerTest, g, d, B = 5,
                                resample = strategy, progress = FALSE),
      "Gaussian `lm` models only"
    )
  }
})
