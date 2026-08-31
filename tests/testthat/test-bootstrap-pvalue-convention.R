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
