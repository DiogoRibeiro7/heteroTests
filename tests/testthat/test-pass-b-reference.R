library(testthat)

# ---------------------------------------------------------------------------
# Pass B of the statistical validation matrix: group-variance diagnostics.
# Every test is compared with an established implementation or an independent
# reconstruction of the statistic it claims to compute.
# ---------------------------------------------------------------------------

make_group_model <- function(seed = 2026, n_per_group = 30L, scales = c(1, 1.4, 2)) {
  set.seed(seed)
  k <- length(scales)
  grp <- factor(rep(letters[seq_len(k)], each = n_per_group))
  x <- rnorm(k * n_per_group)
  y <- 1 + 0.8 * x + rnorm(k * n_per_group, sd = rep(scales, each = n_per_group))
  data <- data.frame(y = y, x = x, grp = grp)
  list(data = data, model = lm(y ~ x, data = data), grp = grp)
}

test_that("Levene reproduces mean-centred car::leveneTest on model residuals", {
  skip_if_not_installed("car")
  obj <- make_group_model()
  r <- residuals(obj$model)
  ref <- car::leveneTest(r, obj$grp, center = mean)
  ours <- performLeveneTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$`F value`[1]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter), c(unname(ref$Df[1]), unname(ref$Df[2])), tolerance = 0)
  expect_equal(ours$p.value, unname(ref$`Pr(>F)`[1]), tolerance = 1e-10)
})

test_that("Brown-Forsythe reproduces median-centred car::leveneTest", {
  skip_if_not_installed("car")
  obj <- make_group_model()
  r <- residuals(obj$model)
  ref <- car::leveneTest(r, obj$grp, center = median)
  ours <- performBrownForsytheTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$`F value`[1]), tolerance = 1e-10)
  expect_equal(unname(ours$parameter), c(unname(ref$Df[1]), unname(ref$Df[2])), tolerance = 0)
  expect_equal(ours$p.value, unname(ref$`Pr(>F)`[1]), tolerance = 1e-10)
})

test_that("Bartlett reproduces stats::bartlett.test on model residuals", {
  obj <- make_group_model()
  ref <- stats::bartlett.test(residuals(obj$model), obj$grp)
  ours <- performBartlettTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 0)
  expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
  expect_equal(ours$p.value, ref$p.value, tolerance = 0)
})

test_that("Fligner-Killeen reproduces stats::fligner.test on model residuals", {
  obj <- make_group_model()
  ref <- stats::fligner.test(residuals(obj$model), obj$grp)
  ours <- performFlignerKilleenTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 0)
  expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
  expect_equal(ours$p.value, ref$p.value, tolerance = 0)
})

test_that("Hartley uses the maximum-F-ratio distribution", {
  obj <- make_group_model(n_per_group = 24L)
  r <- residuals(obj$model)
  vars <- tapply(r, obj$grp, var)
  fmax <- max(vars) / min(vars)
  df <- 23
  expected_p <- SuppDists::pmaxFratio(fmax, df = df, k = 3, lower.tail = FALSE)
  ours <- performHartleyFmaxTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), fmax, tolerance = 1e-12)
  expect_equal(unname(ours$parameter[["groups"]]), 3, tolerance = 0)
  expect_equal(unname(ours$parameter[["df"]]), df, tolerance = 0)
  expect_equal(ours$p.value, expected_p, tolerance = 1e-12)
})

test_that("Hartley reproduces vartest::hartley.test on balanced groups", {
  skip_if_not_installed("vartest")
  obj <- make_group_model(n_per_group = 25L)
  ref_data <- data.frame(r = residuals(obj$model), grp = obj$grp)
  ref <- vartest::hartley.test(r ~ grp, data = ref_data, verbose = FALSE)
  ours <- performHartleyFmaxTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-12)
  expect_equal(ours$p.value, ref$p.value, tolerance = 1e-12)
})

test_that("O'Brien reproduces vartest::obrien.test on model residuals", {
  skip_if_not_installed("vartest")
  obj <- make_group_model(n_per_group = 28L)
  ref_data <- data.frame(r = residuals(obj$model), grp = obj$grp)
  ref <- vartest::obrien.test(r ~ grp, data = ref_data, center = "mean", verbose = FALSE)
  ours <- performOBrienTest(obj$model, obj$data, "grp")

  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10)
  expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
  expect_equal(ours$p.value, ref$p.value, tolerance = 1e-10)
})

test_that("O'Brien matches an independent observation-level reconstruction", {
  obj <- make_group_model(n_per_group = 20L)
  r <- residuals(obj$model)
  grp <- obj$grp
  levels_grp <- levels(grp)
  transformed <- numeric(length(r))

  for (level in levels_grp) {
    idx <- which(grp == level)
    ni <- length(idx)
    centre <- mean(r[idx])
    si2 <- var(r[idx])
    transformed[idx] <- (
      (ni - 1.5) * ni * (r[idx] - centre)^2 - 0.5 * (ni - 1) * si2
    ) / ((ni - 1) * (ni - 2))
  }

  ref_tab <- anova(lm(transformed ~ grp))
  ours <- performOBrienTest(obj$model, obj$data, "grp")
  expect_equal(unname(ours$statistic), unname(ref_tab$`F value`[1]), tolerance = 1e-12)
  expect_equal(ours$p.value, unname(ref_tab$`Pr(>F)`[1]), tolerance = 1e-12)
})

test_that("modified Bartlett is exactly the Bartlett compatibility alias", {
  obj <- make_group_model()
  canonical <- performBartlettTest(obj$model, obj$data, "grp")
  alias <- performModifiedBartlettTest(obj$model, obj$data, "grp")

  expect_equal(alias$statistic, canonical$statistic, tolerance = 0)
  expect_equal(alias$parameter, canonical$parameter, tolerance = 0)
  expect_equal(alias$p.value, canonical$p.value, tolerance = 0)
})
