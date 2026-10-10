library(testthat)

# ---------------------------------------------------------------------------
# The resampling tests on weighted fits.
#
# A weighted lm is ordinary least squares on sqrt(w) y and sqrt(w) X, and its
# errors have constant variance there under the null hypothesis. On a weighted
# fit performWildBootstrapTest(), performWhiteTestBootstrap() and
# rbootstrap_test_statistic() resample in that regression. The wild bootstrap
# refits it directly; the other two hand a model to a test function, so they
# refit on the original scale with the weights. Each check below rebuilds the
# replicates of one function along the other route, from the same random
# draws, so it fails if a replicate drops the weights, resamples the raw
# residuals, forgets to return a draw to the scale of its observation, or
# omits the centring.
# ---------------------------------------------------------------------------

make_weighted_fit <- function(seed = 7, n = 150, weight_power = 4) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x^2 * rnorm(n)
  d$w <- 1 / d$x^weight_power
  list(data = d, model = lm(y ~ x + z, data = d, weights = w))
}

squared_pearson_r2 <- function(e, formula, data) {
  data$.e2 <- e^2
  nrow(data) * summary(lm(stats::update(formula, .e2 ~ .), data = data))$r.squared
}

bootstrap_p_value <- function(replicates, observed) {
  (1 + sum(replicates >= observed)) / (length(replicates) + 1)
}

# --- one route against the other ---------------------------------------------

test_that("the wild bootstrap of a weighted fit is the refit with its weights", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model
  n <- nrow(d)
  B <- 5L

  set.seed(11)
  ours <- performWildBootstrapTest(m, d, B = B, progress = FALSE)

  # The package works in the transformed regression. Here each replicate is
  # refitted on the original scale with lm(weights = w) and the statistic is
  # taken from its Pearson residuals.
  pool <- sqrt(d$w) * residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  set.seed(11)
  expected <- vapply(seq_len(B), function(b) {
    v <- sample(c(-1, 1), n, replace = TRUE)
    e <- sample(pool, n, replace = TRUE) * v
    db <- d
    db$y <- fitted(m) + e / sqrt(d$w)
    refit <- lm(y ~ x + z, data = db, weights = w)
    squared_pearson_r2(sqrt(d$w) * residuals(refit), ~ x + z, d)
  }, numeric(1))
  observed <- squared_pearson_r2(sqrt(d$w) * residuals(m), ~ x + z, d)

  expect_equal(unname(ours$statistic), observed, tolerance = 1e-8)
  expect_equal(ours$bootstrap$replicates, expected, tolerance = 1e-8)
  expect_equal(ours$p.value, bootstrap_p_value(expected, observed), tolerance = 1e-12)
})

test_that("the bootstrap White test of a weighted fit is the transformed regression", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model
  B <- 5L

  set.seed(12)
  ours <- suppressWarnings(performWhiteTestBootstrap(m, d, B = B))

  # The package refits with the weights. Here each replicate is the ordinary
  # least-squares fit of sqrt(w) yhat + e* on sqrt(w) X, with e* drawn from
  # the centred Pearson residuals.
  s <- sqrt(d$w)
  X <- model.matrix(m)
  pool <- s * residuals(m)
  pool <- pool - mean(pool)
  white <- ~ x + z + I(x^2) + I(z^2) + I(x * z)
  set.seed(12)
  expected <- vapply(seq_len(B), function(b) {
    e <- sample(pool, replace = TRUE)
    squared_pearson_r2(lm.fit(s * X, s * fitted(m) + e)$residuals, white, d)
  }, numeric(1))
  observed <- squared_pearson_r2(s * residuals(m), white, d)

  expect_equal(unname(ours$statistic), observed, tolerance = 1e-8)
  expect_equal(ours$boot_statistics, expected, tolerance = 1e-8)
  expect_equal(ours$p.value, bootstrap_p_value(expected, observed), tolerance = 1e-12)
})

test_that("null resampling in rbootstrap_test_statistic() follows the transformed regression", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model
  n <- nrow(d)
  B <- 5L

  set.seed(13)
  ours <- suppressWarnings(
    rbootstrap_test_statistic(performKoenkerTest, m, d, B = B, progress = FALSE)
  )

  s <- sqrt(d$w)
  X <- model.matrix(m)
  pool <- s * residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  set.seed(13)
  expected <- vapply(seq_len(B), function(b) {
    e <- sample(pool, n, replace = TRUE)
    squared_pearson_r2(lm.fit(s * X, s * fitted(m) + e)$residuals, ~ x + z, d)
  }, numeric(1))
  observed <- squared_pearson_r2(s * residuals(m), ~ x + z, d)

  expect_equal(ours$original_statistic, observed, tolerance = 1e-8)
  expect_equal(ours$replicates, expected, tolerance = 1e-8)
  expect_equal(ours$p_value, bootstrap_p_value(expected, observed), tolerance = 1e-12)
})

test_that("pairs resampling draws each row with its weight", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model
  n <- nrow(d)
  B <- 5L

  set.seed(14)
  ours <- suppressWarnings(rbootstrap_test_statistic(
    performKoenkerTest, m, d, B = B, resample = "pairs", progress = FALSE
  ))

  set.seed(14)
  expected <- vapply(seq_len(B), function(b) {
    db <- d[sample.int(n, replace = TRUE), , drop = FALSE]
    refit <- lm(y ~ x + z, data = db, weights = w)
    squared_pearson_r2(sqrt(db$w) * residuals(refit), ~ x + z, db)
  }, numeric(1))

  expect_equal(ours$replicates, expected, tolerance = 1e-8)
  expect_true(is.na(ours$p_value))
})

test_that("a fit with equal weights resamples as the fit without weights", {
  obj <- make_weighted_fit()
  d <- obj$data
  d$equal <- 3
  unweighted <- lm(y ~ x + z, data = d)
  equal <- lm(y ~ x + z, data = d, weights = equal)

  run <- function(model, f) {
    set.seed(21)
    suppressWarnings(f(model))
  }
  calls <- list(
    wild = function(model) {
      r <- performWildBootstrapTest(model, d, B = 29, progress = FALSE)
      list(statistic = r$statistic, p = r$p.value, replicates = r$bootstrap$replicates)
    },
    mammen = function(model) {
      r <- performWildBootstrapTest(model, d, B = 29, distribution = "mammen",
                                    progress = FALSE)
      list(statistic = r$statistic, p = r$p.value, replicates = r$bootstrap$replicates)
    },
    white = function(model) {
      r <- performWhiteTestBootstrap(model, d, B = 29)
      list(statistic = r$statistic, p = r$p.value, replicates = r$boot_statistics)
    },
    null = function(model) {
      r <- rbootstrap_test_statistic(performWhiteTest, model, d, B = 29, progress = FALSE)
      list(statistic = r$original_statistic, p = r$p_value, replicates = r$replicates)
    },
    pairs = function(model) {
      r <- rbootstrap_test_statistic(performKoenkerTest, model, d, B = 29,
                                     resample = "pairs", progress = FALSE)
      list(statistic = r$original_statistic, p = r$ci, replicates = r$replicates)
    }
  )
  for (nm in names(calls)) {
    a <- run(unweighted, calls[[nm]])
    b <- run(equal, calls[[nm]])
    expect_equal(unname(b$statistic), unname(a$statistic), tolerance = 1e-8, label = nm)
    expect_equal(b$p, a$p, tolerance = 1e-8, label = nm)
    expect_equal(b$replicates, a$replicates, tolerance = 1e-8, label = nm)
  }
})

test_that("the robust wrappers bootstrap a weighted fit through rbootstrap_test_statistic()", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model

  set.seed(31)
  wrapped <- suppressWarnings(performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19))
  set.seed(31)
  direct <- suppressWarnings(rbootstrap_test_statistic(
    performWhiteTest, m, d, B = 19, cross_products = TRUE, progress = FALSE
  ))

  expect_equal(wrapped$p.value_bootstrap, direct$p_value, tolerance = 1e-12)
  expect_equal(wrapped$robust_details$bootstrap$replicates, direct$replicates,
               tolerance = 1e-12)
  expect_true(all(is.finite(direct$replicates)))
})

test_that("resampling a weighted fit needs one weight per row of the data", {
  obj <- make_weighted_fit()
  d <- obj$data
  longer <- rbind(d, d[1:5, ])

  expect_error(
    rbootstrap_test_statistic(performKoenkerTest, obj$model, longer, B = 5,
                              resample = "pairs", progress = FALSE),
    "one weight per row"
  )
})

# --- simulated guards ------------------------------------------------------------

test_that("the bootstrap tests hold their level on a correctly weighted fit", {
  skip_on_cran()
  # sd = x^2 fitted with the weights 1 / x^4, so the null hypothesis holds for
  # the weighted model. In 0.12.0 each of these refused the fit.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  tests <- list(
    wild = list(reps = 80, f = function(o) {
      performWildBootstrapTest(o$model, o$data, B = 99, progress = FALSE)$p.value
    }),
    white = list(reps = 30, f = function(o) {
      performWhiteTestBootstrap(o$model, o$data, B = 49)$p.value
    }),
    rbootstrap = list(reps = 30, f = function(o) {
      rbootstrap_test_statistic(performKoenkerTest, o$model, o$data, B = 49,
                                progress = FALSE)$p_value
    })
  )
  for (nm in names(tests)) {
    p <- vapply(seq_len(tests[[nm]]$reps), function(i) {
      suppressWarnings(tests[[nm]]$f(make_weighted_fit(seed = 3000 + i)))
    }, numeric(1))
    expect_lt(mean(p < 0.05), 0.15, label = paste("rejection rate of", nm))
  }
})

test_that("the bootstrap tests reject weights that are too weak", {
  skip_on_cran()
  # sd = x^2 fitted with the weights 1 / x, which leave the weighted fit
  # heteroscedastic. A replicate that dropped the weights, or did not return
  # its draws to the scale of each observation, would carry the
  # heteroscedasticity into the reference distribution and lose this power.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  tests <- list(
    wild = function(o) {
      performWildBootstrapTest(o$model, o$data, B = 99, progress = FALSE)$p.value
    },
    white = function(o) performWhiteTestBootstrap(o$model, o$data, B = 49)$p.value,
    rbootstrap = function(o) {
      rbootstrap_test_statistic(performKoenkerTest, o$model, o$data, B = 49,
                                progress = FALSE)$p_value
    }
  )
  for (nm in names(tests)) {
    p <- vapply(seq_len(15), function(i) {
      suppressWarnings(tests[[nm]](make_weighted_fit(seed = 4000 + i, weight_power = 1)))
    }, numeric(1))
    expect_gt(mean(p < 0.05), 0.8, label = paste("rejection rate of", nm))
  }
})
