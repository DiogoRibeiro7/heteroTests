library(testthat)

# ---------------------------------------------------------------------------
# The resampling tests on weighted fits.
#
# A weighted lm is ordinary least squares on sqrt(w) y and sqrt(w) X, and its
# errors have constant variance there under the null hypothesis. On a weighted
# fit performWildBootstrapTest(), performWhiteTestBootstrap() and
# bootstrapTestStatistic() resample in that regression. The wild bootstrap
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

test_that("null resampling in bootstrapTestStatistic() follows the transformed regression", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model
  n <- nrow(d)
  B <- 5L

  set.seed(13)
  ours <- suppressWarnings(
    bootstrapTestStatistic(performKoenkerTest, m, d, B = B, progress = FALSE)
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
  ours <- suppressWarnings(bootstrapTestStatistic(
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

check_equal_weights <- function(unweighted, equal, d, label) {
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
      r <- bootstrapTestStatistic(performWhiteTest, model, d, B = 29, progress = FALSE)
      list(statistic = r$original_statistic, p = r$p_value, replicates = r$replicates)
    },
    pairs = function(model) {
      r <- bootstrapTestStatistic(performKoenkerTest, model, d, B = 29,
                                  resample = "pairs", progress = FALSE)
      list(statistic = r$original_statistic, p = r$ci, replicates = r$replicates)
    }
  )
  for (nm in names(calls)) {
    a <- run(unweighted, calls[[nm]])
    b <- run(equal, calls[[nm]])
    what <- paste(label, nm)
    expect_equal(unname(b$statistic), unname(a$statistic), tolerance = 1e-8, label = what)
    expect_equal(b$p, a$p, tolerance = 1e-8, label = what)
    expect_equal(b$replicates, a$replicates, tolerance = 1e-8, label = what)
  }
}

test_that("a fit with equal weights resamples as the fit without weights", {
  # With and without an intercept: the White bootstrap centres the Pearson
  # residuals only when the model has one, because only then are the residuals
  # of the unweighted fit centred.
  obj <- make_weighted_fit()
  d <- obj$data
  d$equal <- 3
  for (f in list(y ~ x + z, y ~ 0 + x + z)) {
    check_equal_weights(lm(f, data = d), lm(f, data = d, weights = equal), d,
                        label = deparse(f))
  }
})

test_that("the robust wrappers bootstrap a weighted fit through bootstrapTestStatistic()", {
  obj <- make_weighted_fit()
  d <- obj$data
  m <- obj$model

  set.seed(31)
  wrapped <- suppressWarnings(performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19))
  set.seed(31)
  direct <- suppressWarnings(bootstrapTestStatistic(
    performWhiteTest, m, d, B = 19, cross_products = TRUE, progress = FALSE
  ))

  expect_equal(wrapped$p.value_bootstrap, direct$p_value, tolerance = 1e-12)
  expect_equal(wrapped$robust_details$bootstrap$replicates, direct$replicates,
               tolerance = 1e-12)
  expect_true(all(is.finite(direct$replicates)))
})

test_that("resampling a weighted fit needs the rows the model was fitted to", {
  obj <- make_weighted_fit()
  d <- obj$data
  longer <- rbind(d, d[1:5, ])
  renamed <- d
  rownames(renamed) <- paste0("r", seq_len(nrow(d)))

  # Rows the fit does not have are left out, and each row keeps its weight.
  for (strategy in c("null", "pairs")) {
    set.seed(15)
    ours <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, obj$model, longer, B = 5, resample = strategy, progress = FALSE
    ))
    set.seed(15)
    expected <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, obj$model, d, B = 5, resample = strategy, progress = FALSE
    ))
    expect_equal(ours$replicates, expected$replicates, tolerance = 1e-12)
  }
  # The package's tests refuse data whose row names do not match the fit, so
  # the refusal is checked with a test function that does not look at them.
  by_position <- function(model, data) list(statistic = sum(stats::residuals(model)^2))
  for (strategy in c("null", "pairs")) {
    expect_error(
      bootstrapTestStatistic(by_position, obj$model, renamed, B = 5,
                             resample = strategy, progress = FALSE),
      "rows the model was fitted to"
    )
  }
})

test_that("weighted resampling follows the rows of the data, in any order", {
  # The weights, the fitted values and the offset belong to rows. Before they
  # were matched by row name, data holding the same rows sorted by z drew each
  # row with another row's weight. The model has an offset argument, outside
  # the span of the regressors, so that its matching is checked too.
  obj <- make_weighted_fit()
  d <- obj$data
  d$o <- 0.3 * d$x^2
  m <- lm(y ~ x + z, data = d, weights = w, offset = o)
  sorted <- d[order(d$z), ]
  n <- nrow(sorted)

  set.seed(41)
  pairs <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, sorted, B = 5, resample = "pairs", progress = FALSE
  ))
  set.seed(41)
  expected <- vapply(seq_len(5), function(b) {
    db <- sorted[sample.int(n, replace = TRUE), , drop = FALSE]
    refit <- lm(y ~ x + z, data = db, weights = w, offset = o)
    squared_pearson_r2(sqrt(db$w) * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)

  set.seed(42)
  null <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, sorted, B = 5, progress = FALSE
  ))
  s <- sqrt(sorted$w)
  pool <- sqrt(d$w) * residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  fit_sorted <- fitted(m)[rownames(sorted)]
  set.seed(42)
  expected <- vapply(seq_len(5), function(b) {
    db <- sorted
    db$y <- fit_sorted + sample(pool, n, replace = TRUE) / s
    refit <- lm(y ~ x + z, data = db, weights = w, offset = o)
    squared_pearson_r2(s * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)
})

test_that("a weighted refit keeps an offset given as an argument", {
  # lm(..., offset = o) keeps o in the model frame, not in the formula, so a
  # refit from the formula alone would drop it.
  obj <- make_weighted_fit()
  d <- obj$data
  # Not in the span of the regressors, or dropping it would not show.
  d$o <- 0.3 * d$x^2
  m <- lm(y ~ x + z, data = d, weights = w, offset = o)
  n <- nrow(d)
  s <- sqrt(d$w)
  sX <- s * model.matrix(m)

  # A refit that keeps the offset has residuals on the weighted scale equal to
  # those of the regenerated errors on sqrt(w) X; one that drops it does not.
  set.seed(51)
  white <- suppressWarnings(performWhiteTestBootstrap(m, d, B = 5))
  pool <- s * residuals(m)
  pool <- pool - mean(pool)
  white_terms <- ~ x + z + I(x^2) + I(z^2) + I(x * z)
  set.seed(51)
  expected <- vapply(seq_len(5), function(b) {
    squared_pearson_r2(qr.resid(qr(sX), sample(pool, replace = TRUE)), white_terms, d)
  }, numeric(1))
  expect_equal(white$boot_statistics, expected, tolerance = 1e-8)

  set.seed(52)
  null <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, d, B = 5, progress = FALSE
  ))
  pool <- s * residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  set.seed(52)
  expected <- vapply(seq_len(5), function(b) {
    squared_pearson_r2(qr.resid(qr(sX), sample(pool, n, replace = TRUE)), ~ x + z, d)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)

  set.seed(53)
  pairs <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, d, B = 5, resample = "pairs", progress = FALSE
  ))
  set.seed(53)
  expected <- vapply(seq_len(5), function(b) {
    db <- d[sample.int(n, replace = TRUE), , drop = FALSE]
    refit <- lm(y ~ x + z, data = db, weights = w, offset = o)
    squared_pearson_r2(sqrt(db$w) * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)

  set.seed(54)
  wild <- performWildBootstrapTest(m, d, B = 5, progress = FALSE)
  pool <- s * residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  set.seed(54)
  expected <- vapply(seq_len(5), function(b) {
    v <- sample(c(-1, 1), n, replace = TRUE)
    e <- sample(pool, n, replace = TRUE) * v
    db <- d
    db$y <- fitted(m) + e / s
    refit <- lm(y ~ x + z, data = db, weights = w, offset = o)
    squared_pearson_r2(s * residuals(refit), ~ x + z, d)
  }, numeric(1))
  expect_equal(wild$bootstrap$replicates, expected, tolerance = 1e-8)
})

test_that("a column of the data cannot stand in for the weights of a refit", {
  # The weights are put into the call of each refit as values. Passed by name,
  # a column of the same name in the data would have been used instead.
  obj <- make_weighted_fit()
  d <- obj$data
  noisy <- d
  noisy$.ht_prior_weights <- runif(nrow(d))
  noisy$weights <- runif(nrow(d))

  run <- function(data) {
    set.seed(61)
    white <- suppressWarnings(performWhiteTestBootstrap(obj$model, data, B = 5))
    set.seed(62)
    null <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, obj$model, data, B = 5, progress = FALSE
    ))
    set.seed(63)
    pairs <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, obj$model, data, B = 5, resample = "pairs", progress = FALSE
    ))
    c(white$boot_statistics, null$replicates, pairs$replicates)
  }
  expect_equal(run(noisy), run(d), tolerance = 1e-12)
})

# --- simulated guards ------------------------------------------------------------

test_that("the bootstrap tests hold their level on a correctly weighted fit", {
  skip_on_cran()
  # sd = x^2 fitted with the weights 1 / x^4, so the null hypothesis holds for
  # the weighted model. In 0.12.0 each of these refused the fit.
  #
  # The rejection rate alone cannot catch a replicate that drops the weights
  # or leaves its draws on the weighted scale: such replicates are
  # heteroscedastic, so the observed statistic looks small against them and
  # the test never rejects. The p-values then pile up near 1. Under the null
  # hypothesis they are close to uniform, so their mean is checked as well;
  # with 30 replications its standard error is about 0.05.
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
      bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = 49,
                             progress = FALSE)$p_value
    })
  )
  for (nm in names(tests)) {
    p <- vapply(seq_len(tests[[nm]]$reps), function(i) {
      heteroTests:::clearValidationCache()
      suppressWarnings(tests[[nm]]$f(make_weighted_fit(seed = 3000 + i)))
    }, numeric(1))
    expect_lt(mean(p < 0.05), 0.15, label = paste("rejection rate of", nm))
    expect_gt(mean(p), 0.3, label = paste("mean p-value of", nm))
    expect_lt(mean(p), 0.7, label = paste("mean p-value of", nm))
  }
})

test_that("the bootstrap tests reject weights that are too weak", {
  skip_on_cran()
  # sd = x^2 fitted with the weights 1 / x, which leave the weighted fit
  # heteroscedastic, so the tests should reject. This guards the power the
  # resampling keeps. It does not guard the resampling itself: replicates that
  # drop the weights or leave their draws on the weighted scale still reject
  # these weights most of the time. The reconstructions above and the mean
  # p-value under the null hypothesis catch those.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  tests <- list(
    wild = function(o) {
      performWildBootstrapTest(o$model, o$data, B = 99, progress = FALSE)$p.value
    },
    white = function(o) performWhiteTestBootstrap(o$model, o$data, B = 49)$p.value,
    rbootstrap = function(o) {
      bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = 49,
                             progress = FALSE)$p_value
    }
  )
  for (nm in names(tests)) {
    p <- vapply(seq_len(15), function(i) {
      heteroTests:::clearValidationCache()
      suppressWarnings(tests[[nm]](make_weighted_fit(seed = 4000 + i, weight_power = 1)))
    }, numeric(1))
    expect_gt(mean(p < 0.05), 0.8, label = paste("rejection rate of", nm))
  }
})
