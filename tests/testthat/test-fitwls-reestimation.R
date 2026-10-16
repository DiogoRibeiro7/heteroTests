library(testthat)

# ---------------------------------------------------------------------------
# The bootstrap tests on a fitWLS() fit estimate the weights again in each
# replicate.
#
# fitWLS() estimates its weights from the residuals of the fit it is given.
# Before 0.15.0 the bootstrap tests held those weights fixed in every
# replicate, as if they were known, and rejected a true null hypothesis too
# often. Since 0.15.0 performWildBootstrapTest(), performWhiteTestBootstrap()
# and bootstrapTestStatistic() run fitWLS() again on each replicate of a fit
# that carries its record of the variance function, by default
# (reestimate_weights = NULL). reestimate_weights = FALSE holds the weights
# fixed, as before; any other model, weighted or not, has no record and keeps
# the path it always took.
#
# The checks below rebuild the replicates of each function by calling
# fitWLS() itself on the regenerated data, from the same random draws, so
# they fail if a replicate leaves out the estimation, estimates it from the
# wrong residuals or regressors, or forgets the weights or the offset of the
# model fitWLS() was given. The p-values the old path gave in 0.14.0 are
# written out where a call must return them.
# ---------------------------------------------------------------------------

make_fgls_fit <- function(seed = 3, n = 80, weights = FALSE, offset = FALSE) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + exp(0.3 * d$x) * rnorm(n)
  d$w <- 1 / d$x
  d$o <- 0.2 * d$x^2
  # The true weights of y, and a response with constant variance, for models
  # whose p-values are not all at the floor.
  d$wt <- exp(-0.6 * d$x)
  d$y0 <- 1 + 2 * d$x + 0.5 * d$z + rnorm(n)
  base <- if (weights && offset) {
    lm(y ~ x + z, data = d, weights = w, offset = o)
  } else if (weights) {
    lm(y ~ x + z, data = d, weights = w)
  } else if (offset) {
    lm(y ~ x + z, data = d, offset = o)
  } else {
    lm(y ~ x + z, data = d)
  }
  list(data = d, base = base, fit = fitWLS(base))
}

squared_pearson_r2 <- function(e, formula, data) {
  data$.e2 <- e^2
  nrow(data) * summary(lm(stats::update(formula, .e2 ~ .), data = data))$r.squared
}

bootstrap_p_value <- function(replicates, observed) {
  ok <- is.finite(replicates)
  (1 + sum(replicates[ok] >= observed)) / (sum(ok) + 1)
}

# fitWLS() run again on regenerated data, as a user would: the model it was
# given, refitted to `db`, and the same var_formula and form.
refit_by_fitwls <- function(obj, db, var_formula = NULL, form = "exponential") {
  base <- stats::update(obj$base, data = db)
  fitWLS(base, data = db, var_formula = var_formula, form = form)
}

quiet <- function(expr) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  suppressWarnings(expr)
}

# --- the default ---------------------------------------------------------------

test_that("the weights are estimated again by default on a fitWLS() fit only", {
  obj <- make_fgls_fit()
  d <- obj$data
  record <- attr(obj$fit, "variance_function")
  expect_identical(rwls_reestimation(obj$fit, NULL, "f"), record)
  expect_identical(rwls_reestimation(obj$fit, TRUE, "f"), record)
  expect_null(rwls_reestimation(obj$fit, FALSE, "f"))

  others <- list(
    lm = lm(y ~ x + z, data = d),
    weighted_lm = lm(y ~ x + z, data = d, weights = w),
    glm = glm(y ~ x + z, family = gaussian(), data = d),
    weighted_glm = glm(y ~ x + z, family = gaussian(), data = d, weights = w)
  )
  for (nm in names(others)) {
    expect_null(rwls_reestimation(others[[nm]], NULL, "f"), label = nm)
    expect_null(rwls_reestimation(others[[nm]], FALSE, "f"), label = nm)
  }
})

test_that("the default on a fitWLS() fit is TRUE, and differs from FALSE", {
  obj <- make_fgls_fit()
  d <- obj$data
  fit <- obj$fit
  calls <- list(
    wild = function(...) {
      performWildBootstrapTest(fit, d, B = 19, progress = FALSE, ...)$bootstrap$replicates
    },
    white = function(...) performWhiteTestBootstrap(fit, d, B = 19, ...)$boot_statistics,
    null = function(...) {
      bootstrapTestStatistic(performKoenkerTest, fit, d, B = 19, progress = FALSE, ...)$replicates
    },
    pairs = function(...) {
      bootstrapTestStatistic(performKoenkerTest, fit, d, B = 19, resample = "pairs",
                             progress = FALSE, ...)$replicates
    }
  )
  for (nm in names(calls)) {
    run <- function(...) {
      set.seed(5)
      quiet(calls[[nm]](...))
    }
    default <- run()
    expect_true(all(is.finite(default)), label = nm)
    expect_identical(default, run(reestimate_weights = TRUE), label = nm)
    expect_false(isTRUE(all.equal(default, run(reestimate_weights = FALSE))), label = nm)
  }
})

# --- the old path, unchanged ----------------------------------------------------

# p-values of 0.14.0, computed with set.seed(11) before each call on the data
# of make_fgls_fit(), B = 99 and progress = FALSE.
old_p_values <- list(
  fitwls = c(wild = 0.76, white = 0.55, null = 0.79),
  lm = c(wild = 0.14, white = 0.22, null = 0.14),
  weighted_lm = c(wild = 0.06, white = 0.06, null = 0.09),
  weighted_glm = c(wild = 0.06, white = 0.06)
)

three_p_values <- function(model, d, ...) {
  set.seed(11)
  wild <- performWildBootstrapTest(model, d, B = 99, progress = FALSE, ...)$p.value
  set.seed(11)
  white <- performWhiteTestBootstrap(model, d, B = 99, ...)$p.value
  out <- c(wild = wild, white = white)
  if (!inherits(model, "glm")) {
    set.seed(11)
    out[["null"]] <- bootstrapTestStatistic(performKoenkerTest, model, d, B = 99,
                                            progress = FALSE, ...)$p_value
  }
  out
}

test_that("reestimate_weights = FALSE returns the values of 0.14.0 on a fitWLS() fit", {
  obj <- make_fgls_fit()
  expect_identical(quiet(three_p_values(obj$fit, obj$data, reestimate_weights = FALSE)),
                   old_p_values$fitwls)

  # The old path is the one a weighted fit without the record takes: the same
  # fit stripped of it gives identical() results, field by field. The call
  # of bootstrapTestStatistic() records the argument, so it is left out.
  stripped <- obj$fit
  attr(stripped, "variance_function") <- NULL
  d <- obj$data
  run <- function(model, ...) {
    set.seed(12)
    wild <- performWildBootstrapTest(model, d, B = 29, progress = FALSE, ...)
    set.seed(12)
    white <- performWhiteTestBootstrap(model, d, B = 29, ...)
    set.seed(12)
    null <- bootstrapTestStatistic(performKoenkerTest, model, d, B = 29, progress = FALSE, ...)
    set.seed(12)
    pairs <- bootstrapTestStatistic(performKoenkerTest, model, d, B = 29, resample = "pairs",
                                    progress = FALSE, ...)
    null$call <- NULL
    pairs$call <- NULL
    list(wild = wild, white = white, null = null, pairs = pairs)
  }
  expect_identical(quiet(run(obj$fit, reestimate_weights = FALSE)), quiet(run(stripped)))
})

test_that("an lm, a weighted lm and a Gaussian glm return the values of 0.14.0", {
  obj <- make_fgls_fit()
  d <- obj$data
  models <- list(
    lm = lm(y0 ~ x + z, data = d),
    weighted_lm = lm(y ~ x + z, data = d, weights = wt),
    weighted_glm = glm(y ~ x + z, family = gaussian(), data = d, weights = wt)
  )
  for (nm in names(models)) {
    expect_identical(quiet(three_p_values(models[[nm]], d)), old_p_values[[nm]], label = nm)
    # And the default is FALSE for them, to the last field.
    full <- function(...) {
      set.seed(13)
      list(
        wild = performWildBootstrapTest(models[[nm]], d, B = 19, progress = FALSE, ...),
        white = performWhiteTestBootstrap(models[[nm]], d, B = 19, ...)
      )
    }
    expect_identical(quiet(full()), quiet(full(reestimate_weights = FALSE)), label = nm)
  }
  # A glm is refused by bootstrapTestStatistic() whatever the argument.
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, models$weighted_glm, d, B = 9,
                           progress = FALSE),
    "supports Gaussian `lm` models only"
  )
})

# --- what a replicate is ----------------------------------------------------------

test_that("each wild bootstrap replicate is a fitWLS() fit of the replicate", {
  for (variant in list(c(FALSE, FALSE), c(TRUE, FALSE), c(FALSE, TRUE))) {
    obj <- make_fgls_fit(weights = variant[1], offset = variant[2])
    d <- obj$data
    fit <- obj$fit
    n <- nrow(d)
    B <- 5L
    label <- sprintf("weights = %s, offset = %s", variant[1], variant[2])

    set.seed(21)
    ours <- quiet(performWildBootstrapTest(fit, d, B = B, progress = FALSE))

    s <- sqrt(weights(fit))
    pool <- s * residuals(fit) / sqrt(1 - hatvalues(fit))
    pool <- pool - mean(pool)
    set.seed(21)
    expected <- vapply(seq_len(B), function(b) {
      v <- sample(c(-1, 1), n, replace = TRUE)
      e <- sample(pool, n, replace = TRUE) * v
      db <- d
      db$y <- fitted(fit) + e / s
      refit <- quiet(refit_by_fitwls(obj, db))
      squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), ~ x + z, d)
    }, numeric(1))
    observed <- squared_pearson_r2(s * residuals(fit), ~ x + z, d)

    expect_equal(ours$bootstrap$replicates, expected, tolerance = 1e-8, label = label)
    expect_equal(ours$p.value, bootstrap_p_value(expected, observed), tolerance = 1e-12,
                 label = label)
  }
})

test_that("each White bootstrap replicate is a fitWLS() fit of the replicate", {
  for (variant in list(c(FALSE, FALSE), c(TRUE, TRUE))) {
    obj <- make_fgls_fit(weights = variant[1], offset = variant[2])
    d <- obj$data
    fit <- obj$fit
    B <- 5L
    label <- sprintf("weights = %s, offset = %s", variant[1], variant[2])

    set.seed(22)
    ours <- quiet(performWhiteTestBootstrap(fit, d, B = B))

    s <- sqrt(weights(fit))
    pool <- s * residuals(fit)
    pool <- pool - mean(pool)
    white <- ~ x + z + I(x^2) + I(z^2) + I(x * z)
    set.seed(22)
    expected <- vapply(seq_len(B), function(b) {
      db <- d
      db$y <- fitted(fit) + sample(pool, replace = TRUE) / s
      refit <- quiet(refit_by_fitwls(obj, db))
      squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), white, d)
    }, numeric(1))

    expect_equal(ours$boot_statistics, expected, tolerance = 1e-8, label = label)
  }
})

test_that("bootstrapTestStatistic() hands the test function a fitWLS() fit of each replicate", {
  obj <- make_fgls_fit(weights = TRUE)
  d <- obj$data
  fit <- obj$fit
  n <- nrow(d)
  B <- 5L

  set.seed(23)
  null <- quiet(bootstrapTestStatistic(performKoenkerTest, fit, d, B = B, progress = FALSE))
  s <- sqrt(weights(fit))
  pool <- s * residuals(fit) / sqrt(1 - hatvalues(fit))
  pool <- pool - mean(pool)
  set.seed(23)
  expected <- vapply(seq_len(B), function(b) {
    db <- d
    db$y <- fitted(fit) + sample(pool, n, replace = TRUE) / s
    refit <- quiet(refit_by_fitwls(obj, db))
    squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)

  # Pairs resampling draws each row with its variance regressors and its
  # weight in the model fitWLS() was given.
  set.seed(24)
  pairs <- quiet(bootstrapTestStatistic(performKoenkerTest, fit, d, B = B, resample = "pairs",
                                        progress = FALSE))
  set.seed(24)
  expected <- vapply(seq_len(B), function(b) {
    db <- d[sample.int(n, replace = TRUE), , drop = FALSE]
    refit <- quiet(refit_by_fitwls(obj, db))
    squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)
})

test_that("the variance function is estimated again with its form and its regressors", {
  # A power variance function of a variable the mean model does not use,
  # given to fitWLS() through `data`. The record holds its logarithm, so the
  # bootstrap needs neither the variable nor `data` to estimate it again.
  obj <- make_fgls_fit()
  d <- obj$data
  d$v <- d$x + runif(nrow(d))
  fit <- fitWLS(obj$base, data = d, var_formula = ~ v, form = "power")
  n <- nrow(d)
  B <- 5L
  set.seed(25)
  ours <- quiet(performWildBootstrapTest(fit, d[c("x", "z", "y")], B = B, progress = FALSE))
  s <- sqrt(weights(fit))
  pool <- s * residuals(fit) / sqrt(1 - hatvalues(fit))
  pool <- pool - mean(pool)
  set.seed(25)
  expected <- vapply(seq_len(B), function(b) {
    v <- sample(c(-1, 1), n, replace = TRUE)
    e <- sample(pool, n, replace = TRUE) * v
    db <- d
    db$y <- fitted(fit) + e / s
    refit <- quiet(fitWLS(lm(y ~ x + z, data = db), data = db, var_formula = ~ v, form = "power"))
    squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), ~ x + z, d)
  }, numeric(1))
  expect_equal(ours$bootstrap$replicates, expected, tolerance = 1e-8)
})

test_that("fitWLS() records the weights of a weighted model it refits", {
  obj <- make_fgls_fit()
  record <- attr(obj$fit, "variance_function")
  expect_named(record, c("form", "var_formula", "label", "regressors", "coefficients",
                         "index", "usable", "reason"))
  weighted <- make_fgls_fit(weights = TRUE)
  expect_identical(attr(weighted$fit, "variance_function")$initial_weights,
                   weights(weighted$base))
})

# --- failed replicates ------------------------------------------------------------

test_that("a replicate whose variance function cannot be fitted is left out", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")))
  obj <- make_fgls_fit()
  d <- obj$data
  fit <- obj$fit
  # Every second estimation of a variance function fails, as one does where
  # fitWLS() would fall back to equal weights.
  real <- rvariance_function_on
  count <- 0L
  local_mocked_bindings(rvariance_function_on = function(...) {
    count <<- count + 1L
    v <- real(...)
    if (count %% 2L == 0L) {
      v$usable <- FALSE
      v$reason <- "the fitted variance function is flat"
    }
    v
  })
  B <- 10L
  set.seed(31)
  wild <- quiet(performWildBootstrapTest(fit, d, B = B, progress = FALSE))
  failed <- seq(2L, B, by = 2L)
  expect_true(all(is.na(wild$bootstrap$replicates[failed])))
  expect_true(all(is.finite(wild$bootstrap$replicates[-failed])))
  expect_identical(wild$bootstrap$effective_samples, B %/% 2L)
  expect_equal(wild$p.value, bootstrap_p_value(wild$bootstrap$replicates, wild$statistic[[1]]))

  count <- 0L
  set.seed(32)
  white <- quiet(performWhiteTestBootstrap(fit, d, B = B))
  expect_true(all(is.na(white$boot_statistics[failed])))
  expect_equal(white$p.value,
               bootstrap_p_value(white$boot_statistics, unname(white$statistic[1])))

  count <- 0L
  set.seed(33)
  null <- quiet(bootstrapTestStatistic(performKoenkerTest, fit, d, B = B, progress = FALSE))
  expect_true(all(is.na(null$replicates[failed])))
  expect_identical(null$effective_samples, B %/% 2L)
  expect_equal(null$p_value, bootstrap_p_value(null$replicates, null$original_statistic))
})

test_that("when every replicate fails, the tests say so as before", {
  obj <- make_fgls_fit()
  d <- obj$data
  fit <- obj$fit
  # Constant variance regressors leave the variance function flat in every
  # replicate.
  record <- attr(fit, "variance_function")
  record$regressors[] <- 1
  attr(fit, "variance_function") <- record
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  expect_warning(
    wild <- performWildBootstrapTest(fit, d, B = 5, progress = FALSE),
    "All wild bootstrap replications failed"
  )
  expect_true(is.na(wild$p.value))
  expect_warning(
    null <- bootstrapTestStatistic(performKoenkerTest, fit, d, B = 5, progress = FALSE),
    "All bootstrap replicates failed"
  )
  expect_true(is.na(null$p_value))
  expect_error(suppressWarnings(performWhiteTestBootstrap(fit, d, B = 5)),
               "no usable replicates")
})

# --- what is refused ----------------------------------------------------------------

test_that("TRUE needs a fitWLS() fit, and the argument a single logical value", {
  obj <- make_fgls_fit()
  d <- obj$data
  for (model in list(lm(y ~ x + z, data = d), lm(y ~ x + z, data = d, weights = w),
                     glm(y ~ x + z, family = gaussian(), data = d, weights = w))) {
    expect_error(
      performWildBootstrapTest(model, d, B = 9, progress = FALSE, reestimate_weights = TRUE),
      "performWildBootstrapTest\\(reestimate_weights = TRUE\\) needs a fit returned by fitWLS\\(\\)"
    )
    expect_error(
      performWhiteTestBootstrap(model, d, B = 9, reestimate_weights = TRUE),
      "needs a fit returned by fitWLS\\(\\)"
    )
  }
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, lm(y ~ x + z, data = d, weights = w), d,
                           B = 9, progress = FALSE, reestimate_weights = TRUE),
    "bootstrapTestStatistic\\(reestimate_weights = TRUE\\) needs a fit returned by fitWLS\\(\\)"
  )
  for (bad in list(NA, "yes", c(TRUE, FALSE), 1)) {
    expect_error(
      performWildBootstrapTest(obj$fit, d, B = 9, progress = FALSE, reestimate_weights = bad),
      "`reestimate_weights` must be NULL, TRUE or FALSE"
    )
  }
})

test_that("a fit on which fitWLS() fell back to equal weights holds them fixed", {
  obj <- make_fgls_fit()
  d <- obj$data
  fit <- obj$fit
  record <- attr(fit, "variance_function")
  record$usable <- FALSE
  record$reason <- "the fitted variance function is flat"
  attr(fit, "variance_function") <- record
  run <- function(...) {
    set.seed(41)
    quiet(performWildBootstrapTest(fit, d, B = 9, progress = FALSE, ...))
  }
  expect_identical(run(), run(reestimate_weights = FALSE))
  expect_error(run(reestimate_weights = TRUE), "fell back to equal weights")
})

test_that("a variance function of the response is refused unless the weights are held fixed", {
  obj <- make_fgls_fit()
  d <- obj$data
  fit <- quiet(fitWLS(obj$base, var_formula = ~ y))
  expect_error(
    performWildBootstrapTest(fit, d, B = 9, progress = FALSE),
    "`var_formula` uses the response"
  )
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, fit, d, B = 9, progress = FALSE),
    "`var_formula` uses the response"
  )
  expect_error(performWhiteTestBootstrap(fit, d, B = 9), "`var_formula` uses the response")
  expect_true(is.finite(quiet(performWildBootstrapTest(
    fit, d, B = 9, progress = FALSE, reestimate_weights = FALSE
  ))$p.value))
})

# --- the deprecated name, the robust tests and reproducibility ----------------------

test_that("rbootstrap_test_statistic() takes the argument and forwards it", {
  obj <- make_fgls_fit()
  d <- obj$data
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (value in list(NULL, FALSE)) {
    set.seed(51)
    expect_warning(
      alias <- rbootstrap_test_statistic(performKoenkerTest, obj$fit, d, B = 9,
                                         progress = FALSE, reestimate_weights = value),
      "deprecated"
    )
    set.seed(51)
    new_name <- bootstrapTestStatistic(performKoenkerTest, obj$fit, d, B = 9,
                                       progress = FALSE, reestimate_weights = value)
    expect_identical(alias$replicates, new_name$replicates)
    expect_identical(alias$p_value, new_name$p_value)
  }
})

test_that("the robust tests bootstrap a fitWLS() fit with its weights estimated again", {
  obj <- make_fgls_fit()
  d <- obj$data
  set.seed(52)
  wrapped <- quiet(performWhiteTestRobust(obj$fit, d, bootstrap = TRUE, B = 9))
  set.seed(52)
  direct <- quiet(bootstrapTestStatistic(performWhiteTest, obj$fit, d, B = 9,
                                         cross_products = TRUE, progress = FALSE))
  expect_identical(wrapped$robust_details$bootstrap$replicates, direct$replicates)
  set.seed(52)
  fixed <- quiet(bootstrapTestStatistic(performWhiteTest, obj$fit, d, B = 9,
                                        cross_products = TRUE, progress = FALSE,
                                        reestimate_weights = FALSE))
  expect_false(isTRUE(all.equal(direct$replicates, fixed$replicates)))
})

test_that("a seeded bootstrap that estimates the weights again repeats itself", {
  obj <- make_fgls_fit()
  d <- obj$data
  run <- function() {
    set.seed(61)
    list(
      wild = performWildBootstrapTest(obj$fit, d, B = 19, progress = FALSE),
      white = performWhiteTestBootstrap(obj$fit, d, B = 19),
      null = bootstrapTestStatistic(performKoenkerTest, obj$fit, d, B = 19,
                                    progress = FALSE)[c("replicates", "p_value")]
    )
  }
  expect_identical(quiet(run()), quiet(run()))
})

test_that("runHeteroTestsParallel() estimates the weights again on the stream of the test", {
  skip_on_cran()
  obj <- make_fgls_fit()
  d <- obj$data
  # With one worker the tests run in the session, test i on stream i.
  run <- function() {
    set.seed(62)
    quiet(runHeteroTestsParallel(obj$fit, d, tests = "wild_bootstrap", n_cores = 1))
  }
  first <- run()
  expect_identical(first$wild_bootstrap$bootstrap$replicates,
                   run()$wild_bootstrap$bootstrap$replicates)
  on_stream <- function(...) {
    set.seed(62)
    streams <- rtask_streams(1L)
    quiet(rrun_on_stream(streams[[1L]], function(i) {
      performWildBootstrapTest(obj$fit, d, progress = FALSE, ...)
    }, 1L))
  }
  expect_identical(first$wild_bootstrap$bootstrap$replicates,
                   on_stream(reestimate_weights = TRUE)$bootstrap$replicates)
  expect_false(isTRUE(all.equal(first$wild_bootstrap$bootstrap$replicates,
                                on_stream(reestimate_weights = FALSE)$bootstrap$replicates)))
})
