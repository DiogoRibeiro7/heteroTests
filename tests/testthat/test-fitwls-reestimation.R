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

# The value of `expr` and the messages of the warnings it gave, muffled.
collect_warnings <- function(expr) {
  messages <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = messages)
}

strip_record <- function(fit) {
  attr(fit, "variance_function") <- NULL
  fit
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
    expect_identical_numbers(default, run(reestimate_weights = TRUE), label = nm)
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
  expect_identical_numbers(quiet(run(obj$fit, reestimate_weights = FALSE)), quiet(run(stripped)))
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
    expect_identical_numbers(quiet(full()), quiet(full(reestimate_weights = FALSE)), label = nm)
  }
  # bootstrapTestStatistic() refused every glm. Since the eighth step of the
  # model context (N8) it bootstraps a Gaussian glm with the identity link as
  # the lm it is, with its prior weights, whatever the argument.
  for (reestimate in list(NULL, FALSE)) {
    boot <- function(model) {
      set.seed(5)
      bootstrapTestStatistic(performKoenkerTest, model, d, B = 9, progress = FALSE,
                             reestimate_weights = reestimate)
    }
    glm_boot <- boot(models$weighted_glm)
    lm_boot <- boot(models$weighted_lm)
    expect_equal(glm_boot$replicates, lm_boot$replicates, tolerance = 1e-8)
    expect_identical(glm_boot$p_value, lm_boot$p_value)
  }
})

# --- what a replicate is ----------------------------------------------------------

test_that("each wild bootstrap replicate is a fitWLS() fit of the replicate", {
  for (variant in list(c(FALSE, FALSE), c(TRUE, FALSE), c(FALSE, TRUE), c(TRUE, TRUE))) {
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

test_that("the regressors and weights of each row follow the rows of the data, in any order", {
  obj <- make_fgls_fit(weights = TRUE)
  fit <- obj$fit
  set.seed(27)
  shuffled <- obj$data[sample.int(nrow(obj$data)), ]
  n <- nrow(shuffled)
  B <- 5L
  rows <- rownames(shuffled)

  set.seed(28)
  null <- quiet(bootstrapTestStatistic(performKoenkerTest, fit, shuffled, B = B,
                                       progress = FALSE))
  s <- sqrt(weights(fit))
  names(s) <- names(residuals(fit))
  pool <- s * residuals(fit) / sqrt(1 - hatvalues(fit))
  pool <- pool - mean(pool)
  set.seed(28)
  expected <- vapply(seq_len(B), function(b) {
    db <- shuffled
    db$y <- fitted(fit)[rows] + sample(pool, n, replace = TRUE) / s[rows]
    refit <- quiet(refit_by_fitwls(obj, db))
    squared_pearson_r2(sqrt(weights(refit)) * residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)

  set.seed(29)
  pairs <- quiet(bootstrapTestStatistic(performKoenkerTest, fit, shuffled, B = B,
                                        resample = "pairs", progress = FALSE))
  set.seed(29)
  expected <- vapply(seq_len(B), function(b) {
    db <- shuffled[sample.int(n, replace = TRUE), , drop = FALSE]
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

test_that("the studentized statistic of a model with an offset is Koenker's", {
  # Its refit of the design left the offset out: on lm(y ~ x + z, offset = o)
  # in this design it gave 19.91, where Koenker's statistic is 18.79. Since
  # 0.16.0 the refit carries the offset, and the two agree, on the model and
  # on its fitWLS() fit.
  for (variant in list(c(FALSE, TRUE), c(TRUE, TRUE))) {
    obj <- make_fgls_fit(weights = variant[1], offset = variant[2])
    d <- obj$data
    label <- sprintf("weights = %s", variant[1])
    for (model in list(obj$base, obj$fit)) {
      koenker <- quiet(performKoenkerTest(model, d))
      for (r in list(quiet(performStudentizedBPTest(model, d)),
                     quiet(heteroTests:::.studentized_bp_test(model, d)),
                     quiet(performBPTestRobust(model, d)))) {
        expect_equal(unname(r$statistic), unname(koenker$statistic), tolerance = 1e-10,
                     label = label)
        expect_equal(r$p.value, koenker$p.value, tolerance = 1e-10, label = label)
      }
    }
  }
  obj <- make_fgls_fit(offset = TRUE)
  expect_equal(unname(quiet(performStudentizedBPTest(obj$base, obj$data))$statistic), 18.79065,
               tolerance = 1e-6)
})

test_that("a fit made before 0.15.0 is estimated again from the weights in its model frame", {
  # fitWLS() before 0.15.0 recorded no initial_weights. Its model frame is the
  # frame of the model it refitted, which holds those weights as (weights).
  obj <- make_fgls_fit(weights = TRUE, offset = TRUE)
  d <- obj$data
  old_fit <- obj$fit
  record <- attr(old_fit, "variance_function")
  record$initial_weights <- NULL
  attr(old_fit, "variance_function") <- record
  expect_identical(rwls_reestimation(old_fit, NULL, "f")$initial_weights,
                   weights(obj$base))
  run <- function(model) {
    set.seed(26)
    list(
      wild = performWildBootstrapTest(model, d, B = 9, progress = FALSE)$bootstrap$replicates,
      white = performWhiteTestBootstrap(model, d, B = 9)$boot_statistics,
      null = bootstrapTestStatistic(performKoenkerTest, model, d, B = 9,
                                    progress = FALSE)$replicates
    )
  }
  expect_identical_numbers(quiet(run(old_fit)), quiet(run(obj$fit)))
  # An unweighted fit has no weights in its frame, and is fitted unweighted.
  unweighted <- make_fgls_fit()
  expect_null(rwls_reestimation(unweighted$fit, NULL, "f")$initial_weights)
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

  # Each says once how many failed.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  count <- 0L
  caught <- collect_warnings(performWildBootstrapTest(fit, d, B = B, progress = FALSE))
  expect_identical(caught$warnings, paste0(
    "performWildBootstrapTest(): 5 of the 10 replicates failed and were left out, ",
    "among them any whose variance function could not be fitted again; the results ",
    "are computed from the 5 that remain."
  ))
})

test_that("a replicate does not warn about the residuals it floors", {
  # Pairs resampling of a model with a level of three rows draws, now and
  # then, a replicate with that level once, whose residual there is zero.
  # fitWLS() floors it before taking logarithms and says so, which in a
  # replicate would warn once each time.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")))
  set.seed(30)
  n <- 60
  # The rare level is not the first, so it has a column of its own and a
  # replicate that draws it once fits that row exactly.
  d <- data.frame(x = runif(n, 1, 5),
                  g = factor(c(rep("a", 3), sample(c("b", "c"), n - 3, replace = TRUE)),
                             levels = c("b", "c", "a")))
  d$y <- 1 + d$x + as.integer(d$g) + exp(0.3 * d$x) * rnorm(n)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fit <- fitWLS(lm(y ~ x + g, data = d))
  run <- function() {
    set.seed(31)
    collect_warnings(bootstrapTestStatistic(performKoenkerTest, fit, d, B = 49,
                                            resample = "pairs", progress = FALSE))
  }
  caught <- run()
  expect_false(any(grepl("floored", caught$warnings)))
  expect_true(sum(is.finite(caught$value$replicates)) > 0L)
  # Without the muffling the same draws warn, once per replicate concerned,
  # and the replicates are the same.
  local({
    local_mocked_bindings(rwithout_floor_warning = function(expr) expr)
    loud <- run()
    expect_gt(sum(grepl("floored", loud$warnings)), 1L)
    expect_identical_numbers(loud$value$replicates, caught$value$replicates)
  })
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
  expect_identical_numbers(run(), run(reestimate_weights = FALSE))
  expect_error(run(reestimate_weights = TRUE), "fell back to equal weights")
})

test_that("a variance regressor of the response holds the weights fixed, with a warning", {
  obj <- make_fgls_fit()
  d <- obj$data
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # ~ y, and ~ ., whose expansion on the model frame holds the response.
  fits <- list(
    y = suppressWarnings(fitWLS(obj$base, var_formula = ~ y)),
    dot = suppressWarnings(fitWLS(obj$base, var_formula = ~ .))
  )
  expect_identical(colnames(attr(fits$dot, "variance_function")$regressors), c("y", "x", "z"))
  for (nm in names(fits)) {
    fit <- fits[[nm]]
    expect_error(
      performWildBootstrapTest(fit, d, B = 9, progress = FALSE, reestimate_weights = TRUE),
      "computed from the response", label = nm
    )
    run <- function(model, ...) {
      set.seed(43)
      list(
        wild = performWildBootstrapTest(model, d, B = 9, progress = FALSE, ...),
        white = performWhiteTestBootstrap(model, d, B = 9, ...),
        null = bootstrapTestStatistic(performKoenkerTest, model, d, B = 9,
                                      progress = FALSE, ...)[c("replicates", "p_value")]
      )
    }
    caught <- collect_warnings(run(fit))
    default <- caught$value
    # One from each function; the White test may add warnings of its own.
    expect_identical(sum(grepl("computed from the response", caught$warnings)), 3L,
                     label = nm)
    # The values of 0.14.0, which the same fit without its record gives.
    expect_identical_numbers(default, run(strip_record(fit)), label = nm)
    expect_identical_numbers(default, run(fit, reestimate_weights = FALSE), label = nm)
  }
})

# A variable of the response that the right-hand side does not use is
# regenerated with it, and a regressor of such a variable, or the response
# term itself, as `~ .` names it, is computed from the response.
response_rule_data <- function() {
  obj <- make_fgls_fit()
  d <- obj$data
  set.seed(48)
  d$yp <- exp(0.2 * d$x + 0.3 * rnorm(nrow(d)))
  d$r <- d$y / d$x
  d
}

test_that("a regressor of a variable the response regenerates holds the weights fixed", {
  d <- response_rule_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  ratio <- lm(I(y / x) ~ z, data = d)
  held <- list(
    ratio_y = suppressWarnings(fitWLS(ratio, data = d, var_formula = ~ y)),
    # Conservative: x is a variable of I(y / x) and not of the right-hand side.
    ratio_x = fitWLS(ratio, data = d, var_formula = ~ x),
    ratio_dot = suppressWarnings(fitWLS(ratio, var_formula = ~ .)),
    log_dot = suppressWarnings(fitWLS(lm(log(yp) ~ x + z, data = d), var_formula = ~ .))
  )
  expect_identical(colnames(attr(held$ratio_dot, "variance_function")$regressors),
                   c("`I(y/x)`", "z"))
  expect_identical(colnames(attr(held$log_dot, "variance_function")$regressors),
                   c("`log(yp)`", "x", "z"))
  for (nm in names(held)) {
    fit <- held[[nm]]
    expect_error(
      performWildBootstrapTest(fit, d, B = 9, progress = FALSE, reestimate_weights = TRUE),
      "computed from the response", label = nm
    )
    run <- function(...) {
      set.seed(47)
      performWildBootstrapTest(fit, d, B = 9, progress = FALSE, ...)
    }
    caught <- collect_warnings(run())
    expect_true(any(grepl("computed from the response.*d\\$r <- d\\$y / d\\$x",
                          caught$warnings)), label = nm)
    expect_identical_numbers(caught$value, suppressWarnings(run(reestimate_weights = FALSE)),
                             label = nm)
  }
})

test_that("a regressor of a variable the design holds fixed is estimated again", {
  d <- response_rule_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  estimated <- list(
    # The response given as a column says that x stays.
    column = fitWLS(lm(r ~ z, data = d), data = d, var_formula = ~ x),
    # x is on the right-hand side, so only y is regenerated.
    log_ratio = fitWLS(lm(log(yp / x) ~ log(x), data = d)),
    log_ratio_x = fitWLS(lm(log(yp / x) ~ log(x) + z, data = d), data = d, var_formula = ~ x),
    # An offset is part of the design.
    offset = fitWLS(lm(y ~ x + offset(o), data = d), data = d, var_formula = ~ o)
  )
  for (nm in names(estimated)) {
    fit <- estimated[[nm]]
    expect_identical(
      rregressors_from_response(fit, attr(fit, "variance_function")$regressors),
      character(), label = nm
    )
    set.seed(44)
    caught <- collect_warnings(performWildBootstrapTest(fit, d, B = 19, progress = FALSE))
    expect_false(any(grepl("computed from the response", caught$warnings)), label = nm)
    set.seed(44)
    off <- performWildBootstrapTest(fit, d, B = 19, progress = FALSE,
                                    reestimate_weights = FALSE)
    expect_true(all(is.finite(caught$value$bootstrap$replicates)), label = nm)
    expect_false(isTRUE(all.equal(caught$value$bootstrap$replicates,
                                  off$bootstrap$replicates)), label = nm)
  }
})

test_that("a column of the design is never computed from the response, whatever its name", {
  # A level of g spelled like the response names the design column
  # factor(g)y, which reads as the variable y; the design is held fixed in
  # every replicate, so the weights are estimated again.
  d <- response_rule_data()
  d$g <- rep(c("a", "y", "b"), length.out = nrow(d))
  d$gf <- factor(d$g)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (base in list(lm(y ~ x + factor(g), data = d), lm(y ~ x + gf, data = d))) {
    fit <- fitWLS(base)
    Z <- attr(fit, "variance_function")$regressors
    expect_true(any(grepl("y$", colnames(Z))))
    expect_identical(rregressors_from_response(fit, Z), character())
    set.seed(49)
    caught <- collect_warnings(performWildBootstrapTest(fit, d, B = 19, progress = FALSE))
    expect_false(any(grepl("computed from the response", caught$warnings)))
    set.seed(49)
    off <- performWildBootstrapTest(fit, d, B = 19, progress = FALSE,
                                    reestimate_weights = FALSE)
    expect_false(isTRUE(all.equal(caught$value$bootstrap$replicates,
                                  off$bootstrap$replicates)))
  }
  # The response term itself is not a column of the design, so `~ .` still
  # flags it when every variable of the response is on the right-hand side.
  both <- suppressWarnings(fitWLS(lm(I(y / x) ~ x + y, data = d), var_formula = ~ .))
  expect_identical(
    rregressors_from_response(both, attr(both, "variance_function")$regressors), "`I(y/x)`"
  )
})

test_that("the routes that inherit the default hold such weights fixed too, with a warning", {
  skip_on_cran()
  obj <- make_fgls_fit()
  d <- obj$data
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fit <- suppressWarnings(fitWLS(obj$base, var_formula = ~ y))
  through_runner <- function(model) {
    set.seed(45)
    runHeteroTests(model, d, tests = c("wild_bootstrap", "white_bootstrap"),
                   use_cache = FALSE)
  }
  # The White test warns on the outliers of some replicates by itself, so the
  # warnings are collected rather than expected one by one.
  caught <- collect_warnings(through_runner(fit))
  runner <- caught$value
  expect_true(any(grepl("computed from the response", caught$warnings)))
  plain <- suppressWarnings(through_runner(strip_record(fit)))
  # The runner did not fall back on another test: each name holds its test.
  expect_match(runner$wild_bootstrap$method, "Wild bootstrap")
  expect_match(runner$white_bootstrap$method, "Bootstrap White")
  expect_identical(runner$wild_bootstrap$bootstrap$replicates,
                   plain$wild_bootstrap$bootstrap$replicates)
  expect_identical(runner$white_bootstrap$boot_statistics,
                   plain$white_bootstrap$boot_statistics)

  robust <- function(model) {
    set.seed(46)
    performWhiteTestRobust(model, d, bootstrap = TRUE, B = 9)$robust_details$bootstrap
  }
  caught <- collect_warnings(robust(fit))
  expect_true(any(grepl("computed from the response", caught$warnings)))
  expect_identical(caught$value, suppressWarnings(robust(strip_record(fit))))
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
    expect_identical_numbers(alias$replicates, new_name$replicates)
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
  expect_identical_numbers(wrapped$robust_details$bootstrap$replicates, direct$replicates)
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
  expect_identical_numbers(quiet(run()), quiet(run()))
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
