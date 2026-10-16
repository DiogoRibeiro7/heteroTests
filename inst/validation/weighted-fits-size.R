# Tests applied to weighted fits ------------------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/weighted-fits-size.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=50 Rscript ... .
# Release evidence uses N_MC=400 for the first block, the same as the full
# sweep, and five times that for the others, whose point is a difference of a
# few percentage points. MC_CORES runs the scenarios of block 4 in parallel.
#
# lm(..., weights = w) states that the error variance is sigma^2 / w_i. Two
# questions follow, and the two blocks answer them.
#
# Block 1, known weights. The data are heteroscedastic and the model is fitted
# with exactly the right weights. A test that reads the standardized residuals
# should then reject at its nominal level: the rejection rate here is a size.
# Before 0.12.0 the tests read the raw residuals and this script gave 1.000 for
# every heteroscedasticity diagnostic.
#
# Block 2, estimated weights. The weights come from fitWLS(), with the variance
# function correctly specified. The tests of constant variance take the weights
# as known, so their reference distributions are only approximate here;
# performVarianceFormTest() accounts for the estimation. The block records how
# far off the approximation is, at two sample sizes, because the question is
# whether the discrepancy goes away as the sample grows. It does not.
#
# Block 3, estimated weights from the wrong variance function. The variance is
# a power of x and fitWLS() fits an exponential. This is the case the tests of
# constant variance are usually asked to catch after a weighted fit. The block
# shows which of them can: a test on the variance regressors themselves has
# little left to find, and one that looks beyond them does.
#
# Block 4, the resampling tests. performWildBootstrapTest(),
# performWhiteTestBootstrap() and rbootstrap_test_statistic() regenerate the
# response and refit the model, on a weighted fit in the regression of
# sqrt(w) y on sqrt(w) X. Each runs with B = 499 at five times N_MC, where the
# 99% interval around a true 5% is [0.037, 0.063]. With exact weights the
# rejection rate is a size, and so it is with weights that fitWLS() estimated
# from a correctly specified variance function. The rows "held fixed" hold
# those weights fixed, reestimate_weights = FALSE, as the tests of block 2
# take them as known, and as the package did by default before 0.15.0. The
# rows "re-estimated" estimate them again in every replicate,
# reestimate_weights = TRUE, the default on a fitWLS() fit since 0.15.0.
#
# Those four rows were measured in 0.12.0, before the package could estimate
# the weights again, with a repetition of the resampling outside it: for the
# wild and the White bootstrap resample_weighted() below, which repeats their
# resampling with that one change, and for rbootstrap_test_statistic() a test
# function that calls fitWLS(), koenker_reestimated(). Their rows were not run
# again when the script was changed to call the package. On 200 of the 2000
# samples of each of the four, the package gives the p-values and the
# replicates of the repetition: weighted-fits-refit-reproduction.R, which
# reads the repetition from this file. Their numbers are those of 0.12.0, and
# their design text, which said "not in the package" there, was changed in
# 0.15.0 to the text this script writes. check_repetition() asserts, on one
# sample, that the repetition reproduces the package with the weights held
# fixed and with them estimated again. The power rows run at N_MC
# replications: the weights 1 / x where the standard deviation is x^2, which
# leave the weighted fit heteroscedastic. The last rows fit the design of
# block 2 with the weights of its true variance function, so that the rows
# with estimated weights have a comparison in the same design.
#
# Block 4 is run by scenario, each with its own seed, so its results do not
# depend on MC_CORES, which runs the scenarios in parallel through
# parallel::mclapply(). Blocks 1 to 3 run first and in sequence.
#
# Each replication of block 4 starts by clearing the package's validation
# cache. The tests store the result of every input validation, keyed by the
# model and data, and nothing removes the entries. A scenario of block 4
# validates about a million new models, and without the clearing each of the
# slowest scenarios grew past 7.5 GB of memory in four and a half hours. The
# cache only saves repeating a validation, so clearing it changes no value.

is_source_checkout <- file.exists("DESCRIPTION") &&
  any(grepl(
    "^Package:[[:space:]]*heteroTests[[:space:]]*$",
    readLines("DESCRIPTION", warn = FALSE),
    fixed = FALSE
  ))

if (requireNamespace("pkgload", quietly = TRUE) && is_source_checkout) {
  suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(heteroTests))
}

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

alpha <- 0.05
n_mc <- get_count("N_MC", "400", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
n_obs <- 150L
invisible(ht_set_log_level("SILENT"))

# --- block 1: data with known weights -----------------------------------------

# The cross-sectional design of the full sweep under its alternative, sd = x^2,
# fitted with the weights that undo it.
make_xs <- function() {
  x <- runif(n_obs, 1, 5)
  z <- runif(n_obs, 1, 5)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + x^2 * rnorm(n_obs), x = x, z = z)
  d$g <- cut(d$x, 3, labels = c("a", "b", "c"))
  d$w <- 1 / x^4
  list(model = lm(y ~ x + z, data = d, weights = w), data = d)
}

# A series whose standard deviation rises along the sample. Left unweighted
# that reads as volatility clustering to the ARCH-type tests.
make_ts <- function() {
  tt <- seq_len(n_obs)
  s <- 0.3 + tt / 50
  d <- data.frame(y = 1 + 0.01 * tt + s * rnorm(n_obs), x = tt, w = 1 / s^2)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

make_reset <- function() {
  x <- runif(n_obs, 1, 5)
  d <- data.frame(x = x, y = 1 + 2 * x + x^2 * rnorm(n_obs), w = 1 / x^4)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

make_panel <- function() {
  n_i <- 30L
  n_t <- 6L
  id <- rep(seq_len(n_i), each = n_t)
  tt <- rep(seq_len(n_t), times = n_i)
  x <- runif(n_i * n_t, 1, 5)
  d <- data.frame(id = id, time = tt, x = x,
                  y = 1 + 2 * x + x^2 * rnorm(n_i * n_t), w = 1 / x^4)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

KNOWN <- list(
  list("performCookWeisbergTest", make_xs, function(o) performCookWeisbergTest(o$model)),
  list("performDavidianCarrollTest", make_xs, function(o) performDavidianCarrollTest(o$model)),
  list("performHarveyTest", make_xs, function(o) performHarveyTest(o$model)),
  list("performNCVTest", make_xs, function(o) performCookWeisbergTest(o$model)),
  list("performSpearmanTest", make_xs, function(o) performSpearmanTest(o$model)),
  list("performSpreadLevelTest", make_xs, function(o) performSpreadLevelTest(o$model)),
  list("performBPTest", make_xs, function(o) performBPTest(o$model, o$data)),
  list("performKoenkerTest", make_xs, function(o) performKoenkerTest(o$model, o$data)),
  # performStudentizedBPTest() is deprecated in 0.14.0 and keeps its own
  # implementation. The row measures that function, whose warning the
  # suppressWarnings() around each call silences.
  list("performStudentizedBPTest", make_xs, function(o) performStudentizedBPTest(o$model, o$data)),
  list("performWhiteTest", make_xs, function(o) performWhiteTest(o$model, o$data)),
  list("performWhiteTestRobust", make_xs, function(o) performWhiteTestRobust(o$model, o$data)),
  list("performBPTestRobust", make_xs, function(o) performBPTestRobust(o$model, o$data)),
  list("performWhiteTestStreaming", make_xs,
       function(o) performWhiteTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performBPTestStreaming", make_xs,
       function(o) performBPTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performKoenkerTestStreaming", make_xs,
       function(o) performKoenkerTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performHighDimensionalTest", make_xs, function(o) performHighDimensionalTest(o$model, o$data)),
  list("performRankPermutationTest", make_xs,
       function(o) performRankPermutationTest(o$model, o$data, B = 199, progress = FALSE)),
  list("performBartlettTest", make_xs, function(o) performBartlettTest(o$model, o$data, "g")),
  list("performBrownForsytheTest", make_xs, function(o) performBrownForsytheTest(o$model, o$data, "g")),
  list("performFlignerKilleenTest", make_xs, function(o) performFlignerKilleenTest(o$model, o$data, "g")),
  list("performHartleyFmaxTest", make_xs, function(o) performHartleyFmaxTest(o$model, o$data, "g")),
  list("performLeveneTest", make_xs, function(o) performLeveneTest(o$model, o$data, "g")),
  list("performOBrienTest", make_xs, function(o) performOBrienTest(o$model, o$data, "g")),
  # The default, robust = TRUE since 0.15.0. The row was regenerated then from
  # this seed; with robust = FALSE the same run gives the row of 0.14.0.
  list("performGlejserTest", make_xs, function(o) performGlejserTest(o$model, o$data, "x")),
  list("performParkTest", make_xs, function(o) performParkTest(o$model, o$data, "x")),
  list("performGQTest", make_xs, function(o) performGQTest(o$model, o$data, "x")),
  list("performSzroeterTest", make_xs, function(o) performSzroeterTest(o$model, o$data, "x")),
  list("performArchLMTest", make_ts, function(o) performArchLMTest(o$model, lags = 4)),
  list("performMcLeodLiTest", make_ts, function(o) performMcLeodLiTest(o$model, lags = 4)),
  list("performRESETTest", make_reset, function(o) performRESETTest(o$model)),
  list("performBPRandomEffectsTest", make_panel,
       function(o) performBPRandomEffectsTest(o$model, o$data, "id")),
  list("performPesaranTest", make_panel,
       function(o) performPesaranTest(o$model, o$data, "id", "time")),
  # This one fits quantile regressions without the weights and refuses a
  # weighted fit. It is listed so the matrix is complete; every call should
  # fail. The resampling tests are in block 4.
  list("performQuantileRegressionTest", make_xs,
       function(o) performQuantileRegressionTest(o$model, o$data))
)

# --- block 2: weights estimated by fitWLS() -----------------------------------

# The log-variance is linear in x, so the exponential variance function that
# fitWLS() fits by default is correctly specified.
make_fgls <- function(errors, n, sd_fun = function(x) exp(0.3 * x)) {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  e <- if (errors == "normal") rnorm(n) else rt(n, df = 5) / sqrt(5 / 3)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + sd_fun(x) * e, x = x, z = z)
  list(model = fitWLS(lm(y ~ x + z, data = d)), data = d)
}

# --- block 3: weights estimated from the wrong variance function --------------

# sigma^2 = x^3, a power of x, which the exponential form cannot follow.
sd_power <- function(x) x^1.5

ESTIMATED <- list(
  list("performKoenkerTest", function(o) performKoenkerTest(o$model, o$data)),
  list("performBPTest", function(o) performBPTest(o$model, o$data)),
  list("performWhiteTest", function(o) performWhiteTest(o$model, o$data)),
  list("performNCVTest", function(o) performCookWeisbergTest(o$model)),
  list("performHarveyTest", function(o) performHarveyTest(o$model)),
  list("performVarianceFormTest", function(o) performVarianceFormTest(o$model))
)

# --- block 4: the resampling tests --------------------------------------------

n_boot <- 499L
seed_boot <- 20261013L

# The design of block 1, sd = x^2, fitted with the weights 1 / x^4 that undo
# it, or with 1 / x, which leave the weighted fit heteroscedastic.
make_power_of_x <- function(n, weight_power) {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + x^2 * rnorm(n), x = x, z = z)
  d$w <- 1 / x^weight_power
  list(model = lm(y ~ x + z, data = d, weights = w), data = d)
}

# The design of block 2, sd = exp(0.3 x), fitted with the weights of its true
# variance function, exp(-0.6 x), in place of those fitWLS() estimates. The
# rows held fixed and re-estimated are compared with these.
make_true_weights <- function(n) {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + exp(0.3 * x) * rnorm(n), x = x, z = z)
  d$w <- exp(-0.6 * x)
  list(model = lm(y ~ x + z, data = d, weights = w), data = d)
}

# The statistics of the bootstrap tests, n R^2 of the squared Pearson residuals
# on x and z (Koenker's, which the wild bootstrap uses) or on x, z, their
# squares and their product (White's). check_repetition() asserts that they
# equal what performKoenkerTest() and performWhiteTest() return; they are
# written out because those functions validate their input on every call,
# which is most of the cost of a replicate.
koenker_statistic <- function(fit, data) {
  e2 <- (sqrt(stats::weights(fit)) * stats::residuals(fit))^2
  nrow(data) * summary(lm(e2 ~ x + z, data = data))$r.squared
}
white_statistic <- function(fit, data) {
  e2 <- (sqrt(stats::weights(fit)) * stats::residuals(fit))^2
  nrow(data) * summary(lm(e2 ~ x + z + I(x^2) + I(z^2) + I(x * z), data = data))$r.squared
}

# The resampling of performWildBootstrapTest() and performWhiteTestBootstrap()
# on a fitWLS() fit, repeated with the weights estimated again in every
# replicate, as it was measured in 0.12.0. With reestimate = FALSE each
# replicate is refitted with the weights of the fit, as the package functions
# do with reestimate_weights = FALSE, and with reestimate = TRUE fitWLS() is
# run on it, as they do with reestimate_weights = TRUE; check_repetition()
# asserts that the two agree either way. The wild bootstrap resamples the
# Pearson residuals divided by sqrt(1 - h) and centred, times Rademacher
# signs; the White bootstrap resamples the centred Pearson residuals. Each
# draw is returned to the scale of its observation by dividing by sqrt(w).
resample_weighted <- function(o, scheme, reestimate, B = n_boot) {
  model <- o$model
  d <- o$data
  n <- nrow(d)
  fixed_weights <- stats::weights(model)
  root_w <- sqrt(fixed_weights)
  pearson <- root_w * stats::residuals(model)
  pool <- if (scheme == "wild") {
    r <- pearson / sqrt(pmax(1 - stats::hatvalues(model), .Machine$double.eps))
    r - mean(r)
  } else {
    pearson - mean(pearson)
  }
  statistic <- if (scheme == "wild") koenker_statistic else white_statistic
  observed <- statistic(model, d)
  replicates <- vapply(seq_len(B), function(b) {
    e <- if (scheme == "wild") {
      v <- sample(c(-1, 1), n, replace = TRUE)
      sample(pool, n, replace = TRUE) * v
    } else {
      sample(pool, replace = TRUE)
    }
    db <- d
    db$y <- stats::fitted(model) + e / root_w
    db$fixed_weights <- fixed_weights
    tryCatch({
      fit <- if (reestimate) {
        fitWLS(lm(y ~ x + z, data = db))
      } else {
        lm(y ~ x + z, data = db, weights = fixed_weights)
      }
      statistic(fit, db)
    }, error = function(e) NA_real_)
  }, numeric(1))
  ok <- is.finite(replicates)
  list(
    p.value = (1 + sum(replicates[ok] >= observed)) / (sum(ok) + 1),
    replicates = replicates
  )
}

# Koenker's statistic after fitWLS() has estimated the weights again from the
# data it is given. On the data of the fit itself it is the statistic of the
# fit, so through rbootstrap_test_statistic() with the weights of the refit
# held fixed only the replicates change. This is how the row was measured in
# 0.12.0; the script now gives performKoenkerTest() to bootstrapTestStatistic()
# with reestimate_weights = TRUE.
koenker_reestimated <- function(model, data) {
  list(statistic = koenker_statistic(fitWLS(lm(y ~ x + z, data = data)), data))
}

check_repetition <- function() {
  set.seed(seed_boot - 1L)
  o <- make_fgls("normal", n_obs)
  same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8))
  koenker <- unname(performKoenkerTest(o$model, o$data)$statistic)
  white <- unname(suppressWarnings(performWhiteTest(o$model, o$data))$statistic)
  agree <- c(
    same(koenker_statistic(o$model, o$data), koenker),
    same(koenker_reestimated(o$model, o$data)$statistic, koenker),
    same(white_statistic(o$model, o$data), white)
  )
  if (!all(agree)) {
    stop("The statistics written out here differ from the package's.")
  }
  for (reestimate in c(FALSE, TRUE)) {
    for (scheme in c("wild", "white")) {
      set.seed(seed_boot - 2L)
      ours <- resample_weighted(o, scheme, reestimate = reestimate, B = 25L)
      set.seed(seed_boot - 2L)
      theirs <- if (scheme == "wild") {
        r <- performWildBootstrapTest(o$model, o$data, B = 25L, progress = FALSE,
                                      reestimate_weights = reestimate)
        list(p.value = r$p.value, replicates = r$bootstrap$replicates)
      } else {
        r <- suppressWarnings(performWhiteTestBootstrap(o$model, o$data, B = 25L,
                                                        reestimate_weights = reestimate))
        list(p.value = r$p.value, replicates = r$boot_statistics)
      }
      if (!same(ours$replicates, theirs$replicates) || ours$p.value != theirs$p.value) {
        stop("The repetition of the ", scheme, " bootstrap does not reproduce the package",
             if (reestimate) " with the weights estimated again." else ".")
      }
    }
  }
  # The test function that calls fitWLS(), given replicates whose weights are
  # held fixed, against the package estimating them again.
  set.seed(seed_boot - 3L)
  ours <- bootstrapTestStatistic(koenker_reestimated, o$model, o$data, B = 25L,
                                 progress = FALSE, reestimate_weights = FALSE)
  set.seed(seed_boot - 3L)
  theirs <- bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = 25L,
                                   progress = FALSE, reestimate_weights = TRUE)
  if (!same(ours$replicates, theirs$replicates) || ours$p_value != theirs$p_value) {
    stop("The test function that calls fitWLS() does not reproduce the package.")
  }
  message(
    "the statistics and the repeated wild and White bootstraps reproduce the ",
    "package, with the weights held fixed and estimated again"
  )
}

# The weights of a fitWLS() fit are held fixed or estimated again as the row
# says. The other rows fit lm() with weights, which are held fixed whatever
# the argument, and leave it at its default.
boot_p_value <- function(test, weights, o) {
  reestimate <- switch(weights,
    "estimated, held fixed" = FALSE,
    "estimated, re-estimated" = TRUE,
    NULL
  )
  switch(test,
    performWildBootstrapTest = performWildBootstrapTest(
      o$model, o$data, B = n_boot, progress = FALSE, reestimate_weights = reestimate
    ),
    performWhiteTestBootstrap = performWhiteTestBootstrap(
      o$model, o$data, B = n_boot, reestimate_weights = reestimate
    ),
    rbootstrap_test_statistic = {
      r <- bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = n_boot,
                                  progress = FALSE, reestimate_weights = reestimate)
      list(p.value = r$p_value)
    }
  )
}

boot_tests <- c(
  "performWildBootstrapTest", "performWhiteTestBootstrap", "rbootstrap_test_statistic"
)
boot_scenario <- function(test, weights, n) {
  expand.grid(test = test, weights = weights, n = n,
              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
}
# The wild bootstrap, the cheapest, is also run at 600 observations, where the
# tests of block 2 are further from their level than at 150.
boot_scenarios <- rbind(
  boot_scenario(boot_tests, "exact", n_obs),
  boot_scenario(boot_tests, c("estimated, held fixed", "estimated, re-estimated"), n_obs),
  boot_scenario("performWildBootstrapTest",
                c("estimated, held fixed", "estimated, re-estimated"), 600L),
  boot_scenario(boot_tests, "too weak", n_obs),
  # Added after the others so that their seeds, seed_boot + i, stay as they
  # were.
  boot_scenario(boot_tests, "true", n_obs),
  boot_scenario("performWildBootstrapTest", "true", 600L)
)
boot_design <- function(test, weights) {
  switch(weights,
    "exact" = "sd = x^2 fitted with weights 1 / x^4",
    "too weak" = "sd = x^2 fitted with weights 1 / x",
    "true" = "sd = exp(0.3 x), the fitWLS() design, fitted with the true weights exp(-0.6 x)",
    "estimated, held fixed" = "fitWLS(), exponential variance correctly specified",
    "estimated, re-estimated" = paste(
      "fitWLS(), exponential variance correctly specified;",
      "fitWLS() again in each replicate"
    )
  )
}

run_boot_scenario <- function(i) {
  s <- boot_scenarios[i, ]
  # A power near 1 is measured precisely with fewer replications.
  replications <- if (s$weights == "too weak") n_mc else 5L * n_mc
  set.seed(seed_boot + i)
  p <- vapply(seq_len(replications), function(rep) {
    # The package keeps the result of every input validation, and one
    # replication validates some 500 new models. Clearing that cache keeps the
    # memory of a scenario bounded; it changes no value and draws no number.
    heteroTests:::clearValidationCache()
    o <- switch(s$weights,
      "exact" = make_power_of_x(s$n, 4),
      "too weak" = make_power_of_x(s$n, 1),
      "true" = make_true_weights(s$n),
      make_fgls("normal", s$n)
    )
    pval(function(o) boot_p_value(s$test, s$weights, o), o)
  }, numeric(1))
  summarise(s$test, s$weights, boot_design(s$test, s$weights), "normal", s$n, p)
}

# --- run ----------------------------------------------------------------------

pval <- function(fn, o) {
  r <- tryCatch(suppressWarnings(suppressMessages(fn(o))), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

summarise <- function(test, weights, design, errors, n, p) {
  ok <- p[!is.na(p)]
  rate <- if (length(ok)) mean(ok < alpha) else NA_real_
  message(sprintf("%-30s %-9s %-6s n=%3d reject %s%s", test, weights, errors, n,
                  if (is.na(rate)) "  .  " else sprintf("%.3f", rate),
                  if (length(ok) < length(p)) {
                    sprintf("   [%d of %d calls failed]", length(p) - length(ok), length(p))
                  } else {
                    ""
                  }))
  data.frame(
    test = test,
    weights = weights,
    design = design,
    errors = errors,
    n = n,
    replications = length(p),
    rejection_rate = rate,
    mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
    effective = length(ok),
    failures = length(p) - length(ok),
    stringsAsFactors = FALSE
  )
}

rows <- list()
for (t in KNOWN) {
  set.seed(20261008L)
  p <- vapply(seq_len(n_mc), function(i) pval(t[[3]], t[[2]]()), numeric(1))
  rows[[length(rows) + 1L]] <- summarise(
    t[[1]], "known", "heteroscedastic data, exact weights", "normal", n_obs, p
  )
}
for (errors in c("normal", "t5")) {
  for (n in c(150L, 600L)) {
    for (t in ESTIMATED) {
      set.seed(20261009L)
      p <- vapply(seq_len(5L * n_mc), function(i) pval(t[[2]], make_fgls(errors, n)), numeric(1))
      rows[[length(rows) + 1L]] <- summarise(
        t[[1]], "estimated", "fitWLS(), exponential variance correctly specified",
        errors, n, p
      )
    }
  }
}
for (n in c(150L, 600L)) {
  for (t in ESTIMATED) {
    set.seed(20261010L)
    p <- vapply(seq_len(5L * n_mc),
                function(i) pval(t[[2]], make_fgls("normal", n, sd_power)), numeric(1))
    rows[[length(rows) + 1L]] <- summarise(
      t[[1]], "misspecified", "fitWLS(), exponential fitted to sigma^2 = x^3",
      "normal", n, p
    )
  }
}

check_repetition()
index <- seq_len(nrow(boot_scenarios))
boot_rows <- if (n_cores > 1L) {
  parallel::mclapply(index, run_boot_scenario, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(index, run_boot_scenario)
}

# A worker that was killed returns NULL and one that failed returns the error,
# and rbind() would drop the first without a word.
complete <- vapply(boot_rows, function(r) is.data.frame(r) && nrow(r) == 1L, logical(1))
if (!all(complete)) {
  stop("No result for block 4 scenario(s) ", paste(which(!complete), collapse = ", "), ".")
}

out <- do.call(rbind, c(rows, boot_rows))
path <- file.path("inst", "validation", "weighted-fits-size.csv")
if (!dir.exists(dirname(path))) path <- "weighted-fits-size.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
