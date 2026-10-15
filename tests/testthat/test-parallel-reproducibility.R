# Seeded resampling with parallel = TRUE.
#
# Before 0.15.0 the replicates computed in worker processes drew from random
# number generators that set.seed() in the session did not reach, so a seeded
# call with parallel = TRUE returned a different p-value each time it ran, and
# performWhiteTestBootstrap(parallel = TRUE) stopped on Windows, which cannot
# fork, whenever it asked for more than one worker. Since 0.15.0 replicate i
# draws from stream i of a L'Ecuyer-CMRG generator seeded by one draw from the
# session's generator: the result depends on the seed alone, not on the number
# of workers or on whether they are forked or run on a socket cluster. A
# serial call draws from the session's generator, as before, so the parallel
# and the serial result differ; each is a bootstrap of the same data, and the
# last test checks that their replicates have the same distribution.
#
# On the socket path the workers load the installed heteroTests, so these
# tests check the installed code there: under R CMD check that is this
# version.

parallel_data <- function() {
  set.seed(31)
  n <- 80
  d <- data.frame(x = runif(n, 1, 4), z = rnorm(n))
  d$y <- 1 + d$x + 0.5 * d$z + rnorm(n, sd = d$x)
  list(d = d, m = lm(y ~ x + z, data = d))
}

test_that("a seeded parallel bootstrap repeats itself with any number of workers", {
  skip_on_cran()
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(cores, parallel = TRUE) {
    set.seed(2026)
    bootstrapTestStatistic(performKoenkerTest, s$m, s$d, B = 39, parallel = parallel,
                           n_cores = cores, progress = FALSE)
  }
  first <- run(2)
  second <- run(2)
  one_worker <- run(1)
  expect_true(all(is.finite(first$replicates)))
  expect_identical(first$replicates, second$replicates)
  expect_identical(first$p_value, second$p_value)
  expect_identical(first$replicates, one_worker$replicates)

  # The serial call draws from the session's generator, as before 0.15.0.
  serial <- run(NULL, parallel = FALSE)
  expect_true(all(is.finite(serial$replicates)))
  expect_false(identical(serial$replicates, first$replicates))
})

test_that("the White bootstrap is reproducible in parallel, and Windows can run it", {
  skip_on_cran()
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(cores) {
    set.seed(7)
    performWhiteTestBootstrap(s$m, s$d, B = 39, parallel = TRUE, n_cores = cores)
  }
  first <- run(2)
  expect_true(all(is.finite(first$boot_statistics)))
  expect_identical(first$boot_statistics, run(2)$boot_statistics)
  expect_identical(first$p.value, run(1)$p.value)
  expect_error(
    performWhiteTestBootstrap(s$m, s$d, B = 9, parallel = TRUE, n_cores = 0),
    "`n_cores` must be NULL or a positive integer"
  )
})

test_that("runHeteroTestsParallel() is reproducible with a resampling test", {
  skip_on_cran()
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function() {
    set.seed(99)
    suppressWarnings(
      runHeteroTestsParallel(s$m, s$d, tests = c("koenker", "wild_bootstrap"), n_cores = 2)
    )
  }
  first <- run()
  second <- run()
  expect_true(is.finite(first$wild_bootstrap$p.value))
  expect_identical(first$wild_bootstrap$bootstrap$replicates,
                   second$wild_bootstrap$bootstrap$replicates)
  expect_identical(first$koenker$statistic, performKoenkerTest(s$m, s$d)$statistic)
})

test_that("the streams take one draw from the session's generator and leave its kind", {
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  kind <- RNGkind()
  set.seed(5)
  streams <- rtask_streams(3)
  after <- stats::runif(1)
  expect_identical(RNGkind(), kind)
  set.seed(5)
  invisible(sample.int(.Machine$integer.max, 1L))
  expect_identical(stats::runif(1), after)
  # The streams are L'Ecuyer-CMRG states, each the next stream of the one
  # before, and running a task on one leaves the session's generator alone.
  expect_identical(streams[[2]], parallel::nextRNGStream(streams[[1]]))
  set.seed(5)
  state <- .Random.seed
  draw <- rrun_on_stream(streams[[1]], function(i) stats::runif(2), 1L)
  expect_identical(.Random.seed, state)
  expect_identical(rrun_on_stream(streams[[1]], function(i) stats::runif(2), 1L), draw)
})

test_that("parallel and serial replicates have the same distribution", {
  skip_on_cran()
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  B <- 399
  set.seed(12)
  serial <- bootstrapTestStatistic(performKoenkerTest, s$m, s$d, B = B, progress = FALSE)
  set.seed(12)
  parallel <- bootstrapTestStatistic(performKoenkerTest, s$m, s$d, B = B, parallel = TRUE,
                                     n_cores = 2, progress = FALSE)
  # Two sets of draws from the same bootstrap distribution: a two-sample
  # Kolmogorov-Smirnov test should not reject, and the two p-values estimate
  # the same probability, so they differ by less than four standard errors of
  # their difference.
  expect_gt(suppressWarnings(stats::ks.test(serial$replicates, parallel$replicates))$p.value,
            0.001)
  p <- (serial$p_value + parallel$p_value) / 2
  expect_lt(abs(serial$p_value - parallel$p_value), 4 * sqrt(2 * p * (1 - p) / B) + 2 / B)
})
