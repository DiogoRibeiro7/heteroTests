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
# runHeteroTestsParallel() draws test i from stream i too when it runs in the
# session, with n_cores = 1, so its value does not depend on n_cores either.
#
# On the socket path the workers load the installed heteroTests, so these
# tests check the installed code there: under R CMD check that is this
# version. In a development run, such as devtools::test(), the installed copy
# may be other code under the same version number, a release or an older
# build of the branch, and a test would check it without saying so. A test
# that runs on a socket cluster is skipped then, unless the installed copy
# holds the parallel code of the source: the functions below, compared
# without their source references. Forked workers run the code of the
# session.
parallel_code <- c(
  "rtask_streams", "rrun_on_stream", "rparallel_tasks", "runHeteroTestsParallel",
  "bootstrapTestStatistic", "performWhiteTestBootstrap", "performWildBootstrapTest",
  # What a replicate of a fitWLS() fit calls when it estimates the weights
  # again, in the worker.
  "rwls_refit_replicate", "rvariance_function_on", "rweighted_refit", "rfgls_fit"
)

skip_unless_workers_load_this_version <- function(forked = FALSE) {
  if ((forked && .Platform$OS.type == "unix") || testthat::is_checking()) {
    return(invisible(TRUE))
  }
  installed <- find.package("heteroTests", lib.loc = .libPaths(), quiet = TRUE)
  if (length(installed) == 0L) {
    skip("the socket workers would find no installed heteroTests")
  }
  code <- new.env()
  lazyLoad(file.path(installed[[1L]], "R", "heteroTests"), envir = code)
  source_code <- asNamespace("heteroTests")
  text <- function(name, env) deparse(utils::removeSource(get(name, envir = env)))
  differs <- Filter(function(name) {
    !exists(name, envir = code, inherits = FALSE) ||
      !identical(text(name, code), text(name, source_code))
  }, parallel_code)
  if (length(differs) > 0L) {
    skip(sprintf(
      "the socket workers would load the heteroTests in %s, whose %s differs from the source",
      installed[[1L]], differs[[1L]]
    ))
  }
  invisible(TRUE)
}

parallel_data <- function() {
  set.seed(31)
  n <- 80
  d <- data.frame(x = runif(n, 1, 4), z = rnorm(n))
  d$y <- 1 + d$x + 0.5 * d$z + rnorm(n, sd = d$x)
  list(d = d, m = lm(y ~ x + z, data = d))
}

test_that("a seeded parallel bootstrap repeats itself with any number of workers", {
  skip_on_cran()
  skip_unless_workers_load_this_version(forked = TRUE)
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

test_that("a parallel bootstrap of a fitWLS() fit estimates the weights again on each stream", {
  skip_on_cran()
  skip_unless_workers_load_this_version(forked = TRUE)
  s <- parallel_data()
  fit <- fitWLS(s$m)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(cores, ...) {
    set.seed(2027)
    suppressWarnings(bootstrapTestStatistic(performKoenkerTest, fit, s$d, B = 19,
                                            parallel = TRUE, n_cores = cores,
                                            progress = FALSE, ...))
  }
  first <- run(2)
  expect_true(all(is.finite(first$replicates)))
  expect_identical(first$replicates, run(2)$replicates)
  expect_identical(first$replicates, run(1)$replicates)
  expect_identical(first$replicates, run(2, reestimate_weights = TRUE)$replicates)
  expect_false(isTRUE(all.equal(first$replicates,
                                run(2, reestimate_weights = FALSE)$replicates)))

  set.seed(2028)
  white <- suppressWarnings(performWhiteTestBootstrap(fit, s$d, B = 19, parallel = TRUE,
                                                      n_cores = 2))
  set.seed(2028)
  again <- suppressWarnings(performWhiteTestBootstrap(fit, s$d, B = 19, parallel = TRUE,
                                                      n_cores = 1))
  expect_true(all(is.finite(white$boot_statistics)))
  expect_identical(white$boot_statistics, again$boot_statistics)
})

test_that("the White bootstrap is reproducible in parallel, and Windows can run it", {
  skip_on_cran()
  skip_unless_workers_load_this_version(forked = TRUE)
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
  skip_unless_workers_load_this_version()
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

test_that("runHeteroTestsParallel() gives the same value whatever n_cores", {
  skip_on_cran()
  skip_unless_workers_load_this_version()
  s <- parallel_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(tests, cores) {
    set.seed(99)
    suppressWarnings(runHeteroTestsParallel(s$m, s$d, tests = tests, n_cores = cores))
  }
  # With n_cores = 1 the tests run in the session, on the streams the workers
  # use. Before, they drew from the session's generator there, and the wild
  # bootstrap p-value moved with n_cores.
  two <- run(c("koenker", "wild_bootstrap"), 2)
  one <- run(c("koenker", "wild_bootstrap"), 1)
  expect_identical(one$wild_bootstrap$bootstrap$replicates,
                   two$wild_bootstrap$bootstrap$replicates)
  expect_identical(one$wild_bootstrap$p.value, two$wild_bootstrap$p.value)
  # One test makes the default one worker; it takes stream 1, as the first of
  # two tests does on two workers.
  first_of_two <- run(c("wild_bootstrap", "koenker"), 2)
  alone <- run("wild_bootstrap", NULL)
  expect_identical(alone$wild_bootstrap$bootstrap$replicates,
                   first_of_two$wild_bootstrap$bootstrap$replicates)
})

test_that("runHeteroTestsParallel() runs a model created at top level", {
  skip_on_cran()
  skip_unless_workers_load_this_version()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Called at top level, the task reached the socket workers with `model`
  # still a promise to evaluate the name of the model in the global
  # environment, and the workers, which have no such object, stopped with
  # "object 'm1' not found". The arguments are forced in the session since.
  assign(".ht_top_level_model", lm(mpg ~ wt + qsec, data = mtcars), envir = globalenv())
  on.exit(rm(".ht_top_level_model", envir = globalenv()), add = TRUE)
  set.seed(99)
  top <- suppressWarnings(evalq(
    runHeteroTestsParallel(.ht_top_level_model, mtcars, tests = c("koenker", "wild_bootstrap"),
                           n_cores = 2),
    globalenv()
  ))
  expect_true(is.finite(top$wild_bootstrap$p.value))
  expect_identical(
    top$koenker$statistic,
    performKoenkerTest(get(".ht_top_level_model", envir = globalenv()), mtcars)$statistic
  )
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
  skip_unless_workers_load_this_version(forked = TRUE)
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
