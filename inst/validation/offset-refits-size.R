# The resampling tests on a fit with an offset ---------------------------------
#
# Run from the package root:
#   Rscript inst/validation/offset-refits-size.R
#
# N_MC sets the replications of the corrected tests and N_MC_BEFORE those of
# the tests as they were before the correction; both can be lowered for
# exploratory runs, e.g. N_MC=100 N_MC_BEFORE=100 Rscript ... . The evidence
# uses N_MC=5000, where the 99% interval around a true 5% is [0.042, 0.058],
# and N_MC_BEFORE=2000, with B = 199 bootstrap replications, and one cell
# again with B = 999 (see the scenarios). The cells before the correction
# describe code that is gone, and are not evidence for the release gate.
# SCENARIOS=9, for example, runs the ninth scenario alone and replaces its
# row in the CSV.
#
# Each scenario is cut into chunks of 500 replications, 125 for the one with
# B = 999, each with its own seed, so that one scenario can run on several
# cores and the results still do not depend on their number. MC_CORES runs the chunks in parallel,
# through parallel::mclapply() where processes can be forked and a socket
# cluster on Windows.
#
# lm(..., offset = o) keeps o in the model frame and not in the formula.
# performWildBootstrapTest(), performWhiteTestBootstrap() and
# rbootstrap_test_statistic() regenerate the response, or the rows, and refit
# the model. Until they were corrected, the refit of an unweighted fit came
# from the formula alone and dropped the offset; the wild bootstrap stopped
# with an error instead, because it evaluated the call of the fit again in
# its model frame, where the offset is "(offset)"; and
# rbootstrap_test_statistic() took the fitted values by position, so data
# holding the fitted rows in another order regenerated each row from another
# row's fitted value.
#
# The design: y = 1 + 2 x + 0.5 z + o + e with o = 0.5 x^2, x and z uniform on
# (1, 5), e standard normal and 100 observations, fitted as
# lm(y ~ x + z, data, offset = o). The variance is constant, so every
# rejection rate below is a size. The offset is not in the span of the
# regressors, so a refit that drops it leaves the part of 0.5 x^2 that x and z
# do not explain in the residuals of every replicate.
#
# Block 1, the corrected tests. The three tests on that fit, with B = 199;
# rbootstrap_test_statistic() with performKoenkerTest() as its test function,
# once on the data as generated and once on the same rows in a random order.
# The fifth row fits log(x) in place of x, y = 1 + 2 log(x) + 0.5 z + o + e,
# because the wild bootstrap also failed on a transformed regressor.
# rbootstrap_test_statistic() on the data as generated runs a second time
# with B = 999, its default: with B = 199 a test whose level is exact gives
# p < 0.05 0.045 of the time, and with B = 999, 0.049.
#
# Block 2, as the tests computed it before. The refits of the White bootstrap
# and of rbootstrap_test_statistic() written out as the package made them,
# without the offset and, for the data in another order, with the fitted
# values taken by position. check_repetition() asserts first that, with the
# offset kept and the rows matched by name, the written-out resampling
# reproduces the package's replicates, so that the dropped offset and the
# positions are the only differences. The wild bootstrap is not in this
# block: it stopped with an error on every one of these fits.
#
# Each replication starts by clearing the package's validation cache. The
# tests store the result of every input validation, keyed by the model and
# the data, and each of the slowest scenarios, 5000 replications of 200 calls
# of a test, validates a million new models; the one with B = 999, five
# million. Clearing the cache only saves repeating a validation, so it
# changes no value. On a machine shared with other work, the first eight
# scenarios ran for two hours with MC_CORES = 5, and the ninth for three and a
# half hours with MC_CORES = 8.

is_source_checkout <- file.exists("DESCRIPTION") &&
  any(grepl(
    "^Package:[[:space:]]*heteroTests[[:space:]]*$",
    readLines("DESCRIPTION", warn = FALSE),
    fixed = FALSE
  ))

load_package <- function() {
  if (requireNamespace("pkgload", quietly = TRUE) && is_source_checkout) {
    suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(heteroTests))
  }
  invisible(ht_set_log_level("SILENT"))
}
load_package()

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

alpha <- 0.05
n_mc <- get_count("N_MC", "5000", 20L)
n_mc_before <- get_count("N_MC_BEFORE", "2000", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
n_boot <- 199L
n_obs <- 100L
chunk_size <- 500L
seed <- 20261011L

# --- designs --------------------------------------------------------------------

make_offset <- function(n = n_obs, transform = "none") {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  o <- 0.5 * x^2
  if (transform == "log") {
    d <- data.frame(y = 1 + 2 * log(x) + 0.5 * z + o + rnorm(n), x = x, z = z, o = o)
    model <- lm(y ~ log(x) + z, data = d, offset = o)
  } else {
    d <- data.frame(y = 1 + 2 * x + 0.5 * z + o + rnorm(n), x = x, z = z, o = o)
    model <- lm(y ~ x + z, data = d, offset = o)
  }
  list(model = model, data = d)
}

# The same rows in a random order, with their row names.
shuffle_rows <- function(o) {
  o$data <- o$data[sample.int(nrow(o$data)), , drop = FALSE]
  o
}

# --- the resampling written out -------------------------------------------------

# n R^2 of the squared residuals on x and z (Koenker's statistic) or on x, z,
# their squares and their product (White's). check_repetition() asserts that
# they equal what performKoenkerTest() and performWhiteTest() return.
koenker_statistic <- function(e, data) {
  data$.e2 <- e^2
  nrow(data) * summary(lm(.e2 ~ x + z, data = data))$r.squared
}
white_statistic <- function(e, data) {
  data$.e2 <- e^2
  nrow(data) * summary(lm(.e2 ~ x + z + I(x^2) + I(z^2) + I(x * z), data = data))$r.squared
}

# The null-imposed resampling of rbootstrap_test_statistic() and the
# resampling of performWhiteTestBootstrap() on an unweighted fit. The first
# draws from the residuals divided by sqrt(1 - h) and centred, the second
# from the residuals as they are. keep_offset = FALSE refits from the formula
# alone and by_name = FALSE takes the fitted values by position, which is what
# the package did before the correction; with both TRUE this is what it does
# now, and check_repetition() asserts that the two then agree.
resample_by_hand <- function(obj, scheme, keep_offset, by_name, B = n_boot) {
  model <- obj$model
  d <- obj$data
  n <- nrow(d)
  fitted_values <- stats::fitted(model)
  if (by_name) {
    fitted_values <- fitted_values[rownames(d)]
  }
  pool <- if (scheme == "null") {
    r <- stats::residuals(model) / sqrt(pmax(1 - stats::hatvalues(model), .Machine$double.eps))
    r - mean(r)
  } else {
    stats::residuals(model)
  }
  statistic <- if (scheme == "null") koenker_statistic else white_statistic
  # The observed statistic is computed on the rows in the order of the fit.
  fit_data <- d[names(stats::residuals(model)), , drop = FALSE]
  observed <- statistic(stats::residuals(model), fit_data)
  replicates <- vapply(seq_len(B), function(b) {
    draw <- if (scheme == "null") sample(pool, n, replace = TRUE) else sample(pool, replace = TRUE)
    db <- d
    db$y <- fitted_values + draw
    tryCatch({
      # The offset is the column o of the data.
      fit <- if (keep_offset) {
        lm(y ~ x + z, data = db, offset = o)
      } else {
        lm(y ~ x + z, data = db)
      }
      statistic(stats::residuals(fit), db)
    }, error = function(err) NA_real_)
  }, numeric(1))
  ok <- is.finite(replicates)
  list(
    p.value = (1 + sum(replicates[ok] >= observed)) / (sum(ok) + 1),
    observed = observed,
    replicates = replicates
  )
}

check_repetition <- function() {
  same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-8))
  set.seed(seed - 1L)
  o <- make_offset()
  shuffled <- shuffle_rows(o)
  agree <- c(
    same(resample_by_hand(o, "null", TRUE, TRUE, B = 1L)$observed,
         unname(performKoenkerTest(o$model, o$data)$statistic)),
    same(resample_by_hand(o, "white", TRUE, TRUE, B = 1L)$observed,
         unname(suppressWarnings(performWhiteTest(o$model, o$data))$statistic))
  )
  if (!all(agree)) {
    stop("The statistics written out here differ from the package's.")
  }
  cases <- list(
    list(o, "white", function(x) {
      r <- suppressWarnings(performWhiteTestBootstrap(x$model, x$data, B = 25L))
      list(p.value = r$p.value, replicates = r$boot_statistics)
    }),
    list(o, "null", function(x) {
      r <- suppressWarnings(bootstrapTestStatistic(
        performKoenkerTest, x$model, x$data, B = 25L, progress = FALSE
      ))
      list(p.value = r$p_value, replicates = r$replicates)
    }),
    list(shuffled, "null", function(x) {
      r <- suppressWarnings(bootstrapTestStatistic(
        performKoenkerTest, x$model, x$data, B = 25L, progress = FALSE
      ))
      list(p.value = r$p_value, replicates = r$replicates)
    })
  )
  for (k in seq_along(cases)) {
    case <- cases[[k]]
    set.seed(seed - 2L)
    ours <- resample_by_hand(case[[1]], case[[2]], keep_offset = TRUE, by_name = TRUE, B = 25L)
    set.seed(seed - 2L)
    theirs <- case[[3]](case[[1]])
    if (!same(ours$replicates, theirs$replicates) || ours$p.value != theirs$p.value) {
      stop("The written-out resampling does not reproduce the package, case ", k, ".")
    }
  }
  message("the statistics and the written-out resampling reproduce the package")
}

# --- scenarios ------------------------------------------------------------------

scenarios <- data.frame(
  test = c(
    "performWildBootstrapTest", "performWhiteTestBootstrap",
    "rbootstrap_test_statistic", "rbootstrap_test_statistic",
    "performWildBootstrapTest",
    "performWhiteTestBootstrap", "rbootstrap_test_statistic", "rbootstrap_test_statistic"
  ),
  version = rep(c("corrected", "before"), c(5L, 3L)),
  terms = c(rep("x + z", 4L), "log(x) + z", rep("x + z", 3L)),
  rows = c("fitted order", "fitted order", "fitted order", "another order",
           "fitted order", "fitted order", "fitted order", "another order"),
  B = n_boot,
  chunk = chunk_size,
  stringsAsFactors = FALSE
)
# Added after the others, so that their seeds stay as they were: the third
# scenario again with B = 999, the default of rbootstrap_test_statistic().
# With B = 199 a test whose level is exact gives p < 0.05 with probability
# 9 / 200 = 0.045, and with B = 999 with probability 49 / 1000 = 0.049. A
# replication costs five times as much, so the chunks are smaller.
scenarios <- rbind(scenarios, data.frame(
  test = "rbootstrap_test_statistic", version = "corrected", terms = "x + z",
  rows = "fitted order", B = 999L, chunk = 125L, stringsAsFactors = FALSE
))

p_value <- function(s, o) {
  if (s$version == "before") {
    scheme <- if (s$test == "performWhiteTestBootstrap") "white" else "null"
    return(resample_by_hand(o, scheme, keep_offset = FALSE, by_name = FALSE, B = s$B))
  }
  switch(s$test,
    performWildBootstrapTest = performWildBootstrapTest(o$model, o$data, B = s$B,
                                                        progress = FALSE),
    performWhiteTestBootstrap = performWhiteTestBootstrap(o$model, o$data, B = s$B),
    rbootstrap_test_statistic = {
      r <- bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = s$B,
                                     progress = FALSE)
      list(p.value = r$p_value)
    }
  )
}

pval <- function(fn) {
  r <- tryCatch(suppressWarnings(suppressMessages(fn())), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

# SCENARIOS, a comma-separated list of scenario numbers, runs those alone and
# replaces their rows in the CSV, keeping the others. A scenario gives the
# same row whether it runs alone or with the rest.
selected <- trimws(strsplit(Sys.getenv("SCENARIOS", unset = ""), ",")[[1]])
selected <- if (length(selected) == 0L || identical(selected, "")) {
  seq_len(nrow(scenarios))
} else {
  as.integer(selected)
}
if (anyNA(selected) || any(!selected %in% seq_len(nrow(scenarios)))) {
  stop("SCENARIOS must name scenarios between 1 and ", nrow(scenarios), ".")
}

# One chunk of one scenario: its own seed, so that the p-values do not depend
# on which worker draws them or in what order.
units <- do.call(rbind, lapply(selected, function(i) {
  total <- if (scenarios$version[i] == "corrected") n_mc else n_mc_before
  size <- scenarios$chunk[i]
  starts <- seq(1L, total, by = size)
  data.frame(scenario = i, chunk = seq_along(starts),
             size = pmin(size, total - starts + 1L))
}))
# The slowest chunks go first, so that the workers finish together. The order
# changes no value.
cost <- c(performWildBootstrapTest = 0.3, performWhiteTestBootstrap = 1.3,
          rbootstrap_test_statistic = 1.5)
units$cost <- units$size * scenarios$B[units$scenario] / n_boot *
  ifelse(scenarios$version[units$scenario] == "corrected",
         cost[scenarios$test[units$scenario]], 0.4)
units <- units[order(-units$cost, units$scenario, units$chunk), ]

run_chunk <- function(k) {
  u <- units[k, ]
  s <- scenarios[u$scenario, ]
  set.seed(seed + 1000L * u$scenario + u$chunk)
  vapply(seq_len(u$size), function(rep) {
    heteroTests:::clearValidationCache()
    o <- make_offset(transform = if (s$terms == "log(x) + z") "log" else "none")
    if (s$rows == "another order") {
      o <- shuffle_rows(o)
    }
    pval(function() p_value(s, o))
  }, numeric(1))
}

summarise_scenario <- function(i, p) {
  s <- scenarios[i, ]
  ok <- p[!is.na(p)]
  rate <- if (length(ok)) mean(ok < alpha) else NA_real_
  message(sprintf("%-27s %-9s %-10s %-13s B=%3d n_mc=%4d reject %s%s", s$test, s$version,
                  s$terms, s$rows, s$B, length(p),
                  if (is.na(rate)) "  .  " else sprintf("%.4f", rate),
                  if (length(ok) < length(p)) {
                    sprintf("   [%d of %d calls failed]", length(p) - length(ok), length(p))
                  } else {
                    ""
                  }))
  data.frame(
    test = s$test,
    version = s$version,
    offset = "argument",
    terms = s$terms,
    rows = s$rows,
    errors = "normal",
    n = n_obs,
    B = s$B,
    replications = length(p),
    rejection_rate = rate,
    mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
    effective = length(ok),
    failures = length(p) - length(ok),
    stringsAsFactors = FALSE
  )
}

# --- run ------------------------------------------------------------------------

check_repetition()
index <- seq_len(nrow(units))
results <- if (n_cores == 1L) {
  lapply(index, run_chunk)
} else if (.Platform$OS.type != "windows") {
  parallel::mclapply(index, run_chunk, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  # mclapply() cannot fork on Windows. Each worker of a socket cluster loads
  # the package itself, from the same directory, and receives the functions
  # and settings defined above. The cluster is started afresh for every
  # n_cores chunks, as mclapply() forks afresh for every chunk, so that a
  # worker does not carry what earlier chunks left in memory: before the
  # validation cache was bounded it kept every key as a symbol, which R never
  # frees, and a worker grew by about a kilobyte per call of a test and slowed
  # down as it grew.
  out <- vector("list", length(index))
  for (batch in split(index, ceiling(seq_along(index) / n_cores))) {
    cl <- parallel::makeCluster(min(n_cores, length(batch)))
    parallel::clusterCall(cl, setwd, getwd())
    parallel::clusterExport(cl, ls(globalenv()), envir = globalenv())
    parallel::clusterCall(cl, load_package)
    done <- tryCatch(parallel::clusterApplyLB(cl, batch, run_chunk), error = function(e) e)
    parallel::stopCluster(cl)
    if (inherits(done, "error")) {
      break
    }
    out[batch] <- done
  }
  out
}

# A worker that was killed returns NULL and one that failed returns the error,
# and c() would drop the first without a word.
complete <- length(results) == length(index) &&
  all(vapply(seq_along(index), function(k) {
    is.numeric(results[[k]]) && length(results[[k]]) == units$size[k]
  }, logical(1)))
if (!complete) {
  stop("Not every chunk returned a result.")
}

out <- do.call(rbind, lapply(selected, function(i) {
  mine <- which(units$scenario == i)
  mine <- mine[order(units$chunk[mine])]
  summarise_scenario(i, unlist(results[mine], use.names = FALSE))
}))
path <- file.path("inst", "validation", "offset-refits-size.csv")
if (!dir.exists(dirname(path))) path <- "offset-refits-size.csv"
if (length(selected) < nrow(scenarios) && file.exists(path)) {
  # The rows of the scenarios that did not run are kept, in the order of the
  # scenarios.
  key <- function(x) paste(x$test, x$version, x$terms, x$rows, x$B, sep = "|")
  kept <- utils::read.csv(path, stringsAsFactors = FALSE)
  kept <- kept[!key(kept) %in% key(out), , drop = FALSE]
  out <- rbind(kept, out)
  out <- out[order(match(key(out), key(scenarios))), , drop = FALSE]
}
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
