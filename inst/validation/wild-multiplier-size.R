# The multiplier of the wild bootstrap: Rademacher against Mammen ----------------
#
# Run from the package root:
#   Rscript inst/validation/wild-multiplier-size.R
#
# N_MC sets the replications of the size cells and N_MC_POWER those of the
# power cells, N_BOOT the bootstrap samples of each test and CHUNK_SIZE the
# replications of a chunk; e.g. N_MC=40 N_MC_POWER=20 N_BOOT=99 Rscript ...
# for an exploratory run. The evidence uses N_MC=5000, N_MC_POWER=1000,
# N_BOOT=999 and CHUNK_SIZE=500, the defaults.
#
# Each cell is cut into chunks of CHUNK_SIZE replications, each with its own
# seed, seed + 1000 x cell + chunk, so the results do not depend on how many
# processes ran them. MC_CORES runs the chunks in parallel through
# parallel::mclapply() where processes can be forked. Where they cannot
# (Windows), the chunks can run as separate processes: UNITS, for example
# "1:10", runs those chunks, numbered as in the `units` table below, and saves
# their p-values to the .rds file PART instead of writing the CSV; MERGE, a
# directory, reads every .rds file in it, checks that together they hold every
# chunk once, and writes the CSV, which is then the one a single process
# writes. CSV names the file written, by default
# inst/validation/wild-multiplier-size.csv.
#
# performWildBootstrapTest() resamples the leverage-standardised, centred
# residuals with replacement and multiplies each by a draw v from the
# distribution named by `distribution`: Rademacher, v = -1 or 1 with equal
# probability, or Mammen's two-point distribution, v = (1 - sqrt(5)) / 2 with
# probability (sqrt(5) + 1) / (2 sqrt(5)) and (1 + sqrt(5)) / 2 otherwise.
# Both have E v = 0 and E v^2 = 1. Rademacher has E v^3 = 0 and E v^4 = 1, so
# its bootstrap errors are symmetric and their squares are the squares of the
# resampled residuals; Mammen's has E v^3 = 1 and E v^4 = 2, so its bootstrap
# errors keep the skewness of the residuals and their squares are more
# dispersed than those of the residuals. Which matters more for the
# Breusch-Pagan statistic the test computes, n R^2 of the squared residuals on
# the regressors, is the question.
#
# The design is the cross-sectional one of pass-a-size-power.R,
# y = 1 + 2 x1 + 0.5 x2 + sigma_i e_i with x1 uniform on (1, 5) and x2
# standard normal, fitted as lm(y ~ x1 + x2), at 30, 50 and 100 observations.
# The errors have mean 0 and variance 1: Gaussian, t with 5 degrees of
# freedom, and a centred exponential, rexp(n) - 1, skewness 2. The exponential
# is the skewed distribution because skewness is what separates the two
# multipliers, and in glejser-skewed-errors.R it is the error under which the
# corrected Glejser statistic was outside the release gate at 50 observations;
# the lognormal of that study adds tails heavy enough to test the statistic
# rather than the multiplier.
#
# Block 1, size: sigma_i = 1. Each replication fits one sample and tests it
# with both multipliers, so that the two rates are paired and their
# difference is measured more precisely than either rate.
#
# Block 2, power: sigma_i^2 = exp(0.4 x1), the alternative power_exp of
# pass-a-size-power.R, with Gaussian errors, at N_MC_POWER replications. It
# is recorded so that a tie on size can be broken by power if wanted.
#
# With B = 999 the p-value (1 + #{T* >= T}) / (B + 1) is a multiple of 1/1000,
# and a test whose level is exact gives p < 0.05 with probability
# 49 / 1000 = 0.049. A size cell is judged against the release-gate interval,
# the 99% interval around 0.05, and against the 99% interval around 0.049.
# The decision rule was written here before the full run started; the script
# was first committed with its results. The default should be the multiplier
# whose size is inside the interval in more of the nine size cells, under
# each of the two intervals, provided that in at least one cell where only
# it is inside, the paired difference of the two rates exceeds
# 2.576 of its Monte Carlo standard errors. Otherwise neither is better within
# Monte Carlo error, and the default, Rademacher, stays.
#
# Each replication starts by clearing the package's validation cache, which
# changes no value and draws no random number.

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
n_mc <- get_count("N_MC", "5000", 20L)
n_mc_power <- get_count("N_MC_POWER", "1000", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
n_boot <- get_count("N_BOOT", "999", 19L)
chunk_size <- get_count("CHUNK_SIZE", "500", 1L)
seed <- 20261015L
multipliers <- c("rademacher", "mammen")
invisible(ht_set_log_level("SILENT"))

# --- errors and design ----------------------------------------------------------

# Mean 0 and variance 1.
draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    exponential = rexp(n) - 1,
    stop("unknown error distribution")
  )
}

make_ols <- function(n, errors, gamma = 0) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1, x2 = x2,
    y = 1 + 2 * x1 + 0.5 * x2 + exp(0.5 * gamma * x1) * draw_errors(n, errors)
  )
  list(model = lm(y ~ x1 + x2, data = d), data = d)
}

# --- scenarios ------------------------------------------------------------------

scenarios <- rbind(
  expand.grid(
    block = "size", errors = c("normal", "t5", "exponential"), n = c(30L, 50L, 100L),
    gamma = 0, replications = n_mc, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  ),
  expand.grid(
    block = "power", errors = "normal", n = c(30L, 50L, 100L),
    gamma = 0.4, replications = n_mc_power, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
)

# One row per chunk: its cell, its number within the cell and its replications.
units <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  total <- scenarios$replications[i]
  starts <- seq(1L, total, by = chunk_size)
  data.frame(scenario = i, chunk = seq_along(starts),
             size = pmin(chunk_size, total - starts + 1L))
}))
# The seed of a chunk, seed + 1000 x cell + chunk, is its own only while a cell
# has fewer than 1000 chunks.
stopifnot(max(units$chunk) < 1000L)

# --- run ------------------------------------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

# The p-values of one chunk, one row per multiplier and one column per
# replication. Both multipliers test the same sample.
run_unit <- function(k) {
  u <- units[k, ]
  s <- scenarios[u$scenario, ]
  set.seed(seed + 1000L * u$scenario + u$chunk)
  p <- vapply(seq_len(u$size), function(rep) {
    heteroTests:::clearValidationCache()
    o <- make_ols(s$n, s$errors, s$gamma)
    vapply(multipliers, function(m) {
      pval(performWildBootstrapTest(
        o$model, o$data, B = n_boot, distribution = m, progress = FALSE
      ))
    }, numeric(1))
  }, numeric(length(multipliers)))
  matrix(p, nrow = length(multipliers), dimnames = list(multipliers, NULL))
}

summarise_scenario <- function(i, p) {
  s <- scenarios[i, ]
  # The replications in which both tests returned a p-value, for the paired
  # comparison: how often each rejected where the other did not.
  both <- colSums(is.na(p)) == 0L
  reject <- p[, both, drop = FALSE] < alpha
  rows <- lapply(seq_along(multipliers), function(j) {
    ok <- p[j, ][!is.na(p[j, ])]
    rate <- if (length(ok)) mean(ok < alpha) else NA_real_
    data.frame(
      scenario = i,
      block = s$block,
      multiplier = multipliers[j],
      errors = s$errors,
      n = s$n,
      gamma = s$gamma,
      bootstrap = n_boot,
      replications = s$replications,
      rejection_rate = rate,
      mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
      effective = length(ok),
      failures = s$replications - length(ok),
      paired = sum(both),
      rejects_alone = sum(reject[j, ] & !reject[-j, ]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-5s %-11s n=%3d gamma=%.1f  rademacher %.4f  mammen %.4f",
    s$block, s$errors, s$n, s$gamma, out$rejection_rate[1], out$rejection_rate[2]
  ))
  out
}

# A worker that was killed returns NULL and one that failed returns the error,
# and c() would drop the first without a word.
check_units <- function(results, index) {
  complete <- length(results) == length(index) &&
    all(vapply(seq_along(index), function(j) {
      r <- results[[j]]
      is.matrix(r) && identical(dim(r), c(length(multipliers), units$size[index[j]]))
    }, logical(1)))
  if (!complete) {
    stop("Not every chunk returned a result.")
  }
}

chosen <- Sys.getenv("UNITS", unset = "")
merge_dir <- Sys.getenv("MERGE", unset = "")
if (nzchar(merge_dir)) {
  parts <- lapply(list.files(merge_dir, pattern = "\\.rds$", full.names = TRUE), readRDS)
  index <- unlist(lapply(parts, `[[`, "index"))
  results <- unlist(lapply(parts, `[[`, "p"), recursive = FALSE)
  if (anyDuplicated(index) || !setequal(index, seq_len(nrow(units)))) {
    stop("The parts in ", merge_dir, " do not hold every chunk exactly once.")
  }
  if (!all(vapply(parts, function(x) identical(x$settings, list(n_mc, n_mc_power, n_boot,
                                                                 chunk_size, seed)),
                  logical(1)))) {
    stop("The parts in ", merge_dir, " were run with other settings.")
  }
  results <- results[order(index)]
  index <- sort(index)
} else {
  index <- seq_len(nrow(units))
  if (nzchar(chosen)) {
    index <- intersect(index, eval(parse(text = chosen)))
  }
  results <- if (n_cores > 1L) {
    parallel::mclapply(index, run_unit, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    lapply(index, run_unit)
  }
}
check_units(results, index)

if (nzchar(chosen) && !nzchar(merge_dir)) {
  part <- Sys.getenv("PART", unset = "")
  if (!nzchar(part)) {
    stop("UNITS needs PART, the .rds file to save the p-values to.")
  }
  saveRDS(list(index = index, p = results,
               settings = list(n_mc, n_mc_power, n_boot, chunk_size, seed)), part)
  message("saved chunks ", paste(index, collapse = ", "), " to ", part)
} else {
  out <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
    mine <- which(units$scenario[index] == i)
    mine <- mine[order(units$chunk[index[mine]])]
    summarise_scenario(i, do.call(cbind, results[mine]))
  }))
  path <- Sys.getenv("CSV", unset = file.path("inst", "validation", "wild-multiplier-size.csv"))
  if (!dir.exists(dirname(path))) path <- "wild-multiplier-size.csv"
  utils::write.csv(out, path, row.names = FALSE)
  message("\nwrote ", path)
}
