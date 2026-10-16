# The re-estimating bootstraps of weighted-fits-size.R: prototype and package --
#
# Run from the package root:
#   Rscript inst/validation/weighted-fits-refit-reproduction.R
#
# STEP sets which samples of each cell are checked, every STEP-th; N_MC the
# replications of a cell, which must be those of weighted-fits-size.R for the
# samples to be its samples; N_BOOT the bootstrap samples of each test. The
# evidence uses STEP=10, N_MC=2000 and N_BOOT=499, the defaults: 200 samples
# of each cell, with the B of the recorded rows.
#
# Block 4 of weighted-fits-size.R measures the three resampling tests on
# fitWLS() fits with the weights estimated again in each replicate. Its four
# rows "estimated, re-estimated" were measured in 0.12.0, before the package
# could do this, by a repetition of the resampling outside it:
# resample_weighted() for the wild and the White bootstrap, and
# bootstrapTestStatistic() with a test function that calls fitWLS() for
# Koenker's statistic. Since 0.15.0 the three functions estimate the weights
# of a fitWLS() fit again themselves (`reestimate_weights`), and
# weighted-fits-size.R calls them. The rows were not run again. This script
# checks instead that the package reproduces the repetition on the samples of
# those rows: on each sample it checks, it runs the repetition and the package
# from the same state of the random number generator, and compares their
# p-values, their replicates, the replicates that failed and the state each
# leaves the generator in.
#
# The samples are those of weighted-fits-size.R: cell i of its block 4 starts
# from set.seed(seed_boot + i) and draws its samples in turn, each followed by
# its bootstrap. A sample that is not checked is skipped by drawing the random
# numbers it would draw, without fitting anything: the three uniform and
# normal vectors of the data, then, for each replicate, the indices of the
# resampled residuals and, for the wild bootstrap, its signs. Before the first
# sample of each cell the script checks that skipping a sample leaves the
# generator where running the repetition on it does.
#
# The design, the repetition and the seeds are read from weighted-fits-size.R
# itself: the script evaluates the assignments named in `taken` below and
# nothing else of that file.
#
# CELLS, for example "1,3", runs those cells, numbered as in `cells` below,
# and saves their results to the .rds file PART instead of writing the CSV;
# MERGE, a directory, reads every .rds file in it, checks that together they
# hold every cell once, and writes the CSV, which is then the one a single
# process writes. On Windows, which cannot fork, that is how the cells run in
# parallel. CSV names the file written, by default
# inst/validation/weighted-fits-refit-reproduction.csv.
#
# Each sample starts by clearing the package's validation cache, as in
# weighted-fits-size.R, which changes no value and draws no random number.

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
step <- get_count("STEP", "10", 1L)
replications <- get_count("N_MC", "2000", 1L)
invisible(ht_set_log_level("SILENT"))

# --- the design and the repetition, from weighted-fits-size.R ------------------

source_script <- file.path("inst", "validation", "weighted-fits-size.R")
if (!file.exists(source_script)) source_script <- "weighted-fits-size.R"
taken <- c(
  "n_obs", "make_fgls", "n_boot", "seed_boot", "koenker_statistic",
  "white_statistic", "resample_weighted", "koenker_reestimated",
  "boot_tests", "boot_scenario", "boot_scenarios"
)
for (e in parse(source_script, keep.source = FALSE)) {
  if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]]) &&
        as.character(e[[2L]]) %in% taken) {
    eval(e, globalenv())
  }
}
missing_names <- taken[!vapply(taken, exists, logical(1), envir = globalenv())]
if (length(missing_names)) {
  stop("Not found in ", source_script, ": ", paste(missing_names, collapse = ", "), ".")
}
n_boot <- get_count("N_BOOT", as.character(n_boot), 19L)

# The cells: the rows of block 4 whose weights are estimated again, with their
# number there, which decides their seed.
refit <- which(boot_scenarios$weights == "estimated, re-estimated")
cells <- data.frame(scenario = refit, test = boot_scenarios$test[refit],
                    n = boot_scenarios$n[refit], stringsAsFactors = FALSE)
checked <- seq(step, replications, by = step)

# --- the two computations -----------------------------------------------------

# Each returns the p-value and the replicates.
prototype <- function(test, o) {
  switch(test,
    performWildBootstrapTest = resample_weighted(o, "wild", reestimate = TRUE),
    performWhiteTestBootstrap = resample_weighted(o, "white", reestimate = TRUE),
    rbootstrap_test_statistic = {
      # As in 0.12.0: the package holds the weights fixed in the refit, and the
      # test function estimates them again from the data of the replicate.
      r <- bootstrapTestStatistic(koenker_reestimated, o$model, o$data, B = n_boot,
                                  progress = FALSE, reestimate_weights = FALSE)
      list(p.value = r$p_value, replicates = r$replicates)
    }
  )
}

package <- function(test, o) {
  switch(test,
    performWildBootstrapTest = {
      r <- performWildBootstrapTest(o$model, o$data, B = n_boot, progress = FALSE,
                                    reestimate_weights = TRUE)
      list(p.value = r$p.value, replicates = r$bootstrap$replicates)
    },
    performWhiteTestBootstrap = {
      r <- performWhiteTestBootstrap(o$model, o$data, B = n_boot, reestimate_weights = TRUE)
      list(p.value = r$p.value, replicates = r$boot_statistics)
    },
    rbootstrap_test_statistic = {
      r <- bootstrapTestStatistic(performKoenkerTest, o$model, o$data, B = n_boot,
                                  progress = FALSE, reestimate_weights = TRUE)
      list(p.value = r$p_value, replicates = r$replicates)
    }
  )
}

quietly <- function(expr) {
  tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
}

# The random numbers one sample draws, without the fits: make_fgls() with
# normal errors draws two uniform vectors and a normal one, and each replicate
# draws n indices of resampled residuals, after n signs in the wild bootstrap.
# sample(x, k, replace = TRUE) draws as sample.int(length(x), k, TRUE) does.
skip_sample <- function(test, n) {
  stats::runif(n)
  stats::runif(n)
  stats::rnorm(n)
  for (b in seq_len(n_boot)) {
    if (test == "performWildBootstrapTest") {
      sample.int(2L, n, replace = TRUE)
    }
    sample.int(n, n, replace = TRUE)
  }
  invisible(NULL)
}

state <- function() get(".Random.seed", envir = globalenv())
set_state <- function(s) assign(".Random.seed", s, envir = globalenv())

run_cell <- function(k) {
  cell <- cells[k, ]
  # Skipping a sample must leave the generator where running it does.
  set.seed(seed_boot + cell$scenario)
  heteroTests:::clearValidationCache()
  o <- make_fgls("normal", cell$n)
  quietly(prototype(cell$test, o))
  ran <- state()
  set.seed(seed_boot + cell$scenario)
  skip_sample(cell$test, cell$n)
  if (!identical(state(), ran)) {
    stop("Skipping a sample of cell ", k, " does not draw what running it draws.")
  }

  set.seed(seed_boot + cell$scenario)
  rows <- vector("list", length(checked))
  for (i in seq_len(max(checked))) {
    if (!i %in% checked) {
      skip_sample(cell$test, cell$n)
      next
    }
    heteroTests:::clearValidationCache()
    o <- make_fgls("normal", cell$n)
    before <- state()
    a <- quietly(prototype(cell$test, o))
    after_prototype <- state()
    set_state(before)
    heteroTests:::clearValidationCache()
    b <- quietly(package(cell$test, o))
    after_package <- state()
    # The sample after this one follows the repetition, as in
    # weighted-fits-size.R in 0.12.0.
    set_state(after_prototype)

    p_a <- if (is.null(a)) NA_real_ else a$p.value
    p_b <- if (is.null(b)) NA_real_ else b$p.value
    ra <- if (is.null(a)) rep(NA_real_, n_boot) else a$replicates
    rb <- if (is.null(b)) rep(NA_real_, n_boot) else b$replicates
    both <- is.finite(ra) & is.finite(rb)
    rows[[match(i, checked)]] <- data.frame(
      sample = i,
      p_prototype = p_a,
      p_package = p_b,
      failed_prototype = sum(!is.finite(ra)),
      failed_package = sum(!is.finite(rb)),
      same_failures = identical(is.finite(ra), is.finite(rb)),
      max_rel_diff = if (any(both)) {
        max(abs(ra[both] - rb[both]) / pmax(abs(ra[both]), .Machine$double.eps))
      } else {
        NA_real_
      },
      same_state = identical(after_prototype, after_package)
    )
  }
  per_sample <- do.call(rbind, rows)
  message(sprintf("cell %d (%s, n = %d): %d samples checked", k, cell$test, cell$n,
                  nrow(per_sample)))
  per_sample
}

summarise_cell <- function(k, s) {
  cell <- cells[k, ]
  agree <- (s$p_prototype < alpha) == (s$p_package < alpha)
  data.frame(
    test = cell$test,
    weights = "estimated, re-estimated",
    n = cell$n,
    scenario = cell$scenario,
    replications = replications,
    step = step,
    bootstrap = n_boot,
    samples = nrow(s),
    decisions_agree = sum(agree, na.rm = TRUE),
    p_values_equal = sum(s$p_prototype == s$p_package, na.rm = TRUE),
    max_abs_dp = max(abs(s$p_prototype - s$p_package)),
    max_rel_replicate_diff = max(s$max_rel_diff),
    samples_failed_prototype = sum(s$failed_prototype > 0L),
    samples_failed_package = sum(s$failed_package > 0L),
    samples_same_failures = sum(s$same_failures),
    samples_same_state = sum(s$same_state),
    rejections_prototype = sum(s$p_prototype < alpha),
    rejections_package = sum(s$p_package < alpha),
    stringsAsFactors = FALSE
  )
}

settings <- list(step, replications, n_boot, seed_boot)
chosen <- Sys.getenv("CELLS", unset = "")
merge_dir <- Sys.getenv("MERGE", unset = "")
if (nzchar(merge_dir)) {
  parts <- lapply(list.files(merge_dir, pattern = "\\.rds$", full.names = TRUE), readRDS)
  index <- unlist(lapply(parts, `[[`, "index"))
  results <- unlist(lapply(parts, `[[`, "samples"), recursive = FALSE)
  if (anyDuplicated(index) || !setequal(index, seq_len(nrow(cells)))) {
    stop("The parts in ", merge_dir, " do not hold every cell exactly once.")
  }
  if (!all(vapply(parts, function(x) identical(x$settings, settings), logical(1)))) {
    stop("The parts in ", merge_dir, " were run with other settings.")
  }
  results <- results[order(index)]
  index <- sort(index)
} else {
  index <- seq_len(nrow(cells))
  if (nzchar(chosen)) {
    index <- intersect(index, eval(parse(text = paste0("c(", chosen, ")"))))
  }
  results <- lapply(index, run_cell)
}

if (nzchar(chosen) && !nzchar(merge_dir)) {
  part <- Sys.getenv("PART", unset = "")
  if (!nzchar(part)) {
    stop("CELLS needs PART, the .rds file to save the results to.")
  }
  saveRDS(list(index = index, samples = results, settings = settings), part)
  message("saved cells ", paste(index, collapse = ", "), " to ", part)
} else {
  out <- do.call(rbind, lapply(seq_along(index), function(j) {
    summarise_cell(index[j], results[[j]])
  }))
  path <- Sys.getenv("CSV", unset = file.path("inst", "validation",
                                              "weighted-fits-refit-reproduction.csv"))
  if (!dir.exists(dirname(path))) path <- "weighted-fits-refit-reproduction.csv"
  utils::write.csv(out, path, row.names = FALSE)
  message("\nwrote ", path)
}
