# Size and power of a prototype of the Dette-Neumeyer-Van Keilegom test --------
#
# Run from the package root:
#   Rscript inst/validation/variance-form-dnvk-size-power.R
#
# N_MC sets the replications per cell (default 1000) and N_MC_GATE those of the
# twelve cells that decide criterion 2 of the release gate in README.md: the
# null cells of block 1 with Gaussian errors (default 5000, where the 99%
# interval around 5% is [0.042, 0.058]; at 1000 replications it is
# [0.032, 0.068]). Because each cell has its own seed, the first N_MC
# replications of a gate cell are the ones a run with N_MC_GATE = N_MC makes.
# N_BOOT sets the bootstrap samples per test (default 199, so that
# 0.05 * (B + 1) is an integer and the bootstrap p-value (1 + #) / (B + 1) is
# exact at 5%). MC_CORES runs the cells in parallel through
# parallel::mclapply(). SCENARIOS, for example "1:12", runs a subset of the
# cells and CSV names the file it writes, so that disjoint subsets can run as
# separate processes where mclapply() cannot fork (Windows); because each cell
# has its own seed, the rows of the subsets, put back in the order of the
# `scenario` column, are those of one full run.
#
# Each replication ends by clearing the package's validation cache. Every call
# to performVarianceFormTest() stores the result of its input validation, keyed
# by the model, and nothing removes the entries, so a cell of 5000 new models
# would grow by some 80 MB. The cache only saves repeating a validation, so
# clearing it changes no value and draws no random number.
#
# The test is the one of Dette, Neumeyer and Van Keilegom (2007), as defined in
# the authors' submitted manuscript and coded in plain R in
# variance-form-dnvk-prototype.R; nothing in R/ is used for it. The paper's
# mean is a kernel smooth of one covariate. Here the mean is the linear model
# the package tests, fitted by least squares, and the variance function and the
# smoother use the one variable the variance depends on, x. The bootstrap
# refits the linear model to every bootstrap sample. See
# variance-form-dnvk-design.md.
#
# Block 1, the designs of variance-form-size-power.R: y = 1 + 2 x + 0.5 z +
# sigma(x) e with x and z uniform on (1, 5) and the model y ~ x + z, at 50, 150
# and 400 observations, with Gaussian and with t5 errors scaled to unit
# variance, for each form against each true variance function. A cell is a
# null cell when the form nests the truth. A sixth truth is added to the five
# of that study, a log-variance with a sine wave of one period over the range
# of x on top of the exponential one. It is antisymmetric about the middle of
# the range, so the squares and products that performVarianceFormTest() adds
# by default hardly span it: it is the departure a consistent test is for. It
# was fixed before any result was seen.
#
# Four tests are run on each sample:
#   - dnvk_cm, dnvk_ks: the Cramer-von Mises and Kolmogorov-Smirnov statistics
#     of the prototype, each with its bootstrap p-value, on the variance
#     regressor x, with the Epanechnikov kernel and the bandwidth of
#     dnvk_bandwidth(): h = 0.3 (n / 100)^(-0.3) times the range of x;
#   - form_test_x: performVarianceFormTest(var_formula = ~ x), the existing
#     test on the same single variable, against its square (one degree of
#     freedom), which is the like-for-like comparison;
#   - form_test_default: performVarianceFormTest() with its defaults, the
#     variance regressors x and z against their squares and product (three
#     degrees of freedom), as variance-form-size-power.R runs it.
#
# Block 2, the bandwidth: block 1 at 150 observations with Gaussian errors, for
# the null truths of each form, the quadratic log-variance and the wave, with
# the constant of the bandwidth halved (0.15) and doubled (0.6).

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
n_mc <- get_count("N_MC", "1000", 20L)
n_mc_gate <- get_count("N_MC_GATE", "5000", 20L)
n_boot <- get_count("N_BOOT", "199", 19L)
n_cores <- get_count("MC_CORES", "1", 1L)
seed <- 20261102L
invisible(ht_set_log_level("SILENT"))

prototype <- file.path("inst", "validation", "variance-form-dnvk-prototype.R")
if (!file.exists(prototype)) prototype <- "variance-form-dnvk-prototype.R"
source(prototype)

# --- data-generating processes ------------------------------------------------------

TRUTHS <- list(
  constant    = list(sd = function(x) rep(1, length(x)),
                     label = "sigma^2 = 1"),
  exponential = list(sd = function(x) exp(0.3 * x),
                     label = "sigma^2 = exp(0.6 x)"),
  power       = list(sd = function(x) x^1.5,
                     label = "sigma^2 = x^3"),
  linear      = list(sd = function(x) sqrt(0.5 + x),
                     label = "sigma^2 = 0.5 + x"),
  quadratic   = list(sd = function(x) exp(0.25 * (x - 3)^2),
                     label = "sigma^2 = exp(0.5 (x - 3)^2)"),
  wave        = list(sd = function(x) exp(0.3 * x + 0.4 * sin(pi * (x - 1) / 2)),
                     label = "sigma^2 = exp(0.6 x + 0.8 sin(pi (x - 1) / 2))")
)

NESTS <- list(
  exponential = c("constant", "exponential"),
  power       = c("constant", "power")
)

make_data <- function(n, sd_fun, errors) {
  x <- stats::runif(n, 1, 5)
  z <- stats::runif(n, 1, 5)
  e <- if (errors == "normal") stats::rnorm(n) else stats::rt(n, df = 5) / sqrt(5 / 3)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + sd_fun(x) * e, x = x, z = z)
  list(model = stats::lm(y ~ x + z, data = d), data = d)
}

# --- scenarios ----------------------------------------------------------------------

grid <- function(block, bandwidth, form, truth, errors, n) {
  expand.grid(
    block = block, bandwidth = bandwidth, form = form, truth = truth,
    errors = errors, n = n, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  grid("main", 0.3, names(NESTS), names(TRUTHS), c("normal", "t5"), c(50L, 150L, 400L)),
  grid("bandwidth", c(0.15, 0.6), "exponential",
       c("constant", "exponential", "quadratic", "wave"), "normal", 150L),
  grid("bandwidth", c(0.15, 0.6), "power",
       c("constant", "power", "quadratic", "wave"), "normal", 150L)
)
scenarios$hypothesis <- ifelse(
  mapply(function(form, truth) truth %in% NESTS[[form]], scenarios$form, scenarios$truth),
  "null", "alternative"
)
scenarios$replications <- ifelse(
  scenarios$block == "main" & scenarios$errors == "normal" & scenarios$hypothesis == "null",
  n_mc_gate, n_mc
)

# --- run ----------------------------------------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

tests <- c("dnvk_cm", "dnvk_ks", "form_test_x", "form_test_default")

run_scenario <- function(i) {
  s <- scenarios[i, ]
  set.seed(seed + i)
  draws <- vapply(seq_len(s$replications), function(rep) {
    o <- make_data(s$n, TRUTHS[[s$truth]]$sd, s$errors)
    form_x <- pval(performVarianceFormTest(o$model, var_formula = ~x, form = s$form))
    form_default <- pval(performVarianceFormTest(o$model, form = s$form))
    x <- o$data$x
    W <- dnvk_weights(x, dnvk_bandwidth(x, s$bandwidth))
    bt <- dnvk_bootstrap(
      o$data$y, x, dnvk_mean_lm(stats::model.matrix(o$model)), W,
      dnvk_forms[[s$form]], B = n_boot
    )
    # See the header: this changes no value and draws no random number.
    heteroTests:::clearValidationCache()
    if (is.null(bt)) {
      return(c(NA_real_, NA_real_, form_x, form_default, NA_real_))
    }
    c(
      dnvk_p_value(bt$cm, bt$boot[, "cm"]),
      dnvk_p_value(bt$ks, bt$boot[, "ks"]),
      form_x, form_default, bt$failed
    )
  }, numeric(5))

  rows <- lapply(seq_along(tests), function(j) {
    p <- draws[j, ]
    ok <- !is.na(p)
    rate <- if (any(ok)) mean(p[ok] <= alpha) else NA_real_
    data.frame(
      scenario = i,
      block = s$block,
      bandwidth = s$bandwidth,
      form = s$form,
      truth = s$truth,
      truth_label = TRUTHS[[s$truth]]$label,
      hypothesis = s$hypothesis,
      errors = s$errors,
      n = s$n,
      test = tests[j],
      replications = s$replications,
      bootstrap = if (j <= 2L) n_boot else NA_integer_,
      rejection_rate = rate,
      mc_se = if (any(ok)) sqrt(rate * (1 - rate) / sum(ok)) else NA_real_,
      effective = sum(ok),
      failures = sum(!ok),
      bootstrap_failures = if (j <= 2L) sum(draws[5L, ], na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-9s c=%.2f %-11s <- %-11s %-6s n=%3d %-11s cm %.3f ks %.3f form(x) %.3f form %.3f",
    s$block, s$bandwidth, s$form, s$truth, s$errors, s$n, s$hypothesis,
    out$rejection_rate[1], out$rejection_rate[2], out$rejection_rate[3], out$rejection_rate[4]
  ))
  out
}

index <- seq_len(nrow(scenarios))
chosen <- Sys.getenv("SCENARIOS", unset = "")
if (nzchar(chosen)) {
  index <- intersect(index, eval(parse(text = chosen)))
}
results <- if (n_cores > 1L) {
  parallel::mclapply(index, run_scenario, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(index, run_scenario)
}

# A worker that was killed returns NULL and one that failed returns the error,
# and rbind() would drop the first without a word.
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) == length(tests), logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(index[!complete], collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- Sys.getenv(
  "CSV",
  unset = file.path("inst", "validation", "variance-form-dnvk-size-power.csv")
)
if (!dir.exists(dirname(path))) path <- "variance-form-dnvk-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
