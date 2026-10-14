# Size and power of a prototype of the Wang-Zhou test ------------------------------
#
# Run from the package root:
#   Rscript inst/validation/variance-form-wz-size-power.R
#
# N_MC sets the replications per cell (default 1000) and N_MC_GATE those of the
# twelve cells that decide criterion 2 of the release gate in README.md: the
# null cells with Gaussian errors (default 5000, where the 99% interval around
# 5% is [0.042, 0.058]; at 1000 replications it is [0.032, 0.068]). Because
# each cell has its own seed, the first N_MC replications of a gate cell are
# the ones a run with N_MC_GATE = N_MC makes. N_BOOT sets the bootstrap samples
# (default 199). MC_CORES runs the cells in parallel through
# parallel::mclapply(). SCENARIOS, for example "1:12", runs a subset of the
# cells and CSV names the file it writes, so that disjoint subsets can run as
# separate processes where mclapply() cannot fork (Windows); because each cell
# has its own seed, the rows of the subsets, put back in the order of the
# `scenario` column, are those of one full run.
#
# Each replication ends by clearing the package's validation cache. Every call
# to performVarianceFormTest() stores the result of its input validation, keyed
# by the model. When this study started the cache kept every entry, so a cell
# of 5000 new models would have grown by some 80 MB; it now keeps the 1000 most
# recent (NEWS.md). The cache only saves repeating a validation, so clearing it
# changes no value and draws no random number.
#
# The test is the one of Wang and Zhou (2007), as defined in the authors'
# working paper and coded in plain R in variance-form-wz-prototype.R; nothing
# in R/ is used for it. Its mean is the linear model the package tests, fitted
# with the variance function by pseudo-likelihood, and it smooths in the one
# variable the variance depends on, x.
#
# The designs are those of variance-form-dnvk-size-power.R, block 1, which are
# those of variance-form-size-power.R with a sixth true variance: y = 1 + 2 x +
# 0.5 z + sigma(x) e with x and z uniform on (1, 5) and the model y ~ x + z, at
# 50, 150 and 400 observations, with Gaussian and with t5 errors scaled to unit
# variance, for each form against each true variance function. A cell is a
# null cell when the form nests the truth. The sixth truth is a log-variance
# with a sine wave of one period over the range of x on top of the exponential
# one, which the squares and products that performVarianceFormTest() adds by
# default hardly span.
#
# The tests on each sample:
#   - wz_bootstrap: the prototype with its bootstrap p-value, B = N_BOOT, with
#     h = c (n / 100)^(-1/5) times the range of x. c = 0.15 is the test whose
#     level the release gate judges. At 50 and 150 observations the test is
#     also computed with c = 0.10 and 0.25, to say how much the bandwidth
#     matters; the three share the samples and the bootstrap samples, since the
#     fits do not depend on h. At 400 observations only c = 0.15 is computed,
#     because each bandwidth there costs as much as the rest of the test;
#   - wz_asymptotic: the same statistic standardized by a plug-in variance and
#     referred to the standard normal, one-sided, at the same bandwidths. The
#     working paper gives the limit but no variance estimate, so this is an
#     addition (wz_standardized());
#   - form_test_x: performVarianceFormTest(var_formula = ~ x), the existing
#     test on the same single variable, the like-for-like comparison;
#   - form_test_default: performVarianceFormTest() with its defaults.
# The working paper does not name its kernel. KERNEL selects it; the default is
# the Epanechnikov kernel, chosen before this study ran because it reproduces
# the paper's Table 1 and the Gaussian kernel does not
# (variance-form-wz-reproduction.csv).
#
# The prototype of Dette, Neumeyer and Van Keilegom is not run again: its rates
# in the same designs, on other samples, are in variance-form-dnvk-size-power.csv
# and make-table.R prints them beside these.

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
seed <- 20261202L
constants <- c(0.10, 0.15, 0.25)
kernel <- Sys.getenv("KERNEL", unset = "epanechnikov")
invisible(ht_set_log_level("SILENT"))

prototype <- file.path("inst", "validation", "variance-form-wz-prototype.R")
if (!file.exists(prototype)) prototype <- "variance-form-wz-prototype.R"
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

scenarios <- expand.grid(
  form = names(NESTS), truth = names(TRUTHS), errors = c("normal", "t5"),
  n = c(50L, 150L, 400L), KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)
scenarios$hypothesis <- ifelse(
  mapply(function(form, truth) truth %in% NESTS[[form]], scenarios$form, scenarios$truth),
  "null", "alternative"
)
scenarios$replications <- ifelse(
  scenarios$errors == "normal" & scenarios$hypothesis == "null", n_mc_gate, n_mc
)

# --- run ----------------------------------------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

run_scenario <- function(i) {
  s <- scenarios[i, ]
  cs <- if (s$n >= 400L) 0.15 else constants
  nc <- length(cs)
  set.seed(seed + i)
  draws <- vapply(seq_len(s$replications), function(rep) {
    o <- make_data(s$n, TRUTHS[[s$truth]]$sd, s$errors)
    form_x <- pval(performVarianceFormTest(o$model, var_formula = ~x, form = s$form))
    form_default <- pval(performVarianceFormTest(o$model, form = s$form))
    x <- o$data$x
    w <- if (s$form == "power") log(x) else x
    Kmats <- lapply(cs, function(cc) wz_kernel_matrix(x, wz_bandwidth(x, cc), kernel))
    bt <- wz_bootstrap(o$data$y, stats::model.matrix(o$model), cbind(1, w), Kmats, B = n_boot)
    # See the header: this changes no value and draws no random number.
    heteroTests:::clearValidationCache()
    if (is.null(bt)) {
      return(c(rep(NA_real_, 2L * nc), form_x, form_default, NA_real_))
    }
    c(
      vapply(seq_len(nc), function(j) wz_p_value(bt$statistic[j], bt$boot[, j]), numeric(1)),
      stats::pnorm(bt$z, lower.tail = FALSE),
      form_x, form_default, bt$failed
    )
  }, numeric(2L * nc + 3L))

  summarise <- function(test, bandwidth, p, boot_failures) {
    ok <- !is.na(p)
    rate <- if (any(ok)) mean(p[ok] <= alpha) else NA_real_
    data.frame(
      scenario = i,
      form = s$form,
      truth = s$truth,
      truth_label = TRUTHS[[s$truth]]$label,
      hypothesis = s$hypothesis,
      errors = s$errors,
      n = s$n,
      test = test,
      bandwidth = bandwidth,
      kernel = if (startsWith(test, "wz_")) kernel else NA_character_,
      replications = s$replications,
      bootstrap = if (test == "wz_bootstrap") n_boot else NA_integer_,
      rejection_rate = rate,
      mc_se = if (any(ok)) sqrt(rate * (1 - rate) / sum(ok)) else NA_real_,
      effective = sum(ok),
      failures = sum(!ok),
      bootstrap_failures = boot_failures,
      stringsAsFactors = FALSE
    )
  }
  boot_failures <- sum(draws[2L * nc + 3L, ], na.rm = TRUE)
  rows <- c(
    lapply(seq_len(nc), function(j) {
      summarise("wz_bootstrap", cs[j], draws[j, ], boot_failures)
    }),
    lapply(seq_len(nc), function(j) {
      summarise("wz_asymptotic", cs[j], draws[nc + j, ], NA_real_)
    }),
    list(
      summarise("form_test_x", NA_real_, draws[2L * nc + 1L, ], NA_real_),
      summarise("form_test_default", NA_real_, draws[2L * nc + 2L, ], NA_real_)
    )
  )
  out <- do.call(rbind, rows)
  main <- out$test == "wz_bootstrap" & out$bandwidth == 0.15
  message(sprintf(
    "%-11s <- %-11s %-6s n=%3d %-11s wz %.3f [%s] asym %.3f form(x) %.3f form %.3f",
    s$form, s$truth, s$errors, s$n, s$hypothesis, out$rejection_rate[main],
    paste(sprintf("%.3f", out$rejection_rate[out$test == "wz_bootstrap"]), collapse = " "),
    out$rejection_rate[out$test == "wz_asymptotic" & out$bandwidth == 0.15],
    out$rejection_rate[out$test == "form_test_x"],
    out$rejection_rate[out$test == "form_test_default"]
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
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) >= 4L, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(index[!complete], collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- Sys.getenv(
  "CSV",
  unset = file.path("inst", "validation", "variance-form-wz-size-power.csv")
)
if (!dir.exists(dirname(path))) path <- "variance-form-wz-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
