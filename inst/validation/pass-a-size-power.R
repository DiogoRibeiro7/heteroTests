# ---------------------------------------------------------------------------
# Validation artefact: Pass A (classical regression diagnostics)
#
# Monte Carlo size and power for every inferential test in Pass A of the
# statistical validation matrix. This script is the reproducible source of
# `pass-a-size-power.csv`; regenerate with
#
#     Rscript inst/validation/pass-a-size-power.R [n_mc]
#
# Reference equivalence is checked separately, and exactly, in
# `tests/testthat/test-pass-a-reference.R`. This script measures behaviour:
# whether each test rejects at its nominal level under the null, and how often
# it rejects under controlled alternatives.
#
# Release gate: every test marked "validated" in the matrix must hold empirical
# size within Monte Carlo error of the nominal 5% under the Gaussian null.
# With n_mc = 5000 the standard error at alpha = 0.05 is sqrt(.05*.95/5000) =
# 0.0031, so the two-sided 99% interval is roughly [0.042, 0.058].
# ---------------------------------------------------------------------------

# Prefer the source tree when run from the repository root, so the artefact
# records the behaviour of the code under review rather than of whatever
# version happens to be installed in the library.
in_source_tree <- file.exists("DESCRIPTION") &&
  any(grepl("Package: heteroTests", readLines("DESCRIPTION", warn = FALSE), fixed = TRUE))
if (in_source_tree && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  suppressMessages(library(heteroTests))
}
ht_set_log_level("SILENT")

args <- commandArgs(trailingOnly = TRUE)
n_mc_arg <- suppressWarnings(as.integer(args[length(args) > 0L][1L]))
N_MC <- if (length(n_mc_arg) == 1L && !is.na(n_mc_arg) && n_mc_arg > 0L) n_mc_arg else 5000L
ALPHA <- 0.05

# --- data generating processes ---------------------------------------------

# Cross-sectional: y = 1 + 2 x1 + 0.5 x2 + sigma_i * e_i, with x1 > 0 so that
# the log and inverse transformations used by Park and Glejser are defined.
gen_cs <- function(n, dgp, gamma = 0) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  sd_i <- switch(dgp,
    null      = rep(1, n),
    null_t5   = rep(1, n),
    exp_var   = exp(0.5 * gamma * x1),      # sigma_i^2 = exp(gamma x1)
    quad_var  = sqrt(1 + gamma * x1^2),     # sigma_i^2 = 1 + gamma x1^2
    stop("unknown dgp")
  )
  err <- if (dgp == "null_t5") rt(n, 5) / sqrt(5 / 3) else rnorm(n)
  data.frame(x1 = x1, x2 = x2, y = 1 + 2 * x1 + 0.5 * x2 + sd_i * err)
}

# Time series: mean-zero series, optionally with ARCH(1) conditional variance.
gen_ts <- function(n, dgp, gamma = 0.6) {
  switch(dgp,
    null    = rnorm(n),
    null_t5 = rt(n, 5) / sqrt(5 / 3),
    arch    = {
      z <- rnorm(n)
      e <- numeric(n)
      e[1] <- z[1]
      for (t in 2:n) e[t] <- sqrt(0.2 + gamma * e[t - 1]^2) * z[t]
      e
    },
    stop("unknown dgp")
  )
}

# --- the tests under study --------------------------------------------------

p_or_na <- function(expr) tryCatch(expr$p.value, error = function(e) NA_real_)

CS_TESTS <- list(
  `Breusch-Pagan`         = function(m, d) p_or_na(performBPTest(m, d)),
  `Koenker`               = function(m, d) p_or_na(performKoenkerTest(m, d)),
  `White`                 = function(m, d) p_or_na(performWhiteTest(m, d)),
  `Goldfeld-Quandt`       = function(m, d) p_or_na(performGQTest(m, d, order_by = "x1")),
  `Harvey`                = function(m, d) p_or_na(performHarveyTest(m)),
  `Harvey (studentized)`  = function(m, d) p_or_na(performHarveyTest(m, studentize = TRUE)),
  `Park`                  = function(m, d) p_or_na(performParkTest(m, d, "x1")),
  `Glejser`               = function(m, d) p_or_na(performGlejserTest(m, d, "x1")),
  `Szroeter`              = function(m, d) p_or_na(performSzroeterTest(m, d, order_by = "x1")),
  `Cook-Weisberg`         = function(m, d) p_or_na(performCookWeisbergTest(m)),
  `NCV`                   = function(m, d) p_or_na(performNCVTest(m))
)

TS_TESTS <- list(
  `ARCH LM (q = 3)`    = function(m, d) p_or_na(performArchLMTest(m, lags = 3)),
  `McLeod-Li (m = 10)` = function(m, d) p_or_na(performMcLeodLiTest(m, lags = 10))
)

# --- simulation loops -------------------------------------------------------

run_cs <- function(dgp, gamma = 0, n = 100L, n_mc = N_MC) {
  p <- matrix(NA_real_, n_mc, length(CS_TESTS), dimnames = list(NULL, names(CS_TESTS)))
  for (i in seq_len(n_mc)) {
    d <- gen_cs(n, dgp, gamma)
    m <- lm(y ~ x1 + x2, data = d)
    for (j in seq_along(CS_TESTS)) p[i, j] <- CS_TESTS[[j]](m, d)
  }
  colMeans(p < ALPHA, na.rm = TRUE)
}

run_ts <- function(dgp, n = 300L, n_mc = N_MC) {
  p <- matrix(NA_real_, n_mc, length(TS_TESTS), dimnames = list(NULL, names(TS_TESTS)))
  for (i in seq_len(n_mc)) {
    d <- data.frame(v = gen_ts(n, dgp))
    m <- lm(v ~ 1, data = d)
    for (j in seq_along(TS_TESTS)) p[i, j] <- TS_TESTS[[j]](m, d)
  }
  colMeans(p < ALPHA, na.rm = TRUE)
}

# --- run --------------------------------------------------------------------

set.seed(20260831)

cs_scenarios <- list(
  list(id = "size_gaussian_n100",  dgp = "null",     gamma = 0,    n = 100L),
  list(id = "size_gaussian_n40",   dgp = "null",     gamma = 0,    n = 40L),
  list(id = "size_t5_n100",        dgp = "null_t5",  gamma = 0,    n = 100L),
  list(id = "power_exp_g0.4_n100", dgp = "exp_var",  gamma = 0.4,  n = 100L),
  list(id = "power_quad_g0.15_n100", dgp = "quad_var", gamma = 0.15, n = 100L)
)

ts_scenarios <- list(
  list(id = "size_gaussian_n300", dgp = "null",    n = 300L),
  list(id = "size_t5_n300",       dgp = "null_t5", n = 300L),
  list(id = "power_arch1_a0.6_n300", dgp = "arch", n = 300L)
)

rows <- list()
for (sc in cs_scenarios) {
  message("cross-sectional scenario: ", sc$id)
  r <- run_cs(sc$dgp, sc$gamma, sc$n)
  rows[[length(rows) + 1L]] <- data.frame(
    block = "cross-sectional", scenario = sc$id, n = sc$n,
    test = names(r), rejection_rate = as.numeric(r),
    stringsAsFactors = FALSE
  )
}
for (sc in ts_scenarios) {
  message("time-series scenario: ", sc$id)
  r <- run_ts(sc$dgp, sc$n)
  rows[[length(rows) + 1L]] <- data.frame(
    block = "time-series", scenario = sc$id, n = sc$n,
    test = names(r), rejection_rate = as.numeric(r),
    stringsAsFactors = FALSE
  )
}

results <- do.call(rbind, rows)
results$n_mc <- N_MC
results$alpha <- ALPHA
results$mc_se <- sqrt(results$rejection_rate * (1 - results$rejection_rate) / N_MC)

out_dir <- file.path("inst", "validation")
if (!dir.exists(out_dir)) out_dir <- "."
write.csv(results, file.path(out_dir, "pass-a-size-power.csv"), row.names = FALSE)

wide <- reshape(
  results[, c("test", "scenario", "rejection_rate")],
  idvar = "test", timevar = "scenario", direction = "wide"
)
names(wide) <- sub("^rejection_rate\\.", "", names(wide))
print(wide, row.names = FALSE, digits = 3)

cat("\nR version: ", R.version.string, "\n", sep = "")
cat("heteroTests: ", as.character(utils::packageVersion("heteroTests")), "\n", sep = "")
cat("replications: ", N_MC, "\n", sep = "")
