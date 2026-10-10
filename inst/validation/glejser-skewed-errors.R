# The Glejser test under asymmetric errors --------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/glejser-skewed-errors.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000, where the 99% interval around a true 5% is
# [0.042, 0.058]. MC_CORES runs the scenarios in parallel. Each scenario has
# its own seed, so the results do not depend on the number of cores.
#
# Glejser's statistic regresses |e-hat| on a transformation of a regressor.
# |e-hat| differs from |e| by sign(e) x'(beta-hat - beta), and the mean of that
# term is zero only when positive and negative errors are equally likely
# (Godfrey 1996). performGlejserTest(robust = TRUE) regresses
# |e-hat| - m-hat * e-hat instead, with m-hat the mean sign of the residuals
# (Im 2000; Machado and Santos Silva 2000). The script measures both, and
# Koenker's test beside them, in four blocks.
#
# Koenker's test appears twice. performKoenkerTest() takes every regressor of
# the mean equation, two here, and is what a user who follows the help page
# runs. The other row is Koenker's statistic on the one regressor the Glejser
# test is given, n R^2 of the squared residuals on it with one degree of
# freedom, computed here because the package has no call for it. That row is
# the like-for-like comparison.
#
# Block 1, size. The cross-sectional design of pass-a-size-power.R under its
# null, with errors of six shapes, all scaled to mean 0 and variance 1. The
# variance regressor is x1 itself, a column of the mean equation.
#
# Block 2, size with a regressor outside the mean equation. The same design
# with transformation = "inverse", so the auxiliary regressor is 1 / x1.
#
# Block 3, size after a weighted fit. The standard deviation is proportional
# to x1 and the model is fitted with the weights that undo it, so the null
# hypothesis is true of the weighted model. The tests read its Pearson
# residuals.
#
# Block 4, power. sigma_i^2 = exp(gamma * x1). Under asymmetric errors the
# rejection rate of the uncorrected statistic is not a power, because its size
# is not 5%; it is recorded for comparison only.

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
n_cores <- get_count("MC_CORES", "1", 1L)
seed <- 20261012L
invisible(ht_set_log_level("SILENT"))

# --- errors ---------------------------------------------------------------------

# Mean 0 and variance 1 throughout. The skewness is 0 for the first two, 1.26
# for the centred chi-squared with 5 degrees of freedom, 2 for the centred
# exponential, -2 for its mirror image and 6.18 for the centred lognormal.
draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    chisq5 = (rchisq(n, df = 5) - 5) / sqrt(10),
    exponential = rexp(n) - 1,
    exponential_left = 1 - rexp(n),
    lognormal = (rlnorm(n) - exp(0.5)) / sqrt((exp(1) - 1) * exp(1)),
    stop("unknown error distribution")
  )
}

# --- designs --------------------------------------------------------------------

# y = 1 + 2 x1 + 0.5 x2 + sigma_i e_i, with x1 > 0 so that 1 / x1 is defined.
make_ols <- function(n, errors, gamma = 0) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1, x2 = x2,
    y = 1 + 2 * x1 + 0.5 * x2 + exp(0.5 * gamma * x1) * draw_errors(n, errors)
  )
  list(model = lm(y ~ x1 + x2, data = d), data = d)
}

make_weighted <- function(n, errors) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1, x2 = x2,
    y = 1 + 2 * x1 + 0.5 * x2 + x1 * draw_errors(n, errors),
    w = 1 / x1^2
  )
  list(model = lm(y ~ x1 + x2, data = d, weights = w), data = d)
}

# Koenker's statistic on the regressor the Glejser test uses.
koenker_same_regressor <- function(o, transformation) {
  e <- residuals(o$model)
  w <- weights(o$model)
  if (!is.null(w)) e <- sqrt(w) * e
  z <- if (transformation == "inverse") 1 / o$data$x1 else o$data$x1
  statistic <- length(e) * stats::cor(e^2, z)^2
  list(p.value = stats::pchisq(statistic, df = 1, lower.tail = FALSE))
}

# --- scenarios ------------------------------------------------------------------

scenario <- function(block, errors, n, transformation = "abs", weights = "none", gamma = 0) {
  expand.grid(
    block = block, errors = errors, n = n, transformation = transformation,
    weights = weights, gamma = gamma,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  scenario(
    "size",
    c("normal", "t5", "chisq5", "exponential", "exponential_left", "lognormal"),
    c(50L, 150L, 400L, 1000L)
  ),
  scenario(
    "size, regressor outside the mean equation",
    c("normal", "exponential", "lognormal"), c(50L, 400L),
    transformation = "inverse"
  ),
  scenario(
    "size, weighted fit",
    c("normal", "exponential", "lognormal"), c(150L, 400L),
    weights = "exact"
  ),
  scenario("power", c("normal", "exponential"), c(50L, 150L), gamma = c(0.2, 0.4))
)

# --- run ------------------------------------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

run_scenario <- function(i) {
  s <- scenarios[i, ]
  set.seed(seed + i)
  p <- vapply(seq_len(n_mc), function(rep) {
    o <- if (s$weights == "exact") {
      make_weighted(s$n, s$errors)
    } else {
      make_ols(s$n, s$errors, s$gamma)
    }
    c(
      glejser = pval(performGlejserTest(o$model, o$data, "x1", s$transformation)),
      glejser_robust = pval(
        performGlejserTest(o$model, o$data, "x1", s$transformation, robust = TRUE)
      ),
      koenker_same_regressor = pval(koenker_same_regressor(o, s$transformation)),
      koenker = pval(performKoenkerTest(o$model, o$data))
    )
  }, numeric(4))

  rows <- lapply(rownames(p), function(test) {
    ok <- p[test, ][!is.na(p[test, ])]
    rate <- if (length(ok)) mean(ok < alpha) else NA_real_
    data.frame(
      block = s$block,
      test = test,
      errors = s$errors,
      n = s$n,
      transformation = s$transformation,
      weights = s$weights,
      gamma = s$gamma,
      replications = n_mc,
      rejection_rate = rate,
      mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
      effective = length(ok),
      failures = n_mc - length(ok),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-42s %-17s n=%4d gamma=%.1f  glejser %.4f  robust %.4f  koenker(z) %.4f  koenker %.4f",
    s$block, s$errors, s$n, s$gamma,
    out$rejection_rate[1], out$rejection_rate[2], out$rejection_rate[3], out$rejection_rate[4]
  ))
  out
}

index <- seq_len(nrow(scenarios))
results <- if (n_cores > 1L) {
  parallel::mclapply(index, run_scenario, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(index, run_scenario)
}

# A worker that was killed returns NULL and one that failed returns the error,
# and rbind() would drop the first without a word.
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) == 4L, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(which(!complete), collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- file.path("inst", "validation", "glejser-skewed-errors.csv")
if (!dir.exists(dirname(path))) path <- "glejser-skewed-errors.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
