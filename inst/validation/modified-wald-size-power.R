# The modified Wald test for groupwise heteroscedasticity --------------------------
#
# Run from the package root:
#   Rscript inst/validation/modified-wald-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000, where the 99% interval around a true 5% is
# [0.042, 0.058]. MC_CORES runs the scenarios in parallel. Each scenario has
# its own seed, so the results do not depend on the number of cores.
#
# The statistic is the one of Baum (2001) and of Stata's xttest3, following
# Greene (2000, p. 598). The package does not export it: the size measured here
# is why. For unit i with T_i residuals e_it of the fixed-effects (within) fit,
#
#   s2_i = sum_t e_it^2 / T_i
#   V_i  = sum_t (e_it^2 - s2_i)^2 / (T_i (T_i - 1))
#   W    = sum_i (s2_i - s2)^2 / V_i,   s2 = sum_it e_it^2 / n,
#
# referred to a chi-squared distribution with as many degrees of freedom as
# there are units. Baum's article does not define s2; xttest3 1.0.8 computes it
# as the variance of all the residuals with divisor n, which is the formula
# above because the within residuals have mean zero. With that s2 the function
# below reproduces the example in Baum (2001), chi2(4) = 279.13, and with
# divisor n - 1 it would give 287.09. The script checks the first, and the
# coefficients and sigma_e Baum prints, before it simulates.
#
# xttest3 1.0.8 drops a unit whose V_i is below 1e-12 from the sum and keeps
# the degrees of freedom. This script does not: it records no statistic for
# such a replication and counts it under `failures`. With continuous errors
# and at least five periods it did not happen in any replication here.
#
# Each cell records two rejection rates from the same statistic: the published
# test, with N degrees of freedom, and the same statistic referred to N - 1.
# s2 is a weighted mean of the s2_i, so the N terms of W satisfy one linear
# restriction, and as the number of periods grows W tends to a chi-squared
# with N - 1 degrees of freedom. The second row is not the reference Baum
# gives; it shows how much of the error is the reference distribution.
#
# Block 1, size. Balanced panels with 10, 30 and 100 units and 5, 10, 30 and
# 100 periods: y_it = a_i + 0.5 x1_it - 0.25 x2_it + e_it, with the unit
# effect a_i correlated with x1, and Gaussian or t5 errors scaled to unit
# variance.
#
# Block 2, size in two unbalanced panels: 30 units with 10, 30 or 100 periods
# (ten units each), and 100 units with 5, 10, 20 or 30 periods (25 each).
#
# Block 3, size in long panels: 2, 10 and 30 units over 300 and 1000 periods,
# to find where the level is reached, if anywhere.
#
# Block 4, power. The error variance of unit i is exp(tau z_i - tau^2 / 2),
# with z_i standard normal and drawn once per replication, so the unit
# variances are lognormal with mean 1. tau = 0.25 and 0.5. Where the size is
# not 5% the rejection rate is not a power; it is recorded for completeness.

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
seed <- 20261013L
invisible(ht_set_log_level("SILENT"))

# --- the statistic ----------------------------------------------------------------

# `e` the residuals of the fixed-effects fit, `id` the unit of each, as integers
# 1..N. Returns the statistic and the number of units, or NULL if a unit has
# V_i below the threshold xttest3 1.0.8 uses.
modified_wald <- function(e, id) {
  periods <- tabulate(id)
  s2_i <- rowsum(e^2, id, reorder = TRUE)[, 1] / periods
  v_i <- rowsum((e^2 - s2_i[id])^2, id, reorder = TRUE)[, 1] / (periods * (periods - 1))
  if (any(v_i < 1e-12)) return(NULL)
  s2 <- sum((e - mean(e))^2) / length(e)
  c(statistic = sum((s2_i - s2)^2 / v_i), units = length(periods))
}

# --- the published example ----------------------------------------------------------

# Baum (2001) runs xtreg i f c, fe on Greene's (2000) Table 15.1, the Grunfeld
# investment data for five firms, without firm 2 (Chrysler), and prints the
# slopes f .1065863 and c .3474248, sigma_e 77.151807 and chi2(4) = 279.13.
# plm::Grunfeld holds the same series except three values of US Steel, which
# are set to Greene's here; a comparison with the file of Table 15.1 that
# Baum's example loads found no other difference.
if (requireNamespace("plm", quietly = TRUE)) {
  grunfeld <- get(utils::data("Grunfeld", package = "plm", envir = environment()))
  greene <- grunfeld[grunfeld$firm %in% c(1L, 2L, 3L, 8L), ]
  us_steel <- greene$firm == 2L
  greene$inv[us_steel & greene$year == 1940L] <- 261.6
  greene$capital[us_steel & greene$year == 1946L] <- 232.6
  greene$inv[us_steel & greene$year == 1952L] <- 645.2
  fit <- lm(inv ~ value + capital + factor(firm), data = greene)
  e <- residuals(fit)
  id <- as.integer(factor(greene$firm))
  # The slopes agree to the seven decimals printed. sigma_e agrees to seven
  # significant digits: Stata's file of Table 15.1 stores the series in single
  # precision, and from those values sigma_e is 77.1518067, against 77.1518092
  # from the double-precision values here.
  sigma_e <- sqrt(sum(e^2) / fit$df.residual)
  printed <- c(value = 0.1065863, capital = 0.3474248)
  if (any(abs(coef(fit)[names(printed)] - printed) > 5e-8) ||
        abs(sigma_e - 77.151807) > 5e-6) {
    stop("The fit does not reproduce the xtreg output printed in Baum (2001).")
  }
  example <- modified_wald(e, id)
  if (round(example[["statistic"]], 2) != 279.13) {
    stop("The statistic does not reproduce Baum (2001): ", example[["statistic"]], ".")
  }
  # The same statistic with s2 = sum(e^2) / (n - 1), the divisor xttest3 does
  # not use.
  s2_i <- tapply(e^2, id, mean)
  v_i <- tapply(e^2, id, function(x) sum((x - mean(x))^2) / (length(x) * (length(x) - 1)))
  other <- sum((s2_i - sum(e^2) / (length(e) - 1))^2 / v_i)
  # What panelbox 1.0.2 returns under the name ModifiedWaldTest:
  # sum_i T_i log(s2 / s2_i), with s2 = e'e / (n - N - k) and s2_i the sample
  # variance of unit i's residuals.
  panelbox <- sum(tabulate(id) * log(sum(e^2) / fit$df.residual / tapply(e, id, stats::var)))
  message(sprintf(paste(
    "Baum (2001) example: chi2(%d) = %.4f, published 279.13; %.4f with divisor n - 1;",
    "%.4f by the formula of panelbox's ModifiedWaldTest."
  ), example[["units"]], example[["statistic"]], other, panelbox))
} else {
  message("plm is not installed; the check against Baum (2001) is skipped.")
}

# --- designs ------------------------------------------------------------------------

draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    stop("unknown error distribution")
  )
}

# The within residuals of y on x1 and x2 with a fixed effect per unit; the same
# residuals as lm(y ~ x1 + x2 + factor(id)), computed without the dummies.
within_residuals <- function(y, X, id, periods) {
  demean <- function(v) v - (rowsum(v, id, reorder = TRUE)[, 1] / periods)[id]
  yd <- demean(y)
  Xd <- apply(X, 2L, demean)
  drop(yd - Xd %*% solve(crossprod(Xd), crossprod(Xd, yd)))
}

draw_panel <- function(periods, errors, tau = 0) {
  units <- length(periods)
  id <- rep.int(seq_len(units), periods)
  a <- rnorm(units)
  x1 <- 0.5 * a[id] + rnorm(length(id))
  x2 <- runif(length(id))
  sd_i <- exp(0.5 * (tau * rnorm(units) - tau^2 / 2))
  y <- a[id] + 0.5 * x1 - 0.25 * x2 + sd_i[id] * draw_errors(length(id), errors)
  list(y = y, X = cbind(x1, x2), id = id, periods = periods)
}

# The shortcut and the dummy-variable fit must give the same residuals.
local({
  set.seed(seed)
  p <- draw_panel(c(4L, 7L, 12L, 25L, 5L), "normal", tau = 0.5)
  d <- data.frame(y = p$y, x1 = p$X[, 1], x2 = p$X[, 2], id = factor(p$id))
  shortcut <- within_residuals(p$y, p$X, p$id, p$periods)
  gap <- max(abs(shortcut - residuals(lm(y ~ x1 + x2 + id, data = d))))
  if (gap > 1e-8) stop("The within residuals differ from the dummy-variable fit by ", gap, ".")
})

# --- scenarios ------------------------------------------------------------------------

shapes <- list(
  unbalanced_30 = rep(c(10L, 30L, 100L), each = 10L),
  unbalanced_100 = rep(c(5L, 10L, 20L, 30L), each = 25L)
)

scenario <- function(block, errors, units, periods, shape = "balanced", tau = 0) {
  expand.grid(
    block = block, errors = errors, units = units, periods = periods,
    shape = shape, tau = tau,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  scenario("size", c("normal", "t5"), c(10L, 30L, 100L), c(5L, 10L, 30L, 100L)),
  scenario("size, unbalanced", c("normal", "t5"), 30L, NA_integer_, shape = "unbalanced_30"),
  scenario("size, unbalanced", c("normal", "t5"), 100L, NA_integer_, shape = "unbalanced_100"),
  scenario("size, long panels", c("normal", "t5"), c(2L, 10L, 30L), c(300L, 1000L)),
  scenario("power", "normal", c(10L, 30L, 100L), c(10L, 100L), tau = c(0.25, 0.5))
)

periods_of <- function(s) {
  if (s$shape == "balanced") rep.int(s$periods, s$units) else shapes[[s$shape]]
}

# --- run ------------------------------------------------------------------------------

run_scenario <- function(i) {
  s <- scenarios[i, ]
  periods <- periods_of(s)
  set.seed(seed + i)
  p <- vapply(seq_len(n_mc), function(r) {
    d <- draw_panel(periods, s$errors, s$tau)
    w <- modified_wald(within_residuals(d$y, d$X, d$id, d$periods), d$id)
    if (is.null(w)) return(c(NA_real_, NA_real_))
    c(
      stats::pchisq(w[["statistic"]], w[["units"]], lower.tail = FALSE),
      stats::pchisq(w[["statistic"]], w[["units"]] - 1, lower.tail = FALSE)
    )
  }, numeric(2))
  rownames(p) <- c("modified_wald", "modified_wald_n_minus_1")

  rows <- lapply(rownames(p), function(test) {
    ok <- p[test, ][!is.na(p[test, ])]
    rate <- if (length(ok)) mean(ok < alpha) else NA_real_
    data.frame(
      block = s$block,
      test = test,
      errors = s$errors,
      units = s$units,
      periods = if (s$shape == "balanced") as.character(s$periods) else
        paste(unique(periods), collapse = ", "),
      mean_periods = mean(periods),
      shape = s$shape,
      tau = s$tau,
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
    "%-18s %-6s N=%3d T=%-14s tau=%.2f  df N %.4f  df N-1 %.4f",
    s$block, s$errors, s$units, out$periods[1], s$tau,
    out$rejection_rate[1], out$rejection_rate[2]
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
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) == 2L, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(which(!complete), collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- file.path("inst", "validation", "modified-wald-size-power.csv")
if (!dir.exists(dirname(path))) path <- "modified-wald-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
