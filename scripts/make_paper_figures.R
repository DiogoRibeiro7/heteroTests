#!/usr/bin/env Rscript
# Regenerates the figures used in paper/heteroTests.tex from real package runs:
#   * paper/scaling_plot.pdf  -- empirical runtime vs sample size (log-log)
#   * paper/power_curves.pdf  -- Monte-Carlo power vs effect size
# It also writes paper/power_summary.csv with the gamma = 1.0 power table.
#
# Usage: Rscript scripts/make_paper_figures.R [reps]

suppressWarnings(suppressMessages({
  library(heteroTests)
  library(ggplot2)
}))
options(warn = -1)

args <- commandArgs(trailingOnly = TRUE)
reps <- if (length(args) >= 1) as.integer(args[[1]]) else 300L
set.seed(20260622)
out <- "paper"

## ---------------------------------------------------------------------------
## Data-generating process: Var(e_i) = sigma^2 (1 + gamma * x_i), x_i in (0,1)
## ---------------------------------------------------------------------------
gen <- function(n, gamma) {
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = sqrt(1 + gamma * x))
  data.frame(y = y, x = x)
}

pval <- function(test, model, d) {
  tryCatch(
    switch(test,
      White    = performWhiteTest(model, d)$p.value,
      `Breusch-Pagan` = performBPTest(model, d)$p.value,
      Koenker  = performKoenkerTest(model, d)$p.value,
      `Goldfeld-Quandt` = performGQTest(model, d, order_by = "x")$p.value,
      Harvey   = performHarveyTest(model, d)$p.value),
    error = function(e) NA_real_)
}

## ---------------------------------------------------------------------------
## Power simulation
## ---------------------------------------------------------------------------
tests  <- c("White", "Breusch-Pagan", "Koenker", "Goldfeld-Quandt")
gammas <- c(0, 1, 2, 3, 4)
ns     <- c(50, 100, 200, 500)

rows <- list()
for (n in ns) for (g in gammas) {
  rej <- setNames(numeric(length(tests)), tests)
  for (r in seq_len(reps)) {
    d <- gen(n, g)
    m <- lm(y ~ x, data = d)
    for (t in tests) {
      p <- pval(t, m, d)
      if (!is.na(p) && p < 0.05) rej[[t]] <- rej[[t]] + 1
    }
  }
  for (t in tests) {
    rows[[length(rows) + 1]] <- data.frame(
      test = t, n = n, gamma = g, power = 100 * rej[[t]] / reps)
  }
  cat(sprintf("power: n=%d gamma=%.2f done\n", n, g))
}
power_df <- do.call(rbind, rows)
power_df$n <- factor(power_df$n, levels = ns)

p_power <- ggplot(power_df, aes(gamma, power, colour = n, linetype = test)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.1) +
  scale_colour_grey(start = 0.75, end = 0.05) +
  labs(x = expression("Effect size " * gamma),
       y = "Power (%)", colour = "n", linetype = "Test") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")
ggsave(file.path(out, "power_curves.pdf"), p_power, width = 7, height = 4.3)
cat("wrote power_curves.pdf\n")

## gamma = 2.0 summary table (the four tests shown in the figure)
g1 <- 2.0
counts <- setNames(lapply(tests, function(.) setNames(numeric(length(ns)), ns)), tests)
for (n in ns) {
  rej <- setNames(numeric(length(tests)), tests)
  for (r in seq_len(reps)) {
    d <- gen(n, g1); m <- lm(y ~ x, data = d)
    for (t in tests) { p <- pval(t, m, d); if (!is.na(p) && p < 0.05) rej[[t]] <- rej[[t]] + 1 }
  }
  for (t in tests) counts[[t]][[as.character(n)]] <- 100 * rej[[t]] / reps
}
power_summary <- do.call(rbind, lapply(tests, function(t)
  data.frame(test = t, t(counts[[t]]), check.names = FALSE)))
write.csv(power_summary, file.path(out, "power_summary.csv"), row.names = FALSE)
cat("wrote power_summary.csv\n")

## ---------------------------------------------------------------------------
## Scaling: runtime vs n (log-log)
## ---------------------------------------------------------------------------
scale_ns <- c(1000, 3000, 10000, 30000, 100000)
scale_tests <- c("White", "Breusch-Pagan", "Koenker")
srows <- list()
for (n in scale_ns) {
  d <- gen(n, 0.5); m <- lm(y ~ x, data = d)
  for (t in scale_tests) {
    tm <- median(replicate(3, system.time(pval(t, m, d))[["elapsed"]]))
    srows[[length(srows) + 1]] <- data.frame(test = t, n = n, seconds = tm)
  }
  cat(sprintf("scaling: n=%d done\n", n))
}
scale_df <- do.call(rbind, srows)
scale_df$seconds[scale_df$seconds <= 0] <- 1e-4   # guard log(0) on fast cells

p_scale <- ggplot(scale_df, aes(n, seconds, colour = test, shape = test)) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  scale_x_log10() + scale_y_log10() +
  scale_colour_grey(start = 0.6, end = 0.05) +
  labs(x = "Sample size (log scale)", y = "Elapsed time, s (log scale)",
       colour = "Test", shape = "Test") +
  theme_minimal(base_size = 11)
ggsave(file.path(out, "scaling_plot.pdf"), p_scale, width = 7, height = 4.3)
cat("wrote scaling_plot.pdf\n")
cat("done\n")
