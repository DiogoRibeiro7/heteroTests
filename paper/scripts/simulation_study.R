#!/usr/bin/env Rscript

# Reproducible Monte Carlo study for the R Journal article.
#
# Usage:
#   Rscript paper/scripts/simulation_study.R [replications]
#
# The default is intended for manuscript results. A smaller value can be used
# locally for a smoke test, but should not be committed as final evidence.

suppressPackageStartupMessages(library(heteroTests))

args <- commandArgs(trailingOnly = TRUE)
n_rep <- if (length(args) >= 1L) as.integer(args[[1L]]) else 2000L

if (length(n_rep) != 1L || is.na(n_rep) || n_rep < 100L) {
  stop("replications must be a single integer >= 100", call. = FALSE)
}

seed <- 20261007L
alpha <- 0.05
sample_sizes <- c(50L, 100L, 250L)
strengths <- c(0.25, 0.50, 0.75, 1.00)

tests <- c(
  white = "White",
  breusch_pagan = "Breusch-Pagan",
  koenker = "Koenker",
  goldfeld_quandt = "Goldfeld-Quandt"
)

error_generators <- list(
  gaussian = function(n) stats::rnorm(n),
  t5 = function(n) {
    # Standardise t(5) to unit variance.
    stats::rt(n, df = 5) / sqrt(5 / 3)
  }
)

variance_patterns <- list(
  linear = function(x) 0.5 + x,
  step = function(x) ifelse(x < 0.5, 0.5, 1.5),
  u_shape = function(x) 0.35 + 2 * (x - 0.5)^2
)

normalise_profile <- function(values) {
  values <- as.numeric(values)
  values / sqrt(mean(values^2))
}

blend_profile <- function(x, pattern, strength) {
  target <- normalise_profile(pattern(x))
  raw <- (1 - strength) + strength * target
  normalise_profile(raw)
}

run_test <- function(name, model, data) {
  tryCatch(
    switch(
      name,
      white = performWhiteTest(model, data),
      breusch_pagan = performBPTest(model, data),
      koenker = performKoenkerTest(model, data),
      goldfeld_quandt = performGQTest(
        model,
        data,
        order_by = "x",
        alternative = "two.sided"
      ),
      stop("Unknown test: ", name, call. = FALSE)
    )$p.value,
    error = function(e) NA_real_
  )
}

summarise_rejections <- function(rejections) {
  valid <- is.finite(rejections)
  n_valid <- sum(valid)
  if (n_valid == 0L) {
    return(c(
      estimate = NA_real_,
      mc_se = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      n_valid = 0
    ))
  }

  p_hat <- mean(rejections[valid])
  se <- sqrt(p_hat * (1 - p_hat) / n_valid)

  c(
    estimate = p_hat,
    mc_se = se,
    lower = max(0, p_hat - 1.96 * se),
    upper = min(1, p_hat + 1.96 * se),
    n_valid = n_valid
  )
}

set.seed(seed)

rows <- list()
row_id <- 0L

# Homoscedastic null: empirical size under two error distributions.
for (error_name in names(error_generators)) {
  error_fn <- error_generators[[error_name]]

  for (n in sample_sizes) {
    reject <- matrix(
      NA,
      nrow = n_rep,
      ncol = length(tests),
      dimnames = list(NULL, names(tests))
    )

    for (b in seq_len(n_rep)) {
      x <- stats::runif(n)
      eps <- error_fn(n)
      data <- data.frame(x = x, y = 1 + 2 * x + eps)
      model <- stats::lm(y ~ x, data = data)

      for (test_name in names(tests)) {
        p <- run_test(test_name, model, data)
        reject[b, test_name] <- is.finite(p) && p < alpha
      }
    }

    for (test_name in names(tests)) {
      s <- summarise_rejections(reject[, test_name])
      row_id <- row_id + 1L
      rows[[row_id]] <- data.frame(
        scenario = "size",
        pattern = "constant",
        error = error_name,
        n = n,
        strength = 0,
        test = tests[[test_name]],
        estimate = s[["estimate"]],
        mc_se = s[["mc_se"]],
        lower = s[["lower"]],
        upper = s[["upper"]],
        n_valid = as.integer(s[["n_valid"]]),
        replications = n_rep,
        seed = seed,
        stringsAsFactors = FALSE
      )
    }
  }
}

# Power: Gaussian errors, multiple variance patterns and effect strengths.
for (pattern_name in names(variance_patterns)) {
  pattern_fn <- variance_patterns[[pattern_name]]

  for (n in sample_sizes) {
    for (strength in strengths) {
      reject <- matrix(
        NA,
        nrow = n_rep,
        ncol = length(tests),
        dimnames = list(NULL, names(tests))
      )

      for (b in seq_len(n_rep)) {
        x <- stats::runif(n)
        sigma <- blend_profile(x, pattern_fn, strength)
        eps <- stats::rnorm(n)
        data <- data.frame(x = x, y = 1 + 2 * x + sigma * eps)
        model <- stats::lm(y ~ x, data = data)

        for (test_name in names(tests)) {
          p <- run_test(test_name, model, data)
          reject[b, test_name] <- is.finite(p) && p < alpha
        }
      }

      for (test_name in names(tests)) {
        s <- summarise_rejections(reject[, test_name])
        row_id <- row_id + 1L
        rows[[row_id]] <- data.frame(
          scenario = "power",
          pattern = pattern_name,
          error = "gaussian",
          n = n,
          strength = strength,
          test = tests[[test_name]],
          estimate = s[["estimate"]],
          mc_se = s[["mc_se"]],
          lower = s[["lower"]],
          upper = s[["upper"]],
          n_valid = as.integer(s[["n_valid"]]),
          replications = n_rep,
          seed = seed,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

results <- do.call(rbind, rows)
results <- results[order(
  results$scenario,
  results$pattern,
  results$error,
  results$n,
  results$strength,
  results$test
), ]

output_dir <- file.path("paper", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

utils::write.csv(
  results,
  file.path(output_dir, "simulation_results.csv"),
  row.names = FALSE
)

metadata <- c(
  sprintf("seed: %d", seed),
  sprintf("replications: %d", n_rep),
  sprintf("alpha: %.3f", alpha),
  sprintf("sample_sizes: %s", paste(sample_sizes, collapse = ",")),
  sprintf("strengths: %s", paste(strengths, collapse = ",")),
  "size_errors: gaussian,t5",
  "power_errors: gaussian",
  "patterns: linear,step,u_shape"
)
writeLines(metadata, file.path(output_dir, "simulation_metadata.txt"))

message("Wrote ", nrow(results), " rows to paper/generated/simulation_results.csv")
