#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(heteroTests)
})

args <- commandArgs(trailingOnly = TRUE)
quick_mode <- "--quick" %in% args
profile_memory <- !("--no-memory" %in% args)

sample_sizes <- if (quick_mode) {
  c(100L, 1000L, 10000L)
} else {
  c(100L, 500L, 1000L, 10000L, 100000L, 1000000L)
}
replicates <- if (quick_mode) {
  c(3L, 2L, 1L)
} else {
  c(5L, 5L, 3L, 2L, 2L, 1L)
}

if (profile_memory && !requireNamespace("bench", quietly = TRUE)) {
  message("bench package not installed; disabling memory profiling.")
  profile_memory <- FALSE
}

baseline_packages <- c("lmtest", "car")
missing_baseline <- baseline_packages[!vapply(baseline_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_baseline) > 0L) {
  message("Missing baseline packages: ", paste(missing_baseline, collapse = ", "))
}

message("Running heteroTests benchmark suite...")
results <- heteroTests::run_benchmark_suite(
  sample_sizes = sample_sizes,
  replicates = replicates,
  hetero_patterns = c("none", "linear", "group"),
  hetero_strength = 1,
  baseline_packages = baseline_packages,
  seed = 2024L,
  n_predictors = 4L,
  profile_memory = profile_memory,
  progress = TRUE
)

report <- heteroTests::generate_benchmark_report(results, accuracy_tolerance = 1e-5)

output_dir <- file.path("inst", "benchmarks")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
write.csv(results$performance, file.path(output_dir, paste0("performance_", timestamp, ".csv")), row.names = FALSE)
write.csv(results$accuracy, file.path(output_dir, paste0("accuracy_raw_", timestamp, ".csv")), row.names = FALSE)
write.csv(report$speed, file.path(output_dir, paste0("speed_summary_", timestamp, ".csv")), row.names = FALSE)
write.csv(report$memory, file.path(output_dir, paste0("memory_summary_", timestamp, ".csv")), row.names = FALSE)
write.csv(report$accuracy, file.path(output_dir, paste0("accuracy_summary_", timestamp, ".csv")), row.names = FALSE)
write.csv(report$recommendations, file.path(output_dir, paste0("recommendations_", timestamp, ".csv")), row.names = FALSE)
write.csv(report$scalability, file.path(output_dir, paste0("scalability_", timestamp, ".csv")), row.names = FALSE)

message("Benchmark suite completed.")
message("Results written to ", normalizePath(output_dir, winslash = "/", mustWork = FALSE))
