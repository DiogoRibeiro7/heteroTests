#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(heteroTests)
})

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args

seed <- 20261007L
set.seed(seed)

output_dir <- file.path("paper", "generated")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

sample_sizes <- if (quick) c(100L, 1000L, 10000L) else c(100L, 1000L, 10000L, 100000L)
replicates <- if (quick) c(3L, 2L, 1L) else c(5L, 5L, 3L, 2L)

message("Running reference-package benchmark...")
reference <- run_benchmark_suite(
  sample_sizes = sample_sizes,
  tests = c("breusch_pagan", "koenker", "ncv"),
  replicates = replicates,
  hetero_patterns = c("none", "linear", "group"),
  hetero_strength = 1,
  baseline_packages = c("lmtest", "car"),
  seed = seed,
  n_predictors = 4L,
  profile_memory = requireNamespace("bench", quietly = TRUE),
  progress = FALSE
)
reference_report <- generate_benchmark_report(reference, accuracy_tolerance = 1e-6)

utils::write.csv(
  reference$performance,
  file.path(output_dir, "performance_reference_raw.csv"),
  row.names = FALSE
)
utils::write.csv(
  reference$accuracy,
  file.path(output_dir, "performance_reference_accuracy_raw.csv"),
  row.names = FALSE
)
utils::write.csv(
  reference_report$speed,
  file.path(output_dir, "performance_reference_speed.csv"),
  row.names = FALSE
)
utils::write.csv(
  reference_report$memory,
  file.path(output_dir, "performance_reference_memory.csv"),
  row.names = FALSE
)
utils::write.csv(
  reference_report$accuracy,
  file.path(output_dir, "performance_reference_accuracy.csv"),
  row.names = FALSE
)

make_case <- function(n, seed_offset) {
  set.seed(seed + seed_offset)
  x1 <- stats::rnorm(n)
  x2 <- stats::rnorm(n)
  y <- 1 + 1.5 * x1 - 0.7 * x2 + stats::rnorm(n, sd = 0.8 + 0.4 * abs(x1))
  data <- data.frame(y = y, x1 = x1, x2 = x2)
  list(data = data, model = stats::lm(y ~ x1 + x2, data = data))
}

run_once <- function(fun) {
  if (requireNamespace("bench", quietly = TRUE)) {
    result <- NULL
    b <- bench::mark(
      result <- fun(),
      iterations = 1,
      memory = TRUE,
      check = FALSE
    )
    list(
      result = result,
      seconds = as.numeric(b$median / bench::as_bench_time("1s")),
      memory_mb = as.numeric(b$mem_alloc) / (1024^2)
    )
  } else {
    start <- proc.time()
    result <- fun()
    elapsed <- proc.time() - start
    list(
      result = result,
      seconds = as.numeric(elapsed[["elapsed"]]),
      memory_mb = NA_real_
    )
  }
}

stream_tests <- list(
  white = list(
    exact = function(m, d) performWhiteTest(m, d),
    stream = function(m, d) performWhiteTestStreaming(
      m, d, chunk_size = 5000L, progress = FALSE
    )
  ),
  breusch_pagan = list(
    exact = function(m, d) performBPTest(m, d),
    stream = function(m, d) performBPTestStreaming(
      m, d, chunk_size = 5000L, progress = FALSE
    )
  ),
  koenker = list(
    exact = function(m, d) performKoenkerTest(m, d),
    stream = function(m, d) performKoenkerTestStreaming(
      m, d, chunk_size = 5000L, progress = FALSE
    )
  )
)

stream_sizes <- if (quick) c(1000L, 10000L) else c(1000L, 10000L, 100000L, 250000L)
stream_reps <- if (quick) 1L else 3L

stream_rows <- list()
row_id <- 0L

for (n in stream_sizes) {
  for (rep in seq_len(stream_reps)) {
    case <- make_case(n, seed_offset = n + rep)

    for (test_name in names(stream_tests)) {
      exact <- run_once(function() stream_tests[[test_name]]$exact(case$model, case$data))
      streamed <- run_once(function() stream_tests[[test_name]]$stream(case$model, case$data))

      stat_exact <- unname(exact$result$statistic[[1]])
      stat_stream <- unname(streamed$result$statistic[[1]])
      p_exact <- exact$result$p.value
      p_stream <- streamed$result$p.value

      for (implementation in c("exact", "streaming")) {
        obj <- if (implementation == "exact") exact else streamed
        row_id <- row_id + 1L
        stream_rows[[row_id]] <- data.frame(
          test = test_name,
          sample_size = n,
          replicate = rep,
          implementation = implementation,
          seconds = obj$seconds,
          memory_mb = obj$memory_mb,
          statistic = if (implementation == "exact") stat_exact else stat_stream,
          p_value = if (implementation == "exact") p_exact else p_stream,
          statistic_diff = abs(stat_exact - stat_stream),
          p_value_diff = abs(p_exact - p_stream),
          seed = seed,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}

stream_df <- do.call(rbind, stream_rows)
utils::write.csv(
  stream_df,
  file.path(output_dir, "performance_streaming_raw.csv"),
  row.names = FALSE
)

aggregate_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0L) NA_real_ else stats::median(x)
}

stream_summary <- stats::aggregate(
  cbind(seconds, memory_mb, statistic_diff, p_value_diff) ~
    test + sample_size + implementation,
  data = stream_df,
  FUN = aggregate_median
)

utils::write.csv(
  stream_summary,
  file.path(output_dir, "performance_streaming_summary.csv"),
  row.names = FALSE
)

metadata <- c(
  sprintf("seed: %d", seed),
  sprintf("reference_sample_sizes: %s", paste(sample_sizes, collapse = ",")),
  sprintf("reference_replicates: %s", paste(replicates, collapse = ",")),
  sprintf("stream_sample_sizes: %s", paste(stream_sizes, collapse = ",")),
  sprintf("stream_replicates: %d", stream_reps),
  "stream_chunk_size: 5000",
  sprintf("bench_available: %s", requireNamespace("bench", quietly = TRUE))
)
writeLines(metadata, file.path(output_dir, "performance_metadata.txt"))

message("Performance study complete.")
