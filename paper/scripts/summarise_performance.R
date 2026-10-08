#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

output_dir <- file.path("paper", "generated")
ref_speed_path <- file.path(output_dir, "performance_reference_speed.csv")
ref_acc_path <- file.path(output_dir, "performance_reference_accuracy.csv")
stream_path <- file.path(output_dir, "performance_streaming_summary.csv")

required <- c(ref_speed_path, ref_acc_path, stream_path)
missing <- required[!file.exists(required)]
if (length(missing) > 0L) {
  stop("Missing benchmark outputs: ", paste(missing, collapse = ", "), call. = FALSE)
}

ref_speed <- utils::read.csv(ref_speed_path, stringsAsFactors = FALSE)
ref_acc <- utils::read.csv(ref_acc_path, stringsAsFactors = FALSE)
stream <- utils::read.csv(stream_path, stringsAsFactors = FALSE)

utils::write.csv(
  subset(ref_speed, package == "heteroTests"),
  file.path(output_dir, "performance_heterotests_speed.csv"),
  row.names = FALSE
)

utils::write.csv(
  ref_acc,
  file.path(output_dir, "performance_accuracy_table.csv"),
  row.names = FALSE
)

p <- ggplot(
  stream,
  aes(
    x = sample_size,
    y = seconds,
    linetype = implementation,
    shape = implementation
  )
) +
  geom_line() +
  geom_point(size = 1.8) +
  facet_wrap(~test, scales = "free_y") +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    x = "Sample size",
    y = "Median elapsed time (seconds)",
    linetype = "Implementation",
    shape = "Implementation"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(output_dir, "performance_streaming_runtime.pdf"),
  p,
  width = 7.5,
  height = 4.5
)

if (any(is.finite(stream$memory_mb))) {
  pm <- ggplot(
    subset(stream, is.finite(memory_mb)),
    aes(
      x = sample_size,
      y = memory_mb,
      linetype = implementation,
      shape = implementation
    )
  ) +
    geom_line() +
    geom_point(size = 1.8) +
    facet_wrap(~test, scales = "free_y") +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      x = "Sample size",
      y = "Median allocated memory (MB)",
      linetype = "Implementation",
      shape = "Implementation"
    ) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")

  ggsave(
    file.path(output_dir, "performance_streaming_memory.pdf"),
    pm,
    width = 7.5,
    height = 4.5
  )
}

message("Wrote manuscript performance summaries.")
