#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(ggplot2))

input <- file.path("paper", "generated", "simulation_results.csv")
if (!file.exists(input)) {
  stop("Missing simulation results: ", input, call. = FALSE)
}

results <- utils::read.csv(input, stringsAsFactors = FALSE)
required <- c(
  "scenario", "pattern", "error", "n", "strength", "test",
  "estimate", "mc_se", "lower", "upper", "n_valid",
  "replications", "seed"
)
missing <- setdiff(required, names(results))
if (length(missing) > 0L) {
  stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
}

output_dir <- file.path("paper", "generated")

size <- subset(results, scenario == "size")
size$estimate_pct <- 100 * size$estimate
size$lower_pct <- 100 * size$lower
size$upper_pct <- 100 * size$upper

size_table <- size[, c(
  "error", "n", "test", "estimate_pct", "lower_pct", "upper_pct", "n_valid"
)]
names(size_table) <- c(
  "Error", "n", "Test", "Rejection (%)", "MC lower (%)",
  "MC upper (%)", "Valid replications"
)
utils::write.csv(
  size_table,
  file.path(output_dir, "simulation_size_table.csv"),
  row.names = FALSE
)

power <- subset(results, scenario == "power")
power$pattern <- factor(
  power$pattern,
  levels = c("linear", "step", "u_shape"),
  labels = c("Linear", "Step", "U-shaped")
)
power$n <- factor(power$n, levels = sort(unique(power$n)))

p <- ggplot(
  power,
  aes(
    x = strength,
    y = estimate,
    linetype = test,
    group = interaction(test, n)
  )
) +
  geom_line(aes(alpha = n), linewidth = 0.8) +
  geom_point(aes(shape = n), size = 1.6) +
  facet_wrap(~pattern, nrow = 1) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  scale_x_continuous(breaks = sort(unique(power$strength))) +
  labs(
    x = "Heteroscedastic effect strength",
    y = "Empirical rejection rate",
    linetype = "Diagnostic",
    shape = "Sample size",
    alpha = "Sample size"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggsave(
  file.path(output_dir, "simulation_power.pdf"),
  p,
  width = 8.5,
  height = 3.8
)

# Compact machine-readable extracts for manuscript chunks.
size_key <- subset(size, error == "gaussian" & n == 100)
power_key <- subset(power, strength == 1)

utils::write.csv(
  size_key,
  file.path(output_dir, "simulation_size_n100.csv"),
  row.names = FALSE
)
utils::write.csv(
  power_key,
  file.path(output_dir, "simulation_power_full_strength.csv"),
  row.names = FALSE
)

message("Wrote manuscript simulation summaries to ", output_dir)
