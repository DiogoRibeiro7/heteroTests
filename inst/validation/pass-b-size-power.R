# Pass B statistical validation: group-variance diagnostics ------------------
#
# Run from the package root after installing the package and all Suggests:
#   Rscript inst/validation/pass-b-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000.

suppressPackageStartupMessages(library(heteroTests))

n_mc <- as.integer(Sys.getenv("N_MC", unset = "5000"))
if (!is.finite(n_mc) || n_mc < 100L) {
  stop("N_MC must be an integer >= 100.")
}

alpha <- 0.05
base_seed <- 27000L

methods <- c(
  "Levene",
  "Brown-Forsythe",
  "Bartlett",
  "Fligner-Killeen",
  "Hartley Fmax",
  "O'Brien",
  "Modified Bartlett alias"
)

run_tests <- function(data) {
  model <- lm(y ~ x, data = data)
  c(
    "Levene" = performLeveneTest(model, data, "g")$p.value,
    "Brown-Forsythe" = performBrownForsytheTest(model, data, "g")$p.value,
    "Bartlett" = suppressWarnings(performBartlettTest(model, data, "g")$p.value),
    "Fligner-Killeen" = performFlignerKilleenTest(model, data, "g")$p.value,
    "Hartley Fmax" = suppressWarnings(performHartleyFmaxTest(model, data, "g")$p.value),
    "O'Brien" = performOBrienTest(model, data, "g")$p.value,
    "Modified Bartlett alias" = suppressWarnings(performModifiedBartlettTest(model, data, "g")$p.value)
  )
}

simulate_design <- function(seed, n_per_group, scales, error = c("normal", "t5")) {
  error <- match.arg(error)
  set.seed(seed)
  k <- length(scales)
  n <- k * n_per_group
  g <- factor(rep(letters[seq_len(k)], each = n_per_group))
  x <- rnorm(n)
  z <- if (error == "normal") rnorm(n) else rt(n, df = 5) / sqrt(5 / 3)
  y <- 1 + 0.8 * x + z * rep(scales, each = n_per_group)
  data.frame(y = y, x = x, g = g)
}

scenarios <- list(
  gaussian_null_n30 = list(n = 30L, scales = c(1, 1, 1), error = "normal"),
  gaussian_null_n15 = list(n = 15L, scales = c(1, 1, 1), error = "normal"),
  t5_null_n30 = list(n = 30L, scales = c(1, 1, 1), error = "t5"),
  moderate_hetero = list(n = 30L, scales = c(1, 1.35, 1.8), error = "normal"),
  strong_hetero = list(n = 30L, scales = c(1, 1.5, 2.25), error = "normal")
)

rows <- list()
row_id <- 1L

for (scenario_name in names(scenarios)) {
  cfg <- scenarios[[scenario_name]]
  rejected <- setNames(integer(length(methods)), methods)
  failures <- setNames(integer(length(methods)), methods)

  for (b in seq_len(n_mc)) {
    d <- simulate_design(
      seed = base_seed + row_id * 100000L + b,
      n_per_group = cfg$n,
      scales = cfg$scales,
      error = cfg$error
    )
    p <- tryCatch(run_tests(d), error = function(e) rep(NA_real_, length(methods)))
    names(p) <- methods
    failures <- failures + as.integer(!is.finite(p))
    rejected <- rejected + as.integer(is.finite(p) & p < alpha)
  }

  effective <- n_mc - failures
  rate <- rejected / effective
  mc_se <- sqrt(rate * (1 - rate) / effective)

  rows[[row_id]] <- data.frame(
    scenario = scenario_name,
    method = methods,
    n_per_group = cfg$n,
    error = cfg$error,
    scale_pattern = paste(cfg$scales, collapse = ":"),
    replications = n_mc,
    effective_replications = effective,
    rejection_rate = rate,
    mc_se = mc_se,
    failures = failures,
    stringsAsFactors = FALSE
  )
  row_id <- row_id + 1L
}

results <- do.call(rbind, rows)
out <- file.path("inst", "validation", "pass-b-size-power.csv")
utils::write.csv(results, out, row.names = FALSE)
print(results, row.names = FALSE)
cat("\nWrote ", out, "\n", sep = "")
