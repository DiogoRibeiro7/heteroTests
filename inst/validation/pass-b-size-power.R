# Pass B statistical validation: group-variance diagnostics ------------------
#
# Run from the package root:
#   Rscript inst/validation/pass-b-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000.

is_source_checkout <- file.exists("DESCRIPTION") &&
  any(grepl(
    "Package: heteroTests",
    readLines("DESCRIPTION", warn = FALSE),
    fixed = TRUE
  ))

if (requireNamespace("pkgload", quietly = TRUE) && is_source_checkout) {
  suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(heteroTests))
}

get_n_mc <- function() {
  n_mc <- as.integer(Sys.getenv("N_MC", unset = "5000"))
  if (!is.finite(n_mc) || n_mc < 100L) {
    stop("N_MC must be an integer >= 100.")
  }
  n_mc
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
  calls <- list(
    "Levene" = function() performLeveneTest(model, data, "g")$p.value,
    "Brown-Forsythe" = function() performBrownForsytheTest(model, data, "g")$p.value,
    "Bartlett" = function() suppressWarnings(performBartlettTest(model, data, "g")$p.value),
    "Fligner-Killeen" = function() performFlignerKilleenTest(model, data, "g")$p.value,
    "Hartley Fmax" = function() suppressWarnings(performHartleyFmaxTest(model, data, "g")$p.value),
    "O'Brien" = function() performOBrienTest(model, data, "g")$p.value,
    "Modified Bartlett alias" = function() suppressWarnings(performModifiedBartlettTest(model, data, "g")$p.value)
  )

  vapply(
    calls,
    function(run_one) tryCatch(
      as.numeric(run_one()),
      error = function(e) NA_real_
    ),
    numeric(1)
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

summarise_pass_b_counts <- function(rejected, failures, n_mc) {
  effective <- n_mc - failures
  if (any(effective <= 0L)) {
    failed_methods <- names(effective)[effective <= 0L]
    stop(
      sprintf(
        "Pass B produced no usable replications for: %s",
        paste(failed_methods, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  rate <- rejected / effective
  mc_se <- sqrt(rate * (1 - rate) / effective)
  list(effective = effective, rate = rate, mc_se = mc_se)
}

run_pass_b_validation <- function(n_mc = get_n_mc()) {
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
      p <- run_tests(d)
      failures <- failures + as.integer(!is.finite(p))
      rejected <- rejected + as.integer(is.finite(p) & p < alpha)
    }

    summary <- summarise_pass_b_counts(rejected, failures, n_mc)

    rows[[row_id]] <- data.frame(
      scenario = scenario_name,
      method = methods,
      n_per_group = cfg$n,
      error = cfg$error,
      scale_pattern = paste(cfg$scales, collapse = ":"),
      replications = n_mc,
      effective_replications = summary$effective,
      rejection_rate = summary$rate,
      mc_se = summary$mc_se,
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
  invisible(results)
}

if (sys.nframe() == 0L) {
  run_pass_b_validation()
}
