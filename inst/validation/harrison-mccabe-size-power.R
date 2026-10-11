# The Harrison-McCabe test ------------------------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/harrison-mccabe-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000, where the 99% interval around a true 5% is
# [0.042, 0.058]. N_NULL, 100000 by default, is the number of samples in the
# first block. MC_CORES runs the scenarios in parallel. Each scenario has its
# own seed, so the results do not depend on the number of cores.
#
# performHarrisonMcCabeTest() orders the observations by a variable and takes
# the share of the residual sum of squares in the first half. Its p-value is
# exact under normal errors given the regressors. The script measures it in
# five blocks, with the Goldfeld-Quandt and Szroeter tests beside it on the
# same data and the same ordering. All three are one-sided tests of a variance
# that increases with x1, each with its default split. The Glejser test of
# |e| on x1 with robust = TRUE is run as well: it is the test the help page
# recommends when the errors may be heavy-tailed. It is two-sided, so its
# rejection rates under the alternatives are not compared with the others.
#
# Block 1, the null distribution. One design of each size of the
# cross-sectional design of pass-a-size-power.R, ordered by x1, and one with
# weights, held fixed while N_NULL normal error vectors are drawn. Each sample
# gets the p-value the test computes for it, and the rejection rates at 1%, 5%
# and 10% are recorded. The same samples are also referred to the null that
# lmtest::hmctest(simulate.p = TRUE) simulates, the residuals of a regression
# on a constant alone, computed here exactly rather than by simulation.
#
# Block 2, size. The cross-sectional design under its null, Gaussian errors,
# 40, 100 and 400 observations. The design is redrawn in every sample.
#
# Block 3, size under t5 errors scaled to unit variance. The null distribution
# assumes normal errors.
#
# Block 4, size after a weighted fit. The standard deviation is proportional
# to x1 and the model is fitted with the weights that undo it, so the null
# hypothesis is true of the weighted model. The tests read its Pearson
# residuals, and the Harrison-McCabe null distribution uses the weighted
# design.
#
# Block 5, power, against the two alternatives of pass-a-size-power.R:
# sigma_i^2 = exp(0.4 x1) and sigma_i^2 = 1 + 0.15 x1^2.

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
n_null <- get_count("N_NULL", "100000", 1000L)
n_cores <- get_count("MC_CORES", "1", 1L)
seed <- 20261015L
invisible(ht_set_log_level("SILENT"))

# The functions the test computes its p-value with.
hmc_spectrum <- heteroTests:::rhmc_spectrum
hmc_tails <- heteroTests:::rhmc_tails

# --- designs --------------------------------------------------------------------

draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    stop("unknown error distribution")
  )
}

# y = 1 + 2 x1 + 0.5 x2 + sigma_i e_i, with x1 uniform on (1, 5).
make_data <- function(n, errors, variance, gamma, weights) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  sd <- switch(variance,
    constant = if (weights == "exact") x1 else rep(1, n),
    exp = exp(0.5 * gamma * x1),
    quad = sqrt(1 + gamma * x1^2),
    stop("unknown variance function")
  )
  d <- data.frame(x1 = x1, x2 = x2, y = 1 + 2 * x1 + 0.5 * x2 + sd * draw_errors(n, errors))
  if (weights == "exact") {
    d$w <- 1 / x1^2
    model <- lm(y ~ x1 + x2, data = d, weights = w)
  } else {
    model <- lm(y ~ x1 + x2, data = d)
  }
  list(model = model, data = d)
}

# --- scenarios ------------------------------------------------------------------

scenario <- function(block, errors, n, weights = "none", variance = "constant", gamma = 0) {
  expand.grid(
    block = block, errors = errors, n = n, weights = weights, variance = variance,
    gamma = gamma, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
}

scenarios <- rbind(
  scenario("null distribution", "normal", c(40L, 100L, 400L)),
  scenario("null distribution", "normal", 100L, weights = "exact"),
  scenario("size", "normal", c(40L, 100L, 400L)),
  scenario("size", "t5", c(40L, 100L, 400L)),
  scenario("size, weighted fit", "normal", c(40L, 100L, 400L), weights = "exact"),
  scenario("power", "normal", c(40L, 100L), variance = "exp", gamma = 0.4),
  scenario("power", "normal", c(40L, 100L), variance = "quad", gamma = 0.15)
)

# --- block 1: the null distribution given the design ----------------------------

run_null_distribution <- function(s) {
  x1 <- runif(s$n, 1, 5)
  x2 <- rnorm(s$n)
  ord <- order(x1)
  design <- cbind(1, x1, x2)
  if (s$weights == "exact") design <- design / x1
  design <- design[ord, , drop = FALSE]
  m <- floor(s$n / 2)
  basis <- qr(design)
  exact <- hmc_spectrum(basis, m)
  regressor_free <- hmc_spectrum(qr(matrix(1, s$n, 1)), m)

  chunk <- 10000L
  p <- NULL
  for (start in seq(1L, n_null, by = chunk)) {
    size <- min(chunk, n_null - start + 1L)
    resid <- qr.resid(basis, matrix(rnorm(s$n * size), s$n, size))
    share <- colSums(resid[seq_len(m), , drop = FALSE]^2) / colSums(resid^2)
    tails <- vapply(share, function(b) hmc_tails(b, exact), numeric(2))
    p <- rbind(p, cbind(
      harrison_mccabe_less = tails["lower", ],
      harrison_mccabe_greater = tails["upper", ],
      harrison_mccabe_two_sided = pmin(1, 2 * pmin(tails["lower", ], tails["upper", ])),
      regressor_free_less = if (s$weights == "none") {
        vapply(share, function(b) hmc_tails(b, regressor_free)[["lower"]], numeric(1))
      } else {
        NA_real_
      }
    ))
  }

  rows <- list()
  for (test in colnames(p)) {
    ok <- p[, test][!is.na(p[, test])]
    if (!length(ok)) next
    for (level in c(0.01, 0.05, 0.10)) {
      rate <- mean(ok < level)
      rows[[length(rows) + 1L]] <- data.frame(
        block = s$block, test = test, errors = s$errors, n = s$n, weights = s$weights,
        variance = s$variance, gamma = s$gamma, level = level,
        replications = n_null, rejection_rate = rate,
        mc_se = sqrt(rate * (1 - rate) / length(ok)),
        effective = length(ok), failures = n_null - length(ok),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# --- blocks 2 to 5: size and power ----------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

run_monte_carlo <- function(s) {
  p <- vapply(seq_len(n_mc), function(rep) {
    o <- make_data(s$n, s$errors, s$variance, s$gamma, s$weights)
    c(
      harrison_mccabe = pval(performHarrisonMcCabeTest(o$model, o$data, "x1")),
      goldfeld_quandt = pval(performGQTest(o$model, o$data, "x1")),
      szroeter = pval(performSzroeterTest(o$model, o$data, "x1")),
      glejser_robust = pval(performGlejserTest(o$model, o$data, "x1", robust = TRUE))
    )
  }, numeric(4))

  rows <- lapply(rownames(p), function(test) {
    ok <- p[test, ][!is.na(p[test, ])]
    rate <- if (length(ok)) mean(ok < alpha) else NA_real_
    data.frame(
      block = s$block, test = test, errors = s$errors, n = s$n, weights = s$weights,
      variance = s$variance, gamma = s$gamma, level = alpha,
      replications = n_mc, rejection_rate = rate,
      mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
      effective = length(ok), failures = n_mc - length(ok),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

run_scenario <- function(i) {
  s <- scenarios[i, ]
  set.seed(seed + i)
  out <- if (s$block == "null distribution") run_null_distribution(s) else run_monte_carlo(s)
  shown <- out[out$level == alpha, , drop = FALSE]
  message(sprintf(
    "%-18s %-6s n=%3d %-5s %-8s gamma=%.2f  %s",
    s$block, s$errors, s$n, s$weights, s$variance, s$gamma,
    paste(sprintf("%s %.4f", shown$test, shown$rejection_rate), collapse = "  ")
  ))
  out
}

# --- run ------------------------------------------------------------------------

index <- seq_len(nrow(scenarios))
results <- if (n_cores > 1L) {
  parallel::mclapply(index, run_scenario, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(index, run_scenario)
}

# A worker that was killed returns NULL and one that failed returns the error,
# and rbind() would drop the first without a word.
expected_rows <- ifelse(scenarios$block == "null distribution",
                        ifelse(scenarios$weights == "none", 12L, 9L), 4L)
complete <- vapply(index, function(i) {
  is.data.frame(results[[i]]) && nrow(results[[i]]) == expected_rows[i]
}, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(which(!complete), collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- file.path("inst", "validation", "harrison-mccabe-size-power.csv")
if (!dir.exists(dirname(path))) path <- "harrison-mccabe-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
