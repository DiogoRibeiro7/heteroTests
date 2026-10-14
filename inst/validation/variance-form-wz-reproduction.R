# Table 1 of Wang and Zhou (2007), reproduced --------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/variance-form-wz-reproduction.R
#
# N_MC sets the replications per cell (default 2000; the paper used 400).
# MC_CORES runs the cells in parallel through parallel::mclapply(). Each cell
# has its own seed, so the results do not depend on the number of cores.
# SCENARIOS, for example "1:6", runs a subset of the cells and CSV names the
# file it writes, so that disjoint subsets can run as separate processes where
# mclapply() cannot fork (Windows); because each cell has its own seed, the
# rows of the subsets, put back in the order of the `scenario` column, are
# those of one full run. The script does not load the package.
#
# This is the reference check of this stage 1: no implementation of the test
# exists to compare with. The table is that of the authors' working paper (UW
# Biostatistics Working Paper 299, September 2006, revised after review); the
# published article could not be obtained, and its table may differ. See
# variance-form-wz-design.md.
#
# Simulation study 1 of the paper: Y_i = 1 + 2 x_i + sigma_i eps_i, x uniform on
# (0, 1), standard normal errors, and the null hypothesis that the variance is
# log-linear, sigma^2 = exp(theta_0 + theta_1 x). Three standard deviations
# (10):
#   (1) sigma = exp(-0.5 - 0.25 x)                          the null hypothesis
#   (2) sigma = exp(-0.5 - 0.25 x - 6 (x - 0.5)^2)
#   (3) sigma = exp(-0.5 - 0.25 x - 1.5 sin(2 pi x)^2)
# at 50 and 100 observations, with h = 0.10, 0.15, 0.20 and 0.25, the
# prototype of variance-form-wz-prototype.R with B = 200 bootstrap samples as
# in the paper, and the 5% level. The four bandwidths are evaluated on the same
# samples and the same bootstrap samples, since the fits do not depend on h.
# The Wald test of the paper's table, of theta_2 = 0 in a quadratic
# log-variance under normal errors, runs on the same samples.
#
# The paper does not name its kernel or say which quantile of the bootstrap
# statistics it uses. The script runs the Gaussian and the Epanechnikov
# kernels, and uses the package's p-value, (1 + #{T* >= T}) / (B + 1).

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

alpha <- 0.05
n_mc <- get_count("N_MC", "2000", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
n_boot <- 200L
seed <- 20261201L
bandwidths <- c(0.10, 0.15, 0.20, 0.25)

prototype <- file.path("inst", "validation", "variance-form-wz-prototype.R")
if (!file.exists(prototype)) prototype <- "variance-form-wz-prototype.R"
source(prototype)

SD <- list(
  "(1)" = function(x) exp(-0.5 - 0.25 * x),
  "(2)" = function(x) exp(-0.5 - 0.25 * x - 6 * (x - 0.5)^2),
  "(3)" = function(x) exp(-0.5 - 0.25 * x - 1.5 * sin(2 * pi * x)^2)
)

# Table 1 of the working paper: the T_n test at h = 0.10, 0.15, 0.20, 0.25, and
# the Wald test, for n = 50 and n = 100.
PAPER_TN <- list(
  "(1)" = list("50" = c(0.048, 0.050, 0.048, 0.050), "100" = c(0.055, 0.053, 0.053, 0.053)),
  "(2)" = list("50" = c(0.633, 0.735, 0.780, 0.810), "100" = c(0.943, 0.970, 0.983, 0.988)),
  "(3)" = list("50" = c(0.658, 0.690, 0.648, 0.513), "100" = c(0.973, 0.980, 0.963, 0.903))
)
PAPER_WALD <- list(
  "(1)" = c("50" = 0.103, "100" = 0.070),
  "(2)" = c("50" = 0.995, "100" = 1.000),
  "(3)" = c("50" = 0.140, "100" = 0.165)
)

scenarios <- expand.grid(
  model = names(SD), n = c(50L, 100L), kernel = c("gaussian", "epanechnikov"),
  KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
)

run_scenario <- function(i) {
  s <- scenarios[i, ]
  set.seed(seed + i)
  draws <- vapply(seq_len(n_mc), function(rep) {
    x <- stats::runif(s$n)
    y <- 1 + 2 * x + SD[[s$model]](x) * stats::rnorm(s$n)
    X <- cbind(1, x)
    wald <- wz_wald_quadratic(y, X, x)
    Kmats <- lapply(bandwidths, function(h) wz_kernel_matrix(x, h, s$kernel))
    bt <- wz_bootstrap(y, X, cbind(1, x), Kmats, B = n_boot)
    if (is.null(bt)) return(c(rep(NA_real_, length(bandwidths)), wald, NA_real_))
    c(
      vapply(seq_along(bandwidths), function(j) wz_p_value(bt$statistic[j], bt$boot[, j]),
             numeric(1)),
      wald, bt$failed
    )
  }, numeric(length(bandwidths) + 2L))

  nb <- length(bandwidths)
  ok <- !is.na(draws[1L, ])
  row <- function(test, h, reject, keep, paper) {
    rate <- mean(reject[keep])
    data.frame(
      scenario = i, model = s$model, n = s$n, kernel = s$kernel, test = test, h = h,
      replications = n_mc, bootstrap = if (test == "wald") NA_integer_ else n_boot,
      rejection_rate = rate, mc_se = sqrt(rate * (1 - rate) / sum(keep)),
      effective = sum(keep), failures = sum(!keep),
      bootstrap_failures = if (test == "wald") NA_real_ else sum(draws[nb + 2L, ok]),
      paper_rate = paper, stringsAsFactors = FALSE
    )
  }
  rows <- lapply(seq_len(nb), function(j) {
    row("wang_zhou", bandwidths[j], draws[j, ] <= alpha, ok,
        PAPER_TN[[s$model]][[as.character(s$n)]][j])
  })
  wald_ok <- !is.na(draws[nb + 1L, ])
  rows[[nb + 1L]] <- row("wald", NA_real_, draws[nb + 1L, ] > stats::qchisq(1 - alpha, 1),
                         wald_ok, PAPER_WALD[[s$model]][[as.character(s$n)]])
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-12s %s n=%3d  Tn %s  Wald %.3f (%.3f)", s$kernel, s$model, s$n,
    paste(sprintf("%.3f (%.3f)", out$rejection_rate[1:nb], out$paper_rate[1:nb]), collapse = " "),
    out$rejection_rate[nb + 1L], out$paper_rate[nb + 1L]
  ))
  out
}

index <- seq_len(nrow(scenarios))
chosen <- Sys.getenv("SCENARIOS", unset = "")
if (nzchar(chosen)) {
  index <- intersect(index, eval(parse(text = chosen)))
}
results <- if (n_cores > 1L) {
  parallel::mclapply(index, run_scenario, mc.cores = n_cores, mc.preschedule = FALSE)
} else {
  lapply(index, run_scenario)
}

# A worker that was killed returns NULL and one that failed returns the error,
# and rbind() would drop the first without a word.
complete <- vapply(results, function(r) {
  is.data.frame(r) && nrow(r) == length(bandwidths) + 1L
}, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(index[!complete], collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- Sys.getenv(
  "CSV",
  unset = file.path("inst", "validation", "variance-form-wz-reproduction.csv")
)
if (!dir.exists(dirname(path))) path <- "variance-form-wz-reproduction.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
