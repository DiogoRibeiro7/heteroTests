# Why the prototype of the Dette-Neumeyer-Van Keilegom test is conservative ----
#
# Run from the package root:
#   Rscript inst/validation/variance-form-dnvk-bootstrap-check.R
#
# N_MC sets the replications per cell (default 1000) and N_BOOT the bootstrap
# samples (default 199). MC_CORES runs the cells in parallel through
# parallel::mclapply(). Each cell has its own seed, so the results do not
# depend on the number of cores. SCENARIOS, for example "1:4", runs a subset of
# the cells and CSV names the file it writes, so that disjoint subsets can run
# as separate processes where mclapply() cannot fork (Windows); the rows of the
# subsets, put back in the order of the `scenario` column, are those of one
# full run. The script does not load the package.
#
# In variance-form-dnvk-size-power.R the prototype rejects a true variance
# function too seldom when that function is not constant. This script asks
# whether the cause is the estimate of the variance function that generates
# the bootstrap data. On each sample it computes the Cramer-von Mises statistic
# once and bootstraps it twice, with the same random numbers:
#
#   - "fitted": as the manuscript does, Y* = m-hat + sigma_theta-hat eps*, with
#     theta-hat the least-squares fit (2.11);
#   - "true shape": Y* = m-hat + s sigma_0 eps*, with sigma_0 the true
#     standard deviation function and s the constant that gives it the mean
#     fitted variance, so that only the shape differs from "fitted". This is
#     not available in practice; it isolates the effect of theta-hat.
#
# The designs are the Gaussian null cells of block 1 of
# variance-form-dnvk-size-power.R at 150 and 400 observations. The CSV also
# records the mean of the fitted slope of the variance function (b in exp(a +
# b x), or in a x^b) beside its true value, the mean observed statistic, and
# the mean of the bootstrap statistics under each scheme.

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

alpha <- 0.05
n_mc <- get_count("N_MC", "1000", 20L)
n_boot <- get_count("N_BOOT", "199", 19L)
n_cores <- get_count("MC_CORES", "1", 1L)
seed <- 20261103L

prototype <- file.path("inst", "validation", "variance-form-dnvk-prototype.R")
if (!file.exists(prototype)) prototype <- "variance-form-dnvk-prototype.R"
source(prototype)

TRUTHS <- list(
  constant    = list(sd = function(x) rep(1, length(x)), label = "sigma^2 = 1"),
  exponential = list(sd = function(x) exp(0.3 * x), label = "sigma^2 = exp(0.6 x)"),
  power       = list(sd = function(x) x^1.5, label = "sigma^2 = x^3")
)
# The slope of the variance function each truth implies under each form.
TRUE_SLOPE <- list(
  exponential = c(constant = 0, exponential = 0.6),
  power = c(constant = 0, power = 3)
)

scenarios <- rbind(
  expand.grid(form = "exponential", truth = c("constant", "exponential"), n = c(150L, 400L),
              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE),
  expand.grid(form = "power", truth = c("constant", "power"), n = c(150L, 400L),
              KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
)

# Bootstrap statistics generated with the standard deviations `sd_gen`, from
# the same random numbers whatever `sd_gen` is.
bootstrap_cm <- function(obs, x, mean_fit, W, form, sd_gen, state) {
  assign(".Random.seed", state, envir = globalenv())
  n <- length(x)
  vapply(seq_len(n_boot), function(b) {
    estar <- obs$eps[sample.int(n, n, replace = TRUE)] + 0.2 * stats::rnorm(n)
    sb <- dnvk_statistic(obs$fitted + sd_gen * estar, x, mean_fit, W, form)
    if (is.null(sb)) NA_real_ else sb$cm
  }, numeric(1))
}

run_scenario <- function(i) {
  s <- scenarios[i, ]
  form <- dnvk_forms[[s$form]]
  sd_fun <- TRUTHS[[s$truth]]$sd
  set.seed(seed + i)
  draws <- vapply(seq_len(n_mc), function(rep) {
    x <- stats::runif(s$n, 1, 5)
    z <- stats::runif(s$n, 1, 5)
    y <- 1 + 2 * x + 0.5 * z + sd_fun(x) * stats::rnorm(s$n)
    mean_fit <- dnvk_mean_lm(cbind(1, x, z))
    W <- dnvk_weights(x, dnvk_bandwidth(x))
    obs <- dnvk_statistic(y, x, mean_fit, W, form)
    if (is.null(obs)) return(rep(NA_real_, 6L))
    true_sd <- sd_fun(x) * sqrt(mean(obs$sd_theta^2) / mean(sd_fun(x)^2))
    state <- get(".Random.seed", envir = globalenv())
    fitted <- bootstrap_cm(obs, x, mean_fit, W, form, obs$sd_theta, state)
    true_shape <- bootstrap_cm(obs, x, mean_fit, W, form, true_sd, state)
    c(
      dnvk_p_value(obs$cm, fitted), dnvk_p_value(obs$cm, true_shape),
      obs$theta[2L], obs$cm, mean(fitted, na.rm = TRUE), mean(true_shape, na.rm = TRUE)
    )
  }, numeric(6))

  ok <- !is.na(draws[1L, ])
  rows <- lapply(c(fitted = 1L, `true shape` = 2L), function(j) {
    p <- draws[j, ok]
    rate <- mean(p <= alpha)
    data.frame(
      scenario = i,
      form = s$form,
      truth = s$truth,
      truth_label = TRUTHS[[s$truth]]$label,
      n = s$n,
      bootstrap_data = if (j == 1L) "fitted" else "true shape",
      replications = n_mc,
      bootstrap = n_boot,
      rejection_rate = rate,
      mc_se = sqrt(rate * (1 - rate) / sum(ok)),
      effective = sum(ok),
      failures = sum(!ok),
      true_slope = TRUE_SLOPE[[s$form]][[s$truth]],
      mean_slope = mean(draws[3L, ok]),
      mean_statistic = mean(draws[4L, ok]),
      mean_bootstrap_statistic = mean(draws[4L + j, ok]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-11s <- %-11s n=%3d  fitted %.3f  true shape %.3f  slope %.3f (%.1f)  T %.4f  T* %.4f / %.4f",
    s$form, s$truth, s$n, out$rejection_rate[1], out$rejection_rate[2], out$mean_slope[1],
    out$true_slope[1], out$mean_statistic[1], out$mean_bootstrap_statistic[1],
    out$mean_bootstrap_statistic[2]
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
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) == 2L, logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(index[!complete], collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- Sys.getenv(
  "CSV",
  unset = file.path("inst", "validation", "variance-form-dnvk-bootstrap-check.csv")
)
if (!dir.exists(dirname(path))) path <- "variance-form-dnvk-bootstrap-check.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
