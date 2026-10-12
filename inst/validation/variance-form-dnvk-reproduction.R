# Two tables of Dette, Neumeyer and Van Keilegom (2007), reproduced ------------
#
# Run from the package root:
#   Rscript inst/validation/variance-form-dnvk-reproduction.R
#
# N_MC sets the replications per cell (default 2000; the paper used 1000).
# MC_CORES runs the cells in parallel through parallel::mclapply(). Each cell
# has its own seed, so the results do not depend on the number of cores.
# SCENARIOS, for example "1:12", runs a subset of the cells and CSV names the
# file it writes, so that disjoint subsets can run as separate processes where
# mclapply() cannot fork (Windows); because each cell has its own seed, the
# rows of the subsets, put back in the order of the `scenario` column, are
# those of one full run.
#
# This is the reference check of stage 1: no implementation of the test exists
# to compare with. The tables are those of the authors' submitted manuscript
# (Dette and Van Keilegom, technical report of 9 September 2005, SFB 475 and
# IAP Statistics Network TR 0544); the published article could not be
# obtained, and its tables may differ. See variance-form-dnvk-design.md.
#
# The prototype in variance-form-dnvk-prototype.R is used as the paper
# describes the test, with no package code: Nadaraya-Watson estimates of the
# mean and the variance with h = 0.3, X uniform on [0, 1], standard normal
# errors, B = 100 bootstrap samples with v = 0.2, the Cramer-von Mises
# statistic, and the rejection rule (4.2), T > T*_(floor(B (1 - alpha))).
# With B = 100 that rule rejects an exactly calibrated statistic 4/101, 6/101
# and 11/101 of the time at 2.5%, 5% and 10% (0.040, 0.059, 0.109), because the
# observed statistic is the 101st member of an exchangeable set.
#
# Two details are not in the manuscript, and the script runs each choice:
#
#   - "as described": the Epanechnikov kernel, which has the compact support
#     its assumption (A1)(ii) asks for, though not the two derivatives, and
#     the residuals resampled as they are, as (4.1) says;
#   - "Gaussian kernel": the same with the Gaussian kernel;
#   - "unit-variance residuals": the residuals centred and scaled so that
#     eps* has variance 1. Only Table 4.5 has this row: for constant variance
#     the scale of eps* cannot change the statistic, and for 1 + theta x^2,
#     which is not closed under a change of scale, the bootstrap data satisfy
#     the null hypothesis only with this choice.
#
# The first is the reproduction; the other two say how much the unstated
# details move it.
#
# Table 4.1 (homoscedasticity, sigma = 0.5):
#   (I)   m(x) = 1 + sin(x)   sigma(x) = 0.5 exp(c x)
#   (II)  m(x) = 1 + x        sigma(x) = 0.5 (1 + c sin(10 x))^2
#   (III) m(x) = 1 + x        sigma(x) = 0.5 (1 + c x)^2
# Table 4.5 (H0: sigma^2(x) = 1 + theta x^2):
#   (VII) m(x) = 1 + x        sigma^2(x) = 1 + 3 x^2 + 2.5 c sin(2 pi x)
# The manuscript prints (VII) as sigma(x) = ...; only the variance reading
# makes c = 0 a null case, as the text says it is, so that reading is used.
# c = 0 is the null hypothesis in every model.

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

n_mc <- get_count("N_MC", "2000", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
n_boot <- 100L
seed <- 20261101L
levels <- c(0.025, 0.05, 0.10)

prototype <- file.path("inst", "validation", "variance-form-dnvk-prototype.R")
if (!file.exists(prototype)) prototype <- "variance-form-dnvk-prototype.R"
source(prototype)

# --- the paper's designs and its figures ------------------------------------------

MODELS <- list(
  I = list(
    mean = function(x) 1 + sin(x),
    sd = function(x, c) 0.5 * exp(c * x),
    form = "constant"
  ),
  II = list(
    mean = function(x) 1 + x,
    sd = function(x, c) 0.5 * (1 + c * sin(10 * x))^2,
    form = "constant"
  ),
  III = list(
    mean = function(x) 1 + x,
    sd = function(x, c) 0.5 * (1 + c * x)^2,
    form = "constant"
  ),
  VII = list(
    mean = function(x) 1 + x,
    sd = function(x, c) sqrt(1 + 3 * x^2 + 2.5 * c * sin(2 * pi * x)),
    form = "one_plus_theta_x2"
  )
)

# Tables 4.1 and 4.5 of the manuscript, h = 0.3: one row per model and c, the
# rates at 2.5%, 5% and 10% for n = 50, 100 and 200 in that order. The cell
# n = 200, c = 0 of (VII) reads 0.023, 0.095, 0.093 in the manuscript; its 5%
# entry is above its 10% entry, so it is a misprint, and it is kept as printed.
PAPER <- rbind(
  c("I", 0.0, 0.038, 0.058, 0.101, 0.038, 0.057, 0.103, 0.032, 0.052, 0.106),
  c("I", 0.5, 0.136, 0.173, 0.246, 0.292, 0.352, 0.471, 0.564, 0.638, 0.736),
  c("I", 1.0, 0.431, 0.502, 0.614, 0.822, 0.863, 0.922, 0.991, 0.997, 0.999),
  c("II", 0.0, 0.033, 0.061, 0.109, 0.039, 0.054, 0.099, 0.037, 0.055, 0.109),
  c("II", 0.5, 0.219, 0.279, 0.414, 0.497, 0.600, 0.740, 0.949, 0.968, 0.992),
  c("II", 1.0, 0.466, 0.539, 0.650, 0.789, 0.838, 0.902, 0.988, 0.993, 0.999),
  c("III", 0.0, 0.035, 0.057, 0.108, 0.038, 0.061, 0.099, 0.038, 0.049, 0.098),
  c("III", 0.5, 0.285, 0.351, 0.471, 0.631, 0.700, 0.797, 0.948, 0.967, 0.984),
  c("III", 1.0, 0.651, 0.708, 0.810, 0.959, 0.973, 0.991, 1.000, 1.000, 1.000),
  c("VII", 0.0, 0.034, 0.055, 0.110, 0.034, 0.049, 0.096, 0.023, 0.095, 0.093),
  c("VII", 0.5, 0.200, 0.248, 0.337, 0.368, 0.440, 0.544, 0.662, 0.723, 0.827),
  c("VII", 1.0, 0.383, 0.456, 0.604, 0.715, 0.794, 0.888, 0.956, 0.976, 0.990)
)
paper_rate <- function(model, c, n, alpha) {
  row <- PAPER[PAPER[, 1L] == model & as.numeric(PAPER[, 2L]) == c, ]
  col <- 2L + 3L * (match(n, c(50L, 100L, 200L)) - 1L) + match(alpha, levels)
  as.numeric(row[col])
}

VARIANTS <- list(
  "as described" = list(kernel = "epanechnikov", unit_variance = FALSE,
                        models = names(MODELS)),
  "Gaussian kernel" = list(kernel = "gaussian", unit_variance = FALSE,
                           models = names(MODELS)),
  "unit-variance residuals" = list(kernel = "epanechnikov", unit_variance = TRUE,
                                   models = "VII")
)

scenarios <- do.call(rbind, lapply(names(VARIANTS), function(v) {
  g <- expand.grid(
    c = c(0, 0.5, 1), n = c(50L, 100L, 200L), model = VARIANTS[[v]]$models,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  g$variant <- v
  g[, c("variant", "model", "c", "n")]
}))

# --- run --------------------------------------------------------------------------

run_scenario <- function(i) {
  s <- scenarios[i, ]
  spec <- MODELS[[s$model]]
  variant <- VARIANTS[[s$variant]]
  form <- dnvk_forms[[spec$form]]
  set.seed(seed + i)
  reject <- vapply(seq_len(n_mc), function(rep) {
    x <- stats::runif(s$n)
    y <- spec$mean(x) + spec$sd(x, s$c) * stats::rnorm(s$n)
    W <- dnvk_weights(x, 0.3, variant$kernel)
    bt <- dnvk_bootstrap(
      y, x, dnvk_mean_nw(W), W, form, B = n_boot, v = 0.2,
      unit_variance = variant$unit_variance
    )
    if (is.null(bt)) return(rep(NA_real_, length(levels) + 1L))
    c(
      vapply(levels, function(a) dnvk_reject_paper(bt$cm, bt$boot[, "cm"], a), logical(1)),
      bt$failed
    )
  }, numeric(length(levels) + 1L))

  ok <- !is.na(reject[1L, ])
  rows <- lapply(seq_along(levels), function(j) {
    rate <- mean(reject[j, ok] == 1)
    data.frame(
      scenario = i,
      variant = s$variant,
      table = if (s$model == "VII") "4.5" else "4.1",
      model = s$model,
      c = s$c,
      n = s$n,
      h = 0.3,
      kernel = variant$kernel,
      unit_variance = variant$unit_variance,
      level = levels[j],
      replications = n_mc,
      bootstrap = n_boot,
      rejection_rate = rate,
      mc_se = sqrt(rate * (1 - rate) / sum(ok)),
      effective = sum(ok),
      failures = sum(!ok),
      bootstrap_failures = sum(reject[length(levels) + 1L, ok]),
      paper_rate = paper_rate(s$model, s$c, s$n, levels[j]),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  message(sprintf(
    "%-23s (%s) c = %.1f n = %3d   2.5%% %.3f (%.3f)   5%% %.3f (%.3f)   10%% %.3f (%.3f)",
    s$variant, s$model, s$c, s$n,
    out$rejection_rate[1], out$paper_rate[1], out$rejection_rate[2], out$paper_rate[2],
    out$rejection_rate[3], out$paper_rate[3]
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
complete <- vapply(results, function(r) is.data.frame(r) && nrow(r) == length(levels), logical(1))
if (!all(complete)) {
  stop("No result for scenario(s) ", paste(index[!complete], collapse = ", "), ".")
}

out <- do.call(rbind, results)
path <- Sys.getenv(
  "CSV",
  unset = file.path("inst", "validation", "variance-form-dnvk-reproduction.csv")
)
if (!dir.exists(dirname(path))) path <- "variance-form-dnvk-reproduction.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
