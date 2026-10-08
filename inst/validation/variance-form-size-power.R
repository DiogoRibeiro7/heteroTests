# Size and power of performVarianceFormTest() ---------------------------------
#
# Run from the package root:
#   Rscript inst/validation/variance-form-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=200 Rscript ... .
# Release evidence uses N_MC=5000, which puts the Monte Carlo standard error at
# 0.31% for a test holding its nominal 5% level, so the release gate in
# README.md applies.
#
# The null of this test is a variance function, so "size" is not one number: it
# is the rejection rate whenever the form under test is the true one, which
# includes constant variance, a special case of every form. Each form is
# therefore tested against five truths. A cell is a size cell when the form
# under test nests the truth and a power cell otherwise.
#
# One of the truths is additive, sigma^2 = 0.5 + x. Neither form nests it, and
# over the range of x used here it is close to both, so its cells measure how
# little the test can say about two shapes the data cannot tell apart.

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

get_n_mc <- function() {
  n_mc <- as.integer(Sys.getenv("N_MC", unset = "5000"))
  if (!is.finite(n_mc) || n_mc < 20L) {
    stop("N_MC must be an integer >= 20.")
  }
  n_mc
}

alpha <- 0.05
n_mc <- get_n_mc()
invisible(ht_set_log_level("SILENT"))

# --- data-generating processes ----------------------------------------------

# y = 1 + 2 x + 0.5 z + sigma(x) e, with x and z uniform on (1, 5) so that the
# logarithms of the power form are defined. The variance depends on x alone;
# the test is run with its defaults, so the variance regressors are x and z and
# the added terms are x^2, z^2 and x z.
TRUTHS <- list(
  constant    = list(sd = function(x) rep(1, length(x)),
                     label = "sigma^2 = 1"),
  exponential = list(sd = function(x) exp(0.3 * x),
                     label = "sigma^2 = exp(0.6 x)"),
  power       = list(sd = function(x) x^1.5,
                     label = "sigma^2 = x^3"),
  linear      = list(sd = function(x) sqrt(0.5 + x),
                     label = "sigma^2 = 0.5 + x"),
  quadratic   = list(sd = function(x) exp(0.25 * (x - 3)^2),
                     label = "sigma^2 = exp(0.5 (x - 3)^2)")
)

# Which truths each form nests, i.e. where its null hypothesis holds.
NESTS <- list(
  exponential = c("constant", "exponential"),
  power       = c("constant", "power")
)

make_data <- function(n, sd_fun, errors) {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  e <- if (errors == "normal") rnorm(n) else rt(n, df = 5) / sqrt(5 / 3)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + sd_fun(x) * e, x = x, z = z)
  lm(y ~ x + z, data = d)
}

pval <- function(model, form) {
  r <- tryCatch(
    suppressWarnings(performVarianceFormTest(model, form = form)),
    error = function(e) NULL
  )
  if (is.null(r)) NA_real_ else as.numeric(r$p.value)
}

grid <- expand.grid(
  n = c(50L, 150L, 400L),
  truth = names(TRUTHS),
  form = names(NESTS),
  errors = c("normal", "t5"),
  stringsAsFactors = FALSE
)

rows <- vector("list", nrow(grid))
for (k in seq_len(nrow(grid))) {
  g <- grid[k, ]
  # One seed per cell, so a cell can be re-run on its own and the result does
  # not depend on the order of the grid.
  set.seed(20261008L + k)
  p <- vapply(
    seq_len(n_mc),
    function(i) pval(make_data(g$n, TRUTHS[[g$truth]]$sd, g$errors), g$form),
    numeric(1)
  )
  ok <- p[!is.na(p)]
  rate <- if (length(ok)) mean(ok < alpha) else NA_real_
  rows[[k]] <- data.frame(
    form = g$form,
    truth = g$truth,
    truth_label = TRUTHS[[g$truth]]$label,
    hypothesis = if (g$truth %in% NESTS[[g$form]]) "null" else "alternative",
    errors = g$errors,
    n = g$n,
    replications = n_mc,
    rejection_rate = rate,
    mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
    effective = length(ok),
    failures = sum(is.na(p)),
    stringsAsFactors = FALSE
  )
  message(sprintf("%-11s <- %-11s %-6s n=%3d  %-11s reject %s  failures %d",
                  g$form, g$truth, g$errors, g$n, rows[[k]]$hypothesis,
                  if (is.na(rate)) "  .  " else sprintf("%.3f", rate),
                  sum(is.na(p))))
}

out <- do.call(rbind, rows)
path <- file.path("inst", "validation", "variance-form-size-power.csv")
if (!dir.exists(dirname(path))) path <- "variance-form-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
