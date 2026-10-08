# Tests applied to weighted fits ------------------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/weighted-fits-size.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=50 Rscript ... .
# Release evidence uses N_MC=400 for the first block, the same as the full
# sweep, and five times that for the second, whose point is a difference of a
# few percentage points.
#
# lm(..., weights = w) states that the error variance is sigma^2 / w_i. Two
# questions follow, and the two blocks answer them.
#
# Block 1, known weights. The data are heteroscedastic and the model is fitted
# with exactly the right weights. A test that reads the standardized residuals
# should then reject at its nominal level: the rejection rate here is a size.
# Before 0.12.0 the tests read the raw residuals and this script gave 1.000 for
# every heteroscedasticity diagnostic.
#
# Block 2, estimated weights. The weights come from fitWLS(), with the variance
# function correctly specified. The tests of constant variance take the weights
# as known, so their reference distributions are only approximate here;
# performVarianceFormTest() accounts for the estimation. The block records how
# far off the approximation is, at two sample sizes, because the question is
# whether the discrepancy goes away as the sample grows. It does not.
#
# Block 3, estimated weights from the wrong variance function. The variance is
# a power of x and fitWLS() fits an exponential. This is the case the tests of
# constant variance are usually asked to catch after a weighted fit. The block
# shows which of them can: a test on the variance regressors themselves has
# little left to find, and one that looks beyond them does.

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
  n_mc <- as.integer(Sys.getenv("N_MC", unset = "400"))
  if (!is.finite(n_mc) || n_mc < 20L) {
    stop("N_MC must be an integer >= 20.")
  }
  n_mc
}

alpha <- 0.05
n_mc <- get_n_mc()
n_obs <- 150L
invisible(ht_set_log_level("SILENT"))

# --- block 1: data with known weights -----------------------------------------

# The cross-sectional design of the full sweep under its alternative, sd = x^2,
# fitted with the weights that undo it.
make_xs <- function() {
  x <- runif(n_obs, 1, 5)
  z <- runif(n_obs, 1, 5)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + x^2 * rnorm(n_obs), x = x, z = z)
  d$g <- cut(d$x, 3, labels = c("a", "b", "c"))
  d$w <- 1 / x^4
  list(model = lm(y ~ x + z, data = d, weights = w), data = d)
}

# A series whose standard deviation rises along the sample. Left unweighted
# that reads as volatility clustering to the ARCH-type tests.
make_ts <- function() {
  tt <- seq_len(n_obs)
  s <- 0.3 + tt / 50
  d <- data.frame(y = 1 + 0.01 * tt + s * rnorm(n_obs), x = tt, w = 1 / s^2)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

make_reset <- function() {
  x <- runif(n_obs, 1, 5)
  d <- data.frame(x = x, y = 1 + 2 * x + x^2 * rnorm(n_obs), w = 1 / x^4)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

make_panel <- function() {
  n_i <- 30L
  n_t <- 6L
  id <- rep(seq_len(n_i), each = n_t)
  tt <- rep(seq_len(n_t), times = n_i)
  x <- runif(n_i * n_t, 1, 5)
  d <- data.frame(id = id, time = tt, x = x,
                  y = 1 + 2 * x + x^2 * rnorm(n_i * n_t), w = 1 / x^4)
  list(model = lm(y ~ x, data = d, weights = w), data = d)
}

KNOWN <- list(
  list("performCookWeisbergTest", make_xs, function(o) performCookWeisbergTest(o$model)),
  list("performDavidianCarrollTest", make_xs, function(o) performDavidianCarrollTest(o$model)),
  list("performHarveyTest", make_xs, function(o) performHarveyTest(o$model)),
  list("performNCVTest", make_xs, function(o) performNCVTest(o$model)),
  list("performSpearmanTest", make_xs, function(o) performSpearmanTest(o$model)),
  list("performSpreadLevelTest", make_xs, function(o) performSpreadLevelTest(o$model)),
  list("performBPTest", make_xs, function(o) performBPTest(o$model, o$data)),
  list("performKoenkerTest", make_xs, function(o) performKoenkerTest(o$model, o$data)),
  list("performStudentizedBPTest", make_xs, function(o) performStudentizedBPTest(o$model, o$data)),
  list("performWhiteTest", make_xs, function(o) performWhiteTest(o$model, o$data)),
  list("performWhiteTestRobust", make_xs, function(o) performWhiteTestRobust(o$model, o$data)),
  list("performBPTestRobust", make_xs, function(o) performBPTestRobust(o$model, o$data)),
  list("performWhiteTestStreaming", make_xs,
       function(o) performWhiteTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performBPTestStreaming", make_xs,
       function(o) performBPTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performKoenkerTestStreaming", make_xs,
       function(o) performKoenkerTestStreaming(o$model, o$data, chunk_size = 50, progress = FALSE)),
  list("performHighDimensionalTest", make_xs, function(o) performHighDimensionalTest(o$model, o$data)),
  list("performRankPermutationTest", make_xs,
       function(o) performRankPermutationTest(o$model, o$data, B = 199, progress = FALSE)),
  list("performBartlettTest", make_xs, function(o) performBartlettTest(o$model, o$data, "g")),
  list("performBrownForsytheTest", make_xs, function(o) performBrownForsytheTest(o$model, o$data, "g")),
  list("performFlignerKilleenTest", make_xs, function(o) performFlignerKilleenTest(o$model, o$data, "g")),
  list("performHartleyFmaxTest", make_xs, function(o) performHartleyFmaxTest(o$model, o$data, "g")),
  list("performLeveneTest", make_xs, function(o) performLeveneTest(o$model, o$data, "g")),
  list("performOBrienTest", make_xs, function(o) performOBrienTest(o$model, o$data, "g")),
  list("performGlejserTest", make_xs, function(o) performGlejserTest(o$model, o$data, "x")),
  list("performParkTest", make_xs, function(o) performParkTest(o$model, o$data, "x")),
  list("performGQTest", make_xs, function(o) performGQTest(o$model, o$data, "x")),
  list("performSzroeterTest", make_xs, function(o) performSzroeterTest(o$model, o$data, "x")),
  list("performArchLMTest", make_ts, function(o) performArchLMTest(o$model, lags = 4)),
  list("performMcLeodLiTest", make_ts, function(o) performMcLeodLiTest(o$model, lags = 4)),
  list("performRESETTest", make_reset, function(o) performRESETTest(o$model)),
  list("performBPRandomEffectsTest", make_panel,
       function(o) performBPRandomEffectsTest(o$model, o$data, "id")),
  list("performPesaranTest", make_panel,
       function(o) performPesaranTest(o$model, o$data, "id", "time")),
  # These refit the mean model without the weights and refuse a weighted fit.
  # They are listed so the matrix is complete; every call should fail.
  list("performWildBootstrapTest", make_xs,
       function(o) performWildBootstrapTest(o$model, o$data, B = 199, progress = FALSE)),
  list("performWhiteTestBootstrap", make_xs,
       function(o) performWhiteTestBootstrap(o$model, o$data, B = 199)),
  list("performQuantileRegressionTest", make_xs,
       function(o) performQuantileRegressionTest(o$model, o$data))
)

# --- block 2: weights estimated by fitWLS() -----------------------------------

# The log-variance is linear in x, so the exponential variance function that
# fitWLS() fits by default is correctly specified.
make_fgls <- function(errors, n, sd_fun = function(x) exp(0.3 * x)) {
  x <- runif(n, 1, 5)
  z <- runif(n, 1, 5)
  e <- if (errors == "normal") rnorm(n) else rt(n, df = 5) / sqrt(5 / 3)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + sd_fun(x) * e, x = x, z = z)
  list(model = fitWLS(lm(y ~ x + z, data = d)), data = d)
}

# --- block 3: weights estimated from the wrong variance function --------------

# sigma^2 = x^3, a power of x, which the exponential form cannot follow.
sd_power <- function(x) x^1.5

ESTIMATED <- list(
  list("performKoenkerTest", function(o) performKoenkerTest(o$model, o$data)),
  list("performBPTest", function(o) performBPTest(o$model, o$data)),
  list("performWhiteTest", function(o) performWhiteTest(o$model, o$data)),
  list("performNCVTest", function(o) performNCVTest(o$model)),
  list("performHarveyTest", function(o) performHarveyTest(o$model)),
  list("performVarianceFormTest", function(o) performVarianceFormTest(o$model))
)

# --- run ----------------------------------------------------------------------

pval <- function(fn, o) {
  r <- tryCatch(suppressWarnings(suppressMessages(fn(o))), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

summarise <- function(test, weights, design, errors, n, p) {
  ok <- p[!is.na(p)]
  rate <- if (length(ok)) mean(ok < alpha) else NA_real_
  message(sprintf("%-30s %-9s %-6s n=%3d reject %s%s", test, weights, errors, n,
                  if (is.na(rate)) "  .  " else sprintf("%.3f", rate),
                  if (length(ok) < length(p)) {
                    sprintf("   [%d of %d calls failed]", length(p) - length(ok), length(p))
                  } else {
                    ""
                  }))
  data.frame(
    test = test,
    weights = weights,
    design = design,
    errors = errors,
    n = n,
    replications = length(p),
    rejection_rate = rate,
    mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
    effective = length(ok),
    failures = length(p) - length(ok),
    stringsAsFactors = FALSE
  )
}

rows <- list()
for (t in KNOWN) {
  set.seed(20261008L)
  p <- vapply(seq_len(n_mc), function(i) pval(t[[3]], t[[2]]()), numeric(1))
  rows[[length(rows) + 1L]] <- summarise(
    t[[1]], "known", "heteroscedastic data, exact weights", "normal", n_obs, p
  )
}
for (errors in c("normal", "t5")) {
  for (n in c(150L, 600L)) {
    for (t in ESTIMATED) {
      set.seed(20261009L)
      p <- vapply(seq_len(5L * n_mc), function(i) pval(t[[2]], make_fgls(errors, n)), numeric(1))
      rows[[length(rows) + 1L]] <- summarise(
        t[[1]], "estimated", "fitWLS(), exponential variance correctly specified",
        errors, n, p
      )
    }
  }
}
for (n in c(150L, 600L)) {
  for (t in ESTIMATED) {
    set.seed(20261010L)
    p <- vapply(seq_len(5L * n_mc),
                function(i) pval(t[[2]], make_fgls("normal", n, sd_power)), numeric(1))
    rows[[length(rows) + 1L]] <- summarise(
      t[[1]], "misspecified", "fitWLS(), exponential fitted to sigma^2 = x^3",
      "normal", n, p
    )
  }
}

out <- do.call(rbind, rows)
path <- file.path("inst", "validation", "weighted-fits-size.csv")
if (!dir.exists(dirname(path))) path <- "weighted-fits-size.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
