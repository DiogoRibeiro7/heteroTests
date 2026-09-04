# Full-sweep statistical validation: every exported perform*Test() ------------
#
# Run from the package root:
#   Rscript inst/validation/full-sweep-size-power.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=50 Rscript ... .
# Release evidence uses N_MC=400, which puts the Monte Carlo standard error at
# 1.1% for a test holding its nominal 5% level.
#
# Unlike Pass A and Pass B, this sweep covers tests with different nulls, so a
# single data-generating process will not do. Each test is driven by the null
# and alternative appropriate to what it actually tests: heteroscedasticity for
# the variance diagnostics, ARCH errors for the time-series ones, an omitted
# quadratic for RESET, a random intercept for the random-effects LM test, and a
# common time factor for the cross-sectional dependence test. Reading a
# rejection rate as "power" only makes sense against that test's own
# alternative.

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

# --- data-generating processes ----------------------------------------------

make_xs <- function(sd_fun) {
  x <- runif(n_obs, 1, 5)
  z <- runif(n_obs, 1, 5)
  d <- data.frame(y = 1 + 2 * x + 0.5 * z + rnorm(n_obs, sd = sd_fun(x)),
                  x = x, z = z)
  d$g <- cut(d$x, 3, labels = c("a", "b", "c"))
  list(model = lm(y ~ x + z, data = d), data = d)
}

make_ts <- function(alpha1) {
  e <- numeric(n_obs)
  h <- numeric(n_obs)
  h[1] <- 1
  e[1] <- rnorm(1)
  for (t in 2:n_obs) {
    h[t] <- 0.2 + alpha1 * e[t - 1]^2
    e[t] <- rnorm(1, sd = sqrt(h[t]))
  }
  tt <- seq_len(n_obs)
  d <- data.frame(y = 1 + 0.01 * tt + e, x = tt)
  list(model = lm(y ~ x, data = d), data = d)
}

make_panel <- function(sd_u, factor_sd) {
  n_i <- 30L
  n_t <- 6L
  id <- rep(seq_len(n_i), each = n_t)
  tt <- rep(seq_len(n_t), times = n_i)
  x <- runif(n_i * n_t, 1, 5)
  u <- if (sd_u > 0) rep(rnorm(n_i, sd = sd_u), each = n_t) else 0
  f <- if (factor_sd > 0) factor_sd * rnorm(n_t)[tt] else 0
  d <- data.frame(id = id, time = tt, x = x,
                  y = 1 + 2 * x + u + f + rnorm(n_i * n_t))
  list(model = lm(y ~ x, data = d), data = d)
}

make_reset <- function(quad) {
  x <- runif(n_obs, 1, 5)
  d <- data.frame(x = x, y = 1 + 2 * x + quad * x^2 + rnorm(n_obs))
  list(model = lm(y ~ x, data = d), data = d)
}

FAMILIES <- list(
  hetero = list(null = function() make_xs(function(x) rep(1, length(x))),
                alt  = function() make_xs(function(x) x^2),
                alt_label = "sd = x^2"),
  arch   = list(null = function() make_ts(0),
                alt  = function() make_ts(0.6),
                alt_label = "ARCH(1), alpha = 0.6"),
  effect = list(null = function() make_panel(0, 0),
                alt  = function() make_panel(2, 0),
                alt_label = "random intercepts"),
  csdep  = list(null = function() make_panel(0, 0),
                alt  = function() make_panel(0, 2),
                alt_label = "common time factor"),
  form   = list(null = function() make_reset(0),
                alt  = function() make_reset(0.6),
                alt_label = "omitted quadratic")
)

# --- how each exported test is invoked --------------------------------------

TESTS <- list(
  list("performCookWeisbergTest", "hetero", function(o) performCookWeisbergTest(o$model)),
  list("performDavidianCarrollTest", "hetero", function(o) performDavidianCarrollTest(o$model)),
  list("performHarveyTest", "hetero", function(o) performHarveyTest(o$model)),
  list("performNCVTest", "hetero", function(o) performNCVTest(o$model)),
  list("performSpearmanTest", "hetero", function(o) performSpearmanTest(o$model)),
  list("performSpreadLevelTest", "hetero", function(o) performSpreadLevelTest(o$model)),
  list("performBPTest", "hetero", function(o) performBPTest(o$model, o$data)),
  list("performBreuschPaganTest", "hetero", function(o) performBreuschPaganTest(o$model, o$data)),
  list("performKoenkerTest", "hetero", function(o) performKoenkerTest(o$model, o$data)),
  list("performStudentizedBPTest", "hetero", function(o) performStudentizedBPTest(o$model, o$data)),
  list("performWhiteTest", "hetero", function(o) performWhiteTest(o$model, o$data)),
  list("performQuantileRegressionTest", "hetero", function(o) performQuantileRegressionTest(o$model, o$data)),
  list("performHighDimensionalTest", "hetero", function(o) performHighDimensionalTest(o$model, o$data)),
  list("performRankPermutationTest", "hetero", function(o) performRankPermutationTest(o$model, o$data, B = 199)),
  list("performWildBootstrapTest", "hetero", function(o) performWildBootstrapTest(o$model, o$data, B = 199)),
  list("performBartlettTest", "hetero", function(o) performBartlettTest(o$model, o$data, "g")),
  list("performBrownForsytheTest", "hetero", function(o) performBrownForsytheTest(o$model, o$data, "g")),
  list("performFlignerKilleenTest", "hetero", function(o) performFlignerKilleenTest(o$model, o$data, "g")),
  list("performHartleyFmaxTest", "hetero", function(o) performHartleyFmaxTest(o$model, o$data, "g")),
  list("performLeveneTest", "hetero", function(o) performLeveneTest(o$model, o$data, "g")),
  list("performOBrienTest", "hetero", function(o) performOBrienTest(o$model, o$data, "g")),
  list("performBoxMTest", "hetero", function(o) performBoxMTest(o$data[, c("y", "x")], o$data$g)),
  list("performGlejserTest", "hetero", function(o) performGlejserTest(o$model, o$data, "x")),
  list("performParkTest", "hetero", function(o) performParkTest(o$model, o$data, "x")),
  list("performGQTest", "hetero", function(o) performGQTest(o$model, o$data, "x")),
  list("performSzroeterTest", "hetero", function(o) performSzroeterTest(o$model, o$data, "x")),
  list("performArchLMTest", "arch", function(o) performArchLMTest(o$model, lags = 4)),
  list("performMcLeodLiTest", "arch", function(o) performMcLeodLiTest(o$model, lags = 4)),
  list("performRESETTest", "form", function(o) performRESETTest(o$model)),
  list("performBPRandomEffectsTest", "effect",
       function(o) performBPRandomEffectsTest(o$model, o$data, "id")),
  list("performPesaranTest", "csdep",
       function(o) performPesaranTest(o$model, o$data, "id", "time"))
)

pval <- function(fn, o) {
  r <- tryCatch(suppressWarnings(fn(o)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

rate <- function(p) {
  ok <- p[!is.na(p)]
  if (length(ok) == 0L) return(c(rate = NA_real_, eff = 0, se = NA_real_))
  r <- mean(ok < alpha)
  c(rate = r, eff = length(ok), se = sqrt(r * (1 - r) / length(ok)))
}

rows <- vector("list", length(TESTS))
for (k in seq_along(TESTS)) {
  nm <- TESTS[[k]][[1]]
  fam <- FAMILIES[[TESTS[[k]][[2]]]]
  fn <- TESTS[[k]][[3]]

  set.seed(20260904L)
  p_null <- vapply(seq_len(n_mc), function(i) pval(fn, fam$null()), numeric(1))
  set.seed(20260905L)
  p_alt <- vapply(seq_len(n_mc), function(i) pval(fn, fam$alt()), numeric(1))

  s <- rate(p_null)
  a <- rate(p_alt)
  rows[[k]] <- data.frame(
    test = nm,
    family = TESTS[[k]][[2]],
    alternative = fam$alt_label,
    replications = n_mc,
    size = unname(s["rate"]),
    size_mc_se = unname(s["se"]),
    power = unname(a["rate"]),
    power_mc_se = unname(a["se"]),
    failures = sum(is.na(p_null)) + sum(is.na(p_alt)),
    stringsAsFactors = FALSE
  )
  message(sprintf("%-32s size %.3f  power %.3f", nm, s["rate"], a["rate"]))
}

out <- do.call(rbind, rows)
path <- file.path("inst", "validation", "full-sweep-size-power.csv")
if (!dir.exists(dirname(path))) path <- "full-sweep-size-power.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
