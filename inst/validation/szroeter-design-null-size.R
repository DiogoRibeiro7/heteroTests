# Szroeter's test with a null distribution taken from the design ---------------
#
# Run from the package root:
#   Rscript inst/validation/szroeter-design-null-size.R
#
# N_MC sets the replications of every size cell, 5000 by default, where the
# 99% interval around a true 5% is the release gate, [0.042, 0.058]. N_NULL,
# 100000 by default, is the number of samples of the first block. CHUNK_SIZE,
# 500 by default, cuts each size cell into chunks. PARTS names a directory
# where every chunk is saved as unit-NNNN.rds as soon as it is done, with the
# settings and the md5 of this script; a chunk whose file is there, written by
# the same script with the same settings, is not run again, so a run that was
# stopped resumes where it stopped. UNITS, for example "1:40", runs only those
# chunks, numbered as in the `units` table, so that separate processes can
# share the work on Windows; MC_CORES runs the chunks in parallel through
# parallel::mclapply() where processes can be forked. Before each chunk the
# script waits until the machine has MIN_FREE_GB of free memory, 4 by default,
# where it can tell. When every chunk is in PARTS, the script writes CSV, by
# default inst/validation/szroeter-design-null-size.csv, and RDS, when set, an
# .rds file with the rates, the paired comparisons, the checks, the settings
# and sessionInfo().
#
# performSzroeterTest() orders the Pearson residuals by `order_by` and computes
# h = sum_i i e_(i)^2 / sum_i e_(i)^2, which it standardises with the mean
# (n + 1) / 2 and the variance (n^2 - 1) / (6 n), whatever the design. Under
# normal errors of constant variance, given the design, h is a ratio of
# quadratic forms, as the Harrison-McCabe statistic is: with M the
# residual-maker of the (weighted) design in the ordered sample, A = diag(1,
# ..., n), nu = n - k and lambda_1, ..., lambda_nu the eigenvalues of M A M on
# the residual space (those of Q2' A Q2, Q2 an orthonormal basis of that
# space),
#
#   h = sum_j lambda_j z_j^2 / sum_j z_j^2,   z_j independent N(0, 1).
#
# The ratio is independent of its denominator, so its moments are ratios of
# moments: E h = mean(lambda) = tr(M A) / nu, and
# Var h = 2 sum_j (lambda_j - mean(lambda))^2 / (nu (nu + 2)). Without
# regressors the eigenvalues are 1, ..., n and these are (n + 1) / 2 and
# (n^2 - 1) / (6 (n + 2)); the package's variance is Szroeter's (1978)
# asymptotic one. The script measures three p-values of the default
# alternative, a variance that increases along the ordering:
#
#   current         the package's, from performSzroeterTest();
#   design_moments  (h - E h) / sqrt(Var h) against the standard normal, with
#                   the exact moments given the design;
#   design_exact    P(h >= h_obs) from the eigenvalues, by Imhof's (1961)
#                   formula as the package evaluates it for
#                   performHarrisonMcCabeTest() (rhmc_tails()): exact under
#                   normal errors, given the design.
#
# The design is that of harrison-mccabe-size-power.R: y = 1 + 2 x1 + 0.5 x2 +
# sd_i e_i with x1 uniform on (1, 5) and x2 standard normal, ordered by x1,
# fitted as lm(y ~ x1 + x2), at 40, 100 and 400 observations, Gaussian errors,
# t5 errors scaled to unit variance, and the weighted fit of that study, sd_i =
# x1 with weights 1 / x1^2, under which the null hypothesis is true of the
# weighted model. Each size cell takes the seed of its scenario in that
# script, seed + scenario, with its seed 20261015, and draws its samples as
# that script does, so the samples are the ones it tested, and the column
# `current` reproduces its rates for performSzroeterTest(). A chunk draws the
# samples before it and discards them.
#
# Block 1, the null distribution given the design: one design of each size,
# unweighted and weighted, held fixed while N_NULL normal error vectors are
# drawn; each method's critical values at 1%, 5% and 10% are computed once,
# so the rates are the conditional sizes of the three methods given the
# design. Block 2: the size cells, with the design redrawn in every sample.

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
invisible(ht_set_log_level("SILENT"))

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

script_path <- local({
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(arg)) normalizePath(sub("^--file=", "", arg[1]), mustWork = FALSE) else NA_character_
})
script_md5 <- if (!is.na(script_path) && file.exists(script_path)) {
  unname(tools::md5sum(script_path))
} else {
  NA_character_
}

alpha <- 0.05
levels_tested <- c(0.01, 0.05, 0.10)
n_mc <- get_count("N_MC", "5000", 20L)
n_null <- get_count("N_NULL", "100000", 1000L)
chunk_size <- get_count("CHUNK_SIZE", "500", 1L)
n_cores <- get_count("MC_CORES", "1", 1L)
min_free_gb <- as.numeric(Sys.getenv("MIN_FREE_GB", unset = "4"))
# The seed of harrison-mccabe-size-power.R.
seed <- 20261015L
parts_dir <- Sys.getenv("PARTS", unset = file.path(tempdir(), "szroeter-parts"))
dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "szroeter-design-null-size.csv")
  } else {
    "szroeter-design-null-size.csv"
  }
)
rds_path <- Sys.getenv("RDS", unset = "")
settings <- list(n_mc = n_mc, n_null = n_null, chunk_size = chunk_size, seed = seed,
                 md5 = script_md5)
methods <- c("current", "design_moments", "design_exact")
hmc_tails <- heteroTests:::rhmc_tails

# --- designs, as in harrison-mccabe-size-power.R --------------------------------

draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    stop("unknown error distribution")
  )
}

make_data <- function(n, errors, weights) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  sd <- if (weights == "exact") x1 else rep(1, n)
  d <- data.frame(x1 = x1, x2 = x2, y = 1 + 2 * x1 + 0.5 * x2 + sd * draw_errors(n, errors))
  if (weights == "exact") {
    d$w <- 1 / x1^2
    model <- lm(y ~ x1 + x2, data = d, weights = w)
  } else {
    model <- lm(y ~ x1 + x2, data = d)
  }
  list(model = model, data = d)
}

# The random numbers make_data() draws, without the fit.
skip_sample <- function(n, errors) {
  runif(n, 1, 5)
  rnorm(n)
  draw_errors(n, errors)
  invisible(NULL)
}

# --- the null distribution of h given the design ---------------------------------

# The eigenvalues of M A M on the residual space of the ordered design.
szroeter_spectrum <- function(design_ordered) {
  n <- nrow(design_ordered)
  qx <- qr(design_ordered)
  k <- qx$rank
  q2 <- qr.Q(qx, complete = TRUE)[, (k + 1L):n, drop = FALSE]
  values <- eigen(crossprod(q2, q2 * seq_len(n)), symmetric = TRUE, only.values = TRUE)$values
  list(values = values, multiplicity = rep(1, length(values)), nu = n - k, k = k)
}

design_moments <- function(spectrum) {
  lam <- spectrum$values
  nu <- spectrum$nu
  c(mean = mean(lam), var = 2 * sum((lam - mean(lam))^2) / (nu * (nu + 2)))
}

# h, the ordered Pearson residuals and the ordered weighted design of a fit.
szroeter_parts <- function(model, ordering) {
  w <- stats::weights(model)
  X <- stats::model.matrix(model)
  e <- stats::residuals(model)
  if (!is.null(w)) {
    X <- sqrt(w) * X
    e <- sqrt(w) * e
  }
  ord <- order(ordering)
  e <- e[ord]
  n <- length(e)
  list(h = sum(seq_len(n) * e^2) / sum(e^2), design = X[ord, , drop = FALSE], n = n)
}

design_p_values <- function(h, spectrum) {
  m <- design_moments(spectrum)
  c(design_moments = stats::pnorm((h - m[["mean"]]) / sqrt(m[["var"]]), lower.tail = FALSE),
    design_exact = hmc_tails(h, spectrum)[["upper"]])
}

# --- scenarios ------------------------------------------------------------------

# `scenario` is the row of the scenario in harrison-mccabe-size-power.R.
cells <- data.frame(
  block = rep(c("size", "size", "size, weighted fit"), each = 3),
  errors = rep(c("normal", "t5", "normal"), each = 3),
  n = rep(c(40L, 100L, 400L), 3),
  weights = rep(c("none", "none", "exact"), each = 3),
  scenario = c(5:7, 8:10, 11:13),
  stringsAsFactors = FALSE
)
cells$cell <- seq_len(nrow(cells))
null_designs <- data.frame(n = rep(c(40L, 100L, 400L), 2),
                           weights = rep(c("none", "exact"), each = 3),
                           stringsAsFactors = FALSE)

units <- rbind(
  data.frame(kind = "null", cell = seq_len(nrow(null_designs)), chunk = 1L, size = n_null),
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
    starts <- seq(1L, n_mc, by = chunk_size)
    data.frame(kind = "size", cell = i, chunk = seq_along(starts),
               size = pmin(chunk_size, n_mc - starts + 1L))
  }))
)
# The null-distribution units run first in a split of UNITS by halves only by
# chance; the slow units are those with 400 observations.

# --- block 1 --------------------------------------------------------------------

run_null_unit <- function(u) {
  s <- null_designs[u$cell, ]
  set.seed(seed + 100000L + u$cell)
  x1 <- runif(s$n, 1, 5)
  x2 <- rnorm(s$n)
  ord <- order(x1)
  design <- cbind(1, x1, x2)
  if (s$weights == "exact") design <- design / x1
  design <- design[ord, , drop = FALSE]
  spectrum <- szroeter_spectrum(design)
  m <- design_moments(spectrum)
  n <- s$n
  basis <- qr(design)
  h <- numeric(0)
  for (start in seq(1L, u$size, by = 10000L)) {
    size <- min(10000L, u$size - start + 1L)
    resid <- qr.resid(basis, matrix(rnorm(n * size), n, size))
    h <- c(h, colSums(seq_len(n) * resid^2) / colSums(resid^2))
  }
  upper <- function(q) hmc_tails(q, spectrum)[["upper"]]
  rows <- lapply(levels_tested, function(level) {
    z <- stats::qnorm(level, lower.tail = FALSE)
    critical <- c(
      current = (n + 1) / 2 + z * sqrt((n^2 - 1) / (6 * n)),
      design_moments = m[["mean"]] + z * sqrt(m[["var"]]),
      design_exact = stats::uniroot(function(q) upper(q) - level,
                                    range(spectrum$values) + c(1e-9, -1e-9),
                                    tol = 1e-10)$root
    )
    data.frame(block = "null distribution", errors = "normal", n = n, weights = s$weights,
               method = names(critical), level = level, replications = length(h),
               rejection_rate = vapply(critical, function(cv) mean(h >= cv), numeric(1)),
               critical_value = unname(critical), mean_h = mean(h), var_h = stats::var(h),
               design_mean = m[["mean"]], design_var = m[["var"]],
               fixed_mean = (n + 1) / 2, fixed_var = (n^2 - 1) / (6 * n),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$mc_se <- sqrt(out$rejection_rate * (1 - out$rejection_rate) / out$replications)
  rownames(out) <- NULL
  out
}

# --- block 2 --------------------------------------------------------------------

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NULL)
  r
}

run_size_unit <- function(u) {
  s <- cells[u$cell, ]
  set.seed(seed + s$scenario)
  for (r in seq_len((u$chunk - 1L) * chunk_size)) skip_sample(s$n, s$errors)
  out <- matrix(NA_real_, u$size, 9L, dimnames = list(NULL, c(
    methods, "h", "h_package", "design_mean", "design_var", "fixed_mean", "fixed_var"
  )))
  for (r in seq_len(u$size)) {
    o <- make_data(s$n, s$errors, s$weights)
    res <- pval(performSzroeterTest(o$model, o$data, "x1"))
    parts <- szroeter_parts(o$model, o$data$x1)
    spectrum <- szroeter_spectrum(parts$design)
    m <- design_moments(spectrum)
    out[r, ] <- c(
      current = if (is.null(res)) NA_real_ else res$p.value,
      design_p_values(parts$h, spectrum),
      h = parts$h,
      h_package = if (is.null(res)) NA_real_ else unname(res$estimate[["h"]]),
      design_mean = m[["mean"]], design_var = m[["var"]],
      fixed_mean = (s$n + 1) / 2, fixed_var = (s$n^2 - 1) / (6 * s$n)
    )
  }
  out
}

# --- run ------------------------------------------------------------------------

free_ram_gb <- function() {
  kb <- tryCatch(
    if (.Platform$OS.type == "windows") {
      as.numeric(system2(
        "powershell",
        c("-NoProfile", "-Command", "(Get-CimInstance Win32_OperatingSystem).FreePhysicalMemory"),
        stdout = TRUE, stderr = FALSE
      )[1])
    } else if (file.exists("/proc/meminfo")) {
      line <- grep("^MemAvailable:", readLines("/proc/meminfo"), value = TRUE)
      as.numeric(gsub("[^0-9]", "", line))
    } else {
      NA_real_
    },
    error = function(e) NA_real_
  )
  kb / 1024^2
}

wait_for_memory <- function() {
  repeat {
    free <- free_ram_gb()
    if (is.na(free) || free >= min_free_gb) {
      return(invisible(free))
    }
    message(sprintf("%s: %.1f GB free, below %.1f GB; waiting", format(Sys.time(), "%H:%M"),
                    free, min_free_gb))
    Sys.sleep(60)
  }
}

part_file <- function(k) file.path(parts_dir, sprintf("unit-%04d.rds", k))
part_done <- function(k) {
  f <- part_file(k)
  file.exists(f) && identical(tryCatch(readRDS(f)$settings, error = function(e) NULL), settings)
}

run_unit <- function(k) {
  if (part_done(k)) return(invisible(k))
  wait_for_memory()
  u <- units[k, ]
  started <- Sys.time()
  result <- if (u$kind == "null") run_null_unit(u) else run_size_unit(u)
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  saveRDS(list(unit = k, kind = u$kind, cell = u$cell, chunk = u$chunk, result = result,
               settings = settings, elapsed = elapsed), part_file(k))
  message(sprintf("%s: unit %d (%s, cell %d, chunk %d) in %.1f s",
                  format(Sys.time(), "%H:%M:%S"), k, u$kind, u$cell, u$chunk, elapsed))
  invisible(k)
}

units_spec <- Sys.getenv("UNITS", unset = "")
index <- if (nzchar(units_spec)) {
  intersect(seq_len(nrow(units)), eval(parse(text = units_spec)))
} else {
  seq_len(nrow(units))
}
if (n_cores > 1L && .Platform$OS.type != "windows") {
  invisible(parallel::mclapply(index, run_unit, mc.cores = n_cores, mc.preschedule = FALSE))
} else {
  invisible(lapply(index, run_unit))
}

missing_units <- which(!vapply(seq_len(nrow(units)), part_done, logical(1)))
if (length(missing_units)) {
  message(length(missing_units), " chunks are not in ", parts_dir, " yet; no CSV written.")
  quit(save = "no")
}

# --- summary --------------------------------------------------------------------

gate <- c(0.05 - 2.576 * sqrt(0.05 * 0.95 / n_mc), 0.05 + 2.576 * sqrt(0.05 * 0.95 / n_mc))
null_rows <- do.call(rbind, lapply(which(units$kind == "null"), function(k) {
  readRDS(part_file(k))$result
}))

samples <- lapply(seq_len(nrow(cells)), function(i) {
  ks <- which(units$kind == "size" & units$cell == i)
  ks <- ks[order(units$chunk[ks])]
  do.call(rbind, lapply(ks, function(k) readRDS(part_file(k))$result))
})

size_rows <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  s <- cells[i, ]
  p <- samples[[i]]
  do.call(rbind, lapply(methods, function(m) {
    do.call(rbind, lapply(levels_tested, function(level) {
      ok <- p[, m][!is.na(p[, m])]
      rate <- mean(ok < level)
      data.frame(
        cell = i, block = s$block, errors = s$errors, n = s$n, weights = s$weights,
        method = m, level = level, replications = nrow(p), effective = length(ok),
        rejection_rate = rate, mc_se = sqrt(rate * (1 - rate) / length(ok)),
        in_gate = if (level == alpha) rate >= gate[1] & rate <= gate[2] else NA,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

# How far the design-based p-values move from the current ones on the same
# samples, and how often a decision at 5% changes; the paired difference of
# the rejection rates and its standard error.
paired_rows <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  s <- cells[i, ]
  p <- samples[[i]]
  do.call(rbind, lapply(c("design_moments", "design_exact"), function(m) {
    ok <- !is.na(p[, "current"]) & !is.na(p[, m])
    a <- p[ok, m] < alpha
    b <- p[ok, "current"] < alpha
    d <- mean(a) - mean(b)
    discord <- mean(a != b)
    diff_p <- p[ok, m] - p[ok, "current"]
    data.frame(
      cell = i, block = s$block, errors = s$errors, n = s$n, weights = s$weights,
      method = m, rate = mean(a), rate_current = mean(b), difference = d,
      difference_se = sqrt((discord - d^2) / sum(ok)),
      decisions_changed = sum(a != b), rejected_only_by_method = sum(a & !b),
      rejected_only_by_current = sum(b & !a),
      median_abs_p_change = stats::median(abs(diff_p)),
      q95_abs_p_change = unname(stats::quantile(abs(diff_p), 0.95)),
      max_abs_p_change = max(abs(diff_p)),
      median_abs_p_change_below_0.2 = {
        near <- p[ok, "current"] < 0.2
        if (any(near)) stats::median(abs(diff_p[near])) else NA_real_
      },
      mean_design_mean_minus_fixed = mean(p[ok, "design_mean"] - p[ok, "fixed_mean"]),
      mean_design_sd_over_fixed = mean(sqrt(p[ok, "design_var"] / p[ok, "fixed_var"])),
      stringsAsFactors = FALSE
    )
  }))
}))

# --- checks ---------------------------------------------------------------------

check_tolerance <- 1e-8
checks <- list(
  h_against_package = max(vapply(samples, function(p) {
    max(abs(p[, "h"] - p[, "h_package"]) / p[, "h_package"], na.rm = TRUE)
  }, numeric(1)))
)
# The exact moments against those of the N_NULL samples of block 1, in units
# of the Monte Carlo standard error of the simulated mean, and as a ratio of
# variances.
checks$null_moments <- unique(data.frame(
  n = null_rows$n, weights = null_rows$weights,
  mean_z = (null_rows$mean_h - null_rows$design_mean) /
    sqrt(null_rows$design_var / null_rows$replications),
  var_ratio = null_rows$var_h / null_rows$design_var,
  fixed_mean_z = (null_rows$mean_h - null_rows$fixed_mean) /
    sqrt(null_rows$design_var / null_rows$replications),
  fixed_var_ratio = null_rows$var_h / null_rows$fixed_var
))
# Without regressors the eigenvalues are 1, ..., n, and the moments
# (n + 1) / 2 and (n^2 - 1) / (6 (n + 2)).
checks$no_regressors <- local({
  n <- 40
  m <- design_moments(list(values = seq_len(n), nu = n))
  c(mean = m[["mean"]] - (n + 1) / 2, var = m[["var"]] - (n^2 - 1) / (6 * (n + 2)))
})
# The help-page examples of performSzroeterTest(), with the three p-values.
example_rows <- local({
  rows <- list()
  add <- function(label, model, data, order_by) {
    res <- performSzroeterTest(model, data, order_by)
    parts <- szroeter_parts(model, data[[order_by]])
    spectrum <- szroeter_spectrum(parts$design)
    m <- design_moments(spectrum)
    rows[[length(rows) + 1L]] <<- data.frame(
      example = label, n = parts$n, h = parts$h, Q_current = unname(res$statistic),
      Q_design = (parts$h - m[["mean"]]) / sqrt(m[["var"]]),
      p_current = res$p.value, p_design_moments = design_p_values(parts$h, spectrum)[[1]],
      p_design_exact = design_p_values(parts$h, spectrum)[[2]],
      fixed_mean = (parts$n + 1) / 2, design_mean = m[["mean"]],
      fixed_sd = sqrt((parts$n^2 - 1) / (6 * parts$n)), design_sd = sqrt(m[["var"]]),
      stringsAsFactors = FALSE
    )
  }
  add("lm(mpg ~ wt + qsec, mtcars), order_by = \"wt\"", lm(mpg ~ wt + qsec, data = mtcars),
      mtcars, "wt")
  set.seed(404)
  n <- 150
  x <- sort(runif(n))
  y <- 2 + 0.5 * x + rnorm(n, sd = 0.4 + 0.8 * x)
  df <- data.frame(y, x)
  add("help example, n = 150, sd = 0.4 + 0.8 x", lm(y ~ x, data = df), df, "x")
  do.call(rbind, rows)
})

runtime_minutes <- sum(vapply(seq_len(nrow(units)), function(k) {
  e <- readRDS(part_file(k))$elapsed
  if (is.null(e)) NA_real_ else e
}, numeric(1))) / 60

shown <- size_rows[size_rows$level == alpha, ]
for (i in unique(shown$cell)) {
  x <- shown[shown$cell == i, ]
  message(sprintf("%-18s %-6s n=%3d %-5s current %.4f  design_moments %.4f  design_exact %.4f",
                  x$block[1], x$errors[1], x$n[1], x$weights[1],
                  x$rejection_rate[x$method == "current"],
                  x$rejection_rate[x$method == "design_moments"],
                  x$rejection_rate[x$method == "design_exact"]))
}
out <- rbind(
  data.frame(null_rows[, c("block", "errors", "n", "weights", "method", "level",
                           "replications", "rejection_rate", "mc_se")],
             in_gate = NA, stringsAsFactors = FALSE),
  size_rows[, c("block", "errors", "n", "weights", "method", "level", "replications",
                "rejection_rate", "mc_se", "in_gate")]
)
utils::write.csv(out, csv_path, row.names = FALSE)
message("wrote ", csv_path)
session <- utils::sessionInfo()
if (nzchar(rds_path)) {
  saveRDS(list(results = out, null_distribution = null_rows, size = size_rows,
               paired = paired_rows, examples = example_rows, checks = checks,
               cells = cells, units = units,
               settings = c(settings, list(gate = gate, runtime_minutes = runtime_minutes,
                                           package_version = as.character(
                                             utils::packageVersion("heteroTests")))),
               session = session), rds_path)
  message("wrote ", rds_path)
}
print(null_rows)
print(paired_rows)
print(example_rows)
print(checks)
print(session)
