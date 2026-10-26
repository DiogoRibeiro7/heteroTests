# The wild bootstrap with and without its multiplier ------------------------------
#
# Run from the package root:
#   Rscript inst/validation/wild-no-multiplier-size.R
#
# N_MC sets the replications of the size cells and N_MC_POWER those of the
# power cells, N_BOOT the bootstrap samples of each test and CHUNK_SIZE the
# replications of a chunk; the evidence uses N_MC=5000, N_MC_POWER=1000,
# N_BOOT=999 and CHUNK_SIZE=500, the defaults, those of
# wild-multiplier-size.R. CHECK_REPS, 2 by default, is the number of
# replications at the start of every cell that are also given to
# performWildBootstrapTest(), whose p-values the script must reproduce exactly.
# PARTS names a directory where every chunk is saved as unit-NNNN.rds as soon
# as it is done, with the settings and the md5 of this script; a chunk whose
# file is there, written by the same script with the same settings, is not
# run again, so a run that was stopped resumes where it stopped. UNITS, for
# example "1:60", runs only those chunks, numbered as in the `units` table,
# so that separate processes can share the work on Windows; MC_CORES runs the
# chunks in parallel through parallel::mclapply() where processes can be
# forked. Before each chunk the script waits until the machine has
# MIN_FREE_GB of free memory, 4 by default, where it can tell. When every
# chunk is in PARTS, the script writes CSV, by default
# inst/validation/wild-no-multiplier-size.csv, and RDS, when set, an .rds file
# with the rates, the paired differences, the checks, the settings and
# sessionInfo().
#
# performWildBootstrapTest() resamples the leverage-standardised, centred
# residuals r_i = e_i / sqrt(1 - h_ii) - mean with replacement and multiplies
# each by a draw v of the multiplier: y*_i = yhat_i + r*_i v_i. The variants:
#
#   rademacher  v = -1 or 1 with equal probability (the default);
#   mammen      Mammen's two-point distribution;
#   none        v = 1: a plain residual bootstrap of the r_i, which already
#               imposes the null hypothesis, since the r_i are drawn i.i.d.
#
# The script reproduces the function's resampling rather than calling it: for
# each multiplier, B times, the draw of v and then the draw of the residuals,
# in the function's order and with its calls to sample(). A replicate's
# statistic is n R^2 of the regression of its squared residuals on the
# regressors, its residuals being M (yhat + e*) = M e*, with M the
# residual-maker of the design; the function refits lm.fit() on the design,
# which gives the same residuals to rounding. The observed statistic is the
# package's (.compute_bp_statistic()). `none` uses the residuals drawn for
# `rademacher`, without their signs, so the two differ only by the multiplier;
# `mammen` draws its own, after them, as the function does when it is called
# with each multiplier in turn. The samples, the order of the calls and the
# seeds are those of wild-multiplier-size.R, so in its cells the rates of
# `rademacher` and `mammen` are the ones it reported, and the first
# CHECK_REPS replications of each cell are checked against the package.
#
# The design is the cross-sectional one of pass-a-size-power.R,
# y = 1 + 2 x1 + 0.5 x2 + sigma_i e_i with x1 uniform on (1, 5) and x2
# standard normal, fitted as lm(y ~ x1 + x2). Block 1, size: sigma_i = 1, and
# Gaussian, t5 (scaled to unit variance) and centred exponential errors,
# rexp(n) - 1, at 30, 50, 100 and 200 observations. The cells with 30, 50 and
# 100 are cells 1 to 9 of wild-multiplier-size.R, with its seeds,
# seed + 1000 x cell + chunk; those with 200 observations are new, cells 13
# to 15, after that study's power cells. Block 2, power: sigma_i^2 =
# exp(0.4 x1) with Gaussian errors at 30, 50 and 100 observations, cells 10 to
# 12 of that study, at N_MC_POWER replications.
#
# With B = 999 the p-value (1 + #{T* >= T}) / (B + 1) is a multiple of 1/1000,
# and a test whose level is exact gives p < 0.05 with probability 0.049. A
# size cell is judged against the release gate, the 99% interval around 0.05,
# and against the 99% interval around 0.049. The variants test the same
# samples, so their difference is reported paired, with its standard error.
#
# Each replication starts by clearing the package's validation cache, which
# changes no value and draws no random number.

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
n_mc <- get_count("N_MC", "5000", 20L)
n_mc_power <- get_count("N_MC_POWER", "1000", 20L)
n_boot <- get_count("N_BOOT", "999", 19L)
chunk_size <- get_count("CHUNK_SIZE", "500", 1L)
check_reps <- get_count("CHECK_REPS", "2", 0L)
n_cores <- get_count("MC_CORES", "1", 1L)
min_free_gb <- as.numeric(Sys.getenv("MIN_FREE_GB", unset = "4"))
# The seed of wild-multiplier-size.R.
seed <- 20261015L
parts_dir <- Sys.getenv("PARTS", unset = file.path(tempdir(), "wild-no-multiplier-parts"))
dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "wild-no-multiplier-size.csv")
  } else {
    "wild-no-multiplier-size.csv"
  }
)
rds_path <- Sys.getenv("RDS", unset = "")
settings <- list(n_mc = n_mc, n_mc_power = n_mc_power, n_boot = n_boot,
                 chunk_size = chunk_size, check_reps = check_reps, seed = seed,
                 md5 = script_md5)
variants <- c("rademacher", "mammen", "none")

# --- errors and design, as in wild-multiplier-size.R ----------------------------

draw_errors <- function(n, errors) {
  switch(errors,
    normal = rnorm(n),
    t5 = rt(n, df = 5) / sqrt(5 / 3),
    exponential = rexp(n) - 1,
    stop("unknown error distribution")
  )
}

make_ols <- function(n, errors, gamma = 0) {
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  d <- data.frame(
    x1 = x1, x2 = x2,
    y = 1 + 2 * x1 + 0.5 * x2 + exp(0.5 * gamma * x1) * draw_errors(n, errors)
  )
  list(model = lm(y ~ x1 + x2, data = d), data = d)
}

# --- the bootstrap, as performWildBootstrapTest() draws it ------------------------

# The multipliers of the function, with its calls to sample().
multiplier_draw <- list(
  rademacher = function(n) sample(c(-1, 1), size = n, replace = TRUE),
  mammen = function(n) {
    root5 <- sqrt(5)
    values <- c((1 - root5) / 2, (1 + root5) / 2)
    probs <- c((root5 + 1) / (2 * root5), (root5 - 1) / (2 * root5))
    sample(values, size = n, replace = TRUE, prob = probs)
  }
)

# n R^2 of the regression of each column of `u` on the design, which holds
# the constant and the regressors of the auxiliary regression.
bp_columns <- function(qx, u) {
  rss <- colSums(qr.resid(qx, u)^2)
  tss <- colSums(sweep(u, 2, colMeans(u))^2)
  nrow(u) * (1 - rss / tss)
}

# The p-values of the three variants on one fitted sample. The draws are
# those of performWildBootstrapTest() called with `rademacher` and then with
# `mammen`; `none` reuses the residuals drawn for `rademacher`.
bootstrap_p_values <- function(model) {
  residuals <- stats::residuals(model)
  design <- stats::model.matrix(model)
  predictors <- design[, -1, drop = FALSE]
  n <- length(residuals)
  observed <- heteroTests:::.compute_bp_statistic(residuals, predictors)$statistic[[1]]
  leverage <- stats::hatvalues(model)
  null_residuals <- residuals / sqrt(pmax(1 - leverage, .Machine$double.eps))
  null_residuals <- null_residuals - mean(null_residuals)
  qx <- qr(design)
  drawn <- list()
  signs <- list()
  for (m in c("rademacher", "mammen")) {
    v <- matrix(NA_real_, n, n_boot)
    r <- matrix(NA_real_, n, n_boot)
    for (b in seq_len(n_boot)) {
      v[, b] <- multiplier_draw[[m]](n)
      r[, b] <- sample(null_residuals, n, replace = TRUE)
    }
    drawn[[m]] <- r
    signs[[m]] <- v
  }
  errors <- list(
    rademacher = drawn$rademacher * signs$rademacher,
    mammen = drawn$mammen * signs$mammen,
    none = drawn$rademacher
  )
  vapply(variants, function(m) {
    stat <- bp_columns(qx, qr.resid(qx, errors[[m]])^2)
    (sum(stat >= observed) + 1) / (n_boot + 1)
  }, numeric(1))
}

pval <- function(expr) {
  r <- tryCatch(suppressWarnings(suppressMessages(expr)), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

# --- scenarios ------------------------------------------------------------------

# `cell` is the scenario of wild-multiplier-size.R for 30, 50 and 100
# observations, and its seed.
scenarios <- rbind(
  data.frame(cell = 1:9, block = "size", errors = rep(c("normal", "t5", "exponential"), 3),
             n = rep(c(30L, 50L, 100L), each = 3), gamma = 0, replications = n_mc,
             stringsAsFactors = FALSE),
  data.frame(cell = 13:15, block = "size", errors = c("normal", "t5", "exponential"),
             n = 200L, gamma = 0, replications = n_mc, stringsAsFactors = FALSE),
  data.frame(cell = 10:12, block = "power", errors = "normal", n = c(30L, 50L, 100L),
             gamma = 0.4, replications = n_mc_power, stringsAsFactors = FALSE)
)
units <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  total <- scenarios$replications[i]
  starts <- seq(1L, total, by = chunk_size)
  data.frame(scenario = i, cell = scenarios$cell[i], chunk = seq_along(starts),
             size = pmin(chunk_size, total - starts + 1L))
}))
stopifnot(max(units$chunk) < 1000L)

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

# The p-values of one chunk, one row per variant and one column per
# replication, and the checks of its first replications against the package.
run_unit <- function(k) {
  if (part_done(k)) return(invisible(k))
  wait_for_memory()
  u <- units[k, ]
  s <- scenarios[u$scenario, ]
  started <- Sys.time()
  set.seed(seed + 1000L * u$cell + u$chunk)
  p <- matrix(NA_real_, length(variants), u$size, dimnames = list(variants, NULL))
  checks <- list()
  for (r in seq_len(u$size)) {
    if (u$chunk == 1L && r <= check_reps) {
      # The package on the same sample and the same random numbers.
      state <- get(".Random.seed", envir = globalenv())
      heteroTests:::clearValidationCache()
      o <- make_ols(s$n, s$errors, s$gamma)
      package <- vapply(c("rademacher", "mammen"), function(m) {
        pval(performWildBootstrapTest(o$model, o$data, B = n_boot, distribution = m,
                                      progress = FALSE))
      }, numeric(1))
      state_after <- get(".Random.seed", envir = globalenv())
      assign(".Random.seed", state, envir = globalenv())
    }
    heteroTests:::clearValidationCache()
    o <- make_ols(s$n, s$errors, s$gamma)
    p[, r] <- tryCatch(bootstrap_p_values(o$model), error = function(e) rep(NA_real_, 3))
    if (u$chunk == 1L && r <= check_reps) {
      checks[[length(checks) + 1L]] <- data.frame(
        cell = u$cell, replication = r, multiplier = names(package), package = package,
        script = p[names(package), r],
        same_random_numbers = identical(state_after, get(".Random.seed", envir = globalenv())),
        stringsAsFactors = FALSE
      )
    }
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  saveRDS(list(unit = k, cell = u$cell, chunk = u$chunk, p = p,
               checks = if (length(checks)) do.call(rbind, checks) else NULL,
               settings = settings, elapsed = elapsed), part_file(k))
  message(sprintf("%s: unit %d (cell %d, chunk %d) in %.1f s", format(Sys.time(), "%H:%M:%S"),
                  k, u$cell, u$chunk, elapsed))
  invisible(k)
}

# The chunks run in the order UNITS gives, so that a second process can take
# them from the other end; a chunk already saved is skipped.
units_spec <- Sys.getenv("UNITS", unset = "")
index <- if (nzchar(units_spec)) {
  intersect(eval(parse(text = units_spec)), seq_len(nrow(units)))
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

interval <- function(centre) {
  half <- 2.576 * sqrt(centre * (1 - centre) / n_mc)
  c(centre - half, centre + half)
}
gate <- interval(0.05)
gate_exact <- interval(0.049)

parts <- lapply(seq_len(nrow(units)), function(k) readRDS(part_file(k)))
checks <- do.call(rbind, lapply(parts, `[[`, "checks"))
p_of <- function(i) {
  ks <- which(units$scenario == i)
  ks <- ks[order(units$chunk[ks])]
  do.call(cbind, lapply(ks, function(k) parts[[k]]$p))
}

rates <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  p <- p_of(i)
  do.call(rbind, lapply(variants, function(v) {
    ok <- p[v, ][!is.na(p[v, ])]
    rate <- mean(ok < alpha)
    data.frame(
      cell = s$cell, block = s$block, variant = v, errors = s$errors, n = s$n,
      gamma = s$gamma, bootstrap = n_boot, replications = s$replications,
      rejection_rate = rate, mc_se = sqrt(rate * (1 - rate) / length(ok)),
      effective = length(ok), failures = s$replications - length(ok),
      in_gate = if (s$block == "size") rate >= gate[1] & rate <= gate[2] else NA,
      in_interval_0.049 = if (s$block == "size") {
        rate >= gate_exact[1] & rate <= gate_exact[2]
      } else {
        NA
      },
      stringsAsFactors = FALSE
    )
  }))
}))

pairs <- list(c("none", "rademacher"), c("none", "mammen"), c("mammen", "rademacher"))
paired <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  p <- p_of(i)
  do.call(rbind, lapply(pairs, function(pr) {
    both <- !is.na(p[pr[1], ]) & !is.na(p[pr[2], ])
    a <- p[pr[1], both] < alpha
    b <- p[pr[2], both] < alpha
    d <- mean(a) - mean(b)
    se <- sqrt((mean(a != b) - d^2) / sum(both))
    data.frame(
      cell = s$cell, block = s$block, errors = s$errors, n = s$n, gamma = s$gamma,
      comparison = paste(pr, collapse = " - "), difference = d, se = se, z = d / se,
      first_alone = sum(a & !b), second_alone = sum(b & !a), paired = sum(both),
      stringsAsFactors = FALSE
    )
  }))
}))

runtime_minutes <- sum(vapply(parts, function(x) x$elapsed, numeric(1))) / 60
shown <- rates
for (i in seq_len(nrow(scenarios))) {
  x <- shown[shown$cell == scenarios$cell[i], ]
  message(sprintf("%-5s %-11s n=%3d  rademacher %.4f  mammen %.4f  none %.4f",
                  x$block[1], x$errors[1], x$n[1],
                  x$rejection_rate[x$variant == "rademacher"],
                  x$rejection_rate[x$variant == "mammen"],
                  x$rejection_rate[x$variant == "none"]))
}
utils::write.csv(rates, csv_path, row.names = FALSE)
message("wrote ", csv_path)
session <- utils::sessionInfo()
if (nzchar(rds_path)) {
  saveRDS(list(results = rates, paired = paired, checks = checks, scenarios = scenarios,
               units = units,
               settings = c(settings, list(gate = gate, gate_exact = gate_exact,
                                           runtime_minutes = runtime_minutes,
                                           package_version = as.character(
                                             utils::packageVersion("heteroTests")))),
               session = session), rds_path)
  message("wrote ", rds_path)
}
print(checks)
print(paired)
print(session)
