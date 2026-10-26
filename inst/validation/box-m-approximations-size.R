# Box's M: the chi-square and the F approximation -------------------------------
#
# Run from the package root:
#   Rscript inst/validation/box-m-approximations-size.R
#
# N_MC sets the replications of every cell, 5000 by default, where the 99%
# interval around a true 5% is the release gate, [0.042, 0.058]. CHUNK_SIZE,
# 1000 by default, cuts each cell into chunks, each with its own seed,
# seed + 1000 x cell + chunk, so the results do not depend on how many
# processes ran them. PARTS names a directory where every chunk is saved as
# unit-NNNN.rds as soon as it is done, with the settings and the md5 of this
# script; a chunk whose file is there, written by the same script with the
# same settings, is not run again, so a run that was stopped resumes where it
# stopped. UNITS, for example "1:100", runs only those chunks, numbered as in
# the `units` table, so that separate processes can share the work on
# Windows; MC_CORES runs the chunks in parallel through parallel::mclapply()
# where processes can be forked. Before each chunk the script waits until the
# machine has MIN_FREE_GB of free memory, 4 by default, where it can tell.
# When every chunk is in PARTS, the script writes CSV, by default
# inst/validation/box-m-approximations-size.csv, and RDS, when set, an .rds
# file with the rates, the checks, the settings and sessionInfo().
#
# performBoxMTest() refers M (1 - q) to a chi-square with (g - 1) k (k + 1) / 2
# degrees of freedom, with q its correction factor. Box (1949) and the texts
# that follow him (Morrison; Rencher and Christensen, 2012, section 7.3.2)
# give, with nu_i = n_i - 1 the degrees of freedom of group i, nu their sum,
# p the number of variables and g the number of groups,
#
#   M  = nu log|S_pooled| - sum_i nu_i log|S_i|
#   c1 = (sum_i 1 / nu_i - 1 / nu) (2 p^2 + 3 p - 1) / (6 (p + 1) (g - 1))
#   c2 = (sum_i 1 / nu_i^2 - 1 / nu^2) (p - 1) (p + 2) / (6 (g - 1))
#   f1 = (g - 1) p (p + 1) / 2
#
# The chi-square approximation is M (1 - c1) ~ chi-square(f1). The F
# approximation is, with f2 = (f1 + 2) / |c2 - c1^2|,
#
#   c2 > c1^2:  b = f1 / (1 - c1 - f1 / f2),  F = M / b
#   c2 < c1^2:  b = f2 / (1 - c1 + 2 / f2),   F = f2 M / (f1 (b - M))
#
# and F ~ F(f1, f2). When c2 = c1^2, f2 is infinite and the first form is the
# chi-square approximation divided by f1. The package's q is
# (2 k^2 + 3 k - 1) (g - 1) / (6 (k + 1) (g - 1)) (sum 1 / nu_i - 1 / nu), whose
# (g - 1) / (g - 1) is 1, so q = (g - 1) c1: the two agree for two groups and
# the package's statistic is smaller for more. The script measures three
# p-values on every sample: the package's (`package_chisq`), Box's chi-square
# (`box_chisq`) and Box's F (`box_f`). biotools::boxM() and heplots::boxM()
# compute Box's chi-square; DFA.CANCOR::HOMOGENEITY() computes Box's F, as
# SPSS reports it. The checks block compares the script with each that is
# installed, to 1e-8, and with stats::bartlett.test() for one variable, where
# Box's chi-square is Bartlett's test.
#
# The designs. The statistic does not change when every observation is
# multiplied by the same nonsingular matrix or a group is shifted, so under the
# Gaussian null the groups are drawn from N(0, I) without loss of generality.
# Block 1, "gaussian": g in 2, 3, 5 groups of n_i in 10, 20, 50, 100
# observations each, p in 1, 2, 4, 6, 10 variables, every cell with
# n_i - 1 >= p. Block 2, "unbalanced": Gaussian groups of sizes 15 and 60,
# 30 and 120, 10, 20 and 40, 20, 50 and 100, and 10, 15, 20, 30 and 50, with
# p in 2 and 4. Block 3, "t5": the coordinates are independent t with 5
# degrees of freedom scaled to unit variance, 3 groups of 20, 50 or 100, p in
# 2 and 4; Box's M assumes normal data and is known not to be robust to
# kurtosis. Block 4, "sweep": the design of full-sweep-size-power.R, which
# gave the size 0.012: 150 observations of y = 1 + 2 x + 0.5 z + e with x and
# z uniform on (1, 5) and e standard normal, the data (y, x), and the groups
# cut(x, 3). The null is true there, since x is uniform within each third,
# but x is uniform, with an excess kurtosis of -1.2.

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
chunk_size <- get_count("CHUNK_SIZE", "1000", 1L)
n_cores <- get_count("MC_CORES", "1", 1L)
min_free_gb <- as.numeric(Sys.getenv("MIN_FREE_GB", unset = "4"))
seed <- 20261024L
parts_dir <- Sys.getenv("PARTS", unset = file.path(tempdir(), "box-m-parts"))
dir.create(parts_dir, showWarnings = FALSE, recursive = TRUE)
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "box-m-approximations-size.csv")
  } else {
    "box-m-approximations-size.csv"
  }
)
rds_path <- Sys.getenv("RDS", unset = "")
settings <- list(n_mc = n_mc, chunk_size = chunk_size, seed = seed, md5 = script_md5)
approximations <- c("package_chisq", "box_chisq", "box_f")

# --- the two approximations -------------------------------------------------------

# The quantities of Box (1949) from the group covariance matrices.
box_parts <- function(covs, nu) {
  p <- ncol(covs[[1]])
  g <- length(covs)
  total <- sum(nu)
  pooled <- Reduce(`+`, Map(`*`, covs, nu)) / total
  logdet <- function(S) as.numeric(determinant(S, logarithm = TRUE)$modulus)
  M <- total * logdet(pooled) - sum(nu * vapply(covs, logdet, numeric(1)))
  c1 <- (sum(1 / nu) - 1 / total) * (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
  c2 <- (sum(1 / nu^2) - 1 / total^2) * (p - 1) * (p + 2) / (6 * (g - 1))
  f1 <- (g - 1) * p * (p + 1) / 2
  list(M = M, c1 = c1, c2 = c2, f1 = f1, p = p, g = g)
}

box_chisq <- function(b) {
  stat <- b$M * (1 - b$c1)
  c(statistic = stat, df = b$f1, p.value = stats::pchisq(stat, b$f1, lower.tail = FALSE))
}

box_f <- function(b) {
  f1 <- b$f1
  gap <- b$c2 - b$c1^2
  f2 <- (f1 + 2) / abs(gap)
  if (gap >= 0) {
    scale <- f1 / (1 - b$c1 - f1 / f2)
    stat <- b$M / scale
  } else {
    scale <- f2 / (1 - b$c1 + 2 / f2)
    stat <- if (b$M < scale) f2 * b$M / (f1 * (scale - b$M)) else Inf
  }
  c(statistic = stat, df1 = f1, df2 = f2, p.value = stats::pf(stat, f1, f2, lower.tail = FALSE),
    branch = if (gap >= 0) 1 else 2)
}

group_covs <- function(Y, group) {
  lapply(levels(group), function(l) stats::cov(Y[group == l, , drop = FALSE]))
}

# --- scenarios ------------------------------------------------------------------

new_cell <- function(block, sizes, p, errors) {
  data.frame(block = block, sizes = paste(sizes, collapse = ","), g = length(sizes),
             p = as.integer(p), errors = errors, stringsAsFactors = FALSE)
}
cells <- list()
for (g in c(2L, 3L, 5L)) for (p in c(1L, 2L, 4L, 6L, 10L)) for (ni in c(10L, 20L, 50L, 100L)) {
  if (ni - 1L >= p) cells[[length(cells) + 1L]] <- new_cell("gaussian", rep(ni, g), p, "normal")
}
for (sizes in list(c(15L, 60L), c(30L, 120L), c(10L, 20L, 40L), c(20L, 50L, 100L),
                   c(10L, 15L, 20L, 30L, 50L))) {
  for (p in c(2L, 4L)) cells[[length(cells) + 1L]] <- new_cell("unbalanced", sizes, p, "normal")
}
for (p in c(2L, 4L)) for (ni in c(20L, 50L, 100L)) {
  cells[[length(cells) + 1L]] <- new_cell("t5", rep(ni, 3L), p, "t5")
}
cells[[length(cells) + 1L]] <- new_cell("sweep", c(150L), 2L, "sweep")
cells <- do.call(rbind, cells)
cells$cell <- seq_len(nrow(cells))
cells$g[cells$block == "sweep"] <- 3L

units <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  starts <- seq(1L, n_mc, by = chunk_size)
  data.frame(cell = i, chunk = seq_along(starts), size = pmin(chunk_size, n_mc - starts + 1L))
}))
stopifnot(max(units$chunk) < 1000L)

# One sample of a cell: the data and the groups.
draw_sample <- function(s) {
  if (s$errors == "sweep") {
    x <- runif(150, 1, 5)
    z <- runif(150, 1, 5)
    e <- rnorm(150)
    y <- 1 + 2 * x + 0.5 * z + e
    return(list(Y = cbind(y = y, x = x), group = cut(x, 3, labels = c("a", "b", "c"))))
  }
  sizes <- as.integer(strsplit(s$sizes, ",")[[1]])
  N <- sum(sizes)
  Y <- switch(s$errors,
    normal = matrix(rnorm(N * s$p), N, s$p),
    t5 = matrix(rt(N * s$p, df = 5) / sqrt(5 / 3), N, s$p)
  )
  colnames(Y) <- paste0("v", seq_len(s$p))
  list(Y = Y, group = factor(rep(seq_along(sizes), sizes)))
}

# --- checks against the reference implementations --------------------------------

check_tolerance <- 1e-8
rel_diff <- function(a, b) abs(a - b) / max(1, abs(b))
run_checks <- function() {
  set.seed(seed - 1L)
  sets <- list(
    iris = list(Y = as.matrix(iris[, 1:4]), group = iris$Species),
    iris_two = list(Y = as.matrix(iris[iris$Species != "virginica", 1:4]),
                    group = droplevels(iris$Species[iris$Species != "virginica"]))
  )
  for (s in list(c(10, 10, 10, 4), c(20, 50, 100, 2), c(12, 30, 1), c(15, 60, 1),
                 c(10, 15, 20, 30, 50, 6), c(100, 100, 3), c(25, 25, 2), c(10, 10, 2))) {
    sizes <- s[-length(s)]
    p <- s[length(s)]
    Y <- matrix(rnorm(sum(sizes) * p), ncol = p)
    # Unequal covariance matrices, so the statistics are not all near the null.
    Y <- Y * rep(seq_along(sizes), sizes)^0.3
    colnames(Y) <- paste0("v", seq_len(p))
    sets[[paste(s, collapse = "-")]] <- list(Y = Y, group = factor(rep(seq_along(sizes), sizes)))
  }
  rows <- list()
  for (nm in names(sets)) {
    Y <- sets[[nm]]$Y
    group <- sets[[nm]]$group
    b <- box_parts(group_covs(Y, group), as.numeric(table(group)) - 1)
    chi <- box_chisq(b)
    fa <- box_f(b)
    pkg <- performBoxMTest(as.data.frame(Y), group)
    add <- function(reference, quantity, ours, theirs) {
      rows[[length(rows) + 1L]] <<- data.frame(
        data = nm, g = b$g, p = b$p, branch = fa[["branch"]], reference = reference,
        quantity = quantity, ours = ours, theirs = theirs, rel_diff = rel_diff(ours, theirs),
        agrees = rel_diff(ours, theirs) <= check_tolerance, stringsAsFactors = FALSE
      )
    }
    # The package's statistic is M (1 - (g - 1) c1).
    add("performBoxMTest", "M (1 - (g - 1) c1)", b$M * (1 - (b$g - 1) * b$c1), unname(pkg$statistic))
    add("performBoxMTest", "df", b$f1, unname(pkg$parameter))
    # Equal for two groups only; for more the package's is smaller.
    add("performBoxMTest", "Box chi-square statistic", chi[["statistic"]], unname(pkg$statistic))
    # biotools::boxM() and heplots::boxM() stop on a single variable, so they
    # are checked from two variables on.
    if (requireNamespace("biotools", quietly = TRUE) && b$p > 1) {
      r <- biotools::boxM(as.data.frame(Y), group)
      add("biotools::boxM", "chi-square statistic", chi[["statistic"]], unname(r$statistic))
      add("biotools::boxM", "chi-square p-value", chi[["p.value"]], unname(r$p.value))
    }
    if (requireNamespace("heplots", quietly = TRUE) && b$p > 1) {
      r <- heplots::boxM(Y, group)
      add("heplots::boxM", "chi-square statistic", chi[["statistic"]], unname(r$statistic))
      add("heplots::boxM", "chi-square p-value", chi[["p.value"]], unname(r$p.value))
    }
    # So does DFA.CANCOR::HOMOGENEITY(); two groups of two variables take the
    # second form of the F approximation, c2 < c1^2, as one variable does.
    if (requireNamespace("DFA.CANCOR", quietly = TRUE) && b$p > 1) {
      d <- data.frame(grp = factor(as.integer(group)), Y)
      r <- suppressMessages(DFA.CANCOR::HOMOGENEITY(d, groups = "grp", variables = colnames(Y),
                                                     verbose = FALSE))$BoxMtest
      add("DFA.CANCOR::HOMOGENEITY", "M", b$M, unname(r[1]))
      add("DFA.CANCOR::HOMOGENEITY", "F statistic", fa[["statistic"]], unname(r[2]))
      add("DFA.CANCOR::HOMOGENEITY", "df1", fa[["df1"]], unname(r[3]))
      add("DFA.CANCOR::HOMOGENEITY", "df2", fa[["df2"]], unname(r[4]))
      add("DFA.CANCOR::HOMOGENEITY", "F p-value", fa[["p.value"]], unname(r[5]))
    }
    # With one variable M is Bartlett's statistic before its correction and
    # c1 his correction, which he divides by, M / (1 + c1), where Box
    # multiplies, M (1 - c1).
    if (b$p == 1) {
      r <- stats::bartlett.test(as.numeric(Y), group)
      add("stats::bartlett.test", "M / (1 + c1)", b$M / (1 + b$c1), unname(r$statistic))
    }
  }
  do.call(rbind, rows)
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

# The p-values of one chunk: one row per replication, one column per
# approximation, and the size of the largest group of the sweep design.
run_unit <- function(k) {
  if (part_done(k)) return(invisible(k))
  wait_for_memory()
  u <- units[k, ]
  s <- cells[u$cell, ]
  started <- Sys.time()
  set.seed(seed + 1000L * u$cell + u$chunk)
  p <- matrix(NA_real_, u$size, length(approximations), dimnames = list(NULL, approximations))
  for (r in seq_len(u$size)) {
    o <- draw_sample(s)
    b <- box_parts(group_covs(o$Y, o$group), as.numeric(table(o$group)) - 1)
    p[r, "package_chisq"] <- performBoxMTest(as.data.frame(o$Y), o$group)$p.value
    p[r, "box_chisq"] <- box_chisq(b)[["p.value"]]
    p[r, "box_f"] <- box_f(b)[["p.value"]]
  }
  elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
  saveRDS(list(unit = k, cell = u$cell, chunk = u$chunk, p = p, settings = settings,
               elapsed = elapsed), part_file(k))
  message(sprintf("%s: unit %d (cell %d, chunk %d) in %.1f s", format(Sys.time(), "%H:%M:%S"),
                  k, u$cell, u$chunk, as.numeric(difftime(Sys.time(), started, units = "secs"))))
  invisible(k)
}

started <- Sys.time()
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

checks <- run_checks()
if (!all(checks$agrees)) {
  print(checks[!checks$agrees, ])
}
gate <- c(0.05 - 2.576 * sqrt(0.05 * 0.95 / n_mc), 0.05 + 2.576 * sqrt(0.05 * 0.95 / n_mc))

out <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  s <- cells[i, ]
  mine <- units$cell == i
  p <- do.call(rbind, lapply(which(mine), function(k) readRDS(part_file(k))$p))
  # The constants of the cell; the sweep's group sizes vary, so they are
  # those of three groups of 50.
  sizes <- if (s$errors == "sweep") rep(50, 3) else as.numeric(strsplit(s$sizes, ",")[[1]])
  b <- box_parts(replicate(length(sizes), diag(s$p), simplify = FALSE), sizes - 1)
  fa <- box_f(b)
  do.call(rbind, lapply(approximations, function(a) {
    do.call(rbind, lapply(levels_tested, function(level) {
      ok <- p[, a][!is.na(p[, a])]
      rate <- mean(ok < level)
      data.frame(
        cell = i, block = s$block, errors = s$errors, g = s$g, sizes = s$sizes, p = s$p,
        approximation = a, level = level, replications = nrow(p), effective = length(ok),
        rejection_rate = rate, mc_se = sqrt(rate * (1 - rate) / length(ok)),
        in_gate = if (level == alpha) rate >= gate[1] & rate <= gate[2] else NA,
        c1 = b$c1, c2 = b$c2, df1 = b$f1, df2 = fa[["df2"]], f_branch = fa[["branch"]],
        stringsAsFactors = FALSE
      )
    }))
  }))
}))

# The CPU time of the chunks, wherever they ran.
runtime_minutes <- sum(vapply(seq_len(nrow(units)), function(k) {
  e <- readRDS(part_file(k))$elapsed
  if (is.null(e)) NA_real_ else e
}, numeric(1))) / 60
shown <- out[out$level == alpha, ]
for (i in unique(shown$cell)) {
  x <- shown[shown$cell == i, ]
  message(sprintf("%-10s %-6s g=%d sizes=%-16s p=%2d  package %.4f  box chisq %.4f  box F %.4f",
                  x$block[1], x$errors[1], x$g[1], x$sizes[1], x$p[1],
                  x$rejection_rate[x$approximation == "package_chisq"],
                  x$rejection_rate[x$approximation == "box_chisq"],
                  x$rejection_rate[x$approximation == "box_f"]))
}
utils::write.csv(out, csv_path, row.names = FALSE)
message("wrote ", csv_path)
session <- utils::sessionInfo()
if (nzchar(rds_path)) {
  saveRDS(list(results = out, checks = checks, cells = cells, units = units,
               settings = c(settings, list(gate = gate, runtime_minutes = runtime_minutes,
                                           package_version = as.character(
                                             utils::packageVersion("heteroTests")))),
               session = session), rds_path)
  message("wrote ", rds_path)
}
print(checks)
print(session)
