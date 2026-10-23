# The variants of White's, Koenker's and the Breusch-Pagan tests with fixed effects
#
# Run from the package root:
#   Rscript inst/validation/fixed-effects-variants-size.R
#
# fixed-effects-size.R measured performWhiteTest(), performKoenkerTest() and
# performBPTest() on lm(y ~ x + subject) with G subjects of T observations,
# and found that they over-reject when the subjects are many and small. Since
# 0.16.0 they warn when a factor has more levels than observations per level
# (decided by the author on 2026-10-23). This script measures, on two designs
# of that study where the three tests over-reject, 30 x 4 and 100 x 8, the
# size at the 5% level of every other route to the three statistics, to
# decide which of them should give the warning too: one that over-rejects
# there gives it, one that holds its level does not. It also counts the
# warnings each call gives, which should be one or none. A third design, a
# factor that enters only through an interaction (below), checks the tests
# the rule counts such a factor for, and the streaming White test, which
# computes there.
#
# The routes: the robust forms, performWhiteTestRobust() with
# method = "standard" and "reduced" and performBPTestRobust() with
# studentize = TRUE and FALSE, with their defaults otherwise (no bootstrap);
# the streaming tests; the deprecated performStudentizedBPTest(); the
# registered "white", "koenker", "breusch_pagan" and "student_bp", through
# cachedTest(use_cache = FALSE); and the resampling tests,
# performWhiteTestBootstrap(), performWildBootstrapTest() and
# bootstrapTestStatistic() with performKoenkerTest(), the bootstrap of the
# robust forms with bootstrap = TRUE, each with B = N_BOOT.
#
# The design is that of fixed-effects-size.R: y = 1 + 0.5 x + a_i + e with x,
# the subject effects a_i and the errors e standard normal, drawn afresh in
# every replication, fitted as lm(y ~ x + subject); the designs of a factor
# only in interactions are described where they are defined. Each design runs
# N_MC replications, 2000 by default, in chunks of CHUNK_SIZE, 250, each with
# its own seed, and each call from a seed of its own, so the results do not
# depend on how the chunks are split between processes; the resampling routes
# on 100 subjects run N_MC_BOOT_LARGE, N_MC by default. N_BOOT, 199 by
# default, is the number of bootstrap samples, small to keep the run short:
# at the 5% level a p-value (1 + #) / (B + 1) of an exact test rejects with
# probability 0.05 when B = 199. MC_CORES runs the chunks in parallel through
# parallel::mclapply() where processes can be forked. Where they cannot
# (Windows), the chunks can run as separate processes: UNITS, for example
# "1:4", runs those chunks, numbered as in the `units` table below, and saves
# their rows to the .rds file PART instead of writing the CSV; MERGE, a
# directory, reads every .rds file in it, checks that together they hold every
# chunk once, and writes the CSV; LIST prints the table and stops. CSV names
# the file written, by default inst/validation/fixed-effects-variants-size.csv.
# The evidence is N_MC_BOOT_LARGE=1000 and the other defaults, run on Windows
# as separate processes of UNITS, one PART each, then MERGE; the resampling
# routes on 100 x 8 take about 20 seconds a replication, a chunk of them about
# 100 minutes.

is_source_checkout <- file.exists("DESCRIPTION") &&
  any(grepl(
    "^Package:[[:space:]]*heteroTests[[:space:]]*$",
    readLines("DESCRIPTION", warn = FALSE)
  ))

if (requireNamespace("pkgload", quietly = TRUE) && is_source_checkout) {
  suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
} else {
  suppressPackageStartupMessages(library(heteroTests))
}
invisible(ht_set_log_level("SILENT"))
# No result and no check is answered from a cache.
options(heteroTests.cache_size = 0, heteroTests.validation_cache_size = 0)

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

n_mc <- get_count("N_MC", "2000", 1L)
n_mc_boot_large <- get_count("N_MC_BOOT_LARGE", as.character(n_mc), 1L)
chunk_size <- get_count("CHUNK_SIZE", "250", 1L)
B <- get_count("N_BOOT", "199", 19L)
n_cores <- get_count("MC_CORES", "1", 1L)
seed <- 20261023L
# The last three designs are a factor that enters only through an
# interaction, a slope for each subject: y = 1 + (0.5 + b_i) x + e with b_i
# standard normal, fitted as lm(y ~ x + x:subject), with 30 subjects of 3, 30
# of 4 and 100 of 8, all over the rule. They run the asymptotic routes only.
# On them the streaming White test computes, where a factor that is a main
# effect stops it.
designs <- list(
  d30x4 = c(G = 30, T = 4, slopes = 0), d100x8 = c(G = 100, T = 8, slopes = 0),
  d30x3_slopes = c(G = 30, T = 3, slopes = 1), d30x4_slopes = c(G = 30, T = 4, slopes = 1),
  d100x8_slopes = c(G = 100, T = 8, slopes = 1)
)

draw <- function(G, T, slopes = FALSE) {
  g <- rep(seq_len(G), each = T)
  n <- G * T
  if (slopes) {
    b <- stats::rnorm(G)[g]
    x <- stats::rnorm(n)
    y <- 1 + (0.5 + b) * x + stats::rnorm(n)
    d <- data.frame(y = y, x = x, subject = factor(g))
    return(list(d = d, m = stats::lm(y ~ x + x:subject, data = d)))
  }
  a <- stats::rnorm(G)[g]
  x <- stats::rnorm(n)
  y <- 1 + 0.5 * x + a + stats::rnorm(n)
  d <- data.frame(y = y, x = x, subject = factor(g))
  list(d = d, m = stats::lm(y ~ x + subject, data = d))
}

# The p-value of a call, the number of warnings of class
# heteroTests_fixed_effects_warning it gave, and its refusal, if any. The
# other warnings, such as that of a deprecated name, are muffled.
run <- function(expr) {
  fe <- 0L
  value <- tryCatch(
    withCallingHandlers(
      suppressMessages(expr),
      heteroTests_fixed_effects_warning = function(w) {
        fe <<- fe + 1L
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) e
  )
  if (inherits(value, "error")) {
    return(list(p = NA_real_, fe = fe, error = conditionMessage(value)))
  }
  list(p = value, fe = fe, error = "")
}

asymptotic <- list(
  white = function(m, d) performWhiteTest(m, d)$p.value,
  koenker = function(m, d) performKoenkerTest(m, d)$p.value,
  breusch_pagan = function(m, d) performBPTest(m, d)$p.value,
  white_robust_standard = function(m, d) performWhiteTestRobust(m, d)$p.value,
  white_robust_reduced = function(m, d) performWhiteTestRobust(m, d, method = "reduced")$p.value,
  bp_robust_studentize = function(m, d) performBPTestRobust(m, d)$p.value,
  bp_robust_classical = function(m, d) performBPTestRobust(m, d, studentize = FALSE)$p.value,
  white_streaming = function(m, d) performWhiteTestStreaming(m, d, progress = FALSE)$p.value,
  bp_streaming = function(m, d) performBPTestStreaming(m, d, progress = FALSE)$p.value,
  koenker_streaming = function(m, d) performKoenkerTestStreaming(m, d, progress = FALSE)$p.value,
  studentized_bp = function(m, d) performStudentizedBPTest(m, d)$p.value,
  registry_white = function(m, d) cachedTest("white", m, d, use_cache = FALSE)$p.value,
  registry_koenker = function(m, d) cachedTest("koenker", m, d, use_cache = FALSE)$p.value,
  registry_breusch_pagan = function(m, d) {
    cachedTest("breusch_pagan", m, d, use_cache = FALSE)$p.value
  },
  registry_student_bp = function(m, d) cachedTest("student_bp", m, d, use_cache = FALSE)$p.value
)

bootstrap <- list(
  white_bootstrap = function(m, d) performWhiteTestBootstrap(m, d, B = B)$p.value,
  wild_bootstrap = function(m, d) {
    performWildBootstrapTest(m, d, B = B, progress = FALSE)$p.value
  },
  bootstrap_statistic_koenker = function(m, d) {
    bootstrapTestStatistic(performKoenkerTest, m, d, B = B, progress = FALSE)$p_value
  }
)

# The chunks: a design, a group of routes and a chunk of replications, the
# costly ones first.
# The replications of a design and group of routes: N_MC_BOOT_LARGE for the
# resampling routes on 100 subjects, N_MC otherwise.
replications_of <- function(design, group) {
  if (group == "bootstrap" && designs[[design]][["G"]] >= 100) n_mc_boot_large else n_mc
}
units <- do.call(rbind, lapply(names(designs), function(design) {
  do.call(rbind, lapply(c("bootstrap", "asymptotic"), function(group) {
    chunks <- seq_len(ceiling(replications_of(design, group) / chunk_size))
    data.frame(chunk = chunks, group = group, design = design, stringsAsFactors = FALSE)
  }))
}))
units <- units[!(grepl("_slopes$", units$design) & units$group == "bootstrap"), ]
cost <- c(d100x8.bootstrap = 15, d30x4.bootstrap = 2.7, d100x8.asymptotic = 0.6,
          d30x4.asymptotic = 0.4, d100x8_slopes.asymptotic = 0.5,
          d30x4_slopes.asymptotic = 0.35, d30x3_slopes.asymptotic = 0.3)
units <- units[order(-cost[paste(units$design, units$group, sep = ".")], units$design,
                     units$chunk), c("design", "group", "chunk")]
rownames(units) <- NULL

run_unit <- function(k) {
  u <- units[k, ]
  design_id <- match(u$design, names(designs))
  gt <- designs[[u$design]]
  fns <- if (u$group == "bootstrap") bootstrap else asymptotic
  size <- min(chunk_size, replications_of(u$design, u$group) - (u$chunk - 1L) * chunk_size)
  started <- Sys.time()
  rows <- lapply(seq_len(size), function(r) {
    # The data of replication r of the chunk depend on the design, the chunk
    # and r only; each route then runs from a seed of its own.
    set.seed(seed + 100000L * design_id + 1000L * u$chunk + r)
    s <- draw(gt[["G"]], gt[["T"]], slopes = gt[["slopes"]] == 1)
    do.call(rbind, lapply(names(fns), function(v) {
      set.seed(seed + 7L * r + match(v, names(fns)))
      out <- run(fns[[v]](s$m, s$d))
      data.frame(design = u$design, chunk = u$chunk, replication = r, variant = v,
                 p = out$p, fe_warnings = out$fe, error = out$error,
                 stringsAsFactors = FALSE)
    }))
  })
  message(sprintf("%s: %s %s chunk %d in %.1f min", format(Sys.time(), "%H:%M:%S"), u$design,
                  u$group, u$chunk, as.numeric(difftime(Sys.time(), started, units = "mins"))))
  do.call(rbind, rows)
}

# --- run, or merge the parts -----------------------------------------------------

if (nzchar(Sys.getenv("LIST"))) {
  print(units)
  quit(save = "no")
}
merge_dir <- Sys.getenv("MERGE", unset = "")
units_spec <- Sys.getenv("UNITS", unset = "")
if (nzchar(merge_dir)) {
  rows <- do.call(rbind, lapply(list.files(merge_dir, pattern = "\\.rds$", full.names = TRUE),
                                readRDS))
} else {
  index <- if (nzchar(units_spec)) {
    eval(parse(text = units_spec))
  } else {
    seq_len(nrow(units))
  }
  done <- if (n_cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(index, run_unit, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    lapply(index, run_unit)
  }
  rows <- do.call(rbind, done)
  if (nzchar(units_spec)) {
    part <- Sys.getenv("PART", unset = "")
    if (!nzchar(part)) {
      stop("UNITS needs PART, the .rds file its rows are saved to.")
    }
    saveRDS(rows, part)
    message("wrote ", part)
    quit(save = "no")
  }
}

# Every chunk once, every replication once per route.
expected <- do.call(rbind, lapply(seq_len(nrow(units)), function(k) {
  u <- units[k, ]
  fns <- if (u$group == "bootstrap") bootstrap else asymptotic
  size <- min(chunk_size, replications_of(u$design, u$group) - (u$chunk - 1L) * chunk_size)
  expand.grid(design = u$design, chunk = u$chunk, replication = seq_len(size),
              variant = names(fns), stringsAsFactors = FALSE)
}))
key <- function(x) paste(x$design, x$chunk, x$replication, x$variant)
if (anyDuplicated(key(rows)) || !setequal(key(rows), key(expected))) {
  stop("The rows do not hold every replication of every chunk once.")
}

# --- summary --------------------------------------------------------------------

variants <- c(names(asymptotic), names(bootstrap))
out <- do.call(rbind, lapply(names(designs), function(design) {
  ran <- intersect(variants, rows$variant[rows$design == design])
  do.call(rbind, lapply(ran, function(v) {
    x <- rows[rows$design == design & rows$variant == v, ]
    ok <- !is.na(x$p)
    rate <- function(a) if (any(ok)) mean(x$p[ok] < a) else NA_real_
    data.frame(
      design = design, G = designs[[design]][["G"]], T = designs[[design]][["T"]],
      n = designs[[design]][["G"]] * designs[[design]][["T"]], variant = v,
      B = if (v %in% names(bootstrap)) B else NA_integer_,
      replications = nrow(x), effective = sum(ok),
      reject_01 = rate(0.01), reject_05 = rate(0.05), reject_10 = rate(0.10),
      mc_se_05 = if (any(ok)) sqrt(rate(0.05) * (1 - rate(0.05)) / sum(ok)) else NA_real_,
      fe_warnings_min = min(x$fe_warnings), fe_warnings_max = max(x$fe_warnings),
      refusal = if (any(!ok)) x$error[!ok][1] else "",
      stringsAsFactors = FALSE
    )
  }))
}))

for (i in seq_len(nrow(out))) {
  o <- out[i, ]
  message(sprintf("%-7s %-28s reject 5%% %6.4f (n = %d)  warnings %d to %d %s", o$design,
                  o$variant, o$reject_05, o$effective, o$fe_warnings_min, o$fe_warnings_max,
                  if (nzchar(o$refusal)) paste("refused:", o$refusal) else ""))
}
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "fixed-effects-variants-size.csv")
  } else {
    "fixed-effects-variants-size.csv"
  }
)
utils::write.csv(out, csv_path, row.names = FALSE)
message("wrote ", csv_path)
print(utils::sessionInfo())
