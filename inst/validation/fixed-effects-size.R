# Size of White's, Koenker's and the Breusch-Pagan tests with fixed effects ------
#
# Run from the package root:
#   Rscript inst/validation/fixed-effects-size.R
#
# N_MC sets the replications of every cell and N_MC_LARGE those of the cells
# with 100 subjects or more (N_MC by default); e.g. N_MC=100 Rscript ... for
# an exploratory run. The evidence uses N_MC=2000, the default, where the 99%
# interval around a true 5% is [0.037, 0.063], and CHECK_REPS=5; a second run
# with N_MC=10000, on the same seeds, so that its first 2000 replications of
# each cell are those of the first, recounts the rules of
# analyse-fixed-effects-size.R with less Monte Carlo error. Each cell is cut
# into chunks of 250 replications, each with its own seed, so the results do
# not depend on MC_CORES, which runs the chunks in parallel, MC_CORES at a
# time: through parallel::mclapply() where processes can be forked and a
# socket cluster on Windows. Before each set of chunks the script waits until
# the machine has MIN_FREE_GB of free memory, 4 by default, where it can tell.
# CHECK_REPS, 3 by default, is the number of replications of every cell
# checked against the package. CSV names the file written, by default
# inst/validation/fixed-effects-size.csv (fixed-effects-size.csv in the
# current directory when there is no inst/validation); the run with
# N_MC=10000 is inst/validation/fixed-effects-size-10000.csv. RDS, when set,
# names an .rds file that also keeps the checks against the package call by
# call, the scenarios, the settings and sessionInfo(); it is not kept in the
# repository.
#
# The tables and the search for a rule are made from the CSV by
# analyse-fixed-effects-size.R. The study decided the warning that
# performWhiteTest(), performKoenkerTest() and performBPTest() give since
# 0.16.0 on a factor with more levels than observations per level (decided by
# the author on 2026-10-23; see "Fixed effects for small groups" in their help
# pages). It was run on 0.15.0, origin/main 3cf5dca, before the warning; the
# check against the package muffles warnings, so the warning changes no row.
#
# The design: G subjects of T observations each,
#
#   y_it = 1 + 0.5 x_it + a_i + e_it,
#
# with x, the subject effects a_i and the errors e standard normal and drawn
# afresh in every replication, fitted as lm(y ~ x + subject) with `subject` a
# factor. The errors are homoscedastic, so every rejection is a false one.
# Block 1, the grid: G in 5, 10, 20, 30, 50, 100, 200 and T in 2, 3, 4, 5, 6,
# 8, 10, 15, 20, 30. White's test is not run with T = 2, where its auxiliary
# regression fits the squared residuals exactly and the test refuses it;
# Koenker's and the Breusch-Pagan tests are, where they compute.
# Block 2, unbalanced: subject i has T_i observations, the sizes 2 to 6, or
# 10 to 30 in steps of 5, repeated in turn, so that every size has G / 5
# subjects.
# Block 3, two regressors: y = 1 + 0.5 x1 + 0.5 x2 + a_i + e, fitted as
# lm(y ~ x1 + x2 + subject).
# Block 4, x correlated with the subject effects: x_it = (a_i + z_it) / sqrt(2)
# with z standard normal, so that x has unit variance and a correlation of
# 0.71 with a_i; the errors are homoscedastic as before.
#
# The p-values are the asymptotic ones of performWhiteTest(),
# performKoenkerTest() and performBPTest(), with their defaults. White's test
# includes the cross-products of the regressors when there are at most 10 of
# them (max_interactions = 10), which with one continuous regressor are the
# fits with G <= 10 subjects: its auxiliary regression then has the products
# x * subject, a slope in x for every subject, and 2G degrees of freedom,
# where with G > 10 it has the regressors and their squares only, G + 1.
#
# The statistics are computed here on matrices of replications, from the
# within (subject-demeaned) transformation, which gives the residuals of the
# fit and the auxiliary regressions on the subject dummies without forming
# them; White's regression with the products is fitted replication by
# replication. The first CHECK_REPS replications of every cell are fitted with
# lm() and given to the three functions, and the statistic, the degrees of
# freedom and the p-value must agree to 1e-8 (relative for the statistic,
# absolute for the p-value). A cell where the package refuses a test is
# recorded as refused for that test, with its message.

is_source_checkout <- file.exists("DESCRIPTION") &&
  any(grepl(
    "^Package:[[:space:]]*heteroTests[[:space:]]*$",
    readLines("DESCRIPTION", warn = FALSE),
    fixed = FALSE
  ))

load_package <- function() {
  if (requireNamespace("pkgload", quietly = TRUE) && is_source_checkout) {
    suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(heteroTests))
  }
  invisible(ht_set_log_level("SILENT"))
}
load_package()

get_count <- function(name, default, minimum) {
  value <- as.integer(Sys.getenv(name, unset = default))
  if (!is.finite(value) || value < minimum) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  value
}

n_mc <- get_count("N_MC", "2000", 20L)
n_mc_large <- get_count("N_MC_LARGE", as.character(n_mc), 20L)
check_reps <- get_count("CHECK_REPS", "3", 1L)
n_cores <- get_count("MC_CORES", "1", 1L)
min_free_gb <- as.numeric(Sys.getenv("MIN_FREE_GB", unset = "4"))
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "fixed-effects-size.csv")
  } else {
    "fixed-effects-size.csv"
  }
)
rds_path <- Sys.getenv("RDS", unset = "")
chunk_size <- 250L
seed <- 20261023L
levels_tested <- c(0.01, 0.05, 0.10)
check_tolerance <- 1e-8
tests <- c("White", "Koenker", "BP")
# The package's own limits: rvalidateModelInputs() refuses fewer observations
# (R/performWhiteTest.R, R/performKoenkerTest.R, R/performBPTest.R), White's
# test includes cross-products for at most max_interactions regressors, and
# refuses an auxiliary regression that fits the squared residuals exactly.
min_obs <- c(White = 20L, Koenker = 15L, BP = 15L)
max_interactions <- 10L
exact_tolerance <- sqrt(.Machine$double.eps)

# --- scenarios ------------------------------------------------------------------

grid_G <- c(5L, 10L, 20L, 30L, 50L, 100L, 200L)
grid_T <- c(2L, 3L, 4L, 5L, 6L, 8L, 10L, 15L, 20L, 30L)

new_scenario <- function(block, G, sizes, design, k = 1L, x_on_effect = FALSE) {
  list(block = block, G = as.integer(G), sizes = as.integer(sizes), design = design,
       k = as.integer(k), x_on_effect = x_on_effect)
}

scenarios <- c(
  unlist(lapply(grid_G, function(G) lapply(grid_T, function(T) {
    new_scenario("grid", G, rep(T, G), sprintf("%d x %d", G, T))
  })), recursive = FALSE),
  lapply(c(10L, 30L, 100L), function(G) {
    new_scenario("unbalanced", G, rep(2:6, length.out = G), sprintf("%d x (2 to 6)", G))
  }),
  lapply(c(30L, 100L, 200L), function(G) {
    new_scenario("unbalanced", G, rep(seq(10L, 30L, by = 5L), length.out = G),
                 sprintf("%d x (10 to 30)", G))
  }),
  lapply(list(c(5L, 5L), c(10L, 4L), c(10L, 8L), c(30L, 4L), c(30L, 8L), c(100L, 4L),
              c(100L, 8L)), function(gt) {
    new_scenario("two regressors", gt[1], rep(gt[2], gt[1]), sprintf("%d x %d", gt[1], gt[2]),
                 k = 2L)
  }),
  lapply(list(c(10L, 4L), c(10L, 8L), c(30L, 4L), c(30L, 8L), c(100L, 4L), c(100L, 8L)),
         function(gt) {
    new_scenario("x on the effects", gt[1], rep(gt[2], gt[1]), sprintf("%d x %d", gt[1], gt[2]),
                 x_on_effect = TRUE)
  })
)

replications_of <- function(s) if (s$G >= 100L) n_mc_large else n_mc

units <- do.call(rbind, lapply(seq_along(scenarios), function(i) {
  total <- replications_of(scenarios[[i]])
  starts <- seq(1L, total, by = chunk_size)
  data.frame(scenario = i, chunk = seq_along(starts),
             size = pmin(chunk_size, total - starts + 1L))
}))

# --- data -----------------------------------------------------------------------

# R replications of scenario s, one per column. The subject effects, the
# regressors and the errors are drawn in that order.
draw_chunk <- function(s, R) {
  g <- rep(seq_len(s$G), times = s$sizes)
  n <- length(g)
  effect <- matrix(stats::rnorm(s$G * R), s$G, R)[g, , drop = FALSE]
  x <- lapply(seq_len(s$k), function(j) matrix(stats::rnorm(n * R), n, R))
  if (s$x_on_effect) {
    x[[1]] <- (effect + x[[1]]) / sqrt(2)
  }
  y <- 1 + effect + matrix(stats::rnorm(n * R), n, R)
  for (j in seq_len(s$k)) {
    y <- y + 0.5 * x[[j]]
  }
  list(g = g, n = n, x = x, y = y)
}

# --- statistics on matrices of replications -------------------------------------

# Each column less the mean of its subject: the residuals of a regression on
# the subject dummies with an intercept.
within_groups <- function(M, g, sizes) {
  M - (rowsum(M, g) / sizes)[g, , drop = FALSE]
}

# Column by column, the residuals of V on W[[1]], ..., W[[K]], all already
# within-transformed: those of the regression on the subject dummies and the
# variables W, by the Frisch-Waugh-Lovell theorem.
within_residuals <- function(V, W) {
  K <- length(W)
  R <- ncol(V)
  cross <- array(0, c(K, K, R))
  right <- matrix(0, K, R)
  for (a in seq_len(K)) {
    right[a, ] <- colSums(W[[a]] * V)
    for (b in seq_len(a)) {
      cross[a, b, ] <- cross[b, a, ] <- colSums(W[[a]] * W[[b]])
    }
  }
  coef <- matrix(vapply(seq_len(R), function(j) {
    solve(matrix(cross[, , j], K, K), right[, j])
  }, numeric(K)), K, R)
  out <- V
  for (a in seq_len(K)) {
    out <- out - W[[a]] * rep(coef[a, ], each = nrow(V))
  }
  out
}

# White's auxiliary regression with the cross-products, for one replication:
# the squared residuals on [1, X, X^2, X_i X_j], X the regressors of the fit
# without the intercept. Collinear columns, the squares of the dummies and the
# products of two dummies, are dropped by the pivoting of qr(). Returns the
# residual sum of squares and the rank less one.
white_products_fit <- function(e2, X) {
  p <- ncol(X)
  pairs <- utils::combn(p, 2L)
  Z <- cbind(1, X, X^2, X[, pairs[1L, ], drop = FALSE] * X[, pairs[2L, ], drop = FALSE])
  fit <- qr(Z)
  c(rss = sum(qr.resid(fit, e2)^2), df = fit$rank - 1L)
}

# The three statistics, their degrees of freedom and p-values for every
# replication of a chunk. NA where the package refuses the test.
chunk_statistics <- function(s, dat) {
  g <- dat$g
  n <- dat$n
  R <- ncol(dat$y)
  G <- s$G
  sizes <- s$sizes
  within <- function(M) within_groups(M, g, sizes)
  xw <- lapply(dat$x, within)
  e <- within_residuals(within(dat$y), xw)
  e2 <- e^2
  total <- colSums((e2 - rep(colMeans(e2), each = n))^2)
  e2w <- within(e2)

  stat <- matrix(NA_real_, R, 3L, dimnames = list(NULL, tests))
  df <- matrix(NA_real_, R, 3L, dimnames = list(NULL, tests))

  # Koenker and Breusch-Pagan: the squared residuals on the regressors.
  if (n >= min_obs[["Koenker"]]) {
    rss <- colSums(within_residuals(e2w, xw)^2)
    explained <- total - rss
    stat[, "Koenker"] <- n * explained / total
    stat[, "BP"] <- 0.5 * explained / (colSums(e2) / n)^2
    df[, c("Koenker", "BP")] <- s$k + G - 1L
  }

  # White: with the cross-products when there are at most max_interactions
  # regressors, otherwise the regressors and their squares; the squares of the
  # dummies are the dummies.
  p <- s$k + G - 1L
  # Not run on the grid's pairs, the exact fit the test refuses.
  pairs_only <- s$block == "grid" && min(sizes) == 2L
  if (n >= min_obs[["White"]] && !pairs_only) {
    if (p <= max_interactions) {
      dummies <- outer(g, seq_len(G)[-1L], `==`) * 1
      fits <- vapply(seq_len(R), function(j) {
        X <- cbind(do.call(cbind, lapply(dat$x, function(v) v[, j])), dummies)
        white_products_fit(e2[, j], X)
      }, numeric(2))
      rss <- fits["rss", ]
      df[, "White"] <- fits["df", ]
    } else {
      squares <- lapply(dat$x, function(v) within(v^2))
      rss <- colSums(within_residuals(e2w, c(xw, squares))^2)
      df[, "White"] <- 2L * s$k + G - 1L
    }
    refused <- df[, "White"] + 1L >= n | rss <= exact_tolerance * total
    stat[, "White"] <- ifelse(refused, NA_real_, n * (1 - rss / total))
  }

  pvalue <- stats::pchisq(stat, df, lower.tail = FALSE)
  list(stat = stat, df = df, p = matrix(pvalue, R, 3L, dimnames = list(NULL, tests)))
}

# --- the check against the package ----------------------------------------------

package_value <- function(fn, model, data) {
  r <- tryCatch(suppressWarnings(suppressMessages(fn(model, data))), error = function(e) e)
  if (inherits(r, "error")) {
    return(list(refused = TRUE, message = conditionMessage(r), stat = NA_real_,
                df = NA_real_, p = NA_real_))
  }
  list(refused = FALSE, message = "", stat = unname(r$statistic),
       df = unname(r$parameter), p = r$p.value)
}

check_replications <- function(s, dat, mine, reps) {
  fns <- list(White = performWhiteTest, Koenker = performKoenkerTest, BP = performBPTest)
  names_x <- if (s$k == 1L) "x" else paste0("x", seq_len(s$k))
  do.call(rbind, lapply(reps, function(j) {
    d <- data.frame(y = dat$y[, j], subject = factor(dat$g))
    for (a in seq_len(s$k)) d[[names_x[a]]] <- dat$x[[a]][, j]
    model <- stats::lm(stats::reformulate(c(names_x, "subject"), response = "y"), data = d)
    # White's test on the grid's pairs is not simulated, but its refusal is
    # checked: the script's value is NA there.
    do.call(rbind, lapply(tests, function(test) {
      pkg <- package_value(fns[[test]], model, d)
      own_stat <- unname(mine$stat[j, test])
      own_df <- unname(mine$df[j, test])
      own_p <- unname(mine$p[j, test])
      data.frame(
        replication = j, test = test,
        package_refused = pkg$refused, script_refused = is.na(own_stat),
        package_message = pkg$message,
        package_statistic = pkg$stat, script_statistic = own_stat,
        package_df = pkg$df, script_df = own_df,
        statistic_rel_diff = abs(pkg$stat - own_stat) / max(1, abs(pkg$stat)),
        p_abs_diff = abs(pkg$p - own_p),
        stringsAsFactors = FALSE
      )
    }))
  }))
}

run_chunk <- function(k) {
  u <- units[k, ]
  s <- scenarios[[u$scenario]]
  set.seed(seed + 1000L * u$scenario + u$chunk)
  dat <- draw_chunk(s, u$size)
  mine <- chunk_statistics(s, dat)
  check <- if (u$chunk == 1L) check_replications(s, dat, mine, seq_len(min(check_reps, u$size)))
  list(stat = mine$stat, df = mine$df, p = mine$p, check = check)
}

# --- run ------------------------------------------------------------------------

# Free memory in GB, or NA where it cannot be read.
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

started <- Sys.time()
index <- seq_len(nrow(units))
results <- vector("list", length(index))
# The memory is checked before every set of chunks; with one core, before
# every 20 chunks, as a chunk takes seconds.
batch_size <- if (n_cores == 1L) 20L else n_cores
for (batch in split(index, ceiling(seq_along(index) / batch_size))) {
  wait_for_memory()
  done <- if (n_cores == 1L) {
    lapply(batch, run_chunk)
  } else if (.Platform$OS.type != "windows") {
    parallel::mclapply(batch, run_chunk, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    cl <- NULL
    for (attempt in 1:3) {
      cl <- tryCatch(parallel::makeCluster(min(n_cores, length(batch)), timeout = 3600),
                     error = function(e) NULL)
      if (!is.null(cl)) break
      wait_for_memory()
    }
    if (is.null(cl)) {
      stop("A socket cluster could not be started.")
    }
    parallel::clusterCall(cl, setwd, getwd())
    parallel::clusterExport(cl, ls(globalenv()), envir = globalenv())
    parallel::clusterCall(cl, load_package)
    out <- tryCatch(parallel::clusterApplyLB(cl, batch, run_chunk), error = function(e) e)
    parallel::stopCluster(cl)
    out
  }
  if (inherits(done, "error")) {
    break
  }
  results[batch] <- done
  last <- units[max(batch), ]
  message(sprintf("%s: %d of %d chunks, scenario %d (%s %s)", format(Sys.time(), "%H:%M:%S"),
                  max(batch), length(index), last$scenario, scenarios[[last$scenario]]$block,
                  scenarios[[last$scenario]]$design))
}

complete <- length(results) == length(index) &&
  all(vapply(seq_along(index), function(k) {
    is.list(results[[k]]) && is.matrix(results[[k]]$p) && nrow(results[[k]]$p) == units$size[k]
  }, logical(1)))
if (!complete) {
  stop("Not every chunk returned a result.")
}
runtime_minutes <- as.numeric(difftime(Sys.time(), started, units = "mins"))

# --- summary --------------------------------------------------------------------

checks <- do.call(rbind, lapply(seq_along(scenarios), function(i) {
  first <- which(units$scenario == i & units$chunk == 1L)
  ch <- results[[first]]$check
  if (is.null(ch) || !nrow(ch)) return(NULL)
  cbind(scenario = i, block = scenarios[[i]]$block, design = scenarios[[i]]$design, ch,
        stringsAsFactors = FALSE)
}))

out <- do.call(rbind, lapply(seq_along(scenarios), function(i) {
  s <- scenarios[[i]]
  mine <- which(units$scenario == i)
  mine <- mine[order(units$chunk[mine])]
  stat <- do.call(rbind, lapply(results[mine], `[[`, "stat"))
  df <- do.call(rbind, lapply(results[mine], `[[`, "df"))
  p <- do.call(rbind, lapply(results[mine], `[[`, "p"))
  n <- sum(s$sizes)
  do.call(rbind, lapply(tests, function(test) {
    ok <- !is.na(p[, test])
    ch <- checks[checks$scenario == i & checks$test == test, , drop = FALSE]
    not_run <- s$block == "grid" && test == "White" && min(s$sizes) == 2L
    rate <- function(a) if (any(ok)) mean(p[ok, test] < a) else NA_real_
    data.frame(
      block = s$block, design = s$design, regressors = s$k,
      x_on_effect = s$x_on_effect, G = s$G,
      T = if (length(unique(s$sizes)) == 1L) s$sizes[1] else NA_integer_,
      T_min = min(s$sizes), T_max = max(s$sizes), n = n, test = test,
      status = if (not_run) "not run (exact fit)" else if (!any(ok)) "refused" else "computed",
      package_message = if (nrow(ch) && all(ch$package_refused)) ch$package_message[1] else "",
      df = if (any(ok)) stats::median(df[ok, test]) else NA_real_,
      df_over_n = if (any(ok)) stats::median(df[ok, test]) / n else NA_real_,
      replications = nrow(p), effective = sum(ok),
      reject_01 = rate(0.01), reject_05 = rate(0.05), reject_10 = rate(0.10),
      mc_se_05 = if (any(ok)) sqrt(rate(0.05) * (1 - rate(0.05)) / sum(ok)) else NA_real_,
      mean_statistic = if (any(ok)) mean(stat[ok, test]) else NA_real_,
      var_statistic = if (any(ok)) stats::var(stat[ok, test]) else NA_real_,
      checked = nrow(ch),
      check_agrees = if (nrow(ch)) {
        all(ch$package_refused == ch$script_refused) &&
          all(ch$statistic_rel_diff[!ch$package_refused] <= check_tolerance) &&
          all(ch$p_abs_diff[!ch$package_refused] <= check_tolerance) &&
          all(ch$package_df[!ch$package_refused] == ch$script_df[!ch$package_refused])
      } else NA,
      check_max_stat_rel_diff = if (nrow(ch) && any(!ch$package_refused)) {
        max(ch$statistic_rel_diff[!ch$package_refused])
      } else NA_real_,
      check_max_p_abs_diff = if (nrow(ch) && any(!ch$package_refused)) {
        max(ch$p_abs_diff[!ch$package_refused])
      } else NA_real_,
      stringsAsFactors = FALSE
    )
  }))
}))

for (i in seq_len(nrow(out))) {
  o <- out[i, ]
  message(sprintf("%-16s %-16s %-7s %-9s reject 5%% %6.4f  df %5s  check %s", o$block, o$design,
                  o$test, o$status, o$reject_05, format(o$df), format(o$check_agrees)))
}
if (any(out$check_agrees %in% FALSE)) {
  warning("The script disagrees with the package in ", sum(out$check_agrees %in% FALSE),
          " cells; see `checks`.", call. = FALSE)
}

session <- utils::sessionInfo()
settings <- list(n_mc = n_mc, n_mc_large = n_mc_large, check_reps = check_reps,
                 chunk_size = chunk_size, seed = seed, mc_cores = n_cores,
                 check_tolerance = check_tolerance, runtime_minutes = runtime_minutes,
                 package_version = as.character(utils::packageVersion("heteroTests")))
utils::write.csv(out, csv_path, row.names = FALSE)
message(sprintf("\n%.1f minutes; wrote %s", runtime_minutes, csv_path))
if (nzchar(rds_path)) {
  saveRDS(list(results = out, checks = checks, scenarios = scenarios, settings = settings,
               session = session), rds_path)
  message("wrote ", rds_path)
}
print(session)
