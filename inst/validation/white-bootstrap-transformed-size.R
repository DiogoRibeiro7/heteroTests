# The White bootstrap on a transformed response ---------------------------------
#
# Run from the package root:
#   Rscript inst/validation/white-bootstrap-transformed-size.R
#
# N_MC sets the replications of the size cell and N_MC_POWER those of the
# power cell; e.g. N_MC=100 N_MC_POWER=40 Rscript ... for an exploratory run.
# The evidence uses N_MC=5000 and N_MC_POWER=400 with B = 199. With B = 199 a
# test whose level is exact gives p < 0.05 with probability 9 / 200 = 0.045,
# and at 5000 replications the 99% interval around that is [0.037, 0.053].
# Each cell is cut into chunks of 250 replications, each with its own seed,
# so the results do not depend on MC_CORES, which runs the chunks in
# parallel, MC_CORES at a time: through parallel::mclapply() where processes
# can be forked and a socket cluster on Windows. Before each set of chunks
# the script waits until the machine has MIN_FREE_GB of free memory, 4 by
# default, where it can tell.
#
# performWhiteTestBootstrap() regenerates the response and refits the model.
# For a transformed response, such as log(y) in log(y) ~ x + z, it wrote the
# regenerated values into a column named "log(y)", which the refit did not
# read: it computed log(y) from y again, so every replicate was the data of
# the fit, every replicate statistic was the observed one, and the
# p-value was 1 whatever the data. tests/testthat/test-resampling-refits.R
# shows that on a single sample; nothing needs simulating to know that such
# a test never rejects.
#
# The design: log(y) = 1 + 0.3 x + 0.1 z + s(x) e with x and z uniform on
# (1, 5), e standard normal and 100 observations, fitted as
# lm(log(y) ~ x + z). The size cell has s(x) = 0.3, constant variance on the
# scale of the model; the power cell has s(x) = 0.1 x.
#
# Each replication starts by clearing the package's validation cache, which
# changes no value.

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

alpha <- 0.05
n_mc <- get_count("N_MC", "5000", 20L)
n_mc_power <- get_count("N_MC_POWER", "400", 20L)
n_cores <- get_count("MC_CORES", "1", 1L)
min_free_gb <- as.numeric(Sys.getenv("MIN_FREE_GB", unset = "4"))
n_boot <- 199L
n_obs <- 100L
chunk_size <- 250L
seed <- 20261012L

make_log_response <- function(sd_fun) {
  x <- runif(n_obs, 1, 5)
  z <- runif(n_obs, 1, 5)
  d <- data.frame(x = x, z = z)
  d$y <- exp(1 + 0.3 * x + 0.1 * z + sd_fun(x) * rnorm(n_obs))
  list(model = lm(log(y) ~ x + z, data = d), data = d)
}

scenarios <- data.frame(
  cell = c("size", "power"),
  errors = c("sd = 0.3 on the log scale", "sd = 0.1 x on the log scale"),
  replications = c(n_mc, n_mc_power),
  stringsAsFactors = FALSE
)
sd_of <- list(size = function(x) rep(0.3, length(x)), power = function(x) 0.1 * x)

units <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  total <- scenarios$replications[i]
  starts <- seq(1L, total, by = chunk_size)
  data.frame(scenario = i, chunk = seq_along(starts),
             size = pmin(chunk_size, total - starts + 1L))
}))

pval <- function(fn) {
  r <- tryCatch(suppressWarnings(suppressMessages(fn())), error = function(e) NULL)
  if (is.null(r) || is.null(r$p.value)) return(NA_real_)
  as.numeric(r$p.value)[1]
}

run_chunk <- function(k) {
  u <- units[k, ]
  s <- scenarios[u$scenario, ]
  set.seed(seed + 1000L * u$scenario + u$chunk)
  vapply(seq_len(u$size), function(rep) {
    heteroTests:::clearValidationCache()
    o <- make_log_response(sd_of[[s$cell]])
    pval(function() performWhiteTestBootstrap(o$model, o$data, B = n_boot))
  }, numeric(1))
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

# The chunks run MC_CORES at a time, each set after a check of free memory.
# mclapply() forks afresh for every chunk; on Windows, where it cannot fork,
# a socket cluster is started for every set, each worker loading the package
# itself, from the same directory, and receiving the functions and settings
# defined above.
index <- seq_len(nrow(units))
results <- vector("list", length(index))
for (batch in split(index, ceiling(seq_along(index) / n_cores))) {
  wait_for_memory()
  done <- if (n_cores == 1L) {
    lapply(batch, run_chunk)
  } else if (.Platform$OS.type != "windows") {
    parallel::mclapply(batch, run_chunk, mc.cores = n_cores, mc.preschedule = FALSE)
  } else {
    # A worker that cannot start leaves the master waiting for it, for as long
    # as `timeout` allows; an hour is longer than any chunk takes. The cluster
    # is tried again, after another check of free memory, before giving up.
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
}

# A worker that was killed returns NULL and one that failed returns the error,
# and c() would drop the first without a word.
complete <- length(results) == length(index) &&
  all(vapply(seq_along(index), function(k) {
    is.numeric(results[[k]]) && length(results[[k]]) == units$size[k]
  }, logical(1)))
if (!complete) {
  stop("Not every chunk returned a result.")
}

out <- do.call(rbind, lapply(seq_len(nrow(scenarios)), function(i) {
  s <- scenarios[i, ]
  mine <- which(units$scenario == i)
  p <- unlist(results[mine[order(units$chunk[mine])]], use.names = FALSE)
  ok <- p[!is.na(p)]
  rate <- if (length(ok)) mean(ok < alpha) else NA_real_
  message(sprintf("%-6s %-28s n_mc=%4d reject %.4f", s$cell, s$errors, length(p), rate))
  data.frame(
    test = "performWhiteTestBootstrap",
    response = "log(y)",
    cell = s$cell,
    errors = s$errors,
    n = n_obs,
    B = n_boot,
    replications = length(p),
    rejection_rate = rate,
    mc_se = if (length(ok)) sqrt(rate * (1 - rate) / length(ok)) else NA_real_,
    effective = length(ok),
    failures = length(p) - length(ok),
    stringsAsFactors = FALSE
  )
}))
path <- file.path("inst", "validation", "white-bootstrap-transformed-size.csv")
if (!dir.exists(dirname(path))) path <- "white-bootstrap-transformed-size.csv"
utils::write.csv(out, path, row.names = FALSE)
message("\nwrote ", path)
