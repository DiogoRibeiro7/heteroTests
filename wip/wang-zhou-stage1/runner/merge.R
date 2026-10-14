# Merge the per-scenario CSVs of a queue run into one CSV, in scenario order.
# Usage: Rscript merge.R <dir> <expected scenarios, e.g. 1:81> <out.csv> [<override dir>]
# Rows of scenarios present in <override dir> replace those of <dir>.
args <- commandArgs(trailingOnly = TRUE)
dir <- args[1]
expected <- eval(parse(text = args[2]))
out <- args[3]
override <- if (length(args) >= 4) args[4] else NULL
read_dir <- function(d) {
  files <- list.files(d, pattern = "^part-[0-9]+\\.csv$", full.names = TRUE)
  do.call(rbind, lapply(files, function(f) { h <- names(utils::read.csv(f, nrows = 1)); cc <- ifelse(h == "table", "character", NA); utils::read.csv(f, stringsAsFactors = FALSE, colClasses = cc) }))
}
x <- read_dir(dir)
if (!is.null(override)) {
  y <- read_dir(override)
  x <- rbind(x[!x$scenario %in% y$scenario, ], y)
}
missing <- setdiff(expected, unique(x$scenario))
if (length(missing)) stop("missing scenarios: ", paste(missing, collapse = ", "))
extra <- setdiff(unique(x$scenario), expected)
if (length(extra)) stop("unexpected scenarios: ", paste(extra, collapse = ", "))
x <- x[order(x$scenario, seq_len(nrow(x))), ]
utils::write.csv(x, out, row.names = FALSE)
cat("wrote", nrow(x), "rows for", length(unique(x$scenario)), "scenarios to", out, "\n")
