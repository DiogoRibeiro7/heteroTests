# Render `pass-a-size-power.csv` as the Markdown table embedded in README.md.
#
#     Rscript inst/validation/make-table.R
#
# Kept separate from the simulation so the table can be regenerated without
# re-running the Monte Carlo study.

csv <- file.path("inst", "validation", "pass-a-size-power.csv")
if (!file.exists(csv)) csv <- "pass-a-size-power.csv"
res <- utils::read.csv(csv, stringsAsFactors = FALSE)

wide <- stats::reshape(
  res[, c("test", "block", "scenario", "rejection_rate")],
  idvar = c("test", "block"), timevar = "scenario", direction = "wide"
)
names(wide) <- sub("^rejection_rate\\.", "", names(wide))

fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))

emit <- function(block, cols, headers) {
  rows <- wide[wide$block == block, , drop = FALSE]
  cols <- intersect(cols, names(rows))
  cat("| Test | ", paste(headers, collapse = " | "), " |\n", sep = "")
  cat("| --- | ", paste(rep("---:", length(cols)), collapse = " | "), " |\n", sep = "")
  for (i in seq_len(nrow(rows))) {
    cat("| ", rows$test[i], " | ",
        paste(fmt(unlist(rows[i, cols])), collapse = " | "), " |\n", sep = "")
  }
  cat("\n")
}

cat("### Cross-sectional block\n\n")
emit("cross-sectional",
     c("size_gaussian_n100", "size_gaussian_n40", "size_t5_n100",
       "power_exp_g0.4_n100", "power_quad_g0.15_n100"),
     c("Size, Gaussian n=100", "Size, Gaussian n=40", "Size, t5 n=100",
       "Power, exp n=100", "Power, quad n=100"))

cat("### Time-series block\n\n")
emit("time-series",
     c("size_gaussian_n300", "size_t5_n300", "power_arch1_a0.6_n300"),
     c("Size, Gaussian n=300", "Size, t5 n=300", "Power, ARCH(1) n=300"))

cat(sprintf("Replications: %d. Nominal level: %.2f. Monte Carlo standard error at the nominal level: %.4f.\n",
            res$n_mc[1], res$alpha[1], sqrt(0.05 * 0.95 / res$n_mc[1])))

# --- Pass B ----------------------------------------------------------------

csv_b <- file.path("inst", "validation", "pass-b-size-power.csv")
if (!file.exists(csv_b)) csv_b <- "pass-b-size-power.csv"
if (file.exists(csv_b)) {
  b <- utils::read.csv(csv_b, stringsAsFactors = FALSE)
  wide_b <- stats::reshape(
    b[, c("method", "scenario", "rejection_rate")],
    idvar = "method", timevar = "scenario", direction = "wide"
  )
  names(wide_b) <- sub("^rejection_rate\\.", "", names(wide_b))

  cat("\n### Group-variance block (Pass B)\n\n")
  cols <- setdiff(names(wide_b), "method")
  cat("| Test | ", paste(cols, collapse = " | "), " |\n", sep = "")
  cat("| --- | ", paste(rep("---:", length(cols)), collapse = " | "), " |\n", sep = "")
  for (i in seq_len(nrow(wide_b))) {
    vals <- unlist(wide_b[i, cols])
    cat("| ", wide_b$method[i], " | ",
        paste(ifelse(is.na(vals), "--", sprintf("%.3f", vals)), collapse = " | "),
        " |\n", sep = "")
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: %.2f.\n",
    b$replications[1], 0.05
  ))
}
