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

emit <- function(rows, cols, headers, label = "test") {
  # Drop headers alongside any column the CSV does not carry, so the two stay
  # aligned; previously only `cols` was filtered.
  present <- cols %in% names(rows)
  cols <- cols[present]
  headers <- headers[present]
  cat("| Test | ", paste(headers, collapse = " | "), " |\n", sep = "")
  cat("| --- | ", paste(rep("---:", length(cols)), collapse = " | "), " |\n", sep = "")
  for (i in seq_len(nrow(rows))) {
    cat("| ", rows[[label]][i], " | ",
        paste(fmt(unlist(rows[i, cols])), collapse = " | "), " |\n", sep = "")
  }
  cat("\n")
}

cat("### Cross-sectional block\n\n")
emit(wide[wide$block == "cross-sectional", , drop = FALSE],
     c("size_gaussian_n100", "size_gaussian_n40", "size_t5_n100",
       "power_exp_g0.4_n100", "power_quad_g0.15_n100"),
     c("Size, Gaussian n=100", "Size, Gaussian n=40", "Size, t5 n=100",
       "Power, exp n=100", "Power, quad n=100"))

cat("### Time-series block\n\n")
emit(wide[wide$block == "time-series", , drop = FALSE],
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
  # Named explicitly rather than taken from reshape's first-appearance order,
  # which made the column order depend on how the CSV rows happened to be
  # written.
  scenarios <- c("gaussian_null_n30", "gaussian_null_n15", "t5_null_n30",
                 "moderate_hetero", "strong_hetero")
  emit(wide_b, scenarios, scenarios, label = "method")
  cat(sprintf(
    "Replications: %d. Nominal level: %.2f.\n",
    b$replications[1], 0.05
  ))
}

# --- Full sweep -------------------------------------------------------------

csv_s <- file.path("inst", "validation", "full-sweep-size-power.csv")
if (!file.exists(csv_s)) csv_s <- "full-sweep-size-power.csv"
if (file.exists(csv_s)) {
  sw <- utils::read.csv(csv_s, stringsAsFactors = FALSE)
  # Flag anything more than three Monte Carlo standard errors from nominal, so
  # the reader is not left to do the arithmetic.
  alpha <- 0.05
  z <- (sw$size - alpha) / sw$size_mc_se
  # is.na(), not !is.finite(): an empirical size of exactly 0 or 1 gives an
  # estimated standard error of 0 and hence z = -Inf or +Inf. Those are the
  # broken-procedure outliers this flag exists to surface -- the bootstrap
  # p-value corrected in 0.10.0 had size 0.000 -- so they must fall through to
  # the comparison rather than be treated as unflaggable.
  flag <- ifelse(is.na(z) | abs(z) <= 3, "",
                 ifelse(z > 0, " (high)", " (low)"))

  cat("\n### Full sweep over every exported test\n\n")
  has_t5 <- "size_t5" %in% names(sw)
  cat("| Test | Alternative | Size | Size, t5 | Power |\n")
  cat("| --- | --- | ---: | ---: | ---: |\n")
  for (i in seq_len(nrow(sw))) {
    t5 <- if (has_t5 && !is.na(sw$size_t5[i])) sprintf("%.3f", sw$size_t5[i]) else "--"
    cat(sprintf("| `%s()` | %s | %.3f%s | %s | %.3f |\n",
                sw$test[i], sw$alternative[i], sw$size[i], flag[i], t5,
                sw$power[i]))
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: %.2f. Monte Carlo standard error at the nominal level: %.4f.\n",
    sw$replications[1], alpha, sqrt(alpha * (1 - alpha) / sw$replications[1])))

  # The prose summary is generated rather than written by hand: an earlier
  # revision of the README quoted counts taken from a different run and a z
  # computed against the nominal standard error instead of the estimated one,
  # and both were wrong. Anything the README asserts about this table is
  # produced here, from the same CSV the table comes from.
  hetero <- sw$family == "hetero"
  inside <- !is.na(z) & abs(z) <= 3
  outside <- which(!is.na(z) & !inside)

  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(
    "The sweep covers %d exported tests: %d heteroscedasticity diagnostics and %d with other nulls (%s). ",
    nrow(sw), sum(hetero), sum(!hetero),
    paste(sort(unique(sw$family[!hetero])), collapse = ", ")))
  cat(sprintf(
    "%d of the %d heteroscedasticity tests fall within three Monte Carlo standard errors of the nominal %.2f.\n",
    sum(hetero & inside), sum(hetero), alpha))

  if (length(outside)) {
    cat("\nOutside that band:\n\n")
    for (i in outside) {
      cat(sprintf("- `%s()`, size %.3f, %.2f standard errors %s nominal.\n",
                  sw$test[i], sw$size[i], abs(z[i]),
                  if (z[i] > 0) "above" else "below"))
    }
  }
  # Tail sensitivity is the point of the second null: the normal-theory
  # statistics assume a fourth moment that t_5 barely supplies, and this is
  # where they separate from the rank- and median-based ones.
  if (has_t5) {
    zt <- (sw$size_t5 - alpha) / sw$size_t5_mc_se
    # Two-sided, and non-finite counts as failing, to match the Gaussian block
    # above. One-sided would have counted a badly conservative result as
    # holding its level, and an empirical size of exactly zero gives an
    # estimated standard error of zero and hence zt = -Inf, which a `zt <= 3`
    # comparison passes.
    ok <- hetero & !is.na(zt) & is.finite(zt) & abs(zt) <= 3
    fragile <- which(hetero & !is.na(zt) & (!is.finite(zt) | abs(zt) > 3))
    cat(sprintf(
      "\nUnder a homoscedastic t5 null, %d of the %d heteroscedasticity tests hold their level, meaning they land within three standard errors of nominal in either direction.",
      sum(ok), sum(hetero & !is.na(zt))))
    if (length(fragile)) {
      cat(" These do not:\n\n")
      for (i in fragile[order(-sw$size_t5[fragile])]) {
        cat(sprintf("- `%s()`, %.3f against a nominal %.2f, %s-rejecting.\n",
                    sw$test[i], sw$size_t5[i], alpha,
                    if (zt[i] < 0) "under" else "over"))
      }
    } else {
      cat("\n")
    }
  }

  near <- which(!is.na(z) & inside & abs(z) > 2 & hetero)
  if (length(near)) {
    cat("\nInside the band but worth naming, at more than two standard errors:\n\n")
    for (i in near) {
      cat(sprintf("- `%s()`, size %.3f, %.2f standard errors %s nominal.\n",
                  sw$test[i], sw$size[i], abs(z[i]),
                  if (z[i] > 0) "above" else "below"))
    }
  }
}

# --- Weighted fits ------------------------------------------------------------

csv_w <- file.path("inst", "validation", "weighted-fits-size.csv")
if (!file.exists(csv_w)) csv_w <- "weighted-fits-size.csv"
if (file.exists(csv_w)) {
  wf <- utils::read.csv(csv_w, stringsAsFactors = FALSE)
  rate <- function(x) ifelse(is.na(x), "refused", sprintf("%.3f", x))

  cat("\n### Weighted fits, known weights\n\n")
  known <- wf[wf$weights == "known", , drop = FALSE]
  cat("| Test | Rejection rate |\n")
  cat("| --- | ---: |\n")
  for (i in seq_len(nrow(known))) {
    cat(sprintf("| `%s()` | %s |\n", known$test[i], rate(known$rejection_rate[i])))
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    known$replications[1], sqrt(0.05 * 0.95 / known$replications[1])))

  cat("\n### Weighted fits, weights estimated by `fitWLS()`\n\n")
  est <- wf[wf$weights == "estimated", , drop = FALSE]
  est$cell <- paste(est$errors, est$n, sep = "_")
  wide_w <- stats::reshape(
    est[, c("test", "cell", "rejection_rate")],
    idvar = "test", timevar = "cell", direction = "wide"
  )
  cat("| Test | Gaussian, n = 150 | Gaussian, n = 600 | t5, n = 150 | t5, n = 600 |\n")
  cat("| --- | ---: | ---: | ---: | ---: |\n")
  for (i in seq_len(nrow(wide_w))) {
    cat(sprintf("| `%s()` | %s | %s | %s | %s |\n", wide_w$test[i],
                rate(wide_w$rejection_rate.normal_150[i]),
                rate(wide_w$rejection_rate.normal_600[i]),
                rate(wide_w$rejection_rate.t5_150[i]),
                rate(wide_w$rejection_rate.t5_600[i])))
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    est$replications[1], sqrt(0.05 * 0.95 / est$replications[1])))

  wrong <- wf[wf$weights == "misspecified", , drop = FALSE]
  if (nrow(wrong) > 0L) {
    cat("\n### Weighted fits, `fitWLS()` with the wrong variance function\n\n")
    wide_m <- stats::reshape(
      wrong[, c("test", "n", "rejection_rate")],
      idvar = "test", timevar = "n", direction = "wide"
    )
    cat("| Test | n = 150 | n = 600 |\n")
    cat("| --- | ---: | ---: |\n")
    for (i in seq_len(nrow(wide_m))) {
      cat(sprintf("| `%s()` | %s | %s |\n", wide_m$test[i],
                  rate(wide_m$rejection_rate.150[i]), rate(wide_m$rejection_rate.600[i])))
    }
    cat(sprintf("\nReplications: %d. Gaussian errors.\n", wrong$replications[1]))
  }
}

# --- Variance form ------------------------------------------------------------

csv_v <- file.path("inst", "validation", "variance-form-size-power.csv")
if (!file.exists(csv_v)) csv_v <- "variance-form-size-power.csv"
if (file.exists(csv_v)) {
  vf <- utils::read.csv(csv_v, stringsAsFactors = FALSE)
  vf$key <- paste(vf$form, vf$truth, sep = "|")
  rate <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))

  for (errors in c("normal", "t5")) {
    block <- vf[vf$errors == errors, , drop = FALSE]
    wide_v <- stats::reshape(
      block[, c("key", "form", "truth_label", "hypothesis", "n", "rejection_rate")],
      idvar = c("key", "form", "truth_label", "hypothesis"),
      timevar = "n", direction = "wide"
    )
    cat(sprintf("\n### Variance form, %s errors\n\n",
                if (errors == "normal") "Gaussian" else "t5"))
    cat("| Form tested | True variance | Hypothesis | n = 50 | n = 150 | n = 400 |\n")
    cat("| --- | --- | --- | ---: | ---: | ---: |\n")
    for (i in seq_len(nrow(wide_v))) {
      cat(sprintf("| %s | `%s` | %s | %s | %s | %s |\n",
                  wide_v$form[i], wide_v$truth_label[i], wide_v$hypothesis[i],
                  rate(wide_v$rejection_rate.50[i]), rate(wide_v$rejection_rate.150[i]),
                  rate(wide_v$rejection_rate.400[i])))
    }
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    vf$replications[1], sqrt(0.05 * 0.95 / vf$replications[1])))

  # The generated summary, for the same reason the full sweep has one.
  null_rows <- vf[vf$hypothesis == "null", , drop = FALSE]
  gaussian <- null_rows$errors == "normal"
  # The release gate of this README: the approximate 99% interval around the
  # nominal level, [0.042, 0.058] at 5000 replications.
  half <- stats::qnorm(0.995) * sqrt(0.05 * 0.95 / null_rows$replications)
  in_gate <- abs(null_rows$rejection_rate - 0.05) <= half
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(
    "%d of the %d null cells with Gaussian errors, and %d of the %d with t5 errors, fall inside the release-gate interval [%.3f, %.3f]. Across all %d null cells the rejection rate runs from %.3f to %.3f.\n",
    sum(gaussian & in_gate), sum(gaussian), sum(!gaussian & in_gate), sum(!gaussian),
    0.05 - half[1], 0.05 + half[1],
    nrow(null_rows), min(null_rows$rejection_rate), max(null_rows$rejection_rate)))
}

# --- Survey designs -----------------------------------------------------------

csv_s <- file.path("inst", "validation", "survey-designs-size.csv")
if (!file.exists(csv_s)) csv_s <- "survey-designs-size.csv"
if (file.exists(csv_s)) {
  sv <- utils::read.csv(csv_s, stringsAsFactors = FALSE)
  procedures <- c(
    ols = "`runSurveyHeteroTests()`",
    weights_as_precision = "Sampling weights as precision weights",
    design_wald = "Design-based Wald test (not in the package)",
    design_score = "Design-based score test (not in the package)"
  )
  designs <- c(
    ignorable = "Ignorable",
    ignorable_strong = "Ignorable, strong",
    stratified = "Stratified",
    informative = "Informative",
    clustered = "Clustered",
    heteroscedastic = "Power"
  )
  tests <- c(koenker = "Koenker", white = "White")
  for (test in names(tests)) {
    for (n in sort(unique(sv$n))) {
      cat(sprintf("\n### Survey designs, %s regressors, n = %d\n\n", tests[[test]], n))
      cat("| Procedure | ", paste(designs, collapse = " | "), " |\n", sep = "")
      cat("| --- | ", paste(rep("---:", length(designs)), collapse = " | "), " |\n", sep = "")
      for (procedure in names(procedures)) {
        cells <- vapply(names(designs), function(design) {
          hit <- sv$test == test & sv$n == n & sv$procedure == procedure &
            sv$design == design
          if (any(hit)) sprintf("%.3f", sv$rejection_rate[hit][1L]) else "--"
        }, character(1))
        cat("| ", procedures[[procedure]], " | ", paste(cells, collapse = " | "), " |\n", sep = "")
      }
    }
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    sv$replications[1], sqrt(0.05 * 0.95 / sv$replications[1])))
}
