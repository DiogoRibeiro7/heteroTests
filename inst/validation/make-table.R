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
# The row `Glejser` is Glejser's statistic, robust = FALSE, which has not been
# the default of performGlejserTest() since 0.15.0.
wide$test[wide$test == "Glejser"] <- "Glejser (`robust = FALSE`)"

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

  # The resampling tests, block 4 of weighted-fits-size.R. B is not a column of
  # the CSV: every row of the block uses n_boot = 499, set in that script.
  boot_B <- 499L
  boot_weights <- c(
    "exact" = "exact, `sd = x^2`",
    "true" = "true, `fitWLS()` design",
    "estimated, held fixed" = "`fitWLS()`, held fixed",
    "estimated, re-estimated" = "`fitWLS()`, estimated again in each replicate"
  )
  boot <- wf[wf$weights %in% c(names(boot_weights), "too weak"), , drop = FALSE]
  if (nrow(boot) > 0L) {
    boot_label <- function(test) {
      ifelse(test == "rbootstrap_test_statistic",
             "`rbootstrap_test_statistic()`, Koenker", sprintf("`%s()`", test))
    }
    size <- boot[boot$weights != "too weak", , drop = FALSE]
    sizes <- sort(unique(size$n))
    cat("\n### Weighted fits, the resampling tests, size\n\n")
    cat("| Test | Weights | ", paste(sprintf("n = %d", sizes), collapse = " | "), " |\n", sep = "")
    cat("| --- | --- | ", paste(rep("---:", length(sizes)), collapse = " | "), " |\n", sep = "")
    for (w in names(boot_weights)) {
      for (test in unique(size$test)) {
        hit <- size$weights == w & size$test == test
        if (!any(hit)) next
        cells <- vapply(sizes, function(n) {
          cell <- hit & size$n == n
          if (any(cell)) rate(size$rejection_rate[cell][1L]) else "--"
        }, character(1))
        cat("| ", boot_label(test), " | ", boot_weights[[w]], " | ",
            paste(cells, collapse = " | "), " |\n", sep = "")
      }
    }
    footer <- paste(
      "\nReplications: %d, with B = %d. Nominal level: 0.05.",
      "Monte Carlo standard error at the nominal level: %.4f.\n"
    )
    cat(sprintf(footer, size$replications[1], boot_B,
                sqrt(0.05 * 0.95 / size$replications[1])))

    power <- boot[boot$weights == "too weak", , drop = FALSE]
    if (nrow(power) > 0L) {
      cat("\n### Weighted fits, the resampling tests, power against weights that are too weak\n\n")
      cat("| Test | n = 150 |\n")
      cat("| --- | ---: |\n")
      for (i in seq_len(nrow(power))) {
        cat(sprintf("| %s | %s |\n", boot_label(power$test[i]), rate(power$rejection_rate[i])))
      }
      cat(sprintf("\nReplications: %d, with B = %d.\n", power$replications[1], boot_B))
    }

    # The generated summary. The interval is the 99% band around 0.05 at the
    # number of replications of the size rows.
    se0 <- sqrt(0.05 * 0.95 / size$replications)
    half <- stats::qnorm(0.995) * se0[1]
    z <- (size$rejection_rate - 0.05) / se0
    in_gate <- abs(size$rejection_rate - 0.05) <= half
    span <- function(keep) {
      sprintf("%.3f to %.3f", min(size$rejection_rate[keep]), max(size$rejection_rate[keep]))
    }
    koenker <- function(n) {
      est$rejection_rate[est$test == "performKoenkerTest" & est$errors == "normal" & est$n == n]
    }
    cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
    summary_text <- paste0(
      "With exact weights in the design `sd = x^2` the three resampling tests ",
      "reject %s of the time, and with the true weights of the `fitWLS()` design ",
      "%s. With the weights of `fitWLS()` held fixed they reject %s, where ",
      "`performKoenkerTest()` on the same design rejects %.3f at 150 ",
      "observations and %.3f at 600; with the weights estimated again in each ",
      "replicate they reject %s. %d of the %d size cells fall inside [%.3f, %.3f]"
    )
    cat(sprintf(
      summary_text,
      span(size$weights == "exact"), span(size$weights == "true"),
      span(size$weights == "estimated, held fixed"),
      koenker(150), koenker(600), span(size$weights == "estimated, re-estimated"),
      sum(in_gate), length(in_gate), 0.05 - half, 0.05 + half
    ))
    cell_name <- function(i) {
      sprintf("%s, weights %s, n = %d", boot_label(size$test[i]),
              boot_weights[[size$weights[i]]], size$n[i])
    }
    outside <- which(!in_gate)
    if (length(outside)) {
      cat(". Outside it:\n\n")
      for (i in outside) {
        cat(sprintf("- %s: %.4f.\n", cell_name(i), size$rejection_rate[i]))
      }
    } else {
      cat(".\n")
    }
    near <- which(in_gate & abs(z) > 2)
    if (length(near)) {
      cat("\nInside the band but worth naming, at more than two standard errors:\n\n")
      for (i in near) {
        cat(sprintf("- %s: %.4f, %.1f standard errors %s nominal.\n", cell_name(i),
                    size$rejection_rate[i], abs(z[i]), if (z[i] > 0) "above" else "below"))
      }
    }
    if (nrow(power) > 0L) {
      power_range <- range(power$rejection_rate)
      cat(sprintf("\nAgainst the weights 1 / x they reject %s of the time.\n",
                  if (power_range[1] == power_range[2]) {
                    sprintf("%.3f", power_range[1])
                  } else {
                    sprintf("%.3f to %.3f", power_range[1], power_range[2])
                  }))
    }
  }
}

# --- The resampling tests on a fit with an offset ------------------------------

csv_o <- file.path("inst", "validation", "offset-refits-size.csv")
if (!file.exists(csv_o)) csv_o <- "offset-refits-size.csv"
if (file.exists(csv_o)) {
  of <- utils::read.csv(csv_o, stringsAsFactors = FALSE)
  rate <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))
  label <- function(test) {
    ifelse(test == "rbootstrap_test_statistic",
           "`rbootstrap_test_statistic()`, Koenker", sprintf("`%s()`", test))
  }
  rows_label <- c("fitted order" = "in the order of the fit", "another order" = "in another order")
  now <- of[of$version == "corrected", , drop = FALSE]
  before <- of[of$version == "before", , drop = FALSE]
  before_rate <- function(i) {
    hit <- before$test == now$test[i] & before$terms == now$terms[i] &
      before$rows == now$rows[i] & before$B == now$B[i]
    if (any(hit)) before$rejection_rate[hit][1L] else NA_real_
  }
  now$before <- vapply(seq_len(nrow(now)), before_rate, numeric(1))

  cat("\n### The resampling tests on a fit with an offset\n\n")
  cat("| Test | Terms | Rows of the data | B | Corrected | Before |\n")
  cat("| --- | --- | --- | ---: | ---: | ---: |\n")
  for (i in seq_len(nrow(now))) {
    cat(sprintf("| %s | `%s` | %s | %d | %s | %s |\n", label(now$test[i]), now$terms[i],
                rows_label[[now$rows[i]]], now$B[i], rate(now$rejection_rate[i]),
                rate(now$before[i])))
  }
  footer <- paste(
    "\nReplications: %d for the corrected cells and %d for those before the",
    "correction. Gaussian errors, %d observations. Nominal level: 0.05.\n"
  )
  cat(sprintf(footer, now$replications[1], before$replications[1], now$n[1]))

  # The generated summary. With B replicates a test whose level is exact gives
  # p < 0.05 with probability (ceiling(0.05 (B + 1)) - 1) / (B + 1): 0.045 with
  # B = 199 and 0.049 with B = 999. A cell with B = 199 is judged against the
  # 99% interval around that rate; the cell with B = 999, whose exact rate is
  # close to 0.05, against the release gate around 0.05.
  now$exact <- (ceiling(0.05 * (now$B + 1)) - 1) / (now$B + 1)
  now$reference <- ifelse(now$B == 999L, 0.05, now$exact)
  now$se <- sqrt(now$reference * (1 - now$reference) / now$replications)
  half <- stats::qnorm(0.995) * now$se
  now$inside <- abs(now$rejection_rate - now$reference) <= half
  now$z <- (now$rejection_rate - now$reference) / now$se
  b199 <- now$B == 199L
  b999 <- now$B == 999L
  cell_name <- function(i) {
    sprintf("%s, `%s`, rows %s, B = %d", label(now$test[i]), now$terms[i],
            rows_label[[now$rows[i]]], now$B[i])
  }
  before_of <- function(test, rows) {
    rate(before$rejection_rate[before$test == test & before$rows == rows][1L])
  }
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  summary_text <- paste0(
    "With B = 199 a test whose level is exact gives p < 0.05 with probability %.3f, ",
    "and the %d corrected cells with B = 199 are judged against the 99%% interval ",
    "around it at %d replications, [%.3f, %.3f]: they reject %.3f to %.3f of the ",
    "time, and %d of them fall inside it."
  )
  cat(sprintf(summary_text, now$exact[b199][1], sum(b199), now$replications[b199][1],
              now$reference[b199][1] - half[b199][1], now$reference[b199][1] + half[b199][1],
              min(now$rejection_rate[b199]), max(now$rejection_rate[b199]),
              sum(now$inside[b199])))
  if (any(b999)) {
    i <- which(b999)[1]
    exact_half <- stats::qnorm(0.995) *
      sqrt(now$exact[i] * (1 - now$exact[i]) / now$replications[i])
    inside_exact <- abs(now$rejection_rate[i] - now$exact[i]) <= exact_half
    cat(sprintf(paste0(
      " With B = 999 the exact rate is %.3f, and the cell with B = 999, %s, is ",
      "judged against the release gate, [%.3f, %.3f]: it rejects %.4f of the ",
      "time, %s the gate, and %s the 99%% interval around its exact rate, ",
      "[%.3f, %.3f]."
    ), now$exact[i], cell_name(i), 0.05 - half[i], 0.05 + half[i], now$rejection_rate[i],
    if (now$inside[i]) "inside" else "outside", if (inside_exact) "inside" else "outside",
    now$exact[i] - exact_half, now$exact[i] + exact_half))
  }
  cat(sprintf(paste0(
    " Before the correction the White bootstrap rejected %s, and ",
    "`rbootstrap_test_statistic()` %s on the data in the order of the fit and %s on ",
    "the same rows in another order; the wild bootstrap stopped with an error"
  ),
  before_of("performWhiteTestBootstrap", "fitted order"),
  before_of("rbootstrap_test_statistic", "fitted order"),
  before_of("rbootstrap_test_statistic", "another order")))
  outside <- which(!now$inside)
  if (length(outside)) {
    cat(". Corrected cells outside their interval:\n\n")
    for (i in outside) {
      cat(sprintf("- %s: %.4f.\n", cell_name(i), now$rejection_rate[i]))
    }
  } else {
    cat(".\n")
  }
  near <- which(now$inside & abs(now$z) > 2)
  if (length(near)) {
    cat("\nInside their interval but worth naming, at more than two standard errors:\n\n")
    for (i in near) {
      cat(sprintf("- %s: %.4f, %.1f standard errors %s %.3f.\n", cell_name(i),
                  now$rejection_rate[i], abs(now$z[i]), if (now$z[i] > 0) "above" else "below",
                  now$reference[i]))
    }
  }
}

# --- The White bootstrap on a transformed response ------------------------------

csv_t <- file.path("inst", "validation", "white-bootstrap-transformed-size.csv")
if (!file.exists(csv_t)) csv_t <- "white-bootstrap-transformed-size.csv"
if (file.exists(csv_t)) {
  wt <- utils::read.csv(csv_t, stringsAsFactors = FALSE)
  cat("\n### The White bootstrap on a transformed response\n\n")
  cat("| Cell | Errors | Replications | Rejection rate |\n")
  cat("| --- | --- | ---: | ---: |\n")
  for (i in seq_len(nrow(wt))) {
    cat(sprintf("| %s | %s | %d | %.3f |\n", wt$cell[i], wt$errors[i], wt$replications[i],
                wt$rejection_rate[i]))
  }
  cat(sprintf(paste(
    "\n`performWhiteTestBootstrap()` on `lm(log(y) ~ x + z)`, B = %d, %d observations.",
    "Nominal level: 0.05.\n"
  ), wt$B[1], wt$n[1]))

  # The size cell is judged against the rate at which a test whose level is
  # exact gives p < 0.05 with this B: 9 / 200 = 0.045 with B = 199.
  size <- wt[wt$cell == "size", , drop = FALSE]
  exact <- (ceiling(0.05 * (size$B + 1)) - 1) / (size$B + 1)
  half <- stats::qnorm(0.995) * sqrt(exact * (1 - exact) / size$replications)
  inside <- abs(size$rejection_rate - exact) <= half
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(paste0(
    "With constant variance on the scale of the model the test rejects %.4f of ",
    "the time at the 5%% level. With B = %d a test whose level is exact gives ",
    "p < 0.05 with probability %.3f, and the 99%% interval around that at %d ",
    "replications is [%.3f, %.3f]; the rate is %s that interval, %.1f standard ",
    "errors %s %.3f. Against a standard deviation of 0.1 x on the log scale it ",
    "rejects %.3f of the time.\n"
  ),
  size$rejection_rate, size$B, exact, size$replications, exact - half, exact + half,
  if (inside) "inside" else "outside",
  abs(size$rejection_rate - exact) / sqrt(exact * (1 - exact) / size$replications),
  if (size$rejection_rate >= exact) "above" else "below", exact,
  wt$rejection_rate[wt$cell == "power"]))
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

# --- Glejser under asymmetric errors ------------------------------------------

csv_g <- file.path("inst", "validation", "glejser-skewed-errors.csv")
if (!file.exists(csv_g)) csv_g <- "glejser-skewed-errors.csv"
if (file.exists(csv_g)) {
  gl <- utils::read.csv(csv_g, stringsAsFactors = FALSE)
  # `robust` is named in both labels. The default was FALSE up to 0.14.0 and is
  # TRUE since 0.15.0, so a label without it would change meaning.
  tests <- c(
    glejser = "`performGlejserTest(robust = FALSE)`",
    glejser_robust = "`performGlejserTest(robust = TRUE)`",
    koenker_same_regressor = "Koenker's statistic on the same regressor",
    koenker = "`performKoenkerTest()`"
  )
  errors <- c(
    normal = "Gaussian",
    t5 = "t5",
    chisq5 = "chi-squared(5)",
    exponential = "exponential",
    exponential_left = "exponential, mirrored",
    lognormal = "lognormal"
  )
  skewness <- c(
    normal = "0", t5 = "0", chisq5 = "1.26", exponential = "2",
    exponential_left = "-2", lognormal = "6.18"
  )

  # One row per error distribution and test, one column per sample size.
  emit_glejser <- function(block, title, by_gamma = FALSE) {
    rows <- gl[gl$block == block, , drop = FALSE]
    sizes <- sort(unique(rows$n))
    cat(sprintf("\n### %s\n\n", title))
    cat("| Errors | Skewness | ", if (by_gamma) "gamma | ", "Test | ",
        paste(sprintf("n = %d", sizes), collapse = " | "), " |\n", sep = "")
    cat("| --- | ---: | ", if (by_gamma) "---: | ", "--- | ",
        paste(rep("---:", length(sizes)), collapse = " | "), " |\n", sep = "")
    for (law in intersect(names(errors), rows$errors)) {
      for (gamma in sort(unique(rows$gamma[rows$errors == law]))) {
        for (test in names(tests)) {
          cells <- vapply(sizes, function(n) {
            hit <- rows$errors == law & rows$gamma == gamma & rows$test == test & rows$n == n
            if (any(hit)) sprintf("%.3f", rows$rejection_rate[hit][1L]) else "--"
          }, character(1))
          cat("| ", errors[[law]], " | ", skewness[[law]], " | ",
              if (by_gamma) sprintf("%.1f | ", gamma), tests[[test]], " | ",
              paste(cells, collapse = " | "), " |\n", sep = "")
        }
      }
    }
  }

  emit_glejser("size", "Glejser test, size by error distribution")
  emit_glejser(
    "size, regressor outside the mean equation",
    "Glejser test, size with `transformation = \"inverse\"`"
  )
  emit_glejser("size, weighted fit", "Glejser test, size after a correctly weighted fit")
  emit_glejser("power", "Glejser test, power", by_gamma = TRUE)
  # What the asymptotic theory gives for Glejser's statistic, robust = FALSE,
  # the default up to 0.14.0. The numerator of its slope is
  # n^(-1/2) sum (z - zbar) |e-hat|. With u the error, m = E sign(u) and
  # rho^2 the R^2 of z on the regressors of the mean equation, its variance is
  #   Var(z) {Var|u| + rho^2 [m^2 Var(u) - 2 m E(u|u|)]},
  # and the t statistic assumes Var(z) Var|u|. Nothing here is simulated.
  error_moments <- function(density, centre, scale) {
    expect <- function(g) {
      stats::integrate(function(x) g((x - centre) / scale) * density(x), 0, Inf,
                       rel.tol = 1e-10, subdivisions = 2000L)$value
    }
    c(m = expect(sign), abs = expect(abs), signed_square = expect(function(u) u * abs(u)))
  }
  predicted_size <- function(mo, rho2, alpha = 0.05) {
    ratio <- 1 + rho2 * (mo[["m"]]^2 - 2 * mo[["m"]] * mo[["signed_square"]]) /
      (1 - mo[["abs"]]^2)
    2 * stats::pnorm(-stats::qnorm(1 - alpha / 2) / sqrt(ratio))
  }
  moments <- list(
    chisq5 = error_moments(function(x) stats::dchisq(x, 5), 5, sqrt(10)),
    exponential = error_moments(stats::dexp, 1, 1),
    lognormal = error_moments(stats::dlnorm, exp(0.5), sqrt((exp(1) - 1) * exp(1)))
  )
  # R^2 of 1 / x on x for x uniform on (1, 5).
  uniform_mean <- function(g) stats::integrate(function(x) g(x) / 4, 1, 5, rel.tol = 1e-12)$value
  rho2_inverse <- (1 - uniform_mean(function(x) 1 / x) * 3)^2 /
    ((uniform_mean(function(x) 1 / x^2) - uniform_mean(function(x) 1 / x)^2) * 4 / 3)
  simulated <- function(block, law) {
    rate <- gl$rejection_rate[gl$block == block & gl$errors == law & gl$test == "glejser"]
    if (length(rate)) sprintf("%.3f to %.3f", min(rate), max(rate)) else "--"
  }
  cat("\n### Glejser test, size of Glejser's statistic against its asymptotic value\n\n")
  cat("| Errors | Regressor | Asymptotic | Simulated |\n")
  cat("| --- | --- | ---: | ---: |\n")
  for (law in names(moments)) {
    cat(sprintf("| %s | `x1` | %.3f | %s |\n", errors[[law]],
                predicted_size(moments[[law]], 1), simulated("size", law)))
    outside <- simulated("size, regressor outside the mean equation", law)
    if (outside != "--") {
      cat(sprintf("| %s | `1 / x1` | %.3f | %s |\n", errors[[law]],
                  predicted_size(moments[[law]], rho2_inverse), outside))
    }
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    gl$replications[1], sqrt(0.05 * 0.95 / gl$replications[1])))

  # The generated summary, for the same reason the full sweep has one.
  null_rows <- gl[gl$block != "power" & gl$test %in% c("glejser", "glejser_robust"), ,
                  drop = FALSE]
  symmetric <- null_rows$errors %in% c("normal", "t5")
  robust <- null_rows$test == "glejser_robust"
  half <- stats::qnorm(0.995) * sqrt(0.05 * 0.95 / null_rows$replications)
  in_gate <- abs(null_rows$rejection_rate - 0.05) <= half
  span <- function(keep) {
    sprintf("%.3f to %.3f", min(null_rows$rejection_rate[keep]), max(null_rows$rejection_rate[keep]))
  }
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(
    "Under symmetric errors Glejser's statistic, `robust = FALSE`, rejects %s of the time and the corrected one, `robust = TRUE`, the default since 0.15.0, %s. Under asymmetric errors Glejser's statistic rejects %s and the corrected one %s. Of the %d null cells of the corrected statistic, %d fall inside the release-gate interval [%.4f, %.4f]",
    span(symmetric & !robust), span(symmetric & robust),
    span(!symmetric & !robust), span(!symmetric & robust),
    sum(robust), sum(robust & in_gate), 0.05 - half[1], 0.05 + half[1]))
  outside <- which(robust & !in_gate)
  if (length(outside)) {
    cat(". Outside it:\n\n")
    for (i in outside) {
      cat(sprintf("- %s errors, n = %d, %s: %.4f.\n",
                  errors[[null_rows$errors[i]]], null_rows$n[i], null_rows$block[i],
                  null_rows$rejection_rate[i]))
    }
  } else {
    cat(".\n")
  }
}

# --- Modified Wald test for groupwise heteroscedasticity ----------------------

csv_m <- file.path("inst", "validation", "modified-wald-size-power.csv")
if (!file.exists(csv_m)) csv_m <- "modified-wald-size-power.csv"
if (file.exists(csv_m)) {
  mw <- utils::read.csv(csv_m, stringsAsFactors = FALSE, colClasses = c(periods = "character"))
  laws <- c(normal = "Gaussian", t5 = "t5")
  references <- c(
    modified_wald = "chi-squared, N df (xttest3)",
    modified_wald_n_minus_1 = "chi-squared, N - 1 df"
  )
  rate_of <- function(rows) {
    if (nrow(rows)) sprintf("%.3f", rows$rejection_rate[1L]) else "--"
  }

  # One row per error distribution, number of units and reference
  # distribution, one column per number of periods.
  emit_mw <- function(block, title) {
    rows <- mw[mw$block == block, , drop = FALSE]
    periods <- sort(unique(as.integer(rows$periods)))
    cat(sprintf("\n### %s\n\n", title))
    cat("| Errors | Units | Reference | ",
        paste(sprintf("T = %d", periods), collapse = " | "), " |\n", sep = "")
    cat("| --- | ---: | --- | ", paste(rep("---:", length(periods)), collapse = " | "), " |\n",
        sep = "")
    for (law in names(laws)) {
      for (units in sort(unique(rows$units))) {
        for (test in names(references)) {
          cells <- vapply(periods, function(t) {
            rate_of(rows[rows$errors == law & rows$units == units & rows$test == test &
                           rows$periods == as.character(t), , drop = FALSE])
          }, character(1))
          cat("| ", laws[[law]], " | ", units, " | ", references[[test]], " | ",
              paste(cells, collapse = " | "), " |\n", sep = "")
        }
      }
    }
  }

  emit_mw("size", "Modified Wald test, size in balanced panels")

  unbalanced <- mw[mw$block == "size, unbalanced", , drop = FALSE]
  cat("\n### Modified Wald test, size in unbalanced panels\n\n")
  cat("| Errors | Units | Periods per unit | Mean periods | ",
      paste(references, collapse = " | "), " |\n", sep = "")
  cat("| --- | ---: | --- | ---: | ---: | ---: |\n")
  for (law in names(laws)) {
    for (units in sort(unique(unbalanced$units))) {
      rows <- unbalanced[unbalanced$errors == law & unbalanced$units == units, , drop = FALSE]
      cells <- vapply(names(references), function(test) rate_of(rows[rows$test == test, ]),
                      character(1))
      # Two decimals, trailing zeros dropped: "%.1f" rounds 16.25 to 16.2.
      mean_periods <- sub("\\.?0+$", "", sprintf("%.2f", rows$mean_periods[1]))
      cat(sprintf("| %s | %d | %s | %s | %s |\n", laws[[law]], units, rows$periods[1],
                  mean_periods, paste(cells, collapse = " | ")))
    }
  }

  emit_mw("size, long panels", "Modified Wald test, size in long panels")

  power <- mw[mw$block == "power" & mw$test == "modified_wald", , drop = FALSE]
  size_gaussian <- mw[mw$block == "size" & mw$test == "modified_wald" & mw$errors == "normal", ,
                      drop = FALSE]
  taus <- sort(unique(power$tau))
  cat("\n### Modified Wald test, rejection rate under lognormal unit variances\n\n")
  cat("| Units | Periods | Size, Gaussian | ",
      paste(sprintf("tau = %.2f", taus), collapse = " | "), " |\n", sep = "")
  cat("| ---: | ---: | ---: | ", paste(rep("---:", length(taus)), collapse = " | "), " |\n",
      sep = "")
  for (periods in sort(unique(as.integer(power$periods)))) {
    for (units in sort(unique(power$units))) {
      here <- function(rows) {
        rows[rows$units == units & rows$periods == as.character(periods), , drop = FALSE]
      }
      cells <- vapply(taus, function(tau) rate_of(here(power[power$tau == tau, ])), character(1))
      cat(sprintf("| %d | %d | %s | %s |\n", units, periods, rate_of(here(size_gaussian)),
                  paste(cells, collapse = " | ")))
    }
  }
  cat(sprintf(paste(
    "\nReplications: %d. Nominal level: 0.05.",
    "Monte Carlo standard error at the nominal level: %.4f.\n"
  ), mw$replications[1], sqrt(0.05 * 0.95 / mw$replications[1])))

  # The generated summary, for the same reason the full sweep has one. The
  # published test is the one with N degrees of freedom; every block but the
  # last is a null.
  null_rows <- mw[mw$block != "power" & mw$test == "modified_wald", , drop = FALSE]
  half <- stats::qnorm(0.995) * sqrt(0.05 * 0.95 / null_rows$replications)
  inside <- abs(null_rows$rejection_rate - 0.05) <= half
  below <- null_rows$rejection_rate < 0.05 - half
  grid <- null_rows$block == "size"
  span <- function(keep) {
    sprintf("%.3f to %.3f", min(null_rows$rejection_rate[keep]),
            max(null_rows$rejection_rate[keep]))
  }
  describe <- function(i) {
    sprintf("- %s errors, %d units, %s periods: %.4f.\n", laws[[null_rows$errors[i]]],
            null_rows$units[i], null_rows$periods[i], null_rows$rejection_rate[i])
  }
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  grid_units <- range(null_rows$units[grid])
  grid_periods <- range(as.integer(null_rows$periods[grid]))
  cat(sprintf(
    paste(
      "In the balanced panels of %d to %d units and %d to %d periods the test rejects",
      "a true null hypothesis %s of the time with Gaussian errors and %s with t5 errors.",
      "Of its %d null cells, %d fall inside the release-gate interval [%.4f, %.4f]"
    ),
    grid_units[1], grid_units[2], grid_periods[1], grid_periods[2],
    span(grid & null_rows$errors == "normal"), span(grid & null_rows$errors == "t5"),
    nrow(null_rows), sum(inside), 0.05 - half[1], 0.05 + half[1]
  ))
  if (any(inside)) {
    cat(":\n\n")
    for (i in which(inside)) cat(describe(i))
  } else {
    cat(".\n")
  }
  if (any(below)) {
    cat("\nBelow it:\n\n")
    for (i in which(below)) cat(describe(i))
  }
  cat(sprintf("\nThe other %d are above it.\n", sum(!inside & !below)))
}

# --- Harrison-McCabe ------------------------------------------------------------

csv_h <- file.path("inst", "validation", "harrison-mccabe-size-power.csv")
if (!file.exists(csv_h)) csv_h <- "harrison-mccabe-size-power.csv"
if (file.exists(csv_h)) {
  hm <- utils::read.csv(csv_h, stringsAsFactors = FALSE)
  tests <- c(
    harrison_mccabe = "`performHarrisonMcCabeTest()`",
    goldfeld_quandt = "`performGQTest()`",
    szroeter = "`performSzroeterTest()`",
    glejser_robust = "`performGlejserTest(robust = TRUE)`, two-sided"
  )
  rate_of <- function(rows, ...) {
    keys <- list(...)
    hit <- rep(TRUE, nrow(rows))
    for (key in names(keys)) hit <- hit & rows[[key]] == keys[[key]]
    if (any(hit)) rows$rejection_rate[hit][1L] else NA_real_
  }
  fmt <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))

  # Block 1: the exact p-value on samples drawn given the design.
  nd <- hm[hm$block == "null distribution", , drop = FALSE]
  null_tests <- c(
    harrison_mccabe_less = "`alternative = \"less\"`",
    harrison_mccabe_greater = "`alternative = \"greater\"`",
    harrison_mccabe_two_sided = "`alternative = \"two.sided\"`",
    regressor_free_less = "Null of `lmtest::hmctest()`, regressors ignored"
  )
  levels <- sort(unique(nd$level))
  cat("\n### Harrison-McCabe test, the p-value on samples drawn given the design\n\n")
  cat("| Design | P-value | ", paste(sprintf("%g%%", 100 * levels), collapse = " | "), " |\n",
      sep = "")
  cat("| --- | --- | ", paste(rep("---:", length(levels)), collapse = " | "), " |\n", sep = "")
  designs <- unique(nd[, c("n", "weights")])
  for (i in seq_len(nrow(designs))) {
    label <- sprintf("n = %d%s", designs$n[i],
                     if (designs$weights[i] == "exact") ", weighted" else "")
    for (test in intersect(names(null_tests), nd$test)) {
      cells <- vapply(levels, function(level) {
        fmt(rate_of(nd, n = designs$n[i], weights = designs$weights[i], test = test,
                    level = level))
      }, character(1))
      if (all(cells == "--")) next
      cat("| ", label, " | ", null_tests[[test]], " | ", paste(cells, collapse = " | "), " |\n",
          sep = "")
    }
  }
  cat(sprintf(
    "\nSamples per design: %d. Monte Carlo standard error at the 5%% level: %.4f.\n",
    nd$replications[1], sqrt(0.05 * 0.95 / nd$replications[1])))

  # Blocks 2 to 4: size; block 5: power.
  mc <- hm[hm$block != "null distribution", , drop = FALSE]
  size_blocks <- list(
    list(block = "size", errors = "normal", label = "Gaussian"),
    list(block = "size", errors = "t5", label = "t5"),
    list(block = "size, weighted fit", errors = "normal", label = "Gaussian, weighted fit")
  )
  sizes <- sort(unique(mc$n[mc$block != "power"]))
  cat("\n### Harrison-McCabe test, size\n\n")
  cat("| Errors | Test | ", paste(sprintf("n = %d", sizes), collapse = " | "), " |\n", sep = "")
  cat("| --- | --- | ", paste(rep("---:", length(sizes)), collapse = " | "), " |\n", sep = "")
  for (b in size_blocks) {
    for (test in names(tests)) {
      cells <- vapply(sizes, function(n) {
        fmt(rate_of(mc, block = b$block, errors = b$errors, test = test, n = n))
      }, character(1))
      cat("| ", b$label, " | ", tests[[test]], " | ", paste(cells, collapse = " | "), " |\n",
          sep = "")
    }
  }

  power <- mc[mc$block == "power", , drop = FALSE]
  power_sizes <- sort(unique(power$n))
  variances <- c(exp = "`sigma^2 = exp(0.4 x1)`", quad = "`sigma^2 = 1 + 0.15 x1^2`")
  cat("\n### Harrison-McCabe test, power\n\n")
  cat("| Variance | Test | ", paste(sprintf("n = %d", power_sizes), collapse = " | "), " |\n",
      sep = "")
  cat("| --- | --- | ", paste(rep("---:", length(power_sizes)), collapse = " | "), " |\n",
      sep = "")
  for (variance in intersect(names(variances), power$variance)) {
    for (test in names(tests)) {
      cells <- vapply(power_sizes, function(n) {
        fmt(rate_of(power, variance = variance, test = test, n = n))
      }, character(1))
      cat("| ", variances[[variance]], " | ", tests[[test]], " | ", paste(cells, collapse = " | "),
          " |\n", sep = "")
    }
  }
  cat(sprintf(
    "\nReplications: %d. Nominal level: 0.05. Monte Carlo standard error at the nominal level: %.4f.\n",
    mc$replications[1], sqrt(0.05 * 0.95 / mc$replications[1])))

  # The generated summary, for the same reason the full sweep has one. The gate
  # is the 99% interval around the nominal level at the number of samples.
  z_of <- function(rows) {
    (rows$rejection_rate - rows$level) / sqrt(rows$level * (1 - rows$level) / rows$effective)
  }
  in_gate <- function(rows) abs(z_of(rows)) <= stats::qnorm(0.995)
  span <- function(x) sprintf("%.3f to %.3f", min(x), max(x))
  exact_rows <- nd[nd$test != "regressor_free_less", , drop = FALSE]
  free_rows <- nd[nd$test == "regressor_free_less" & nd$level == 0.05, , drop = FALSE]
  gaussian_null <- mc[mc$test == "harrison_mccabe" & mc$errors == "normal" &
                        mc$block != "power", , drop = FALSE]
  t5 <- mc[mc$block == "size" & mc$errors == "t5", , drop = FALSE]
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(
    paste0(
      "On samples drawn given the design, %d of the %d rejection rates of the p-value fall ",
      "inside the 99%% interval around their level"
    ),
    sum(in_gate(exact_rows)), nrow(exact_rows)))
  outside <- which(!in_gate(exact_rows))
  if (length(outside)) {
    cat(sprintf(
      ". Outside it: %s.",
      paste(sprintf(
        "n = %d%s, `alternative = \"%s\"`, %g%% level, %.4f (%.1f standard errors)",
        exact_rows$n[outside],
        ifelse(exact_rows$weights[outside] == "exact", ", weighted", ""),
        sub("_", ".", sub("^harrison_mccabe_", "", exact_rows$test[outside])),
        100 * exact_rows$level[outside], exact_rows$rejection_rate[outside],
        z_of(exact_rows)[outside]
      ), collapse = "; ")))
  } else {
    cat(".")
  }
  cat(sprintf(
    " The null of `lmtest::hmctest()` rejects %s of the time at the 5%% level in the same samples (n = %s).\n\n",
    paste(sprintf("%.3f", free_rows$rejection_rate), collapse = ", "),
    paste(free_rows$n, collapse = ", ")))
  cat(sprintf(
    paste0(
      "Of the %d null cells of `performHarrisonMcCabeTest()` with Gaussian errors, with and ",
      "without weights, %d fall inside the release-gate interval [%.4f, %.4f], at %s"
    ),
    nrow(gaussian_null), sum(in_gate(gaussian_null)),
    0.05 - stats::qnorm(0.995) * sqrt(0.05 * 0.95 / gaussian_null$effective[1]),
    0.05 + stats::qnorm(0.995) * sqrt(0.05 * 0.95 / gaussian_null$effective[1]),
    span(gaussian_null$rejection_rate)))
  outside <- which(!in_gate(gaussian_null))
  if (length(outside)) {
    cat(". Outside it:\n\n")
    for (i in outside) {
      cat(sprintf("- %s, n = %d: %.4f.\n", gaussian_null$block[i], gaussian_null$n[i],
                  gaussian_null$rejection_rate[i]))
    }
    cat("\n")
  } else {
    cat(". ")
  }
  # The three ordered statistics are linear in the squared errors to first
  # order, so their null variance is proportional to the kurtosis less 1, and
  # each one-sided test at level alpha has asymptotic size
  # pnorm(-qnorm(1 - alpha) / sqrt((kurtosis - 1) / 2)). The kurtosis of t5 is
  # 3 + 6 / (5 - 4) = 9. Nothing here is simulated.
  t5_kurtosis <- 3 + 6 / (5 - 4)
  cat(sprintf(
    paste0(
      "Under t5 errors it rejects %s of the time, `performGQTest()` %s, ",
      "`performSzroeterTest()` %s and `performGlejserTest(robust = TRUE)` %s. ",
      "The asymptotic rate of the three one-sided tests under t5 errors is %.3f.\n"
    ),
    span(t5$rejection_rate[t5$test == "harrison_mccabe"]),
    span(t5$rejection_rate[t5$test == "goldfeld_quandt"]),
    span(t5$rejection_rate[t5$test == "szroeter"]),
    span(t5$rejection_rate[t5$test == "glejser_robust"]),
    stats::pnorm(-stats::qnorm(1 - 0.05) / sqrt((t5_kurtosis - 1) / 2))))

  # Power, in percentage points, against the two one-sided tests on the same
  # samples.
  points <- function(other) {
    cells <- unique(power[, c("variance", "n")])
    vapply(seq_len(nrow(cells)), function(i) {
      100 * (rate_of(power, variance = cells$variance[i], n = cells$n[i], test = other) -
               rate_of(power, variance = cells$variance[i], n = cells$n[i],
                       test = "harrison_mccabe"))
    }, numeric(1))
  }
  gq <- points("goldfeld_quandt")
  sz <- points("szroeter")
  cat(sprintf(
    paste0(
      "\nIn the %d power cells, `performGQTest()` has %.1f to %.1f percentage points more ",
      "power than `performHarrisonMcCabeTest()`, and `performSzroeterTest()` %.1f to %.1f.\n"
    ),
    length(gq), min(gq), max(gq), min(sz), max(sz)))
}

# --- Dette-Neumeyer-Van Keilegom prototype: the manuscript's tables -------------

csv_r <- file.path("inst", "validation", "variance-form-dnvk-reproduction.csv")
if (!file.exists(csv_r)) csv_r <- "variance-form-dnvk-reproduction.csv"
if (file.exists(csv_r)) {
  rp_all <- utils::read.csv(csv_r, stringsAsFactors = FALSE)
  # A difference of two independent binomial rates, here with `effective`
  # replications and in the manuscript with 1000, standardized with the pooled
  # rate. The manuscript's cell (VII), n = 200, c = 0 at 5% reads 0.095 above
  # 0.093 at 10%; it is a misprint and is left out of the counts.
  pooled <- (rp_all$rejection_rate * rp_all$effective + rp_all$paper_rate * 1000) /
    (rp_all$effective + 1000)
  se_diff <- sqrt(pooled * (1 - pooled) * (1 / rp_all$effective + 1 / 1000))
  rp_all$z <- ifelse(se_diff > 0, (rp_all$rejection_rate - rp_all$paper_rate) / se_diff, 0)
  rp_all$within <- abs(rp_all$z) <= stats::qnorm(0.995)
  rp_all$misprint <- rp_all$model == "VII" & rp_all$c == 0 & rp_all$n == 200 &
    rp_all$level == 0.05
  rp_all$diff_pts <- 100 * (rp_all$rejection_rate - rp_all$paper_rate)
  # A tenth of a point, with + 0 so that a rounded zero prints without a sign.
  pts <- function(x) sprintf("%.1f", round(x, 1) + 0)

  rp <- rp_all[rp_all$variant == "as described", , drop = FALSE]
  sizes <- sort(unique(rp$n))
  titles <- c(
    "4.1" = "Manuscript Table 4.1, constant variance, h = 0.3: here (manuscript)",
    "4.5" = "Manuscript Table 4.5, sigma^2 = 1 + theta x^2, h = 0.3: here (manuscript)"
  )
  for (tab in names(titles)) {
    rows <- rp[rp$table == tab, , drop = FALSE]
    if (!nrow(rows)) next
    cat(sprintf("\n### %s\n\n", titles[[tab]]))
    cat("| Model | c | Level | ", paste(sprintf("n = %d", sizes), collapse = " | "), " |\n",
        sep = "")
    cat("| --- | ---: | ---: | ", paste(rep("---:", length(sizes)), collapse = " | "), " |\n",
        sep = "")
    for (model in unique(rows$model)) {
      for (cc in sort(unique(rows$c))) {
        for (lv in sort(unique(rows$level))) {
          cells <- vapply(sizes, function(n) {
            hit <- rows$model == model & rows$c == cc & rows$level == lv & rows$n == n
            sprintf("%.3f (%.3f)", rows$rejection_rate[hit], rows$paper_rate[hit])
          }, character(1))
          cat(sprintf("| (%s) | %.1f | %.1f%% | %s |\n", model, cc, 100 * lv,
                      paste(cells, collapse = " | ")))
        }
      }
    }
  }

  # Every variant against the manuscript, by table.
  cat("\n### Manuscript tables, by the choices the manuscript does not state\n\n")
  cat("| Choice | Table | Null rates within the interval | Alternative rates within the interval |",
      "Alternatives, here less the manuscript, percentage points |\n")
  cat("| --- | --- | ---: | ---: | --- |\n")
  for (v in unique(rp_all$variant)) {
    for (tab in sort(unique(rp_all$table[rp_all$variant == v]))) {
      r <- rp_all[rp_all$variant == v & rp_all$table == tab & !rp_all$misprint, , drop = FALSE]
      null_r <- r$c == 0
      cat(sprintf("| %s | %s | %d of %d | %d of %d | %s on average, %s to %s |\n",
                  v, tab, sum(r$within & null_r), sum(null_r), sum(r$within & !null_r),
                  sum(!null_r), pts(mean(r$diff_pts[!null_r])), pts(min(r$diff_pts[!null_r])),
                  pts(max(r$diff_pts[!null_r]))))
    }
  }
  cat(sprintf(
    paste0(
      "\nReplications: %d here and 1000 in the manuscript, with B = %d bootstrap samples and the ",
      "rule (4.2). A rate is within the interval when it is within the 99%% interval of the ",
      "difference between two independent estimates.\n"
    ),
    rp$replications[1], rp$bootstrap[1]))

  # What the rule (4.2) gives an exactly calibrated statistic: it rejects when
  # more than floor(B (1 - alpha)) bootstrap statistics fall below the observed
  # one, that is when it is among the top B - floor(B (1 - alpha)) + 1 of B + 1.
  exact_rule <- function(alpha, B) (B - floor(B * (1 - alpha) + 1e-9) + 1) / (B + 1)
  ok <- !rp$misprint
  null_cells <- rp$c == 0 & ok
  alt_cells <- rp$c > 0
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  cat(sprintf(
    paste0(
      "As described, of the %d rates of the two tables, leaving out the misprinted one, %d are ",
      "within the 99%% interval of the difference: %d of the %d under the null hypothesis (c = 0) ",
      "and %d of the %d under the alternatives. Under the null hypothesis the rates here run from ",
      "%.3f to %.3f at 5%%, where the rule (4.2) with B = %d rejects an exactly calibrated ",
      "statistic %.3f of the time (%.3f at 2.5%%, %.3f at 10%%).\n"
    ),
    sum(ok), sum(rp$within & ok), sum(rp$within & null_cells), sum(null_cells),
    sum(rp$within & alt_cells), sum(alt_cells),
    min(rp$rejection_rate[null_cells & rp$level == 0.05]),
    max(rp$rejection_rate[null_cells & rp$level == 0.05]),
    rp$bootstrap[1], exact_rule(0.05, rp$bootstrap[1]),
    exact_rule(0.025, rp$bootstrap[1]), exact_rule(0.10, rp$bootstrap[1])))
  for (tab in sort(unique(rp$table))) {
    keep <- alt_cells & rp$table == tab
    cat(sprintf(
      paste0(
        "Under the alternatives of Table %s the rate here is higher than the ",
        "manuscript's in %d of %d cells.\n"
      ),
      tab, sum(rp$diff_pts[keep] > 0), sum(keep)))
  }
  outside <- which(!rp$within & ok)
  if (length(outside)) {
    cat("\nOutside the interval, as described:\n\n")
    for (i in outside) {
      cat(sprintf("- (%s), c = %.1f, n = %d, %.1f%% level: %.3f here, %.3f in the manuscript.\n",
                  rp$model[i], rp$c[i], rp$n[i], 100 * rp$level[i],
                  rp$rejection_rate[i], rp$paper_rate[i]))
    }
  }
}

# --- Dette-Neumeyer-Van Keilegom prototype: size and power ----------------------

csv_d <- file.path("inst", "validation", "variance-form-dnvk-size-power.csv")
if (!file.exists(csv_d)) csv_d <- "variance-form-dnvk-size-power.csv"
if (file.exists(csv_d)) {
  dv <- utils::read.csv(csv_d, stringsAsFactors = FALSE)
  tests <- c(
    dnvk_cm = "Cram\u00e9r-von Mises, prototype",
    dnvk_ks = "Kolmogorov-Smirnov, prototype",
    form_test_x = "`performVarianceFormTest(var_formula = ~ x)`",
    form_test_default = "`performVarianceFormTest()`"
  )
  truth_order <- c("constant", "exponential", "power", "linear", "quadratic", "wave")
  main <- dv[dv$block == "main", , drop = FALSE]
  sizes <- sort(unique(main$n))
  rate_at <- function(rows, ...) {
    keys <- list(...)
    hit <- rep(TRUE, nrow(rows))
    for (k in names(keys)) hit <- hit & rows[[k]] == keys[[k]]
    if (any(hit)) rows$rejection_rate[hit][1L] else NA_real_
  }
  fmt3 <- function(x) ifelse(is.na(x), "--", sprintf("%.3f", x))

  cat("\n### Prototype beside performVarianceFormTest(), size\n\n")
  cat("| Form tested | True variance | Test | ",
      paste(c(sprintf("Gaussian, n = %d", sizes), sprintf("t5, n = %d", sizes)), collapse = " | "),
      " |\n", sep = "")
  cat("| --- | --- | --- | ", paste(rep("---:", 2L * length(sizes)), collapse = " | "), " |\n",
      sep = "")
  for (form in c("exponential", "power")) {
    nulls <- unique(main$truth[main$form == form & main$hypothesis == "null"])
    for (truth in intersect(truth_order, nulls)) {
      label <- main$truth_label[main$truth == truth][1L]
      for (test in names(tests)) {
        cells <- c(
          vapply(sizes, function(n) {
            fmt3(rate_at(main, form = form, truth = truth, test = test, errors = "normal", n = n))
          }, character(1)),
          vapply(sizes, function(n) {
            fmt3(rate_at(main, form = form, truth = truth, test = test, errors = "t5", n = n))
          }, character(1))
        )
        cat(sprintf("| %s | `%s` | %s | %s |\n", form, label, tests[[test]],
                    paste(cells, collapse = " | ")))
      }
    }
  }

  for (errors in c("normal", "t5")) {
    cat(sprintf("\n### Prototype beside performVarianceFormTest(), power, %s errors\n\n",
                if (errors == "normal") "Gaussian" else "t5"))
    cat("| Form tested | True variance | Test | ",
        paste(sprintf("n = %d", sizes), collapse = " | "), " |\n", sep = "")
    cat("| --- | --- | --- | ", paste(rep("---:", length(sizes)), collapse = " | "), " |\n",
        sep = "")
    for (form in c("exponential", "power")) {
      alt <- main[main$form == form & main$hypothesis == "alternative", , drop = FALSE]
      for (truth in intersect(truth_order, unique(alt$truth))) {
        label <- alt$truth_label[alt$truth == truth][1L]
        for (test in names(tests)) {
          cells <- vapply(sizes, function(n) {
            fmt3(rate_at(alt, truth = truth, test = test, errors = errors, n = n))
          }, character(1))
          cat(sprintf("| %s | `%s` | %s | %s |\n", form, label, tests[[test]],
                      paste(cells, collapse = " | ")))
        }
      }
    }
  }

  bw <- dv[dv$block == "bandwidth", , drop = FALSE]
  if (nrow(bw)) {
    n_bw <- bw$n[1L]
    constants <- sort(unique(c(bw$bandwidth, main$bandwidth)))
    both <- rbind(bw, main[main$errors == "normal" & main$n == n_bw, , drop = FALSE])
    cat(sprintf("\n### Prototype, constant of the bandwidth, Gaussian errors, n = %d\n\n", n_bw))
    cat("| Form tested | True variance | Hypothesis | Test | ",
        paste(sprintf("c = %.2f", constants), collapse = " | "), " |\n", sep = "")
    cat("| --- | --- | --- | --- | ", paste(rep("---:", length(constants)), collapse = " | "),
        " |\n", sep = "")
    for (form in c("exponential", "power")) {
      for (truth in intersect(truth_order, unique(bw$truth[bw$form == form]))) {
        rows <- both[both$form == form & both$truth == truth, , drop = FALSE]
        for (test in c("dnvk_cm", "dnvk_ks")) {
          cells <- vapply(constants, function(cc) {
            fmt3(rate_at(rows, bandwidth = cc, test = test))
          }, character(1))
          cat(sprintf("| %s | `%s` | %s | %s | %s |\n", form, rows$truth_label[1L],
                      rows$hypothesis[1L], tests[[test]], paste(cells, collapse = " | ")))
        }
      }
    }
  }

  reps <- sort(unique(main$replications))
  cat(sprintf(
    paste0(
      "\nReplications: %s, with B = %d bootstrap samples for the prototype. Nominal level: 0.05. ",
      "Monte Carlo standard error at the nominal level: %s.\n"
    ),
    paste(reps, collapse = " and "), dv$bootstrap[!is.na(dv$bootstrap)][1L],
    paste(sprintf("%.4f", sqrt(0.05 * 0.95 / reps)), collapse = " and ")))

  # The generated summary. The release gate applies to the Gaussian null cells
  # of block 1, each at its own number of replications.
  null_rows <- main[main$hypothesis == "null", , drop = FALSE]
  half <- stats::qnorm(0.995) * sqrt(0.05 * 0.95 / null_rows$replications)
  in_band <- abs(null_rows$rejection_rate - 0.05) <= half
  gaussian <- null_rows$errors == "normal"
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  for (test in names(tests)) {
    keep_g <- gaussian & null_rows$test == test
    keep_t <- !gaussian & null_rows$test == test
    cat(sprintf(
      paste0(
        "- %s: %d of the %d Gaussian null cells inside the 99%% interval at their replications ",
        "(rates %.3f to %.3f); %d of the %d t5 null cells (rates %.3f to %.3f).\n"
      ),
      tests[[test]], sum(in_band & keep_g), sum(keep_g),
      min(null_rows$rejection_rate[keep_g]), max(null_rows$rejection_rate[keep_g]),
      sum(in_band & keep_t), sum(keep_t),
      min(null_rows$rejection_rate[keep_t]), max(null_rows$rejection_rate[keep_t])))
  }
  # The release gate is decided by the Gaussian cells; the Cramer-von Mises
  # statistic is the manuscript's.
  outside <- which(!in_band & gaussian & null_rows$test == "dnvk_cm")
  if (length(outside)) {
    cat("\nGaussian null cells of the Cram\u00e9r-von Mises prototype outside that ",
        "interval:\n\n", sep = "")
    for (i in outside) {
      cat(sprintf("- %s form, `%s`, n = %d: %.4f.\n",
                  null_rows$form[i], null_rows$truth_label[i], null_rows$n[i],
                  null_rows$rejection_rate[i]))
    }
  }

  # Power against the like-for-like existing test, on the same samples.
  alt <- main[main$hypothesis == "alternative", , drop = FALSE]
  cells <- unique(alt[, c("form", "truth", "errors", "n")])
  gap <- vapply(seq_len(nrow(cells)), function(i) {
    at <- function(test) {
      rate_at(alt, form = cells$form[i], truth = cells$truth[i], errors = cells$errors[i],
              n = cells$n[i], test = test)
    }
    100 * (at("dnvk_cm") - at("form_test_x"))
  }, numeric(1))
  cat(sprintf(
    paste0(
      "\nIn the %d alternative cells of block 1 the Cram\u00e9r-von Mises prototype rejects more ",
      "often than `performVarianceFormTest(var_formula = ~ x)` in %d; the difference runs from ",
      "%.1f to %.1f percentage points. By true variance:\n\n"
    ),
    length(gap), sum(gap > 0), min(gap), max(gap)))
  for (truth in intersect(truth_order, unique(cells$truth))) {
    keep <- cells$truth == truth
    cat(sprintf("- `%s`: %.1f to %.1f points.\n",
                alt$truth_label[alt$truth == truth][1L], min(gap[keep]), max(gap[keep])))
  }

  proto <- dv[dv$test == "dnvk_cm", , drop = FALSE]
  share_obs <- proto$failures / proto$replications
  share_boot <- proto$bootstrap_failures / (proto$effective * proto$bootstrap)
  worst_obs <- which.max(share_obs)
  worst_boot <- which.max(share_boot)
  null_obs <- proto$hypothesis == "null"
  cat(sprintf(
    paste0(
      "\nThe variance function had no least-squares fit, so that the prototype could not be ",
      "computed, in %d of the %d samples: at most %.1f%% of the samples of a null cell and %.1f%% ",
      "of those of any cell (%s form, `%s`, %s errors, n = %d). It had none in %.2f%% of all ",
      "bootstrap samples, at most %.2f%% in a cell (%s form, `%s`, %s errors, n = %d); those ",
      "samples are left out of the bootstrap distribution.\n"
    ),
    sum(proto$failures), sum(proto$replications),
    100 * max(share_obs[null_obs]), 100 * share_obs[worst_obs],
    proto$form[worst_obs], proto$truth_label[worst_obs],
    if (proto$errors[worst_obs] == "normal") "Gaussian" else "t5", proto$n[worst_obs],
    100 * sum(proto$bootstrap_failures) / sum(proto$effective * proto$bootstrap),
    100 * share_boot[worst_boot],
    proto$form[worst_boot], proto$truth_label[worst_boot],
    if (proto$errors[worst_boot] == "normal") "Gaussian" else "t5", proto$n[worst_boot]))
}

# --- Dette-Neumeyer-Van Keilegom prototype: the source of its conservatism ------

csv_b <- file.path("inst", "validation", "variance-form-dnvk-bootstrap-check.csv")
if (!file.exists(csv_b)) csv_b <- "variance-form-dnvk-bootstrap-check.csv"
if (file.exists(csv_b)) {
  bc <- utils::read.csv(csv_b, stringsAsFactors = FALSE)
  cat("\n### Prototype, bootstrap data from the fitted and from the true variance function\n\n")
  cat("| Form tested | True variance | n | True slope | Mean fitted slope | Mean T | ",
      "Mean T*, fitted | Mean T*, true shape | Size, fitted | Size, true shape |\n", sep = "")
  cat("| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |\n")
  cells <- unique(bc[, c("form", "truth", "truth_label", "n")])
  for (i in seq_len(nrow(cells))) {
    rows <- bc[bc$form == cells$form[i] & bc$truth == cells$truth[i] & bc$n == cells$n[i], ,
               drop = FALSE]
    fit <- rows[rows$bootstrap_data == "fitted", ]
    tru <- rows[rows$bootstrap_data == "true shape", ]
    cat(sprintf("| %s | `%s` | %d | %.1f | %.3f | %.4f | %.4f | %.4f | %.3f | %.3f |\n",
                cells$form[i], cells$truth_label[i], cells$n[i], fit$true_slope, fit$mean_slope,
                fit$mean_statistic, fit$mean_bootstrap_statistic, tru$mean_bootstrap_statistic,
                fit$rejection_rate, tru$rejection_rate))
  }
  cat(sprintf(
    paste0(
      "\nReplications: %d, with B = %d bootstrap samples drawn from the same random numbers ",
      "in both schemes. Gaussian errors.\n"
    ),
    bc$replications[1], bc$bootstrap[1]))
}

# --- The multiplier of the wild bootstrap ----------------------------------------

csv_w <- file.path("inst", "validation", "wild-multiplier-size.csv")
if (!file.exists(csv_w)) csv_w <- "wild-multiplier-size.csv"
if (file.exists(csv_w)) {
  wm <- utils::read.csv(csv_w, stringsAsFactors = FALSE)
  error_label <- c(normal = "Gaussian", t5 = "t5", exponential = "exponential")
  multiplier_label <- c(rademacher = "Rademacher", mammen = "Mammen")
  z99 <- stats::qnorm(0.995)
  # A rate with its 99% Monte Carlo interval, to four places, because the
  # decision turns on the third.
  with_interval <- function(rate, se, sign = FALSE) {
    sprintf(if (sign) "%+.4f [%+.4f, %+.4f]" else "%.4f [%.4f, %.4f]",
            rate, rate - z99 * se, rate + z99 * se)
  }

  # One row per cell, with the two multipliers side by side and their paired
  # difference d = (a_m - a_r) / N, where a_m and a_r count the samples that
  # only Mammen's or only Rademacher's multiplier rejected and N the samples
  # both tested; its variance is ((a_m + a_r) / N - d^2) / N.
  cells <- unique(wm[, c("scenario", "block", "errors", "n", "gamma", "replications",
                         "bootstrap")])
  cells <- cells[order(cells$scenario), , drop = FALSE]
  by_multiplier <- function(multiplier, column) {
    rows <- wm[wm$multiplier == multiplier, , drop = FALSE]
    rows[[column]][match(cells$scenario, rows$scenario)]
  }
  for (m in names(multiplier_label)) {
    cells[[m]] <- by_multiplier(m, "rejection_rate")
    cells[[paste0("se_", m)]] <- by_multiplier(m, "mc_se")
    cells[[paste0("alone_", m)]] <- by_multiplier(m, "rejects_alone")
  }
  cells$paired <- by_multiplier("mammen", "paired")
  cells$difference <- (cells$alone_mammen - cells$alone_rademacher) / cells$paired
  cells$se_difference <- sqrt(
    ((cells$alone_mammen + cells$alone_rademacher) / cells$paired - cells$difference^2) /
      cells$paired
  )

  emit_multiplier <- function(rows, title) {
    cat(sprintf("\n### %s\n\n", title))
    cat("| Errors | n | Rademacher | Mammen | Mammen - Rademacher |\n")
    cat("| --- | ---: | ---: | ---: | ---: |\n")
    for (i in seq_len(nrow(rows))) {
      cat(sprintf("| %s | %d | %s | %s | %s |\n", error_label[[rows$errors[i]]], rows$n[i],
                  with_interval(rows$rademacher[i], rows$se_rademacher[i]),
                  with_interval(rows$mammen[i], rows$se_mammen[i]),
                  with_interval(rows$difference[i], rows$se_difference[i], sign = TRUE)))
    }
  }
  size <- cells[cells$block == "size", , drop = FALSE]
  power <- cells[cells$block == "power", , drop = FALSE]
  emit_multiplier(size, "Wild bootstrap, size by multiplier")
  cat(sprintf(paste(
    "\nRejection rates at the 5%% level with their 99%% Monte Carlo intervals.",
    "Replications: %d per cell, both multipliers on each sample. B = %d.\n"
  ), size$replications[1], size$bootstrap[1]))
  if (nrow(power)) {
    emit_multiplier(power, sprintf(
      "Wild bootstrap, power against `sigma^2 = exp(%.1f x1)` by multiplier", power$gamma[1]
    ))
    cat(sprintf(paste(
      "\nGaussian errors. Rejection rates at the 5%% level with their 99%% Monte Carlo",
      "intervals. Replications: %d per cell, both multipliers on each sample. B = %d.\n"
    ), power$replications[1], power$bootstrap[1]))
  }

  # The size cells against the release gate and against the interval around
  # the rate of an exact test, and the decision rule fixed in the script: the
  # multiplier inside more often, under each interval, provided that in at
  # least one cell where only it is inside the paired difference is outside
  # its 99% interval.
  exact <- (ceiling(0.05 * (size$bootstrap[1] + 1)) - 1) / (size$bootstrap[1] + 1)
  reps <- size$replications[1]
  intervals <- list(
    gate = 0.05 + c(-1, 1) * z99 * sqrt(0.05 * 0.95 / reps),
    exact = exact + c(-1, 1) * z99 * sqrt(exact * (1 - exact) / reps)
  )
  interval_label <- c(
    gate = "the release gate, the 99% interval around 0.05",
    exact = sprintf("the 99%% interval around %.3f, the rate of an exact test with B = %d",
                    exact, size$bootstrap[1])
  )
  cell_name <- function(i) sprintf("%s errors, n = %d", error_label[[size$errors[i]]], size$n[i])
  clear <- abs(size$difference) > z99 * size$se_difference
  winners <- character()
  cat("\n<!-- generated by make-table.R; do not edit the numbers by hand -->\n\n")
  for (name in names(intervals)) {
    bounds <- intervals[[name]]
    ok <- vapply(names(multiplier_label), function(m) {
      size[[m]] >= bounds[1] & size[[m]] <= bounds[2]
    }, logical(nrow(size)))
    ok <- matrix(ok, nrow = nrow(size), dimnames = list(NULL, names(multiplier_label)))
    cat(sprintf(paste(
      "Against %s, [%.4f, %.4f], Rademacher's multiplier is inside in %d of the %d size",
      "cells and Mammen's in %d."
    ), interval_label[[name]], bounds[1], bounds[2], sum(ok[, "rademacher"]), nrow(size),
    sum(ok[, "mammen"])))
    for (m in names(multiplier_label)) {
      out <- which(!ok[, m])
      if (length(out)) {
        cat(sprintf(" %s is outside in %s.", multiplier_label[[m]], paste(vapply(out, function(i) {
          sprintf("%s (%.4f)", cell_name(i), size[[m]][i])
        }, character(1)), collapse = "; ")))
      }
    }
    cat("\n\n")
    counts <- colSums(ok)
    winners[name] <- "none"
    if (counts[["rademacher"]] != counts[["mammen"]]) {
      more <- names(which.max(counts))
      only <- ok[, more] & !ok[, setdiff(names(multiplier_label), more)]
      if (any(clear[only])) winners[name] <- more
    }
  }
  verdict <- if (length(unique(winners)) == 1L && winners[[1]] != "none") {
    sprintf(paste(
      "%s's multiplier holds the level in more of the grid under both intervals, by more",
      "than Monte Carlo error in at least one cell."
    ), multiplier_label[[winners[[1]]]])
  } else {
    paste(
      "Neither multiplier holds the level in more of the grid by more than Monte Carlo error",
      "under both intervals, so by the rule fixed in the script the default, Rademacher, stays."
    )
  }
  cat(sprintf(paste(
    "%s The paired difference, Mammen less Rademacher, is %+.4f to %+.4f across the size",
    "cells, and its 99%% Monte Carlo interval excludes zero in %d of the %d.\n"
  ), verdict, min(size$difference), max(size$difference), sum(clear), nrow(size)))
  if (nrow(power)) {
    cat(sprintf(paste(
      "\nAgainst `sigma^2 = exp(%.1f x1)` the paired difference in power, Mammen less",
      "Rademacher, is %s at n = %s.\n"
    ), power$gamma[1],
    paste(sprintf("%+.3f", power$difference), collapse = ", "),
    paste(power$n, collapse = ", ")))
  }
}
