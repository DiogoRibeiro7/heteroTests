# Tables and the search for a rule, from the output of fixed-effects-size.R ------
#
# Run from the package root:
#   Rscript inst/validation/analyse-fixed-effects-size.R [<results.csv> [<sensitivity.csv>]]
#
# Prints Markdown: the size tables at 1%, 5% and 10% for G x T and each test,
# the variants, the mean statistic against (G - 1) T / (T - 1) + 1, and the
# misclassification count of candidate rules on (G, T) that would separate
# the cells whose size at 5% is within [2.5%, 7.5%] from those outside. The
# results are inst/validation/fixed-effects-size.csv by default. The second
# file, a run with more replications on the same seeds, by default
# inst/validation/fixed-effects-size-10000.csv when it exists, is used to
# recount the rules with less Monte Carlo error. Does not load the package.

args <- commandArgs(trailingOnly = TRUE)
in_validation <- function(file) {
  path <- file.path("inst", "validation", file)
  if (file.exists(path)) path else file
}
main_csv <- if (length(args) > 0) args[1] else in_validation("fixed-effects-size.csv")
sens_csv <- if (length(args) > 1) args[2] else in_validation("fixed-effects-size-10000.csv")
read_run <- function(path) {
  results <- utils::read.csv(path, stringsAsFactors = FALSE)
  list(results = results, settings = list(n_mc = max(results$replications)))
}
main <- read_run(main_csv)
sens <- if (file.exists(sens_csv)) read_run(sens_csv) else NULL
res <- main$results
tests <- c("White", "Koenker", "BP")
test_names <- c(White = "White", Koenker = "Koenker", BP = "Breusch-Pagan")
lo <- 0.025
hi <- 0.075

pct <- function(x, digits = 1) ifelse(is.na(x), "", formatC(100 * x, format = "f", digits = digits))

grid <- res[res$block == "grid", ]
G_values <- sort(unique(grid$G))
T_values <- sort(unique(grid$T))

size_table <- function(test, column) {
  x <- grid[grid$test == test, ]
  head <- paste0("| G \\ T | ", paste(T_values, collapse = " | "), " |")
  rule <- paste0("|---:|", paste(rep("---:", length(T_values)), collapse = "|"), "|")
  rows <- vapply(G_values, function(G) {
    cells <- vapply(T_values, function(T) {
      o <- x[x$G == G & x$T == T, ]
      if (o$status == "not run (exact fit)") return("n/r")
      if (o$status == "refused") return("ref")
      v <- pct(o[[column]])
      if (column == "reject_05" && (o[[column]] < lo || o[[column]] > hi)) paste0("**", v, "**") else v
    }, character(1))
    paste0("| ", G, " | ", paste(cells, collapse = " | "), " |")
  }, character(1))
  c(head, rule, rows)
}

cat("## Size at 5%, G subjects x T observations\n\n")
cat("Rejection rate in %, 2,000 replications per cell. Bold: outside [2.5%, 7.5%].",
    "n/r: not run (T = 2, the exact fit White's test refuses). ref: refused by the package",
    "(fewer than 20 observations for White, 15 for Koenker and Breusch-Pagan).\n\n")
for (test in tests) {
  cat("### ", test_names[[test]], "\n\n", sep = "")
  cat(size_table(test, "reject_05"), sep = "\n")
  cat("\n\n")
}

cat("## Size at 1% and 10%\n\n")
for (column in c("reject_01", "reject_10")) {
  for (test in tests) {
    cat("### ", test_names[[test]], ", ", if (column == "reject_01") "1%" else "10%", "\n\n", sep = "")
    cat(size_table(test, column), sep = "\n")
    cat("\n\n")
  }
}

cat("## All grid cells\n\n")
cat("Rejection at 1 / 5 / 10% in %, the degrees of freedom and df / n of each test.",
    "White's auxiliary regression has the products x * subject when G <= 10 (2G degrees of",
    "freedom) and not above (G + 1).\n\n")
cat("| G | T | n | White df (df/n) | White | Koenker / BP df (df/n) | Koenker | Breusch-Pagan |\n")
cat("|---:|---:|---:|---:|---|---:|---|---|\n")
for (G in G_values) for (T in T_values) {
  o <- grid[grid$G == G & grid$T == T, ]
  three <- function(test) {
    x <- o[o$test == test, ]
    if (x$status != "computed") return(x$status)
    sprintf("%s / %s / %s", pct(x$reject_01), pct(x$reject_05), pct(x$reject_10))
  }
  dfs <- function(test) {
    x <- o[o$test == test, ]
    if (x$status != "computed") return("")
    sprintf("%g (%.3f)", x$df, x$df_over_n)
  }
  cat(sprintf("| %d | %d | %d | %s | %s | %s | %s | %s |\n", G, T, G * T, dfs("White"),
              three("White"), dfs("Koenker"), three("Koenker"), three("BP")))
}
cat("\n")

# --- the mean statistic ---------------------------------------------------------

cat("## The mean statistic\n\n")
k <- grid[grid$test == "Koenker" & grid$status == "computed" & grid$T > 2, ]
k$pred <- (k$G - 1) * k$T / (k$T - 1) + 1
k$dev <- k$mean_statistic / k$pred - 1
cat(sprintf(paste(
  "Koenker's statistic has mean (G - 1) T / (T - 1) + 1 where the chi-squared it is referred",
  "to has mean G: over the %d grid cells with T >= 3 the simulated mean is within %.1f%% of it",
  "(median absolute deviation %.2f%%). Its variance is close to 2G for T >= 5",
  "(ratio %.2f to %.2f) and smaller below (%.2f to %.2f at T = 3). So the statistic is a",
  "chi-squared shifted by about (G - 1) / (T - 1), which grows with G at fixed T, while the",
  "spread of the reference distribution grows as sqrt(2G).\n\n"
), nrow(k), 100 * max(abs(k$dev)), 100 * stats::median(abs(k$dev)),
min(k$var_statistic[k$T >= 5] / (2 * k$df[k$T >= 5])), max(k$var_statistic[k$T >= 5] / (2 * k$df[k$T >= 5])),
min(k$var_statistic[k$T == 3] / (2 * k$df[k$T == 3])), max(k$var_statistic[k$T == 3] / (2 * k$df[k$T == 3]))))
cat("| T | T / (T - 1) | mean statistic / df, G = 200 (Koenker) | White | Breusch-Pagan |\n|---:|---:|---:|---:|---:|\n")
for (T in T_values[T_values > 2]) {
  r <- function(test) {
    o <- grid[grid$test == test & grid$G == 200 & grid$T == T, ]
    sprintf("%.3f", o$mean_statistic / o$df)
  }
  cat(sprintf("| %d | %.3f | %s | %s | %s |\n", T, T / (T - 1), r("Koenker"), r("White"), r("BP")))
}
cat("\n")

# --- variants -------------------------------------------------------------------

cat("## Variants\n\n")
cat("Rejection rates in % at 1 / 5 / 10%, and the balanced one-regressor grid cell beside it at 5%.\n\n")
cat("| Block | Design | n | White df | White | Koenker | Breusch-Pagan | Grid cell at 5% (W / K / BP) |\n")
cat("|---|---|---:|---:|---|---|---|---|\n")
variants <- res[res$block != "grid", ]
keys <- unique(variants[, c("block", "design")])
for (i in seq_len(nrow(keys))) {
  v <- variants[variants$block == keys$block[i] & variants$design == keys$design[i], ]
  three <- function(test) {
    o <- v[v$test == test, ]
    if (o$status != "computed") return(o$status)
    sprintf("%s / %s / %s", pct(o$reject_01), pct(o$reject_05), pct(o$reject_10))
  }
  G <- v$G[1]
  T_ref <- if (keys$block[i] == "unbalanced") round(v$n[1] / G) else v$T[1]
  ref <- grid[grid$G == G & grid$T == T_ref, ]
  ref_txt <- if (nrow(ref)) {
    sprintf("%d x %d: %s / %s / %s", G, T_ref, pct(ref$reject_05[ref$test == "White"]),
            pct(ref$reject_05[ref$test == "Koenker"]), pct(ref$reject_05[ref$test == "BP"]))
  } else ""
  cat(sprintf("| %s | %s | %d | %s | %s | %s | %s | %s |\n", keys$block[i], keys$design[i], v$n[1],
              format(v$df[v$test == "White"]), three("White"), three("Koenker"), three("BP"), ref_txt))
}
cat("\n")

# --- rules ----------------------------------------------------------------------

cells_of <- function(r, test) {
  g <- r[r$block == "grid" & r$status == "computed", ]
  if (test == "all three") {
    keys <- unique(g[, c("G", "T")])
    inside <- vapply(seq_len(nrow(keys)), function(i) {
      o <- g[g$G == keys$G[i] & g$T == keys$T[i], ]
      all(o$reject_05 >= lo & o$reject_05 <= hi)
    }, logical(1))
    rate <- vapply(seq_len(nrow(keys)), function(i) {
      o <- g[g$G == keys$G[i] & g$T == keys$T[i], ]
      o$reject_05[which.max(abs(o$reject_05 - 0.05))]
    }, numeric(1))
    df <- vapply(seq_len(nrow(keys)), function(i) {
      o <- g[g$G == keys$G[i] & g$T == keys$T[i], ]
      max(o$df_over_n)
    }, numeric(1))
    data.frame(G = keys$G, T = keys$T, n = keys$G * keys$T, df_over_n = df, rate = rate,
               inside = inside)
  } else {
    o <- g[g$test == test, ]
    data.frame(G = o$G, T = o$T, n = o$n, df_over_n = o$df_over_n, rate = o$reject_05,
               inside = o$reject_05 >= lo & o$reject_05 <= hi)
  }
}

scores <- list(
  "T >= t" = function(d) -d$T,
  "G <= g" = function(d) d$G,
  "n >= m" = function(d) -d$n,
  "df / n <= k" = function(d) d$df_over_n,
  "(G - 1) / (T - 1) <= k" = function(d) (d$G - 1) / (d$T - 1),
  "G / (T - 1)^2 <= k" = function(d) d$G / (d$T - 1)^2
)

best_threshold <- function(score, inside) {
  u <- sort(unique(score))
  cuts <- c(u[1] - 1, (u[-1] + u[-length(u)]) / 2, u[length(u)] + 1)
  errs <- vapply(cuts, function(k) sum((score <= k) != inside), numeric(1))
  best <- which(errs == min(errs))
  list(k = cuts[best[1]], errors = min(errs), ties = length(best), all_out = best[1] == 1L)
}

describe_cut <- function(name, k, all_out = FALSE) {
  if (all_out) return("none: flags every cell")
  switch(name,
    "T >= t" = sprintf("T >= %g", ceiling(-k)),
    "G <= g" = sprintf("G <= %g", floor(k)),
    "n >= m" = sprintf("n >= %g", ceiling(-k)),
    "df / n <= k" = sprintf("df / n <= %.3f", k),
    "(G - 1) / (T - 1) <= k" = sprintf("(G - 1) / (T - 1) <= %.2f", k),
    "G / (T - 1)^2 <= k" = sprintf("G / (T - 1)^2 <= %.3f", k))
}

rule_rows <- function(r, label) {
  out <- list()
  for (test in c(tests, "all three")) {
    d <- cells_of(r, test)
    for (name in names(scores)) {
      s <- scores[[name]](d)
      b <- best_threshold(s, d$inside)
      wrong <- which((s <= b$k) != d$inside)
      out[[length(out) + 1L]] <- data.frame(
        run = label, test = test, rule = name, best = describe_cut(name, b$k, b$all_out),
        threshold = b$k, cells = nrow(d),
        inside = sum(d$inside), errors = b$errors,
        false_inside = sum(s[wrong] <= b$k), false_outside = sum(s[wrong] > b$k),
        broken_by = paste(sprintf("%dx%d (%s)", d$G[wrong], d$T[wrong], pct(d$rate[wrong])),
                          collapse = ", "),
        stringsAsFactors = FALSE)
    }
    # Rectangle: T >= t and G <= g.
    best <- NULL
    for (t in T_values) for (g in G_values) {
      pred <- d$T >= t & d$G <= g
      e <- sum(pred != d$inside)
      if (is.null(best) || e < best$e) best <- list(t = t, g = g, e = e, pred = pred)
    }
    wrong <- which(best$pred != d$inside)
    out[[length(out) + 1L]] <- data.frame(
      run = label, test = test, rule = "T >= t and G <= g",
      best = sprintf("T >= %d and G <= %d", best$t, best$g), threshold = NA_real_,
      cells = nrow(d),
      inside = sum(d$inside), errors = best$e,
      false_inside = sum(best$pred[wrong]), false_outside = sum(!best$pred[wrong]),
      broken_by = paste(sprintf("%dx%d (%s)", d$G[wrong], d$T[wrong], pct(d$rate[wrong])),
                        collapse = ", "),
      stringsAsFactors = FALSE)
    # No free parameter: the chi-squared shifted by (G - 1) / (T - 1).
    dd <- r[r$block == "grid" & r$status == "computed", ]
    pred_inside <- if (test == "all three") {
      vapply(seq_len(nrow(d)), function(i) {
        o <- dd[dd$G == d$G[i] & dd$T == d$T[i], ]
        all(stats::pchisq(stats::qchisq(0.95, o$df) - (o$G - 1) / (o$T - 1), o$df,
                          lower.tail = FALSE) <= hi)
      }, logical(1))
    } else {
      o <- dd[dd$test == test, ]
      stats::pchisq(stats::qchisq(0.95, o$df) - (o$G - 1) / (o$T - 1), o$df, lower.tail = FALSE) <= hi
    }
    wrong <- which(pred_inside != d$inside)
    out[[length(out) + 1L]] <- data.frame(
      run = label, test = test, rule = "shifted chi-squared size <= 7.5%",
      best = "no free parameter", threshold = NA_real_, cells = nrow(d),
      inside = sum(d$inside),
      errors = length(wrong), false_inside = sum(pred_inside[wrong]),
      false_outside = sum(!pred_inside[wrong]),
      broken_by = paste(sprintf("%dx%d (%s)", d$G[wrong], d$T[wrong], pct(d$rate[wrong])),
                        collapse = ", "),
      stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

rules <- rule_rows(res, sprintf("%d replications", main$settings$n_mc))
if (!is.null(sens)) {
  rules <- rbind(rules, rule_rows(sens$results, sprintf("%d replications", sens$settings$n_mc)))
}

cat("## Rules on (G, T)\n\n")
cat("For each candidate, the threshold with the fewest misclassified grid cells (cells whose",
    "size at 5% is inside [2.5%, 7.5%] predicted outside, or the reverse). False inside: the rule",
    "lets through a cell outside the band; false outside: it flags a cell inside. The cells",
    "listed are those it misclassifies, with their size at 5% (for 'all three', the rate",
    "furthest from 5%).\n\n")
for (run in unique(rules$run)) {
  cat("### ", run, "\n\n", sep = "")
  cat("| Test | Rule | Best threshold | Cells (inside) | Misclassified | False inside | False outside |\n")
  cat("|---|---|---|---:|---:|---:|---:|\n")
  x <- rules[rules$run == run, ]
  for (i in seq_len(nrow(x))) {
    cat(sprintf("| %s | %s | %s | %d (%d) | %d | %d | %d |\n", x$test[i], x$rule[i], x$best[i],
                x$cells[i], x$inside[i], x$errors[i], x$false_inside[i], x$false_outside[i]))
  }
  cat("\n")
}

runs <- list(list(label = unique(rules$run)[1], results = res))
if (!is.null(sens)) runs[[2]] <- list(label = unique(rules$run)[2], results = sens$results)

for (run in runs) {
  cat("### Cells that break each rule (", run$label, ")\n\n", sep = "")
  x <- rules[rules$run == run$label, ]
  for (i in seq_len(nrow(x))) {
    cat(sprintf("- **%s, %s** (%s): %s\n", x$test[i], x$rule[i], x$best[i],
                if (nzchar(x$broken_by[i])) x$broken_by[i] else "none"))
  }
  cat("\n")
}

# The rule (G - 1) / (T - 1) <= k fitted on the grid, applied to the variants
# with T the mean number of observations per subject, n / G.
for (run in runs) {
  cat("### The (G - 1) / (T - 1) rule on the variants (", run$label, ")\n\n", sep = "")
  cat("Threshold of each test fitted on the grid of the same run; T = n / G.",
      "Cells whose size at 5% the rule misclassifies are in bold.\n\n")
  fitted <- rules[rules$run == run$label & rules$rule == "(G - 1) / (T - 1) <= k", ]
  vr <- run$results[run$results$block != "grid", ]
  cat("| Block | Design | (G - 1) / (n / G - 1) |",
      paste(sprintf(" %s (k = %.2f) |", test_names[tests], fitted$threshold[match(tests, fitted$test)]),
            collapse = ""), "\n", sep = "")
  cat("|---|---|---:|", paste(rep("---:|", length(tests)), collapse = ""), "\n", sep = "")
  variant_errors <- setNames(integer(length(tests)), tests)
  variant_cells <- setNames(integer(length(tests)), tests)
  for (i in seq_len(nrow(keys))) {
    v <- vr[vr$block == keys$block[i] & vr$design == keys$design[i], ]
    score <- (v$G[1] - 1) / (v$n[1] / v$G[1] - 1)
    cells <- vapply(tests, function(test) {
      o <- v[v$test == test, ]
      if (o$status != "computed") return(o$status)
      inside <- o$reject_05 >= lo && o$reject_05 <= hi
      predicted <- score <= fitted$threshold[fitted$test == test]
      variant_cells[[test]] <<- variant_cells[[test]] + 1L
      if (inside != predicted) {
        variant_errors[[test]] <<- variant_errors[[test]] + 1L
        sprintf("**%s**", pct(o$reject_05))
      } else pct(o$reject_05)
    }, character(1))
    cat(sprintf("| %s | %s | %.2f | %s |\n", keys$block[i], keys$design[i], score,
                paste(cells, collapse = " | ")))
  }
  cat("\nMisclassified:", paste(sprintf("%s %d of %d", test_names[tests], variant_errors,
                                         variant_cells), collapse = "; "), "\n\n")
}
