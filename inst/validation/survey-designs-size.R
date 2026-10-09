# Tests of constant variance on survey data ------------------------------------
#
# Run from the package root:
#   Rscript inst/validation/survey-designs-size.R
#
# N_MC can be overridden for exploratory runs, e.g. N_MC=100 Rscript ... .
# Release evidence uses N_MC=2000, which puts the Monte Carlo standard error at
# 0.0049 at the nominal level.
#
# runSurveyHeteroTests() takes the data out of a survey design, fits the model
# by ordinary least squares and runs the ordinary tests. It does not use the
# sampling weights, strata or clusters. This script says when that holds its
# level, and why the three ways of bringing the design in were not adopted.
#
# The population model is y = 1 + 2 x + e throughout, with x standard normal
# and Var(e) = 1 except in the last design, so every rejection rate but the
# last column is a size.
#
# Designs
#   ignorable         The chance of selection depends on x alone, as
#                     exp(0.4 x). Weights vary about 5 to 1.
#   ignorable_strong  The same with exp(0.9 x). Weights vary about 36 to 1.
#   stratified        Three strata cut on x, sampled at 15%, 35% and 50% of the
#                     sample, with finite-population corrections.
#   informative       Units with x > 0 and |e| > 1 are four times as likely to
#                     be selected. The errors are homoscedastic in the
#                     population and not in the sample.
#   clustered         Clusters of ten with equal weights, a cluster-level
#                     component in x and a cluster effect in the error whose
#                     scale varies between clusters.
#   heteroscedastic   The ignorable design with sd(e) = exp(0.2 x). Power.
#
# Procedures
#   ols                   What runSurveyHeteroTests() does, called through it.
#   weights_as_precision  The same test on lm(weights = sampling weights),
#                         which reads the Pearson residuals sqrt(w) e. A test
#                         applied to a survey::svyglm() fit computes the same
#                         statistic.
#   design_wald           Not in the package. Squared residuals of the
#                         svyglm() fit regressed on the variance regressors
#                         with svyglm(), and survey::regTermTest(method =
#                         "Wald") on the slopes.
#   design_score          Not in the package. The Rao score version of the
#                         same test: the variance of the weighted score is
#                         estimated from the design under the null.

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
if (!requireNamespace("survey", quietly = TRUE)) {
  stop("This study needs the survey package.")
}

get_n_mc <- function() {
  n_mc <- as.integer(Sys.getenv("N_MC", unset = "2000"))
  if (!is.finite(n_mc) || n_mc < 20L) {
    stop("N_MC must be an integer >= 20.")
  }
  n_mc
}

alpha <- 0.05
n_mc <- get_n_mc()
sample_sizes <- c(300L, 1000L)
seed <- 20261011L
invisible(ht_set_log_level("SILENT"))

# --- designs ------------------------------------------------------------------

# Poisson sampling from a population six times the target sample size.
draw_unequal <- function(n, selection, error_sd = function(x) 1) {
  size <- 6L * n
  x <- stats::rnorm(size)
  e <- stats::rnorm(size) * error_sd(x)
  relative <- selection(x, e)
  chance <- pmin(1, relative * n / sum(relative))
  taken <- stats::runif(size) < chance
  d <- data.frame(y = 1 + 2 * x[taken] + e[taken], x = x[taken], w = 1 / chance[taken])
  list(data = d, design = survey::svydesign(ids = ~1, weights = ~w, data = d))
}

bounded <- function(x) pmax(pmin(x, 2), -2)

DESIGNS <- list(
  ignorable = function(n) {
    draw_unequal(n, function(x, e) exp(0.4 * bounded(x)))
  },
  ignorable_strong = function(n) {
    draw_unequal(n, function(x, e) exp(0.9 * bounded(x)))
  },
  stratified = function(n) {
    size <- 6L * n
    x <- stats::rnorm(size)
    y <- 1 + 2 * x + stats::rnorm(size)
    stratum <- cut(x, c(-Inf, -0.5, 0.5, Inf), labels = c("low", "middle", "high"))
    take <- round(c(low = 0.15, middle = 0.35, high = 0.50) * n)
    rows <- unlist(lapply(names(take), function(s) sample(which(stratum == s), take[[s]])))
    counts <- table(stratum)
    d <- data.frame(y = y[rows], x = x[rows], stratum = stratum[rows])
    d$fpc <- as.numeric(counts[as.character(d$stratum)])
    d$w <- d$fpc / take[as.character(d$stratum)]
    list(
      data = d,
      design = survey::svydesign(
        ids = ~1, strata = ~stratum, weights = ~w, fpc = ~fpc, data = d
      )
    )
  },
  informative = function(n) {
    draw_unequal(n, function(x, e) ifelse(x > 0 & abs(e) > 1, 4, 1))
  },
  clustered = function(n) {
    clusters <- n %/% 10L
    id <- rep(seq_len(clusters), each = 10L)
    x <- stats::rnorm(clusters)[id] + 0.3 * stats::rnorm(length(id))
    effect <- (stats::rnorm(clusters) * abs(stats::rt(clusters, 5)))[id]
    d <- data.frame(y = 1 + 2 * x + effect + stats::rnorm(length(id)), x = x, id = id, w = 1)
    list(data = d, design = survey::svydesign(ids = ~id, weights = ~w, data = d))
  },
  heteroscedastic = function(n) {
    draw_unequal(
      n,
      function(x, e) exp(0.4 * bounded(x)),
      error_sd = function(x) exp(0.2 * x)
    )
  }
)

# --- procedures ---------------------------------------------------------------

variance_regressors <- function(fit, white) {
  x <- stats::model.matrix(fit)
  z <- x[, colnames(x) != "(Intercept)", drop = FALSE]
  if (white) {
    z <- cbind(z, z^2)
  }
  colnames(z) <- paste0("z", seq_len(ncol(z)))
  z
}

design_based <- function(design, white) {
  fit <- survey::svyglm(y ~ x, design = design)
  inner <- fit$survey.design
  squared <- (as.numeric(fit$y) - as.numeric(fit$fitted.values))^2
  z <- variance_regressors(fit, white)
  slopes <- stats::reformulate(colnames(z))

  wald_design <- inner
  wald_design$variables <- cbind(wald_design$variables, z, squared = squared)
  auxiliary <- survey::svyglm(
    stats::reformulate(colnames(z), response = "squared"),
    design = wald_design
  )
  wald <- survey::regTermTest(auxiliary, slopes, method = "Wald")

  w <- stats::weights(inner)
  centred <- sweep(z, 2, colSums(w * z) / sum(w))
  score <- centred * (squared - sum(w * squared) / sum(w))
  score_design <- inner
  score_design$variables <- cbind(score_design$variables, score)
  total <- survey::svytotal(slopes, score_design)
  estimate <- stats::coef(total)
  statistic <- drop(estimate %*% solve(stats::vcov(total), estimate))

  c(
    design_wald = as.numeric(wald$p),
    design_score = stats::pf(
      statistic / ncol(z), ncol(z), survey::degf(inner),
      lower.tail = FALSE
    )
  )
}

one_draw <- function(make, n, test) {
  sample <- make(n)
  white <- identical(test, "white")
  runner <- if (white) performWhiteTest else performKoenkerTest
  package <- suppressWarnings(
    runSurveyHeteroTests(y ~ x, sample$design, tests = test, progress = FALSE)
  )[[1L]]$p.value
  weighted <- stats::lm(y ~ x, data = sample$data, weights = w)
  c(
    ols = package,
    weights_as_precision = suppressWarnings(runner(weighted, sample$data))$p.value,
    design_based(sample$design, white)
  )
}

# --- run ----------------------------------------------------------------------

set.seed(seed)
rows <- list()
for (design_name in names(DESIGNS)) {
  for (n in sample_sizes) {
    for (test in c("koenker", "white")) {
      p <- vapply(seq_len(n_mc), function(i) {
        tryCatch(
          one_draw(DESIGNS[[design_name]], n, test),
          error = function(e) rep(NA_real_, 4L)
        )
      }, numeric(4L))
      rate <- rowMeans(p < alpha, na.rm = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        test = test,
        procedure = c("ols", "weights_as_precision", "design_wald", "design_score"),
        design = design_name,
        n = n,
        replications = n_mc,
        rejection_rate = unname(rate),
        mc_se = sqrt(unname(rate) * (1 - unname(rate)) / n_mc),
        failures = unname(rowSums(is.na(p))),
        stringsAsFactors = FALSE
      )
      cat(sprintf(
        "%-17s n = %-5d %-8s %s\n", design_name, n, test,
        paste(sprintf("%.3f", rate), collapse = "  ")
      ))
    }
  }
}

results <- do.call(rbind, rows)
out <- file.path("inst", "validation", "survey-designs-size.csv")
if (!dir.exists(dirname(out))) out <- "survey-designs-size.csv"
utils::write.csv(results, out, row.names = FALSE)
cat("Wrote ", nrow(results), " rows to ", out, "\n", sep = "")
