# Where the over-rejection with fixed effects comes from ---------------------------
#
# Run from the package root:
#   Rscript inst/validation/fixed-effects-dummies.R
#
# fixed-effects-size.R found that White's, Koenker's and the Breusch-Pagan
# tests over-reject on lm(y ~ x + subject) when the subjects are many and
# small. This script splits the auxiliary regression of Koenker's and the
# Breusch-Pagan statistics, on the same replications, into the regression of
# the squared residuals on the subject dummies (G - 1 degrees of freedom) and
# the one on x alone (1 degree of freedom), to show which part over-rejects.
# A diagnostic, not a proposed test; it does not load the package. The x-alone
# columns are what a test of the squared residuals against x, without the
# dummies, would give (a variance-regressor argument of performBPTest() and
# performKoenkerTest(), recorded in the roadmap for after 1.0.0).
#
# The design is that of fixed-effects-size.R, balanced: G subjects of T
# observations, y = 1 + 0.5 x + a_i + e with x and e standard normal, at seven
# of its cells. The residuals are computed by the within transformation, which
# gives those of the fit, and do not depend on the subject effects a_i or on
# the coefficients, so neither is drawn. N_MC replications per cell, 2000 by
# default, all from one seed. CSV names the file written, by default
# inst/validation/fixed-effects-dummies.csv. It takes seconds.

n_mc <- as.integer(Sys.getenv("N_MC", unset = "2000"))
csv_path <- Sys.getenv(
  "CSV",
  unset = if (dir.exists(file.path("inst", "validation"))) {
    file.path("inst", "validation", "fixed-effects-dummies.csv")
  } else {
    "fixed-effects-dummies.csv"
  }
)

set.seed(20261024L)
cells <- list(c(30, 3), c(100, 3), c(100, 4), c(200, 5), c(30, 8), c(100, 8), c(200, 30))
R <- n_mc
out <- do.call(rbind, lapply(cells, function(gt) {
  G <- gt[1]
  T <- gt[2]
  n <- G * T
  g <- rep(seq_len(G), each = T)
  within <- function(M) M - (rowsum(M, g) / T)[g, , drop = FALSE]
  x <- matrix(stats::rnorm(n * R), n, R)
  e <- matrix(stats::rnorm(n * R), n, R)
  xw <- within(x)
  ew <- within(e)
  b <- colSums(xw * ew) / colSums(xw^2)
  r <- ew - xw * rep(b, each = n)
  e2 <- r^2
  e2c <- e2 - rep(colMeans(e2), each = n)
  tss <- colSums(e2c^2)
  # The explained sums of squares of the squared residuals: on [1, x], with
  # 1 degree of freedom; on [1, dummies], with G - 1; on [1, x, dummies],
  # those of the tests, with G.
  xc <- x - rep(colMeans(x), each = n)
  ess_x <- colSums(xc * e2c)^2 / colSums(xc^2)
  ess_d <- tss - colSums(within(e2)^2)
  e2w <- within(e2)
  ess_k <- tss - (colSums(e2w^2) - colSums(xw * e2w)^2 / colSums(xw^2))
  # Koenker's statistic is n ESS / TSS; the Breusch-Pagan statistic is half
  # the explained sum of squares of e^2 / sigma^2 - 1, ESS / (2 sigma^4), with
  # sigma^2 the mean of the squared residuals.
  sigma4 <- colMeans(e2)^2
  rate <- function(stat, df) mean(stats::pchisq(stat, df, lower.tail = FALSE) < 0.05)
  data.frame(
    G = G, T = T, n = n, replications = R,
    koenker = rate(n * ess_k / tss, G),
    koenker_dummies_only = rate(n * ess_d / tss, G - 1),
    koenker_x_only = rate(n * ess_x / tss, 1),
    breusch_pagan = rate(0.5 * ess_k / sigma4, G),
    breusch_pagan_dummies_only = rate(0.5 * ess_d / sigma4, G - 1),
    breusch_pagan_x_only = rate(0.5 * ess_x / sigma4, 1)
  )
}))
print(out)
utils::write.csv(out, csv_path, row.names = FALSE)
message("wrote ", csv_path)
