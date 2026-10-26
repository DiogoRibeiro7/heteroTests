# The reference of Fligner-Killeen's statistic with the tie at each group
# median exact, built without the tolerance of rfligner_rounding_ties()
# (R/performFlignerKilleenTest.R). In a group of even size the two residuals
# in the middle, a <= b, lie at the same distance from the median in exact
# arithmetic, (b - a) / 2, which rounding often breaks. Here the deviations
# from the group medians are computed as stats::fligner.test() computes them,
# and the two in the middle of each group of even size are set to -h and h,
# with h = (b - a) / 2. The result has a median of zero in every group, so
# stats::fligner.test() of it centres nothing and ranks the two as tied: the
# statistic of exact arithmetic on residuals with no other tie. Since 0.16.0
# performFlignerKilleenTest() gives this value.
fligner_median_tied <- function(r, g) {
  g <- droplevels(factor(g))
  r <- unname(as.vector(r))
  deviations <- numeric(length(r))
  for (level in levels(g)) {
    i <- which(g == level)
    v <- r[i]
    dev <- v - stats::median(v)
    n_i <- length(v)
    if (n_i %% 2L == 0L) {
      o <- order(v)
      h <- (v[o[n_i / 2L + 1L]] - v[o[n_i / 2L]]) / 2
      dev[o[n_i / 2L]] <- -h
      dev[o[n_i / 2L + 1L]] <- h
    }
    deviations[i] <- dev
  }
  stats::fligner.test(deviations, g)
}
