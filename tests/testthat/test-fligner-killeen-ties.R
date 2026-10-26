# performFlignerKilleenTest() and its ties (0.16.0). The deviations from the
# group medians are ranked on residuals computed observation by observation,
# sqrt(w) (y - X b - o), where the residuals an lm stores, from its QR
# decomposition, differ in their last bits between repeated observations
# (rfligner_residuals()), and observations with the same values of the
# model's variables are given one residual (rfligner_same_observations());
# and deviations closer than
# 8 * .Machine$double.eps times the largest absolute residual of their group
# are ranked as tied, which restores the tie of the two residuals in the
# middle of a group of even size (rfligner_rounding_ties()).

test_that("the lm and the Gaussian glm of the same model give the same statistic", {
  # The case that exposed it: before 0.16.0 the lm gave 9.3670076662104975
  # and the glm 9.3670316853686657, on Windows.
  d <- contract_data()$d
  fits <- list(
    lm = lm(y ~ x1 + x2, data = d),
    glm = glm(y ~ x1 + x2, data = d)
  )
  out <- lapply(fits, performFlignerKilleenTest, data = d, group = "group")
  expect_identical(out$lm$statistic, out$glm$statistic)
  expect_identical(out$lm$p.value, out$glm$p.value)
  ref <- fligner_median_tied(residuals(fits$glm, type = "pearson"), d$group)
  expect_identical(unname(out$lm$statistic), unname(ref$statistic))
  expect_identical(out$lm$p.value, ref$p.value)
  expect_equal(unname(out$lm$statistic), 9.3670316853686657, tolerance = 1e-12)

  # The weighted lm and the weighted Gaussian glm, whose Pearson residuals are
  # sqrt(w) e.
  d$w <- 1 / d$x1^2
  weighted <- list(
    lm = lm(y ~ x1 + x2, data = d, weights = w),
    glm = glm(y ~ x1 + x2, data = d, weights = w)
  )
  out <- lapply(weighted, performFlignerKilleenTest, data = d, group = "group")
  expect_identical(out$lm$statistic, out$glm$statistic)
  expect_identical(out$lm$p.value, out$glm$p.value)
})

test_that("repeated observations tie, as in exact arithmetic", {
  # An integer response on a factor: the residuals of the lm centred at their
  # group medians are the responses centred at theirs, so the statistic of
  # exact ties is stats::fligner.test() of the responses. Before 0.16.0 the
  # lm gave 1.6149 and 2.9985 for these two, where exact ties give 1.5420 and
  # 2.4926.
  g <- factor(rep(1:4, length.out = 60))
  set.seed(8060)
  small <- data.frame(y = stats::rpois(60, 3), g = g)
  set.seed(9060)
  far <- data.frame(y = 1e4 + stats::rpois(60, 3), g = g)
  for (d in list(small, far)) {
    exact <- stats::fligner.test(d$y, d$g)
    for (m in list(lm(y ~ g, data = d), glm(y ~ g, data = d))) {
      out <- performFlignerKilleenTest(m, d, "g")
      expect_equal(unname(out$statistic), unname(exact$statistic), tolerance = 1e-12)
      expect_equal(out$p.value, exact$p.value, tolerance = 1e-12)
    }
  }

  # Responses recorded with one decimal: sleep, whose statistic was 0.1539
  # before 0.16.0, and that of exact ties, 0.2127, is stats::fligner.test() of
  # the responses scaled to integers; warpbreaks, 3.4404 before and 3.9015.
  exact <- stats::fligner.test(round(10 * sleep$extra), sleep$group)
  out <- performFlignerKilleenTest(lm(extra ~ group, data = sleep), sleep, "group")
  expect_equal(unname(out$statistic), unname(exact$statistic), tolerance = 1e-12)
  expect_equal(unname(out$statistic), 0.2127120, tolerance = 1e-6)
  exact <- stats::fligner.test(warpbreaks$breaks, warpbreaks$tension)
  out <- performFlignerKilleenTest(lm(breaks ~ tension, data = warpbreaks), warpbreaks, "tension")
  expect_equal(unname(out$statistic), unname(exact$statistic), tolerance = 1e-12)

  # poly() builds its basis with a QR decomposition of its own, which gives
  # repeated observations design rows that differ in their last bits. They
  # have the same values of the model's variables, so they are given one
  # residual, and the poly() form gives the value of the raw powers, that of
  # exact ties, 0.91385; before 0.16.0 it gave 0.8001.
  set.seed(5)
  b <- data.frame(x = round(stats::runif(30, 0, 10), 1))
  b$y <- round(10 * b$x + stats::rnorm(30), 1)
  d <- b[c(1:30, 1:30, 1:10), ]
  d$g <- factor(rep(1:4, length.out = 70))
  basis <- performFlignerKilleenTest(lm(y ~ poly(x, 3), data = d), d, "g")
  powers <- performFlignerKilleenTest(lm(y ~ x + I(x^2) + I(x^3), data = d), d, "g")
  expect_equal(unname(basis$statistic), unname(powers$statistic), tolerance = 1e-12)
  expect_equal(basis$p.value, powers$p.value, tolerance = 1e-12)
  expect_equal(unname(basis$statistic), 0.91385, tolerance = 1e-4)

  # Every row twice: the lm and the glm give the same statistic.
  twice <- iris[c(seq_len(150), seq_len(150)), ]
  for (f in list(Sepal.Length ~ Petal.Length, Sepal.Length ~ Petal.Length + Species,
                 Sepal.Length ~ poly(Petal.Length, 2) + Species)) {
    a <- performFlignerKilleenTest(lm(f, data = twice), twice, "Species")
    b <- performFlignerKilleenTest(glm(f, data = twice), twice, "Species")
    expect_identical(a$statistic, b$statistic)
    expect_identical(a$p.value, b$p.value)
  }
})

test_that("a tie that rounding broke at a group median is ranked as a tie", {
  # Two groups of four whose middle residuals, 0.1 and 0.3 in the first and
  # 0.7 and 1.3 in the second, sit at the same distance from the median in
  # exact arithmetic. In IEEE arithmetic the median of the first is the
  # double nearest 0.2, and 0.1 - 0.2 and 0.3 - 0.2 differ in size.
  x <- c(-2.5, 0.1, 0.3, 2.2, -1.9, 0.7, 1.3, 3.1)
  g <- factor(rep(c("a", "b"), each = 4))
  centred <- x - tapply(x, g, stats::median)[g]
  expect_false(abs(centred[2]) == abs(centred[3]))
  tied <- rfligner_rounding_ties(x, g)
  expect_false(is.null(tied))
  expect_identical(abs(tied[2]), abs(tied[3]))
  expect_identical(abs(tied[6]), abs(tied[7]))
  # The other deviations are left as they are.
  expect_identical(tied[-c(2, 3, 6, 7)], unname(as.vector(centred[-c(2, 3, 6, 7)])))
  expect_identical(
    unname(stats::fligner.test(tied, g)$statistic),
    unname(fligner_median_tied(x, g)$statistic)
  )
})

test_that("only deviations within the tolerance of their groups are tied", {
  # The tolerance is 8 eps times the largest absolute residual of the group,
  # the smaller of the two for deviations in different groups: here 8 eps.
  # The deviation of the second group, 1 + 4 eps in size, is tied with the 1
  # of the first group; 1 + 64 eps is not.
  eps <- .Machine$double.eps
  two <- factor(rep(c("a", "b"), each = 3))
  close <- rfligner_rounding_ties(c(-1, 0, 1, -1 - 4 * eps, 0, 5), two)
  expect_identical(close, c(-1, 0, 1, -1, 0, 5))
  expect_null(rfligner_rounding_ties(c(-1, 0, 1, -1 - 64 * eps, 0, 5), two))

  # A group far from zero does not widen the tolerance of the others: the
  # deviations 1 and 1 + 1e-12 of the second and third groups stay apart,
  # which a tolerance of 8 eps times the largest residual of all, 1e8,
  # would tie.
  three <- factor(rep(c("a", "b", "c"), each = 3))
  x <- c(1e8 - 1, 1e8, 1e8 + 1, -1, 0, 1 + 1e-12, -1, 0, 1)
  expect_null(rfligner_rounding_ties(x, three))
})

test_that("residuals with no tie that rounding broke give stats::fligner.test() unchanged", {
  # Groups of odd size, whose medians are residuals: the centring is exact,
  # and the residuals computed row by row rank as those of the fit.
  d <- contract_data()$d
  d$odd <- factor(rep(c("a", "b", "c", "d"), times = c(31, 29, 31, 29)))
  m <- lm(y ~ x1 + x2, data = d)
  expect_true(all(table(d$odd) %% 2L == 1L))
  expect_null(rfligner_rounding_ties(unname(residuals(m)), d$odd))
  ours <- performFlignerKilleenTest(m, d, "odd")
  ref <- stats::fligner.test(residuals(m), d$odd)
  expect_identical(unname(ours$statistic), unname(ref$statistic))
  expect_identical(ours$p.value, ref$p.value)
})
