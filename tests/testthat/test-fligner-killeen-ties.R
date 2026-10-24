# performFlignerKilleenTest() and a tie at the median (0.16.0). In a group of
# even size the two residuals in the middle lie at the same distance from the
# group median, a tie that rounding often breaks, so that the statistic of an
# lm and of the Gaussian glm of the same model differed by about 1e-6 of its
# value. Deviations from the group medians closer than
# 8 * .Machine$double.eps * max(abs(residuals)) are ranked as tied
# (rfligner_rounding_ties()).

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

test_that("exact ties are averaged as stats::fligner.test() does, and distinct values kept", {
  # Integer responses on a factor, in groups of odd and of even size: the
  # deviations from the group medians are integers or halves of integers,
  # with many exact ties and no two distinct values closer than 1/2. The
  # residuals of the Gaussian glm are those of the group means, and its test
  # is that of the responses themselves.
  set.seed(3)
  g <- factor(rep(c("a", "b", "c", "d"), times = c(15, 16, 17, 12)))
  y <- stats::rpois(length(g), 3)
  d <- data.frame(y = y, g = g)
  expect_null(rfligner_rounding_ties(y, g))
  ours <- performFlignerKilleenTest(glm(y ~ g, data = d), d, "g")
  ref <- stats::fligner.test(y, g)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-12)
  expect_equal(ours$p.value, ref$p.value, tolerance = 1e-12)

  # Only deviations closer than 8 * eps * max|x| are ranked as tied: here
  # max|x| = 5, so 40 eps. The deviations of the second group, 1 + 4 eps and
  # 1 + 64 eps from its median in size, are tied with the 1 of the first
  # group in the first case and not in the second.
  eps <- .Machine$double.eps
  two <- factor(rep(c("a", "b"), each = 3))
  close <- rfligner_rounding_ties(c(-1, 0, 1, -1 - 4 * eps, 0, 5), two)
  expect_identical(close, c(-1, 0, 1, -1, 0, 5))
  expect_null(rfligner_rounding_ties(c(-1, 0, 1, -1 - 64 * eps, 0, 5), two))
})

test_that("residuals with no tie that rounding broke give stats::fligner.test() unchanged", {
  # Groups of odd size, whose medians are residuals: the centring is exact.
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
