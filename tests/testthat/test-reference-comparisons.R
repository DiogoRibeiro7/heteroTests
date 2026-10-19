library(testthat)


test_that("performBPTest matches lmtest::bptest", {
  skip_if_not_installed("lmtest")
  set.seed(101)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- performBPTest(model, df)
  ref <- lmtest::bptest(model, studentize = FALSE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("performKoenkerTest matches studentized lmtest::bptest", {
  skip_if_not_installed("lmtest")
  set.seed(101)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- performKoenkerTest(model, df)
  ref <- lmtest::bptest(model, studentize = TRUE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("studentized BP matches lmtest::bptest with studentize", {
  skip_if_not_installed("lmtest")
  set.seed(202)
  df <- data.frame(y = rnorm(150), x1 = rnorm(150), x2 = rnorm(150))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- without_deprecation(performStudentizedBPTest(model, df))
  ref <- lmtest::bptest(model, studentize = TRUE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("performGQTest matches lmtest::gqtest across alternatives", {
  skip_if_not_installed("lmtest")
  data(mtcars)
  model <- lm(mpg ~ wt + qsec, data = mtcars)

  for (alternative in c("greater", "two.sided", "less")) {
    ours <- performGQTest(
      model,
      mtcars,
      order_by = "wt",
      fraction = 0.2,
      alternative = alternative
    )
    ref <- lmtest::gqtest(
      model,
      point = 0.5,
      fraction = 0.2,
      alternative = alternative,
      order.by = mtcars$wt
    )

    expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10)
    expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
    expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-10)
    expect_equal(ours$alternative, ref$alternative)
  }
})

# lmtest::gqtest() fits each segment to the rows of the design of the fit,
# model.matrix(), and so does performGQTest() since 0.16.0 (decided
# 2026-10-18). Before, an unweighted fit was refitted from the formula on each
# segment, which computed again a term that depends on the rows it is
# computed from: ns() and bs() place their knots at quantiles of those rows,
# and I(x1 > median(x1)) takes their median.
test_that("performGQTest() on a term that depends on its rows matches lmtest::gqtest()", {
  skip_if_not_installed("lmtest")
  set.seed(404)
  n <- 150
  df <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n))
  df$y <- 1 + sin(2 * df$x1) - df$x2 + rnorm(n, sd = df$x1)
  models <- list(
    ns = lm(y ~ splines::ns(x1, 3) + x2, data = df),
    bs = lm(y ~ splines::bs(x1, df = 4) + x2, data = df),
    median = lm(y ~ I(x1 > median(x1)) + x2, data = df),
    poly = lm(y ~ poly(x1, 2) + x2, data = df)
  )
  # Ordered by x1, a basis function of bs(), or the indicator, is zero on a
  # whole segment, whose design is then rank-deficient: lmtest::gqtest() takes
  # its degrees of freedom from the number of columns, and performGQTest()
  # from the rank of the segment, as before 0.16.0. They are compared where
  # every column varies in both segments.
  orderings <- list(ns = c("x1", "x2"), bs = "x2", median = "x2", poly = c("x1", "x2"))
  for (nm in names(models)) {
    for (order_by in orderings[[nm]]) {
      ours <- performGQTest(models[[nm]], df, order_by = order_by)
      ref <- lmtest::gqtest(models[[nm]], point = 0.5, fraction = 0.2, order.by = df[[order_by]])
      info <- paste(nm, order_by)
      expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10, info = info)
      expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0, info = info)
      expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-10, info = info)
    }
  }
  # The same columns computed beforehand give the same value.
  basis <- splines::ns(df$x1, 3)
  df$n1 <- basis[, 1]
  df$n2 <- basis[, 2]
  df$n3 <- basis[, 3]
  expect_equal(
    unname(performGQTest(models$ns, df, order_by = "x1")$statistic),
    unname(performGQTest(lm(y ~ n1 + n2 + n3 + x2, data = df), df, order_by = "x1")$statistic),
    tolerance = 1e-10
  )
})

# Ordered by x1, the indicator I(x1 > median(x1)) is constant on each segment,
# and a basis function of bs() can be zero on a whole segment: the design of
# the segment is then rank-deficient. Its degrees of freedom are its rows minus
# its rank, as lm.fit() gives them, which with normal errors makes the ratio
# of the two mean squares exactly F; lmtest::gqtest() subtracts the number of
# columns instead.
test_that("performGQTest() takes the degrees of freedom of a segment from its rank", {
  set.seed(404)
  n <- 150
  df <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n))
  df$y <- 1 + sin(2 * df$x1) - df$x2 + rnorm(n, sd = df$x1)
  models <- list(
    median = lm(y ~ I(x1 > median(x1)) + x2, data = df),
    bs = lm(y ~ splines::bs(x1, df = 4) + x2, data = df)
  )
  for (nm in names(models)) {
    m <- models[[nm]]
    ours <- performGQTest(m, df, order_by = "x1")
    X <- model.matrix(m)
    y <- model.response(model.frame(m))
    ord <- order(df$x1)
    point1 <- floor(0.4 * n)
    point2 <- ceiling(0.6 * n + 0.01)
    segment <- function(rows) lm.fit(X[ord[rows], , drop = FALSE], y[ord[rows]])
    first <- segment(seq_len(point1))
    second <- segment(seq.int(point2, n))
    expect_true(first$rank < ncol(X) || second$rank < ncol(X), info = nm)
    expect_identical(unname(ours$parameter), c(second$df.residual, first$df.residual), info = nm)
    statistic <- (sum(second$residuals^2) / second$df.residual) /
      (sum(first$residuals^2) / first$df.residual)
    expect_equal(unname(ours$statistic), statistic, tolerance = 1e-12, info = nm)
    expect_equal(ours$p.value,
                 stats::pf(statistic, second$df.residual, first$df.residual, lower.tail = FALSE),
                 tolerance = 1e-12, info = nm)
  }
})

# lmtest::gqtest() leaves the offset out, so the reference of a fit with an
# offset is lmtest::gqtest() of the same model fitted to the response minus
# the offset; performGQTest() fits each segment with the offset of the fit
# since 0.16.0.
test_that("performGQTest() on a fit with an offset matches lmtest::gqtest() of y - o", {
  skip_if_not_installed("lmtest")
  set.seed(405)
  n <- 150
  df <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n), u = rnorm(n))
  df$o <- 0.8 * df$u
  df$y <- 1 + df$x1 - df$x2 + df$o + rnorm(n, sd = df$x1)
  df$yo <- df$y - df$o
  reference_model <- lm(yo ~ x1 + x2, data = df)
  for (m in list(lm(y ~ x1 + x2, data = df, offset = o), lm(y ~ x1 + x2 + offset(o), data = df))) {
    for (alternative in c("greater", "two.sided", "less")) {
      ours <- performGQTest(m, df, order_by = "x1", alternative = alternative)
      ref <- lmtest::gqtest(reference_model, point = 0.5, fraction = 0.2,
                            alternative = alternative, order.by = df$x1)
      expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10)
      expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
      expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-10)
    }
  }
})

test_that("Levene test aligns with car::leveneTest", {
  skip_if_not_installed("car")
  set.seed(303)
  df <- data.frame(
    y = rnorm(90),
    g = factor(rep(letters[1:3], each = 30)),
    x = rnorm(90)
  )
  model <- lm(y ~ x, data = df)
  ours <- performLeveneTest(model, df, "g")
  ref <- car::leveneTest(resid(model) ~ g, data = df, center = mean)
  expect_equal(unname(ours$statistic), ref$`F value`[1], tolerance = 1e-6)
  expect_equal(unname(ours$p.value), ref$`Pr(>F)`[1], tolerance = 1e-6)
})
