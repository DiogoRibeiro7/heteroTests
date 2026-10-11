# performHarrisonMcCabeTest(): the share of the residual sum of squares in the
# first part of the ordered sample, with its exact p-value given the design.

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# The cross-sectional design of inst/validation/pass-a-size-power.R. The rows
# are in random order with respect to x1, so the test has to reorder the
# design as well as the residuals.
hmc_data <- function(n = 60, seed = 1, sd = function(x1) rep(1, length(x1))) {
  set.seed(seed)
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  d <- data.frame(x1 = x1, x2 = x2, y = 1 + 2 * x1 + 0.5 * x2 + sd(x1) * rnorm(n))
  d$w <- 1 / x1^2
  list(data = d, model = lm(y ~ x1 + x2, data = d))
}

# The statistic from its definition: refit on the sorted data and take the
# share of the first m squared residuals.
hmc_by_definition <- function(formula, data, order_by, m) {
  sorted <- data[order(data[[order_by]]), , drop = FALSE]
  e <- residuals(lm(formula, data = sorted))
  sum(e[seq_len(m)]^2) / sum(e^2)
}

# P(b <= b0) from the full n x n matrices of the ordered design and Imhof's
# formula integrated over u, written independently of the package code.
hmc_lower_tail_by_definition <- function(design, m, b0) {
  n <- nrow(design)
  resid_maker <- diag(n) - design %*% solve(crossprod(design), t(design))
  first <- diag(c(rep(1, m), rep(0, n - m)))
  lambda <- eigen(resid_maker %*% first %*% resid_maker - b0 * resid_maker,
                  symmetric = TRUE, only.values = TRUE)$values
  lambda <- lambda[abs(lambda) > 1e-12]
  integrand <- function(u) {
    vapply(u, function(v) {
      sin(0.5 * sum(atan(lambda * v))) / (v * prod((1 + lambda^2 * v^2)^0.25))
    }, numeric(1))
  }
  0.5 - integrate(integrand, 0, Inf, rel.tol = 1e-12, abs.tol = 1e-15,
                  subdivisions = 5000L)$value / pi
}

test_that("the statistic is the one lmtest::hmctest() computes", {
  skip_if_not_installed("lmtest")

  mtcars_fit <- lm(mpg ~ wt + qsec, data = mtcars)
  for (order_by in c("wt", "qsec", "hp")) {
    for (point in c(0.5, 0.3, 0.75, 8)) {
      result <- quiet(performHarrisonMcCabeTest(mtcars_fit, mtcars, order_by, point = point))
      reference <- lmtest::hmctest(
        mtcars_fit, point = point, order.by = stats::as.formula(paste("~", order_by)),
        data = mtcars, simulate.p = FALSE
      )
      expect_equal(unname(result$statistic), unname(reference$statistic), tolerance = 1e-8,
                   info = paste(order_by, point))
    }
  }

  # An odd number of observations, where floor(point * n) and round(point * n)
  # differ.
  obj <- hmc_data(n = 57, seed = 3)
  for (point in c(0.5, 0.3, 0.77, 10, 40)) {
    result <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = point))
    reference <- lmtest::hmctest(obj$model, point = point, order.by = ~x1, data = obj$data,
                                 simulate.p = FALSE)
    expect_equal(unname(result$statistic), unname(reference$statistic), tolerance = 1e-8,
                 info = point)
    expect_equal(unname(result$parameter[["m"]]), if (point < 1) floor(point * 57) else point)
  }
})

test_that("the statistic is the share of the first part of the ordered residuals", {
  obj <- hmc_data(n = 45, seed = 2)
  # 0.3 * 45 is 13.5, where floor() and round() part: the first part holds 13
  # observations, as in lmtest::hmctest() and Harrison (1981).
  expect_identical(floor(0.3 * 45), 13)
  expect_identical(round(0.3 * 45), 14)
  for (point in c(0.5, 0.2, 0.3, 30)) {
    m <- if (point < 1) floor(point * 45) else point
    result <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = point))
    expect_equal(unname(result$statistic),
                 hmc_by_definition(y ~ x1 + x2, obj$data, "x1", m), tolerance = 1e-10)
    expect_equal(unname(result$parameter), c(m, 45))
  }

  # Ordered by a variable that is not in the model.
  obj$data$z <- rnorm(45)
  result <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "z"))
  expect_equal(unname(result$statistic),
               hmc_by_definition(y ~ x1 + x2, obj$data, "z", 22), tolerance = 1e-10)
})

test_that("the p-value is exact where the statistic has a beta distribution", {
  # With an intercept and a dummy for the second part of the ordering, the
  # first part's residuals are deviations from its own mean and the second
  # part's from its own, so b ~ Beta((m - 1) / 2, (n - m - 1) / 2). The rows
  # are shuffled: a test that left the design in the order of the data, that
  # simulated from a constant alone as lmtest::hmctest() does, or that
  # dropped repeated eigenvalues, gives a different p-value.
  for (n in c(21L, 40L, 101L)) {
    set.seed(n)
    m <- floor(n / 2)
    d <- data.frame(z = sample(n), y = rnorm(n))
    d$upper <- as.numeric(d$z > m)
    fit <- lm(y ~ upper, data = d)
    a <- (m - 1) / 2
    b <- (n - m - 1) / 2

    less <- quiet(performHarrisonMcCabeTest(fit, d, "z"))
    greater <- quiet(performHarrisonMcCabeTest(fit, d, "z", alternative = "greater"))
    both <- quiet(performHarrisonMcCabeTest(fit, d, "z", alternative = "two.sided"))
    share <- unname(less$statistic)

    expect_equal(less$p.value, pbeta(share, a, b), tolerance = 1e-10, info = n)
    expect_equal(greater$p.value, pbeta(share, a, b, lower.tail = FALSE), tolerance = 1e-10,
                 info = n)
    smaller_tail <- min(pbeta(share, a, b), pbeta(share, a, b, lower.tail = FALSE))
    expect_equal(both$p.value, min(1, 2 * smaller_tail), tolerance = 1e-10, info = n)
  }

  # Without an intercept, and with a regressor that is zero in the first part,
  # the first part is not fitted at all: b ~ Beta(m / 2, (n - m - 1) / 2).
  set.seed(9)
  n <- 30L
  m <- 10L
  d <- data.frame(z = sample(n))
  d$x <- ifelse(d$z > m, runif(n, 1, 2), 0)
  d$y <- 3 * d$x + rnorm(n)
  fit <- lm(y ~ 0 + x, data = d)
  result <- quiet(performHarrisonMcCabeTest(fit, d, "z", point = m))
  expect_equal(result$p.value, pbeta(unname(result$statistic), m / 2, (n - m - 1) / 2),
               tolerance = 1e-10)
})

test_that("the exact critical values are those Harrison (1981) published for a trend", {
  # Harrison (1981, Table 3) gives the exact 5% critical values of the test for
  # a constant and the trend 1, ..., n, with m the integer part of n / 2, to
  # three decimals. They are recomputed here from the package's own null
  # distribution, and the exported function is checked to use that
  # distribution on the same design with its rows shuffled.
  published <- c(`10` = 0.147, `20` = 0.243, `30` = 0.289, `40` = 0.317)
  for (n in c(10L, 20L, 30L, 40L)) {
    spectrum <- heteroTests:::rhmc_spectrum(qr(cbind(1, seq_len(n))), n %/% 2)
    critical <- stats::uniroot(function(b) {
      heteroTests:::rhmc_tails(b, spectrum)[["lower"]] - 0.05
    }, c(1e-6, 0.999), tol = 1e-12)$root
    expect_lt(abs(critical - published[[as.character(n)]]), 5e-4)

    set.seed(n)
    d <- data.frame(trend = sample(n), y = rnorm(n))
    result <- quiet(performHarrisonMcCabeTest(lm(y ~ trend, data = d), d, "trend"))
    expect_equal(result$p.value,
                 heteroTests:::rhmc_tails(unname(result$statistic), spectrum)[["lower"]],
                 tolerance = 1e-10)
  }
})

test_that("the p-value lies between those of the beta bounds", {
  # Harrison and McCabe bound b between b_L ~ Beta((m - k) / 2, (n - m) / 2)
  # and b_U ~ Beta(m / 2, (n - m - k) / 2) (Harrison 1981, Section II), so
  # P(b_U <= b0) <= P(b <= b0) <= P(b_L <= b0) for every design. Harrison
  # (1981) prints the 5% bounds (0.193, 0.289) for n = 20 and k = 2 and
  # (0.095, 0.375) for k = 5.
  expect_lt(abs(qbeta(0.05, 8 / 2, 10 / 2) - 0.193), 1e-3)
  expect_lt(abs(qbeta(0.05, 10 / 2, 8 / 2) - 0.289), 1e-3)
  expect_lt(abs(qbeta(0.05, 5 / 2, 10 / 2) - 0.095), 1e-3)
  expect_lt(abs(qbeta(0.05, 10 / 2, 5 / 2) - 0.375), 1e-3)

  set.seed(31)
  for (i in 1:40) {
    n <- sample(15:120, 1)
    k <- sample(2:5, 1)
    m <- sample(k:(n - k), 1)
    d <- data.frame(matrix(rnorm(n * 4), n), z = rnorm(n), y = rnorm(n))
    formula <- stats::reformulate(paste0("X", seq_len(k - 1)), response = "y")
    result <- quiet(performHarrisonMcCabeTest(lm(formula, data = d), d, "z", point = m))
    share <- unname(result$statistic)
    expect_gte(result$p.value, pbeta(share, m / 2, (n - m - k) / 2) - 1e-9)
    expect_lte(result$p.value, pbeta(share, (m - k) / 2, (n - m) / 2) + 1e-9)
  }
})

test_that("the two-sided p-value is twice the smaller tail and at most 1", {
  # Samples whose statistic falls on each side of the centre of its null
  # distribution, so that one of the one-sided p-values is above 0.5.
  found <- c(below = FALSE, above = FALSE)
  for (seed in 1:200) {
    obj <- hmc_data(n = 40, seed = 900 + seed)
    less <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1"))$p.value
    side <- if (less > 0.5 && less < 0.8) "above" else if (less > 0.2 && less < 0.5) "below"
    if (is.null(side) || found[[side]]) next
    found[[side]] <- TRUE
    greater <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1",
                                               alternative = "greater"))$p.value
    both <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1",
                                            alternative = "two.sided"))$p.value
    expect_lte(both, 1)
    expect_gt(both, 0.4)
    expect_equal(both, 2 * min(less, greater), tolerance = 1e-12)
    if (all(found)) break
  }
  expect_true(all(found))
})

test_that("the p-value agrees with the full projection matrix of the ordered design", {
  mtcars_fit <- lm(mpg ~ wt + qsec, data = mtcars)
  design <- model.matrix(mtcars_fit)[order(mtcars$wt), ]
  for (alternative in c("less", "greater")) {
    result <- quiet(performHarrisonMcCabeTest(mtcars_fit, mtcars, "wt", alternative = alternative))
    lower <- hmc_lower_tail_by_definition(design, 16, unname(result$statistic))
    expect_equal(result$p.value, if (alternative == "less") lower else 1 - lower,
                 tolerance = 1e-8, info = alternative)
  }

  obj <- hmc_data(n = 50, seed = 4, sd = function(x1) exp(0.3 * x1))
  fit <- lm(y ~ x1 + x2 + I(x1^2), data = obj$data)
  design <- model.matrix(fit)[order(obj$data$x1), ]
  for (point in c(0.5, 0.3)) {
    m <- floor(point * 50)
    result <- quiet(performHarrisonMcCabeTest(fit, obj$data, "x1", point = point))
    expect_equal(result$p.value, hmc_lower_tail_by_definition(design, m, unname(result$statistic)),
                 tolerance = 1e-8, info = point)
  }
})

test_that("a weighted fit is tested as the equivalent unweighted regression", {
  obj <- hmc_data(n = 60, seed = 5, sd = function(x1) x1)
  weighted <- lm(y ~ x1 + x2, data = obj$data, weights = w)

  transformed <- data.frame(
    x1 = obj$data$x1,
    sw = sqrt(obj$data$w),
    swx1 = sqrt(obj$data$w) * obj$data$x1,
    swx2 = sqrt(obj$data$w) * obj$data$x2,
    swy = sqrt(obj$data$w) * obj$data$y
  )
  unweighted <- lm(swy ~ 0 + sw + swx1 + swx2, data = transformed)

  for (alternative in c("less", "greater", "two.sided")) {
    a <- quiet(performHarrisonMcCabeTest(weighted, obj$data, "x1", alternative = alternative))
    b <- quiet(performHarrisonMcCabeTest(unweighted, transformed, "x1", alternative = alternative))
    expect_equal(unname(a$statistic), unname(b$statistic), tolerance = 1e-10, info = alternative)
    expect_equal(a$p.value, b$p.value, tolerance = 1e-10, info = alternative)
  }

  # The statistic is the share of the Pearson residuals, not of the raw ones.
  e <- (sqrt(obj$data$w) * residuals(weighted))[order(obj$data$x1)]
  a <- quiet(performHarrisonMcCabeTest(weighted, obj$data, "x1"))
  expect_equal(unname(a$statistic), sum(e[1:30]^2) / sum(e^2), tolerance = 1e-10)

  obj$data$w[3] <- 0
  zero <- lm(y ~ x1 + x2, data = obj$data, weights = w)
  expect_error(performHarrisonMcCabeTest(zero, obj$data, "x1"), "zero or non-finite weights")
})

test_that("the result does not depend on how the model is parametrised or the rows ordered", {
  obj <- hmc_data(n = 48, seed = 6, sd = function(x1) exp(0.2 * x1))
  d <- obj$data
  d$x1_scaled <- 0.1 * d$x1
  d$x1_centred <- d$x1 - mean(d$x1)
  d$y_tripled <- 3 * d$y
  fits <- list(
    lm(y ~ x1 + I(x1^2), data = d),
    lm(y ~ x1_scaled + I(x1_scaled^2), data = d),
    lm(y ~ x1_centred + I(x1_centred^2), data = d),
    lm(y ~ poly(x1, 2), data = d),
    lm(y_tripled ~ x1 + I(x1^2), data = d)
  )
  reference <- quiet(performHarrisonMcCabeTest(fits[[1]], d, "x1", alternative = "two.sided"))
  for (i in seq_along(fits)[-1]) {
    result <- quiet(performHarrisonMcCabeTest(fits[[i]], d, "x1", alternative = "two.sided"))
    expect_equal(unname(result$statistic), unname(reference$statistic), tolerance = 1e-10,
                 info = i)
    expect_equal(result$p.value, reference$p.value, tolerance = 1e-10, info = i)
  }

  shuffled <- d[sample(nrow(d)), ]
  refit <- lm(y ~ x1 + I(x1^2), data = shuffled)
  result <- quiet(performHarrisonMcCabeTest(refit, shuffled, "x1", alternative = "two.sided"))
  expect_equal(unname(result$statistic), unname(reference$statistic), tolerance = 1e-10)
  expect_equal(result$p.value, reference$p.value, tolerance = 1e-10)
})

test_that("the alternative is the direction of the variance along the ordering", {
  rising <- hmc_data(n = 80, seed = 7, sd = function(x1) exp(0.5 * x1))
  less <- quiet(performHarrisonMcCabeTest(rising$model, rising$data, "x1"))
  greater <- quiet(performHarrisonMcCabeTest(rising$model, rising$data, "x1",
                                             alternative = "greater"))
  both <- quiet(performHarrisonMcCabeTest(rising$model, rising$data, "x1",
                                          alternative = "two.sided"))

  expect_lt(less$p.value, 0.01)
  expect_gt(greater$p.value, 0.99)
  expect_equal(less$p.value + greater$p.value, 1, tolerance = 1e-10)
  expect_equal(both$p.value, 2 * less$p.value, tolerance = 1e-10)
  expect_identical(less$alternative, "variance increases with 'x1'")
  expect_identical(greater$alternative, "variance decreases with 'x1'")
  expect_identical(both$alternative, "variance changes with 'x1'")

  # The same hypothesis is the default of the Goldfeld-Quandt and Szroeter
  # tests, where it is called "greater".
  expect_lt(quiet(performGQTest(rising$model, rising$data, "x1"))$p.value, 0.01)
  expect_lt(quiet(performSzroeterTest(rising$model, rising$data, "x1"))$p.value, 0.01)

  falling <- hmc_data(n = 80, seed = 7, sd = function(x1) exp(-0.5 * x1))
  expect_lt(
    quiet(performHarrisonMcCabeTest(falling$model, falling$data, "x1",
                                    alternative = "greater"))$p.value,
    0.01
  )
  expect_gt(quiet(performHarrisonMcCabeTest(falling$model, falling$data, "x1"))$p.value, 0.99)
})

test_that("the result is an htest that says what was computed", {
  obj <- hmc_data()
  result <- quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1"))
  expect_htest(result)
  expect_identical(names(result$statistic), "HMC")
  expect_identical(names(result$parameter), c("m", "n"))
  expect_identical(result$method, "Harrison-McCabe test for heteroscedasticity")
  expect_identical(result$data.name, "y ~ x1 + x2")
  expect_gt(unname(result$statistic), 0)
  expect_lt(unname(result$statistic), 1)
})

test_that("arguments are validated", {
  obj <- hmc_data(n = 40)
  for (bad in list(NA_character_, "", c("x1", "x2"), 1, NULL)) {
    expect_error(performHarrisonMcCabeTest(obj$model, obj$data, bad),
                 "`order_by` must be supplied as a single column name")
  }
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "absent"), "absent")
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", alternative = "up"))

  for (bad in list(0, -0.5, NA_real_, Inf, c(0.3, 0.5), "half")) {
    expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = bad),
                 "`point` must be a single positive number")
  }
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 12.5),
               "must be a whole number")
  # Each part needs at least as many observations as there are coefficients.
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 2),
               "Each part needs at least 3")
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 38),
               "Each part needs at least 3")
  expect_error(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 0.05),
               "Each part needs at least 3")
  expect_no_error(quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 3)))
  expect_no_error(quiet(performHarrisonMcCabeTest(obj$model, obj$data, "x1", point = 37)))

  expect_error(performHarrisonMcCabeTest(list(), obj$data, "x1"),
               "Provide an object fitted with stats::lm() or stats::glm()", fixed = TRUE)
  glm_fit <- glm(y ~ x1 + x2, data = obj$data)
  expect_error(performHarrisonMcCabeTest(glm_fit, obj$data, "x1"),
               "needs a model fitted by stats::lm")

  exact <- data.frame(x = 1:20, z = 20:1)
  exact$y <- 2 + 3 * exact$x
  expect_error(quiet(performHarrisonMcCabeTest(lm(y ~ x, data = exact), exact, "z")),
               "Residual variance is too small")

  # A first part that the model fits exactly makes the statistic zero whatever
  # the errors, and there is nothing to test.
  set.seed(8)
  saturated <- data.frame(z = sample(20), y = rnorm(20))
  saturated$first <- as.numeric(saturated$z == 1)
  saturated$second <- as.numeric(saturated$z == 2)
  fit <- lm(y ~ 0 + first + second, data = saturated)
  expect_error(quiet(performHarrisonMcCabeTest(fit, saturated, "z", point = 2)),
               "takes the same value whatever the errors")
})

# --- simulated guards -----------------------------------------------------------

test_that("the p-value is uniform under the null given the design", {
  skip_on_cran()
  # 40000 normal error vectors on the design of mtcars ordered by weight. The
  # Monte Carlo standard error of each rejection rate below is at most 0.0025.
  fit <- lm(mpg ~ wt + qsec + hp, data = mtcars)
  design <- model.matrix(fit)[order(mtcars$wt), ]
  basis <- qr(design)
  set.seed(42)
  errors <- matrix(rnorm(nrow(design) * 40000), nrow(design))
  resid <- qr.resid(basis, errors)
  share <- colSums(resid[1:16, ]^2) / colSums(resid^2)

  observed <- quiet(performHarrisonMcCabeTest(fit, mtcars, "wt"))
  simulated <- mean(share <= observed$statistic)
  expect_lt(abs(observed$p.value - simulated), 4 * sqrt(simulated * (1 - simulated) / 40000))

  # The null mean the help page states, (m - sum of the first m leverages) / (n - k).
  leverage <- rowSums(qr.Q(basis)^2)
  expect_lt(abs(mean(share) - (16 - sum(leverage[1:16])) / (32 - 4)), 4 * sd(share) / 200)

  for (level in c(0.01, 0.05, 0.1)) {
    # The exact critical value, found by inverting the package's p-value.
    critical <- stats::uniroot(function(b) {
      heteroTests:::rhmc_tails(b, heteroTests:::rhmc_spectrum(basis, 16))[["lower"]] - level
    }, c(0.01, 0.99), tol = 1e-12)$root
    expect_lt(abs(mean(share <= critical) - level), 4 * sqrt(level * (1 - level) / 40000))
  }
})

test_that("the test holds its level and has power, with and without weights", {
  skip_on_cran()
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  reps <- 300

  p <- vapply(seq_len(reps), function(i) {
    null <- hmc_data(n = 40, seed = 5000 + i)
    rising <- hmc_data(n = 40, seed = 6000 + i, sd = function(x1) exp(0.2 * x1))
    weighted <- hmc_data(n = 40, seed = 7000 + i, sd = function(x1) x1)
    weighted_fit <- lm(y ~ x1 + x2, data = weighted$data, weights = w)
    c(
      null = quiet(performHarrisonMcCabeTest(null$model, null$data, "x1"))$p.value,
      power = quiet(performHarrisonMcCabeTest(rising$model, rising$data, "x1"))$p.value,
      weighted = quiet(performHarrisonMcCabeTest(weighted_fit, weighted$data, "x1"))$p.value
    )
  }, numeric(3))

  # At 300 replications the Monte Carlo standard error at 5% is 1.3%.
  expect_lt(mean(p["null", ] < 0.05), 0.095)
  expect_gt(mean(p["null", ] < 0.05), 0.01)
  expect_lt(mean(p["weighted", ] < 0.05), 0.095)
  expect_gt(mean(p["weighted", ] < 0.05), 0.01)
  expect_gt(mean(p["power", ] < 0.05), 0.4)
})
