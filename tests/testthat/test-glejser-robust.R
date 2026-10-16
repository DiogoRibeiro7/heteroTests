# performGlejserTest(robust = TRUE): the correction of Im (2000) and of Machado
# and Santos Silva (2000) for asymmetric errors. It is the default since
# 0.15.0; up to 0.14.0 the default was robust = FALSE, Glejser's (1969)
# statistic.

glejser_data <- function(n = 120, seed = 1, errors = c("exponential", "normal", "lognormal")) {
  errors <- match.arg(errors)
  set.seed(seed)
  x1 <- runif(n, 1, 5)
  x2 <- rnorm(n)
  e <- switch(errors,
    exponential = rexp(n) - 1,
    normal = rnorm(n),
    lognormal = (rlnorm(n) - exp(0.5)) / sqrt((exp(1) - 1) * exp(1))
  )
  d <- data.frame(x1 = x1, x2 = x2, y = 1 + 2 * x1 + 0.5 * x2 + e, w = 1 / x1^2)
  list(data = d, model = lm(y ~ x1 + x2, data = d))
}

# n R^2 from the t statistic of a regression on a constant and one regressor.
n_r_squared <- function(result, n) {
  t_stat <- unname(result$statistic)
  n * t_stat^2 / (t_stat^2 + n - 2)
}

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# Glejser's (1969) statistic as the default of 0.14.0 computed it: the t test of
# the slope in the regression of the absolute Pearson residuals on the
# transformed variable, by the same operations in the same order, so that the
# result can be compared with identical().
glejser_1969 <- function(model, z) {
  e <- residuals(model)
  if (!is.null(weights(model))) e <- sqrt(weights(model)) * e
  aux <- lm(abs_res ~ z, data = data.frame(abs_res = abs(e), z = z))
  coefficients <- summary(aux)$coefficients
  structure(
    list(
      statistic = c(t = coefficients[2, 3]),
      parameter = c(df = aux$df.residual),
      p.value = coefficients[2, 4],
      method = "Glejser test for heteroscedasticity",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}

test_that("the default is robust = TRUE, the corrected statistic", {
  expect_identical(formals(performGlejserTest)$robust, TRUE)

  for (errors in c("exponential", "normal")) {
    obj <- glejser_data(errors = errors)
    default <- quiet(performGlejserTest(obj$model, obj$data, "x1"))
    explicit <- quiet(performGlejserTest(obj$model, obj$data, "x1", robust = TRUE))
    expect_htest(default)
    expect_identical(default, explicit, info = errors)
    expect_identical(
      default$method,
      "Glejser test for heteroscedasticity (robust to asymmetric errors)",
      info = errors
    )
  }
})

test_that("robust = FALSE returns what the default of 0.14.0 returned", {
  obj <- glejser_data()
  x <- obj$data$x1
  regressors <- list(abs = abs(x), sqrt = sqrt(x), inverse = 1 / x, inverse_sqrt = 1 / sqrt(x))
  for (transformation in names(regressors)) {
    result <- quiet(performGlejserTest(obj$model, obj$data, "x1", transformation, robust = FALSE))
    expect_identical(result, glejser_1969(obj$model, regressors[[transformation]]),
                     info = transformation)
  }

  # From the model frame, and after a weighted fit.
  expect_identical(
    quiet(performGlejserTest(obj$model, variable = "x1", robust = FALSE)),
    glejser_1969(obj$model, abs(x))
  )
  weighted <- lm(y ~ x1 + x2, data = obj$data, weights = w)
  expect_identical(
    quiet(performGlejserTest(weighted, obj$data, "x1", robust = FALSE)),
    glejser_1969(weighted, abs(x))
  )

  # The values the default returned up to 0.14.0, computed at commit 5ce6042,
  # to 1e-12 rather than identically: the last bits of a QR decomposition can
  # differ between platforms. The identical() comparisons above hold on every
  # platform.
  pinned <- list(
    abs = c(t = -0.234183910481965, p = 0.815248040659971),
    inverse = c(t = -0.411476127924781, p = 0.681469846542957)
  )
  for (transformation in names(pinned)) {
    result <- quiet(performGlejserTest(obj$model, obj$data, "x1", transformation, robust = FALSE))
    expect_equal(unname(result$statistic), pinned[[transformation]][["t"]], tolerance = 1e-12,
                 info = transformation)
    expect_equal(result$p.value, pinned[[transformation]][["p"]], tolerance = 1e-12,
                 info = transformation)
    expect_identical(unname(result$parameter), 118L, info = transformation)
  }
})

test_that("the default reports the values NEWS.md gives for 0.15.0", {
  # Before 0.15.0 these calls returned the values given with robust = FALSE.
  mtcars_model <- lm(mpg ~ wt + qsec, data = mtcars)
  cars_model <- lm(dist ~ speed, data = cars)
  cases <- list(
    list(call = function(robust) performGlejserTest(mtcars_model, mtcars, "wt", robust = robust),
         before = c(t = -0.44953254825848, p = 0.656278705759926),
         now = c(t = -0.39528130563259, p = 0.695432088710059)),
    list(call = function(robust) performGlejserTest(cars_model, cars, "speed", robust = robust),
         before = c(t = 2.05905451377893, p = 0.0449368398858078),
         now = c(t = 1.94313334846181, p = 0.0578771236059652))
  )
  for (i in seq_along(cases)) {
    now <- quiet(cases[[i]]$call(TRUE))
    before <- quiet(cases[[i]]$call(FALSE))
    expect_equal(unname(now$statistic), cases[[i]]$now[["t"]], tolerance = 1e-10, info = i)
    expect_equal(now$p.value, cases[[i]]$now[["p"]], tolerance = 1e-10, info = i)
    expect_equal(unname(before$statistic), cases[[i]]$before[["t"]], tolerance = 1e-10, info = i)
    expect_equal(before$p.value, cases[[i]]$before[["p"]], tolerance = 1e-10, info = i)
  }
  expect_identical(
    quiet(performGlejserTest(mtcars_model, mtcars, "wt")),
    quiet(cases[[1]]$call(TRUE))
  )
})

test_that("robust = TRUE reproduces Im (2000) and Machado and Santos Silva (2000)", {
  obj <- glejser_data()
  e <- residuals(obj$model)
  n <- length(e)
  x <- obj$data$x1
  regressors <- list(abs = abs(x), sqrt = sqrt(x), inverse = 1 / x, inverse_sqrt = 1 / sqrt(x))

  for (transformation in names(regressors)) {
    z <- regressors[[transformation]]
    # Im: |e| - m e, with m the proportion of positive residuals less the
    # proportion of negative ones.
    m_hat <- (sum(e > 0) - sum(e < 0)) / n
    im <- n * summary(lm(I(abs(e) - m_hat * e) ~ z))$r.squared
    # Machado and Santos Silva: e (I(e >= 0) - eta), with eta the proportion of
    # non-negative residuals.
    eta_hat <- mean(e >= 0)
    mss <- n * summary(lm(I(e * ((e >= 0) - eta_hat)) ~ z))$r.squared

    # The slope of Im's auxiliary regression, with its sign and its p-value.
    slope <- summary(lm(I(abs(e) - m_hat * e) ~ z))$coefficients[2, ]

    result <- quiet(performGlejserTest(obj$model, obj$data, "x1", transformation, robust = TRUE))

    expect_htest(result)
    expect_equal(im, mss, tolerance = 1e-10, info = transformation)
    expect_equal(n_r_squared(result, n), im, tolerance = 1e-8, info = transformation)
    expect_equal(unname(result$statistic), slope[["t value"]], tolerance = 1e-8,
                 info = transformation)
    expect_equal(result$p.value, slope[["Pr(>|t|)"]], tolerance = 1e-8, info = transformation)
    expect_equal(unname(result$parameter), n - 2, info = transformation)
    expect_identical(names(result$statistic), "t")
    expect_identical(names(result$parameter), "df")
  }
})

test_that("the method label says which statistic was computed", {
  obj <- glejser_data()
  result <- quiet(performGlejserTest(obj$model, obj$data, "x1", robust = TRUE))
  expect_identical(
    result$method,
    "Glejser test for heteroscedasticity (robust to asymmetric errors)"
  )
})

test_that("the correction changes the statistic when the residuals are skewed", {
  obj <- glejser_data(errors = "exponential")
  original <- quiet(performGlejserTest(obj$model, obj$data, "x1", robust = FALSE))
  corrected <- quiet(performGlejserTest(obj$model, obj$data, "x1", robust = TRUE))

  expect_gt(abs(mean(sign(residuals(obj$model)))), 0.1)
  expect_gt(abs(unname(original$statistic) - unname(corrected$statistic)), 1e-4)
})

test_that("the correction vanishes when the residuals are balanced in sign", {
  # With as many positive as negative residuals m-hat is exactly zero.
  balanced <- NULL
  for (seed in 1:200) {
    obj <- glejser_data(n = 60, seed = seed, errors = "normal")
    if (sum(sign(residuals(obj$model))) == 0) {
      balanced <- obj
      break
    }
  }
  skip_if(is.null(balanced), "no balanced sample among the seeds tried")

  original <- quiet(performGlejserTest(balanced$model, balanced$data, "x1", robust = FALSE))
  corrected <- quiet(performGlejserTest(balanced$model, balanced$data, "x1", robust = TRUE))
  expect_equal(unname(corrected$statistic), unname(original$statistic), tolerance = 1e-12)
  expect_equal(corrected$p.value, original$p.value, tolerance = 1e-12)
})

test_that("the statistic does not depend on the direction of the skew", {
  obj <- glejser_data(errors = "exponential")
  mirrored <- obj$data
  mirrored$y <- -mirrored$y
  mirrored_model <- lm(y ~ x1 + x2, data = mirrored)

  for (transformation in c("abs", "inverse")) {
    right <- quiet(performGlejserTest(obj$model, obj$data, "x1", transformation, robust = TRUE))
    left <- quiet(performGlejserTest(mirrored_model, mirrored, "x1", transformation, robust = TRUE))
    expect_equal(unname(left$statistic), unname(right$statistic), tolerance = 1e-10)
  }
})

test_that("a weighted fit is corrected on its Pearson residuals", {
  obj <- glejser_data()
  weighted <- lm(y ~ x1 + x2, data = obj$data, weights = w)
  e <- sqrt(obj$data$w) * residuals(weighted)
  reference <- summary(lm(I(abs(e) - mean(sign(e)) * e) ~ obj$data$x1))$coefficients[2, ]

  result <- quiet(performGlejserTest(weighted, obj$data, "x1", robust = TRUE))
  expect_equal(unname(result$statistic), reference[["t value"]], tolerance = 1e-8)
  expect_equal(result$p.value, reference[["Pr(>|t|)"]], tolerance = 1e-8)
})

test_that("a residual that is zero up to rounding counts as zero", {
  # Half of the observations are fitted exactly, so their residuals are of the
  # order of 1e-15 and carry the sign of a rounding error. The statistic must
  # not depend on it: the same fit written in five ways gives the same value,
  # and that value is the one obtained with those residuals set to zero.
  x <- rep(1:6, each = 4)
  d <- data.frame(
    x = x,
    y = 3 * x + rep(c(1, 2, 2, 3), times = 6),
    z = rep(c(1, 4, 2, 6, 3, 5, 7, 2), times = 3)
  )
  d$g <- factor(d$x)
  d$x_scaled <- 0.1 * d$x
  d$x_centred <- d$x - mean(d$x)
  d$y_tripled <- 3 * d$y
  fits <- list(
    lm(y ~ x, data = d),
    lm(y ~ x_scaled, data = d),
    lm(y ~ x_centred, data = d),
    lm(y_tripled ~ x, data = d),
    lm(y ~ g, data = d)
  )

  e <- residuals(fits[[1]])
  expect_gt(sum(abs(e) < 1e-10), 6)
  e[abs(e) < 1e-10] <- 0
  reference <- summary(lm(I(abs(e) - mean(sign(e)) * e) ~ d$z))$coefficients[2, "t value"]

  for (fit in fits) {
    result <- quiet(performGlejserTest(fit, d, "z", robust = TRUE))
    expect_equal(unname(result$statistic), reference, tolerance = 1e-8)
  }
})

test_that("`robust` must be a single logical value", {
  obj <- glejser_data()
  for (bad in list("yes", NA, c(TRUE, FALSE), 1, NULL)) {
    expect_error(
      performGlejserTest(obj$model, obj$data, "x1", robust = bad),
      "`robust` must be a single logical value"
    )
  }
})

# --- simulated guards -----------------------------------------------------------

test_that("under skewed errors only the corrected statistic holds its level", {
  skip_on_cran()
  # Centred lognormal errors, constant variance. At 5000 replications the
  # default statistic rejects about 17% of the time and the corrected one about
  # 5%; see inst/validation/glejser-skewed-errors.csv. 400 replications put the
  # Monte Carlo standard error at 1.1% for the second and 1.9% for the first.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  reps <- 400

  p <- vapply(seq_len(reps), function(i) {
    obj <- glejser_data(n = 200, seed = 3000 + i, errors = "lognormal")
    c(
      original = quiet(performGlejserTest(obj$model, obj$data, "x1", robust = FALSE))$p.value,
      corrected = quiet(performGlejserTest(obj$model, obj$data, "x1", robust = TRUE))$p.value
    )
  }, numeric(2))

  expect_gt(mean(p["original", ] < 0.05), 0.11)
  expect_lt(mean(p["corrected", ] < 0.05), 0.085)
  expect_gt(mean(p["corrected", ] < 0.05), 0.02)
})

test_that("under normal errors the two statistics reach the same decision", {
  skip_on_cran()
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  reps <- 300

  p <- vapply(seq_len(reps), function(i) {
    obj <- glejser_data(n = 200, seed = 4000 + i, errors = "normal")
    c(
      original = quiet(performGlejserTest(obj$model, obj$data, "x1", robust = FALSE))$p.value,
      corrected = quiet(performGlejserTest(obj$model, obj$data, "x1", robust = TRUE))$p.value
    )
  }, numeric(2))

  expect_lt(mean(p["corrected", ] < 0.05), 0.09)
  expect_gt(mean((p["original", ] < 0.05) == (p["corrected", ] < 0.05)), 0.98)
})
