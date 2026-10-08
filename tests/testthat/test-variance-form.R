library(testthat)

# ---------------------------------------------------------------------------
# Variance functions: the forms fitWLS() can fit, and performVarianceFormTest(),
# which tests whether a fitted form is adequate.
#
# The reference checks reconstruct each statistic from its definition with
# lm() and anova(), so they are auditable without the package. The simulated
# guards at the end check the two properties the test exists for: it holds its
# level when the form is right, and it rejects when the form is wrong.
# ---------------------------------------------------------------------------

make_power_data <- function(seed = 2026, n = 300) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  # Standard deviation proportional to x: variance x^2, a power function.
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x * rnorm(n)
  list(data = d, model = lm(y ~ x + z, data = d))
}

# --- fitWLS(): the variance functions ---------------------------------------

test_that("fitWLS defaults are the log-variance fit on the model's own design", {
  # The default must not move: it is what fitWLS() has computed since 0.9.0.
  obj <- make_power_data()
  m <- obj$model
  expected <- heteroTests:::rfgls_weights(m$residuals, model.matrix(m))

  expect_identical(fitWLS(m)$weights, expected)
  expect_identical(fitWLS(m, form = "exponential")$weights, expected)
})

test_that("the exponential form with var_formula matches its definition", {
  obj <- make_power_data()
  d <- obj$data
  aux <- lm(log(residuals(obj$model)^2) ~ x, data = d)
  g <- fitted(aux)
  expected <- 1 / exp(g - mean(g))

  fit <- fitWLS(obj$model, var_formula = ~x)
  expect_equal(unname(fit$weights), unname(expected), tolerance = 1e-10)
  expect_equal(
    unname(attr(fit, "variance_function")$coefficients),
    unname(coef(aux)),
    tolerance = 1e-10
  )
})

test_that("the power form is the exponential form in logarithms", {
  obj <- make_power_data()
  d <- obj$data
  d$log_x <- log(d$x)

  power <- fitWLS(obj$model, var_formula = ~x, form = "power")
  in_logs <- fitWLS(obj$model, data = d, var_formula = ~log_x)
  expect_equal(unname(power$weights), unname(in_logs$weights), tolerance = 1e-12)
  expect_identical(
    names(attr(power, "variance_function")$coefficients),
    c("(Intercept)", "log(x)")
  )
})

test_that("the power form recovers the exponent of the variance", {
  # Variance x^2, so the coefficient on log(x) estimates 2. Its standard error
  # at n = 2000 is about 0.11, so the band is more than four of them wide.
  obj <- make_power_data(seed = 5, n = 2000)
  fit <- fitWLS(obj$model, var_formula = ~x, form = "power")
  exponent <- attr(fit, "variance_function")$coefficients[["log(x)"]]

  expect_gt(exponent, 1.5)
  expect_lt(exponent, 2.5)
})

test_that("the power form refuses regressors that are not positive", {
  set.seed(10)
  d <- data.frame(x = rnorm(80))
  d$y <- 1 + d$x + rnorm(80)
  m <- lm(y ~ x, data = d)

  expect_error(fitWLS(m, form = "power"), "strictly positive")
})

test_that("the additive form is not offered", {
  # It was implemented and withdrawn before release: under its own null it
  # rejected 6.1% of the time at the 5% level, outside the release gate, and
  # its fitted variances were not all positive in 6% to 17% of samples.
  obj <- make_power_data()
  expect_error(fitWLS(obj$model, form = "linear"), "should be one of")
  expect_error(performVarianceFormTest(obj$model, form = "linear"), "should be one of")
})

test_that("var_formula can name a variable outside the model through data", {
  obj <- make_power_data()
  d <- obj$data
  set.seed(4)
  d$extra <- runif(nrow(d), 1, 2)

  expect_error(fitWLS(obj$model, var_formula = ~extra), "supplied through `data`")

  fit <- fitWLS(obj$model, data = d, var_formula = ~extra)
  g <- fitted(lm(log(residuals(obj$model)^2) ~ extra, data = d))
  expect_equal(unname(fit$weights), unname(1 / exp(g - mean(g))), tolerance = 1e-10)

  # Rows are matched by name, so the order of `data` does not matter.
  shuffled <- d[sample(nrow(d)), ]
  expect_equal(
    fitWLS(obj$model, data = shuffled, var_formula = ~extra)$weights,
    fit$weights,
    tolerance = 1e-12
  )
})

test_that("var_formula is validated", {
  obj <- make_power_data()
  expect_error(fitWLS(obj$model, var_formula = y ~ x), "one-sided formula")
  expect_error(fitWLS(obj$model, var_formula = "x"), "one-sided formula")
  expect_error(fitWLS(obj$model, var_formula = ~1), "at least one variable")
  expect_error(fitWLS(obj$model, data = obj$data[1:10, ], var_formula = ~x), "rows the model was fitted to")
})

test_that("the variance function carries its own constant", {
  # A mean model through the origin has no constant column to lend, and
  # log sigma^2 = gamma x without one would pin the variance at 1 where x = 0.
  obj <- make_power_data()
  d <- obj$data
  m0 <- lm(y ~ 0 + x + z, data = d)
  g <- fitted(lm(log(residuals(m0)^2) ~ x + z, data = d))

  expect_equal(unname(fitWLS(m0)$weights), unname(1 / exp(g - mean(g))), tolerance = 1e-10)
})

test_that("fitWLS records what it fitted", {
  obj <- make_power_data()
  fit <- fitWLS(obj$model, var_formula = ~x, form = "power")
  spec <- attr(fit, "variance_function")

  expect_identical(spec$form, "power")
  expect_identical(colnames(spec$regressors), "log(x)")
  expect_true(spec$usable)
  expect_null(spec$reason)
  expect_equal(attr(fit, "variance_model"), 1 / fit$weights)
})

# --- performVarianceFormTest(): against its definition -----------------------

# The statistic rebuilt from scratch: fit the variance function, refit by WLS,
# form the squared standardized residuals, and compare two nested regressions.
reconstruct <- function(m, d, form, added) {
  lhs <- log(residuals(m)^2)
  aux <- if (form == "power") lm(lhs ~ log(x), data = d) else lm(lhs ~ x, data = d)
  d$w <- 1 / exp(fitted(aux) - mean(fitted(aux)))
  wls <- lm(formula(m), data = d, weights = w)
  d$r <- d$w * residuals(wls)^2
  d$g <- if (form == "power") log(d$x) else d$x
  d$a <- added(d)
  anova(lm(r ~ g, data = d), lm(r ~ g + a, data = d))
}

test_that("the statistic matches a direct reconstruction for each form", {
  set.seed(8)
  n <- 400
  d <- data.frame(x = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + sqrt(0.5 + d$x) * rnorm(n)
  m <- lm(y ~ x, data = d)
  squares <- list(
    exponential = function(d) d$x^2,
    power = function(d) log(d$x)^2
  )

  for (form in names(squares)) {
    ref <- reconstruct(m, d, form, squares[[form]])
    ours <- performVarianceFormTest(m, form = form)

    expect_equal(unname(ours$statistic), ref$F[2], tolerance = 1e-8, label = form)
    expect_equal(unname(ours$parameter), c(1, ref$Res.Df[2]), label = form)
    expect_equal(ours$p.value, ref$`Pr(>F)`[2], tolerance = 1e-8, label = form)
  }
})

test_that("a formula in `against` names the added terms", {
  set.seed(8)
  n <- 400
  d <- data.frame(x = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + sqrt(0.5 + d$x) * rnorm(n)
  m <- lm(y ~ x, data = d)

  ref <- reconstruct(m, d, "exponential", function(d) log(d$x))
  ours <- performVarianceFormTest(m, against = ~ log(x))
  expect_equal(unname(ours$statistic), ref$F[2], tolerance = 1e-8)
  expect_identical(ours$variance_function$added_terms, "log(x)")

  # A variable the variance function leaves out, supplied through `data`.
  set.seed(1)
  d$other <- runif(n)
  ref_other <- reconstruct(m, d, "exponential", function(d) d$other)
  ours_other <- performVarianceFormTest(m, data = d, against = ~other)
  expect_equal(unname(ours_other$statistic), ref_other$F[2], tolerance = 1e-8)
})

test_that("testing a fitWLS() fit is testing the form it was fitted with", {
  obj <- make_power_data()
  for (form in c("exponential", "power")) {
    direct <- performVarianceFormTest(obj$model, var_formula = ~x, form = form)
    through <- performVarianceFormTest(fitWLS(obj$model, var_formula = ~x, form = form))
    expect_equal(through$statistic, direct$statistic, tolerance = 1e-12)
    expect_equal(through$p.value, direct$p.value, tolerance = 1e-12)
    expect_identical(through$method, direct$method)
  }
})

test_that("the result is an htest that names what was tested", {
  obj <- make_power_data()
  res <- performVarianceFormTest(obj$model, form = "power")

  expect_s3_class(res, "htest")
  expect_named(res$statistic, "F")
  expect_named(res$parameter, c("df1", "df2"))
  # Two variance regressors: two squares and one product.
  expect_equal(unname(res$parameter[["df1"]]), 3)
  expect_named(res$estimate, c("log(x)", "log(z)"))
  expect_match(res$method, "power form")
  expect_identical(res$alternative, "the variance function is misspecified")
  expect_identical(
    res$variance_function$added_terms,
    c("log(x)^2", "log(x):log(z)", "log(z)^2")
  )
  expect_output(print(res), "Specification test of the variance function")
})

test_that("the statistic does not depend on the scale of the response", {
  obj <- make_power_data()
  d <- obj$data
  d$y <- 1000 * d$y
  rescaled <- lm(y ~ x + z, data = d)

  expect_equal(
    unname(performVarianceFormTest(rescaled)$statistic),
    unname(performVarianceFormTest(obj$model)$statistic),
    tolerance = 1e-8
  )
})

test_that("added terms the variance function already spans are not counted", {
  # The square of a dummy variable is the dummy itself, so with one continuous
  # regressor and one dummy the default adds x^2 and x:dummy, not three terms.
  obj <- make_power_data()
  d <- obj$data
  d$high <- as.numeric(d$z > 3)
  m <- lm(y ~ x + high, data = d)
  res <- performVarianceFormTest(m)

  expect_equal(unname(res$parameter[["df1"]]), 2)
  expect_identical(res$variance_function$added_terms, c("x^2", "x:high"))

  expect_error(
    performVarianceFormTest(m, var_formula = ~high),
    "collinear with the variance function"
  )
})

test_that("inputs are validated", {
  obj <- make_power_data()
  d <- obj$data

  expect_error(performVarianceFormTest(obj$model, against = "cubes"), "one-sided formula")
  expect_error(performVarianceFormTest(obj$model, against = 3), "one-sided formula")
  expect_error(
    performVarianceFormTest(fitWLS(obj$model), form = "power"),
    "carries the variance function to test"
  )
  expect_error(
    performVarianceFormTest(fitWLS(obj$model), var_formula = ~x),
    "carries the variance function to test"
  )
  expect_error(
    performVarianceFormTest(lm(y ~ x + z, data = d, weights = 1 / x^2)),
    "did not come from fitWLS"
  )
  expect_error(
    performVarianceFormTest(glm(y ~ x + z, data = d)),
    "must be a fitted `lm` object"
  )
  expect_error(performVarianceFormTest(lm(y ~ x, data = d[1:20, ])))
})

test_that("the test is available through runHeteroTests", {
  obj <- make_power_data()
  suite <- suppressWarnings(
    runHeteroTests(obj$model, obj$data, tests = "variance_form", use_cache = FALSE)
  )
  expect_equal(
    unname(suite$variance_form$statistic),
    unname(performVarianceFormTest(obj$model)$statistic),
    tolerance = 1e-12
  )
})

# --- simulated guards ----------------------------------------------------------

simulate_rejection <- function(reps, sd_fun, form, seed) {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(seed)
  p <- vapply(seq_len(reps), function(i) {
    n <- 200
    x <- runif(n, 1, 5)
    z <- runif(n, 1, 5)
    d <- data.frame(x = x, z = z, y = 1 + 2 * x + 0.5 * z + sd_fun(x) * rnorm(n))
    suppressWarnings(performVarianceFormTest(lm(y ~ x + z, data = d), form = form)$p.value)
  }, numeric(1))
  mean(p < 0.05)
}

test_that("the test holds its level when the form is right", {
  skip_on_cran()
  # The null is heteroscedastic here, which is the point: a test of constant
  # variance rejects these data every time. 300 replications put the Monte
  # Carlo standard error at 1.3%.
  expect_lt(simulate_rejection(300, function(x) exp(0.3 * x), "exponential", 101), 0.10)
  expect_lt(simulate_rejection(300, function(x) x^1.5, "power", 102), 0.10)
  # Constant variance is a special case of every form.
  expect_lt(simulate_rejection(300, function(x) rep(1, length(x)), "exponential", 103), 0.10)
})

test_that("the test rejects a variance function of the wrong shape", {
  skip_on_cran()
  # Log-variance quadratic in x, which neither log-linear form can follow.
  u_shape <- function(x) exp(0.25 * (x - 3)^2)
  expect_gt(simulate_rejection(100, u_shape, "exponential", 201), 0.9)
  expect_gt(simulate_rejection(100, u_shape, "power", 202), 0.9)
})

test_that("a constant-variance test on a fitWLS() fit cannot stand in for it", {
  skip_on_cran()
  # The reason the test exists. With variance x^3 the exponential form is
  # wrong. Koenker's test on the weighted fit asks about the variance
  # regressors themselves, which estimation has already absorbed, and rejects
  # little more often than it does when the form is right; the form test,
  # which looks outside them, rejects most of the time.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(301)
  p <- vapply(seq_len(150), function(i) {
    n <- 400
    x <- runif(n, 1, 5)
    z <- runif(n, 1, 5)
    d <- data.frame(x = x, z = z, y = 1 + 2 * x + 0.5 * z + x^1.5 * rnorm(n))
    wls <- fitWLS(lm(y ~ x + z, data = d))
    suppressWarnings(c(
      koenker = performKoenkerTest(wls, d)$p.value,
      form = performVarianceFormTest(wls)$p.value
    ))
  }, numeric(2))
  rejection <- rowMeans(p < 0.05)

  expect_lt(rejection[["koenker"]], 0.25)
  expect_gt(rejection[["form"]], 0.5)
})
