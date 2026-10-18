# performStudentizedBPTest() is deprecated in 0.14.0 in favour of
# performKoenkerTest(), on the evidence that the two compute the same
# statistic. The first test asserts that to 1e-8 over the designs of the
# heteroscedasticity family in inst/validation/full-sweep-size-power.R (a
# Gaussian null, sd = x^2 and a t5 null, 150 observations), a factor regressor
# and a weighted fit. On a fitWLS() fit they differed before 0.15.0:
# performStudentizedBPTest() read the weights left in the model frame, those
# of the model fitWLS() refitted, and so computed the statistic of the
# unweighted fit. Since 0.15.0 it reads the weights of the fit, and the two
# agree on a fitWLS() fit too, which a test below asserts. On a model with an
# offset the studentized statistic refitted the design without it and gave
# another value; since 0.16.0 the refit carries the offset (the model
# context), and the two agree there too, which the designs with an offset
# below assert.

agreement_designs <- local({
  n <- 150L
  draw <- function(sd_fun, errors = "normal") {
    x <- runif(n, 1, 5)
    z <- runif(n, 1, 5)
    e <- if (errors == "normal") rnorm(n) else rt(n, df = 5) / sqrt(5 / 3)
    d <- data.frame(y = 1 + 2 * x + 0.5 * z + sd_fun(x) * e, x = x, z = z)
    d$g <- cut(d$x, 3, labels = c("a", "b", "c"))
    d
  }
  flat <- function(x) rep(1, length(x))
  list(
    gaussian_null = function() {
      d <- draw(flat)
      list(m = lm(y ~ x + z, d), d = d)
    },
    sd_x_squared = function() {
      d <- draw(function(x) x^2)
      list(m = lm(y ~ x + z, d), d = d)
    },
    t5_null = function() {
      d <- draw(flat, "t5")
      list(m = lm(y ~ x + z, d), d = d)
    },
    factor_regressor = function() {
      d <- draw(function(x) x)
      list(m = lm(y ~ x + g, d), d = d)
    },
    weighted = function() {
      d <- draw(function(x) x^2)
      d$w <- 1 / d$x^4
      list(m = lm(y ~ x + z, d, weights = w), d = d)
    },
    # An offset outside the span of the regressors, as an argument and as an
    # offset() term.
    offset_argument = function() {
      d <- draw(function(x) x)
      d$o <- 0.5 * d$x^2
      d$y <- d$y + d$o
      list(m = lm(y ~ x + z, d, offset = o), d = d)
    },
    offset_term = function() {
      d <- draw(function(x) x)
      d$o <- 0.5 * d$x^2
      d$y <- d$y + d$o
      d$w <- 1 / d$x^2
      list(m = lm(y ~ x + z + offset(o), d, weights = w), d = d)
    }
  )
})

test_that("performStudentizedBPTest() agrees with performKoenkerTest() to 1e-8", {
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (design in names(agreement_designs)) {
    for (i in seq_len(25L)) {
      set.seed(20261014L + i)
      o <- agreement_designs[[design]]()
      # The t5 design draws outliers, about which both functions warn.
      a <- suppressWarnings(performStudentizedBPTest(o$m, o$d))
      b <- suppressWarnings(performKoenkerTest(o$m, o$d))
      label <- sprintf("%s, seed %d", design, 20261014L + i)
      expect_lt(abs(unname(a$statistic) - unname(b$statistic)), 1e-8, label = label)
      expect_identical(unname(a$parameter), unname(b$parameter), label = label)
      expect_lt(abs(a$p.value - b$p.value), 1e-8, label = label)
    }
  }
})

# Before 0.16.0 both refused this rank-deficient fit with "Model coefficients
# must be finite". Since 0.16.0 both compute the statistic on the estimable
# part of the design, the statistic of lm(y ~ x + z) and of lmtest::bptest(),
# which takes its degrees of freedom from the rank of the auxiliary fit.
test_that("both give a rank-deficient fit the statistic of lmtest::bptest()", {
  set.seed(20261015)
  d <- data.frame(x = runif(150, 1, 5), z = runif(150, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x * rnorm(150)
  d$z2 <- 2 * d$z
  m <- lm(y ~ x + z + z2, d)
  reduced <- performKoenkerTest(lm(y ~ x + z, d), d)
  for (r in list(without_deprecation(performStudentizedBPTest(m, d)), performKoenkerTest(m, d))) {
    expect_equal(unname(r$statistic), 20.8736185, tolerance = 1e-8)
    expect_identical(unname(r$parameter), 2L)
    expect_equal(unname(r$statistic), unname(reduced$statistic), tolerance = 1e-10)
    expect_equal(r$p.value, reduced$p.value, tolerance = 1e-10)
  }
  skip_if_not_installed("lmtest")
  bp <- lmtest::bptest(m)
  expect_equal(unname(performKoenkerTest(m, d)$statistic), unname(bp$statistic), tolerance = 1e-10)
  expect_equal(performKoenkerTest(m, d)$p.value, unname(bp$p.value), tolerance = 1e-10)
})

test_that("performKoenkerTest() reads the estimated weights of a fitWLS() fit", {
  set.seed(20261016)
  d <- data.frame(x = runif(150, 1, 5), z = runif(150, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x^2 * rnorm(150)
  wls <- fitWLS(lm(y ~ x + z, d), d)
  d$estimated <- weights(wls)
  reference <- lm(y ~ x + z, d, weights = estimated)
  a <- performKoenkerTest(wls, d)
  b <- performKoenkerTest(reference, d)
  expect_lt(abs(unname(a$statistic) - unname(b$statistic)), 1e-8)
  expect_lt(abs(a$p.value - b$p.value), 1e-8)
})

# A correctly specified fitWLS() design, sd = exp(0.3 x), with input weights
# for the weighted lm and for fitWLS() of a weighted model.
fitwls_design <- function() {
  set.seed(20261017)
  n <- 150
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + exp(0.3 * d$x) * rnorm(n)
  d$w <- 1 / d$x
  d
}

test_that("on a fitWLS() fit the studentized statistic reads the weights of the fit", {
  # Before 0.15.0 it read the weights of the model frame, that of the model
  # fitWLS() refitted, and computed the statistic of the unweighted fit.
  d <- fitwls_design()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (base in list(lm(y ~ x + z, d), lm(y ~ x + z, d, weights = w))) {
    wls <- fitWLS(base)
    koenker <- performKoenkerTest(wls, d)
    internal <- heteroTests:::.studentized_bp_test(wls, d)
    exported <- without_deprecation(performStudentizedBPTest(wls, d))
    robust <- performBPTestRobust(wls, d)
    for (r in list(internal, exported, robust)) {
      expect_lt(abs(unname(r$statistic) - unname(koenker$statistic)), 1e-8)
      expect_lt(abs(r$p.value - koenker$p.value), 1e-8)
    }
  }
})

test_that("the robust bootstrap of a correctly specified fitWLS() fit is no longer tiny", {
  d <- fitwls_design()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fit <- fitWLS(lm(y ~ x + z, d))
  set.seed(3)
  r <- suppressWarnings(performBPTestRobust(fit, d, bootstrap = TRUE, B = 99))
  # In 0.14.0 the statistic was 19.39, that of the unweighted fit, the
  # asymptotic p-value 6.2e-05 and the bootstrap one 0.01. Now the statistic
  # is Koenker's, 1.97.
  expect_lt(abs(unname(r$statistic) - unname(performKoenkerTest(fit, d)$statistic)), 1e-8)
  expect_gt(r$p.value, 0.05)
  expect_identical(r$p.value_bootstrap, 0.59)
})

test_that("the studentized statistic of an lm and a weighted lm is that of 0.14.0", {
  d <- fitwls_design()
  # Computed by 0.14.0's .studentized_bp_test().
  expect_equal(unname(heteroTests:::.studentized_bp_test(lm(y ~ x + z, d), d)$statistic),
               19.391293965792642, tolerance = 1e-12)
  expect_equal(unname(heteroTests:::.studentized_bp_test(lm(y ~ x + z, d, weights = w),
                                                         d)$statistic),
               6.5727512464303572, tolerance = 1e-12)
})

test_that("performStudentizedBPTest() warns, names performKoenkerTest() and keeps its value", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(
    old <- performStudentizedBPTest(m, mtcars),
    "'performStudentizedBPTest' is deprecated.*Use 'performKoenkerTest' instead",
    class = "deprecatedWarning"
  )
  expect_identical(old, heteroTests:::.studentized_bp_test(m, mtcars, data_name = "m"))
  expect_identical(old$data.name, "m")
})

test_that("the student_bp registry name keeps its value and warns once per run", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  count <- 0L
  res <- withCallingHandlers(
    runHeteroTests(m, mtcars, tests = c("student_bp", "koenker"), use_cache = FALSE),
    deprecatedWarning = function(w) {
      count <<- count + 1L
      expect_match(conditionMessage(w), "\"student_bp\" is deprecated.*Use \"koenker\" instead")
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(count, 1L)
  expect_equal(
    unname(res$student_bp$statistic),
    unname(heteroTests:::.studentized_bp_test(m, mtcars)$statistic),
    tolerance = 1e-12
  )
})

test_that("grouped runs with student_bp warn once, not once per group", {
  skip_if_not_installed("dplyr")
  d <- mtcars
  d$grp <- rep(c("a", "b"), length.out = nrow(d))
  d <- rbind(d, d, d)
  grouped <- dplyr::group_by(d, grp)
  count <- 0L
  withCallingHandlers(
    runHeteroTests(mpg ~ wt + qsec, grouped, tests = "student_bp", use_cache = FALSE),
    deprecatedWarning = function(w) {
      count <<- count + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(count, 1L)
})
