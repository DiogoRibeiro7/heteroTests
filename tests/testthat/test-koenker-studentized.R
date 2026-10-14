# performStudentizedBPTest() is deprecated in 0.14.0 in favour of
# performKoenkerTest(), on the evidence that the two compute the same
# statistic. The first test asserts that to 1e-8 over the designs of the
# heteroscedasticity family in inst/validation/full-sweep-size-power.R (a
# Gaussian null, sd = x^2 and a t5 null, 150 observations), a factor regressor
# and a weighted fit. On a fitWLS() fit they differ: performStudentizedBPTest()
# reads the weights left in the model frame, which the roadmap lists under
# 0.16.0, so only performKoenkerTest() is asserted there, against lm() on the
# estimated weights. Asserting the stale value would freeze the fault.

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

test_that("both refuse a rank-deficient fit with the same message", {
  set.seed(20261015)
  d <- data.frame(x = runif(150, 1, 5), z = runif(150, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x * rnorm(150)
  d$z2 <- 2 * d$z
  m <- lm(y ~ x + z + z2, d)
  expect_error(without_deprecation(performStudentizedBPTest(m, d)), "coefficients must be finite")
  expect_error(performKoenkerTest(m, d), "coefficients must be finite")
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
