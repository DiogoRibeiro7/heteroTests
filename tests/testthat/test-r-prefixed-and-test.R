# 0.14.0 renames rbootstrap_test_statistic() to bootstrapTestStatistic(),
# deprecates three other r-prefixed helpers that leave the exports, and
# deprecates the test() generic, whose name masks devtools::test(). Each old
# name keeps working, warns and returns what it returned.

test_that("rbootstrap_test_statistic() warns and returns what bootstrapTestStatistic() returns", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  set.seed(1)
  expect_warning(
    old <- rbootstrap_test_statistic(performKoenkerTest, m, mtcars, B = 19, progress = FALSE),
    "'rbootstrap_test_statistic' is deprecated.*Use 'bootstrapTestStatistic' instead",
    class = "deprecatedWarning"
  )
  set.seed(1)
  new <- bootstrapTestStatistic(performKoenkerTest, m, mtcars, B = 19, progress = FALSE)
  expect_identical_numbers(old[names(old) != "call"], new[names(new) != "call"])
  # Each name records the call it was given, as before.
  expect_identical(as.character(old$call[[1L]]), "rbootstrap_test_statistic")
  expect_identical(as.character(new$call[[1L]]), "bootstrapTestStatistic")
})

test_that("the r-prefixed helpers that leave the exports warn and keep their values", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  base <- performKoenkerTest(m, mtcars)
  msg <- "is deprecated: it is an internal helper of heteroTests"
  expect_warning(e <- rcalculateEffectSize(base, m, mtcars), msg, class = "deprecatedWarning")
  expect_identical_numbers(e, .calculate_effect_size(base, m, mtcars))
  expect_warning(p <- restimate_test_power(base, m, mtcars), msg, class = "deprecatedWarning")
  expect_identical_numbers(p, .estimate_test_power(base, m, mtcars))
  skip_if_not_installed("lmtest")
  expect_warning(
    v <- rvalidate_against_reference("breusch_pagan", m, mtcars),
    msg,
    class = "deprecatedWarning"
  )
  expect_identical_numbers(v, .validate_against_reference("breusch_pagan", m, mtcars))
})

test_that("test() warns, names runDiagnostics() and returns what it returns", {
  d <- HeteroDiagnostic(lm(mpg ~ wt + qsec, data = mtcars), mtcars)
  expect_warning(
    old <- test(d, tests = c("white", "koenker")),
    "'test\\(\\)' is deprecated.*runDiagnostics\\(x\\$model, x\\$data, tests\\)",
    class = "deprecatedWarning"
  )
  new <- runDiagnostics(d$model, d$data, tests = c("white", "koenker"), use_cache = FALSE)
  expect_equal(old$hetero$white$statistic, new$hetero$white$statistic, tolerance = 1e-12)
  expect_equal(old$hetero$koenker$p.value, new$hetero$koenker$p.value, tolerance = 1e-12)
})

test_that("summary() of a HeteroDiagnostic object does not warn", {
  d <- HeteroDiagnostic(lm(mpg ~ wt + qsec, data = mtcars), mtcars)
  count <- 0L
  withCallingHandlers(
    summary(d),
    deprecatedWarning = function(w) {
      count <<- count + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(count, 0L)
})
