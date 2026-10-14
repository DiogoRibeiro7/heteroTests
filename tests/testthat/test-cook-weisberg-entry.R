# 0.14.0 makes performCookWeisbergTest() the one entry point for the
# Cook-Weisberg score test: it gains var_formula, and performNCVTest() is
# deprecated. No value changes: performNCVTest() and the "ncv" registry name
# keep their implementation, and performCookWeisbergTest() without var_formula
# returns what it returned.

test_that("performCookWeisbergTest(var_formula) returns what performNCVTest(var_formula) returned", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  for (f in list(~wt, ~ wt + qsec, ~ I(wt^2))) {
    old <- without_deprecation(performNCVTest(m, var_formula = f))
    expect_identical(performCookWeisbergTest(m, var_formula = f), old)
  }
})

test_that("performCookWeisbergTest() without var_formula keeps its value and labels", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  cw <- performCookWeisbergTest(m)
  ncv <- without_deprecation(performNCVTest(m))
  expect_identical(cw$statistic, ncv$statistic)
  expect_identical(cw$parameter, ncv$parameter)
  expect_identical(cw$p.value, ncv$p.value)
  expect_identical(cw$method, "Cook-Weisberg test for heteroscedasticity")
  expect_identical(cw$alternative, "error variance depends on the fitted values")
})

test_that("performNCVTest() warns, names performCookWeisbergTest() and keeps its value", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  expect_warning(
    old <- performNCVTest(m, var_formula = ~wt),
    "'performNCVTest' is deprecated.*Use 'performCookWeisbergTest' instead",
    class = "deprecatedWarning"
  )
  expect_identical(old, heteroTests:::.ncv_test(m, var_formula = ~wt))
})

test_that("the ncv registry name keeps its value and warns once per run", {
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  count <- 0L
  res <- withCallingHandlers(
    runHeteroTests(m, mtcars, tests = c("ncv", "cook_weisberg"), use_cache = FALSE),
    deprecatedWarning = function(w) {
      count <<- count + 1L
      expect_match(conditionMessage(w), "\"ncv\" is deprecated.*Use \"cook_weisberg\" instead")
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(count, 1L)
  expect_equal(
    unname(res$ncv$statistic),
    unname(heteroTests:::.ncv_test(m)$statistic),
    tolerance = 1e-12
  )
})
