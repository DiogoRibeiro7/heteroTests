# 0.15.0 gives every test the (model, data, ...) convention: `data = NULL` in
# second position, recovered from the model frame when omitted.
# performBoxMTest(data, group) is the documented exception. A call that still
# passes the old second argument by position works in 0.15.x, with a warning
# that shows the new call, and stops working in 1.0.0. No value changes: the
# old and the new form of each call return what the old form returned in
# 0.14.0.

convention_model <- function() {
  set.seed(20261015)
  n <- 120
  d <- data.frame(x1 = runif(n), x2 = rnorm(n), z = rnorm(n))
  d$y <- 1 + 2 * d$x1 - d$x2 + rnorm(n, sd = exp(0.8 * d$x1))
  list(d = d, m = lm(y ~ x1 + x2, data = d))
}

convention_tests <- c(
  "performArchLMTest", "performMcLeodLiTest", "performCookWeisbergTest",
  "performNCVTest", "performSpearmanTest", "performSpreadLevelTest",
  "performDavidianCarrollTest", "performHarveyTest"
)

quiet_test <- function(expr) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  without_deprecation(expr)
}

test_that("the tests take data = NULL in second position", {
  for (fn in convention_tests) {
    f <- formals(getExportedValue("heteroTests", fn))
    expect_identical(names(f)[1:2], c("model", "data"), info = fn)
    expect_null(f$data, info = fn)
  }
  # The documented exception tests the data, not a model.
  expect_identical(names(formals(performBoxMTest)), c("data", "group"))
})

test_that("supplying the data does not change the result", {
  s <- convention_model()
  m <- s$m
  d <- s$d
  for (fn in convention_tests) {
    f <- getExportedValue("heteroTests", fn)
    expect_identical(quiet_test(f(m, d)), quiet_test(f(m)), info = fn)
    expect_identical(quiet_test(f(m, data = d)), quiet_test(f(m)), info = fn)
  }
  expect_identical(
    quiet_test(performCookWeisbergTest(m, d, var_formula = ~ x1 + x2)),
    quiet_test(performCookWeisbergTest(m, var_formula = ~ x1 + x2))
  )
})

test_that("a second argument that is not a data frame is refused", {
  m <- convention_model()$m
  for (fn in c("performSpearmanTest", "performSpreadLevelTest", "performHarveyTest")) {
    f <- getExportedValue("heteroTests", fn)
    expect_error(
      quiet_test(f(m, list(a = 1))), "`data` in .* must be a data frame or NULL",
      info = fn
    )
  }
  # A number is the old `lags`, but not when `lags` is given as well.
  expect_error(
    quiet_test(performArchLMTest(m, 3, lags = 2)),
    "`data` in performArchLMTest\\(\\) must be a data frame or NULL"
  )
})

test_that("old positional calls warn, show the new call and keep their value", {
  m <- convention_model()$m
  q <- function(expr) {
    old <- ht_set_log_level("SILENT")
    on.exit(ht_set_log_level(old), add = TRUE)
    expr
  }
  expect_warning(
    a <- q(performArchLMTest(m, 3)),
    "performArchLMTest\\(m, 3\\) is deprecated.*Use performArchLMTest\\(m, lags = 3\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performArchLMTest(m, lags = 3)))
  # The warning says when the old form goes.
  expect_warning(
    q(performArchLMTest(m, 3)), "stops working in 1\\.0\\.0",
    class = "deprecatedWarning"
  )

  expect_warning(
    a <- q(performMcLeodLiTest(m, 4)),
    "Use performMcLeodLiTest\\(m, lags = 4\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performMcLeodLiTest(m, lags = 4)))

  expect_warning(
    a <- q(performDavidianCarrollTest(m, 3)),
    "Use performDavidianCarrollTest\\(m, degree = 3\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performDavidianCarrollTest(m, degree = 3)))

  expect_warning(
    a <- q(performCookWeisbergTest(m, ~x1)),
    "Use performCookWeisbergTest\\(m, var_formula = ~x1\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performCookWeisbergTest(m, var_formula = ~x1)))

  expect_warning(
    a <- q(performHarveyTest(m, "fitted")),
    "Use performHarveyTest\\(m, auxiliary = \"fitted\"\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performHarveyTest(m, auxiliary = "fitted")))

  expect_warning(
    a <- q(performHarveyTest(m, "fitted", TRUE)),
    "Use performHarveyTest\\(m, auxiliary = \"fitted\", studentize = TRUE\\)",
    class = "deprecatedWarning"
  )
  expect_identical(a, q(performHarveyTest(m, auxiliary = "fitted", studentize = TRUE)))

  # performNCVTest() is deprecated as a whole as well, so it warns twice.
  n_warn <- 0L
  a <- withCallingHandlers(
    q(performNCVTest(m, ~x1)),
    deprecatedWarning = function(w) {
      n_warn <<- n_warn + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(n_warn, 2L)
  expect_identical(a, quiet_test(performNCVTest(m, var_formula = ~x1)))
})

test_that("var_formula is evaluated in data, rows matched by name", {
  skip_if_not_installed("car")
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  # hp is not in the model frame, so only `data` can supply it.
  ours <- quiet_test(performCookWeisbergTest(m, mtcars, var_formula = ~ wt + hp))
  ref <- car::ncvTest(m, var.formula = ~ wt + hp)
  expect_equal(unname(ours$statistic), ref$ChiSquare, tolerance = 1e-8)
  expect_equal(unname(ours$parameter), ref$Df)
  expect_equal(ours$p.value, ref$p, tolerance = 1e-8)

  shuffled <- mtcars[rev(seq_len(nrow(mtcars))), ]
  expect_identical(
    quiet_test(performCookWeisbergTest(m, shuffled, var_formula = ~ wt + hp)),
    ours
  )
  expect_error(
    quiet_test(performCookWeisbergTest(m, mtcars[1:20, ], var_formula = ~hp)),
    "contain the rows used to fit the model"
  )
})
