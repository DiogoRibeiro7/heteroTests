# The tests match the rows of `data` to the residuals by row name. A tibble, a
# data.table, or a data frame whose row names were reset (dplyr::arrange()
# resets them) has row names that only number its rows, so before 0.15.0 such
# data in another order than the fit, or with rows in front of those of the
# fit, were matched by position and gave wrong values without an error. Since
# 0.15.0 a test that reads `data` first takes the rows the row names pick for
# the observations of the fit and compares the response and the variables the
# model frame holds as themselves with the model frame (rcheck_data_rows()),
# and refuses data that do not reproduce it. Valid calls are unchanged: data
# with row names of its own, in any order, and the very tibble or data.table
# the model was fitted on, give what they gave.

# The tests that read `data`, all through rdata_or_model_frame().
guarded_tests <- c(
  "performWhiteTest", "performWhiteTestStreaming", "performWhiteTestRobust",
  "performBPTest", "performBPTestStreaming", "performBreuschPaganTest",
  "performBPTestRobust", "performGQTest", "performKoenkerTest",
  "performKoenkerTestStreaming", "performParkTest", "performStudentizedBPTest",
  "performWhiteTestBootstrap", "performSzroeterTest", "performHarrisonMcCabeTest",
  "performWildBootstrapTest", "performQuantileRegressionTest",
  "performRankPermutationTest", "performHighDimensionalTest",
  "performSpatialHeteroTest", "performLeveneTest", "performGlejserTest",
  "performBrownForsytheTest", "performFlignerKilleenTest", "performBartlettTest",
  "performHartleyFmaxTest", "performOBrienTest", "performPesaranTest",
  "performBPRandomEffectsTest"
)
# These read `data` by position, the i-th row with the i-th residual, so they
# also refuse data with its own row names in another order than the fit.
positional_tests <- c(
  "performPesaranTest", "performBPRandomEffectsTest", "performScatterDiagnostic"
)

guard_recipes <- function() {
  recipes <- contract_recipes[setdiff(names(contract_recipes), "performBoxMTest")]
  recipes$performScatterDiagnostic <- function(m, d) performScatterDiagnostic(m, d, c("x1", "x2"))
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    recipes$performQuantileRegressionTest <- NULL
  }
  if (!requireNamespace("spdep", quietly = TRUE)) {
    recipes$performSpatialHeteroTest <- NULL
  }
  recipes
}

guard_data <- function() {
  d <- contract_data()$d
  set.seed(99)
  d$w <- 1 / d$x1^2
  d$o <- 0.3 * d$x2 + 0.5 * rnorm(nrow(d))
  d$ypos <- exp(0.5 + 0.4 * log(d$x1) + 0.2 * d$x2 + rnorm(nrow(d), sd = 0.3 * d$x1))
  d
}

local_silent_log <- function(frame = parent.frame()) {
  old <- ht_set_log_level("SILENT")
  withr::defer(ht_set_log_level(old), envir = frame)
}

guard_run <- function(fn, m, data, recipes = guard_recipes()) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(7)
  suppressMessages(suppressWarnings(recipes[[fn]](m, data)))
}

expect_same_result <- function(object, expected, info) {
  if (is.list(expected) && !is.null(expected$statistic)) {
    expect_identical(unname(object$statistic), unname(expected$statistic), info = info)
    expect_identical(object$p.value, expected$p.value, info = info)
    expect_identical(unname(object$parameter), unname(expected$parameter), info = info)
  } else {
    expect_identical(unname(object), unname(expected), info = info)
  }
}

expect_refused <- function(fn, m, data, info, recipes = guard_recipes()) {
  expect_error(
    guard_run(fn, m, data, recipes),
    "cannot match the rows of `data` to the observations of the model",
    class = "heteroTests_data_rows_error",
    info = info
  )
}

test_that("a data frame whose row names were reset, in another order, is refused", {
  local_silent_log()
  set.seed(1)
  n <- 120
  d <- data.frame(x = runif(n, 1, 5), z = rnorm(n))
  d$y <- 1 + d$x + d$z + rnorm(n, sd = d$x)
  m <- lm(y ~ x + z, d)
  expect_equal(unname(performKoenkerTest(m, d)$statistic), 17.56, tolerance = 1e-3)
  arranged <- d[order(d$x), ]
  rownames(arranged) <- NULL
  expect_error(
    performKoenkerTest(m, arranged),
    paste0(
      "performKoenkerTest() cannot match the rows of `data` to the observations ",
      "of the model: `data` has no row names of its own"
    ),
    fixed = TRUE
  )
  expect_error(performKoenkerTest(m, arranged), "y, x, z differ from the model frame", fixed = TRUE)
  expect_error(
    performKoenkerTest(m, arranged),
    "Pass the data the model was fitted on, in its order, or `data = NULL` to use the model frame.",
    fixed = TRUE
  )
  # The same rows with their own row names are matched by name, as before.
  expect_identical(
    performKoenkerTest(m, d[order(d$x), ])$statistic,
    performKoenkerTest(m, d)$statistic
  )
})

test_that("reordered tibbles, data.tables and dplyr::arrange() output are refused", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("data.table")
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  set.seed(3)
  perm <- sample(nrow(d))
  forms <- list(
    tibble = dplyr::as_tibble(d)[perm, ],
    data.table = data.table::as.data.table(d[perm, ]),
    arranged = dplyr::arrange(d, x1)
  )
  recipes <- guard_recipes()
  for (fn in intersect(c(guarded_tests, positional_tests), names(recipes))) {
    for (form in names(forms)) {
      expect_refused(fn, m, forms[[form]], info = paste(fn, form), recipes = recipes)
    }
  }
})

test_that("the tests that do not read data still return the value of the fit", {
  skip_if_not_installed("dplyr")
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  arranged <- dplyr::arrange(d, x1)
  recipes <- guard_recipes()
  for (fn in setdiff(names(recipes), c(guarded_tests, positional_tests))) {
    expect_same_result(guard_run(fn, m, arranged, recipes), guard_run(fn, m, d, recipes), info = fn)
  }
})

test_that("rows in front of those of the fit are refused, rows after them are not", {
  skip_on_cran()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  front <- rbind(d[1:5, ], d)
  end <- rbind(d, d[1:5, ])
  expect_lt(.row_names_info(front), 0L)
  recipes <- guard_recipes()
  for (fn in intersect(c(guarded_tests, positional_tests), names(recipes))) {
    expect_refused(fn, m, front, info = fn, recipes = recipes)
  }
  # Rows after those of the fit leave the rows it reads where they were: the
  # tests that match by name give the value on the data of the fit.
  for (fn in setdiff(names(recipes), positional_tests)) {
    expect_same_result(guard_run(fn, m, end, recipes), guard_run(fn, m, d, recipes), info = fn)
  }
  # The tests that read by position refuse another number of rows, with the
  # message on the number of rows the random-effects test already gave.
  for (fn in positional_tests) {
    expect_error(guard_run(fn, m, end, recipes), "requires `data` with 120 observations", info = fn)
  }
})

test_that("repeated row names are refused", {
  local_silent_log()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  repeated <- d
  attr(repeated, "row.names") <- c(1L, seq_len(nrow(d) - 1L))
  expect_true(anyDuplicated(rownames(repeated)) > 0L)
  expect_error(
    performKoenkerTest(m, repeated),
    "the row names of `data` are not unique",
    class = "heteroTests_data_rows_error"
  )
  expect_error(performBartlettTest(m, repeated, "group"), "are not unique")
  expect_error(performScatterDiagnostic(m, repeated, "x1"), "are not unique")
})

test_that("data with its own row names, shuffled, give the values they gave", {
  skip_on_cran()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  set.seed(5)
  shuffled <- d[sample(nrow(d)), ]
  recipes <- guard_recipes()
  for (fn in setdiff(names(recipes), positional_tests)) {
    expect_same_result(guard_run(fn, m, shuffled, recipes), guard_run(fn, m, d, recipes), info = fn)
  }
  # The panel tests and the scatter diagnostic read such data against other
  # residuals, which gave another value: they refuse it.
  for (fn in positional_tests) {
    expect_error(
      guard_run(fn, m, shuffled, recipes),
      "it reads `data` by position, the i-th row with the i-th observation",
      class = "heteroTests_data_rows_error",
      info = fn
    )
  }
})

test_that("the tibble or data.table the model was fitted on is accepted", {
  skip_on_cran()
  skip_if_not_installed("dplyr")
  skip_if_not_installed("data.table")
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  recipes <- guard_recipes()
  tb <- dplyr::as_tibble(d)
  dt <- data.table::as.data.table(d)
  m_tb <- lm(y ~ x1 + x2, data = tb)
  m_dt <- lm(y ~ x1 + x2, data = dt)
  for (fn in names(recipes)) {
    expected <- guard_run(fn, m, d, recipes)
    expect_same_result(guard_run(fn, m_tb, tb, recipes), expected, info = paste(fn, "tibble"))
    expect_same_result(guard_run(fn, m_dt, dt, recipes), expected, info = paste(fn, "data.table"))
  }
})

test_that("na.omit fits are matched by name with or without the rows they dropped", {
  skip_if_not_installed("dplyr")
  d <- guard_data()
  d$x2[c(3, 7, 11)] <- NA
  m <- lm(y ~ x1 + x2, data = d)
  complete <- d[stats::complete.cases(d), ]
  set.seed(6)
  shuffled <- d[sample(nrow(d)), ]
  tests <- c("performKoenkerTest", "performWhiteTest", "performGlejserTest", "performBartlettTest")
  for (fn in tests) {
    expected <- guard_run(fn, m, complete)
    expect_same_result(guard_run(fn, m, d, guard_recipes()), expected, info = fn)
    expect_same_result(guard_run(fn, m, shuffled, guard_recipes()), expected, info = fn)
    expect_refused(fn, m, dplyr::arrange(d, x1), info = fn)
  }
})

test_that("a transformed response is rebuilt from data through the terms of the fit", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  m <- lm(log(ypos) ~ x1 + x2, data = d)
  set.seed(8)
  shuffled <- d[sample(nrow(d)), ]
  for (fn in c("performKoenkerTest", "performWhiteTest", "performParkTest", "performLeveneTest")) {
    expect_same_result(guard_run(fn, m, shuffled), guard_run(fn, m, d), info = fn)
    expect_refused(fn, m, dplyr::arrange(d, x1), info = fn)
  }
  # Only the response differs: the rows of `data` hold another ypos.
  other <- d
  other$ypos <- rev(other$ypos)
  expect_error(performKoenkerTest(m, other), "log(ypos) differs from the model frame", fixed = TRUE)
  # Transformed regressors are not compared themselves, and a value computed
  # from all the rows, as I(x1 - mean(x1)) or poly(), is not a difference.
  for (f in list(y ~ log(x1) + x2, y ~ poly(x1, 2) + x2, y ~ I(x1 - mean(x1)) + I(x2^2))) {
    mf <- lm(f, data = d)
    expect_same_result(
      performKoenkerTest(mf, shuffled), performKoenkerTest(mf, d),
      info = deparse(f)
    )
  }
  # A response computed from all the rows is evaluated on all the rows of
  # `data`, as model.frame() evaluated it before dropping rows.
  d_na <- d
  d_na$x2[c(3, 7)] <- NA
  centred <- lm(I(y - mean(y)) ~ x1 + x2, data = d_na)
  expect_warning(performKoenkerTest(centred, d_na), "Removed 2 observations")
  expect_s3_class(suppressWarnings(performKoenkerTest(centred, d_na)), "htest")
})

test_that("weights and offsets given as columns are matched with their rows", {
  skip_if_not_installed("dplyr")
  d <- guard_data()
  set.seed(9)
  shuffled <- d[sample(nrow(d)), ]
  fits <- list(
    weights = lm(y ~ x1 + x2, data = d, weights = w),
    offset = lm(y ~ x1 + x2, data = d, offset = o),
    offset_term = lm(y ~ x1 + x2 + offset(o), data = d),
    subset = lm(y ~ x1 + x2, data = d, subset = x1 > 0.8),
    glm = glm(y ~ x1 + x2, family = gaussian(), data = d, weights = w)
  )
  for (nm in names(fits)) {
    m <- fits[[nm]]
    for (fn in c("performKoenkerTest", "performWhiteTest", "performGlejserTest")) {
      expect_same_result(guard_run(fn, m, shuffled), guard_run(fn, m, d), info = paste(nm, fn))
      expect_refused(fn, m, dplyr::arrange(d, x1), info = paste(nm, fn))
    }
  }
})

test_that("a tibble whose incomplete rows a test drops is not matched to other rows", {
  # The fit is to the first 100 rows, three of them incomplete; the tibble
  # holds all 120. Dropping the three renumbers what is left, so a match by
  # name after the drop pairs each residual with a later row: Koenker's
  # statistic was 0.0165 against 7.93 on the rows of the fit.
  skip_if_not_installed("dplyr")
  d <- guard_data()
  d$x2[c(3, 7, 11)] <- NA
  tb <- dplyr::as_tibble(d)
  m <- lm(y ~ x1 + x2, data = tb[1:100, ])
  for (fn in c("performKoenkerTest", "performWhiteTest", "performBPTest", "performGlejserTest")) {
    expect_error(
      guard_run(fn, m, tb),
      "numbers the rows that are left afresh",
      class = "heteroTests_data_rows_error",
      info = fn
    )
    # As a data frame its row numbers survive the drop.
    expect_same_result(
      guard_run(fn, m, as.data.frame(tb)),
      guard_run(fn, m, as.data.frame(tb[1:100, ])),
      info = fn
    )
  }
  # The group tests match before they drop rows, and were not affected.
  expect_same_result(
    guard_run("performBartlettTest", m, tb),
    guard_run("performBartlettTest", m, as.data.frame(tb[1:100, ])),
    info = "Bartlett"
  )
})

test_that("the variables of var_formula are read from rows the guard confirmed", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  set.seed(10)
  shuffled <- d[sample(nrow(d)), ]
  expect_identical(
    performCookWeisbergTest(m, shuffled, var_formula = ~ group)$statistic,
    performCookWeisbergTest(m, d, var_formula = ~ group)$statistic
  )
  expect_error(
    performCookWeisbergTest(m, dplyr::arrange(d, x1), var_formula = ~ group),
    "performCookWeisbergTest() cannot match the rows of `data`",
    fixed = TRUE
  )
  # Without var_formula the test reads nothing from `data`.
  expect_identical(
    performCookWeisbergTest(m, dplyr::arrange(d, x1))$statistic,
    performCookWeisbergTest(m, d)$statistic
  )
})

test_that("data without a row of the fit keep the message they had", {
  local_silent_log()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  expect_error(
    performKoenkerTest(m, d[1:100, ]),
    "requires `data` to contain the rows used to fit the model. Missing rows: 101, 102, 103",
    fixed = TRUE
  )
  # The model frame, given back, holds the rows of the fit by name.
  expect_same_result(
    performKoenkerTest(m, stats::model.frame(m)), performKoenkerTest(m, d),
    info = "frame"
  )
})

test_that("runHeteroTests() and runDiagnostics() stop and fall back on nothing", {
  skip_if_not_installed("dplyr")
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  arranged <- dplyr::arrange(d, x1)
  local_silent_log()
  expect_error(
    runHeteroTests(m, arranged, use_cache = FALSE, progress = FALSE),
    "performWhiteTest() cannot match the rows of `data`",
    fixed = TRUE
  )
  expect_error(
    runDiagnostics(m, arranged, use_cache = FALSE, progress = FALSE),
    "performWhiteTest() cannot match the rows of `data`",
    fixed = TRUE
  )
  # The analysis of the failure offers no substitute, so no name can hold
  # another test: here every test of a battery whose fallbacks include the
  # Cook-Weisberg test, which ignores `data`, comes back failed under its own
  # name.
  tests <- c("white", "koenker", "breusch_pagan", "rank_permutation", "high_dimensional")
  out <- .run_hetero_core(
    m, arranged,
    tests = tests, use_cache = FALSE, chunk_threshold_mb = 100,
    chunk_size = 10000, progress = FALSE, recover_failures = TRUE
  )
  for (nm in tests) {
    expect_identical(attr(out[[nm]], "diagnostic"), nm, info = nm)
    expect_identical(attr(out[[nm]], "extras")$issue, "data_rows", info = nm)
    expect_identical(attr(out[[nm]], "extras")$status, "error", info = nm)
    expect_true(is.na(out[[nm]]$statistic), info = nm)
  }
  # The same battery on the data of the fit is unchanged.
  expect_identical(
    unname(runHeteroTests(m, d, use_cache = FALSE, progress = FALSE)$koenker$statistic),
    unname(performKoenkerTest(m, d)$statistic)
  )
})
