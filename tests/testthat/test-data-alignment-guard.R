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
    "Pass the rows of the fit in the order of the fit, or a data frame with its row names.",
    fixed = TRUE
  )
  # Rows that carry the names of the fit but other values.
  other <- d
  other$y <- rev(other$y)
  expect_error(
    performKoenkerTest(m, other[sample(n), ]),
    paste(
      "the rows of `data` that carry the row names of the fit do not hold its",
      "values: y differs from the model frame. Pass the data the model was fitted on."
    ),
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
  # Transformed regressors are computed from `data` through the predvars of
  # the terms and compared. On data that number their rows, which are matched
  # by position, the tibble of the fit passes and the tibble in another order
  # is refused, poly() and ns() included; a term computed from all the rows,
  # such as I(x1 - mean(x1)), is not a difference.
  tb <- dplyr::as_tibble(d)
  formulas <- list(
    y ~ log(x1) + x2, y ~ poly(x1, 2) + x2, y ~ I(x1 - mean(x1)) + I(x2^2),
    y ~ splines::ns(x1, 3) + x2
  )
  for (f in formulas) {
    mf <- lm(f, data = tb)
    expect_same_result(
      performKoenkerTest(mf, tb), performKoenkerTest(lm(f, data = d), d),
      info = deparse(f)
    )
    expect_refused("performKoenkerTest", mf, tb[rev(seq_len(nrow(tb))), ], info = deparse(f))
  }
  # A response computed from all the rows is evaluated on all the rows of
  # `data`, as model.frame() evaluated it before dropping rows.
  d_na <- d
  d_na$x2[c(3, 7)] <- NA
  centred <- lm(I(y - mean(y)) ~ x1 + x2, data = d_na)
  expect_warning(performKoenkerTest(centred, d_na), "Removed 2 observations")
  expect_s3_class(suppressWarnings(performKoenkerTest(centred, d_na)), "htest")
  # Given only the rows the fit kept, such a term is shifted by a constant,
  # which is not a difference either.
  kept <- d_na[rownames(stats::model.frame(centred)), ]
  expect_same_result(
    performKoenkerTest(centred, kept), suppressWarnings(performKoenkerTest(centred, d_na)),
    info = "kept rows"
  )
  in_subset <- lm(y ~ I(x1 - mean(x1)) + x2, data = d, subset = x2 > 0)
  expect_same_result(
    performKoenkerTest(in_subset, d[rownames(stats::model.frame(in_subset)), ]),
    performKoenkerTest(in_subset, d),
    info = "subset"
  )
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
  # The runners keep the class of the refusal.
  for (use_cache in c(FALSE, TRUE)) {
    expect_error(
      runHeteroTests(m, arranged, use_cache = use_cache, progress = FALSE),
      "performWhiteTest\\(\\) cannot match the rows of `data`",
      class = "heteroTests_data_rows_error"
    )
    expect_error(
      runDiagnostics(m, arranged, use_cache = use_cache, progress = FALSE),
      "performWhiteTest\\(\\) cannot match the rows of `data`",
      class = "heteroTests_data_rows_error"
    )
  }
  # A valid call cached first does not let the reordered data through.
  cached <- runHeteroTests(m, d, use_cache = TRUE, progress = FALSE)
  expect_error(
    runHeteroTests(m, arranged, use_cache = TRUE, progress = FALSE),
    class = "heteroTests_data_rows_error"
  )
  expect_s3_class(cachedTest("koenker", m, d), "htest")
  expect_error(cachedTest("koenker", m, arranged), class = "heteroTests_data_rows_error")
  expect_identical(
    unname(runHeteroTests(m, d, use_cache = TRUE, progress = FALSE)$koenker$statistic),
    unname(cached$koenker$statistic)
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

test_that("a model fitted with model = FALSE refuses a frame rebuilt from changed data", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  de <- d
  m <- lm(y ~ x1 + x2, data = de, model = FALSE)
  expected <- performKoenkerTest(m, de)
  expect_same_result(performKoenkerTest(m), expected, info = "unchanged, NULL")
  # The data the fit refers to are reordered after the fit: the frame rebuilt
  # from them is compared with the response of the fit, not with itself.
  de <- dplyr::arrange(de, x1)
  stale <- "cannot use the model frame of `model`: the model was fitted with `model = FALSE`"
  expect_error(performKoenkerTest(m, de), stale, fixed = TRUE)
  expect_error(performKoenkerTest(m), stale, fixed = TRUE)
  expect_error(
    runHeteroTests(m, use_cache = FALSE, progress = FALSE),
    class = "heteroTests_data_rows_error"
  )
  de <- d
  expect_same_result(performKoenkerTest(m), expected, info = "restored")
})

test_that("transformed regressors, weights and offsets are compared too", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  set.seed(31)
  n <- 120
  d <- data.frame(x = runif(n, 1, 5), age = runif(n, 20, 60), z = rnorm(n))
  d$y01 <- rbinom(n, 1, stats::plogis(-1 + 0.5 * d$x))
  d$y <- 1 + d$x + d$z + rnorm(n, sd = d$x)
  d$g <- factor(rep(1:4, length.out = n))
  # A 0/1 response with ties, sorted by it: data arranged by (y01, x) hold the
  # response in the same order and differ only in the transformed regressors.
  d01 <- d[order(d$y01), ]
  rownames(d01) <- NULL
  m01 <- lm(y01 ~ log(x) + poly(age, 2), data = d01)
  arranged <- dplyr::arrange(d01, y01, x)
  expect_identical(arranged$y01, d01$y01)
  for (fn in c("performKoenkerTest", "performWhiteTest", "performGlejserTest")) {
    expect_refused(fn, m01, arranged, info = fn)
  }
  expect_error(performKoenkerTest(m01, arranged), "log(x), poly(age, 2) differ", fixed = TRUE)
  # Rows tied on y, x and z that differ in their weight or their offset.
  tied <- d[rep(seq_len(30), each = 4), ]
  rownames(tied) <- NULL
  tied$w <- runif(nrow(tied), 0.5, 2)
  tied$o <- rnorm(nrow(tied))
  swap <- function(v) stats::ave(v, tied$x, FUN = rev)
  m_w <- lm(y ~ x + z, data = tied, weights = w)
  m_o <- lm(y ~ x + z + offset(o), data = tied)
  m_oa <- lm(y ~ x + z, data = tied, offset = o)
  # Tied rows make the residuals far from normal, which Bartlett's test warns
  # about; the warning is not what is tested here.
  expect_s3_class(suppressWarnings(performBartlettTest(m_w, tied, "g")), "htest")
  swapped_w <- tied
  swapped_w$w <- swap(tied$w)
  swapped_o <- tied
  swapped_o$o <- swap(tied$o)
  expect_error(performBartlettTest(m_w, swapped_w, "g"), "(weights) differs", fixed = TRUE)
  expect_error(performBartlettTest(m_o, swapped_o, "g"), "offset(o) differs", fixed = TRUE)
  expect_error(performBartlettTest(m_oa, swapped_o, "g"), "(offset) differs", fixed = TRUE)
  # A test that reads `o` for an offset() term, with `o` in another order.
  permuted <- tied
  permuted$o <- rev(permuted$o)
  expect_error(performGQTest(m_o, permuted, order_by = "x"), class = "heteroTests_data_rows_error")
  expect_s3_class(performGQTest(m_o, tied, order_by = "x"), "htest")
})

test_that("numbers are compared value by value, dates and times exactly", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  n <- nrow(d)
  # One huge value no longer loosens the comparison of the whole column.
  big <- d
  big$y[1] <- 1e10
  m_big <- lm(y ~ log(x1), data = big)
  swapped <- big
  swapped$y[2:11] <- swapped$y[c(3, 2, 5, 4, 7, 6, 9, 8, 11, 10)]
  # The huge value is an outlier, which the test warns about.
  expect_s3_class(suppressWarnings(performKoenkerTest(m_big, big)), "htest")
  expect_refused("performKoenkerTest", m_big, swapped, info = "1e10")
  for (k in c(1e-200, 1e200)) {
    dk <- d
    dk$y <- dk$y * k
    mk <- lm(y ~ x1 + x2, data = dk)
    sk <- dk
    sk$y[1:2] <- sk$y[2:1]
    expect_error(performKoenkerTest(mk, sk), class = "heteroTests_data_rows_error", info = k)
  }
  near <- d
  near$y <- 1e15 + near$y
  m_near <- lm(y ~ x1 + x2, data = near)
  expect_same_result(
    performKoenkerTest(m_near, dplyr::as_tibble(near)), performKoenkerTest(m_near, near),
    info = "1e15"
  )
  dates <- d
  dates$t <- as.POSIXct("2026-01-01", tz = "UTC") + seq_len(n)
  m_t <- lm(y ~ x1 + t, data = dates)
  one_second <- dates
  one_second$t[1:2] <- one_second$t[2:1]
  expect_error(performKoenkerTest(m_t, one_second), "t differs", fixed = TRUE)
})

test_that("the registry name student_bp checks data as performStudentizedBPTest() does", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  arranged <- dplyr::arrange(d, x1)
  expect_error(
    suppressWarnings(runHeteroTests(m, arranged, tests = "student_bp", use_cache = FALSE,
                                    progress = FALSE)),
    "performStudentizedBPTest() cannot match the rows of `data`",
    fixed = TRUE
  )
  expect_error(
    test_factory$run_test("student_bp", m, arranged),
    class = "heteroTests_data_rows_error"
  )
  expect_identical(
    unname(suppressWarnings(runHeteroTests(m, d, tests = "student_bp", use_cache = FALSE,
                                           progress = FALSE))$student_bp$statistic),
    unname(suppressWarnings(performStudentizedBPTest(m, d))$statistic)
  )
})

test_that("tests that call another test pass it the rows under the fit's names", {
  # A fit to rows in another order, under their own names, and the tibble of
  # the data in their first order: the rows are matched to the fit by position
  # of those names, and the robust and bootstrap tests, which call White's or
  # the Breusch-Pagan test on the aligned rows again, give the value on the
  # data of the fit.
  skip_if_not_installed("dplyr")
  d <- guard_data()
  set.seed(4)
  shuffled <- d[sample(nrow(d)), ]
  m <- lm(y ~ x1 + x2, data = shuffled)
  tb <- dplyr::as_tibble(d)
  for (fn in c("performWhiteTest", "performWhiteTestRobust", "performWhiteTestBootstrap",
               "performBPTestRobust", "performKoenkerTest")) {
    expect_same_result(guard_run(fn, m, tb), guard_run(fn, m, shuffled), info = fn)
  }
  local_silent_log()
  expect_same_result(
    performBPTestRobust(m, tb, studentize = FALSE),
    performBPTestRobust(m, shuffled, studentize = FALSE),
    info = "BP"
  )
})

test_that("messages name the function called and say why", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  d <- guard_data()
  m <- lm(y ~ x1 + x2, data = d)
  arranged <- dplyr::arrange(d, x1)
  expect_error(
    suppressWarnings(performBreuschPaganTest(m, arranged)),
    "^performBreuschPaganTest\\(\\) cannot match"
  )
  expect_error(
    do.call(performKoenkerTest, list(m, arranged)),
    "^performKoenkerTest\\(\\) cannot match"
  )
  expect_error(
    heteroTests::performGlejserTest(m, arranged, "x1"),
    "^performGlejserTest\\(\\) cannot match"
  )
  wide <- lm(y ~ x1 + x2 + w + o + ypos, data = d)
  expect_error(performKoenkerTest(wide, arranged), "y, x1, x2 and 3 more differ", fixed = TRUE)
  # A tibble the test renumbered when it dropped incomplete rows.
  dn <- d
  dn$x2[c(3, 7)] <- NA
  tb <- dplyr::as_tibble(dn)
  m_short <- lm(y ~ x1 + x2, data = tb[1:100, ])
  expect_error(
    suppressWarnings(performKoenkerTest(m_short, tb)),
    "^performKoenkerTest\\(\\) cannot match.*afresh"
  )
  # The scatter diagnostic reads by position and cannot match the names of a
  # fit to character row names against a tibble.
  dc <- d
  rownames(dc) <- paste0("obs", seq_len(nrow(dc)))
  m_c <- lm(y ~ x1 + x2, data = dc)
  expect_error(
    performScatterDiagnostic(m_c, dplyr::as_tibble(dc), "x1"),
    "the row names of `data` are not those of the fit",
    fixed = TRUE
  )
  expect_identical(
    performScatterDiagnostic(m_c, dc, "x1"),
    performScatterDiagnostic(m, d, "x1")
  )
})

test_that("a fitWLS() fit is matched like the fit it reweights", {
  skip_if_not_installed("dplyr")
  d <- guard_data()
  wls <- fitWLS(lm(y ~ x1 + x2, data = d))
  set.seed(12)
  shuffled <- d[sample(nrow(d)), ]
  tests <- c("performKoenkerTest", "performWhiteTest", "performGlejserTest", "performBartlettTest")
  for (fn in tests) {
    expect_same_result(guard_run(fn, wls, shuffled), guard_run(fn, wls, d), info = fn)
    expect_refused(fn, wls, dplyr::arrange(d, x1), info = fn)
  }
})
