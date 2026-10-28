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
  "performBPRandomEffectsTest",
  # The tests computed from the model alone read nothing from `data`, and
  # gave the value of the fit on data the guard refuses; since 0.16.0 they
  # match `data` as every test built on the model context does (decision 31
  # of its design).
  "performSpearmanTest", "performHarveyTest", "performArchLMTest", "performMcLeodLiTest",
  "performCookWeisbergTest", "performNCVTest", "performSpreadLevelTest",
  "performDavidianCarrollTest", "performVarianceFormTest", "performRESETTest"
)
# These read `data` by position, the i-th row with the i-th residual, so from
# 0.15.0 they also refused data with its own row names in another order than
# the fit, and rows after those of the fit. Since 0.16.0 they read the rows the
# model context confirmed, as the others match them by name (below).
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

test_that("every test refuses data the guard cannot match, those that read nothing too", {
  # Until 0.16.0 the tests computed from the model alone gave the value of the
  # fit on such data, which they do not read; since decision 31 of the design
  # of the model context every test matches `data` the same way, so no test
  # is left outside the guard.
  expect_identical(setdiff(names(guard_recipes()), c(guarded_tests, positional_tests)),
                   character())
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
  # tests that match by name give the value on the data of the fit, and since
  # 0.16.0 so do those that read by position, which refused another number of
  # rows ("requires `data` with 120 observations").
  for (fn in names(recipes)) {
    expect_same_result(guard_run(fn, m, end, recipes), guard_run(fn, m, d, recipes), info = fn)
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
  # The panel tests and the scatter diagnostic read such data against other
  # residuals, which gave another value, and from 0.15.0 refused it ("it reads
  # `data` by position, the i-th row with the i-th observation"); since
  # 0.16.0 they match it by name too.
  for (fn in names(recipes)) {
    expect_same_result(guard_run(fn, m, shuffled, recipes), guard_run(fn, m, d, recipes), info = fn)
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
    # As a data frame its row numbers survive the drop.
    expect_same_result(
      guard_run(fn, m, as.data.frame(tb)),
      guard_run(fn, m, as.data.frame(tb[1:100, ])),
      info = fn
    )
  }
  # The tests built on the model context (since 0.16.0) match the rows before
  # they drop any, so the tibble holds the rows of the fit under its original
  # row numbers and gives the value on the rows of the fit. The wild bootstrap
  # dropped them first, and refused the renumbered rows ("numbers the rows
  # that are left afresh"), until the eighth step of the model context.
  for (fn in c("performKoenkerTest", "performWhiteTest", "performBPTest", "performGlejserTest",
               "performWildBootstrapTest")) {
    expect_same_result(
      guard_run(fn, m, tb),
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
  # Without var_formula the test reads nothing from `data`, which since 0.16.0
  # it matches as every test built on the model context does (decision 31 of
  # its design): before, it gave the value of the fit on data in another
  # order.
  expect_error(
    performCookWeisbergTest(m, dplyr::arrange(d, x1)),
    "performCookWeisbergTest() cannot match the rows of `data`",
    fixed = TRUE
  )
  expect_identical(
    performCookWeisbergTest(m, shuffled)$statistic,
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
    tests = tests, use_cache = FALSE, chunk_threshold_mb = Inf,
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
  # A tibble the test renumbered when it dropped incomplete rows. The tests
  # built on the model context drop none and accept it (above), the wild
  # bootstrap, the last that dropped them first, since the eighth step; the
  # preparation of before, which no test calls and the tenth step removes,
  # still refuses it.
  dn <- d
  dn$x2[c(3, 7)] <- NA
  tb <- dplyr::as_tibble(dn)
  m_short <- lm(y ~ x1 + x2, data = tb[1:100, ])
  set.seed(9)
  expect_s3_class(
    suppressWarnings(performWildBootstrapTest(m_short, tb, B = 9, progress = FALSE)), "htest"
  )
  expect_error(
    suppressWarnings(heteroTests:::prepare_model_data_for_test(
      m_short, tb, c("y", "x1", "x2"), "Wild bootstrap"
    )),
    "cannot match.*afresh"
  )
  # The scatter diagnostic read by position and could not match the names of
  # a fit to character row names against a tibble ("the row names of `data`
  # are not those of the fit"); since 0.16.0 it takes the tibble in the order
  # of the fit, which holds one row per observation.
  dc <- d
  rownames(dc) <- paste0("obs", seq_len(nrow(dc)))
  m_c <- lm(y ~ x1 + x2, data = dc)
  expect_identical(
    performScatterDiagnostic(m_c, dplyr::as_tibble(dc), "x1"),
    performScatterDiagnostic(m, d, "x1")
  )
  expect_identical(
    performScatterDiagnostic(m_c, dc, "x1"),
    performScatterDiagnostic(m, d, "x1")
  )
})

test_that("a term that depends on the rows is not compared, the others decide", {
  # na.omit and subset = computed these terms on all the rows of the data and
  # then dropped some, so the rows the fit kept, which are the right rows,
  # compute them otherwise. They are not compared; y, x and z, continuous,
  # tell the rows apart, and the result is that on the full data.
  local_silent_log()
  set.seed(21)
  n <- 120
  d <- data.frame(x = runif(n, 1, 5), z = rnorm(n), g = factor(rep(1:4, length.out = n)))
  d$y <- 1 + d$x + d$z + rnorm(n, sd = d$x)
  d_na <- d
  d_na$z[c(4, 9, 15)] <- NA
  formulas <- list(
    y ~ x + I((x - mean(x))^2) + z, y ~ x + I(x > median(x)) + z,
    y ~ x + rank(x) + z, y ~ I(x - mean(x)) + z
  )
  tests <- list(
    function(m, data) performKoenkerTest(m, data),
    function(m, data) performWhiteTest(m, data),
    function(m, data) performGlejserTest(m, data, "x"),
    function(m, data) performBartlettTest(m, data, "g"),
    function(m, data) performParkTest(m, data, "x")
  )
  for (f in formulas) {
    m_na <- lm(f, data = d_na)
    m_sub <- lm(f, data = d, subset = x > 2)
    kept_sub <- d[rownames(stats::model.frame(m_sub)), ]
    for (k in seq_along(tests)) {
      info <- paste(deparse(f), k)
      full <- suppressWarnings(tests[[k]](m_na, d_na))
      expect_same_result(tests[[k]](m_na, d_na[stats::complete.cases(d_na), ]), full, info = info)
      expect_same_result(tests[[k]](m_sub, kept_sub), tests[[k]](m_sub, d), info = info)
    }
  }
  # The White bootstrap, without `data`, refits each replicate on the stored
  # design, which holds such a term as the fit computed it, on all the rows
  # (since the eighth step of the model context; before, on the model frame,
  # from which the replicate computed it again on the rows it kept).
  m_quad <- lm(formulas[[1]], data = d_na)
  set.seed(3)
  expect_s3_class(suppressWarnings(performWhiteTestBootstrap(m_quad, B = 9)), "htest")
})

test_that("columns that cannot be compared must leave no ties that matter", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  # T1: four earlier rows in front, a 0/1 response of period 4, scale(t). The
  # response matches by position; scale(t) is computed through its predvars
  # and compared exactly, so the shift is seen.
  t1 <- data.frame(t = 1:120, g = factor(rep(1:3, 40)))
  t1$y01 <- rep(c(0, 0, 1, 1), 30)
  m_t1 <- lm(y01 ~ scale(t), data = t1)
  front <- rbind(t1[1:4, ], t1)
  rownames(front) <- NULL
  expect_s3_class(suppressWarnings(performBartlettTest(m_t1, t1, "g")), "htest")
  expect_error(performBartlettTest(m_t1, front, "g"), "scale(t) differs", fixed = TRUE)
  # T1b: every column depends on the rows, so none can be compared.
  t1b <- data.frame(t = 1:120, g = factor(rep(1:3, 40)))
  t1b$y <- t1b$t + rep(c(0, 1, 0, -1), 30) * 3
  m_t1b <- lm(I(y - mean(y)) ~ I(t - mean(t)), data = t1b)
  front_b <- rbind(t1b[1:4, ], t1b)
  rownames(front_b) <- NULL
  expect_s3_class(suppressWarnings(performBartlettTest(m_t1b, t1b, "g")), "htest")
  expect_error(
    performBartlettTest(m_t1b, front_b, "g"),
    "depends on the rows it is computed from", fixed = TRUE
  )
  expect_error(performBartlettTest(m_t1b, front_b, "g"), "no column can be compared", fixed = TRUE)
  # T5: a function of the formula that is no longer found, and a 0/1 response
  # that cannot tell the rows apart on its own: refused, also in the order of
  # the fit, which cannot be confirmed.
  set.seed(5)
  lp <- data.frame(x = runif(120, 1, 5))
  lp$y01 <- rbinom(120, 1, stats::plogis(-2 + 0.7 * lp$x))
  lp <- lp[order(lp$y01), ]
  rownames(lp) <- NULL
  helper <- function(v) v^2 + v
  m_t5 <- lm(y01 ~ helper(x), data = lp)
  rm(helper)
  within <- dplyr::arrange(lp, y01, x)
  expect_error(
    performGlejserTest(m_t5, within, "x"),
    "helper(x) cannot be computed from `data`", fixed = TRUE
  )
  expect_error(performGlejserTest(m_t5, lp, "x"), class = "heteroTests_data_rows_error")
  # T7: computed columns are compared value by value too, so one large x no
  # longer loosens I(x^2), and two rows tied on round(y) cannot be swapped.
  d <- guard_data()
  names(d)[names(d) == "x1"] <- "x"
  d$x[1] <- 1e4
  m_t7 <- lm(I(round(y)) ~ I(x^2), data = d)
  ry <- round(d$y)
  pair <- which(ry == ry[2] & seq_along(ry) > 2)[1]
  swapped <- d
  swapped[c(2, pair), ] <- swapped[c(pair, 2), ]
  rownames(swapped) <- NULL
  expect_s3_class(suppressWarnings(performKoenkerTest(m_t7, d)), "htest")
  expect_error(performKoenkerTest(m_t7, swapped), "I(x^2) differs", fixed = TRUE)
})

test_that("a column from outside data does not need telling rows apart", {
  # Weights or an offset taken from the workspace are read by no test from
  # `data`, so rows that tie on every compared column can be exchanged
  # without changing anything a test reads: the fit's own data pass.
  local_silent_log()
  set.seed(31)
  n <- 120
  dt <- data.frame(
    score = sample(1:5, n, TRUE), g = factor(sample(letters[1:3], n, TRUE)),
    x = runif(n, 1, 5)
  )
  wts <- runif(n, 0.5, 2)
  off <- rnorm(n)
  with_column <- dt
  with_column$w <- wts
  with_column$o <- off
  m_ws <- lm(score ~ g, data = dt, weights = wts)
  m_wc <- lm(score ~ g, data = with_column, weights = w)
  m_os <- lm(score ~ g, data = dt, offset = off)
  m_oc <- lm(score ~ g, data = with_column, offset = o)
  expect_same_result(performKoenkerTest(m_ws, dt), performKoenkerTest(m_wc, with_column),
                     info = "weights")
  # A discrete response: Bartlett's test warns about non-normal residuals.
  expect_same_result(suppressWarnings(performBartlettTest(m_ws, dt, "g")),
                     suppressWarnings(performBartlettTest(m_wc, with_column, "g")),
                     info = "weights")
  expect_same_result(performKoenkerTest(m_os, dt), performKoenkerTest(m_oc, with_column),
                     info = "offset")
  # The same tied rows swapped, with the weights a column of `data`: the
  # weights are compared, and the swap is refused.
  tied <- order(with_column$score, with_column$g)[1:2]
  swapped <- with_column
  swapped[tied, ] <- swapped[rev(tied), ]
  rownames(swapped) <- NULL
  expect_error(performKoenkerTest(m_wc, swapped), "(weights) differs", fixed = TRUE)
  # No column at all can be compared: the guard cannot confirm the data
  # (rcheck_data_rows(), which no test calls since the eighth step of the
  # model context, when the wild bootstrap, the last that did, moved onto
  # it). The tests built on the model context, the scatter diagnostic among
  # them since 0.16.0 and the wild bootstrap since the eighth step, look for
  # the variables of the model first, and `d` is not a column of `data`.
  d <- data.frame(x = runif(n, 1, 5), z = rnorm(n))
  d$y <- 1 + d$x + d$z + rnorm(n)
  expect_error(
    heteroTests:::rcheck_data_rows(lm(d$y ~ d$x + d$z), d, fn = "performX"),
    "no column can be compared", fixed = TRUE
  )
  for (call in list(
    function() performWildBootstrapTest(lm(d$y ~ d$x + d$z), d, B = 9, progress = FALSE),
    function() performScatterDiagnostic(lm(d$y ~ d$x + d$z), d, "x")
  )) {
    expect_error(call(), "The following variables are missing from input data: d.", fixed = TRUE)
  }
})

test_that("a term that depends on the first row only is recognised", {
  # Every row is left out of one of the probes, so I(x - x[1]) and
  # I(x / max(x)) are found to depend on the rows when na.omit dropped the
  # first row and that of the largest x.
  local_silent_log()
  set.seed(32)
  n <- 120
  b <- data.frame(
    x = runif(n, 1, 5), z = rnorm(n), g = factor(rep(1:4, length.out = n)), time = seq_len(n)
  )
  b$y <- 1 + b$x + b$z + rnorm(n, sd = b$x)
  b$z[c(1, which.max(b$x))] <- NA
  kept <- b[stats::complete.cases(b), ]
  formulas <- list(y ~ I(x / max(x)) + z, y ~ I(x - x[1]) + z, y ~ I(time - time[1]) + x + z)
  for (f in formulas) {
    m <- lm(f, data = b)
    expect_same_result(performKoenkerTest(m, kept), suppressWarnings(performKoenkerTest(m, b)),
                       info = deparse(f))
    expect_same_result(performBartlettTest(m, kept, "g"),
                       suppressWarnings(performBartlettTest(m, b, "g")), info = deparse(f))
  }
})

test_that("a model = FALSE fit is checked through its fitted values too", {
  skip_if_not_installed("dplyr")
  local_silent_log()
  set.seed(5)
  lp <- data.frame(x = runif(120, 1, 5))
  lp$y01 <- rbinom(120, 1, stats::plogis(-2 + 0.7 * lp$x))
  lp <- lp[order(lp$y01), ]
  rownames(lp) <- NULL
  # T3: a linear probability model; the data are sorted again after the fit
  # within the ties of the response, which keeps the response where it was.
  dd <- lp
  m <- lm(y01 ~ x, data = dd, model = FALSE)
  expected <- performKoenkerTest(m)
  dd <- dplyr::arrange(dd, y01, x)
  expect_identical(dd$y01, lp$y01)
  expect_error(performKoenkerTest(m), "cannot use the model frame of `model`", fixed = TRUE)
  expect_error(performKoenkerTest(m, dd), "cannot use the model frame of `model`", fixed = TRUE)
  # T4: x shuffled after the fit.
  dd <- lp
  set.seed(6)
  dd$x <- sample(dd$x)
  expect_error(performKoenkerTest(m), "cannot use the model frame of `model`", fixed = TRUE)
  dd <- lp
  expect_same_result(performKoenkerTest(m), expected, info = "restored")
  # A rank-deficient fit leaves its aliased coefficient out of the check.
  dd$x2 <- 2 * dd$x
  m_aliased <- lm(y01 ~ x + x2, data = dd, model = FALSE)
  expect_true(anyNA(stats::coef(m_aliased)))
  expect_null(rcheck_model_frame(m_aliased, stats::model.frame(m_aliased)))
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
