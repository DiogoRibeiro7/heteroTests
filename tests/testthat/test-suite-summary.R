# print() and summary() of the suites of runHeteroTests() (R/suite_summary.R),
# and the data.name the runner gives every test. The printed output is
# compared with the snapshots in _snaps/suite-summary.md; the suites are built
# outside expect_snapshot(), silently, so that no warning or log line of a test
# reaches the snapshot.

suite_quiet <- function(env = parent.frame()) {
  previous <- ht_set_log_level("SILENT")
  withr::defer(ht_set_log_level(previous), envir = env)
}

suite_run <- function(...) {
  suppressWarnings(suppressMessages(runHeteroTests(..., use_cache = FALSE, progress = FALSE)))
}

# A suite whose tests failed, as a grouped run records them: without groups the
# runner stops on the first failure.
suite_failed <- function(model, data, tests) {
  suppressWarnings(heteroTests:::.run_hetero_core(
    model, data,
    tests = tests, use_cache = FALSE, chunk_threshold_mb = 100, chunk_size = 10000,
    progress = FALSE, recover_failures = TRUE
  ))
}

test_that("a suite prints as a table, and its summary as summary.lm() does", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
  expect_snapshot({
    s
    summary(s)
  })
})

test_that("a fallback is named in the notes", {
  suite_quiet()
  # White's test needs 20 observations, and so does its form without cross
  # products: the runner falls back on the Breusch-Pagan test.
  small <- mtcars[1:18, ]
  s <- suite_run(lm(mpg ~ wt, data = small), small)
  expect_identical(attr(s$white, "extras")$status, "fallback")
  expect_snapshot({
    s
    summary(s)
  })
})

test_that("a failed or refused test shows NA and its message", {
  suite_quiet()
  fit <- suppressWarnings(glm(am ~ wt, family = binomial, data = mtcars))
  s <- suite_failed(fit, mtcars, c("white", "koenker"))
  expect_identical(unname(summary(s)$status), c("error", "error"))
  expect_snapshot({
    s
    summary(s)
  })
  # Without groups the runner stops on the refusal, as before.
  expect_error(suite_run(fit, mtcars), "needs a model fitted by stats::lm")
})

test_that("the significance codes follow show.signif.stars, and digits is respected", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = c("white", "koenker"))
  expect_snapshot({
    print(s, signif.stars = FALSE)
    print(summary(s), digits = 6)
  })
  withr::local_options(show.signif.stars = FALSE)
  expect_snapshot({
    s
    summary(s)
  })
})

test_that("parameters that are not degrees of freedom go to the notes", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = c("white", "variance_form", "szroeter"))
  expect_snapshot(s)
  tab <- summary(s)$tests
  expect_identical(colnames(tab), c("Statistic", "df", "df2", "p-value"))
  expect_identical(unname(tab["variance_form", c("df", "df2")]),
                   unname(as.double(s$variance_form$parameter)))
  expect_true(is.na(tab["szroeter", "df"]))
})

test_that("an empty suite and an all-failed suite print", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  empty <- suite_run(m, mtcars, tests = character())
  expect_length(empty, 0L)
  fit <- suppressWarnings(glm(am ~ wt, family = binomial, data = mtcars))
  failed <- suite_failed(fit, mtcars, "white")
  expect_snapshot({
    empty
    summary(empty)
    summary(failed)
  })
  expect_identical(dim(summary(empty)$tests), c(0L, 3L))
})

test_that("a grouped suite prints one table, with the status of each group", {
  skip_if_not_installed("dplyr")
  suite_quiet()
  # cyl = 4 and 8 have 11 and 14 observations, and both tests fall back on
  # the Cook-Weisberg test; with 7, cyl = 6 has too few for any of them.
  g <- suite_run(mpg ~ wt, dplyr::group_by(mtcars, cyl))
  expect_snapshot({
    g
    summary(g)
  })
  tests <- summary(g)$tests
  expect_s3_class(tests, "data.frame")
  expect_identical(
    names(tests), c("cyl", "test", "Statistic", "df", "p-value", "status")
  )
  expect_identical(tests$status, c("fallback", "fallback", "error", "error",
                                   "fallback", "fallback"))
  expect_identical(summary(g)$nobs, c(11L, 7L, 14L))
  expect_identical(tests$Statistic[[1L]], unname(g[[1L]]$white$statistic))
})

test_that("summary(s)$tests holds the values of the results", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
  sm <- summary(s)
  expect_s3_class(sm, "summary.hetero_test_suite")
  expect_named(
    unclass(sm),
    c("call", "formula", "model_class", "nobs", "na.action", "weights", "offset",
      "family", "tests", "method", "diagnostic", "status", "notes")
  )
  tab <- sm$tests
  expect_true(is.matrix(tab) && is.double(tab))
  expect_identical(dimnames(tab), list(names(s), c("Statistic", "df", "p-value")))
  for (name in names(s)) {
    expect_identical(tab[name, "Statistic"], unname(s[[name]]$statistic))
    expect_identical(tab[name, "df"], unname(as.double(s[[name]]$parameter)))
    expect_identical(tab[name, "p-value"], s[[name]]$p.value)
  }
  expect_identical(sm$call, m$call)
  expect_identical(sm$nobs, 32L)
  expect_identical(unname(sm$status), rep("ok", 3L))
  expect_identical(sm$method[["koenker"]], s$koenker$method)
  expect_identical(sm$notes, character())
})

test_that("the summary records the weights, offset, family and dropped rows", {
  suite_quiet()
  d <- mtcars
  d$wt[3] <- NA
  m <- lm(mpg ~ wt + qsec + offset(log(disp)), data = d, weights = 1 / hp)
  sm <- summary(suite_run(m, d))
  expect_identical(sm$weights, "1/hp")
  expect_identical(sm$offset, "offset(log(disp))")
  expect_null(sm$family)
  expect_identical(sm$nobs, 31L)
  expect_s3_class(sm$na.action, "omit")
  g <- glm(mpg ~ wt + qsec, data = mtcars, family = gaussian)
  expect_identical(summary(suite_run(g, mtcars))$family, "gaussian (identity link)")
  # A formula is fitted by the runner, whose call names its own variables.
  sf <- summary(suite_run(mpg ~ wt + qsec, mtcars))
  expect_null(sf$call)
  expect_identical(deparse(sf$formula), "mpg ~ wt + qsec")
})

test_that("print returns its input invisibly", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars)
  sm <- summary(s)
  for (x in list(s, sm)) {
    out <- utils::capture.output(shown <- withVisible(print(x)))
    expect_false(shown$visible)
    expect_identical(shown$value, x)
    expect_gt(length(out), 0L)
  }
  skip_if_not_installed("dplyr")
  g <- suite_run(mpg ~ wt, dplyr::group_by(mtcars, am), tests = "koenker")
  for (x in list(g, summary(g))) {
    utils::capture.output(shown <- withVisible(print(x)))
    expect_false(shown$visible)
    expect_identical(shown$value, x)
  }
})

test_that("a test of the runner that named its argument \"model\" names the formula", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  formula_text <- "mpg ~ wt + qsec"
  s <- suite_run(m, mtcars, tests = c("white", "koenker", "breusch_pagan", "szroeter",
                                      "student_bp"))
  for (name in names(s)) {
    expect_identical(s[[name]]$data.name, formula_text, info = name)
  }
  # Through the caches too: the whole call, and each test.
  clearAnalysisCache()
  clearTestCache()
  cached <- suppressWarnings(runHeteroTests(m, mtcars, progress = FALSE))
  again <- suppressWarnings(runHeteroTests(m, mtcars, progress = FALSE))
  expect_identical(cached$white$data.name, formula_text)
  expect_identical(again, cached)
  expect_identical(cached, suite_run(m, mtcars))
  # A data.name that begins with the formula and says more is kept.
  ncv <- suppressWarnings(suite_run(m, mtcars, tests = "ncv"))
  expect_identical(ncv$ncv$data.name, paste0(formula_text, "; variance model: fitted values"))
  # The fallback of White's test without cross products, and a fallback on
  # another test.
  small <- mtcars[1:18, ]
  fb <- suite_run(lm(mpg ~ wt, data = small), small)
  expect_identical(fb$white$data.name, "mpg ~ wt")
  # Called directly, a test names the object it is given, as before.
  expect_identical(suppressWarnings(performWhiteTest(m, mtcars))$data.name, "m")
  expect_identical(
    suppressWarnings(cachedTest("white", m, mtcars, use_cache = FALSE))$data.name,
    "model"
  )
})

# A diagnostic registered for the calling test only.
suite_local_diagnostic <- function(name, fun, env = parent.frame()) {
  registerDiagnostic(name, fun)
  withr::defer(rm(list = name, envir = heteroTests:::.diagnostic_registry), envir = env)
}

test_that("a registered diagnostic keeps its data.name, and R's names of an F test's df", {
  suite_quiet()
  # var.test() of the two halves of the residuals: its data.name says what it
  # compared, and its parameters are "num df" and "denom df".
  suite_local_diagnostic("halves", function(model, data) {
    r <- stats::residuals(model)
    h <- length(r) %/% 2L
    stats::var.test(r[seq_len(h)], r[-seq_len(h)])
  })
  # A diagnostic that reports "model", as the registry's own functions do.
  suite_local_diagnostic("white_again", function(model, data) performWhiteTest(model, data))
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = c("halves", "white_again"))
  expect_identical(s$halves$data.name, "r[seq_len(h)] and r[-seq_len(h)]")
  expect_identical(s$white_again$data.name, "mpg ~ wt + qsec")
  tab <- coef(summary(s))
  expect_identical(colnames(tab), c("Statistic", "df", "df2", "p-value"))
  expect_identical(unname(tab["halves", c("df", "df2")]), unname(as.double(s$halves$parameter)))
  expect_identical(tab["halves", "p-value"], s$halves$p.value)
  expect_identical(summary(s)$notes, character())
})

test_that("coef() of a summary returns its table", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars)
  expect_identical(coef(summary(s)), summary(s)$tests)
  skip_if_not_installed("dplyr")
  g <- suite_run(mpg ~ wt, dplyr::group_by(mtcars, am), tests = "koenker")
  expect_identical(coef(summary(g)), summary(g)$tests)
})

test_that("the call is shown unless runHeteroTests() fitted the model itself", {
  suite_quiet()
  # Fits of the caller's, from a formula and data held in variables, whatever
  # the names: the runner records what it fits itself, and reads no call.
  formula <- mpg ~ wt + qsec
  data <- mtcars
  fit <- lm(formula, data)
  s <- suite_run(fit, data)
  expect_null(attr(s, "model_origin"))
  expect_identical(summary(s)$call, fit$call)
  model <- formula
  df <- mtcars
  own <- stats::lm(formula = model, data = df)
  expect_identical(summary(suite_run(own, df))$call, own$call)
  # The runner's own fit of a formula.
  sf <- suite_run(formula, data)
  expect_identical(attr(sf, "model_origin"), "formula")
  expect_null(summary(sf)$call)
  expect_identical(deparse(summary(sf)$formula), "mpg ~ wt + qsec")
  skip_if_not_installed("dplyr")
  # The refits within the groups, of a formula and of a fitted model.
  by_am <- dplyr::group_by(mtcars, am)
  for (g in list(suite_run(mpg ~ wt, by_am, tests = "koenker"),
                 suite_run(lm(mpg ~ wt, data = mtcars), by_am, tests = "koenker"))) {
    expect_identical(vapply(unclass(g), attr, "", "model_origin"), c("group", "group"))
    expect_true(all(vapply(summary(g)$groups, function(x) is.null(x$call), logical(1))))
    expect_identical(deparse(summary(g)$formula), "mpg ~ wt")
  }
})

test_that("a suite of the data of a survey design shows the formula", {
  skip_if_not_installed("survey")
  suite_quiet()
  # Equal probabilities, which svydesign() warns that it assumes.
  design <- suppressWarnings(survey::svydesign(ids = ~1, data = mtcars))
  s <- suppressWarnings(runSurveyHeteroTests(mpg ~ wt + qsec, design, tests = "koenker"))
  expect_identical(attr(s, "model_origin"), "survey")
  expect_null(summary(s)$call)
  expect_identical(deparse(summary(s)$formula), "mpg ~ wt + qsec")
})

test_that("a long heading is wrapped to the width of the console", {
  suite_quiet()
  withr::local_options(width = 60)
  d <- mtcars
  m <- lm(mpg ~ wt + qsec + hp + disp + drat + am + gear + carb, data = d)
  out <- utils::capture.output(print(suite_run(m, d, tests = "koenker")))
  expect_true(all(nchar(out) <= 60L))
  expect_match(out[[1L]], "^Heteroscedasticity tests of lm\\(")
})

test_that("weights the call does not record are given by their range", {
  suite_quiet()
  m <- lm(mpg ~ wt + qsec, data = mtcars, weights = 1 / hp)
  m$call$weights <- NULL
  expect_identical(
    heteroTests:::rsuite_weights(m),
    "prior weights not in the call, from 0.00299 to 0.0192"
  )
})

test_that("arguments given to print() replace those passed to printCoefmat()", {
  suite_quiet()
  fit <- suppressWarnings(glm(am ~ wt, family = binomial, data = mtcars))
  failed <- suite_failed(fit, mtcars, c("white", "koenker"))
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars)
  expect_snapshot({
    print(failed, na.print = "-")
    print(s, P.values = FALSE)
  })
})

test_that("a refusal of the model is told from a failure", {
  suite_quiet()
  fit <- suppressWarnings(glm(am ~ wt, family = binomial, data = mtcars))
  notes <- summary(suite_failed(fit, mtcars, "white"))$notes
  expect_match(notes, "^white refused: performWhiteTest\\(\\) needs a model fitted by")
  # A test that stops in its computation failed, with the kind of failure.
  suite_local_diagnostic("broken", function(model, data) stop("the design is singular"))
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  notes <- summary(suite_failed(m, mtcars, "broken"))$notes
  expect_match(notes, "^broken failed \\(rank_deficiency\\): the design is singular")
})

test_that("hand-built and edge suites print without error", {
  hand <- function(statistic, parameter, p.value) {
    structure(list(statistic = statistic, parameter = parameter, p.value = p.value,
                   method = "A test", data.name = "x"), class = "htest")
  }
  s <- structure(
    list(
      missing = hand(c(X = NA_real_), c(df = 2), NA_real_),
      infinite = hand(c(X = Inf), c(df = 1), 0),
      no_p = structure(list(statistic = c(t = 1.5), method = "A test", data.name = "x"),
                       class = "htest"),
      a_list = list(statistic = 3),
      text = "not a test",
      nothing = NULL
    ),
    class = c("hetero_test_suite", "list")
  )
  expect_length(s, 6L)
  sm <- summary(s)
  tab <- coef(sm)
  expect_identical(dim(tab), c(6L, 3L))
  expect_identical(unname(tab[, "Statistic"]), c(NA, Inf, 1.5, 3, NA, NA))
  expect_identical(unname(tab[, "df"]), c(2, 1, NA, NA, NA, NA))
  expect_identical(unname(tab[, "p-value"]), c(NA, 0, NA, NA, NA, NA))
  expect_identical(unname(sm$status), c("ok", "ok", "ok", "other", "other", "other"))
  expect_length(sm$notes, 3L)
  expect_match(sm$notes[[3L]], "^nothing returned an object of class \"NULL\"")
  expect_null(sm$call)
  expect_identical(sm$nobs, NA_integer_)
  for (x in list(s, sm)) {
    out <- utils::capture.output(print(x))
    expect_gt(length(out), 0L)
  }
  expect_match(utils::capture.output(print(s))[[1L]], "^Heteroscedasticity tests$")
})

test_that("the fallback on White's test without cross products is named", {
  suite_quiet()
  registry <- heteroTests:::.diagnostic_registry
  original <- get("white", envir = registry)
  withr::defer(heteroTests:::rregister_package_diagnostic("white", original))
  # White's test with its cross products stops; the runner falls back on its
  # form without them, a function of the package, under the name "white".
  registerDiagnostic("white", function(model, data) stop("the cross products failed"))
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  s <- suite_run(m, mtcars, tests = "white")
  expect_identical(attr(s$white, "extras")$fallback_strategy, "white_simplified")
  expect_identical(s$white$data.name, "mpg ~ wt + qsec")
  sm <- summary(s)
  expect_identical(unname(sm$status), "fallback")
  expect_identical(unname(sm$diagnostic), "white")
  expect_identical(
    sm$notes,
    paste0("white failed, and \"white_simplified\" ran in its place (White's test without ",
           "cross products). The failure: the cross products failed")
  )
  expect_identical(
    unname(coef(sm)["white", "Statistic"]),
    unname(suppressWarnings(performWhiteTest(m, mtcars, cross_products = FALSE))$statistic)
  )
})
