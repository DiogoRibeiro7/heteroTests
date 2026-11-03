# An auxiliary regression of the regressors that the design forces to fit the
# squared residuals exactly: since 0.18.0 Koenker's and the Breusch-Pagan
# tests refuse it, as White's test has since 0.16.0, by the rule of White's
# test (rauxiliary_exact(), R/performWhiteTest.R), and so do the other tests
# whose auxiliary regression can fit the squared residuals, or a function of
# them, exactly. Up to 0.17.0 they returned a statistic of the spread of the
# squared residuals, or the number of observations. A fit the data make
# exact, which the design does not force, is tested (rforced_exact_fit()).

ef_quiet <- function(expr) suppressMessages(suppressWarnings(expr))
ef_error <- function(expr) tryCatch(ef_quiet(expr), error = function(e) e)
ef_class <- "heteroTests_exact_fit_error"

# The paired design of White's exact fit: 30 subjects of two observations.
ef_paired <- function(G = 30, seed = 30) {
  set.seed(seed)
  d <- data.frame(subject = factor(rep(seq_len(G), each = 2)), treat = rep(0:1, G))
  d$y <- 2 + 0.5 * d$treat + stats::rnorm(G)[as.integer(d$subject)] + stats::rnorm(2 * G)
  d
}

# A factor of 10 levels of two observations beside x1, 20 observations.
ef_x1_f <- function() {
  set.seed(9)
  d <- data.frame(x1 = stats::rnorm(20), f = factor(rep(letters[1:10], 2)))
  d$y <- stats::rnorm(20)
  d
}

# A one-way layout of nine groups of ten observations and one of two, with a
# value of the group of two typed 100 times too large: its two residuals,
# +-2336 against a standard deviation of 4.3 for the others, are equal and
# opposite, so the dummies fit the squared residuals to within 1.1e-9 of their
# total, an exact fit by the rule, which the data make and the design does
# not force (from the review of #184).
ef_outlier <- function() {
  set.seed(1)
  d <- data.frame(g = factor(c(rep(1:9, each = 10), 10, 10)))
  d$y <- stats::rnorm(92, mean = 50, sd = 5)
  d$y[91] <- d$y[91] * 100
  d
}

ef_forced <- paste(
  "The design forces the exact fit, as a factor with two observations per level does,",
  "however many levels it has; use more observations per level."
)

test_that("Koenker's and the Breusch-Pagan tests refuse the paired design", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  # 60 on 30 degrees of freedom (p = 0.0009) and 63.25 on 30 (p = 0.0004)
  # up to 0.17.0, the values of lmtest::bptest().
  koenker <- ef_error(performKoenkerTest(m, d))
  expect_s3_class(koenker, ef_class)
  expect_false(inherits(koenker, "heteroTests_white_exact_fit"))
  expect_null(conditionCall(koenker))
  expect_true(koenker$regressors_exact)
  expect_identical(
    conditionMessage(koenker),
    paste(
      "The auxiliary regression of Koenker's test fits the squared residuals exactly: the squared",
      "residuals are a combination of its 31 independent terms with its intercept, for 60",
      "observations. Its R-squared is 1 and its statistic the number of observations, whatever",
      "the data.", ef_forced
    )
  )
  bp <- ef_error(performBPTest(m, d))
  expect_s3_class(bp, ef_class)
  expect_true(bp$regressors_exact)
  expect_identical(
    conditionMessage(bp),
    paste(
      "The auxiliary regression of the Breusch-Pagan test fits the squared residuals exactly: the",
      "squared residuals are a combination of its 31 independent terms with its intercept, for 60",
      "observations. Its R-squared is 1 and its statistic a function of the spread of the squared",
      "residuals alone, whatever their pattern.", ef_forced
    )
  )
  # Without `data`, from the model frame, the same refusal.
  expect_identical(conditionMessage(ef_error(performKoenkerTest(m))), conditionMessage(koenker))
  expect_identical(conditionMessage(ef_error(performBPTest(m))), conditionMessage(bp))
  # The streaming forms, in chunks of any size, refuse it with the same message.
  for (chunk in c(7, 10000)) {
    streamed <- ef_error(performKoenkerTestStreaming(m, d, chunk_size = chunk, progress = FALSE))
    expect_s3_class(streamed, ef_class)
    expect_identical(conditionMessage(streamed), conditionMessage(koenker))
    streamed <- ef_error(performBPTestStreaming(m, d, chunk_size = chunk, progress = FALSE))
    expect_s3_class(streamed, ef_class)
    expect_identical(conditionMessage(streamed), conditionMessage(bp))
  }
  # The refusal comes before the warning on fixed effects for small groups,
  # which the value would have carried, as White's refusal does.
  expect_no_warning(
    tryCatch(performKoenkerTest(m, d), error = function(e) NULL),
    class = "heteroTests_fixed_effects_warning"
  )
  # 500 subjects of two observations are refused as 30 are.
  d500 <- ef_paired(500, seed = 500)
  m500 <- lm(y ~ treat + subject, data = d500)
  err <- ef_error(performKoenkerTest(m500, d500))
  expect_s3_class(err, ef_class)
  expect_match(conditionMessage(err), "of its 501 independent terms with its intercept, for 1000",
               fixed = TRUE)
})

test_that("Koenker's and the Breusch-Pagan tests refuse a factor of two observations a level", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # 20 on 10 (p = 0.029) and 28.03 on 10 (p = 0.002) up to 0.17.0; 0.17.0
  # gave Koenker's with the warning of summary.lm(), "essentially perfect
  # fit", which #183 lost.
  d <- ef_x1_f()
  m <- lm(y ~ x1 + f, data = d)
  for (call in list(quote(performKoenkerTest(m, d)), quote(performBPTest(m, d)),
                    quote(performKoenkerTestStreaming(m, d, progress = FALSE)),
                    quote(performBPTestStreaming(m, d, progress = FALSE)))) {
    err <- ef_error(eval(call))
    expect_s3_class(err, ef_class)
    expect_match(conditionMessage(err), "of its 11 independent terms with its intercept, for 20",
                 fixed = TRUE)
    expect_match(conditionMessage(err), ef_forced, fixed = TRUE)
  }
})

test_that("a fit the data make exact is tested, as up to 0.17.0", {
  skip_if_not_installed("lmtest")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_outlier()
  m <- lm(y ~ g, data = d)
  e2 <- stats::residuals(m)^2
  aux <- stats::lm.fit(cbind(1, stats::model.matrix(m)[, -1]), e2)
  expect_lt(sum(aux$residuals^2) / sum((e2 - mean(e2))^2), sqrt(.Machine$double.eps))
  # A first version of #184 refused it, saying that the design forced it;
  # 0.17.0 gave these values, and White's test refused it from 0.16.0.
  koenker <- ef_quiet(performKoenkerTest(m, d))
  bp <- ef_quiet(performBPTest(m, d))
  expect_equal(unname(koenker$statistic), unname(lmtest::bptest(m)$statistic),
               tolerance = 1e-8)
  expect_equal(unname(bp$statistic), unname(lmtest::bptest(m, studentize = FALSE)$statistic),
               tolerance = 1e-8)
  expect_equal(unname(koenker$statistic), 92, tolerance = 1e-6)
  expect_identical(unname(koenker$parameter), 9L)
  expect_identical(unname(bp$parameter), 9L)
  for (chunk in c(7, 10000)) {
    streamed <- ef_quiet(performKoenkerTestStreaming(m, d, chunk_size = chunk, progress = FALSE))
    expect_equal(unname(streamed$statistic), unname(koenker$statistic), tolerance = 1e-8)
    streamed <- ef_quiet(performBPTestStreaming(m, d, chunk_size = chunk, progress = FALSE))
    expect_equal(unname(streamed$statistic), unname(bp$statistic), tolerance = 1e-8)
  }
  # White's auxiliary regression holds the dummies and their squares, the
  # dummies: its statistic is Koenker's.
  white <- ef_quiet(performWhiteTest(m, d))
  expect_equal(unname(white$statistic), unname(koenker$statistic), tolerance = 1e-8)
  expect_identical(unname(white$parameter), 9L)
  streamed <- ef_quiet(performWhiteTestStreaming(m, d, chunk_size = 7, progress = FALSE))
  expect_equal(unname(streamed$statistic), unname(koenker$statistic), tolerance = 1e-8)
  student <- ef_quiet(performStudentizedBPTest(m, d))
  expect_equal(unname(student$statistic), unname(koenker$statistic), tolerance = 1e-8)
  cw <- ef_quiet(performCookWeisbergTest(m, d, var_formula = ~ g))
  expect_equal(unname(cw$statistic), unname(bp$statistic), tolerance = 1e-8)
  set.seed(2)
  wild <- ef_quiet(performWildBootstrapTest(m, d, B = 19, progress = FALSE))
  expect_equal(unname(wild$statistic), unname(koenker$statistic), tolerance = 1e-8)
  # The runner gives the values, streamed or not, with no fallback.
  for (test in c("koenker", "breusch_pagan", "white")) {
    for (threshold in list(NULL, 0)) {
      args <- list(m, d, tests = test, use_cache = FALSE, progress = FALSE)
      if (!is.null(threshold)) args$chunk_threshold_mb <- threshold
      suite <- ef_quiet(do.call(runHeteroTests, args))
      expect_null(attr(suite[[test]], "extras")$status, info = test)
      expected <- if (test == "breusch_pagan") bp else koenker
      expect_equal(unname(suite[[test]]$statistic), unname(expected$statistic),
                   tolerance = 1e-8, info = test)
    }
  }
  # autoTransform() chooses as 0.17.0 did.
  expect_identical(ef_quiet(autoTransform(m))$method, "boxcox")
})

test_that("pairs whose squared residuals the data make linear in x are tested", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # Pairs with the same x and residuals of opposite sign, sqrt(1 + x) s: e^2
  # is 1 + x, an exact fit of the auxiliary regression of the regressor that
  # the data make: lm(y ~ x) leaves the residuals of other responses at no
  # pattern of pairs. With noise of relative size delta its residual sum of
  # squares is about 52 delta^2 of the total, so 1e-5 is exact by the rule,
  # and 1e-4 is not; all four are tested, n R^2 up to 80.
  set.seed(4)
  x <- rep(stats::runif(40, 0, 2), each = 2)
  s <- rep(c(1, -1), 40)
  for (delta in c(0, 1e-5, 1e-4, 1e-3)) {
    d <- data.frame(x = x, y = 1 + x + sqrt(1 + x) * s * (1 + delta * stats::rnorm(80)))
    m <- lm(y ~ x, data = d)
    ours <- performKoenkerTest(m, d)
    expect_lte(unname(ours$statistic), 80 + 1e-8)
    expect_gt(unname(ours$statistic), 79.9)
    streamed <- performKoenkerTestStreaming(m, d, progress = FALSE)
    expect_equal(unname(streamed$statistic), unname(ours$statistic), tolerance = 1e-8,
                 info = delta)
    bp <- performBPTest(m, d)
    streamed <- performBPTestStreaming(m, d, progress = FALSE)
    expect_equal(unname(streamed$statistic), unname(bp$statistic), tolerance = 1e-8, info = delta)
  }
})

test_that("whether the design forces the fit is decided without the random number generator", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  dout <- ef_outlier()
  mout <- lm(y ~ g, data = dout)
  # The pseudo-response is a fixed sequence of the row index.
  z <- heteroTests:::rpseudo_response(5)
  expect_equal(z, (1:5 * 0.6180339887498949) %% 1 - 0.5)
  set.seed(123)
  seed <- .Random.seed
  expect_identical(heteroTests:::rpseudo_response(60), heteroTests:::rpseudo_response(60))
  expect_true(heteroTests:::rforced_exact_fit(m$qr, function(e) TRUE))
  expect_false(heteroTests:::rforced_exact_fit(NULL, function(e) TRUE) %in% c(TRUE, FALSE))
  ef_error(performKoenkerTest(m, d))
  ef_error(performKoenkerTestStreaming(m, d, progress = FALSE))
  ef_error(performWhiteTest(m, d))
  ef_quiet(performKoenkerTest(mout, dout))
  ef_quiet(performBPTestStreaming(mout, dout, progress = FALSE))
  expect_identical(.Random.seed, seed)
  # A session without a seed is left without one.
  withr::local_seed(1)
  rm(".Random.seed", envir = globalenv())
  ef_error(performBPTest(m, d))
  ef_quiet(performBPTest(mout, dout))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("a fit without a QR decomposition is refused without saying that the design forces it", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  # lm(qr = FALSE) keeps no QR decomposition to push the pseudo-response
  # through (rcontext_qr()): the test cannot tell whether the design or the
  # data make the fit exact, and refuses it, saying only that it is exact.
  m <- lm(y ~ treat + subject, data = d, qr = FALSE)
  for (call in list(quote(performKoenkerTest(m, d)), quote(performBPTest(m, d)),
                    quote(performKoenkerTestStreaming(m, d, progress = FALSE)),
                    quote(performWhiteTest(m, d)))) {
    err <- ef_error(eval(call))
    expect_s3_class(err, ef_class)
    expect_match(conditionMessage(err), "fits the squared residuals exactly", fixed = TRUE)
    expect_no_match(conditionMessage(err), "The design forces", fixed = TRUE)
    expect_identical(err$forced, NA)
  }
  err <- ef_error(performKoenkerTest(m, d))
  expect_identical(
    conditionMessage(err),
    paste(
      "The auxiliary regression of Koenker's test fits the squared residuals exactly: the squared",
      "residuals are a combination of its 31 independent terms with its intercept, for 60",
      "observations. Its R-squared is 1 and its statistic the number of observations, whatever",
      "the data."
    )
  )
  # The refusal is still final in the runner: the regressors fit them.
  expect_true(err$regressors_exact)
  expect_true(heteroTests:::ht_final_exact_fit(err))
})

test_that("the routes to the two statistics, and the other tests of such a regression, refuse it", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  registry <- heteroTests:::.diagnostic_registry
  koenker_like <- "Its R-squared is 1 and its statistic the number of observations"
  spread <- "Its R-squared is 1 and its statistic a function of the spread of the squared"
  calls <- list(
    # 60 on 30 up to 0.17.0.
    list(quote(performBPTestRobust(m, d)), "the studentized Breusch-Pagan test", koenker_like),
    list(quote(performBPTestRobust(m, d, bootstrap = TRUE, B = 19)),
         "the studentized Breusch-Pagan test", koenker_like),
    list(quote(performStudentizedBPTest(m, d)), "the studentized Breusch-Pagan test", koenker_like),
    list(quote(get("student_bp", envir = registry)(m, d)), "the studentized Breusch-Pagan test",
         koenker_like),
    list(quote(get("koenker", envir = registry)(m, d)), "Koenker's test", koenker_like),
    list(quote(bootstrapTestStatistic(performKoenkerTest, m, d, B = 19, progress = FALSE)),
         "Koenker's test", koenker_like),
    # 60, with a p-value of 1: every replicate is an exact fit too.
    list(quote(performWildBootstrapTest(m, d, B = 19, progress = FALSE)),
         "the wild bootstrap test", koenker_like),
    list(quote(performWildBootstrapTest(m, d, B = 19, distribution = "mammen", progress = FALSE)),
         "the wild bootstrap test", koenker_like),
    # 60 on 30, with the warning of summary.lm(): every principal component.
    list(quote(performHighDimensionalTest(m, d, variance_threshold = 1)),
         "the high-dimensional test", koenker_like),
    # 63.25 on 30 up to 0.17.0.
    list(quote(performBPTestRobust(m, d, studentize = FALSE)), "the Breusch-Pagan test", spread),
    list(quote(performBreuschPaganTest(m, d)), "the Breusch-Pagan test", spread),
    list(quote(get("breusch_pagan", envir = registry)(m, d)), "the Breusch-Pagan test", spread),
    list(quote(bootstrapTestStatistic(performBPTest, m, d, B = 19, progress = FALSE)),
         "the Breusch-Pagan test", spread),
    list(quote(performCookWeisbergTest(m, d, var_formula = ~ treat + subject)),
         "the Cook-Weisberg test", spread),
    # 63.07 on 30, and an infinite F.
    list(quote(performHarveyTest(m, d)), "Harvey's test",
         "Its R-squared is 1 and its statistic a function of the spread of the log squared"),
    list(quote(performHarveyTest(m, d, studentize = TRUE)), "Harvey's test",
         "Its R-squared is 1 and its statistic infinite")
  )
  for (call in calls) {
    set.seed(1)
    err <- ef_error(eval(call[[1]]))
    label <- deparse(call[[1]])
    expect_s3_class(err, ef_class)
    expect_match(conditionMessage(err), paste("The auxiliary regression of", call[[2]]),
                 fixed = TRUE, info = label)
    expect_match(conditionMessage(err), call[[3]], fixed = TRUE, info = label)
    expect_match(conditionMessage(err), ef_forced, fixed = TRUE, info = label)
  }
  err <- ef_error(performHarveyTest(m, d))
  expect_match(conditionMessage(err), "fits the log squared residuals exactly: the log squared",
               fixed = TRUE)
  # Whether the regressors of the model fit the squared residuals is not
  # known to the Cook-Weisberg and Harvey tests.
  expect_identical(err$regressors_exact, NA)
  err <- ef_error(performCookWeisbergTest(m, d, var_formula = ~ treat + subject))
  expect_identical(err$regressors_exact, NA)
  # The variance form test: a variance regressor constant in each pair gives
  # equal weights in each pair, whose squared standardized residuals the
  # subject dummies fit exactly; F was 4.06e29 on 28 and 30.
  d$z <- rep(stats::runif(30, 1, 3), each = 2)
  m <- lm(y ~ treat + subject, data = d)
  err <- ef_error(performVarianceFormTest(m, d, var_formula = ~ z, against = ~ subject))
  expect_s3_class(err, ef_class)
  expect_match(conditionMessage(err),
               "The auxiliary regression of the variance form test fits the squared standardized",
               fixed = TRUE)
  # autoTransform() chooses by the Breusch-Pagan test, which refuses every
  # candidate fit.
  d$y <- exp(d$y / 4)
  expect_error(ef_quiet(autoTransform(lm(y ~ treat + subject, data = d))), class = ef_class)
})

test_that("the tests that do not fit the squared residuals exactly give their values", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  # The fitted values, one variance regressor: 0.608 on 1.
  cw <- performCookWeisbergTest(m, d)
  expect_equal(unname(cw$statistic), 0.6077569, tolerance = 1e-6)
  expect_identical(unname(cw$parameter), 1L)
  # Harvey's test of the fitted values and their square.
  harvey <- performHarveyTest(m, d, auxiliary = "fitted")
  expect_identical(unname(harvey$parameter), 2L)
  # The high-dimensional test keeps 26 components by default, which do not
  # fit the squared residuals exactly.
  hd <- ef_quiet(performHighDimensionalTest(m, d))
  expect_identical(unname(hd$parameter), 26L)
  expect_lt(unname(hd$statistic), 60)
})

test_that("one step away from the exact fit the tests give values", {
  skip_if_not_installed("lmtest")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  # One subject observed a third time, 61 observations.
  d61 <- rbind(d, transform(d[1, ], y = y + 1))
  m61 <- lm(y ~ treat + subject, data = d61)
  # 20 subjects of three observations.
  set.seed(3)
  d3 <- data.frame(subject = factor(rep(1:20, each = 3)), x = stats::rnorm(60))
  d3$y <- 1 + d3$x + stats::rnorm(20)[as.integer(d3$subject)] + stats::rnorm(60)
  m3 <- lm(y ~ x + subject, data = d3)
  cases <- list(
    list(m = m61, d = d61, df = 30L, koenker = 59.74929, bp = 64.73806),
    list(m = m3, d = d3, df = 20L, koenker = 31.56376, bp = 37.33361)
  )
  for (case in cases) {
    koenker <- ef_quiet(performKoenkerTest(case$m, case$d))
    bp <- ef_quiet(performBPTest(case$m, case$d))
    expect_equal(unname(koenker$statistic), case$koenker, tolerance = 1e-6)
    expect_equal(unname(bp$statistic), case$bp, tolerance = 1e-6)
    expect_identical(unname(koenker$parameter), case$df)
    expect_identical(unname(bp$parameter), case$df)
    ref <- lmtest::bptest(case$m)
    expect_equal(unname(koenker$statistic), unname(ref$statistic), tolerance = 1e-10)
    for (chunk in c(7, 10000)) {
      streamed <- ef_quiet(performKoenkerTestStreaming(case$m, case$d, chunk_size = chunk,
                                                       progress = FALSE))
      expect_equal(unname(streamed$statistic), case$koenker, tolerance = 1e-6)
      streamed <- ef_quiet(performBPTestStreaming(case$m, case$d, chunk_size = chunk,
                                                  progress = FALSE))
      expect_equal(unname(streamed$statistic), case$bp, tolerance = 1e-6)
    }
    student <- ef_quiet(performStudentizedBPTest(case$m, case$d))
    expect_equal(unname(student$statistic), case$koenker, tolerance = 1e-6)
    set.seed(1)
    wild <- ef_quiet(performWildBootstrapTest(case$m, case$d, B = 19, progress = FALSE))
    expect_equal(unname(wild$statistic), case$koenker, tolerance = 1e-6)
    expect_true(is.finite(ef_quiet(performHarveyTest(case$m, case$d))$statistic))
    expect_true(is.finite(ef_quiet(performCookWeisbergTest(case$m, case$d,
                                                           var_formula = ~ subject))$statistic))
  }
})

test_that("runHeteroTests() stops with the refusal, and takes no fallback", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  run <- function(test, ...) {
    set.seed(1)
    ef_quiet(runHeteroTests(m, d, tests = test, use_cache = FALSE, progress = FALSE, ...))
  }
  # Each returned the statistic of the exact fit up to 0.17.0: 63.25, 60,
  # 60 and 60 with a p-value of 1. Their fallbacks fit the squared residuals
  # on the same regressors, or test the fitted values under their name.
  refusals <- c(
    breusch_pagan = paste(
      "Test cannot be computed for Breusch Pagan: The auxiliary regression of the Breusch-Pagan",
      "test fits"
    ),
    koenker = paste(
      "Test cannot be computed for Koenker: The auxiliary regression of Koenker's test",
      "fits"
    ),
    student_bp = "The auxiliary regression of the studentized Breusch-Pagan test fits",
    wild_bootstrap = "The auxiliary regression of the wild bootstrap test fits"
  )
  for (test in names(refusals)) {
    expect_error(run(test), refusals[[test]], fixed = TRUE, info = test)
  }
  # Streamed, the same refusals.
  for (test in c("breusch_pagan", "koenker")) {
    expect_error(run(test, chunk_threshold_mb = 0), refusals[[test]], fixed = TRUE, info = test)
  }
  # The refusal is final: no cleaning to suggest and no fallback.
  for (fn in list(performBPTest, performKoenkerTest)) {
    err <- ef_error(fn(m, d))
    expect_true(heteroTests:::ht_final_exact_fit(err))
    analysis <- heteroTests:::ht_analyse_failure("koenker", err, m, d)
    expect_identical(analysis$issue, "exact_fit")
    expect_length(analysis$fallbacks, 0L)
    expect_length(analysis$suggestions, 0L)
  }
  # The Cook-Weisberg test of the fitted values is not an exact fit, and is
  # computed when it is asked for.
  expect_equal(unname(run("ncv")$ncv$statistic), 0.6077569, tolerance = 1e-6)
  # A fallback that refuses the exact fit ends the search: the
  # high-dimensional test, which refuses 20 observations, fell back on the
  # Breusch-Pagan test of lm(y ~ x1 + f), 28.03 on 10, under its own name.
  d10 <- ef_x1_f()
  m10 <- lm(y ~ x1 + f, data = d10)
  expect_error(
    ef_quiet(runHeteroTests(m10, d10, tests = "high_dimensional", use_cache = FALSE,
                            progress = FALSE)),
    "high_dimensional requires at least 25 observations", fixed = TRUE
  )
})

test_that("White's refusal carries the general class under its own, with its message", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- ef_paired()
  m <- lm(y ~ treat + subject, data = d)
  err <- ef_error(performWhiteTest(m, d))
  expect_identical(class(err),
                   c("heteroTests_white_exact_fit", ef_class, "error", "condition"))
  expect_identical(
    conditionMessage(err),
    paste(
      "White's auxiliary regression fits the squared residuals exactly: the squared residuals are",
      "a combination of its 31 independent terms with its intercept, for 60 observations. Its",
      "R-squared is 1 and its statistic the number of observations, whatever the data.", ef_forced
    )
  )
  # The regressors alone do not fit the squared residuals exactly on a
  # saturated design: White's refusal is not final there, and the runner
  # falls back on the test without products, as before.
  set.seed(22)
  small <- data.frame(matrix(stats::rnorm(22 * 6), 22, 6))
  names(small) <- paste0("v", 1:6)
  small$y <- stats::rnorm(22, sd = exp(small$v1 / 2))
  m6 <- lm(y ~ v1 + v2 + v3 + v4 + v5 + v6, data = small)
  err <- ef_error(performWhiteTest(m6, small))
  expect_s3_class(err, ef_class)
  expect_false(err$regressors_exact)
  expect_false(heteroTests:::ht_final_exact_fit(err))
})
