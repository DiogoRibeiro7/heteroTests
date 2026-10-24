# The warning of White's, Koenker's and the Breusch-Pagan tests on a factor
# with more levels than observations per level, fixed effects for small
# groups (rwarn_small_groups(), R/model_context.R), decided by the author on
# 2026-10-23 from inst/validation/fixed-effects-size.R: the values stay, with
# a warning of class heteroTests_fixed_effects_warning, given on every call,
# by the caches too, and once for each test through the runners.

fe_class <- "heteroTests_fixed_effects_warning"

# G subjects of T observations, lm(y ~ x + subject) on homoscedastic data.
fe_fixture <- function(G, T, seed = 1) {
  set.seed(seed)
  g <- rep(seq_len(G), each = T)
  d <- data.frame(x = stats::rnorm(G * T), subject = factor(g))
  d$y <- 1 + 0.5 * d$x + stats::rnorm(G)[g] + stats::rnorm(G * T)
  d
}

# The value of `expr` and the warnings of the class it gave, in order. The
# other warnings and the messages are muffled.
fe_capture <- function(expr) {
  caught <- list()
  value <- withCallingHandlers(
    withCallingHandlers(
      suppressMessages(expr),
      heteroTests_fixed_effects_warning = function(w) {
        caught[[length(caught) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    warning = function(w) invokeRestart("muffleWarning")
  )
  list(value = value, warnings = caught)
}

fe_reset <- function() {
  clearTestCache()
  clearAnalysisCache()
  heteroTests:::clearValidationCache()
}

fe_tests <- list(
  performWhiteTest = function(m, d) performWhiteTest(m, d),
  performKoenkerTest = function(m, d) performKoenkerTest(m, d),
  performBPTest = function(m, d) performBPTest(m, d)
)

fe_labels <- c(
  performWhiteTest = "White's test", performKoenkerTest = "Koenker's test",
  performBPTest = "the Breusch-Pagan test"
)

test_that("a factor with as many levels as observations per level gives no warning", {
  # 10 subjects of 10: L^2 = n = 100.
  d <- fe_fixture(10, 10)
  m <- lm(y ~ x + subject, data = d)
  for (fn in names(fe_tests)) {
    expect_no_warning(fe_tests[[fn]](m, d), class = fe_class)
    expect_no_warning(fe_tests[[fn]](m, NULL), class = fe_class)
  }
})

test_that("a factor with more levels than observations per level warns, with the numbers", {
  # One observation fewer: L^2 = 100 > n = 99.
  d <- fe_fixture(10, 10)[-1, ]
  m <- lm(y ~ x + subject, data = d)
  for (fn in names(fe_tests)) {
    out <- fe_capture(fe_tests[[fn]](m, d))
    expect_length(out$warnings, 1L)
    w <- out$warnings[[1L]]
    expect_s3_class(w, fe_class)
    expect_s3_class(w, "warning")
    expect_null(conditionCall(w))
    expect_identical(w$factors, "subject")
    expect_identical(w$levels, 10L)
    expect_identical(w$observations, 99L)
    expect_null(w$main_effect)
    expect_identical(
      conditionMessage(w),
      paste0(
        "The term `subject` has 10 levels for 99 observations, 9.9 per level: with fixed ",
        "effects for small groups, more levels than observations per level, the p-value of ",
        fe_labels[[fn]], " is unreliable; in simulations it was usually far too small, and too ",
        "large when the factor enters only as a slope per level (see 'Fixed effects for small ",
        "groups' in ?", fn, ")."
      )
    )
    # The value is the one the test computes without the warning.
    muffled <- suppressWarnings(fe_tests[[fn]](m, d), classes = fe_class)
    expect_identical(out$value, muffled)
  }
})

test_that("the levels are counted among the observations of the fit", {
  # 10 subjects of 10, the response of the first missing: the fit has 90
  # observations of 9 subjects, L^2 = 81, and no warning, although the
  # factor has 10 levels in the data.
  d <- fe_fixture(10, 10)
  d$y[d$subject == "1"] <- NA
  m <- lm(y ~ x + subject, data = d)
  expect_identical(length(m$residuals), 90L)
  # The rows with a missing response give the warning about missing values,
  # which fe_capture() muffles.
  for (fn in names(fe_tests)) {
    expect_length(fe_capture(fe_tests[[fn]](m, d))$warnings, 0L)
    expect_length(fe_capture(fe_tests[[fn]](m, NULL))$warnings, 0L)
  }
  # Nine observations of the second subject fewer: 81 observations of 9
  # subjects, L^2 = n, and no warning; one of the third fewer, 80, warns.
  d81 <- d[-which(d$subject == "2")[1:9], ]
  m81 <- lm(y ~ x + subject, data = d81)
  expect_identical(length(m81$residuals), 81L)
  expect_length(fe_capture(performKoenkerTest(m81, d81))$warnings, 0L)
  expect_length(fe_capture(performBPTest(m81, d81))$warnings, 0L)
  d80 <- d81[-which(d81$subject == "3")[1], ]
  m80 <- lm(y ~ x + subject, data = d80)
  out <- fe_capture(performKoenkerTest(m80, d80))
  expect_length(out$warnings, 1L)
  expect_identical(out$warnings[[1L]]$levels, 9L)
  expect_identical(out$warnings[[1L]]$observations, 80L)
})

test_that("a slope for each level counts, as a character vector does", {
  # 30 subjects of 3, n = 90: L^2 = 900. The tests rejected too rarely there
  # (fixed-effects-variants-size.R), and the term x:subject is named.
  d <- fe_fixture(30, 3)
  slopes <- lm(y ~ x + x:subject, data = d)
  for (fn in names(fe_tests)) {
    out <- fe_capture(fe_tests[[fn]](slopes, d))
    expect_length(out$warnings, 1L)
    expect_identical(out$warnings[[1L]]$factors, "x:subject")
    expect_identical(out$warnings[[1L]]$levels, 30L)
    expect_match(conditionMessage(out$warnings[[1L]]), "The term `x:subject` has 30 levels",
                 fixed = TRUE)
  }
  d$subject <- as.character(d$subject)
  m <- lm(y ~ x + subject, data = d)
  out <- fe_capture(performKoenkerTest(m, d))
  expect_length(out$warnings, 1L)
  expect_identical(out$warnings[[1L]]$factors, "subject")
  # Two terms over the rule are named in one warning.
  d$wave <- factor(rep(seq_len(30), times = 3))
  two <- lm(y ~ x + subject + wave, data = d)
  out <- fe_capture(performKoenkerTest(two, d))
  expect_length(out$warnings, 1L)
  expect_identical(out$warnings[[1L]]$factors, c("subject", "wave"))
  expect_match(
    conditionMessage(out$warnings[[1L]]),
    "The terms `subject` and `wave` have 30 and 30 levels for 90 observations, 3 and 3 per level",
    fixed = TRUE
  )
})

test_that("the terms of the model count, not its variables", {
  # A variable taken out of the formula does not count: 120 row labels.
  set.seed(2)
  d <- data.frame(id = sprintf("r%03d", 1:120), x = stats::rnorm(120), z = stats::rnorm(120),
                  stringsAsFactors = FALSE)
  d$y <- 1 + d$x + stats::rnorm(120)
  dropped <- lm(y ~ . - id, data = d)
  d$g <- factor(rep(1:60, 2))
  removed <- lm(y ~ x + z + g - g, data = d)
  for (fn in names(fe_tests)) {
    expect_length(fe_capture(fe_tests[[fn]](dropped, d[, c("id", "x", "z", "y")]))$warnings, 0L)
    expect_length(fe_capture(fe_tests[[fn]](removed, d))$warnings, 0L)
  }
  # A crossed a * b of 10 by 10 levels, two observations a cell: a and b have
  # 10 levels each for 200 observations, the term a:b 100 cells.
  crossed <- expand.grid(a = factor(1:10), b = factor(1:10), r = 1:2)
  crossed$x <- stats::rnorm(200)
  crossed$y <- crossed$x + stats::rnorm(200)
  m <- lm(y ~ x + a * b, data = crossed)
  for (fn in c("performKoenkerTest", "performBPTest")) {
    out <- fe_capture(fe_tests[[fn]](m, crossed))
    expect_length(out$warnings, 1L)
    expect_identical(out$warnings[[1L]]$factors, "a:b")
    expect_identical(out$warnings[[1L]]$levels, 100L)
  }
  # A factor-by-factor term without its main effects: 30 cells of 4.
  ab <- expand.grid(a = factor(1:15), b = factor(1:2), r = 1:4)
  ab$x <- stats::rnorm(120)
  ab$y <- ab$x + stats::rnorm(120)
  out <- fe_capture(performKoenkerTest(lm(y ~ x + a:b, data = ab), ab))
  expect_identical(out$warnings[[1L]]$factors, "a:b")
  expect_identical(out$warnings[[1L]]$levels, 30L)
  # Nested classes: 5 schools of 40 classes of 3, the term school:class.
  nested <- expand.grid(r = 1:3, class = factor(1:40), school = factor(1:5))
  nested$x <- stats::rnorm(600)
  nested$y <- nested$x + stats::rnorm(600)
  out <- fe_capture(performKoenkerTest(lm(y ~ x + school / class, data = nested), nested))
  expect_identical(out$warnings[[1L]]$factors, "school:class")
  expect_identical(out$warnings[[1L]]$levels, 200L)
  # interaction(a, b) is a variable of its own.
  out <- fe_capture(performKoenkerTest(lm(y ~ x + interaction(a, b), data = ab), ab))
  expect_identical(out$warnings[[1L]]$factors, "interaction(a, b)")
  expect_identical(out$warnings[[1L]]$levels, 30L)
})

test_that("a name that needs backticks is matched by position", {
  d <- fe_fixture(30, 4)
  names(d)[names(d) == "subject"] <- "my subject"
  m <- lm(y ~ x + `my subject`, data = d)
  out <- fe_capture(performKoenkerTest(m, d))
  expect_length(out$warnings, 1L)
  expect_identical(out$warnings[[1L]]$factors, "`my subject`")
  expect_match(conditionMessage(out$warnings[[1L]]), "The term `my subject` has 30 levels",
               fixed = TRUE)
  skip_if_not_installed("tibble")
  tb <- tibble::as_tibble(d)
  out <- fe_capture(performKoenkerTest(lm(y ~ x + `my subject`, data = tb), tb))
  expect_identical(out$warnings[[1L]]$factors, "`my subject`")
})

test_that("designs within the rule give no warning", {
  # 4 subjects of 30, 20 of 30 and 10 of 10: L^2 <= n.
  for (gt in list(c(4, 30), c(20, 30), c(10, 10))) {
    d <- fe_fixture(gt[1], gt[2])
    m <- lm(y ~ x + subject, data = d)
    for (fn in names(fe_tests)) {
      expect_length(fe_capture(fe_tests[[fn]](m, d))$warnings, 0L)
    }
  }
})

test_that("the warning is muffled by its class, and no other warning with it", {
  d <- fe_fixture(30, 4)
  m <- lm(y ~ x + subject, data = d)
  expect_no_warning(suppressWarnings(performKoenkerTest(m, d), classes = fe_class))
  # The values are those of the auxiliary regression written out.
  r <- suppressWarnings(performKoenkerTest(m, d), classes = fe_class)
  e2 <- stats::residuals(m)^2
  aux <- stats::lm(e2 ~ x + subject, data = d)
  expect_equal(unname(r$statistic), nrow(d) * summary(aux)$r.squared, tolerance = 1e-10)
  expect_equal(unname(r$parameter), 30)
  # A warning of another kind is still given.
  d$x[1] <- NA
  m_na <- lm(y ~ x + subject, data = d)
  expect_warning(
    suppressWarnings(performKoenkerTest(m_na, d), classes = fe_class),
    "Removed 1 observations due to missing values"
  )
})

test_that("the robust, streaming and studentized forms warn once, the bootstraps do not", {
  d <- fe_fixture(30, 4)
  m <- lm(y ~ x + subject, data = d)
  once <- list(
    quote(performWhiteTestRobust(m, d)),
    quote(performWhiteTestRobust(m, d, method = "reduced")),
    quote(performWhiteTestRobust(m, d, bootstrap = TRUE, B = 19)),
    quote(performBPTestRobust(m, d)),
    quote(performBPTestRobust(m, d, studentize = FALSE)),
    quote(performBPTestRobust(m, d, bootstrap = TRUE, B = 19)),
    quote(performBPTestStreaming(m, d, progress = FALSE)),
    quote(performKoenkerTestStreaming(m, d, progress = FALSE)),
    quote(performStudentizedBPTest(m, d)),
    quote(performBreuschPaganTest(m, d))
  )
  for (call in once) {
    set.seed(3)
    out <- fe_capture(eval(call))
    expect_length(out$warnings, 1L)
  }
  none <- list(
    quote(performWhiteTestBootstrap(m, d, B = 19)),
    quote(performWildBootstrapTest(m, d, B = 19, progress = FALSE)),
    quote(bootstrapTestStatistic(performKoenkerTest, m, d, B = 19, progress = FALSE))
  )
  for (call in none) {
    set.seed(3)
    out <- fe_capture(eval(call))
    expect_length(out$warnings, 0L)
  }
  # The streaming White test refuses a model with a factor, and warns on none.
  expect_error(fe_capture(performWhiteTestStreaming(m, d, progress = FALSE)), "rank deficient")
})

test_that("a result answered from either cache gives the warning again", {
  skip_if_not_installed("digest")
  fe_reset()
  on.exit(fe_reset(), add = TRUE)
  d <- fe_fixture(30, 4)
  m <- lm(y ~ x + subject, data = d)
  test_cache <- heteroTests:::.test_cache
  analysis_cache <- heteroTests:::.analysis_cache

  # cachedTest(): the second call is answered from its cache.
  first <- fe_capture(cachedTest("koenker", m, d))
  expect_identical(heteroTests:::rcache_size(test_cache), 1L)
  second <- fe_capture(cachedTest("koenker", m, d))
  expect_identical(heteroTests:::rcache_size(test_cache), 1L)
  expect_identical(second$value, first$value)
  expect_length(first$warnings, 1L)
  expect_identical(second$warnings, first$warnings)

  # runHeteroTests(): the cache of whole calls. The cache of cachedTest() is
  # emptied after the first call, and stays empty, so the second call is
  # answered from the cache of whole calls.
  fe_reset()
  fresh <- fe_capture(runHeteroTests(m, d, progress = FALSE))
  expect_identical(heteroTests:::rcache_size(analysis_cache), 1L)
  clearTestCache()
  cached <- fe_capture(runHeteroTests(m, d, progress = FALSE))
  expect_identical(heteroTests:::rcache_size(test_cache), 0L)
  expect_identical(cached$value, fresh$value)
  expect_length(fresh$warnings, 2L)
  expect_identical(cached$warnings, fresh$warnings)
  expect_match(conditionMessage(cached$warnings[[1L]]), "White's test", fixed = TRUE)
  expect_match(conditionMessage(cached$warnings[[2L]]), "Koenker's test", fixed = TRUE)

  # A test of the runner answered from the cache of cachedTest() warns once.
  fe_reset()
  invisible(fe_capture(cachedTest("white", m, d)))
  mixed <- fe_capture(runHeteroTests(m, d, progress = FALSE))
  expect_length(mixed$warnings, 2L)
  expect_identical(mixed$value, fresh$value)

  # With the caches off, every call computes and warns.
  withr::local_options(heteroTests.cache_size = 0)
  off <- fe_capture(cachedTest("koenker", m, d))
  expect_identical(off$warnings, first$warnings)
})

test_that("runHeteroTests() and runDiagnostics() give the warning once for each test", {
  fe_reset()
  on.exit(fe_reset(), add = TRUE)
  d <- fe_fixture(30, 4)
  m <- lm(y ~ x + subject, data = d)
  tests <- c("white", "koenker", "breusch_pagan")
  for (use_cache in c(FALSE, TRUE, TRUE)) {
    out <- fe_capture(runHeteroTests(m, d, tests = tests, use_cache = use_cache, progress = FALSE))
    expect_length(out$warnings, 3L)
    messages <- vapply(out$warnings, conditionMessage, character(1))
    expect_match(messages[1], "White's test", fixed = TRUE)
    expect_match(messages[2], "Koenker's test", fixed = TRUE)
    expect_match(messages[3], "the Breusch-Pagan test", fixed = TRUE)
  }
  diagnostics <- fe_capture(runDiagnostics(m, d, use_cache = FALSE, progress = FALSE))
  expect_length(diagnostics$warnings, 2L)
  # A formula and its data: the runner fits the model.
  from_formula <- fe_capture(runHeteroTests(y ~ x + subject, d, use_cache = FALSE,
                                            progress = FALSE))
  expect_length(from_formula$warnings, 2L)

  # White's test refused, and replaced by its fallback without cross
  # products: one warning, for the fallback. Eight subjects of three and two
  # regressors have 27 independent terms with the products, for 24
  # observations.
  small <- fe_fixture(8, 3)
  small$z <- stats::rnorm(nrow(small))
  fit <- lm(y ~ x + z + subject, data = small)
  expect_error(performWhiteTest(fit, small), class = "heteroTests_white_exact_fit")
  out <- fe_capture(runHeteroTests(fit, small, tests = "white", use_cache = FALSE,
                                   progress = FALSE))
  expect_identical(attr(out$value$white, "extras")$fallback_strategy, "white_simplified")
  expect_length(out$warnings, 1L)
  expect_match(conditionMessage(out$warnings[[1L]]), "White's test", fixed = TRUE)
})

test_that("under options(warn = 2) the runners store the real values and take no fallback", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  fe_reset()
  on.exit(fe_reset(), add = TRUE)
  d <- fe_fixture(30, 4)
  m <- lm(y ~ x + subject, data = d)
  tests <- c("koenker", "breusch_pagan")
  reference <- list(
    koenker = suppressWarnings(performKoenkerTest(m, d), classes = fe_class),
    breusch_pagan = suppressWarnings(performBPTest(m, d), classes = fe_class)
  )
  # The warning becomes an error of the call, given once the suite is stored.
  withr::with_options(list(warn = 2), {
    expect_error(runHeteroTests(m, d, tests = tests, progress = FALSE), "converted from warning")
  })
  analysis_cache <- heteroTests:::.analysis_cache
  expect_identical(heteroTests:::rcache_size(analysis_cache), 1L)
  # A later call is answered from the cache, with the values of the tests,
  # not those of a fallback, and the two warnings.
  out <- fe_capture(runHeteroTests(m, d, tests = tests, progress = FALSE))
  for (t in tests) {
    expect_identical(unname(out$value[[t]]$statistic), unname(reference[[t]]$statistic))
    expect_identical(out$value[[t]]$method, reference[[t]]$method)
    expect_null(attr(out$value[[t]], "extras")$fallback_strategy)
  }
  expect_length(out$warnings, 2L)
  # cachedTest() and runDiagnostics() store before they give the warning.
  fe_reset()
  withr::with_options(list(warn = 2), {
    expect_error(cachedTest("koenker", m, d), "converted from warning")
    expect_error(runDiagnostics(m, d, tests = tests, progress = FALSE), "converted from warning")
  })
  stored <- fe_capture(cachedTest("koenker", m, d))
  expect_identical(unname(stored$value$statistic), unname(reference$koenker$statistic))
  expect_length(stored$warnings, 1L)
  # Grouped data: every group runs before the warnings are given.
  skip_if_not_installed("dplyr")
  two <- rbind(transform(fe_fixture(15, 4, seed = 3), region = 1),
               transform(fe_fixture(15, 4, seed = 4), region = 2))
  grouped <- fe_capture(runHeteroTests(y ~ x + subject, dplyr::group_by(two, region),
                                       tests = tests, progress = FALSE))
  expect_length(grouped$warnings, 4L)
  expect_length(grouped$value, 2L)
})

test_that("autoTransform() warns once", {
  d <- fe_fixture(30, 4)
  d$y <- exp(d$y / 4)
  m <- lm(y ~ x + subject, data = d)
  out <- fe_capture(autoTransform(m))
  expect_length(out$warnings, 1L)
  expect_identical(out$warnings[[1L]]$factors, "subject")
  expect_match(conditionMessage(out$warnings[[1L]]), "the Breusch-Pagan test", fixed = TRUE)
})
