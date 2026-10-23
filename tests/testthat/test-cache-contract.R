# The contract of the caches (R/cache.R, decided by the author on
# 2026-10-21): a test that draws random numbers is never cached; a
# deterministic one is cached under a key that holds everything that
# determines its result; each cache is bounded, evicting the entry used least
# recently. Every cached result is compared with the result of the same call
# computed with no cache at all, with identical().

cache_reset <- function() {
  clearTestCache()
  clearAnalysisCache()
  heteroTests:::clearValidationCache()
}

cache_sizes <- function() {
  c(
    test = heteroTests:::rcache_size(heteroTests:::.test_cache),
    analysis = heteroTests:::rcache_size(heteroTests:::.analysis_cache)
  )
}

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

cache_statistic <- function(x) if (is.list(x)) unname(x$statistic) else NA

# The value of a call, or the message of its error.
cache_outcome <- function(expr) {
  tryCatch(quiet(expr), error = function(e) structure(conditionMessage(e), class = "cache_error"))
}

# What a call returns in a session that has cached nothing: no result, and no
# validation either.
cache_fresh <- function(test, m, d, ...) {
  cache_reset()
  cache_outcome(cachedTest(test, m, d, ..., use_cache = FALSE))
}

cache_fixture <- function(n = 120, seed = 11) {
  set.seed(seed)
  d <- data.frame(
    x = stats::runif(n, 1, 5), z = stats::rnorm(n), o = stats::runif(n, 0, 20),
    v = stats::runif(n), g = factor(rep(c("a", "b", "c"), length.out = n))
  )
  d$w <- 1 / d$x
  d$w4 <- 4 * d$w
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$o + stats::rnorm(n, sd = 0.3 + 0.4 * d$x)
  d
}

cache_with_listw <- function(d) {
  set.seed(1)
  coords <- cbind(stats::runif(nrow(d)), stats::runif(nrow(d)))
  attr(d, "listw") <- suppressWarnings(
    spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4)))
  )
  d
}

# The tests log at the INFO level; silent for the calling test.
cache_silent <- function(env = parent.frame()) {
  previous <- ht_set_log_level("SILENT")
  withr::defer(ht_set_log_level(previous), envir = env)
}

# A diagnostic flagged as the package flags its deterministic tests, for the
# duration of the calling test only.
cache_local_diagnostic <- function(name, fun, env = parent.frame()) {
  heteroTests:::rregister_package_diagnostic(name, fun)
  withr::defer(
    {
      rm(list = name, envir = heteroTests:::.diagnostic_registry)
      if (exists(name, envir = heteroTests:::.diagnostic_kind, inherits = FALSE)) {
        rm(list = name, envir = heteroTests:::.diagnostic_kind)
      }
    },
    envir = env
  )
}

# The second of two calls must give what it gives in a fresh session, and the
# two must not share a key.
expect_not_shared <- function(test, m1, d1, m2, d2, args1 = list(), args2 = args1) {
  reference <- do.call(cache_fresh, c(list(test, m2, d2), args2))
  cache_reset()
  first <- cache_outcome(do.call(cachedTest, c(list(test, m1, d1), args1)))
  second <- cache_outcome(do.call(cachedTest, c(list(test, m2, d2), args2)))
  expect_identical(second, reference)
  k1 <- heteroTests:::rcache_test_key(test, heteroTests:::rcache_inputs(m1, d1), args1)
  k2 <- heteroTests:::rcache_test_key(test, heteroTests:::rcache_inputs(m2, d2), args2)
  expect_true(is.null(k1) || is.null(k2) || !identical(k1, k2))
  invisible(list(first = first, second = second, reference = reference))
}

cache_deterministic <- c(
  "white", "breusch_pagan", "koenker", "cook_weisberg", "ncv", "spread_level", "student_bp",
  "szroeter", "quantile_regression", "high_dimensional", "variance_form"
)
cache_random <- c("white_bootstrap", "wild_bootstrap", "rank_permutation", "spatial_hetero")

test_that("every registered test is flagged, and only the deterministic ones are cached", {
  kinds <- heteroTests:::.diagnostic_kind
  names_all <- c(cache_deterministic, cache_random, "box_m")
  expect_setequal(intersect(ls(kinds), names_all), names_all)
  expect_true(all(names_all %in% listDiagnostics()))
  for (name in cache_deterministic) {
    expect_identical(get(name, envir = kinds), "deterministic", info = name)
    expect_true(heteroTests:::rcache_cacheable(name), info = name)
  }
  for (name in cache_random) {
    expect_identical(get(name, envir = kinds), "random", info = name)
    expect_false(heteroTests:::rcache_cacheable(name), info = name)
  }
  expect_identical(get("box_m", envir = kinds), "data")
  expect_false(heteroTests:::rcache_cacheable("box_m"))
  expect_false(heteroTests:::rcache_cacheable("no_such_test"))
  expect_false(heteroTests:::rcache_cacheable(NA_character_))
})

test_that("the flags agree with what each registered test draws", {
  # Every registered test at its default replications, and this test and the
  # next take most of the suite's time after the model-context matrix: they
  # run where the full matrix runs (skip_unless_full_matrix(),
  # helper-model-context.R).
  skip_unless_full_matrix()
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  skip_if_not_installed("quantreg")
  skip_if_not_installed("spdep")
  d <- cache_with_listw(cache_fixture())
  m <- lm(y ~ x + z, data = d)
  registry <- heteroTests:::.diagnostic_registry
  for (name in c(cache_deterministic, cache_random)) {
    set.seed(3)
    before <- .Random.seed
    quiet(get(name, envir = registry)(m, d))
    drew <- !identical(before, .Random.seed)
    expect_identical(drew, name %in% cache_random, info = name)
  }
})

test_that("a test that draws random numbers is never cached and leaves the generator alone", {
  # Each resampling test five times at its default replications: where the full
  # matrix runs (skip_unless_full_matrix()).
  skip_unless_full_matrix()
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  skip_if_not_installed("spdep")
  d <- cache_with_listw(cache_fixture())
  m <- lm(y ~ x + z, data = d)
  on.exit(cache_reset(), add = TRUE)
  for (name in cache_random) {
    cache_reset()
    # Without the cache: the result from seed 1, and the draw after it.
    set.seed(1)
    uncached <- quiet(cachedTest(name, m, d, use_cache = FALSE))
    after_uncached <- stats::runif(3)
    # Twice with the cache, from the same seed.
    set.seed(1)
    first <- quiet(cachedTest(name, m, d))
    after_first <- stats::runif(3)
    set.seed(1)
    second <- quiet(cachedTest(name, m, d))
    after_second <- stats::runif(3)
    expect_identical(first, uncached, info = name)
    expect_identical(second, uncached, info = name)
    expect_identical(after_first, after_uncached, info = name)
    expect_identical(after_second, after_uncached, info = name)
    # Another seed gives other replicates, as without the cache.
    set.seed(2)
    other <- quiet(cachedTest(name, m, d))
    set.seed(2)
    expect_identical(other, quiet(cachedTest(name, m, d, use_cache = FALSE)), info = name)
    expect_identical(cache_sizes(), c(test = 0L, analysis = 0L), info = name)
  }
})

test_that("runHeteroTests() runs a resampling test on every call and does not store its battery", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  on.exit(cache_reset(), add = TRUE)
  tests <- c("white", "wild_bootstrap", "koenker")
  run <- function(use_cache) {
    set.seed(5)
    out <- quiet(runHeteroTests(m, d, tests = tests, use_cache = use_cache, progress = FALSE))
    list(out = out, draw = stats::runif(2))
  }
  cache_reset()
  off <- run(FALSE)
  cache_reset()
  first <- run(TRUE)
  second <- run(TRUE)
  # The battery is the call without the cache, identical(), on a first and
  # on a second call, and so are the numbers drawn after it.
  expect_identical(first$out, off$out)
  expect_identical(second$out, off$out)
  expect_identical(first$draw, off$draw)
  expect_identical(second$draw, off$draw)
  # The two deterministic tests are cached one by one; the battery is not.
  expect_identical(cache_sizes(), c(test = 2L, analysis = 0L))

  # The parallel runner and the registry use no cache.
  cache_reset()
  set.seed(5)
  par1 <- quiet(runHeteroTestsParallel(m, d, c("white", "wild_bootstrap"), n_cores = 1))
  set.seed(5)
  par2 <- quiet(runHeteroTestsParallel(m, d, c("white", "wild_bootstrap"), n_cores = 1))
  expect_identical(par1, par2)
  expect_identical(cache_sizes(), c(test = 0L, analysis = 0L))
})

test_that("every cached result is identical() to the uncached one, on a miss and on a hit", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  skip_if_not_installed("quantreg")
  d <- cache_fixture()
  on.exit(cache_reset(), add = TRUE)
  fits <- list(
    ols = lm(y ~ x + z, data = d),
    wls = lm(y ~ x + z, data = d, weights = w),
    glm = glm(y ~ x + z, data = d)
  )
  for (fit in names(fits)) {
    m <- fits[[fit]]
    for (data in list(d, NULL)) {
      for (name in cache_deterministic) {
        info <- paste(fit, name, if (is.null(data)) "without data" else "with data")
        reference <- cache_fresh(name, m, data)
        cache_reset()
        miss <- cache_outcome(cachedTest(name, m, data))
        hit <- cache_outcome(cachedTest(name, m, data))
        expect_identical(miss, reference, info = info)
        expect_identical(hit, reference, info = info)
        if (!inherits(reference, "cache_error")) {
          expect_identical(attr(hit, "model"), m, info = info)
        }
      }
    }
  }
})

test_that("the same coefficients and residuals with another design do not share a key", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  set.seed(5)
  n <- 80
  x1 <- stats::runif(n, 1, 5)
  x2 <- sample(x1)
  e <- unname(stats::residuals(stats::lm(stats::rnorm(n) ~ x1 + x2)))
  ma <- lm(y ~ x, data = data.frame(y = 1 + 2 * x1 + e, x = x1))
  mb <- lm(y ~ x, data = data.frame(y = 1 + 2 * x2 + e, x = x2))
  # The two fits agree to rounding; the copies make them share their
  # coefficients and residuals bit for bit, as the key of 0.15.0 saw them,
  # without changing what either fits.
  expect_equal(mb$coefficients, ma$coefficients, tolerance = 1e-12)
  expect_equal(unname(mb$residuals), unname(ma$residuals), tolerance = 1e-12)
  mb$coefficients <- ma$coefficients
  mb$residuals <- ma$residuals
  for (name in c("white", "koenker", "cook_weisberg", "spread_level")) {
    out <- expect_not_shared(name, ma, NULL, mb, NULL)
    expect_false(identical(cache_statistic(out$first), cache_statistic(out$second)), info = name)
  }

  # A regressor scaled by 100, with the same coefficients: the fit is refused
  # as perfectly explained (R-squared 1.000), by the test and by the checks of
  # the validation cache, which the key of 0.15.0 let it pass on a hit.
  mc <- lm(y ~ x, data = data.frame(y = 1 + 2 * 100 * x1 + e, x = 100 * x1))
  expect_equal(mc$coefficients, ma$coefficients, tolerance = 1e-9)
  mc$coefficients <- ma$coefficients
  mc$residuals <- ma$residuals
  out <- expect_not_shared("white", ma, NULL, mc, NULL)
  expect_s3_class(out$second, "cache_error")
  cache_reset()
  quiet(performKoenkerTest(ma))
  expect_error(quiet(performKoenkerTest(mc)), "perfectly explained")
})

test_that("the same fit with other weights, offset or family does not share a key", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  tests <- c("white", "koenker", "cook_weisberg", "spread_level", "szroeter")

  # Weights scaled by 4 leave the coefficients and the residuals unchanged.
  w1 <- lm(y ~ x + z, data = d, weights = w)
  w4 <- lm(y ~ x + z, data = d, weights = w4)
  expect_equal(w4$coefficients, w1$coefficients, tolerance = 1e-12)
  for (name in tests) {
    expect_not_shared(name, w1, d, w4, d)
  }
  plain <- lm(y ~ x + z, data = d)
  for (name in tests) {
    expect_not_shared(name, plain, d, w1, d)
  }

  # An offset, and the same model of the response minus the offset: the same
  # coefficients and residuals, bit for bit, and other fitted values. On
  # 0.15.0 the Cook-Weisberg test of the second returned that of the first,
  # 0.251 (p = 0.62) in place of its own.
  with_offset <- lm(y ~ x + offset(o), data = d)
  shifted <- lm(I(y - o) ~ x, data = d)
  expect_identical(shifted$coefficients, with_offset$coefficients)
  expect_identical(shifted$residuals, with_offset$residuals)
  for (name in tests) {
    out <- expect_not_shared(name, with_offset, d, shifted, d)
    if (name %in% c("cook_weisberg", "spread_level", "szroeter")) {
      expect_false(identical(cache_statistic(out$first), cache_statistic(out$second)), info = name)
    }
  }
  # And through runHeteroTests(): the battery of the second is its own, with
  # its own model.
  cache_reset()
  fresh_suite <- quiet(runHeteroTests(shifted, d, progress = FALSE))
  cache_reset()
  quiet(runHeteroTests(with_offset, d, progress = FALSE))
  second <- quiet(runHeteroTests(shifted, d, progress = FALSE))
  expect_identical(second, fresh_suite)
  expect_identical(attr(second, "model"), shifted)

  # A Gaussian glm and the lm of the same model.
  as_glm <- glm(y ~ x + z, data = d)
  for (name in tests) {
    expect_not_shared(name, plain, d, as_glm, d)
  }
})

test_that("other test arguments, tests or chunks do not share a key", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  cache_local_diagnostic("cache_contract_white", function(model, data, ...) {
    performWhiteTest(model, data, ...)
  })
  out <- expect_not_shared(
    "cache_contract_white", m, d, m, d,
    args1 = list(cross_products = TRUE), args2 = list(cross_products = FALSE)
  )
  expect_false(identical(cache_statistic(out$first), cache_statistic(out$second)))
  # Another test on the same model and data.
  reference <- cache_fresh("koenker", m, d)
  cache_reset()
  cachedTest("white", m, d)
  expect_identical(cache_outcome(cachedTest("koenker", m, d)), reference)
  # The battery, and the chunks of the streaming tests.
  run <- function(...) quiet(runHeteroTests(m, d, progress = FALSE, ...))
  cache_reset()
  fresh_one <- run(tests = "white")
  cache_reset()
  run(tests = c("white", "koenker"))
  expect_identical(run(tests = "white"), fresh_one)
  cache_reset()
  fresh_chunks <- run(tests = "white", chunk_threshold_mb = 0, chunk_size = 30)
  cache_reset()
  run(tests = "white", chunk_threshold_mb = 0, chunk_size = 20)
  streamed <- run(tests = "white", chunk_threshold_mb = 0, chunk_size = 30)
  expect_identical(streamed, fresh_chunks)
  expect_identical(streamed$white$chunk_size, 30L)
})

test_that("a call that streams and one that does not do not share a key", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  # Since 0.16.0 the default, chunk_threshold_mb = Inf, never streams, and a
  # finite threshold asks for the streaming tests: the key of a whole call
  # holds which of the two ran.
  run <- function(...) {
    quiet(runHeteroTests(m, d, tests = c("white", "koenker"), progress = FALSE, ...))
  }
  exact <- run(use_cache = FALSE)
  streamed <- run(chunk_threshold_mb = 0, use_cache = FALSE)
  expect_false(attr(exact$white, "extras")$streaming)
  expect_true(attr(streamed$white, "extras")$streaming)
  cache_reset()
  expect_identical(run(), exact)
  expect_identical(run(chunk_threshold_mb = 0), streamed)
  expect_identical(run(), exact)
  cache_reset()
  expect_identical(run(chunk_threshold_mb = 0), streamed)
  expect_identical(run(), exact)
  expect_identical(run(chunk_threshold_mb = 0), streamed)
})

test_that("other columns of `data` that a test reads do not share a key", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  cache_local_diagnostic("cache_contract_levene", function(model, data, ...) {
    performLeveneTest(model, data, ...)
  })
  cache_local_diagnostic("cache_contract_glejser", function(model, data, ...) {
    performGlejserTest(model, data, ...)
  })
  cache_local_diagnostic("cache_contract_gq", function(model, data, ...) {
    performGQTest(model, data, ...)
  })
  set.seed(8)
  regrouped <- d
  regrouped$g <- sample(regrouped$g)
  revalued <- d
  revalued$v <- stats::runif(nrow(d))
  cases <- list(
    list("cache_contract_levene", list(group = "g"), regrouped),
    list("cache_contract_glejser", list(variable = "v"), revalued),
    list("cache_contract_gq", list(order_by = "v"), revalued)
  )
  for (case in cases) {
    out <- expect_not_shared(case[[1]], m, d, m, case[[3]], args1 = case[[2]])
    expect_false(
      identical(cache_statistic(out$first), cache_statistic(out$second)),
      info = case[[1]]
    )
  }
  # Another column named by the argument.
  d$v2 <- stats::runif(nrow(d))
  expect_not_shared("cache_contract_gq", m, d, m, d,
                    args1 = list(order_by = "v"), args2 = list(order_by = "v2"))
})

test_that("a fit with model = FALSE, or edited after the fit, is not answered from the cache", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()

  # model = FALSE: the model frame is rebuilt from the data the fit refers to.
  # Once those change, the tests refuse the fit; 0.15.0 returned the result
  # stored before the change.
  env <- new.env()
  assign("dd", d, envir = env)
  m_false <- eval(quote(lm(y ~ x + z, data = dd, model = FALSE)), env)
  cache_reset()
  before <- cache_outcome(cachedTest("white", m_false, NULL))
  expect_s3_class(before, "htest")
  env$dd$x <- rev(env$dd$x)
  after <- cache_outcome(cachedTest("white", m_false, NULL))
  expect_identical(after, cache_fresh("white", m_false, NULL))
  expect_s3_class(after, "cache_error")
  expect_match(unclass(after), "cannot use the model frame of `model`")
  # The same fit with its model frame does not share the key of one without.
  m_true <- lm(y ~ x + z, data = d)
  m_false2 <- lm(y ~ x + z, data = d, model = FALSE)
  expect_not_shared("white", m_true, d, m_false2, d)

  # Edited after the fit (M8): weights, and a coefficient.
  weighted <- lm(y ~ x + z, data = d, weights = w)
  edited <- weighted
  edited$weights <- rev(edited$weights)
  out <- expect_not_shared("koenker", weighted, d, edited, d)
  expect_match(unclass(out$second), "not those its coefficients were fitted with")
  edited <- weighted
  edited$coefficients[["x"]] <- edited$coefficients[["x"]] + 0.5
  out <- expect_not_shared("white", weighted, d, edited, d)
  expect_match(unclass(out$second), "does not reproduce its residuals")
})

test_that("an identical call hits, and so does the same model refitted in another environment", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  runs <- new.env()
  runs$n <- 0L
  cache_local_diagnostic("cache_contract_counted", function(model, data) {
    runs$n <- runs$n + 1L
    performKoenkerTest(model, data)
  })
  # The rule: a call is answered from the cache when the test, its arguments,
  # the values of the fit the key holds and the data are those of the call
  # that stored it (?cachedTest says what the key leaves out: the environment
  # of the formula and the options). The key holds values, not environments,
  # so a refit of the same formula to the same data in another environment
  # hits; the result carries the model of the call, and is identical() to the
  # one the test computes on the refit.
  fit_in <- function(data) lm(y ~ x + z, data = data)
  ma <- fit_in(d)
  mb <- fit_in(d)
  expect_false(identical(environment(formula(ma)), environment(formula(mb))))
  reference <- cache_fresh("cache_contract_counted", mb, d)
  cache_reset()
  runs$n <- 0L
  first <- cachedTest("cache_contract_counted", ma, d)
  again <- cachedTest("cache_contract_counted", ma, d)
  refit <- cachedTest("cache_contract_counted", mb, d)
  expect_identical(runs$n, 1L)
  expect_identical(again, first)
  expect_identical(refit, reference)
  expect_identical(attr(refit, "model"), mb)
  expect_identical(cache_sizes()[["test"]], 1L)

  # The key of a fit is that of its values, whatever their representation:
  # the names of the residuals of a new fit are kept as a deferred
  # as.character() of the row numbers until a test reads them.
  mc <- fit_in(d)
  before <- heteroTests:::rcache_inputs(mc, d)
  quiet(performKoenkerTest(mc, d))
  invisible(paste(names(mc$residuals), collapse = ""))
  expect_identical(heteroTests:::rcache_inputs(mc, d), before)

  # runHeteroTests(): the battery of the refit is answered from the cache.
  cache_reset()
  fresh_suite <- quiet(runHeteroTests(mb, d, progress = FALSE))
  cache_reset()
  first_suite <- quiet(runHeteroTests(ma, d, progress = FALSE))
  refit_suite <- quiet(runHeteroTests(mb, d, progress = FALSE))
  expect_identical(cache_sizes(), c(test = 2L, analysis = 1L))
  expect_identical(refit_suite, fresh_suite)
  expect_identical(attr(refit_suite, "model"), mb)
  expect_identical(attr(refit_suite$white, "model"), mb)
  # A stored result keeps no copy of the model.
  entry <- heteroTests:::.analysis_cache$values[[1]]
  expect_null(attr(entry$value, "model"))
  expect_null(attr(entry$value$white, "model"))
})

test_that("a cache holds at most heteroTests.cache_size results and evicts the least used", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  withr::local_options(heteroTests.cache_size = 3)
  runs <- new.env()
  runs$n <- 0L
  cache_local_diagnostic("cache_contract_counted", function(model, data) {
    runs$n <- runs$n + 1L
    performKoenkerTest(model, data)
  })
  fits <- lapply(1:4, function(seed) {
    d <- cache_fixture(n = 60, seed = seed)
    list(m = lm(y ~ x + z, data = d), d = d)
  })
  call <- function(k) cachedTest("cache_contract_counted", fits[[k]]$m, fits[[k]]$d)
  cache_reset()
  for (k in 1:3) call(k)
  call(1)
  expect_identical(runs$n, 3L)
  call(4)
  expect_identical(cache_sizes()[["test"]], 3L)
  # The second was used least recently, and was dropped; the first was kept.
  call(1)
  expect_identical(runs$n, 4L)
  call(2)
  expect_identical(runs$n, 5L)
  expect_identical(cache_sizes()[["test"]], 3L)

  # runHeteroTests() keeps at most three calls too.
  cache_reset()
  for (k in 1:4) {
    quiet(runHeteroTests(fits[[k]]$m, fits[[k]]$d, tests = "koenker", progress = FALSE))
  }
  expect_identical(cache_sizes(), c(test = 3L, analysis = 3L))
})

test_that("heteroTests.cache_size = 0 turns the caches off, and a wrong value is refused", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  cache_reset()
  cachedTest("white", m, d)
  expect_identical(cache_sizes()[["test"]], 1L)
  withr::with_options(list(heteroTests.cache_size = 0), {
    reference <- cache_fresh("white", m, d)
    expect_identical(cachedTest("white", m, d), reference)
    expect_identical(quiet(runHeteroTests(m, d, progress = FALSE)),
                     quiet(runHeteroTests(m, d, progress = FALSE)))
    expect_identical(cache_sizes(), c(test = 0L, analysis = 0L))
  })
  for (bad in list(-1, NA, "a", 1.5, Inf, c(1, 2))) {
    withr::with_options(list(heteroTests.cache_size = bad), {
      expect_error(cachedTest("white", m, d), "heteroTests.cache_size", fixed = TRUE)
    })
  }
})

test_that("clearTestCache() and clearAnalysisCache() empty their caches", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  cache_reset()
  quiet(runHeteroTests(m, d, progress = FALSE))
  expect_identical(cache_sizes(), c(test = 2L, analysis = 1L))
  expect_invisible(clearAnalysisCache())
  expect_identical(cache_sizes(), c(test = 2L, analysis = 0L))
  expect_invisible(clearTestCache())
  expect_identical(cache_sizes(), c(test = 0L, analysis = 0L))
  expect_null(clearTestCache())
  expect_s3_class(cachedTest("white", m, d), "htest")
})

test_that("a key leaves nothing behind once the caches are cleared", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  fit <- function(seed) {
    d <- cache_fixture(n = 40, seed = seed)
    list(m = lm(y ~ x + z, data = d), d = d)
  }
  run <- function(seeds) {
    for (s in seeds) {
      o <- fit(s)
      cachedTest("koenker", o$m, o$d)
      runHeteroTests(o$m, o$d, progress = FALSE)
    }
    cache_reset()
    invisible(gc())
  }
  run(1:5)
  before <- memory.profile()[["symbol"]]
  run(100 + 1:60)
  # Before 0.16.0 each key was the name of a variable in an environment, which
  # R never frees: three names per iteration, 180 here.
  expect_lt(memory.profile()[["symbol"]] - before, 20)
})

test_that("the validation cache keys keep NA, the order of names and the test name", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z + g, data = d)
  validate <- heteroTests:::rvalidateDataInputs
  references <- list(
    koenker = cache_fresh("koenker", m, d),
    levene = {
      cache_reset()
      cache_outcome(performLeveneTest(m, d, group = "g"))
    }
  )
  expect_s3_class(references$koenker, "htest")
  expect_s3_class(references$levene, "htest")
  # An NA name is refused; before 0.16.0 its refusal was stored under the key
  # of no names, and the calls after it on the same columns were refused with
  # "missing from input data: NA".
  cache_reset()
  expect_error(validate(d, required_vars = NA, min_obs = 0L), "missing from input data: NA")
  expect_silent(validate(d, required_vars = NULL, min_obs = 0L))
  expect_silent(validate(d, required_vars = character(), min_obs = 0L))
  expect_identical(cache_outcome(cachedTest("koenker", m, d)), references$koenker)
  expect_identical(cache_outcome(performLeveneTest(m, d, group = "g")), references$levene)
  # The order of the names, which the message keeps, an empty name and a
  # repeated one.
  cache_reset()
  expect_error(validate(d, c("b1", "a1")), "missing from input data: b1, a1")
  expect_error(validate(d, c("a1", "b1")), "missing from input data: a1, b1")
  expect_error(validate(d, c("x", "")), "missing from input data: \\.")
  expect_silent(validate(d, c("x", "x")))
  expect_silent(validate(d, "x"))
  # The name of the test as it was given, which the messages and the result
  # carry.
  cache_reset()
  sample_size <- heteroTests:::rvalidateSampleSize
  small <- d[1:5, ]
  upper <- sample_size("White", model = NULL, data = small)
  lower <- sample_size("white", model = NULL, data = small)
  expect_identical(upper$details$test_name, "White")
  expect_identical(lower$details$test_name, "white")
})

test_that("the validation cache is bounded by its option, and stores no random draws", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  size <- heteroTests:::.validation_cache_size
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  withr::with_options(list(heteroTests.validation_cache_size = 0), {
    cache_reset()
    quiet(performKoenkerTest(m, d))
    expect_identical(size(), 0L)
  })
  withr::with_options(list(heteroTests.validation_cache_size = 2), {
    cache_reset()
    quiet(performKoenkerTest(m, d))
    expect_identical(size(), 2L)
  })

  # The check of normality tests a random sample of a variable longer than
  # `sample_limit` values: its result is not stored, so a second call draws
  # its sample too, and leaves the generator where the first left it.
  set.seed(4)
  long <- data.frame(r = stats::rnorm(6000))
  check <- function() {
    heteroTests:::rvalidateDistributionalAssumptions(long, list(normality = list(variables = "r")))
  }
  cache_reset()
  set.seed(1)
  first <- check()
  after_first <- stats::runif(2)
  set.seed(1)
  second <- check()
  after_second <- stats::runif(2)
  expect_identical(second, first)
  expect_identical(after_second, after_first)
  expect_identical(size(), 0L)
  # A short variable draws nothing, and its result is stored.
  short <- long[1:100, , drop = FALSE]
  heteroTests:::rvalidateDistributionalAssumptions(short, list(normality = list(variables = "r")))
  expect_identical(size(), 1L)
})

test_that("runHeteroTests() returns the same suite with and without the cache", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  fits <- list(
    lm(y ~ x + z, data = d), lm(y ~ x + z, data = d, weights = w), glm(y ~ x + z, data = d)
  )
  batteries <- list(c("white", "koenker"), c("cook_weisberg", "szroeter", "spread_level"))
  for (k in seq_along(fits)) {
    for (data in list(d, NULL)) {
      for (tests in batteries) {
        info <- paste(k, paste(tests, collapse = "+"), is.null(data))
        run <- function(use_cache) {
          quiet(runHeteroTests(fits[[k]], data, tests = tests, use_cache = use_cache,
                               progress = FALSE))
        }
        cache_reset()
        off <- run(FALSE)
        cache_reset()
        miss <- run(TRUE)
        hit <- run(TRUE)
        expect_identical(miss, off, info = info)
        expect_identical(hit, off, info = info)
        expect_identical(attr(hit[[1]], "extras")$streaming, FALSE, info = info)
      }
    }
  }
})

test_that("a call that falls back on a diagnostic the caches may not hold is not stored", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  registry <- heteroTests:::.diagnostic_registry
  original <- get("breusch_pagan", envir = registry)
  withr::defer(heteroTests:::rregister_package_diagnostic("breusch_pagan", original))
  runs <- new.env()
  runs$n <- 0L
  # A user's diagnostic under the name of a fallback of White's test.
  registerDiagnostic("breusch_pagan", function(model, data) {
    runs$n <- runs$n + 1L
    structure(
      list(statistic = c(X = runs$n), parameter = c(df = 1), p.value = 0.5,
           method = paste("user BP", runs$n), data.name = "model"),
      class = "htest"
    )
  })
  set.seed(9)
  small <- data.frame(x = stats::runif(15), z = stats::rnorm(15))
  small$y <- 1 + small$x + stats::rnorm(15)
  ms <- lm(y ~ x + z, data = small)
  # White's test needs 20 observations; so does its form without cross
  # products, and the runner falls back on "breusch_pagan".
  cache_reset()
  first <- quiet(runHeteroTests(ms, small, tests = "white", progress = FALSE))
  second <- quiet(runHeteroTests(ms, small, tests = "white", progress = FALSE))
  expect_identical(first$white$method, "user BP 1")
  expect_identical(attr(first$white, "extras")$status, "fallback")
  expect_identical(second$white$method, "user BP 2")
  expect_identical(cache_sizes(), c(test = 0L, analysis = 0L))
  # The package's own fallbacks may be kept.
  heteroTests:::rregister_package_diagnostic("breusch_pagan", original)
  cache_reset()
  kept <- quiet(runHeteroTests(ms, small, tests = "white", progress = FALSE))
  expect_identical(attr(kept$white, "diagnostic"), "breusch_pagan")
  expect_identical(cache_sizes()[["analysis"]], 1L)
  expect_identical(quiet(runHeteroTests(ms, small, tests = "white", progress = FALSE)), kept)

  # A fallback on a resampling test: the quantile regression test refuses a
  # weighted fit and the runner falls back on the rank permutation test.
  skip_if_not_installed("quantreg")
  d <- cache_fixture()
  mw <- lm(y ~ x + z, data = d, weights = w)
  run <- function(use_cache) {
    set.seed(1)
    out <- quiet(runHeteroTests(mw, d, tests = "quantile_regression", progress = FALSE,
                                use_cache = use_cache))
    list(out = out, draw = stats::runif(2))
  }
  cache_reset()
  off <- run(FALSE)
  cache_reset()
  a <- run(TRUE)
  b <- run(TRUE)
  expect_identical(attr(off$out$quantile_regression, "diagnostic"), "rank_permutation")
  expect_identical(a, off)
  expect_identical(b, off)
  expect_identical(cache_sizes()[["analysis"]], 0L)
})

test_that("the names of the quantile estimates do not depend on options(digits)", {
  skip_if_not_installed("digest")
  skip_if_not_installed("withr")
  skip_if_not_installed("quantreg")
  cache_silent()
  on.exit(cache_reset(), add = TRUE)
  d <- cache_fixture()
  m <- lm(y ~ x + z, data = d)
  taus <- c(0.25, 0.75)
  reference <- quiet(performQuantileRegressionTest(m, d, taus = taus))
  # The names of the default options, as before 0.16.0.
  expect_identical(colnames(reference$estimate), paste0("tau_", format(taus, trim = TRUE)))
  expect_identical(colnames(reference$estimate), c("tau_0.25", "tau_0.75"))
  withr::with_options(list(digits = 1, scipen = -5, OutDec = ","), {
    expect_identical(quiet(performQuantileRegressionTest(m, d, taus = taus)), reference)
  })
  # A result read from the cache is the one the call computes.
  cache_reset()
  cachedTest("quantile_regression", m, d)
  withr::with_options(list(digits = 1), {
    hit <- quiet(cachedTest("quantile_regression", m, d))
    expect_identical(hit, quiet(cachedTest("quantile_regression", m, d, use_cache = FALSE)))
  })
  expect_identical(colnames(hit$estimate), c("tau_0.25", "tau_0.75"))
})
