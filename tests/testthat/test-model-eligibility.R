# Which models each test accepts, since 0.16.0 (?test_results, "Supported
# models" on the site). Four parts:
#
# - A rank-deficient fit, whose aliased coefficients are NA, is tested on the
#   estimable part of its design: every test that accepts the fit without
#   the aliased columns gives for it what it gives for that fit, which has the
#   same column space, residuals and fitted values. Before 0.16.0 every test
#   refused it with "Model coefficients must be finite".
# - A Gaussian glm with a zero prior weight is refused as the same lm is, by
#   every test that refuses the lm; before 0.16.0 most returned a value.
#   performRESETTest() accepts both: a zero weight drops the observation from
#   both of its nested fits.
# - A glm that is not a least-squares fit, a survey::svyglm() fit and data
#   that lack rows or variables of the fit are refused on every route.
# - The prior weights of a glm, not its working weights, are read.

eligibility_data <- function() {
  d <- contract_data()$d
  d$w <- 1 / d$x1^2
  d$w0 <- d$w
  d$w0[1] <- 0
  d$x1_twice <- 2 * d$x1
  d$high <- as.integer(d$y > stats::median(d$y))
  # Level "p" of f2 never occurs in group "a": the cell a:p of group * f2 is
  # empty, and the coefficient of groupd:f2p is aliased.
  d$f2 <- factor(ifelse(d$group == "a", "n", ifelse(d$x2 > 0, "p", "n")))
  d
}

# Each rank-deficient fit with the same model fitted without its aliased
# columns. For the empty cell the columns the fit estimated are variables of
# the data, so that the design of the reduced fit is the estimable part of the
# design, column for column.
rank_deficient_cases <- function() {
  d <- eligibility_data()
  cell <- lm(y ~ x1 + x2 + group * f2, data = d)
  X <- stats::model.matrix(cell)[, !is.na(stats::coef(cell)), drop = FALSE]
  dr <- d
  dr$gb <- X[, "groupb"]
  dr$gc <- X[, "groupc"]
  dr$gd <- X[, "groupd"]
  dr$f2p <- X[, "f2p"]
  dr$gbf2p <- X[, "groupb:f2p"]
  dr$gcf2p <- X[, "groupc:f2p"]
  list(
    lm = list(
      m = lm(y ~ x1 + x2 + x1_twice, data = d), d = d,
      reduced = lm(y ~ x1 + x2, data = d), dr = d, aliased = "x1_twice"
    ),
    weighted_lm = list(
      m = lm(y ~ x1 + x2 + x1_twice, data = d, weights = w), d = d,
      reduced = lm(y ~ x1 + x2, data = d, weights = w), dr = d, aliased = "x1_twice"
    ),
    gaussian_glm = list(
      m = glm(y ~ x1 + x2 + x1_twice, data = d), d = d,
      reduced = glm(y ~ x1 + x2, data = d), dr = d, aliased = "x1_twice"
    ),
    empty_cell = list(
      m = cell, d = d,
      reduced = lm(y ~ x1 + x2 + gb + gc + gd + f2p + gbf2p + gcf2p, data = dr), dr = dr,
      aliased = "groupd:f2p"
    )
  )
}

eligibility_run <- function(recipe, m, d) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(11)
  tryCatch(
    suppressMessages(suppressWarnings(recipe(m, d))),
    error = function(e) structure(conditionMessage(e), class = "refused"),
    skip = function(e) NULL
  )
}

test_that("the aliased coefficients of a rank-deficient fit are those its QR left out", {
  cases <- rank_deficient_cases()
  for (case in names(cases)) {
    expect_identical(raliased_coefficients(cases[[case]]$m), cases[[case]]$aliased, info = case)
    expect_identical(raliased_coefficients(cases[[case]]$reduced), character(), info = case)
  }
  # An NA that is not aliasing, and NaN, are not.
  m <- lm(y ~ x1 + x2, data = eligibility_data())
  m$coefficients[["x1"]] <- NA_real_
  expect_identical(raliased_coefficients(m), character())
  m$coefficients[["x1"]] <- NaN
  expect_identical(raliased_coefficients(m), character())
  # The design without them is the design of the reduced fit.
  for (case in names(cases)) {
    X <- rnon_aliased_columns(stats::model.matrix(cases[[case]]$m), cases[[case]]$m)
    expect_identical(ncol(X), cases[[case]]$m$rank, info = case)
    expect_equal(unname(X), unname(stats::model.matrix(cases[[case]]$reduced)), ignore_attr = TRUE,
                 info = case)
  }
  # A full-rank design is returned as it is.
  X <- stats::model.matrix(cases$lm$reduced)
  expect_identical(rnon_aliased_columns(X, cases$lm$reduced), X)
})

test_that("every test gives a rank-deficient fit the value of the fit without its aliased terms", {
  skip_on_cran()
  cases <- rank_deficient_cases()
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  compared <- 0L
  for (case in names(cases)) {
    for (fn in tests) {
      a <- eligibility_run(contract_recipes[[fn]], cases[[case]]$m, cases[[case]]$d)
      b <- eligibility_run(contract_recipes[[fn]], cases[[case]]$reduced, cases[[case]]$dr)
      if (is.null(a) || is.null(b)) next
      info <- paste(fn, "on the rank-deficient", case)
      # A test that refuses the reduced fit, such as the quantile regression
      # test on a weighted fit, refuses the rank-deficient one with the same
      # message.
      if (inherits(b, "refused")) {
        expect_identical(a, b, info = info)
        next
      }
      expect_false(inherits(a, "refused"), info = paste(info, a))
      expect_equal(unname(a$statistic), unname(b$statistic), tolerance = 1e-10, info = info)
      expect_equal(a$p.value, b$p.value, tolerance = 1e-10, info = info)
      expect_identical(unname(a$parameter), unname(b$parameter), info = info)
      compared <- compared + 1L
    }
  }
  # Every test on the lm and the empty cell, but the White streaming test on
  # the empty cell, whose auxiliary terms of the dummies are collinear (as on
  # the reduced fit); every test but those that refuse a weighted fit on the
  # weighted lm; every test but the three that refuse every glm on the glm.
  expect_gte(compared, 140L)
})

test_that("auxiliary-regression tests and RESET give a rank-deficient fit the reduced value", {
  # The part of the loop above that runs on CRAN.
  cases <- rank_deficient_cases()
  for (fn in c("performKoenkerTest", "performWhiteTest", "performBPTest", "performGQTest",
               "performRESETTest", "performHarveyTest", "performCookWeisbergTest")) {
    for (case in c("lm", "empty_cell")) {
      a <- eligibility_run(contract_recipes[[fn]], cases[[case]]$m, cases[[case]]$d)
      b <- eligibility_run(contract_recipes[[fn]], cases[[case]]$reduced, cases[[case]]$dr)
      info <- paste(fn, case)
      expect_false(inherits(a, "refused"), info = paste(info, a))
      expect_equal(unname(a$statistic), unname(b$statistic), tolerance = 1e-10, info = info)
      expect_equal(a$p.value, b$p.value, tolerance = 1e-10, info = info)
      expect_identical(unname(a$parameter), unname(b$parameter), info = info)
    }
  }
})

# The design of test-koenker-studentized.R, where lmtest::bptest() returned
# 20.87 on 2 degrees of freedom and the two Koenker statistics of the package
# refused the fit before 0.16.0.
koenker_rank_deficient <- function() {
  set.seed(20261015)
  d <- data.frame(x = runif(150, 1, 5), z = runif(150, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$x * rnorm(150)
  d$z2 <- 2 * d$z
  list(d = d, m = lm(y ~ x + z + z2, d), reduced = lm(y ~ x + z, d))
}

test_that("on a rank-deficient fit the statistics agree with lmtest and car", {
  skip_if_not_installed("lmtest")
  s <- koenker_rank_deficient()
  m <- s$m
  d <- s$d
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  same <- function(ours, statistic, df, p, info) {
    expect_equal(unname(ours$statistic), unname(statistic), tolerance = 1e-8, info = info)
    expect_equal(unname(ours$parameter), unname(df), info = info)
    expect_equal(ours$p.value, unname(p), tolerance = 1e-8, info = info)
  }
  # lmtest::bptest() takes the degrees of freedom from the rank of its
  # auxiliary fit, so it handles the aliased column itself.
  bp <- lmtest::bptest(m)
  expect_equal(unname(bp$statistic), 20.87, tolerance = 1e-3)
  same(performKoenkerTest(m, d), bp$statistic, bp$parameter, bp$p.value, "Koenker")
  same(performKoenkerTestStreaming(m, d, chunk_size = 40, progress = FALSE),
       bp$statistic, bp$parameter, bp$p.value, "Koenker streaming")
  same(without_deprecation(performStudentizedBPTest(m, d)),
       bp$statistic, bp$parameter, bp$p.value, "studentized")
  classical <- lmtest::bptest(m, studentize = FALSE)
  same(performBPTest(m, d), classical$statistic, classical$parameter, classical$p.value, "BP")
  # lmtest::gqtest() and lmtest::resettest() count the columns of the design,
  # the aliased one included, so they are compared on the fit without it.
  gq <- lmtest::gqtest(s$reduced, order.by = ~ x, fraction = 0.2, data = d)
  same(performGQTest(m, d, order_by = "x"), gq$statistic, gq$parameter, gq$p.value, "GQ")
  reset <- lmtest::resettest(s$reduced, power = 2:3, type = "fitted")
  same(performRESETTest(m), reset$statistic, reset$parameter, reset$p.value, "RESET")
  white <- lmtest::bptest(s$reduced, ~ x + z + I(x^2) + I(z^2) + x:z, data = d)
  same(performWhiteTest(m, d), white$statistic, white$parameter, white$p.value, "White")
  skip_if_not_installed("car")
  ncv <- car::ncvTest(m)
  same(performCookWeisbergTest(m, d), ncv$ChiSq, ncv$Df, ncv$p, "Cook-Weisberg")
})

test_that("the quantile regression test fits the estimable part of a rank-deficient design", {
  skip_if_not_installed("quantreg")
  s <- koenker_rank_deficient()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # quantreg::rq() refuses the singular design itself.
  expect_error(
    suppressWarnings(stats::anova(quantreg::rq(y ~ x + z + z2, tau = c(0.25, 0.75), data = s$d),
                                  joint = TRUE)),
    "Singular"
  )
  a <- performQuantileRegressionTest(s$m, s$d)
  b <- performQuantileRegressionTest(s$reduced, s$d)
  expect_identical(a$statistic, b$statistic)
  expect_identical(a$parameter, b$parameter)
  expect_identical(a$p.value, b$p.value)
  # The slopes are those of the columns the fit estimated, under their names.
  expect_identical(a$estimate, b$estimate)
  expect_identical(rownames(a$estimate), c("x", "z"))
})

test_that("the routes give a rank-deficient fit the value of the reduced fit", {
  s <- koenker_rank_deficient()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  ours <- runHeteroTests(s$m, s$d, use_cache = FALSE)
  reduced <- runHeteroTests(s$reduced, s$d, use_cache = FALSE)
  for (name in c("white", "koenker")) {
    expect_equal(unname(ours[[name]]$statistic), unname(reduced[[name]]$statistic),
                 tolerance = 1e-10, info = name)
    expect_identical(unname(ours[[name]]$parameter), unname(reduced[[name]]$parameter), info = name)
    expect_null(attr(ours[[name]], "extras")$status, info = name)
  }
  expect_equal(unname(test_factory$run_test("koenker", s$m, s$d)$statistic),
               unname(performKoenkerTest(s$reduced, s$d)$statistic), tolerance = 1e-10)
  expect_equal(unname(cachedTest("koenker", s$m, s$d, use_cache = FALSE)$statistic),
               unname(performKoenkerTest(s$reduced, s$d)$statistic), tolerance = 1e-10)
  expect_equal(unname(performBPTestRobust(s$m, s$d)$statistic),
               unname(performBPTestRobust(s$reduced, s$d)$statistic), tolerance = 1e-10)
  set.seed(3)
  a <- bootstrapTestStatistic(performKoenkerTest, s$m, s$d, B = 19, progress = FALSE)
  set.seed(3)
  b <- bootstrapTestStatistic(performKoenkerTest, s$reduced, s$d, B = 19, progress = FALSE)
  expect_equal(a$replicates, b$replicates, tolerance = 1e-10)
  expect_identical(a$p_value, b$p_value)
})

test_that("a refusal of a rank-deficient fit names the aliased coefficients", {
  d <- eligibility_data()
  m <- lm(y ~ x1 + x2 + x1_twice, data = d)
  expect_error(
    rstop_aliased(raliased_coefficients(m), "performX()"),
    paste0(
      "performX() cannot use a rank-deficient model: the coefficient of x1_twice is aliased (NA): ",
      "its column of the design is a linear combination of the others. Refit the model without ",
      "the aliased columns."
    ),
    fixed = TRUE
  )
  d$x2_twice <- 2 * d$x2
  m2 <- lm(y ~ x1 + x2 + x1_twice + x2_twice, data = d)
  expect_error(
    rstop_aliased(raliased_coefficients(m2), "performX()"),
    "the coefficients of x1_twice, x2_twice are aliased (NA): their columns", fixed = TRUE
  )
  # The deprecated validateTestInputs() checks the model for any test, so it
  # refuses such a fit, with that message.
  expect_error(
    without_deprecation(validateTestInputs(m, d, "white")),
    "white cannot use a rank-deficient model: the coefficient of x1_twice is aliased", fixed = TRUE
  )
  # A function a user registers with the test factory is not given such a fit,
  # as before 0.16.0; the tests of the package are.
  factory <- TestFactory$new()
  factory$register("mine", function(model, data) performKoenkerTest(model, data))
  factory$register("koenker", performKoenkerTest)
  expect_error(factory$run_test("mine", m, d), "mine cannot use a rank-deficient model")
  expect_s3_class(factory$run_test("koenker", m, d), "htest")
})

test_that("a coefficient that is not finite for another reason is refused by every test", {
  s <- contract_data()
  bad <- list(infinite = s$m, nan = s$m, not_aliased_na = s$m)
  bad$infinite$coefficients[["x1"]] <- Inf
  bad$nan$coefficients[["x1"]] <- NaN
  bad$not_aliased_na$coefficients[["x1"]] <- NA_real_
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  for (input in names(bad)) {
    for (fn in tests) {
      out <- eligibility_run(contract_recipes[[fn]], bad[[input]], s$d)
      if (is.null(out)) next
      expect_true(inherits(out, "refused"), info = paste(fn, input))
      expect_match(unclass(out), "Model coefficients must be finite", info = paste(fn, input))
    }
  }
})

# A replicate of a pairs resample can leave a cell of an interaction empty, so
# that its refit aliases a coefficient the fit estimated. It is not a
# replicate of the model, and fails, as it did before 0.16.0, when every test
# refused such a refit.
test_that("a resampling replicate that aliases a coefficient the fit estimated fails", {
  d <- eligibility_data()
  full <- lm(y ~ x1 + x2 + group, data = d)
  estimated <- names(stats::coef(full))
  expect_false(rrefit_aliases_estimated(full, estimated))
  expect_true(rrefit_aliases_estimated(lm(y ~ x1 + x2 + group * f2, data = d),
                                       c(estimated, "groupd:f2p")))
  # A column the replicate does not have at all, such as the dummy of a level
  # none of its rows holds, is not aliased.
  expect_false(rrefit_aliases_estimated(lm(y ~ x1 + x2, data = d), estimated))
  # The aliased coefficient of a rank-deficient fit was not estimated, so its
  # replicates, which alias it too, do not fail.
  rd <- lm(y ~ x1 + x2 + x1_twice, data = d)
  rd_estimated <- names(stats::coef(rd))[!is.na(stats::coef(rd))]
  expect_false(rrefit_aliases_estimated(rd, rd_estimated))
})

# Zero prior weights -----------------------------------------------------------

zero_weight_fits <- function() {
  d <- eligibility_data()
  list(
    d = d,
    lm = lm(y ~ x1 + x2, data = d, weights = w0),
    glm = glm(y ~ x1 + x2, data = d, weights = w0)
  )
}

test_that("a Gaussian glm with a zero prior weight is refused as the same lm is", {
  z <- zero_weight_fits()
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  refused <- 0L
  for (fn in tests) {
    a <- eligibility_run(contract_recipes[[fn]], z$lm, z$d)
    b <- eligibility_run(contract_recipes[[fn]], z$glm, z$d)
    if (is.null(a) || is.null(b)) next
    if (fn %in% c("performWhiteTestStreaming", "performHarrisonMcCabeTest",
                  "performVarianceFormTest")) {
      # They refuse every glm, and the weighted lm for their own reasons.
      expect_match(unclass(b), "needs a model fitted by stats::lm\\(\\)", info = fn)
      next
    }
    if (fn == "performRESETTest") {
      expect_false(inherits(b, "refused"), info = fn)
      next
    }
    expect_true(inherits(a, "refused"), info = fn)
    expect_identical(b, a, info = fn)
    refused <- refused + 1L
  }
  # Before 0.16.0 the glm returned a value in all but the White bootstrap
  # ("Model residuals must be finite"), the quantile regression test (a
  # weighted fit) and the spatial test, which stopped with another error.
  expect_gte(refused, 34L)
  expect_error(
    performKoenkerTest(z$glm, z$d),
    "The model was fitted with zero or non-finite weights", fixed = TRUE
  )
  expect_error(
    performWhiteTestBootstrap(z$glm, z$d, B = 19),
    "The model was fitted with zero or non-finite weights", fixed = TRUE
  )
})

test_that("the routes refuse a Gaussian glm with a zero prior weight", {
  z <- zero_weight_fits()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  message <- "The model was fitted with zero or non-finite weights"
  expect_error(runHeteroTests(z$glm, z$d, use_cache = FALSE), message, fixed = TRUE)
  expect_error(runDiagnostics(z$glm, z$d, use_cache = FALSE), message, fixed = TRUE)
  expect_error(cachedTest("koenker", z$glm, z$d, use_cache = FALSE), message, fixed = TRUE)
  # The checks of the factory summarise the glm, which warns that the zero
  # weight is not used for the dispersion.
  expect_error(suppressWarnings(test_factory$run_test("white", z$glm, z$d)), message, fixed = TRUE)
  expect_error(
    runHeteroTestsParallel(z$glm, z$d, tests = "koenker", n_cores = 1), message, fixed = TRUE
  )
  expect_error(runPanelTests(z$glm, z$d, "id", "time"), message, fixed = TRUE)
  expect_error(runTimeSeriesTests(z$glm, lags = 2), message, fixed = TRUE)
})

test_that("RESET on a zero weight is RESET on the observations with positive weight", {
  z <- zero_weight_fits()
  positive <- lm(y ~ x1 + x2, data = z$d[z$d$w0 > 0, ], weights = w0)
  expected <- performRESETTest(positive)
  for (fit in list(z$lm, z$glm)) {
    got <- performRESETTest(fit)
    expect_equal(unname(got$statistic), unname(expected$statistic), tolerance = 1e-10)
    expect_identical(unname(got$parameter), unname(expected$parameter))
    expect_equal(got$p.value, expected$p.value, tolerance = 1e-10)
  }
})

test_that("plotBeforeAfter() labels the residuals of a weighted Gaussian glm as weighted", {
  d <- eligibility_data()
  labels <- function(m) levels(plotBeforeAfter(m, m)$data$panel)
  weighted <- c("Original (weighted residuals)", "Remedied (weighted residuals)")
  expect_identical(labels(lm(y ~ x1 + x2, data = d, weights = w)), weighted)
  expect_identical(labels(glm(y ~ x1 + x2, data = d, weights = w)), weighted)
  expect_identical(labels(glm(y ~ x1 + x2, data = d)), c("Original", "Remedied"))
})

# Prior and working weights -------------------------------------------------

test_that("the tests read the prior weights of a glm, not its working weights", {
  d <- eligibility_data()
  gaussian <- glm(y ~ x1 + x2, data = d, weights = w)
  # For the Gaussian family and the identity link the two are equal.
  expect_identical(rleast_squares_weights(gaussian), as.vector(gaussian$prior.weights))
  expect_equal(unname(gaussian$weights), unname(gaussian$prior.weights))
  expect_null(rprior_weights(gaussian))
  # For any other family the working weights depend on the fitted means, and
  # such a glm is refused before any weight is read.
  binomial_fit <- glm(high ~ x1 + x2, data = d, family = binomial)
  expect_false(isTRUE(all.equal(unname(binomial_fit$weights), unname(binomial_fit$prior.weights))))
  expect_null(rleast_squares_weights(binomial_fit))
  expect_error(rpearson_residuals(gaussian), NA)
  expect_error(performKoenkerTest(binomial_fit, d), "needs a model fitted by stats::lm\\(\\)")
  # The residuals of a weighted Gaussian glm are those 0.15.0 read: its
  # deviance residuals, sqrt(w) (y - mu), which are the Pearson residuals.
  expect_identical(rpearson_residuals(gaussian), stats::residuals(gaussian))
  expect_equal(rpearson_residuals(gaussian), stats::residuals(gaussian, type = "pearson"))
})

# Refused models, on every route ----------------------------------------------

route_inputs <- function() {
  d <- eligibility_data()
  set.seed(77)
  d$count <- stats::rpois(nrow(d), exp(0.2 + 0.3 * d$x1))
  inputs <- list(
    binomial = list(m = glm(high ~ x1 + x2, data = d, family = binomial), d = d,
                    message = "needs a model fitted by stats::lm\\(\\)"),
    poisson = list(m = glm(count ~ x1 + x2, data = d, family = poisson), d = d,
                   message = "needs a model fitted by stats::lm\\(\\)"),
    rows_missing = list(m = lm(y ~ x1 + x2, data = d), d = d[1:60, ],
                        message = "requires `data` (to contain the rows|with [0-9]+ observations)"),
    variable_missing = list(m = lm(y ~ x1 + x2, data = d), d = d[, setdiff(names(d), "x2")],
                            message = "The following variables are missing from")
  )
  if (requireNamespace("survey", quietly = TRUE)) {
    design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    inputs$svyglm <- list(m = survey::svyglm(y ~ x1 + x2, design = design), d = d,
                          message = "The model is a survey::svyglm\\(\\) fit")
  }
  inputs
}

test_that("every route refuses a non-least-squares glm, a svyglm and data that do not match", {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  inputs <- route_inputs()
  # The names the package registers; other test files register their own.
  registered <- c(
    "white", "breusch_pagan", "koenker", "cook_weisberg", "ncv", "spread_level", "student_bp",
    "white_bootstrap", "szroeter", "wild_bootstrap", "quantile_regression", "rank_permutation",
    "high_dimensional", "variance_form"
  )
  expect_true(all(registered %in% listDiagnostics()))
  # The factory's "szroeter" is called without the ordering it needs, and
  # stops for that (ROADMAP); "spatial_hetero" needs a neighbour list.
  factory_tests <- setdiff(names(.ht_factory_tests), c("szroeter", "spatial_hetero"))
  for (input in names(inputs)) {
    m <- inputs[[input]]$m
    d <- inputs[[input]]$d
    message <- inputs[[input]]$message
    info <- function(route) paste(route, "on", input)
    expect_error(runHeteroTests(m, d, use_cache = FALSE), message, info = info("runHeteroTests"))
    expect_error(runDiagnostics(m, d, use_cache = FALSE), message, info = info("runDiagnostics"))
    for (name in registered) {
      expect_error(
        without_deprecation(runHeteroTests(m, d, tests = name, use_cache = FALSE)), message,
        info = info(paste("runHeteroTests", name))
      )
      expect_error(
        without_deprecation(cachedTest(name, m, d, use_cache = FALSE)), message,
        info = info(paste("cachedTest", name))
      )
      expect_error(
        get(name, envir = .diagnostic_registry)(m, d), message,
        info = info(paste("registry", name))
      )
    }
    for (name in factory_tests) {
      expect_error(test_factory$run_test(name, m, d), message,
                   info = info(paste("test_factory", name)))
    }
    expect_error(runHeteroTestsParallel(m, d, tests = c("koenker", "white"), n_cores = 1),
                 message, info = info("runHeteroTestsParallel"))
    expect_error(runPanelTests(m, d, "id", "time"), message, info = info("runPanelTests"))
    expect_error(performScatterDiagnostic(m, d, "x1"), message, info = info("scatter"))
    if (!input %in% c("rows_missing", "variable_missing")) {
      # The time-series tests read nothing from `data`.
      expect_error(runTimeSeriesTests(m, lags = 2), message, info = info("runTimeSeriesTests"))
    }
  }
})

test_that("the runners that take no fitted model cannot be given one", {
  inputs <- route_inputs()
  # runSurveyHeteroTests() fits the model by least squares itself, from a
  # formula and a design; a svyglm() fit is not a formula.
  skip_if_not_installed("survey")
  design <- survey::svydesign(ids = ~1, weights = ~w, data = eligibility_data())
  expect_error(runSurveyHeteroTests(inputs$svyglm$m, design), "`formula` must be a model formula")
  # runMultivariateTests() runs registered tests on data and a group, so a
  # model test given the data in place of a model refuses it.
  d <- eligibility_data()
  expect_error(
    runMultivariateTests(d[, c("y", "x2")], d$group, tests = "koenker"),
    "Provide an object fitted with stats::lm\\(\\) or stats::glm\\(\\)"
  )
  # checkModel() checks the class only: it accepts any lm or glm, and the
  # tests decide which they accept (?test_results).
  expect_invisible(checkModel(inputs$binomial$m))
  expect_error(
    checkModel(d), "Provide a model fitted with stats::lm() or stats::glm().", fixed = TRUE
  )
})

test_that("the scatter diagnostic refuses a glm that is not a least-squares fit", {
  inputs <- route_inputs()
  for (input in c("binomial", "poisson")) {
    expect_error(
      performScatterDiagnostic(inputs[[input]]$m, inputs[[input]]$d, "x1"),
      "performScatterDiagnostic\\(\\) needs a model fitted by stats::lm\\(\\): a glm with the",
      info = input
    )
  }
  d <- eligibility_data()
  expect_equal(
    performScatterDiagnostic(glm(y ~ x1 + x2, data = d), d, "x1"),
    performScatterDiagnostic(lm(y ~ x1 + x2, data = d), d, "x1")
  )
})
