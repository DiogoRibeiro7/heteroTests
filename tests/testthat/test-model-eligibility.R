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
# - The three tests that refused every glm until 0.16.0,
#   performWhiteTestStreaming(), performHarrisonMcCabeTest() and
#   performVarianceFormTest(), accept a Gaussian glm with the identity link
#   and return the value of the lm; fitWLS() refits one as the lm it is; and
#   the checks of the model read the R-squared of the least-squares fit, so a
#   near-perfect Gaussian glm is refused as the same lm is.

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
  # Every test on the lm, the glm and the empty cell, but the White streaming
  # test on the empty cell, whose auxiliary terms of the dummies are collinear
  # (as on the reduced fit); every test but those that refuse a weighted fit on
  # the weighted lm. Until 0.16.0 the three tests that refused every glm were
  # not compared on the glm.
  expect_gte(compared, 143L)
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

# The model without the aliased columns is the one lm() keeps: of an aliased
# set, the column that comes last in the order of the terms is left out. Most
# statistics depend only on the space the columns span; White's without its
# cross products depends on which column is left out.
test_that("the reduced model is the one lm() keeps, the later aliased column left out", {
  set.seed(32)
  n <- 160
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  d$x_sum <- d$x1 + d$x2
  d$y <- 1 + d$x1 + rnorm(n, sd = exp(0.4 * d$x1))
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  first <- lm(y ~ x1 + x2 + x_sum, d)
  second <- lm(y ~ x_sum + x1 + x2, d)
  expect_identical(raliased_coefficients(first), "x_sum")
  expect_identical(raliased_coefficients(second), "x2")
  squares <- function(m) performWhiteTest(m, d, cross_products = FALSE)
  for (pair in list(list(first, lm(y ~ x1 + x2, d)), list(second, lm(y ~ x_sum + x1, d)))) {
    expect_equal(unname(squares(pair[[1]])$statistic), unname(squares(pair[[2]])$statistic),
                 tolerance = 1e-10)
    expect_identical(unname(squares(pair[[1]])$parameter), unname(squares(pair[[2]])$parameter))
  }
  # Without the cross products the two orders differ; with them, they agree.
  expect_false(isTRUE(all.equal(unname(squares(first)$statistic),
                                unname(squares(second)$statistic))))
  expect_equal(unname(performWhiteTest(first, d)$statistic),
               unname(performWhiteTest(second, d)$statistic), tolerance = 1e-10)
  expect_equal(unname(performKoenkerTest(first, d)$statistic),
               unname(performKoenkerTest(second, d)$statistic), tolerance = 1e-10)
})

# The power form of the variance-form test takes the logarithms of every
# column of the design, the aliased ones included. The logarithm of an
# aliased column that is not proportional to another is not a combination of
# the logarithms of the others, so the value is not that of the model without
# it; kept so, decided in the review of 0.16.0, because it does not depend on
# the order of the terms, as the value of the model without it does. The
# exponential form, the default, gives the value of the model without it.
test_that("the power form of the variance-form test uses every column of the design", {
  set.seed(31)
  n <- 160
  d <- data.frame(x1 = runif(n, 1, 3), x4 = runif(n, 1, 3))
  d$x14 <- d$x1 + d$x4
  d$y <- 1 + d$x1 + d$x4 + rnorm(n, sd = d$x1)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fits <- list(
    aliased = lm(y ~ x1 + x4 + x14, d), aliased_reordered = lm(y ~ x14 + x1 + x4, d),
    reduced = lm(y ~ x1 + x4, d), reduced_other = lm(y ~ x14 + x1, d)
  )
  run <- function(m, form) performVarianceFormTest(m, d, form = form)
  power <- lapply(fits, run, form = "power")
  exponential <- lapply(fits, run, form = "exponential")
  stat <- function(r) unname(r$statistic)
  # Power: the aliased fits agree with each other, on 6 and 150 degrees of
  # freedom, and with neither reduced fit, which differ from each other.
  expect_equal(stat(power$aliased), 1.352, tolerance = 1e-3)
  expect_equal(stat(power$aliased_reordered), stat(power$aliased), tolerance = 1e-10)
  expect_identical(unname(power$aliased$parameter), c(6L, 150L))
  expect_equal(stat(power$reduced), 2.015, tolerance = 1e-3)
  expect_equal(stat(power$reduced_other), 1.343, tolerance = 1e-3)
  expect_identical(unname(power$reduced$parameter), c(3L, 154L))
  expect_identical(
    power$aliased$variance_function$regressors, c("log(x1)", "log(x4)", "log(x14)")
  )
  # Exponential: all four agree, the value of the model without the alias.
  for (fit in names(fits)) {
    expect_equal(stat(exponential[[fit]]), stat(exponential$reduced), tolerance = 1e-10, info = fit)
    expect_identical(unname(exponential[[fit]]$parameter), c(3L, 154L), info = fit)
  }
})

# Offsets ----------------------------------------------------------------------

# Four tests left the offset out of a refit and, on a model with an offset,
# returned a value that is not that of the model; decided on 2026-10-17, they
# refused the offsets they ignored until the offset fix of 0.16.0. Since then
# they read the offset from the fit (the model context, R/model_context.R):
# Goldfeld-Quandt, the quantile regression test and the studentized
# Breusch-Pagan statistic give the value of the model fitted to the response
# minus the offset, and RESET the nested F test of the model with its offset
# (decided 2026-10-18).
offset_fits <- function() {
  d <- contract_data()$d
  set.seed(33)
  d$u <- rnorm(nrow(d))
  d$o <- 0.8 * d$u
  d$y <- d$y + d$o
  d$yo <- d$y - d$o
  d$w <- 1 / d$x1^2
  d$zero <- 0
  list(
    d = d,
    argument = lm(y ~ x1 + x2, d, offset = o),
    term = lm(y ~ x1 + x2 + offset(o), d),
    weighted_argument = lm(y ~ x1 + x2, d, offset = o, weights = w),
    weighted_term = lm(y ~ x1 + x2 + offset(o), d, weights = w),
    reference = lm(yo ~ x1 + x2, d),
    weighted_reference = lm(yo ~ x1 + x2, d, weights = w),
    zero = lm(y ~ x1 + x2, d, offset = zero),
    plain = lm(y ~ x1 + x2, d)
  )
}

# The fit of the response minus the offset that a fit with an offset stands
# for.
offset_reference <- function(f, k) {
  if (startsWith(k, "weighted")) f$weighted_reference else f$reference
}

# RESET of a fit with an offset, written out: anova() of the fit, with its
# offset and weights, against the fit augmented with the squares and cubes of
# its fitted values X b + o, with the same offset and weights.
offset_reset_reference <- function(m, d) {
  ref <- d
  ref$f2 <- stats::fitted(m)^2
  ref$f3 <- stats::fitted(m)^3
  weights_of_fit <- stats::weights(m)
  ref$wt <- if (is.null(weights_of_fit)) rep(1, nrow(d)) else weights_of_fit
  restricted <- stats::lm(y ~ x1 + x2, data = ref, weights = wt, offset = o)
  augmented <- stats::lm(y ~ x1 + x2 + f2 + f3, data = ref, weights = wt, offset = o)
  stats::anova(restricted, augmented)
}

expect_same_htest <- function(object, expected, info, tolerance = 1e-10) {
  expect_s3_class(object, "htest")
  expect_equal(unname(object$statistic), unname(expected$statistic), tolerance = tolerance,
               info = info)
  expect_equal(unname(object$parameter), unname(expected$parameter), info = info)
  expect_equal(object$p.value, expected$p.value, tolerance = tolerance, info = info)
}

test_that("the four tests give a model with an offset the value of that model", {
  f <- offset_fits()
  d <- f$d
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  offsets <- c("argument", "term", "weighted_argument", "weighted_term")
  # The studentized Breusch-Pagan statistic is that of the model of y - o, and
  # Koenker's statistic of the model with the offset.
  studentized <- list(
    performStudentizedBPTest = function(m) without_deprecation(performStudentizedBPTest(m, d)),
    performBPTestRobust = function(m) performBPTestRobust(m, d),
    .studentized_bp_test = function(m) .studentized_bp_test(m, d),
    performGQTest = function(m) performGQTest(m, d, order_by = "x1")
  )
  for (fn in names(studentized)) {
    for (k in offsets) {
      out <- studentized[[fn]](f[[k]])
      expect_same_htest(out, studentized[[fn]](offset_reference(f, k)), paste(fn, k))
      if (!identical(fn, "performGQTest")) {
        koenker <- performKoenkerTest(f[[k]], d)
        expect_equal(unname(out$statistic), unname(koenker$statistic), tolerance = 1e-10,
                     info = paste(fn, k))
      }
    }
  }
  # Without `data`, from the model frame, which holds an offset given as an
  # argument as "(offset)", and an offset() term with `data`.
  expect_same_htest(without_deprecation(performStudentizedBPTest(f$argument)),
                    without_deprecation(performStudentizedBPTest(f$reference)), "NULL")
  expect_same_htest(performGQTest(f$weighted_argument, order_by = "x1"),
                    performGQTest(f$weighted_reference, order_by = "x1"), "GQ NULL")
  if (requireNamespace("quantreg", quietly = TRUE)) {
    # quantreg::anova.rqs() warns about the sparsity estimates of this design
    # ("non-positive fis"), as on the fit of y - o.
    quantile <- function(m, data = NULL) {
      suppressWarnings(performQuantileRegressionTest(m, data))
    }
    for (k in c("argument", "term")) {
      expect_same_htest(quantile(f[[k]], d), quantile(f$reference, d), paste("quantile", k))
      expect_same_htest(quantile(f[[k]]), quantile(f$reference, d), paste("quantile NULL", k))
    }
  }
  # RESET is the nested F test of the model with its offset, not RESET of
  # the model of y - o, which augments it with the powers of X b alone.
  for (k in offsets) {
    out <- performRESETTest(f[[k]], d)
    reference <- offset_reset_reference(f[[k]], d)
    expect_equal(unname(out$statistic), reference$F[2], tolerance = 1e-10, info = k)
    expect_equal(unname(out$parameter), c(reference$Df[2], reference$Res.Df[2]), info = k)
    expect_equal(out$p.value, reference$`Pr(>F)`[2], tolerance = 1e-10, info = k)
    expect_identical(performRESETTest(f[[k]])$statistic, out$statistic, info = k)
  }
  expect_gt(abs(unname(performRESETTest(f$argument)$statistic) -
                  unname(performRESETTest(f$reference)$statistic)), 0.1)
  # The classical statistic of performBPTestRobust() accounted for an offset
  # already.
  classical <- performBPTestRobust(f$argument, d, studentize = FALSE)
  expect_equal(unname(classical$statistic),
               unname(performBPTestRobust(f$reference, d, studentize = FALSE)$statistic),
               tolerance = 1e-10)
  # An offset that is zero everywhere changes nothing.
  expect_identical(performRESETTest(f$zero)$statistic, performRESETTest(f$plain)$statistic)
  expect_equal(unname(performGQTest(f$zero, d, order_by = "x1")$statistic),
               unname(performGQTest(f$plain, d, order_by = "x1")$statistic), tolerance = 1e-12)
  # Nothing in the package refuses an offset any more.
  expect_false(exists("rrefuse_offset", envir = asNamespace("heteroTests"), inherits = FALSE))
})

test_that("the routes return the value of a model with an offset", {
  f <- offset_fits()
  d <- f$d
  m <- f$argument
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # quantreg::anova.rqs() warns about the sparsity estimates of this design
  # ("non-positive fis"), on the model and on the fit of y - o alike.
  quiet <- function(expr) suppressWarnings(expr)
  expected <- list(
    student_bp = performKoenkerTest(m, d),
    quantile_regression = if (requireNamespace("quantreg", quietly = TRUE)) {
      quiet(performQuantileRegressionTest(f$reference, d))
    }
  )
  names <- "student_bp"
  if (requireNamespace("quantreg", quietly = TRUE)) {
    names <- c(names, "quantile_regression")
  }
  same <- function(object, name, how) {
    expect_equal(unname(object$statistic), unname(expected[[name]]$statistic),
                 tolerance = 1e-10, info = paste(name, how))
  }
  for (name in names) {
    for (use_cache in c(FALSE, TRUE)) {
      res <- quiet(runHeteroTests(m, d, tests = name, use_cache = use_cache))
      same(res[[name]], name, paste("runHeteroTests, use_cache =", use_cache))
      expect_null(attr(res[[name]], "extras")$status, info = name)
    }
    same(quiet(cachedTest(name, m, d, use_cache = FALSE)), name, "cachedTest")
    same(quiet(get(name, envir = .diagnostic_registry)(m, d)), name, "registry")
    same(quiet(test_factory$run_test(name, m, d)), name, "test_factory")
    same(quiet(runHeteroTestsParallel(m, d, tests = name, n_cores = 1))[[name]], name,
         "parallel")
  }
  # runDiagnostics() runs RESET, the nested F test of the model with its offset.
  reference <- offset_reset_reference(m, d)
  diagnostics <- runDiagnostics(m, d, use_cache = FALSE)
  expect_equal(unname(diagnostics$reset$statistic), reference$F[2], tolerance = 1e-10)
  # The default battery accounts for the offset, as before.
  res <- runHeteroTests(m, d, use_cache = FALSE)
  expect_equal(unname(res$koenker$statistic),
               unname(performKoenkerTest(f$reference, d)$statistic), tolerance = 1e-10)
  # Grouped data return the statistic of each group's fit, with its offset.
  skip_if_not_installed("dplyr")
  d$half <- rep(c("a", "b"), length.out = nrow(d))
  grouped <- without_deprecation(runHeteroTests(lm(y ~ x1 + x2, d, offset = o),
                                                dplyr::group_by(d, half), tests = "student_bp"))
  halves <- attr(grouped, "group_keys")$half
  expect_setequal(halves, c("a", "b"))
  for (k in seq_along(grouped)) {
    half <- halves[k]
    rows <- d[d$half == half, ]
    suite <- grouped[[k]]
    expect_null(attr(suite$student_bp, "extras")$status, info = half)
    expect_equal(unname(suite$student_bp$statistic),
                 unname(performKoenkerTest(lm(y ~ x1 + x2, rows, offset = o), rows)$statistic),
                 tolerance = 1e-10, info = half)
  }
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
  # weighted fit) and the spatial test, which stopped with another error. The
  # quantile regression and spatial tests are skipped without quantreg and
  # spdep. Since 0.16.0 the three tests that refused every glm refuse it with
  # the message they give the lm: the White streaming and Harrison-McCabe tests
  # for the zero weight, the variance-form test for weights that did not come
  # from fitWLS().
  expect_gte(refused, 35L)
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

# A Gaussian glm in the three tests that refused every glm ---------------------

# performWhiteTestStreaming(), performHarrisonMcCabeTest() and
# performVarianceFormTest() refused every glm until 0.16.0. A glm with the
# Gaussian family, the identity link and prior weights w is fitted by maximum
# likelihood, which for this family is weighted least squares: its design,
# coefficients, fitted values, residuals and weights are those of
# lm(..., weights = w), up to rounding, and each of the three statistics, and
# its null distribution, is a function of those alone (the section "A Gaussian
# glm is a least-squares fit" of the Supported models page). Since 0.16.0 they
# accept it and return the value of the lm. The comparisons are to 1e-10 for
# the statistic, the p-value and the estimate, which differ by rounding, and
# identical() for everything else the tests return: the parameter, the
# method, the alternative, the data name and what the variance-form test
# records of its variance function.
three_glm_tests <- list(
  performWhiteTestStreaming = function(m, d) {
    performWhiteTestStreaming(m, d, chunk_size = 25, progress = FALSE)
  },
  performHarrisonMcCabeTest = function(m, d) performHarrisonMcCabeTest(m, d, order_by = "x1"),
  performHarrisonMcCabeTest_two_sided = function(m, d) {
    performHarrisonMcCabeTest(m, d, order_by = "x2", point = 1 / 3, alternative = "two.sided")
  },
  performVarianceFormTest = function(m, d) performVarianceFormTest(m, d),
  performVarianceFormTest_power = function(m, d) {
    performVarianceFormTest(m, d, var_formula = ~x1, form = "power")
  },
  performVarianceFormTest_against = function(m, d) {
    performVarianceFormTest(m, d, against = ~ x2 + I(x1^3))
  }
)

# Each design fitted by lm() and by glm(), unweighted and with the prior
# weights 1 / x1^2: plain, with three responses missing under na.omit, with a
# factor regressor, and rank-deficient.
three_glm_cases <- function() {
  d <- eligibility_data()
  d$y_na <- d$y
  d$y_na[c(3, 50, 77)] <- NA
  formulas <- list(
    plain = y ~ x1 + x2,
    na_omit = y_na ~ x1 + x2,
    factor = y ~ x1 + x2 + group,
    rank_deficient = y ~ x1 + x2 + x1_twice
  )
  cases <- list()
  for (design in names(formulas)) {
    f <- formulas[[design]]
    cases[[design]] <- list(
      lm = eval(bquote(lm(.(f), data = d, na.action = na.omit))),
      glm = eval(bquote(glm(.(f), data = d, na.action = na.omit)))
    )
    cases[[paste0(design, "_weighted")]] <- list(
      lm = eval(bquote(lm(.(f), data = d, weights = w, na.action = na.omit))),
      glm = eval(bquote(glm(.(f), data = d, weights = w, na.action = na.omit)))
    )
  }
  list(d = d, cases = cases)
}

expect_same_test_value <- function(a, b, info) {
  expect_false(inherits(a, "refused"), info = paste(info, a))
  expect_false(inherits(b, "refused"), info = paste(info, b))
  rounded <- c("statistic", "p.value", "estimate", "r_squared", "bootstrap")
  expect_identical(names(b), names(a), info = info)
  expect_equal(unname(b$statistic), unname(a$statistic), tolerance = 1e-10, info = info)
  expect_equal(b$p.value, a$p.value, tolerance = 1e-10, info = info)
  expect_equal(b$estimate, a$estimate, tolerance = 1e-10, info = info)
  expect_equal(b$r_squared, a$r_squared, tolerance = 1e-10, info = info)
  # The replicates of a bootstrap, from the same seed.
  expect_equal(b$bootstrap, a$bootstrap, tolerance = 1e-10, info = info)
  expect_identical(b$parameter, a$parameter, info = info)
  expect_identical(b[setdiff(names(b), rounded)], a[setdiff(names(a), rounded)], info = info)
}

test_that("the three tests give a Gaussian glm the value of the lm", {
  s <- three_glm_cases()
  compared <- 0L
  refused <- 0L
  for (case in names(s$cases)) {
    for (fn in names(three_glm_tests)) {
      a <- eligibility_run(three_glm_tests[[fn]], s$cases[[case]]$lm, s$d)
      b <- eligibility_run(three_glm_tests[[fn]], s$cases[[case]]$glm, s$d)
      info <- paste(fn, "on the", case, "Gaussian glm")
      # The weighted lm is refused by the variance-form test, whose weights
      # must come from fitWLS(); the glm is refused with the same message.
      # Since 0.17.0 the White streaming test drops the squares of the
      # dummies of the factor, on which it stopped before.
      if (inherits(a, "refused")) {
        expect_identical(b, a, info = info)
        refused <- refused + 1L
        next
      }
      expect_same_test_value(a, b, info)
      compared <- compared + 1L
    }
  }
  expect_identical(compared, 36L)
  expect_identical(refused, 12L)
})

test_that("the three tests refuse a glm that is not a least-squares fit", {
  d <- eligibility_data()
  set.seed(77)
  d$count <- stats::rpois(nrow(d), exp(0.2 + 0.3 * d$x1))
  fits <- list(
    binomial = glm(high ~ x1 + x2, data = d, family = binomial),
    poisson = glm(count ~ x1 + x2, data = d, family = poisson),
    gaussian_log = glm(count + 1 ~ x1 + x2, data = d, family = gaussian(link = "log"))
  )
  described <- c(
    binomial = "a glm with the binomial family and the logit link",
    poisson = "a glm with the poisson family and the log link",
    gaussian_log = "a glm with the gaussian family and the log link"
  )
  for (fit in names(fits)) {
    for (fn in c("performWhiteTestStreaming", "performHarrisonMcCabeTest",
                 "performVarianceFormTest")) {
      expect_error(
        contract_recipes[[fn]](fits[[fit]], d),
        paste0(
          fn, "() needs a model fitted by stats::lm(): ", described[[fit]], " is not a ",
          "least-squares fit. A glm with the gaussian family and the identity link is accepted."
        ),
        fixed = TRUE, info = paste(fn, fit)
      )
    }
  }
})

# A Gaussian glm with prior weights given by hand has no variance function for
# the variance-form test to re-estimate, as the lm with those weights has
# none; fitWLS() of the glm is the fitWLS() of the lm, and is tested as it is.
test_that("fitWLS() refits a Gaussian glm as the lm it is", {
  s <- three_glm_cases()
  d <- s$d
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (case in c("plain", "plain_weighted", "factor", "rank_deficient")) {
    info <- case
    from_lm <- fitWLS(s$cases[[case]]$lm)
    from_glm <- fitWLS(s$cases[[case]]$glm)
    # lm.wfit() fits both; before 0.16.0 the refit of a glm kept the class
    # of the glm without its family.
    expect_identical(class(from_glm), "lm", info = info)
    expect_equal(stats::coef(from_glm), stats::coef(from_lm), tolerance = 1e-10, info = info)
    expect_equal(stats::weights(from_glm), stats::weights(from_lm), tolerance = 1e-10, info = info)
    expect_equal(stats::residuals(from_glm), stats::residuals(from_lm), tolerance = 1e-10,
                 info = info)
    expect_equal(attr(from_glm, "variance_function"), attr(from_lm, "variance_function"),
                 tolerance = 1e-10, info = info)
    # The prior weights of the weighted glm are recorded as those of the lm.
    expect_identical(attr(from_glm, "variance_function")$initial_weights,
                     attr(from_lm, "variance_function")$initial_weights, info = info)
    expect_no_error(summary(from_glm))
    expect_same_test_value(performVarianceFormTest(from_lm), performVarianceFormTest(from_glm),
                           paste("variance form on fitWLS() of", info))
    expect_same_test_value(performKoenkerTest(from_lm, d), performKoenkerTest(from_glm, d),
                           paste("Koenker on fitWLS() of", info))
  }
  # The bootstrap that estimates the weights again reads the recorded prior
  # weights of the weighted glm.
  run <- function(m) {
    set.seed(5)
    performWildBootstrapTest(m, d, B = 49, progress = FALSE)
  }
  expect_same_test_value(run(fitWLS(s$cases$plain_weighted$lm)),
                         run(fitWLS(s$cases$plain_weighted$glm)), "wild bootstrap")
  # Any other glm is refused, where fitWLS() returned a fit no test accepted.
  binomial_fit <- glm(high ~ x1 + x2, data = d, family = binomial)
  expect_error(
    fitWLS(binomial_fit),
    "fitWLS() needs a model fitted by stats::lm(): a glm with the binomial family", fixed = TRUE
  )
  expect_error(
    performVarianceFormTest(s$cases$plain_weighted$glm),
    "`model` was fitted with weights that did not come from fitWLS()", fixed = TRUE
  )
})

# lm() decides the rank of the design with the tolerance 1e-7, glm.fit() with
# min(1e-7, epsilon / 1000), 1e-11 by default. On a column within that margin
# of a combination of the others the lm aliases it and the glm keeps it, and
# the glm gets the values of lm(..., tol = 1e-11), not those of the default
# lm (the Supported models page). Recorded, not changed.
test_that("a Gaussian glm gives the value of the lm that decides the rank as it does", {
  d <- contract_data()$d
  set.seed(5)
  d$x3 <- d$x1 + 1e-9 * stats::rnorm(nrow(d))
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fit_glm <- glm(y ~ x1 + x2 + x3, data = d)
  fit_lm <- lm(y ~ x1 + x2 + x3, data = d)
  fit_lm_tol <- lm(y ~ x1 + x2 + x3, data = d, tol = 1e-11)
  expect_identical(raliased_coefficients(fit_lm), "x3")
  expect_identical(raliased_coefficients(fit_glm), character())
  expect_identical(fit_glm$rank, fit_lm_tol$rank)
  calls <- list(
    hmc = function(m) performHarrisonMcCabeTest(m, d, order_by = "x1"),
    koenker = function(m) performKoenkerTest(m, d)
  )
  # The design is ill-conditioned by construction, so the rounding of the two
  # fits is amplified: they agree to about 1e-8 here, not 1e-13.
  for (name in names(calls)) {
    a <- calls[[name]](fit_lm_tol)
    b <- calls[[name]](fit_glm)
    expect_equal(unname(b$statistic), unname(a$statistic), tolerance = 1e-6, info = name)
    expect_equal(b$p.value, a$p.value, tolerance = 1e-6, info = name)
    expect_identical(b$parameter, a$parameter, info = name)
    expect_false(isTRUE(all.equal(unname(calls[[name]](fit_lm)$statistic), unname(b$statistic),
                                  tolerance = 1e-6)), info = name)
  }
  # White's streaming test decides the rank of its auxiliary regression as
  # performWhiteTest() does, at the tolerance of qr() on the centred terms,
  # and drops the terms of the column within that margin, as performWhiteTest()
  # drops them; up to 0.16.0 it stopped on them ("rank deficient"). Its checks
  # warn about the collinear column (VIF > 10).
  streamed <- suppressWarnings(performWhiteTestStreaming(fit_glm, d, progress = FALSE))
  exact <- suppressWarnings(performWhiteTest(fit_glm, d))
  expect_identical(unname(streamed$parameter), unname(exact$parameter))
  expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8)
})

# fitWLS() keeps the call of the model it refits, for summary() and update().
# Of a Gaussian glm it records the call of the lm: before, it kept the glm()
# call, so update() fitted a glm again and summary() printed glm(...).
test_that("fitWLS() of a Gaussian glm records the call of the lm", {
  d <- eligibility_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  fit_glm <- glm(y ~ x1 + x2, family = gaussian(), data = d, weights = w,
                 control = glm.control(epsilon = 1e-10), method = "glm.fit")
  fit_lm <- lm(y ~ x1 + x2, data = d, weights = w)
  from_glm <- fitWLS(fit_glm)
  from_lm <- fitWLS(fit_lm)
  # The arguments only glm() takes are left out.
  expect_identical(from_glm$call, quote(stats::lm(formula = y ~ x1 + x2, data = d, weights = w)))
  # The call of an lm is kept as it is.
  expect_identical(from_lm$call, fit_lm$call)
  # update() fits the call again, without the estimated weights, as for the lm.
  refit_glm <- update(from_glm)
  refit_lm <- update(from_lm)
  expect_identical(class(refit_glm), "lm")
  expect_equal(stats::coef(refit_glm), stats::coef(refit_lm), tolerance = 1e-10)
  expect_equal(stats::weights(refit_glm), d$w)
  # summary() prints the call, and its values are those of the lm's fit, as
  # are the predictions.
  expect_match(capture.output(print(summary(from_glm)))[3], "^stats::lm\\(formula = y ~ x1 \\+ x2")
  expect_equal(summary(from_glm)$coefficients, summary(from_lm)$coefficients, tolerance = 1e-10)
  expect_equal(summary(from_glm)$r.squared, summary(from_lm)$r.squared, tolerance = 1e-10)
  expect_equal(stats::predict(from_glm, d[1:5, ]), stats::predict(from_lm, d[1:5, ]),
               tolerance = 1e-10)
  # A family given by position is matched by glm() and left out too.
  positional <- fitWLS(glm(y ~ x1 + x2, gaussian, d))
  expect_identical(positional$call, quote(stats::lm(formula = y ~ x1 + x2, data = d)))
})

# summary() of a glm reports no R-squared. The checks of the model read that
# of the least-squares fit: before 0.16.0 a near-perfect Gaussian glm escaped
# the refusal the same lm met, and checkModel(enhanced = TRUE) stopped on any
# glm with "missing value where TRUE/FALSE needed". It is summary.lm() applied
# to the glm, so it agrees with summary() of the lm on every version of R:
# R 4.3.0 changed what summary.lm() reports for a model with an offset, and
# one minus the deviance over the null deviance agreed with it on 4.3.0 and
# later only.
test_that("the R-squared of a Gaussian glm is that of the lm", {
  d <- eligibility_data()
  d$o <- 0.3 * d$x2
  formulas <- list(y ~ x1 + x2, y ~ 0 + x1 + x2, y ~ x1 + x2 + offset(o), y ~ 0 + x1 + offset(o),
                   y ~ x1 + x2 + x1_twice)
  for (f in formulas) {
    for (weighted in c(FALSE, TRUE)) {
      info <- paste(deparse(f), if (weighted) "weighted" else "")
      if (weighted) {
        fit_lm <- eval(bquote(lm(.(f), data = d, weights = w)))
        fit_glm <- eval(bquote(glm(.(f), data = d, weights = w)))
      } else {
        fit_lm <- eval(bquote(lm(.(f), data = d)))
        fit_glm <- eval(bquote(glm(.(f), data = d)))
      }
      expect_identical(rleast_squares_r_squared(fit_lm), summary(fit_lm)$r.squared, info = info)
      expect_equal(rleast_squares_r_squared(fit_glm), summary(fit_lm)$r.squared,
                   tolerance = 1e-12, info = info)
    }
  }
  expect_identical(rleast_squares_r_squared(glm(high ~ x1 + x2, data = d, family = binomial)),
                   NA_real_)
})

test_that("a near-perfect Gaussian glm is refused as the same lm is", {
  d <- eligibility_data()
  set.seed(91)
  d$y <- 1 + d$x1 - d$x2 + stats::rnorm(nrow(d), sd = 0.001)
  fit_lm <- lm(y ~ x1 + x2, data = d)
  fit_glm <- glm(y ~ x1 + x2, data = d)
  expect_gt(summary(fit_lm)$r.squared, 0.999)
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  # These do not check the R-squared, and give the lm and the glm a value.
  unchecked <- c("performSpreadLevelTest", "performPesaranTest", "performBPRandomEffectsTest",
                 "performDavidianCarrollTest", "performRESETTest")
  refused <- 0L
  for (fn in tests) {
    a <- eligibility_run(contract_recipes[[fn]], fit_lm, d)
    b <- eligibility_run(contract_recipes[[fn]], fit_glm, d)
    if (is.null(a) || is.null(b)) next
    if (fn %in% unchecked) {
      expect_same_test_value(a, b, info = fn)
      next
    }
    expect_true(inherits(a, "refused"), info = fn)
    expect_match(unclass(a), "Model appears perfectly explained", fixed = TRUE, info = fn)
    expect_identical(b, a, info = fn)
    refused <- refused + 1L
  }
  # Every other test but those skipped without quantreg and spdep. Before
  # 0.16.0, 30 of these 34 gave the glm a value; the White bootstrap refused
  # it with this message, and the three tests that refused every glm with
  # theirs.
  expect_gte(refused, 32L)
  expect_error(performKoenkerTest(fit_glm, d), "Model appears perfectly explained", fixed = TRUE)
  # checkModel(enhanced = TRUE) warns about it, as about the lm, and accepts
  # any other glm, which it stopped on.
  expect_warning(checkModel(fit_glm, d, enhanced = TRUE), "Near-perfect fit detected")
  expect_warning(checkModel(fit_lm, d, enhanced = TRUE), "Near-perfect fit detected")
  binomial_fit <- glm(high ~ x1 + x2, data = d, family = binomial)
  expect_invisible(checkModel(binomial_fit, enhanced = TRUE))
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
  # asks for it (`order_by`); "spatial_hetero" needs a neighbour list.
  factory_tests <- setdiff(names(.ht_factory_tests), c("szroeter", "spatial_hetero"))
  # Without quantreg the quantile regression test stops for that first.
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    registered <- setdiff(registered, "quantile_regression")
    factory_tests <- setdiff(factory_tests, "quantile_regression")
  }
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

# Anything but an lm or a glm is refused by every test with one message, which
# names the class and the pooled lm a panel is tested as. Before 0.16.0 a plm
# fit was refused with four different messages, depending on the test.
test_that("every test refuses a plm fit with the same message", {
  skip_if_not_installed("plm")
  d <- eligibility_data()
  pm <- plm::plm(y ~ x1 + x2, data = plm::pdata.frame(d, index = c("id", "time")),
                 model = "pooling")
  refusal <- function(fn) {
    paste0(
      "^Provide an object fitted with stats::lm\\(\\) or stats::glm\\(\\) before running ",
      fn, "\\(\\) \\(received class: plm/panelmodel\\)\\. .*pooled lm\\(\\), with the units"
    )
  }
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  for (fn in tests) {
    out <- eligibility_run(contract_recipes[[fn]], pm, d)
    if (is.null(out)) next
    # A deprecated name calls the test it stands for.
    called <- switch(fn, performBreuschPaganTest = "performBPTest", fn)
    expect_match(unclass(out), refusal(called), info = fn)
  }
  expect_error(performScatterDiagnostic(pm, d, "x1"), refusal("performScatterDiagnostic"))
  expect_error(runPanelTests(pm, d, "id", "time"), refusal("runPanelTests"))
  expect_error(runTimeSeriesTests(pm), refusal("runTimeSeriesTests"))
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
