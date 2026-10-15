# Uniform input validation: every exported test, called as in
# helper-test-recipes.R, meets the same bad input with the same error. The
# fixed set of bad inputs is a glm that is not a least-squares fit, binomial
# and Poisson, a model with an NA coefficient, a data frame that lacks rows
# the model was fitted to, one that lacks a variable of the model, and, when
# the survey package is installed, a survey::svyglm() fit.
#
# Before 0.15.0 most tests returned a value for a glm of any family, and
# seven returned one for some of the other inputs. Refusing them all was
# decided on 2026-10-15. A glm with the Gaussian family and the identity link
# is a least-squares fit and is accepted: the tests return for it what they
# return for the lm (below).

validation_inputs <- function() {
  s <- contract_data()
  d <- s$d
  d$x1_twice <- 2 * d$x1
  d$high <- as.integer(d$y > stats::median(d$y))
  d$count <- stats::rpois(nrow(d), exp(0.2 + 0.3 * d$x1))
  list(
    glm = list(m = glm(high ~ x1 + x2, data = d, family = binomial), d = d),
    poisson = list(m = glm(count ~ x1 + x2, data = d, family = poisson), d = d),
    na_coefficient = list(m = lm(y ~ x1 + x2 + x1_twice, data = d), d = d),
    rows_missing = list(m = s$m, d = s$d[1:60, ]),
    variable_missing = list(m = s$m, d = s$d[, setdiff(names(s$d), "x2")])
  )
}

# A Gaussian svyglm() fit, whose residuals are scaled by the sampling weights.
# Before 0.15.0 performRESETTest() returned a value for one, of any family.
validation_survey_input <- function() {
  s <- contract_data()
  d <- s$d
  d$sw <- 1 / d$x1^2
  design <- survey::svydesign(ids = ~1, weights = ~sw, data = d)
  list(svyglm = list(m = survey::svyglm(y ~ x1 + x2, design = design), d = d))
}

validation_messages <- c(
  glm = "needs a model fitted by stats::lm\\(\\)",
  poisson = "needs a model fitted by stats::lm\\(\\)",
  na_coefficient = "Model coefficients must be finite",
  rows_missing = paste0(
    "requires `data` (to contain the rows used to fit the model|",
    "with [0-9]+ observations to match the fitted model)"
  ),
  variable_missing = "The following variables are missing from",
  svyglm = "The model is a survey::svyglm\\(\\) fit"
)

# These three refuse every glm, the Gaussian one included.
validation_glm_refused <- c(
  "performWhiteTestStreaming", "performHarrisonMcCabeTest", "performVarianceFormTest"
)

# These read the weights of an lm, to refit with them or, in the quantile
# regression test, to refuse a weighted fit. Before 0.15.0 they took a weighted
# Gaussian glm for an unweighted fit; since then they read its prior weights,
# as decided on 2026-10-15, and give what they give for the weighted lm.
validation_weight_readers <- c(
  "performGQTest", "performRESETTest", "performWhiteTestBootstrap", "performWildBootstrapTest",
  "performQuantileRegressionTest"
)

validation_outcome <- function(recipe, m, d) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  tryCatch(
    {
      suppressMessages(suppressWarnings(recipe(m, d)))
      "value"
    },
    error = function(e) conditionMessage(e),
    skip = function(e) NULL
  )
}

validation_expect_refused <- function(inputs) {
  tests <- setdiff(names(contract_recipes), "performBoxMTest")
  for (input in names(inputs)) {
    for (fn in tests) {
      outcome <- validation_outcome(contract_recipes[[fn]], inputs[[input]]$m, inputs[[input]]$d)
      if (is.null(outcome)) next
      expect_match(outcome, validation_messages[[input]], info = paste(fn, "on", input))
    }
  }
}

test_that("every exported test meets each bad input with the same error", {
  validation_expect_refused(validation_inputs())
})

test_that("every exported test refuses a survey::svyglm() fit", {
  skip_if_not_installed("survey")
  validation_expect_refused(validation_survey_input())
})

# A glm with the Gaussian family and the identity link has the coefficients,
# fitted values and residuals of lm(), up to rounding, so a test returns for
# it what it returns for the lm. The comparison is of the statistic and the
# p-value, signed, and of the parameter, on the seed the recipe is run with.
validation_compare_fits <- function(recipe, a, b, d) {
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(m) {
    set.seed(11)
    tryCatch(suppressMessages(suppressWarnings(recipe(m, d))),
             error = function(e) structure(conditionMessage(e), class = "refused"),
             skip = function(e) NULL)
  }
  list(a = run(a), b = run(b))
}

validation_gaussian_glm_cases <- function() {
  s <- contract_data()
  d <- s$d
  d$w <- 1 / d$x1^2
  list(
    unweighted = list(lm = s$m, glm = glm(y ~ x1 + x2, data = d), d = d),
    weighted = list(lm = lm(y ~ x1 + x2, data = d, weights = w),
                    glm = glm(y ~ x1 + x2, data = d, weights = w), d = d)
  )
}

test_that("a Gaussian glm with the identity link gives the result of the lm", {
  cases <- validation_gaussian_glm_cases()
  tests <- setdiff(names(contract_recipes), c("performBoxMTest", validation_glm_refused))
  for (case in names(cases)) {
    for (fn in tests) {
      out <- validation_compare_fits(contract_recipes[[fn]], cases[[case]]$lm,
                                     cases[[case]]$glm, cases[[case]]$d)
      if (is.null(out$a) || is.null(out$b)) next
      info <- paste(fn, "on the", case, "Gaussian glm")
      # A test that refuses the lm, such as the quantile regression test on a
      # weighted fit, refuses the glm too, with the same message.
      if (inherits(out$a, "refused")) {
        expect_identical(out$b, out$a, info = info)
        next
      }
      expect_false(inherits(out$b, "refused"), info = paste(info, out$b))
      # The Fligner-Killeen statistic ranks |e - median(e)| within groups. With
      # an even number of residuals the two in the middle are tied at exactly
      # half their difference, and which of them ranks first is decided by
      # rounding, which differs between the two fits by about 1e-15. That moves
      # the statistic by about 1e-6 of its value.
      tolerance <- if (fn == "performFlignerKilleenTest") 1e-4 else 1e-8
      expect_equal(unname(out$b$statistic), unname(out$a$statistic),
                   tolerance = tolerance, info = info)
      expect_equal(out$b$p.value, out$a$p.value, tolerance = tolerance, info = info)
      expect_equal(unname(out$b$parameter), unname(out$a$parameter), info = info)
    }
  }
})

# The five tests that read the weights of an lm read the prior weights of a
# Gaussian glm, decided on 2026-10-15, and give for it what they give for
# lm(..., weights = w): the statistic and the p-value to 1e-8, the resampling
# tests from the same seed and the same B. The quantile regression test
# refuses a weighted lm, and refuses the weighted glm with the same message.
# Before 0.15.0 they took the glm for an unweighted fit, and RESET returned a
# negative F for it.
validation_weighted_designs <- function() {
  list(
    two_regressors = local({
      set.seed(101)
      n <- 60
      d <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n))
      d$y <- 1 + d$x1 - d$x2 + rnorm(n, sd = d$x1)
      d$w <- 1 / d$x1^2
      list(d = d, f = y ~ x1 + x2)
    }),
    three_regressors = local({
      set.seed(202)
      n <- 150
      d <- data.frame(x1 = rnorm(n), x2 = rexp(n), x3 = runif(n))
      d$y <- 2 + d$x1 + 0.5 * d$x2 - d$x3 + rnorm(n, sd = exp(0.3 * d$x1))
      d$w <- runif(n, 0.2, 3)
      list(d = d, f = y ~ x1 + x2 + x3)
    }),
    log_response_factor = local({
      set.seed(303)
      n <- 45
      d <- data.frame(x1 = runif(n, 1, 3), g = factor(rep(c("a", "b", "c"), length.out = n)))
      d$y <- exp(0.5 + 0.2 * d$x1 + rnorm(n, sd = 0.3))
      d$w <- rexp(n) + 0.1
      list(d = d, f = log(y) ~ x1 + g)
    }),
    integer_weights = local({
      set.seed(404)
      n <- 200
      d <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
      d$y <- d$x1 + rnorm(n, sd = 1 + abs(d$x2))
      d$w <- sample(1:5, n, replace = TRUE)
      list(d = d, f = y ~ x1 + x2)
    })
  )
}

test_that("the tests that read the weights give for a weighted Gaussian glm what they give for the lm", {
  skip_if_not_installed("quantreg")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  calls <- list(
    performGQTest = function(m, d) performGQTest(m, d, order_by = "x1"),
    performRESETTest = function(m, d) performRESETTest(m, d),
    performWhiteTestBootstrap = function(m, d) {
      set.seed(7)
      performWhiteTestBootstrap(m, d, B = 99)
    },
    performWildBootstrapTest = function(m, d) {
      set.seed(7)
      performWildBootstrapTest(m, d, B = 99, progress = FALSE)
    },
    performWildBootstrapTest_mammen = function(m, d) {
      set.seed(8)
      performWildBootstrapTest(m, d, B = 99, distribution = "mammen", progress = FALSE)
    },
    performQuantileRegressionTest = function(m, d) performQuantileRegressionTest(m, d)
  )
  expect_setequal(unique(sub("_mammen$", "", names(calls))), validation_weight_readers)
  run <- function(fn, m, d) {
    tryCatch(suppressWarnings(fn(m, d)),
             error = function(e) structure(conditionMessage(e), class = "refused"))
  }
  compared <- 0L
  designs <- validation_weighted_designs()
  for (design in names(designs)) {
    d <- designs[[design]]$d
    f <- designs[[design]]$f
    fit_lm <- eval(bquote(lm(.(f), data = d, weights = w)))
    fit_glm <- eval(bquote(glm(.(f), data = d, weights = w)))
    for (fn in names(calls)) {
      info <- paste(fn, "on", design)
      a <- run(calls[[fn]], fit_lm, d)
      b <- run(calls[[fn]], fit_glm, d)
      if (inherits(a, "refused")) {
        expect_identical(b, a, info = info)
        next
      }
      expect_false(inherits(b, "refused"), info = paste(info, b))
      expect_equal(unname(b$statistic), unname(a$statistic), tolerance = 1e-8, info = info)
      expect_equal(b$p.value, a$p.value, tolerance = 1e-8, info = info)
      compared <- compared + 1L
    }
  }
  # GQ, RESET and the two wild bootstraps on all four designs, and the White
  # bootstrap on the three with enough observations for it.
  expect_identical(compared, 19L)
  d <- designs$integer_weights$d
  expect_error(
    performQuantileRegressionTest(glm(y ~ x1 + x2, data = d, weights = w), d),
    "performQuantileRegressionTest() does not support weighted fits", fixed = TRUE
  )
})

test_that("the three tests that refuse every glm refuse the Gaussian one too", {
  cases <- validation_gaussian_glm_cases()
  for (fn in validation_glm_refused) {
    out <- validation_outcome(contract_recipes[[fn]], cases$unweighted$glm, cases$unweighted$d)
    expect_match(out, "needs a model fitted by stats::lm\\(\\)", info = fn)
  }
})

test_that("a glm of another family is refused whatever its link", {
  s <- contract_data()
  d <- s$d
  d$count <- stats::rpois(nrow(d), exp(0.2 + 0.3 * d$x1))
  fits <- list(
    poisson = glm(count ~ x1 + x2, data = d, family = poisson),
    gaussian_log = glm(count + 1 ~ x1 + x2, data = d, family = gaussian(link = "log"))
  )
  for (nm in names(fits)) {
    expect_error(
      performKoenkerTest(fits[[nm]], d),
      "performKoenkerTest\\(\\) needs a model fitted by stats::lm\\(\\): a glm with the",
      info = nm
    )
  }
})

test_that("a model-only test checks the data it is given against the model", {
  s <- contract_data()
  m <- s$m
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  # The model frame holds the terms, not the variables, and is accepted.
  logged <- lm(log(y + 10) ~ x1 + x2, data = s$d)
  expect_identical(
    performSpearmanTest(logged, stats::model.frame(logged)),
    performSpearmanTest(logged)
  )
  # So are the data with rows the fit did not use, in any order.
  extra <- rbind(s$d, s$d[1:5, ])
  rownames(extra) <- c(rownames(s$d), paste0("extra", 1:5))
  expect_identical(performHarveyTest(m, extra[sample(nrow(extra)), ]), performHarveyTest(m))
  expect_error(
    performArchLMTest(m, s$d[-7, ], lags = 2),
    "ARCH LM requires `data` to contain the rows used to fit the model. Missing rows: 7"
  )
})
