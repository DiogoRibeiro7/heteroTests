library(testthat)

# ---------------------------------------------------------------------------
# Survey designs.
#
# The help page of runSurveyHeteroTests() promised a survey-weighted fit up to
# 0.12.0. The weights never reached lm(): they were passed as a local variable,
# lm() found stats::weights instead and failed, and a tryCatch() refitted
# without them. runHeteroTests(formula, design) took another route, fitted the
# model with survey::svyglm() and tested that fit, whose residuals() carry the
# square roots of the sampling weights.
#
# Sampling weights are not precision weights, so the second route rejected a
# homoscedastic model whenever the weights varied with the regressors. Both
# routes now do what the first one always did, an ordinary least-squares fit,
# and say so. The guards at the end are the measurements behind that choice;
# inst/validation/survey-designs-size.R has the full study.
# ---------------------------------------------------------------------------

# A sample whose chance of selection depends on the regressor alone, from a
# population with constant error variance.
make_survey_sample <- function(seed, n = 300, equal = FALSE) {
  set.seed(seed)
  size <- 6 * n
  x <- rnorm(size)
  e <- rnorm(size)
  relative <- if (equal) rep(1, size) else exp(0.9 * pmax(pmin(x, 2), -2))
  chance <- pmin(1, relative * n / sum(relative))
  taken <- runif(size) < chance
  data.frame(y = 1 + 2 * x[taken] + e[taken], x = x[taken], w = 1 / chance[taken])
}

design_warnings <- function(expr) {
  seen <- character()
  withCallingHandlers(
    expr,
    warning = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  seen[grepl("survey design", seen, fixed = TRUE)]
}

test_that("the wrapper runs the ordinary tests on an unweighted fit", {
  skip_if_not_installed("survey")
  d <- make_survey_sample(1)
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_warning(
    res <- runSurveyHeteroTests(y ~ x, design, tests = c("white", "koenker")),
    "does not use the sampling weights"
  )
  ols <- lm(y ~ x, data = d)
  expect_equal(
    unname(res$white$statistic),
    unname(performWhiteTest(ols, d)$statistic),
    tolerance = 1e-10
  )
  expect_equal(
    unname(res$koenker$statistic),
    unname(performKoenkerTest(ols, d)$statistic),
    tolerance = 1e-10
  )
  fitted_model <- attr(res, "model")
  expect_false(inherits(fitted_model, "svyglm"))
  expect_null(fitted_model$weights)
  expect_equal(coef(fitted_model), coef(ols), tolerance = 1e-12)
})

test_that("the warning names what the design has", {
  skip_if_not_installed("survey")
  d <- make_survey_sample(2, equal = TRUE)
  d$cluster <- rep(seq_len(ceiling(nrow(d) / 10)), each = 10)[seq_len(nrow(d))]

  equal_weights <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  expect_length(
    design_warnings(runSurveyHeteroTests(y ~ x, equal_weights, tests = "koenker")),
    0L
  )

  clustered <- survey::svydesign(ids = ~cluster, weights = ~w, data = d)
  seen <- design_warnings(runSurveyHeteroTests(y ~ x, clustered, tests = "koenker"))
  expect_length(seen, 1L)
  expect_match(seen, "does not use the clusters")

  d$w <- rep(c(1, 2), length.out = nrow(d))
  both <- survey::svydesign(ids = ~cluster, weights = ~w, data = d)
  expect_match(
    design_warnings(runSurveyHeteroTests(y ~ x, both, tests = "koenker")),
    "sampling weights or clusters"
  )
})

test_that("runHeteroTests() and runDiagnostics() treat a design the same way", {
  skip_if_not_installed("survey")
  d <- make_survey_sample(3)
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  from_wrapper <- suppressWarnings(
    runSurveyHeteroTests(y ~ x, design, tests = "koenker")
  )

  expect_warning(
    direct <- runHeteroTests(
      y ~ x, design,
      tests = "koenker", use_cache = FALSE, progress = FALSE
    ),
    "runHeteroTests\\(\\) does not use the sampling weights"
  )
  expect_identical(direct$koenker$statistic, from_wrapper$koenker$statistic)
  expect_false(inherits(attr(direct, "model"), "svyglm"))

  expect_warning(
    broad <- runDiagnostics(
      y ~ x, design,
      tests = "koenker", use_cache = FALSE, progress = FALSE
    ),
    "runDiagnostics\\(\\) does not use the sampling weights"
  )
  expect_identical(broad$koenker$statistic, from_wrapper$koenker$statistic)

  expect_error(
    runHeteroTests(lm(y ~ x, data = d), design),
    "provide the model as a formula"
  )
})

test_that("a survey-weighted fit is refused", {
  skip_if_not_installed("survey")
  d <- make_survey_sample(4)
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  fit <- survey::svyglm(y ~ x, design = design)
  expect_error(performKoenkerTest(fit, d), "svyglm")
  expect_error(performWhiteTest(fit, d), "svyglm")
  expect_error(performBPTest(fit, d), "svyglm")
  expect_error(performGQTest(fit, d, "x"), "svyglm")
  expect_error(
    runHeteroTests(fit, d, use_cache = FALSE, progress = FALSE),
    "no design-based test"
  )
})

# --- simulated guards -------------------------------------------------------

test_that("the wrapper holds its level when selection depends on the regressors", {
  skip_on_cran()
  skip_if_not_installed("survey")
  # Homoscedastic errors; selection depends on x alone. 150 replications put
  # the Monte Carlo standard error at 1.8%.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  p <- vapply(seq_len(150), function(i) {
    d <- make_survey_sample(3000 + i)
    design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
    suppressWarnings(
      runSurveyHeteroTests(y ~ x, design, tests = "koenker", progress = FALSE)
    )$koenker$p.value
  }, numeric(1))
  expect_lt(mean(p < 0.05), 0.15)
})

test_that("sampling weights read as precision weights reject a constant variance", {
  skip_on_cran()
  # Why the weights are not passed to lm(): on the same samples, the fit that
  # uses them is tested on residuals multiplied by sqrt(w), which are
  # heteroscedastic because the weights vary with x.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  p <- vapply(seq_len(60), function(i) {
    d <- make_survey_sample(3000 + i)
    weighted <- lm(y ~ x, data = d, weights = w)
    suppressWarnings(performKoenkerTest(weighted, d))$p.value
  }, numeric(1))
  expect_gt(mean(p < 0.05), 0.9)
})
