test_that("performWhiteTestRobust attaches robust diagnostics", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  result <- performWhiteTestRobust(model, mtcars, bootstrap = FALSE, B = 10, ci_level = 0.9)

  expect_s3_class(result, "htest")
  expect_true("robust_details" %in% names(result))

  details <- result$robust_details
  expect_true(is.list(details))
  expect_named(details$confidence_interval$estimate, c("lower", "upper"))
  expect_false(details$bootstrap$enabled)
  expect_true(is.list(details$effect_size))
  expect_true(is.list(details$power))
})

test_that("performWhiteTestRobust produces bootstrap diagnostics when requested", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  set.seed(123)
  result <- performWhiteTestRobust(model, mtcars, bootstrap = TRUE, B = 25, ci_level = 0.9)

  details <- result$robust_details
  expect_true(details$bootstrap$enabled)
  expect_length(details$bootstrap$replicates, 25)
  expect_false(anyNA(details$bootstrap$ci))
  expect_true(all(names(details$bootstrap$ci) == c("lower", "upper")))
  expect_true(is.finite(details$bootstrap$p_value))
  expect_true("p.value_bootstrap" %in% names(result))
})

test_that("performBPTestRobust handles studentize toggle", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)

  classic <- performBPTestRobust(model, mtcars, studentize = FALSE, bootstrap = FALSE)
  student <- performBPTestRobust(model, mtcars, studentize = TRUE, bootstrap = FALSE)

  expect_false(classic$robust_details$studentized)
  expect_true(student$robust_details$studentized)
  expect_false(classic$robust_details$bootstrap$enabled)
  expect_true(is.list(student$robust_details$effect_size))

  # The old name `studentized` still works in 0.15.x, warns that it stops
  # working in 1.0.0 and returns what `studentize` returns.
  for (value in c(FALSE, TRUE)) {
    expect_warning(
      old <- performBPTestRobust(model, mtcars, studentized = value, bootstrap = FALSE),
      "`studentized` of performBPTestRobust\\(\\) is deprecated.*1\\.0\\.0.*Use `studentize`",
      class = "deprecatedWarning"
    )
    expect_identical(
      old,
      performBPTestRobust(model, mtcars, studentize = value, bootstrap = FALSE)
    )
  }
  expect_error(
    performBPTestRobust(model, mtcars, studentize = TRUE, studentized = TRUE),
    "Supply `studentize` only"
  )
})

test_that("rcalculateEffectSize supports multiple metrics", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  base_result <- performWhiteTest(model, mtcars)

  cramers <- .calculate_effect_size(base_result, model, mtcars, type = "cramers_v")
  phi <- .calculate_effect_size(base_result, model, mtcars, type = "phi")
  eta <- .calculate_effect_size(base_result, model, mtcars, type = "eta_squared")

  expect_true(cramers$effect_size >= 0)
  expect_equal(phi$type, "phi")
  expect_equal(eta$type, "eta_squared")
})

test_that("bootstrapTestStatistic returns replicates and intervals", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  set.seed(321)
  boot <- bootstrapTestStatistic(performWhiteTest, model, mtcars, B = 10)

  expect_length(boot$replicates, 10)
  expect_named(boot$ci, c("lower", "upper"))
  expect_true(is.numeric(boot$original_statistic))
})

test_that("restimate_test_power returns power diagnostics", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  base_result <- performWhiteTest(model, mtcars)
  power <- .estimate_test_power(base_result, model, mtcars)

  expect_true(is.numeric(power$power))
  expect_true(is.list(power$details))
  expect_true(is.data.frame(power$details$power_curve))
})

test_that("runAdvancedDiagnostics auto-enhances small samples", {
  small_data <- mtcars[seq_len(30), ]
  model_small <- stats::lm(mpg ~ wt + cyl, data = small_data)
  diagnostics <- runAdvancedDiagnostics(model_small, small_data,
    tests = c("white", "breusch_pagan"),
    auto_enhance = TRUE,
    bootstrap_B = 20,
    ci_level = 0.9
  )

  expect_named(diagnostics$results, c("white", "breusch_pagan"))
  expect_true(diagnostics$results$white$robust_details$bootstrap$enabled)
  expect_true(diagnostics$results$breusch_pagan$robust_details$studentized)

  large_diag <- runAdvancedDiagnostics(model_small, small_data,
    tests = "white", auto_enhance = FALSE
  )
  expect_false(large_diag$results$white$robust_details$bootstrap$enabled)
})

test_that("runAdvancedDiagnostics() runs the studentized Breusch-Pagan test whatever auto_enhance is", {
  # `auto_enhance` turns the small-sample bootstrap on and off and nothing
  # else, since 0.16.0. Before, `auto_enhance = FALSE` also turned the
  # studentization off and ran the classical test: 3.135 (p = 0.2086) here.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  model <- stats::lm(mpg ~ wt + qsec, data = mtcars)
  off <- runAdvancedDiagnostics(model, mtcars, auto_enhance = FALSE)
  entry <- off$results$breusch_pagan
  expect_true(entry$robust_details$studentized)
  expect_false(entry$robust_details$bootstrap$enabled)
  expect_equal(unname(entry$statistic), 3.0858363, tolerance = 1e-7)
  expect_equal(entry$p.value, 0.2137564, tolerance = 1e-6)
  expect_identical(entry, performBPTestRobust(model, mtcars, studentize = TRUE, bootstrap = FALSE))
  expect_false(off$results$white$robust_details$bootstrap$enabled)
  expect_false(off$metadata$auto_enhance)

  # From 50 observations neither setting bootstraps, so the two give the same
  # results.
  set.seed(5)
  d <- data.frame(x = stats::runif(60, 1, 5))
  d$y <- 1 + d$x + stats::rnorm(60, sd = d$x)
  big <- stats::lm(y ~ x, data = d)
  expect_identical(
    runAdvancedDiagnostics(big, d, auto_enhance = FALSE)$results,
    runAdvancedDiagnostics(big, d, auto_enhance = TRUE)$results
  )
})

test_that("runAdvancedDiagnostics() takes \"bp\" for \"breusch_pagan\", as its help page says", {
  # Before 0.16.0 "bp" stopped with "Unsupported test requested: bp": the names
  # asked for were compared with the values of the lookup table, not its names.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  model <- stats::lm(mpg ~ wt + qsec, data = mtcars)
  both <- runAdvancedDiagnostics(model, mtcars, tests = c("white", "breusch_pagan"), auto_enhance = FALSE)
  expect_identical(
    runAdvancedDiagnostics(model, mtcars, tests = c("white", "bp"), auto_enhance = FALSE),
    both
  )
  bp <- runAdvancedDiagnostics(model, mtcars, tests = "BP", auto_enhance = FALSE)
  expect_named(bp$results, "breusch_pagan")
  expect_identical(bp$metadata$tests, "breusch_pagan")
  expect_identical(bp$results$breusch_pagan, both$results$breusch_pagan)
  expect_identical(
    runAdvancedDiagnostics(model, mtcars, tests = c("bp", "breusch_pagan"), auto_enhance = FALSE),
    bp
  )
  # Every other name is refused as before.
  expect_error(
    runAdvancedDiagnostics(model, mtcars, tests = "koenker"),
    "Unsupported test requested: koenker",
    fixed = TRUE
  )
  expect_error(
    runAdvancedDiagnostics(model, mtcars, tests = c("bp", "foo")),
    "Unsupported test requested: foo",
    fixed = TRUE
  )
})

test_that("rrunAdvancedDiagnostics() warns, names its new name and keeps its value", {
  small_data <- mtcars[seq_len(30), ]
  model_small <- stats::lm(mpg ~ wt + cyl, data = small_data)
  set.seed(15)
  expect_warning(
    old <- rrunAdvancedDiagnostics(model_small, small_data, bootstrap_B = 20),
    "'rrunAdvancedDiagnostics' is deprecated.*1\\.0\\.0.*Use 'runAdvancedDiagnostics' instead",
    class = "deprecatedWarning"
  )
  set.seed(15)
  expect_identical(old, runAdvancedDiagnostics(model_small, small_data, bootstrap_B = 20))
})

test_that("rvalidate_against_reference reports comparison status", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  comparison <- .validate_against_reference("breusch_pagan", model, mtcars)

  expect_true("status" %in% names(comparison))
  expect_true(comparison$status %in% c("ok", "mismatch", "skipped"))
})

test_that("bootstrap helper converges as replication count grows", {
  set.seed(404)
  df <- data.frame(y = rnorm(60), x = rnorm(60))
  model <- lm(y ~ x, data = df)
  stat_fun <- function(model, data) {
    res <- residuals(model)
    structure(list(statistic = c(mean_resid_sq = mean(res^2))), class = "htest")
  }
  set.seed(123)
  boot_small <- bootstrapTestStatistic(stat_fun, model, df, B = 40)
  set.seed(123)
  boot_large <- bootstrapTestStatistic(stat_fun, model, df, B = 400)
  err_small <- abs(mean(boot_small$replicates) - boot_small$original_statistic)
  err_large <- abs(mean(boot_large$replicates) - boot_large$original_statistic)
  expect_true(err_large <= err_small + 1e-6)
  expect_equal(length(boot_large$replicates), 400L)
  expect_equal(boot_large$effective_samples, 400L)
})

test_that("power estimator recommends at least the observed sample size", {
  set.seed(505)
  df <- data.frame(y = rnorm(50), x = rnorm(50))
  model <- lm(y ~ x, data = df)
  base <- performWhiteTest(model, df)
  power <- .estimate_test_power(base, model, df, target_power = 0.9)
  expect_gte(power$recommended_n, nrow(df))
  expect_true(power$power < 1)
})
