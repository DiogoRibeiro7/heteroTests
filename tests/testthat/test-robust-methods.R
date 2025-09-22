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

test_that("performBPTestRobust handles studentized toggle", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)

  classic <- performBPTestRobust(model, mtcars, studentized = FALSE, bootstrap = FALSE)
  student <- performBPTestRobust(model, mtcars, studentized = TRUE, bootstrap = FALSE)

  expect_false(classic$robust_details$studentized)
  expect_true(student$robust_details$studentized)
  expect_false(classic$robust_details$bootstrap$enabled)
  expect_true(is.list(student$robust_details$effect_size))
})

test_that("rcalculateEffectSize supports multiple metrics", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  base_result <- performWhiteTest(model, mtcars)

  cramers <- rcalculateEffectSize(base_result, model, mtcars, type = "cramers_v")
  phi <- rcalculateEffectSize(base_result, model, mtcars, type = "phi")
  eta <- rcalculateEffectSize(base_result, model, mtcars, type = "eta_squared")

  expect_true(cramers$effect_size >= 0)
  expect_equal(phi$type, "phi")
  expect_equal(eta$type, "eta_squared")
})

test_that("rbootstrap_test_statistic returns replicates and intervals", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  set.seed(321)
  boot <- rbootstrap_test_statistic(performWhiteTest, model, mtcars, B = 10)

  expect_length(boot$replicates, 10)
  expect_named(boot$ci, c("lower", "upper"))
  expect_true(is.numeric(boot$original_statistic))
})

test_that("restimate_test_power returns power diagnostics", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  base_result <- performWhiteTest(model, mtcars)
  power <- restimate_test_power(base_result, model, mtcars)

  expect_true(is.numeric(power$power))
  expect_true(is.list(power$details))
  expect_true(is.data.frame(power$details$power_curve))
})

test_that("rrunAdvancedDiagnostics auto-enhances small samples", {
  small_data <- mtcars[seq_len(30), ]
  model_small <- stats::lm(mpg ~ wt + cyl, data = small_data)
  diagnostics <- rrunAdvancedDiagnostics(model_small, small_data,
    tests = c("white", "breusch_pagan"),
    auto_enhance = TRUE,
    bootstrap_B = 20,
    ci_level = 0.9
  )

  expect_named(diagnostics$results, c("white", "breusch_pagan"))
  expect_true(diagnostics$results$white$robust_details$bootstrap$enabled)
  expect_true(diagnostics$results$breusch_pagan$robust_details$studentized)

  large_diag <- rrunAdvancedDiagnostics(model_small, small_data,
    tests = "white", auto_enhance = FALSE
  )
  expect_false(large_diag$results$white$robust_details$bootstrap$enabled)
})

test_that("rvalidate_against_reference reports comparison status", {
  model <- stats::lm(mpg ~ wt + cyl, data = mtcars)
  comparison <- rvalidate_against_reference("breusch_pagan", model, mtcars)

  expect_true("status" %in% names(comparison))
  expect_true(comparison$status %in% c("ok", "mismatch", "skipped"))
})
