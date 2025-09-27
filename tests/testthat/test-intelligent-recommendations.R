skip_on_cran()

set.seed(123)

test_that("dataset characteristics are profiled", {
  sample_data <- mtcars
  sample_data$lat <- seq_len(nrow(sample_data))
  sample_data$lat[1] <- NA
  profile <- analyseDatasetCharacteristics(sample_data, response = "mpg")

  expect_equal(profile$n_obs, nrow(sample_data))
  expect_equal(profile$n_vars, ncol(sample_data))
  expect_true(is.data.frame(profile$numeric_distribution))
  expect_true(profile$missingness$overall_rate > 0)
  expect_true(profile$detected_spatial_fields)
})

test_that("diagnostic suggestions reflect profile", {
  profile <- list(
    size_bucket = "small",
    high_dimensional = TRUE,
    detected_spatial_fields = TRUE,
    missingness = list(overall_rate = 0.1),
    numeric_distribution = data.frame(
      variable = "x",
      shape = "strong_right_skew",
      stringsAsFactors = FALSE
    )
  )

  recs <- suggestDiagnosticsForProfile(profile)
  expect_true("breusch_pagan" %in% recs$test)
  expect_true("wild_bootstrap" %in% recs$test)
  expect_true("high_dimensional" %in% recs$test)
  expect_true("spatial_hetero" %in% recs$test)
})

test_that("interpretation produces confidence labels", {
  toy_test <- structure(
    list(
      statistic = c(stat = 5.3),
      p.value = 0.004,
      method = "Toy diagnostic"
    ),
    class = "htest"
  )
  interpretations <- interpretHeteroTestResults(list(toy = toy_test), alpha = 0.05)
  expect_equal(nrow(interpretations$results), 1)
  expect_equal(interpretations$results$decision, "Heteroscedasticity detected")
  expect_equal(interpretations$results$confidence, "High")
  expect_match(interpretations$results$interpretation, "Reject")
})

test_that("recommendation engine returns structured guidance", {
  model <- stats::lm(mpg ~ wt + hp, data = mtcars)
  recommendations <- generateHeteroRecommendations(model, mtcars, include_report = FALSE)

  expect_s3_class(recommendations, "hetero_recommendation_report")
  expect_true("profile" %in% names(recommendations))
  expect_true(nrow(recommendations$recommendations) >= 2)
  expect_true(is.data.frame(recommendations$decision_tree))
  expect_true(is.list(recommendations$interpretations))
  expect_true(is.list(recommendations$remediation))
})
