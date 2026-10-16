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
  # The baseline is the default battery of runHeteroTests(), White and
  # Koenker, since 0.15.0; the classical Breusch-Pagan test assumes normal
  # errors.
  expect_true(all(c("koenker", "white") %in% recs$test))
  expect_false("breusch_pagan" %in% recs$test)
  expect_true("wild_bootstrap" %in% recs$test)
  expect_true("high_dimensional" %in% recs$test)
  expect_true("spatial_hetero" %in% recs$test)
})

test_that("a very large profile lists Koenker's test once, with the baseline rationale", {
  profile <- list(
    size_bucket = "very_large",
    high_dimensional = FALSE,
    detected_spatial_fields = FALSE,
    missingness = list(overall_rate = 0),
    numeric_distribution = data.frame(variable = character(), shape = character())
  )

  recs <- suggestDiagnosticsForProfile(profile)
  expect_setequal(recs$test, c("koenker", "white", "quantile_regression"))
  expect_identical(anyDuplicated(recs$test), 0L)
  expect_identical(
    recs$rationale[recs$test == "koenker"],
    paste(
      "Baseline regression-based heteroscedasticity check, studentized so",
      "that it does not assume normal errors."
    )
  )
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

test_that("a small sample is not told to rely on the wild bootstrap", {
  # In inst/validation/wild-multiplier-size.R the wild bootstrap rejects a true
  # null hypothesis 5.9% to 8.2% of the time at the 5% level with skewed errors
  # at 30 to 100 observations, with either multiplier. Before 0.15.0 the
  # warning for a small sample told the user to rely on it.
  profile <- list(size_bucket = "small", missingness = list(overall_rate = 0))
  interpretations <- list(results = data.frame(decision = character(), method = character()))
  warnings <- warnDiagnosticAssumptions(profile, interpretations, NULL, NULL, list(toy = 1))
  small <- grep("^Sample size is limited", warnings, value = TRUE)
  expect_length(small, 1L)
  expect_false(grepl("rely on", small))
  expect_match(
    small, "with skewed errors the wild bootstrap does not hold its level", fixed = TRUE
  )
})
