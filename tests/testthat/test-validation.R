test_that("rvalidateModelInputs accepts well-behaved lm objects", {
  model <- stats::lm(mpg ~ wt, data = mtcars)
  expect_invisible(heteroTests:::rvalidateModelInputs(model, "Example Test"))
})

test_that("rvalidateModelInputs accepts glm objects", {
  model <- stats::glm(mpg ~ wt, data = mtcars, family = stats::gaussian())
  expect_invisible(heteroTests:::rvalidateModelInputs(model, "GLM Test"))
})

test_that("rvalidateModelInputs catches invalid classes", {
  expect_error(
    heteroTests:::rvalidateModelInputs(list(), "Bad Model"),
    "Provide an object fitted with stats::lm() or stats::glm() before running Bad Model (received class: list).",
    fixed = TRUE
  )
})

test_that("rvalidateModelInputs enforces minimum sample size", {
  small <- mtcars[seq_len(5), ]
  model <- stats::lm(mpg ~ wt, data = small)
  expect_error(
    heteroTests:::rvalidateModelInputs(model, "Small Sample", min_obs = 6),
    "Only 5 observations detected but Small Sample requires at least 6. Provide more data or choose a different diagnostic.",
    fixed = TRUE
  )
})

test_that("rvalidateModelInputs detects perfect fits", {
  df <- data.frame(x = 1:5, y = 2 * (1:5) + 3)
  model <- stats::lm(y ~ x, data = df)
  expect_error(
    heteroTests:::rvalidateModelInputs(model, "Perfect Fit", min_obs = 5),
    "Model appears perfectly explained",
    fixed = FALSE
  )
})

test_that("rvalidateModelInputs requires finite residuals", {
  model <- stats::lm(mpg ~ wt, data = mtcars)
  model$residuals[1] <- NA_real_
  expect_error(
    heteroTests:::rvalidateModelInputs(model, "Residual Check"),
    "Model residuals must be finite before running Residual Check.",
    fixed = TRUE
  )
})

test_that("rvalidateDataInputs validates basic data frames", {
  expect_invisible(
    heteroTests:::rvalidateDataInputs(mtcars, required_vars = c("mpg", "wt"), min_obs = 10)
  )
})

test_that("rvalidateDataInputs rejects non-data frames", {
  expect_error(
    heteroTests:::rvalidateDataInputs(matrix(1, nrow = 3, ncol = 2)),
    "Expected a data.frame for input data; coerce your object with as.data.frame() before running diagnostics.",
    fixed = TRUE
  )
})

test_that("rvalidateDataInputs checks required variables", {
  expect_error(
    heteroTests:::rvalidateDataInputs(mtcars, required_vars = c("mpg", "missing_col")),
    "The following variables are missing from input data: missing_col. Verify column names with names(data).",
    fixed = TRUE
  )
})

test_that("rvalidateDataInputs enforces minimum observations", {
  tiny <- mtcars[seq_len(3), ]
  expect_error(
    heteroTests:::rvalidateDataInputs(tiny, min_obs = 5),
    "Input Data contains only 3 observations but at least 5 are required. Add more rows or reduce the minimum threshold.",
    fixed = TRUE
  )
})

test_that("rvalidateDataInputs warns about duplicate column names", {
  dup <- mtcars[seq_len(5), c("mpg", "wt")]
  names(dup) <- c("mpg", "mpg")
  expect_warning(heteroTests:::rvalidateDataInputs(dup, required_vars = "mpg", min_obs = 2))
})

test_that("rhandleMissingValues removes rows with missing data", {
  df <- data.frame(y = c(1, 2, NA), x = c(3, NA, 5))
  result <- expect_warning(
    heteroTests:::rhandleMissingValues(df, c("y", "x")),
    "Removed 2 observations due to missing values in y, x"
  )
  expect_equal(nrow(result$data), 1)
  expect_equal(result$removed_cases, c(2L, 3L))
  expect_equal(result$removed_count, 2L)
  expect_equal(result$removed_fraction, 2 / 3)
  expect_equal(result$removed_variables, c("y", "x"))
  expect_equal(result$loss_message, "Missing values detected in y, x. 2 observations removed.")
})

test_that("rhandleMissingValues fails when strategy is 'fail'", {
  df <- data.frame(y = c(1, 2, NA), x = c(3, NA, 5))
  expect_error(
    heteroTests:::rhandleMissingValues(df, c("y", "x"), strategy = "fail"),
    "Missing values detected in y, x. 2 observations removed."
  )
})

test_that("rhandleMissingValues returns full data when nothing is missing", {
  df <- data.frame(y = c(1, 2, 3), x = c(3, 4, 5))
  result <- heteroTests:::rhandleMissingValues(df, c("y", "x"))
  expect_equal(result$data, df)
  expect_equal(result$removed_count, 0L)
  expect_equal(result$removed_cases, integer(0))
  expect_equal(result$loss_message, NULL)
})

test_that("rhandleMissingValues validates requested variables", {
  df <- data.frame(y = 1:3, x = 1:3)
  expect_error(
    heteroTests:::rhandleMissingValues(df, c("y", "z")),
    "Variable 'z' not found in the supplied data.",
    fixed = FALSE
  )
})

test_that("rvalidateDistributionalAssumptions confirms satisfied checks", {
  set.seed(123)
  df <- data.frame(
    normal = rnorm(100),
    positive = rexp(100, rate = 1),
    varied = rnorm(100)
  )
  res <- heteroTests:::rvalidateDistributionalAssumptions(
    df,
    assumptions = list(
      normality = list(variables = "normal", alpha = 0.01),
      positive = list(variables = "positive", test_name = "Demo Test"),
      variation = list(variables = c("normal", "varied")),
      outliers = list(variables = "varied", threshold = 6)
    )
  )
  expect_true(res$passed)
  expect_length(res$messages, 0)
})

test_that("rvalidateDistributionalAssumptions detects non-normal data", {
  df <- data.frame(x = rep(c(-4, 4), each = 25))
  res <- heteroTests:::rvalidateDistributionalAssumptions(
    df,
    assumptions = list(normality = list(variables = "x", alpha = 0.05))
  )
  expect_false(res$passed)
  expect_true(any(grepl("Severe non-normality", res$messages)))
})

test_that("rvalidateDistributionalAssumptions flags non-positive values", {
  df <- data.frame(x = c(1, 2, -1, 4))
  res <- heteroTests:::rvalidateDistributionalAssumptions(
    df,
    assumptions = list(positive = list(variables = "x", test_name = "Park Test"))
  )
  expect_false(res$passed)
  expect_true(any(grepl("requires strictly positive data", res$messages)))
})

test_that("rvalidateDistributionalAssumptions checks for insufficient variation", {
  df <- data.frame(x = rep(5, 10))
  res <- heteroTests:::rvalidateDistributionalAssumptions(
    df,
    assumptions = list(variation = list(variables = "x", tolerance = 0))
  )
  expect_false(res$passed)
  expect_true(any(grepl("near-zero variance", res$messages)))
})

test_that("rvalidateDistributionalAssumptions highlights extreme outliers", {
  df <- data.frame(x = c(rep(0, 20), 25))
  res <- heteroTests:::rvalidateDistributionalAssumptions(
    df,
    assumptions = list(outliers = list(variables = "x", threshold = 3))
  )
  expect_false(res$passed)
  expect_true(any(grepl("Extreme outliers", res$messages)))
})

test_that("rvalidateGroupingVariable validates healthy grouping variables", {
  df <- data.frame(
    y = rnorm(6),
    grp = factor(rep(letters[1:3], each = 2))
  )
  res <- heteroTests:::rvalidateGroupingVariable(df, "grp", min_group_size = 2)
  expect_true(res$passed)
  expect_equal(res$details$n_groups, 3L)
})

test_that("rvalidateGroupingVariable catches invalid grouping classes", {
  df <- data.frame(y = 1:6, grp = 1:6)
  res <- heteroTests:::rvalidateGroupingVariable(df, "grp")
  expect_false(res$passed)
  expect_true(any(grepl("Grouping variable", res$messages)))
})

test_that("rvalidateGroupingVariable enforces group sizes", {
  df <- data.frame(
    y = rnorm(5),
    grp = factor(c("a", "a", "a", "b", "b"))
  )
  res <- heteroTests:::rvalidateGroupingVariable(df, "grp", min_group_size = 3)
  expect_false(res$passed)
  expect_true(any(grepl("Group 'b' has", res$messages)))
})

test_that("rvalidateGroupingVariable requires multiple groups", {
  df <- data.frame(y = 1:4, grp = factor(rep("a", 4)))
  res <- heteroTests:::rvalidateGroupingVariable(df, "grp", min_groups = 2)
  expect_false(res$passed)
  expect_true(any(grepl("has only", res$messages)))
})

test_that("rvalidateTestRequirements aggregates Bartlett checks", {
  df <- data.frame(
    x = rep(c(-4, 4), each = 25),
    y = rnorm(50),
    grp = factor(rep(c("neg", "pos"), each = 25))
  )
  model <- stats::lm(y ~ x, data = df)
  res <- heteroTests:::rvalidateTestRequirements(
    "Bartlett",
    model = model,
    data = df,
    variables = "x",
    group_var = "grp",
    alpha = 0.05
  )
  expect_false(res$passed)
  expect_true(any(grepl("Severe non-normality", res$messages)))
})

test_that("rvalidateTestRequirements enforces Park test positivity", {
  df <- data.frame(y = 1:5, suspect = c(1, 2, 3, -1, 5))
  model <- stats::lm(y ~ suspect, data = df)
  res <- heteroTests:::rvalidateTestRequirements(
    "Park",
    model = model,
    data = df,
    suspected_var = "suspect"
  )
  expect_false(res$passed)
  expect_true(any(grepl("requires strictly positive data", res$messages)))
})

test_that("rvalidateTestRequirements checks grouping requirements", {
  df <- data.frame(
    y = rnorm(6),
    grp = factor(c("a", "a", "a", "b", "b", "b"))
  )
  model <- stats::lm(y ~ grp, data = df)
  res <- heteroTests:::rvalidateTestRequirements(
    "Levene",
    model = model,
    data = df,
    group_var = "grp",
    min_group_size = 4
  )
  expect_false(res$passed)
  expect_true(any(grepl("Group 'a' has", res$messages)))
})

test_that("rvalidateTestRequirements detects insufficient bootstrap variation", {
  df <- data.frame(y = 1:5, x = 1:5)
  model <- stats::lm(y ~ x, data = df)
  res <- heteroTests:::rvalidateTestRequirements(
    "Residual Bootstrap",
    model = model,
    data = df
  )
  expect_false(res$passed)
  expect_true(any(grepl("Bootstrap diagnostics require residual variance", res$messages)))
})

test_that("rvalidateSampleSize accepts adequate sample sizes", {
  df <- data.frame(y = rnorm(25), x = rnorm(25))
  res <- heteroTests:::rvalidateSampleSize("white", data = df)
  expect_true(res$passed)
  expect_length(res$messages, 0)
})

test_that("rvalidateSampleSize enforces scalar minimums with reasons", {
  set.seed(42)
  tests <- list(
    white = list(n = 19, min = 20L, reason = "Auxiliary regression needs sufficient df"),
    breusch_pagan = list(n = 14, min = 15L, reason = "Asymptotic properties require adequate sample"),
    goldfeld_quandt = list(n = 29, min = 30L, reason = "Data splitting requires sufficient observations"),
    koenker = list(n = 14, min = 15L, reason = "Studentized test needs stability"),
    park = list(n = 9, min = 10L, reason = "Log transformation requires minimum sample"),
    bootstrap_tests = list(n = 49, min = 50L, reason = "Bootstrap needs adequate base sample")
  )

  for (nm in names(tests)) {
    cfg <- tests[[nm]]
    df <- data.frame(y = rnorm(cfg$n), x = rnorm(cfg$n))
    res <- heteroTests:::rvalidateSampleSize(nm, data = df)
    expect_false(res$passed, info = nm)
    expect_equal(res$details$min_obs, cfg$min, info = nm)
    expect_true(any(grepl(cfg$reason, res$messages)), info = nm)
  }
})

test_that("rvalidateSampleSize enforces per-group minimums", {
  set.seed(101)
  levene_df <- data.frame(y = rnorm(8), g = factor(rep(c("a", "b"), each = 4)))
  levene_res <- heteroTests:::rvalidateSampleSize("levene", data = levene_df, groups = levene_df$g)
  expect_false(levene_res$passed)
  expect_equal(levene_res$details$min_obs_per_group, 5L)
  expect_true(any(grepl("Group 'a' has 4 observation", levene_res$messages)))
  expect_true(any(grepl("Group ANOVA needs sufficient observations", levene_res$messages)))

  bartlett_df <- data.frame(y = rnorm(6), g = factor(rep(letters[1:3], each = 2)))
  bartlett_res <- heteroTests:::rvalidateSampleSize("bartlett", data = bartlett_df, groups = bartlett_df$g)
  expect_false(bartlett_res$passed)
  expect_equal(bartlett_res$details$min_obs_per_group, 3L)
  expect_true(any(grepl("Variance estimation minimum", bartlett_res$messages)))
})

test_that("rvalidateSampleSize supports dynamic ARCH LM thresholds", {
  set.seed(202)
  arch_small <- data.frame(y = rnorm(10))
  arch_res <- heteroTests:::rvalidateSampleSize("arch_lm", data = arch_small, lags = 3)
  expect_false(arch_res$passed)
  expect_equal(arch_res$details$min_obs, 11L)
  expect_true(any(grepl("Lag order 3", arch_res$messages)))

  arch_ok <- data.frame(y = rnorm(20))
  arch_pass <- heteroTests:::rvalidateSampleSize("arch_lm", data = arch_ok, lags = 3)
  expect_true(arch_pass$passed)
})

test_that("rprocessValidationResult emits warnings and errors appropriately", {
  expect_invisible(heteroTests:::rprocessValidationResult(NULL))

  warning_result <- list(
    passed = TRUE,
    warnings = c("first warning", "first warning"),
    messages = character(),
    details = list()
  )
  expect_warning(
    heteroTests:::rprocessValidationResult(warning_result),
    "first warning",
    fixed = TRUE
  )

  error_result <- list(
    passed = FALSE,
    warnings = character(),
    messages = c("issue one", "issue two"),
    details = list()
  )
  expect_error(
    heteroTests:::rprocessValidationResult(error_result),
    "issue one\nissue two",
    fixed = TRUE
  )

  expect_error(
    heteroTests:::rprocessValidationResult(list(passed = NULL)),
    "Validation result must be a list with a `passed` element.",
    fixed = TRUE
  )
})

test_that("rvalidateDataInputs rejects empty column sets", {
  empty_df <- data.frame()[seq_len(5), , drop = FALSE]
  expect_error(
    heteroTests:::rvalidateDataInputs(empty_df),
    "The supplied input data contains no columns. Provide the predictors and response used to fit the model.",
    fixed = TRUE
  )
})

test_that("validation helpers leverage caching when digest is available", {
  skip_if_not_installed("digest")
  heteroTests:::clearValidationCache()
  on.exit(heteroTests:::clearValidationCache())
  cache_env <- getFromNamespace(".validation_cache", "heteroTests")
  expect_equal(length(ls(envir = cache_env)), 0L)

  assumptions <- list(normality = list(variables = "mpg", alpha = 0.05))
  res1 <- heteroTests:::rvalidateDistributionalAssumptions(mtcars, assumptions = assumptions)
  size_after_first <- length(ls(envir = cache_env))
  expect_true(size_after_first >= 1L)

  res2 <- heteroTests:::rvalidateDistributionalAssumptions(mtcars, assumptions = assumptions)
  expect_equal(size_after_first, length(ls(envir = cache_env)))
  expect_identical(res1, res2)
})
