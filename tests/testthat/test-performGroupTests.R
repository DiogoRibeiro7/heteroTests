# =============================================================================
# Focused tests for group-based heteroscedasticity diagnostics
# =============================================================================

test_that("performLeveneTest integrates validation helpers", {
  set.seed(123)
  df <- data.frame(
    y = rnorm(30),
    x1 = rnorm(30),
    x2 = rnorm(30),
    g = factor(rep(letters[1:3], each = 10))
  )

  model <- lm(y ~ x1 + x2, data = df)
  expect_htest(performLeveneTest(model, df, "g"))

  df_missing <- df
  df_missing$g[c(2, 5)] <- NA
  expect_warning(
    result_missing <- performLeveneTest(model, df_missing, "g"),
    "Removed 2 observations due to missing values"
  )
  expect_htest(result_missing)

  small_df <- df[seq_len(15), ]
  small_df$g <- factor(rep(c("a", "b", "c"), times = c(4, 5, 6)))
  small_model <- lm(y ~ x1 + x2, data = small_df)
  expect_error(
    performLeveneTest(small_model, small_df, "g"),
    "increase to 5 for stable inference.",
    fixed = TRUE
  )
})

test_that("performBartlettTest warns on severe normality violations", {
  set.seed(456)
  df <- data.frame(
    y = rexp(60, rate = 0.2),
    x = rnorm(60),
    g = factor(rep(letters[1:3], each = 20))
  )

  model <- lm(y ~ x, data = df)
  expect_warning(
    bart_result <- performBartlettTest(model, df, "g"),
    "Severe non-normality detected",
    fixed = FALSE
  )
  expect_htest(bart_result)

  numeric_group <- df
  numeric_group$g <- as.numeric(numeric_group$g)
  expect_error(
    performBartlettTest(model, numeric_group, "g"),
    "Grouping variable 'g' must be a factor or character vector with at least 2 levels.",
    fixed = TRUE
  )
})

test_that("performBrownForsytheTest enforces group requirements", {
  set.seed(789)
  df <- data.frame(
    y = rnorm(36),
    x1 = rnorm(36),
    x2 = rnorm(36),
    g = factor(rep(letters[1:3], each = 12))
  )

  model <- lm(y ~ x1 + x2, data = df)
  expect_htest(performBrownForsytheTest(model, df, "g"))

  small_df <- df[seq_len(14), ]
  small_df$g <- factor(rep(c("a", "b", "c"), times = c(4, 5, 5)))
  small_model <- lm(y ~ x1 + x2, data = small_df)
  expect_error(
    performBrownForsytheTest(small_model, small_df, "g"),
    "increase to 5 for stable inference.",
    fixed = TRUE
  )

  numeric_group <- df
  numeric_group$g <- as.numeric(numeric_group$g)
  expect_error(
    performBrownForsytheTest(model, numeric_group, "g"),
    "Grouping variable 'g' must be a factor or character vector with at least 2 levels.",
    fixed = TRUE
  )
})

test_that("performFlignerKilleenTest catches degenerate rankings", {
  constant_df <- data.frame(
    y = rep(5, 12),
    g = factor(rep(letters[1:3], each = 4))
  )
  constant_model <- lm(y ~ 1, data = constant_df)
  expect_error(
    performFlignerKilleenTest(constant_model, constant_df, "g"),
    "Residual variance is too small to evaluate Fligner-Killeen",
    fixed = FALSE
  )

  set.seed(135)
  df <- data.frame(
    y = rnorm(45),
    x = rnorm(45),
    g = factor(rep(letters[1:3], each = 15))
  )
  model <- lm(y ~ x, data = df)
  expect_htest(performFlignerKilleenTest(model, df, "g"))
})

