# =============================================================================
# Validation coverage for remaining heteroscedasticity diagnostics
# =============================================================================

test_that("performGQTest enforces data preparation rules", {
  set.seed(123)
  n <- 80
  df <- data.frame(
    y = 1 + rnorm(n),
    x1 = runif(n),
    x2 = rnorm(n),
    order_var = rnorm(n)
  )
  model <- lm(y ~ x1 + x2, data = df)
  expect_htest(performGQTest(model, df, "order_var"))

  small_df <- df[seq_len(20), ]
  small_model <- lm(y ~ x1 + x2, data = small_df)
  expect_error(
    performGQTest(small_model, small_df, "order_var"),
    "requires at least 30 observations",
    fixed = FALSE
  )
})

test_that("performKoenkerTest integrates validation helpers", {
  set.seed(321)
  df <- data.frame(
    y = 1 + 2 * runif(60) + rnorm(60),
    x = runif(60),
    z = rnorm(60)
  )
  model <- lm(y ~ x + z, data = df)
  expect_htest(performKoenkerTest(model, df))

  intercept_model <- lm(y ~ 1, data = df)
  expect_error(
    performKoenkerTest(intercept_model, df),
    "requires at least one predictor",
    fixed = FALSE
  )

  small_df <- df[seq_len(12), ]
  small_model <- lm(y ~ x + z, data = small_df)
  expect_error(
    performKoenkerTest(small_model, small_df),
    "requires at least 15 observations",
    fixed = FALSE
  )
})

test_that("performParkTest checks suspected variable positivity", {
  set.seed(456)
  df <- data.frame(
    y = 1 + 3 * runif(50) + rnorm(50),
    x = runif(50) + 0.5,
    z = rnorm(50)
  )
  model <- lm(y ~ z, data = df)
  expect_htest(performParkTest(model, df, "x"))

  df_bad <- df
  df_bad$x[1] <- 0
  expect_error(
    performParkTest(model, df_bad, "x"),
    "must contain only positive values",
    fixed = FALSE
  )
})

test_that("performSpearmanTest requires variability in inputs", {
  set.seed(789)
  df <- data.frame(y = rnorm(40), x = rnorm(40))
  model <- lm(y ~ x, data = df)
  expect_htest(performSpearmanTest(model))

  constant_df <- data.frame(y = rep(5, 20))
  constant_model <- lm(y ~ 1, data = constant_df)
  expect_error(
    performSpearmanTest(constant_model),
    "requires variability in absolute residuals",
    fixed = FALSE
  )
})

test_that("performGlejserTest validates transformations", {
  set.seed(246)
  df <- data.frame(
    y = rnorm(100),
    x = runif(100) + 0.2,
    z = rnorm(100)
  )
  model <- lm(y ~ z, data = df)
  expect_htest(performGlejserTest(model, df, "x", transformation = "sqrt"))

  df_negative <- df
  df_negative$x[1] <- -0.1
  expect_error(
    performGlejserTest(model, df_negative, "x", transformation = "sqrt"),
    "requires non-negative values",
    fixed = FALSE
  )

  df_zero <- df
  df_zero$x[1] <- 0
  expect_error(
    performGlejserTest(model, df_zero, "x", transformation = "inverse"),
    "undefined when 'x' is zero",
    fixed = FALSE
  )
})

test_that("performHarveyTest flags degenerate fitted values", {
  set.seed(135)
  df <- data.frame(y = 1 + 2 * rnorm(80), x = rnorm(80))
  model <- lm(y ~ x, data = df)
  expect_htest(performHarveyTest(model))

  constant_df <- data.frame(y = rep(3, 15))
  constant_model <- lm(y ~ 1, data = constant_df)
  expect_error(
    performHarveyTest(constant_model),
    "requires variability in fitted values",
    fixed = FALSE
  )
})

test_that("performArchLMTest enforces lag-dependent requirements", {
  set.seed(975)
  n <- 120
  df <- data.frame(
    y = rnorm(n),
    x = runif(n)
  )
  model <- lm(y ~ x, data = df)
  expect_htest(performArchLMTest(model, lags = 3))

  tiny_df <- df[seq_len(8), ]
  tiny_model <- lm(y ~ x, data = tiny_df)
  expect_error(
    performArchLMTest(tiny_model, lags = 2),
    "requires at least 9 observations",
    fixed = FALSE
  )

  expect_error(
    performArchLMTest(model, lags = 0),
    "must be a positive integer",
    fixed = FALSE
  )
})

test_that("performMcLeodLiTest validates lag choice", {
  set.seed(864)
  df <- data.frame(y = rnorm(150), x = rnorm(150))
  model <- lm(y ~ x, data = df)
  expect_htest(performMcLeodLiTest(model, lags = 10))

  perfect_df <- data.frame(y = rep(5, 12))
  perfect_model <- lm(y ~ 1, data = perfect_df)
  expect_error(
    performMcLeodLiTest(perfect_model, lags = 2),
    "requires variability in squared residuals",
    fixed = FALSE
  )

  small_df <- df[seq_len(8), ]
  small_model <- lm(y ~ x, data = small_df)
  expect_error(
    performMcLeodLiTest(small_model, lags = 4),
    "requires at least",
    fixed = FALSE
  )
})

test_that("performHartleyFmaxTest enforces grouping rules", {
  set.seed(753)
  df <- data.frame(
    y = rnorm(90),
    x = rnorm(90),
    g = factor(rep(letters[1:3], each = 30))
  )
  model <- lm(y ~ x, data = df)
  expect_htest(performHartleyFmaxTest(model, df, "g"))

  small_group <- df
  small_group$g <- as.character(small_group$g)
  small_group$g[1] <- "d"
  small_group$g <- factor(small_group$g)
  expect_error(
    performHartleyFmaxTest(model, small_group, "g"),
    "Group 'd' has only 1 observations",
    fixed = FALSE
  )
})

test_that("performCameronTrivediTest requires variability", {
  set.seed(642)
  df <- data.frame(y = rnorm(100), x = runif(100))
  model <- lm(y ~ x, data = df)
  expect_htest(performCameronTrivediTest(model))

  perfect_df <- data.frame(x = rnorm(20))
  perfect_df$y <- 1 + 2 * perfect_df$x
  perfect_model <- lm(y ~ x, data = perfect_df)
  expect_error(
    performCameronTrivediTest(perfect_model),
    "Model has perfect fit",
    fixed = FALSE
  )
})

test_that("performOrderedLMTest validates ordering variable", {
  set.seed(531)
  df <- data.frame(y = rnorm(90), x = rnorm(90))
  model <- lm(y ~ x, data = df)
  expect_htest(performOrderedLMTest(model, df, "x"))

  tiny_df <- df[seq_len(2), ]
  tiny_model <- lm(y ~ x, data = tiny_df)
  expect_error(
    performOrderedLMTest(tiny_model, tiny_df, "x"),
    "requires at least 3 observations",
    fixed = FALSE
  )
})

test_that("performStudentizedBPTest honours validation framework", {
  set.seed(420)
  df <- data.frame(y = rnorm(120), x = runif(120))
  model <- lm(y ~ x, data = df)
  expect_htest(performStudentizedBPTest(model, df))

  small_df <- df[seq_len(10), ]
  small_model <- lm(y ~ x, data = small_df)
  expect_error(
    performStudentizedBPTest(small_model, small_df),
    "requires at least 15 observations",
    fixed = FALSE
  )
})

test_that("performWhiteTestBootstrap applies sample size rule", {
  set.seed(2468)
  df <- data.frame(y = rnorm(80), x = rnorm(80))
  model <- lm(y ~ x, data = df)
  expect_s3_class(performWhiteTestBootstrap(model, df, B = 25, parallel = FALSE), "htest")

  small_df <- df[seq_len(30), ]
  small_model <- lm(y ~ x, data = small_df)
  expect_error(
    performWhiteTestBootstrap(small_model, small_df, B = 5),
    "requires at least 50 observations",
    fixed = FALSE
  )
})

test_that("performSzroeterTest validates ordered residuals", {
  set.seed(1357)
  df <- data.frame(y = rnorm(75), x = runif(75))
  model <- lm(y ~ x, data = df)
  expect_htest(performSzroeterTest(model, df, "x"))

  constant_df <- data.frame(y = rep(4, 20), x = seq_len(20))
  constant_model <- lm(y ~ x, data = constant_df)
  expect_error(
    performSzroeterTest(constant_model, constant_df, "x"),
    "requires variability in ordered residuals",
    fixed = FALSE
  )
})

