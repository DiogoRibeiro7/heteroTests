library(testthat)
library(heteroTests)

test_that("performBPTest enforces sample size minimum", {
  set.seed(24)
  df <- data.frame(
    y = rnorm(12),
    x1 = rnorm(12),
    x2 = rnorm(12)
  )
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performBPTest(model, df),
    "requires at least 15"
  )
})

test_that("performBPTest warns about missing values and succeeds", {
  set.seed(99)
  n <- 40
  df <- data.frame(
    y = 1 + rnorm(n),
    x1 = rnorm(n),
    x2 = rnorm(n)
  )
  df$x1[1] <- NA
  df$x2[3] <- NA
  model <- lm(y ~ x1 + x2, data = df)
  expect_warning(
    res <- performBPTest(model, df),
    "Removed 2 observations due to missing values"
  )
  expect_s3_class(res, "htest")
})

test_that("performBPTest detects insufficient residual variation", {
  set.seed(56)
  n <- 40
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- rnorm(n, sd = 1e-8)
  df <- data.frame(y = y, x1 = x1, x2 = x2)
  model <- lm(y ~ x1 + x2, data = df)
  expect_error(
    performBPTest(model, df),
    "Residual variance is too small to evaluate Breusch-Pagan",
    fixed = FALSE
  )
})

test_that("performBPTest aborts for perfect fits", {
  df <- data.frame(
    x = seq_len(20),
    z = rnorm(20)
  )
  df$y <- 1 + 2 * df$x + 3 * df$z
  model <- lm(y ~ x + z, data = df)
  expect_error(
    performBPTest(model, df),
    "Model appears perfectly explained",
    fixed = FALSE
  )
})

# Up to 0.17.0 the Breusch-Pagan and Koenker tests took as degrees of freedom
# the number of columns of the design but its intercept. On a model without an
# intercept whose columns span the constant, the dummies of every level of a
# factor, the auxiliary regression adds an intercept that those columns
# already span: the tests counted 5, with the same statistics, where the rank
# of the auxiliary regression less its intercept is 4, the degrees of freedom
# of lm(y ~ x1 + group), the same column space. The p-values were 0.1237 and
# 0.0617, and are 0.0704 and 0.0325. The streaming tests refused that model
# ("auxiliary regression became rank deficient"). Both count the rank now, as
# White's test does.
test_that("the Breusch-Pagan and Koenker tests count the rank of their auxiliary regression", {
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  no_intercept <- lm(y ~ 0 + group + x1, data = d)
  with_intercept <- lm(y ~ x1 + group, data = d)
  cases <- list(
    list(exact = performBPTest, streaming = performBPTestStreaming,
         robust = function(m, d) performBPTestRobust(m, d, studentize = FALSE),
         registered = "breusch_pagan", statistic = 8.6526688, p = 0.07039232),
    list(exact = performKoenkerTest, streaming = performKoenkerTestStreaming,
         robust = function(m, d) performBPTestRobust(m, d, studentize = TRUE),
         registered = "koenker", statistic = 10.5223823, p = 0.03249007)
  )
  for (case in cases) {
    exact <- case$exact(no_intercept, d)
    info <- exact$method
    expect_identical(unname(exact$parameter), 4L, info = info)
    expect_equal(unname(exact$statistic), case$statistic, tolerance = 1e-7, info = info)
    expect_equal(exact$p.value, case$p, tolerance = 1e-6, info = info)
    # The value of the same column space with an intercept.
    reference <- case$exact(with_intercept, d)
    expect_identical(unname(reference$parameter), 4L, info = info)
    expect_equal(unname(exact$statistic), unname(reference$statistic), tolerance = 1e-10,
                 info = info)
    expect_equal(exact$p.value, reference$p.value, tolerance = 1e-10, info = info)
    # The streaming test gives the value of the exact one, where it refused.
    for (chunk in c(7, 10000)) {
      streamed <- case$streaming(no_intercept, d, chunk_size = chunk, progress = FALSE)
      expect_identical(unname(streamed$parameter), 4L, info = paste(info, chunk))
      expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-10,
                   info = paste(info, chunk))
      expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-10, info = paste(info, chunk))
    }
    # The robust form, whose interval follows the degrees of freedom, and the
    # registered test of the runner, exact and streamed.
    # The studentized robust form is computed apart, and agrees with
    # performKoenkerTest() to 1e-8 (test-koenker-studentized.R).
    robust <- case$robust(no_intercept, d)
    expect_identical(unname(robust$parameter), 4L, info = info)
    expect_equal(robust$p.value, exact$p.value, tolerance = 1e-8, info = info)
    expect_equal(unname(robust$robust_details$confidence_interval$estimate),
                 qchisq(c(0.025, 0.975), 4), tolerance = 1e-10, info = info)
    exact_run <- runHeteroTests(no_intercept, d, tests = case$registered, use_cache = FALSE,
                                progress = FALSE)[[case$registered]]
    # A threshold of 0 MB streams, and warns of the size of the data.
    streamed_run <- suppressWarnings(
      runHeteroTests(no_intercept, d, tests = case$registered, use_cache = FALSE,
                     chunk_threshold_mb = 0, progress = FALSE)
    )[[case$registered]]
    expect_true(attr(streamed_run, "extras")$streaming, info = info)
    for (run in list(exact_run, streamed_run)) {
      expect_null(attr(run, "extras")$status, info = info)
      expect_identical(unname(run$parameter), 4L, info = info)
      expect_equal(run$p.value, exact$p.value, tolerance = 1e-10, info = info)
    }
  }
  # A model whose only regressor is constant has none beyond the intercept:
  # the rank says 0 degrees of freedom, and the tests refuse it, where they
  # gave a statistic of 0 on 1.
  d$one <- 1
  constant <- lm(y ~ 0 + one, data = d)
  expect_error(performBPTest(constant, d), "at least one predictor beyond the intercept",
               fixed = TRUE)
  expect_error(performKoenkerTest(constant, d), "at least one predictor beyond the intercept",
               fixed = TRUE)
  expect_error(performBPTestStreaming(constant, d, progress = FALSE),
               "at least one predictor beyond the intercept", fixed = TRUE)
  expect_error(performKoenkerTestStreaming(constant, d, progress = FALSE),
               "at least one predictor beyond the intercept", fixed = TRUE)
})

# A model without an intercept whose columns do not span the constant keeps
# its degrees of freedom: the auxiliary regression has an intercept of its
# own, and the rank counts each regressor.
test_that("a model without an intercept that spans no constant keeps its degrees of freedom", {
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  m <- lm(y ~ 0 + x1 + x2, data = d)
  expect_identical(unname(performBPTest(m, d)$parameter), 2L)
  expect_identical(unname(performKoenkerTest(m, d)$parameter), 2L)
  expect_identical(unname(performBPTestStreaming(m, d, progress = FALSE)$parameter), 2L)
  expect_identical(unname(performKoenkerTestStreaming(m, d, progress = FALSE)$parameter), 2L)
})
