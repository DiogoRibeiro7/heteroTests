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
  # the rank says 0 degrees of freedom, and the tests refuse it, where the
  # exact ones gave a statistic of 0 on 1 and the streaming ones refused it
  # as rank deficient. The exact and streaming forms refuse it with the same
  # error, the refusal of a model with an intercept alone, lm(y ~ 1).
  d$one <- 1
  constant <- lm(y ~ 0 + one, data = d)
  intercept_only <- lm(y ~ 1, data = d)
  refusals <- list(
    function(m) performBPTest(m, d),
    function(m) performBPTestStreaming(m, d, progress = FALSE),
    function(m) performKoenkerTest(m, d),
    function(m) performKoenkerTestStreaming(m, d, progress = FALSE)
  )
  for (i in seq_along(refusals)) {
    refusal <- tryCatch(refusals[[i]](constant), error = identity)
    expect_s3_class(refusal, "error")
    expect_match(conditionMessage(refusal), "at least one predictor beyond the intercept",
                 fixed = TRUE, info = i)
    alone <- tryCatch(refusals[[i]](intercept_only), error = identity)
    expect_identical(class(refusal), class(alone), info = i)
    expect_identical(conditionMessage(refusal), conditionMessage(alone), info = i)
  }
  # The runner stops with the refusal, as on lm(y ~ 1), and advises a
  # regressor that varies, not the removal of `one`, the only regressor.
  for (tests in list(c("breusch_pagan", "koenker", "white"), c("white", "breusch_pagan"))) {
    stopped <- tryCatch(runHeteroTests(constant, d, tests = tests, use_cache = FALSE),
                        error = identity)
    expect_s3_class(stopped, "error")
    expect_match(conditionMessage(stopped), "Fit a model with a regressor that varies",
                 fixed = TRUE, info = tests[1])
    expect_no_match(conditionMessage(stopped), "Remove constant predictor", fixed = TRUE,
                    info = tests[1])
  }
})

# Up to 0.17.0 the auxiliary regression of both tests was fitted by lm() from
# a formula, on the columns as they are, at the tolerance of lm(). On three
# kinds of full-rank design the statistic was wrong, and is now the value of
# lmtest::bptest() and of the streaming forms, which had it already:
# - a model without an intercept with a regressor far from zero, which that
#   fit dropped as collinear with its intercept while the degrees of freedom
#   counted it: the Breusch-Pagan test gave 2.850 on 2 and Koenker's 2.372
#   for 16.721 and 13.915, the values of lmtest::bptest() given the
#   regressors as `varformula = ~ x1 + x2`;
# - a Gaussian glm, which keeps a regressor at a tolerance of 1e-11 where
#   lm() would alias it: 2.005 and 1.457 for 12.448 and 9.044, the values of
#   lm(y ~ x1 + x2), the same column space;
# - a regressor named `scaled_sq` or `e_squared`, which the formula of the
#   Breusch-Pagan or the Koenker test took for its response: 102.04 and
#   0.0141 for 12.448 and 9.044, the values of the same model under another
#   name.
test_that("the auxiliary regression keeps a regressor far from zero and ignores names", {
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  d$far <- 1e8 + d$x1
  d$far_glm <- 1e6 + d$x1 / 10
  d$scaled_sq <- d$x1
  d$e_squared <- d$x1
  well <- lm(y ~ x1 + x2, data = d)
  cases <- list(
    far = list(m = lm(y ~ 0 + far + x2, data = d), varformula = ~ x1 + x2,
               values = c(16.7205262, 13.9154236)),
    glm = list(m = glm(y ~ far_glm + x2, data = d), values = c(12.4479722, 9.0437629)),
    scaled_sq = list(m = lm(y ~ scaled_sq + x2, data = d), values = c(12.4479722, 9.0437629)),
    e_squared = list(m = lm(y ~ e_squared + x2, data = d), values = c(12.4479722, 9.0437629))
  )
  tests <- list(list(exact = performBPTest, streaming = performBPTestStreaming),
                list(exact = performKoenkerTest, streaming = performKoenkerTestStreaming))
  for (nm in names(cases)) {
    case <- cases[[nm]]
    for (k in 1:2) {
      info <- paste(nm, k)
      exact <- tests[[k]]$exact(case$m, d)
      expect_identical(unname(exact$parameter), 2L, info = info)
      expect_equal(unname(exact$statistic), case$values[k], tolerance = 1e-7, info = info)
      streamed <- tests[[k]]$streaming(case$m, d, chunk_size = 7, progress = FALSE)
      expect_identical(unname(streamed$parameter), 2L, info = info)
      expect_equal(unname(streamed$statistic), unname(exact$statistic), tolerance = 1e-8,
                   info = info)
      expect_equal(streamed$p.value, exact$p.value, tolerance = 1e-8, info = info)
      if (is.null(case$varformula)) {
        # The same column space, well conditioned and under other names.
        reference <- tests[[k]]$exact(well, d)
        expect_equal(unname(exact$statistic), unname(reference$statistic), tolerance = 1e-7,
                     info = info)
        expect_equal(exact$p.value, reference$p.value, tolerance = 1e-7, info = info)
      }
    }
  }
  # The robust form and the runner give the statistic of the test.
  far <- cases$far$m
  robust <- performBPTestRobust(far, d, studentize = FALSE)
  expect_equal(unname(robust$statistic), 16.7205262, tolerance = 1e-7)
  expect_identical(unname(robust$parameter), 2L)
  run <- runHeteroTests(far, d, tests = c("breusch_pagan", "koenker"), use_cache = FALSE)
  expect_equal(unname(run$breusch_pagan$statistic), 16.7205262, tolerance = 1e-7)
  expect_equal(unname(run$koenker$statistic), 13.9154236, tolerance = 1e-7)
  skip_if_not_installed("lmtest")
  for (nm in names(cases)) {
    case <- cases[[nm]]
    # lmtest refits the design of the model at the tolerance of lm.fit(), and
    # on the glm aliases its far regressor: it applies to the other three.
    if (nm == "glm") next
    for (k in 1:2) {
      info <- paste(nm, k)
      ref <- if (is.null(case$varformula)) {
        lmtest::bptest(case$m, studentize = k == 2)
      } else {
        lmtest::bptest(case$m, varformula = case$varformula, data = d, studentize = k == 2)
      }
      exact <- tests[[k]]$exact(case$m, d)
      expect_equal(unname(exact$statistic), unname(ref$statistic), tolerance = 1e-7, info = info)
      expect_identical(unname(exact$parameter), as.integer(ref$parameter), info = info)
      expect_equal(exact$p.value, unname(ref$p.value), tolerance = 1e-7, info = info)
    }
  }
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
