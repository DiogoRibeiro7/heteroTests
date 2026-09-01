library(testthat)

# ---------------------------------------------------------------------------
# Pass C of the statistical validation matrix.
#
# Two tests were withdrawn because their statistics cannot detect
# heteroscedasticity; two more are valid statistics under misleading names, and
# are pinned here so the redundancy stays visible until the API review.
# ---------------------------------------------------------------------------

make_pass_c_model <- function(seed = 31, n = 120) {
  set.seed(seed)
  d <- data.frame(x1 = runif(n, 1, 5), x2 = rnorm(n))
  d$y <- 1 + 2 * d$x1 + 0.5 * d$x2 + rnorm(n, sd = 0.4 + 0.5 * d$x1)
  list(data = d, model = lm(y ~ x1 + x2, data = d))
}

# --- withdrawn -------------------------------------------------------------

test_that("the Rice ratio is insensitive to heteroscedasticity", {
  # Why it was withdrawn: E[(e_i - e_{i-1})^2] = s2_i + s2_{i-1}, so Rice's
  # numerator and the mean squared residual both estimate the mean variance and
  # the ratio sits at one under any variance pattern.
  ratio <- function(r) sum(diff(r)^2) / (2 * (length(r) - 1)) / mean(r^2)
  set.seed(5)
  for (sd_fun in list(
    function(x) rep(1, length(x)),
    function(x) 0.2 + 1.2 * x,
    function(x) exp(x)
  )) {
    vals <- replicate(200, {
      x <- sort(runif(300, 1, 5))
      ratio(rnorm(300, sd = sd_fun(x)))
    })
    expect_equal(mean(vals), 1, tolerance = 0.05)
  }
})

# --- valid statistics under misleading names -------------------------------

# --- Davidian-Carroll ------------------------------------------------------

test_that("performDavidianCarrollTest survives a zero residual", {
  set.seed(3)
  d <- data.frame(x = 1:30)
  d$y <- 2 * d$x + rnorm(30)
  model <- lm(y ~ x, data = d)
  model$residuals[1] <- 0

  expect_warning(performDavidianCarrollTest(model), "numerically zero")
  res <- suppressWarnings(performDavidianCarrollTest(model))
  expect_true(is.finite(res$statistic))
  expect_true(is.finite(res$p.value))
})

test_that("performDavidianCarrollTest matches its auxiliary regression", {
  obj <- make_pass_c_model()
  res <- performDavidianCarrollTest(obj$model, degree = 2)

  fit <- fitted(obj$model)
  aux <- anova(lm(log(residuals(obj$model)^2) ~ poly(fit, 2)))
  expect_equal(unname(res$statistic), aux$`F value`[1], tolerance = 1e-10)
  expect_equal(res$p.value, aux$`Pr(>F)`[1], tolerance = 1e-10)
})

# --- the Pass C methods that were already sound ----------------------------

test_that("the sound Pass C diagnostics still return usable results", {
  obj <- make_pass_c_model()
  set.seed(9)
  results <- list(
    rank_permutation = performRankPermutationTest(obj$model, obj$data, B = 199,
                                                  progress = FALSE),
    high_dimensional = performHighDimensionalTest(obj$model, obj$data),
    wild_bootstrap = performWildBootstrapTest(obj$model, obj$data, B = 199)
  )
  for (nm in names(results)) {
    p <- results[[nm]]$p.value
    expect_true(is.finite(p), info = nm)
    expect_gte(p, 0)
    expect_lte(p, 1)
  }
})
