skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(paste("Package", pkg, "not installed"))
  }
}

test_that("performWildBootstrapTest returns htest", {
  data <- mtcars
  model <- lm(mpg ~ wt + hp, data = data)
  set.seed(123)
  result <- performWildBootstrapTest(model, data, B = 49, progress = FALSE)
  expect_s3_class(result, "htest")
  expect_length(result$bootstrap$replicates, 49)
})

test_that("performWildBootstrapTest has power against heteroscedasticity", {
  set.seed(2)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 0.3 + x)
  d <- data.frame(y, x)
  result <- performWildBootstrapTest(lm(y ~ x, data = d), d, B = 399, progress = FALSE)
  expect_lt(result$p.value, 0.05)
})

test_that("performWildBootstrapTest does not reject under homoscedasticity", {
  set.seed(5)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n)
  d <- data.frame(y, x)
  result <- performWildBootstrapTest(lm(y ~ x, data = d), d, B = 399, progress = FALSE)
  expect_gt(result$p.value, 0.05)
})

# The degrees of freedom the wild bootstrap reports, which its p-value does not
# use, are those of performKoenkerTest(): the rank of the auxiliary regression
# less its intercept. Up to 0.17.0 they were the number of regressors, 5 on a
# model without an intercept whose columns span the constant, where the same
# column space with an intercept gives 4.
test_that("performWildBootstrapTest reports the rank of its auxiliary regression", {
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  set.seed(31)
  no_intercept <- performWildBootstrapTest(lm(y ~ 0 + group + x1, data = d), d, B = 19,
                                           progress = FALSE)
  set.seed(31)
  with_intercept <- performWildBootstrapTest(lm(y ~ x1 + group, data = d), d, B = 19,
                                             progress = FALSE)
  expect_identical(unname(no_intercept$parameter), c(4L, 19L))
  expect_identical(unname(with_intercept$parameter), c(4L, 19L))
  expect_equal(unname(no_intercept$statistic), unname(with_intercept$statistic),
               tolerance = 1e-10)
  koenker <- performKoenkerTest(lm(y ~ 0 + group + x1, data = d), d)
  expect_equal(unname(no_intercept$statistic), unname(koenker$statistic), tolerance = 1e-10)
})

# The statistic of the wild bootstrap is computed on the auxiliary regression
# of performKoenkerTest(). Up to 0.17.0 it was fitted by lm() from a formula
# on the columns as they are, which dropped a regressor far from zero that a
# model without an intercept or a Gaussian glm keeps: 2.372 and 1.457, the
# statistics of x2 alone, for Koenker's 13.915 and 9.044. A model whose
# regressors are all constant, on which the statistic is 0 in every replicate
# (0 on 1 degree of freedom, p = 1, up to 0.17.0), is refused, as Koenker's
# test refuses it.
test_that("the wild bootstrap keeps a regressor far from zero and refuses a constant design", {
  previous <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous), add = TRUE)
  d <- contract_data()$d
  d$far <- 1e8 + d$x1
  d$far_glm <- 1e6 + d$x1 / 10
  fits <- list(far = lm(y ~ 0 + far + x2, data = d), glm = glm(y ~ far_glm + x2, data = d))
  for (nm in names(fits)) {
    set.seed(32)
    wild <- performWildBootstrapTest(fits[[nm]], d, B = 19, progress = FALSE)
    koenker <- performKoenkerTest(fits[[nm]], d)
    expect_equal(unname(wild$statistic), unname(koenker$statistic), tolerance = 1e-10, info = nm)
    expect_identical(unname(wild$parameter), c(2L, 19L), info = nm)
    expect_gt(unname(wild$statistic), 9)
  }
  d$one <- 1
  expect_error(
    performWildBootstrapTest(lm(y ~ 0 + one, data = d), d, B = 19, progress = FALSE),
    "Wild bootstrap test requires at least one predictor beyond the intercept.", fixed = TRUE
  )
})

test_that("HC covariance pseudo-test is absent from built-in registries", {
  expect_false("hc_covariance" %in% names(as.list(heteroTests:::.diagnostic_registry)))
  expect_false("hc_covariance" %in% test_factory$get_available())
})

test_that("performQuantileRegressionTest compares quantiles", {
  skip_if_not_installed("quantreg")
  data <- mtcars[rep(seq_len(nrow(mtcars)), length.out = 64), ]
  data$mpg <- data$mpg + rnorm(nrow(data), sd = 0.01)
  model <- lm(mpg ~ wt + hp, data = data)
  result <- performQuantileRegressionTest(model, data, taus = c(0.25, 0.75))
  expect_s3_class(result, "htest")
  expect_true(result$parameter[["df1"]] > 0)
  expect_true(result$parameter[["df2"]] > 0)
})

test_that("performRankPermutationTest uses permutations", {
  data <- mtcars
  model <- lm(mpg ~ wt + hp, data = data)
  set.seed(321)
  result <- performRankPermutationTest(model, data, B = 199, progress = FALSE)
  expect_s3_class(result, "htest")
  expect_length(result$permutation$replicates, 199)
})

test_that("performHighDimensionalTest handles wide designs", {
  set.seed(42)
  n <- 60
  p <- 25
  X <- matrix(rnorm(n * p), nrow = n)
  beta <- rnorm(p)
  y <- X %*% beta + rnorm(n, sd = 0.5 + 0.1 * scale(X[, 1]))
  df <- as.data.frame(cbind(y = as.numeric(y), X))
  model <- lm(y ~ ., data = df)
  result <- performHighDimensionalTest(model, df, variance_threshold = 0.8, max_components = 8)
  expect_s3_class(result, "htest")
  expect_true(result$projection$components >= 1)
})

test_that("performSpatialHeteroTest analyses squared residuals", {
  skip_if_not_installed("spdep")
  data <- mtcars
  coords <- cbind(runif(nrow(data)), runif(nrow(data)))
  nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 3))
  listw <- spdep::nb2listw(nb)
  model <- lm(mpg ~ wt + hp, data = data)
  result <- performSpatialHeteroTest(model, data, listw = listw, permutations = 99)
  expect_s3_class(result, "htest")
  expect_true(result$parameter[["permutations"]] == 99)
})
