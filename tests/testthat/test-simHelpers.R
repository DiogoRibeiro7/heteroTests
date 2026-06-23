library(heteroTests)

# Test simulate_hetero basic behaviour

test_that("simulate_hetero returns data frame with correct structure", {
  df <- simulate_hetero(n = 50, beta0 = 1, beta1 = 2,
                        sigma_func = sigma_linear, seed = 1)
  expect_true(is.data.frame(df))
  expect_equal(ncol(df), 2)
  expect_equal(colnames(df), c("x", "y"))
  expect_equal(nrow(df), 50)
})

# Test some sigma functions
x <- c(1, 2, 3)

sigma_fns <- list(
  sigma_linear = sigma_linear,
  sigma_exponential = sigma_exponential,
  sigma_inverse = sigma_inverse,
  sigma_gaussian_peak = sigma_gaussian_peak,
  sigma_spatial = function(z) sigma_spatial(data.frame(x = z, y = z)),
  sigma_logistic = sigma_logistic
)

for (nm in names(sigma_fns)) {
  fn <- sigma_fns[[nm]]
  test_that(paste(nm, "returns numeric vector"), {
    res <- fn(x)
    expect_true(is.numeric(res))
    expect_equal(length(res), length(x))
    expect_true(all(res >= 0))
  })
}

# sigma_multiplicative requires mu_func
mu_fun <- function(z) 1 + 2 * z

res_mult <- sigma_multiplicative(x, mu_fun, p = 1)

 test_that("sigma_multiplicative works", {
  expect_true(is.numeric(res_mult))
  expect_equal(length(res_mult), length(x))
  expect_true(all(res_mult >= 0))
})

# Test simulate_arch1
 test_that("simulate_arch1 returns correct columns", {
  df <- simulate_arch1(n = 10, seed = 2)
  expect_true(is.data.frame(df))
  expect_equal(colnames(df), c("time", "y", "sigma"))
  expect_equal(nrow(df), 10)
})

# Test performScatterDiagnostic
 test_that("performScatterDiagnostic correlations", {
  model <- lm(y ~ x1 + x2, data = data_heterosced)
  res <- performScatterDiagnostic(model, data_heterosced, c("x1", "x2"))
  expect_true(is.numeric(res))
  expect_equal(names(res), c("x1", "x2"))
  expect_equal(length(res), 2)
  expect_true(all(abs(res) <= 1))
})

