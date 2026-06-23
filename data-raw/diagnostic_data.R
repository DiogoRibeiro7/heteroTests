# Synthetic data with mild collinearity and a quadratic term, for diagnostics demos.
diagnostic_data <- local({
  set.seed(123)
  n <- 150
  x1 <- stats::rnorm(n)
  x2 <- x1 + stats::rnorm(n, sd = 0.1)
  y <- 1 + 2 * x1 + 3 * x2 + 0.5 * x1^2 + stats::rnorm(n)
  data.frame(x1 = x1, x2 = x2, y = y)
})
usethis::use_data(diagnostic_data, overwrite = TRUE)
