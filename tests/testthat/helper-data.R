# Helper file providing synthetic datasets for tests
set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)

data_homosced <- data.frame(
  y = 1 + 2 * x1 + 3 * x2 + rnorm(n, sd = 1),
  x1 = x1,
  x2 = x2
)

data_heterosced <- data.frame(
  y = 1 + 2 * x1 + 3 * x2 + rnorm(n, sd = abs(x1) + 0.1),
  x1 = x1,
  x2 = x2
)
