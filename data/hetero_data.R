## This file generates hetero_data when data(hetero_data) is called
hetero_data <- local({
  set.seed(42)
  x <- stats::runif(100)
  y <- 1 + 2 * x + stats::rnorm(100, sd = 0.5 + 2 * x)
  data.frame(x = x, y = y)
})
