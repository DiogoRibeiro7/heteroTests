# Synthetic data whose error variance grows linearly with x (textbook heteroscedasticity).
hetero_data <- local({
  set.seed(42)
  x <- stats::runif(100)
  y <- 1 + 2 * x + stats::rnorm(100, sd = 0.5 + 2 * x)
  data.frame(x = x, y = y)
})
usethis::use_data(hetero_data, overwrite = TRUE)
