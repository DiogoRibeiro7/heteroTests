library(testthat)
library(heteroTests)


test_that("Breusch-Pagan test works on homoscedastic data", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performBPTest(model, df)
  expect_s3_class(res, "htest")
  expect_gt(res$p.value, 0.05)
})

test_that("Breusch-Pagan test detects heteroscedasticity", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performBPTest(model, df)
  expect_lt(res$p.value, 0.05)
})

test_that("Goldfeld-Quandt test works on homoscedastic data", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performGQTest(model, df, order_by = "x")
  expect_s3_class(res, "htest")
  expect_gt(res$p.value, 0.05)
})

test_that("Goldfeld-Quandt test detects heteroscedasticity", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performGQTest(model, df, order_by = "x")
  expect_lt(res$p.value, 0.05)
})

# New tests for additional procedures

test_that("Koenker test works on homoscedastic data", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performKoenkerTest(model, df)
  expect_gt(res$p.value, 0.05)
})

test_that("Koenker test detects heteroscedasticity", {
  set.seed(1)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performKoenkerTest(model, df)
  expect_lt(res$p.value, 0.05)
})

test_that("Park test detects variance relationship", {
  set.seed(2)
  n <- 200
  x <- runif(n, 1, 2)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performParkTest(model, df, "x")
  expect_lt(res$p.value, 0.05)
})

test_that("Park test errors on non-positive variable", {
  set.seed(2)
  n <- 50
  x <- runif(n, -1, 1)
  y <- 1 + 2 * x + rnorm(n)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  expect_error(
    performParkTest(model, df, "x"),
    "park requires strictly positive data",
    fixed = TRUE
  )
})

test_that("Spearman test detects monotonic heteroscedasticity", {
  set.seed(3)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performSpearmanTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("Levene test works on equal variances", {
  set.seed(4)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  y <- 1 + 2 * x + rnorm(200)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performLeveneTest(model, df, "g")
  expect_gt(res$p.value, 0.05)
})

test_that("Levene test detects variance differences", {
  set.seed(4)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 3)
  y <- 1 + 2 * x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performLeveneTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Glejser test detects heteroscedasticity", {
  set.seed(5)
  n <- 200
  x <- runif(n, 1, 2)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performGlejserTest(model, df, "x")
  expect_lt(res$p.value, 0.05)
})

test_that("Brown-Forsythe test detects variance differences", {
  set.seed(6)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 3)
  y <- 1 + x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performBrownForsytheTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Fligner-Killeen test detects variance differences", {
  set.seed(6)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 3)
  y <- 1 + x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performFlignerKilleenTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Bartlett test detects variance differences", {
  set.seed(6)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 3)
  y <- 1 + x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performBartlettTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Harvey test detects heteroscedasticity", {
  set.seed(7)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = 1 + 5 * x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performHarveyTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("Harvey test handles zero residuals", {
  df <- data.frame(x = 1:20, y = 1:20)
  model <- lm(y ~ x, data = df)
  expect_error(
    performHarveyTest(model),
    "Model appears perfectly explained",
    fixed = FALSE
  )
})

test_that("ARCH LM test on iid data returns large p-value", {
  set.seed(8)
  n <- 300
  e <- rnorm(n)
  y <- 1 + e
  model <- lm(y ~ 1)
  res <- performArchLMTest(model, lags = 2)
  expect_gt(res$p.value, 0.01)
})

test_that("McLeod-Li test works", {
  set.seed(9)
  n <- 300
  e <- rnorm(n)
  y <- 1 + e
  model <- lm(y ~ 1)
  res <- performMcLeodLiTest(model, lags = 10)
  expect_s3_class(res, "htest")
})

test_that("Hartley Fmax detects variance differences", {
  set.seed(10)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 4)
  y <- 1 + x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performHartleyFmaxTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Cameron-Trivedi test detects heteroscedasticity", {
  set.seed(11)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performCameronTrivediTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("Ordered LM test detects heteroscedasticity", {
  set.seed(12)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performOrderedLMTest(model, df, "x")
  expect_lt(res$p.value, 0.05)
})

# New functions from the roadmap
test_that("O'Brien test works", {
  set.seed(13)
  g <- factor(rep(1:2, each = 100))
  x <- rnorm(200)
  sd_vec <- ifelse(g == 1, 1, 3)
  y <- 1 + x + rnorm(200, sd = sd_vec)
  df <- data.frame(x = x, y = y, g = g)
  model <- lm(y ~ x, data = df)
  res <- performOBrienTest(model, df, "g")
  expect_lt(res$p.value, 0.05)
})

test_that("Cook-Weisberg test detects heteroscedasticity", {
  set.seed(14)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performCookWeisbergTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("NCV test detects slope", {
  set.seed(15)
  n <- 200
  x <- runif(n)
  y <- 1 + 2 * x + rnorm(n, sd = x)
  df <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performNCVTest(model)
  expect_lt(res$p.value, 0.05)
})

test_that("BPRandomEffectsTest returns htest", {
  set.seed(16)
  id <- rep(1:10, each = 5)
  time <- rep(1:5, times = 10)
  x <- rnorm(50)
  y <- 1 + x + rnorm(50)
  df <- data.frame(id = id, time = time, x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performBPRandomEffectsTest(model, df, "id")
  expect_s3_class(res, "htest")
})

test_that("Pesaran test returns htest", {
  set.seed(17)
  id <- rep(1:8, each = 5)
  time <- rep(1:5, times = 8)
  x <- rnorm(40)
  y <- 1 + x + rnorm(40)
  df <- data.frame(id = id, time = time, x = x, y = y)
  model <- lm(y ~ x, data = df)
  res <- performPesaranTest(model, df, "id", "time")
  expect_s3_class(res, "htest")
})
