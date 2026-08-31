library(testthat)


test_that("performBPTest matches lmtest::bptest", {
  skip_if_not_installed("lmtest")
  set.seed(101)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- performBPTest(model, df)
  ref <- lmtest::bptest(model, studentize = FALSE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("performKoenkerTest matches studentized lmtest::bptest", {
  skip_if_not_installed("lmtest")
  set.seed(101)
  df <- data.frame(y = rnorm(120), x1 = rnorm(120), x2 = rnorm(120))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- performKoenkerTest(model, df)
  ref <- lmtest::bptest(model, studentize = TRUE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("studentized BP matches lmtest::bptest with studentize", {
  skip_if_not_installed("lmtest")
  set.seed(202)
  df <- data.frame(y = rnorm(150), x1 = rnorm(150), x2 = rnorm(150))
  model <- lm(y ~ x1 + x2, data = df)
  ours <- performStudentizedBPTest(model, df)
  ref <- lmtest::bptest(model, studentize = TRUE)
  expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-6)
  expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-6)
})

test_that("performGQTest matches lmtest::gqtest across alternatives", {
  skip_if_not_installed("lmtest")
  data(mtcars)
  model <- lm(mpg ~ wt + qsec, data = mtcars)

  for (alternative in c("greater", "two.sided", "less")) {
    ours <- performGQTest(
      model,
      mtcars,
      order_by = "wt",
      fraction = 0.2,
      alternative = alternative
    )
    ref <- lmtest::gqtest(
      model,
      point = 0.5,
      fraction = 0.2,
      alternative = alternative,
      order.by = mtcars$wt
    )

    expect_equal(unname(ours$statistic), unname(ref$statistic), tolerance = 1e-10)
    expect_equal(unname(ours$parameter), unname(ref$parameter), tolerance = 0)
    expect_equal(unname(ours$p.value), unname(ref$p.value), tolerance = 1e-10)
    expect_equal(ours$alternative, ref$alternative)
  }
})

test_that("Levene test aligns with car::leveneTest", {
  skip_if_not_installed("car")
  set.seed(303)
  df <- data.frame(
    y = rnorm(90),
    g = factor(rep(letters[1:3], each = 30)),
    x = rnorm(90)
  )
  model <- lm(y ~ x, data = df)
  ours <- performLeveneTest(model, df, "g")
  ref <- car::leveneTest(resid(model) ~ g, data = df, center = mean)
  expect_equal(unname(ours$statistic), ref$`F value`[1], tolerance = 1e-6)
  expect_equal(unname(ours$p.value), ref$`Pr(>F)`[1], tolerance = 1e-6)
})
