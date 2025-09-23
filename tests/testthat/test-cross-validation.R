library(testthat)
library(heteroTests)

context("Cross-validation with other software")

test_that("Levene test matches car package", {
  skip_if_not_installed("car")

  data(mtcars)
  mtcars$cyl_factor <- factor(mtcars$cyl)

  model <- lm(mpg ~ cyl_factor, data = mtcars)
  our_result <- performLeveneTest(model, mtcars, "cyl_factor")
  reference_result <- car::leveneTest(residuals(model) ~ cyl_factor, data = mtcars, center = mean)

  expect_equal(as.numeric(our_result$statistic),
    as.numeric(reference_result$`F value`[1]),
    tolerance = 1e-6
  )
})

test_that("Bartlett test matches stats package", {
  data(mtcars)
  mtcars$cyl_factor <- factor(mtcars$cyl)
  model <- lm(mpg ~ cyl_factor, data = mtcars)

  our_result <- performBartlettTest(model, mtcars, group = "cyl_factor")
  reference_result <- bartlett.test(residuals(model), mtcars$cyl_factor)

  expect_equal(as.numeric(our_result$statistic),
    as.numeric(reference_result$statistic),
    tolerance = 1e-6
  )
  expect_equal(our_result$p.value,
    reference_result$p.value,
    tolerance = 1e-6
  )
})
