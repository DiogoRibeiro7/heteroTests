library(testthat)
library(heteroTests)

context("Cross-validation with other software")

test_that("Levene test matches car package", {
  skip_if_not_installed("car")

  data(mtcars)
  mtcars$cyl_factor <- factor(mtcars$cyl)

  our_result <- performLeveneTest(mpg ~ cyl_factor, data = mtcars)
  reference_result <- car::leveneTest(mpg ~ cyl_factor, data = mtcars)

  expect_equal(as.numeric(our_result$statistic),
    as.numeric(reference_result$`F value`[1]),
    tolerance = 1e-6
  )
})

test_that("Bartlett test matches stats package", {
  data(mtcars)
  groups <- split(mtcars$mpg, mtcars$cyl)

  our_result <- performBartlettTest(groups)
  reference_result <- bartlett.test(groups)

  expect_equal(as.numeric(our_result$statistic),
    as.numeric(reference_result$statistic),
    tolerance = 1e-6
  )
  expect_equal(our_result$p.value,
    reference_result$p.value,
    tolerance = 1e-6
  )
})
