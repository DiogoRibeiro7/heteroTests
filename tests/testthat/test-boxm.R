library(heteroTests)

test_that("performBoxMTest works on iris", {
  res <- performBoxMTest(iris[,1:4], iris$Species)
  expect_s3_class(res, "htest")
  expect_true(res$p.value <= 1 && res$p.value >= 0)
})

# Box's (1949) chi-square approximation written out: M (1 - c1) on
# (g - 1) p (p + 1) / 2 degrees of freedom, with nu_i = n_i - 1, nu their sum
# and c1 = (sum 1 / nu_i - 1 / nu) (2 p^2 + 3 p - 1) / (6 (p + 1) (g - 1)).
box_m_by_hand <- function(data, group) {
  group <- droplevels(as.factor(group))
  Y <- as.matrix(data)
  p <- ncol(Y)
  g <- nlevels(group)
  nu <- as.numeric(table(group)) - 1
  covs <- lapply(levels(group), function(l) stats::cov(Y[group == l, , drop = FALSE]))
  pooled <- Reduce(`+`, Map(`*`, covs, nu)) / sum(nu)
  M <- sum(nu) * log(det(pooled)) - sum(nu * vapply(covs, function(S) log(det(S)), 0))
  c1 <- (sum(1 / nu) - 1 / sum(nu)) * (2 * p^2 + 3 * p - 1) / (6 * (p + 1) * (g - 1))
  df <- (g - 1) * p * (p + 1) / 2
  stat <- M * (1 - c1)
  c(statistic = stat, df = df, p.value = stats::pchisq(stat, df, lower.tail = FALSE))
}

test_that("performBoxMTest gives the values of biotools::boxM() and heplots::boxM()", {
  # The reference values were computed on 2026-10-25 with biotools::boxM()
  # 4.3 and heplots::boxM() 1.8.6, which agree to the last digit printed here
  # (inst/validation/box-m-approximations-size.R checks the package against
  # both on ten data sets); rstatix::box_m() gives the iris value too. Neither
  # package is a dependency of heteroTests, so the values are pinned. Before
  # 0.16.0 the correction factor was (g - 1) times Box's, and the package gave
  # 135.2229 on iris and 20.3998 on mtcars.
  iris_res <- performBoxMTest(iris[, 1:4], iris$Species)
  expect_equal(unname(iris_res$statistic), 140.943049923, tolerance = 1e-10)
  expect_equal(unname(iris_res$parameter), 20, tolerance = 0)
  expect_equal(iris_res$p.value, 3.35203417832e-20, tolerance = 1e-8)

  # Three unbalanced groups of 11, 7 and 14 cars, three variables.
  cars_res <- performBoxMTest(mtcars[, c("mpg", "hp", "wt")], mtcars$cyl)
  expect_equal(unname(cars_res$statistic), 25.5350623996, tolerance = 1e-10)
  expect_equal(unname(cars_res$parameter), 12, tolerance = 0)
  expect_equal(cars_res$p.value, 0.0124812637686, tolerance = 1e-8)
})

test_that("performBoxMTest is Box's chi-square approximation for any number of groups", {
  set.seed(20261025)
  for (sizes in list(c(12, 30), c(10, 10, 10), c(20, 50, 100), c(10, 15, 20, 30, 50))) {
    for (p in c(1, 2, 4)) {
      group <- factor(rep(seq_along(sizes), sizes))
      Y <- matrix(rnorm(sum(sizes) * p), ncol = p) * as.integer(group)^0.3
      ours <- performBoxMTest(as.data.frame(Y), group)
      ref <- box_m_by_hand(Y, group)
      info <- paste(paste(sizes, collapse = ","), "p =", p)
      expect_equal(unname(ours$statistic), unname(ref[["statistic"]]), tolerance = 1e-12,
                   info = info)
      expect_equal(unname(ours$parameter), unname(ref[["df"]]), tolerance = 0, info = info)
      expect_equal(ours$p.value, unname(ref[["p.value"]]), tolerance = 1e-10, info = info)
    }
  }
})

test_that("performBoxMTest tests the groups observed and drops an empty level", {
  # subset() keeps the level "virginica" with no observation. Before 0.16.0 its
  # covariance matrix was all NA and so was the result, on 20 degrees of
  # freedom; the test is now that of the two species observed, the value of
  # droplevels() of the subset and of biotools::boxM() 4.3 on the subset,
  # 66.8104812066 on 10 degrees of freedom.
  sub <- subset(iris, Species != "virginica")
  expect_identical(levels(sub$Species), levels(iris$Species))
  res <- performBoxMTest(sub[, 1:4], sub$Species)
  two <- droplevels(sub)
  ref <- performBoxMTest(two[, 1:4], two$Species)
  expect_identical(unname(res$statistic), unname(ref$statistic))
  expect_identical(unname(res$parameter), unname(ref$parameter))
  expect_identical(res$p.value, ref$p.value)
  expect_equal(unname(res$statistic), 66.8104812066, tolerance = 1e-10)
  expect_equal(unname(res$parameter), 10, tolerance = 0)
  expect_equal(res$p.value, 1.8233e-10, tolerance = 1e-4)

  # A character group has no empty level, and gives the same values.
  chr <- performBoxMTest(sub[, 1:4], as.character(sub$Species))
  expect_identical(unname(chr$statistic), unname(res$statistic))
  expect_identical(chr$p.value, res$p.value)

  # An unused level of a factor of mtcars' cylinders.
  cyl <- factor(mtcars$cyl, levels = c(4, 6, 8, 10))
  expect_identical(
    unname(performBoxMTest(mtcars[, c("mpg", "hp", "wt")], cyl)$statistic),
    unname(performBoxMTest(mtcars[, c("mpg", "hp", "wt")], factor(mtcars$cyl))$statistic)
  )

  # A group of one observation still has no covariance matrix, and the result
  # is NA as before, whether the group is a factor or a character vector.
  one <- rbind(sub, iris[iris$Species == "virginica", ][1, ])
  expect_true(is.na(performBoxMTest(one[, 1:4], one$Species)$statistic))
  expect_true(is.na(performBoxMTest(one[, 1:4], as.character(one$Species))$statistic))
})

test_that("performBoxMTest with two groups is unchanged by the correction of 0.16.0", {
  # With two groups the factor of 0.15.0, (g - 1) c1, is c1: the values are
  # those of 0.15.0, which gave 66.8104812066 for two species of iris and
  # 15.4292231251 for mtcars by transmission, as biotools::boxM() and
  # heplots::boxM() do.
  two <- droplevels(subset(iris, Species != "virginica"))
  expect_equal(unname(performBoxMTest(two[, 1:4], two$Species)$statistic), 66.8104812066,
               tolerance = 1e-10)
  expect_equal(unname(performBoxMTest(mtcars[, c("mpg", "hp", "wt")], mtcars$am)$statistic),
               15.4292231251, tolerance = 1e-10)
})
