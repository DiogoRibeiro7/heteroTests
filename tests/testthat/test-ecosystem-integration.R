skip_if_not_installed <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    testthat::skip(sprintf("Package %s not installed", pkg))
  }
}

testthat::test_that("hetero test suite tidies correctly", {
  data(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(fit, mtcars, tests = "white")
  testthat::expect_s3_class(res, "hetero_test_suite")
  testthat::expect_s3_class(res[[1]], "hetero_test")
  tidy_res <- generics::tidy(res)
  testthat::expect_true(all(c("diagnostic", "statistic", "p.value") %in% names(tidy_res)))
  glance_res <- generics::glance(res[[1]])
  testthat::expect_equal(nrow(glance_res), 1)
})

testthat::test_that("grouped data runs diagnostics per group", {
  skip_if_not_installed("dplyr")
  data(mtcars)
  grouped <- dplyr::group_by(mtcars, cyl)
  res <- runHeteroTests(mpg ~ wt + qsec, grouped, tests = "white")
  testthat::expect_s3_class(res, "hetero_grouped_suite")
  tidy_res <- generics::tidy(res)
  testthat::expect_true("cyl" %in% names(tidy_res))
})

testthat::test_that("data.table inputs are supported", {
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = dt)
  res <- runHeteroTests(fit, dt, tests = "white")
  testthat::expect_s3_class(res[[1]], "hetero_test")
})

testthat::test_that("autoplot returns ggplot objects", {
  data(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(fit, mtcars, tests = c("white", "breusch_pagan"))
  plot_obj <- ggplot2::autoplot(res)
  testthat::expect_s3_class(plot_obj, "ggplot")
})

testthat::test_that("tidymodels workflows are supported", {
  skip_if_not_installed("workflows")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  wf <- workflows::workflow()
  wf <- workflows::add_model(wf, parsnip::linear_reg())
  wf <- workflows::add_recipe(wf, recipes::recipe(mpg ~ wt + qsec, data = mtcars))
  wf <- workflows::fit(wf, data = mtcars)
  res <- runHeteroTests(wf, tests = "white")
  testthat::expect_s3_class(res[[1]], "hetero_test")
})

testthat::test_that("survey designs route through helper", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  res <- runSurveyHeteroTests(api00 ~ api99 + ell, design, tests = "white")
  testthat::expect_s3_class(res, "hetero_test_suite")
})

