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
  # White's test of the engine's fit, on its model frame. Before 0.16.0 the
  # data were the predictors of the mold, without the response, which White's
  # test refused, and the result was the Cook-Weisberg fallback.
  engine <- workflows::extract_fit_engine(wf)
  testthat::expect_null(attr(res$white, "extras")$status)
  testthat::expect_identical(res$white$method, "White's test for heteroscedasticity")
  testthat::expect_equal(
    unname(res$white$statistic),
    unname(performWhiteTest(engine, stats::model.frame(engine))$statistic)
  )
  # The data the workflow was fitted to lack the response the engine's fit
  # names `..y`: until 0.17.0 they were refused ("`data` lacks ..y. Pass
  # `data = NULL`"); they are mapped to the variables of the engine by the
  # workflow's preprocessing, and the model frame of the engine's fit is
  # tested, as without `data` (N5).
  testthat::expect_identical(runHeteroTests(wf, mtcars, tests = "white"), res)
  testthat::expect_error(
    runHeteroTests(wf, mtcars[, c("wt", "qsec")], tests = "white"),
    "the workflow's preprocessing cannot map `data` to them", fixed = TRUE
  )
})

testthat::test_that("survey designs route through helper", {
  skip_if_not_installed("survey")
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  testthat::expect_warning(
    res <- runSurveyHeteroTests(api00 ~ api99 + ell, design, tests = "white"),
    "does not use the sampling weights"
  )
  testthat::expect_s3_class(res, "hetero_test_suite")
})

