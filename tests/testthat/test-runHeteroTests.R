library(testthat)
library(heteroTests)


test_that("runHeteroTests returns list with default tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(m, mtcars)
  expect_true(is.list(res))
  expect_equal(names(res), c("white", "koenker"))
  lapply(res, expect_s3_class, class = "htest")
})

# 0.15.0 replaced the classical Breusch-Pagan test in the default battery by
# Koenker's studentized form: the classical statistic assumes normal errors,
# and in the full sweep it rejected a true null 28.75% of the time under t5
# errors, against 4.75% for Koenker's. The classical test stays available by
# name.
test_that("the default battery is White and Koenker, and Breusch-Pagan runs by name", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  res <- runHeteroTests(m, mtcars, use_cache = FALSE, progress = FALSE)
  koenker <- performKoenkerTest(m, mtcars)
  bp <- performBPTest(m, mtcars)
  expect_equal(unname(res$koenker$statistic), unname(koenker$statistic), tolerance = 1e-12)
  expect_equal(res$koenker$p.value, koenker$p.value, tolerance = 1e-12)
  expect_equal(unname(res$white$statistic), unname(performWhiteTest(m, mtcars)$statistic),
               tolerance = 1e-12)
  # The two statistics differ on these data, so the test can tell them apart.
  expect_gt(abs(unname(koenker$statistic) - unname(bp$statistic)), 0.01)

  classical <- runHeteroTests(m, mtcars, tests = "breusch_pagan", use_cache = FALSE,
                              progress = FALSE)
  expect_equal(unname(classical$breusch_pagan$statistic), unname(bp$statistic),
               tolerance = 1e-12)
  expect_equal(classical$breusch_pagan$p.value, bp$p.value, tolerance = 1e-12)

  for (fn in list(runHeteroTests, runDiagnostics, compareModelDiagnostics,
                  compareTestResults, runSurveyHeteroTests,
                  getS3method("summary", "HeteroDiagnostic"),
                  getS3method("test", "HeteroDiagnostic"))) {
    expect_identical(eval(formals(fn)$tests), c("white", "koenker"))
  }
})

test_that("the dashboard's default battery is White and Koenker", {
  # launchDiagnosticDashboard() needs shiny to run, so the default is read
  # from the body of the function.
  src <- deparse(body(launchDiagnosticDashboard), width.cutoff = 500L)
  defaults <- grep("default_tests <- intersect(", src, fixed = TRUE, value = TRUE)
  expect_length(defaults, 1L)
  expect_match(defaults, 'c("white", "koenker")', fixed = TRUE)
  expect_false(grepl("breusch_pagan", defaults, fixed = TRUE))
})

test_that("runHeteroTests runs specified tests", {
  data(mtcars)
  m <- lm(mpg ~ wt + qsec, data = mtcars)
  res <- runHeteroTests(m, mtcars, tests = c("white", "koenker", "cook_weisberg"))
  expect_equal(names(res), c("white", "koenker", "cook_weisberg"))
  lapply(res, expect_s3_class, class = "htest")
})
