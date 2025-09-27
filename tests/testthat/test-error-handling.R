testthat::test_that("data cleaning suggestions cover common issues", {
  df <- data.frame(a = c(1, 2, NA), b = c(Inf, 3, 4))
  sugg <- heteroTests:::ht_data_cleaning_suggestions("NA/NaN encountered", df)
  testthat::expect_true(any(grepl("missing values", sugg)))
  testthat::expect_true(any(grepl("infinite values", sugg)))
})

testthat::test_that("heteroscedasticity diagnostics fall back gracefully", {
  data(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = mtcars)
  registry <- heteroTests:::.diagnostic_registry
  original <- get("white", envir = registry)
  on.exit(assign("white", original, envir = registry), add = TRUE)
  assign(
    "white",
    function(model, data) {
      stop("Auxiliary regression is rank deficient", call. = FALSE)
    },
    envir = registry
  )
  res <- runHeteroTests(fit, mtcars, tests = "white", use_cache = FALSE)
  testthat::expect_s3_class(res[[1]], "hetero_test")
  extras <- attr(res[[1]], "extras")
  testthat::expect_equal(extras$status, "fallback")
  testthat::expect_equal(extras$fallback_for, "white")
  testthat::expect_true(any(grepl("collinear", extras$suggestions)))
})
