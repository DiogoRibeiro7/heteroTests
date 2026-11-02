testthat::test_that("data cleaning suggestions cover common issues", {
  df <- data.frame(a = c(1, 2, NA), b = c(Inf, 3, 4))
  sugg <- heteroTests:::ht_data_cleaning_suggestions("NA/NaN encountered", df)
  testthat::expect_true(any(grepl("missing values", sugg)))
  testthat::expect_true(any(grepl("infinite values", sugg)))
})

testthat::test_that("a failure is labelled by whole words, NA only in capitals", {
  issue_of <- function(message) {
    heteroTests:::ht_analyse_failure("white", simpleError(message), NULL, NULL)$issue
  }
  # Before 0.18.0 the words were matched anywhere and in any case: the "na" of
  # a function's name made a data-quality issue, and the "Rank" of another a
  # rank deficiency, with the advice of each.
  inside <- c(
    "performHighDimensionalTest() cannot use this model.",
    "performRankPermutationTest() cannot use this model.",
    "the influence of the design is not finite here.",
    "the signal is not significant."
  )
  for (message in inside) {
    testthat::expect_identical(issue_of(message), "diagnostic_failure", info = message)
    testthat::expect_identical(
      heteroTests:::ht_data_cleaning_suggestions(message), character(), info = message
    )
  }
  # The names of the rank tests are not a rank deficiency either.
  for (message in c(
    "Only 15 observations detected but Rank permutation requires at least 20.",
    "Rank-based statistic requires enough ordered observations",
    "the Spearman rank correlation test cannot run."
  )) {
    testthat::expect_identical(issue_of(message), "diagnostic_failure", info = message)
  }
  testthat::expect_identical(issue_of("Koenker auxiliary regression became rank deficient"),
                             "rank_deficiency")
  testthat::expect_identical(issue_of("Lapack routine dgesv: system is exactly singular"),
                             "rank_deficiency")
  testthat::expect_identical(issue_of("performX() cannot use a rank-deficient model"),
                             "rank_deficiency")
  testthat::expect_identical(issue_of("glm.fit: perfect Separation occurred"), "perfect_separation")
  testthat::expect_identical(issue_of("NA/NaN/Inf in 'y'"), "data_quality")
  testthat::expect_identical(issue_of("Missing values detected in x2."), "data_quality")
  testthat::expect_identical(issue_of("Check for NA or infinite estimates."), "data_quality")
  # "na" and "inf" in small letters are not R's NA and Inf.
  testthat::expect_identical(issue_of("the value was na."), "diagnostic_failure")
  testthat::expect_identical(issue_of("the bound is inf."), "diagnostic_failure")
  sugg <- heteroTests:::ht_data_cleaning_suggestions("NA/NaN/Inf in 'y'")
  testthat::expect_true(any(grepl("missing values", sugg)))
  testthat::expect_true(any(grepl("infinite values", sugg)))
})

testthat::test_that("the refusal of a glm that is not a least-squares fit is final", {
  fit <- suppressWarnings(glm(am ~ wt + hp, data = mtcars, family = binomial))
  e <- tryCatch(performRankPermutationTest(fit, mtcars, B = 9), error = identity)
  testthat::expect_s3_class(e, "heteroTests_glm_family_error")
  failure <- heteroTests:::ht_analyse_failure("rank_permutation", e, fit, mtcars)
  testthat::expect_identical(failure$issue, "glm_family")
  testthat::expect_identical(failure$fallbacks, character())
  testthat::expect_identical(failure$suggestions, character())
  testthat::expect_identical(
    failure$user_message,
    paste0("Model is not a least-squares fit for Rank Permutation: ", conditionMessage(e))
  )
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
