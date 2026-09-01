library(testthat)

make_review_group_model <- function(n_per_group = 8L) {
  set.seed(1801)
  g <- factor(rep(c("a", "b", "c"), each = n_per_group))
  x <- rnorm(length(g))
  y <- 1 + x + rnorm(length(g))
  d <- data.frame(y = y, x = x, g = g)
  list(data = d, model = lm(y ~ x, data = d))
}

test_that("O'Brien drops missing auxiliary groups together with residuals", {
  obj <- make_review_group_model(8L)
  d <- obj$data
  d$g[3] <- NA

  observed <- suppressWarnings(performOBrienTest(obj$model, d, "g"))

  # The diagnostic analyses the residuals of the model it was given. A
  # missing grouping value removes that observation from the comparison; it
  # does not refit the model, which would change every residual rather than
  # drop one. Reconstruct the expected result on that basis.
  keep <- !is.na(d$g)
  r <- residuals(obj$model)[keep]
  g_kept <- droplevels(d$g[keep])

  scores <- numeric(length(r))
  for (lv in levels(g_kept)) {
    i <- which(g_kept == lv)
    ni <- length(i)
    scores[i] <- ((ni - 1.5) * ni * (r[i] - mean(r[i]))^2 -
      0.5 * (ni - 1) * var(r[i])) / ((ni - 1) * (ni - 2))
  }
  expected <- anova(lm(scores ~ g_kept))

  expect_equal(unname(observed$statistic), expected$`F value`[1], tolerance = 1e-12)
  expect_equal(unname(observed$parameter),
               c(expected$Df[1], expected$Df[2]), tolerance = 0)
  expect_equal(observed$p.value, expected$`Pr(>F)`[1], tolerance = 1e-12)

  # Refitting on the complete cases is a different question and gives a
  # different answer; pinning that distinction stops the two being conflated.
  refit_data <- d[keep, , drop = FALSE]
  refit <- suppressWarnings(
    performOBrienTest(lm(y ~ x, data = refit_data), refit_data, "g")
  )
  expect_false(isTRUE(all.equal(unname(observed$statistic),
                                unname(refit$statistic))))
})

test_that("O'Brien allows a zero-variance group when transformation is defined", {
  set.seed(1802)
  n <- 6L
  g <- factor(rep(c("a", "b", "c"), each = n))
  x <- rep(0, length(g))
  # The first group has constant residuals; the other groups supply variation.
  y <- c(rep(1, n), rnorm(n, 0, 1), rnorm(n, 0, 2))
  d <- data.frame(y = y, x = x, g = g)
  model <- lm(y ~ 1, data = d)

  expect_s3_class(performOBrienTest(model, d, "g"), "htest")
})

test_that("Hartley preserves public names and uses integer approximate df", {
  set.seed(1803)
  sizes <- c(5L, 6L, 7L)
  g <- factor(rep(c("a", "b", "c"), times = sizes))
  x <- rnorm(sum(sizes))
  y <- 1 + x + rnorm(sum(sizes), sd = rep(c(1, 1.2, 1.5), times = sizes))
  d <- data.frame(y = y, x = x, g = g)
  model <- lm(y ~ x, data = d)

  expect_warning(
    result <- performHartleyFmaxTest(model, d, "g"),
    "rounded mean group size"
  )
  expect_identical(names(result$statistic), "F")
  expect_identical(result$method, "Hartley's Fmax test")
  expect_true(result$parameter[["df"]] == as.integer(result$parameter[["df"]]))
})

test_that("Bartlett and its compatibility alias support two observations per group", {
  set.seed(1804)
  g <- factor(rep(c("a", "b", "c"), each = 2L))
  x <- seq_along(g)
  y <- c(1, 2, 1, 4, 2, 7)
  d <- data.frame(y = y, x = x, g = g)
  model <- lm(y ~ 1, data = d)

  canonical <- suppressWarnings(performBartlettTest(model, d, "g"))
  alias <- suppressWarnings(performModifiedBartlettTest(model, d, "g"))
  ref <- stats::bartlett.test(residuals(model), g)

  expect_equal(unname(canonical$statistic), unname(ref$statistic), tolerance = 0)
  expect_equal(canonical$p.value, ref$p.value, tolerance = 0)
  expect_identical(alias, canonical)
})

test_that("Pass B summary rejects methods with no usable replications", {
  path <- system.file("validation", "pass-b-size-power.R", package = "heteroTests")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "validation", "pass-b-size-power.R")
  }
  skip_if_not(file.exists(path), "Pass B simulation script is not available")

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  rejected <- c(A = 0L, B = 1L)
  failures <- c(A = 100L, B = 0L)
  expect_error(
    env$summarise_pass_b_counts(rejected, failures, 100L),
    "Pass B produced no usable replications for: A",
    fixed = TRUE
  )
})

test_that("Pass B summary uses effective replications", {
  path <- system.file("validation", "pass-b-size-power.R", package = "heteroTests")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "validation", "pass-b-size-power.R")
  }
  skip_if_not(file.exists(path), "Pass B simulation script is not available")

  env <- new.env(parent = globalenv())
  sys.source(path, envir = env)

  rejected <- c(A = 10L, B = 25L)
  failures <- c(A = 0L, B = 50L)
  summary <- env$summarise_pass_b_counts(rejected, failures, 100L)

  expect_equal(summary$effective, c(A = 100L, B = 50L))
  expect_equal(summary$rate, c(A = 0.10, B = 0.50))
})
