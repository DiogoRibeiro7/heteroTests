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

  expect_s3_class(performOBrienTest(obj$model, d, "g"), "htest")
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

test_that("Pass B simulation catches method failures independently", {
  # Installed packages expose inst/ at the package root, so the source-tree
  # relative path only works when running from a checkout.
  path <- system.file("validation", "pass-b-size-power.R", package = "heteroTests")
  if (!nzchar(path) || !file.exists(path)) {
    path <- testthat::test_path("..", "..", "inst", "validation", "pass-b-size-power.R")
  }
  skip_if_not(file.exists(path), "Pass B simulation script is not available")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")

  expect_match(text, "pkgload::load_all", fixed = TRUE)
  expect_match(text, "tryCatch(as.numeric(run_one())", fixed = TRUE)
  expect_match(text, "if (any(effective <= 0L))", fixed = TRUE)
})
