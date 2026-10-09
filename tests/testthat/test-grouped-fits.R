library(testthat)

# ---------------------------------------------------------------------------
# Grouped analyses of a fitted model.
#
# runHeteroTests(fit, grouped_data) refits the model within each group. Up to
# 0.12.0 the refit passed the weights to stats::lm() as a local variable, and
# lm() does not look there: it found stats::weights instead and stopped with
# "invalid type (closure) for variable '(weights)'" for every fitted model,
# weighted or not. Only a formula with grouped data worked.
#
# The refit now evaluates the model's own call on each group, so the checks
# below compare it with fitting that call to each group by hand.
# ---------------------------------------------------------------------------

make_grouped <- function(seed = 11, n_per_group = 80) {
  set.seed(seed)
  n <- 3 * n_per_group
  d <- data.frame(
    g = rep(c("a", "b", "c"), each = n_per_group),
    x = runif(n, 1, 3),
    z = rnorm(n)
  )
  d$w <- 1 / d$x^2
  d$y <- 1 + 2 * d$x + d$z + d$x * rnorm(n)
  d$b <- rbinom(n, 1, stats::plogis(d$z))
  d
}

group_statistics <- function(suite) {
  vapply(suite, function(s) unname(s[[1]]$statistic), numeric(1))
}

group_models <- function(suite) {
  lapply(suite, function(s) attr(s, "model"))
}

test_that("a fitted lm and its formula give the same grouped results", {
  skip_if_not_installed("dplyr")
  d <- make_grouped()
  grouped <- dplyr::group_by(d, g)
  from_formula <- runHeteroTests(
    y ~ x + z, grouped,
    tests = "koenker", progress = FALSE
  )
  from_fit <- runHeteroTests(
    lm(y ~ x + z, data = d), grouped,
    tests = "koenker", progress = FALSE
  )
  expect_s3_class(from_fit, "hetero_grouped_suite")
  expect_identical(group_statistics(from_fit), group_statistics(from_formula))
  expect_identical(attr(from_fit, "group_keys")$g, c("a", "b", "c"))
})

test_that("each group is refitted with its own rows of the weights", {
  skip_if_not_installed("dplyr")
  d <- make_grouped()
  fit <- lm(y ~ x + z, data = d, weights = w)
  res <- runHeteroTests(
    fit, dplyr::group_by(d, g),
    tests = "koenker", progress = FALSE
  )
  by_hand <- vapply(split(d, d$g), function(part) {
    part_fit <- lm(y ~ x + z, data = part, weights = w)
    unname(performKoenkerTest(part_fit, part)$statistic)
  }, numeric(1))
  expect_equal(group_statistics(res), unname(by_hand), tolerance = 1e-10)
  for (i in seq_along(res)) {
    part <- d[d$g == c("a", "b", "c")[i], ]
    expect_equal(
      unname(stats::weights(group_models(res)[[i]])),
      part$w,
      tolerance = 1e-12
    )
  }
})

test_that("weights that are not a column of the data are refused", {
  skip_if_not_installed("dplyr")
  d <- make_grouped()
  loose_weights <- 1 / d$x^2
  fit <- lm(y ~ x + z, data = d, weights = loose_weights)
  expect_error(
    runHeteroTests(
      fit, dplyr::group_by(d, g),
      tests = "koenker", progress = FALSE
    ),
    "must be a column of `data`"
  )
})

test_that("a grouped glm keeps its family", {
  skip_if_not_installed("dplyr")
  d <- make_grouped()
  fit <- glm(b ~ z, data = d, family = binomial)
  res <- runHeteroTests(
    fit, dplyr::group_by(d, g),
    tests = "white", progress = FALSE
  )
  families <- vapply(
    group_models(res),
    function(m) stats::family(m)$family,
    character(1)
  )
  expect_identical(families, rep("binomial", 3))
})

test_that("the subset of the model is applied within each group", {
  skip_if_not_installed("dplyr")
  d <- make_grouped()
  fit <- lm(y ~ x, data = d, subset = z > 0)
  res <- runHeteroTests(
    fit, dplyr::group_by(d, g),
    tests = "koenker", progress = FALSE
  )
  used <- vapply(group_models(res), stats::nobs, numeric(1))
  expect_equal(unname(used), as.numeric(table(d$g[d$z > 0])))
})

test_that("the refit works when the formula is a local variable", {
  skip_if_not_installed("dplyr")
  run_inside <- function() {
    local_data <- make_grouped()
    local_formula <- y ~ x
    fit <- lm(local_formula, data = local_data)
    runHeteroTests(
      fit, dplyr::group_by(local_data, g),
      tests = "koenker", progress = FALSE
    )
  }
  expect_s3_class(run_inside(), "hetero_grouped_suite")
})
