library(testthat)

# ---------------------------------------------------------------------------
# tidy() and autoplot() of a suite of tests.
#
# Two faults, both of which showed in the plot.
#
# tidy() passed the whole `parameter` vector of a result to data.frame(), so a
# test with two parameters (an F statistic, or the wild bootstrap with its
# degrees of freedom and number of replications) came back as two rows, and
# autoplot() drew it twice.
#
# autoplot() drew the p-values themselves on an axis from 0 to 1, so a test
# that rejected had no visible bar, and it chose the colours with
# `isTRUE(p < 0.05)`, which is FALSE for a suite of more than one test.
# ---------------------------------------------------------------------------

make_suite <- function(tests = c("white", "breusch_pagan", "koenker")) {
  set.seed(42)
  d <- data.frame(x = runif(150, 1, 5))
  d$y <- 1 + 2 * d$x + d$x^2 * rnorm(150)
  fit <- lm(y ~ x, data = d)
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  suppressWarnings(
    runHeteroTests(fit, d, tests = tests, use_cache = FALSE, progress = FALSE)
  )
}

with_p_values <- function(suite, p) {
  for (i in seq_along(suite)) {
    suite[[i]]$p.value <- p[[i]]
  }
  suite
}

test_that("tidy gives one row per test, whatever the test returns", {
  suite <- make_suite(c("koenker", "wild_bootstrap", "variance_form"))
  tidied <- generics::tidy(suite)
  expect_identical(tidied$diagnostic, c("koenker", "wild_bootstrap", "variance_form"))

  # One parameter: nothing in the second column.
  expect_true(is.na(tidied$parameter2[1]))
  # Two parameters: both kept, in order.
  expect_equal(
    c(tidied$parameter[2], tidied$parameter2[2]),
    unname(suite$wild_bootstrap$parameter)
  )
  expect_equal(
    c(tidied$parameter[3], tidied$parameter2[3]),
    unname(suite$variance_form$parameter)
  )
  expect_equal(nrow(generics::glance(suite$variance_form)), 1L)
})

test_that("tidy reports an estimate only when there is exactly one", {
  suite <- make_suite("variance_form")
  one <- generics::tidy(suite)
  expect_equal(one$estimate, unname(suite$variance_form$estimate))

  several <- suite
  several$variance_form$estimate <- c(a = 1, b = 2)
  expect_true(is.na(generics::tidy(several)$estimate))
  expect_equal(nrow(generics::tidy(several)), 1L)
})

test_that("autoplot draws evidence, so a smaller p-value is a longer bar", {
  suite <- with_p_values(make_suite(), c(0.2, 0.03, 0.0004))
  drawn <- ggplot2::autoplot(suite)$data
  drawn <- drawn[match(c("white", "breusch_pagan", "koenker"), drawn$diagnostic), ]

  expect_equal(drawn$.evidence, -log10(c(0.2, 0.03, 0.0004)), tolerance = 1e-12)
  expect_identical(
    as.character(drawn$.state),
    c("does_not_reject", "rejects", "rejects")
  )
  expect_identical(drawn$.label, c("p = 0.200", "p = 0.030", "p < 0.001"))
  # The first test requested is the top bar of a horizontal chart.
  expect_identical(levels(drawn$diagnostic), c("koenker", "breusch_pagan", "white"))
})

test_that("autoplot caps the bars and keeps the p-value in the label", {
  suite <- with_p_values(make_suite(), c(1e-12, 0, 1))
  drawn <- ggplot2::autoplot(suite)$data
  drawn <- drawn[match(c("white", "breusch_pagan", "koenker"), drawn$diagnostic), ]
  expect_equal(drawn$.evidence, c(4, 4, 0))
  expect_identical(drawn$.label, c("p < 0.001", "p < 0.001", "p = 1.000"))
})

test_that("autoplot marks a test without a result instead of dropping it", {
  suite <- with_p_values(make_suite(), c(0.01, NA, 0.5))
  plot <- ggplot2::autoplot(suite)
  drawn <- plot$data[plot$data$diagnostic == "breusch_pagan", ]
  expect_identical(as.character(drawn$.state), "unavailable")
  expect_equal(drawn$.evidence, 0)
  expect_identical(drawn$.label, "no result")
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
})

test_that("autoplot uses the level it is given", {
  suite <- with_p_values(make_suite(), c(0.2, 0.03, 0.0004))
  strict <- ggplot2::autoplot(suite, alpha = 0.01)
  drawn <- strict$data[match(c("white", "breusch_pagan", "koenker"), strict$data$diagnostic), ]
  expect_identical(
    as.character(drawn$.state),
    c("does_not_reject", "does_not_reject", "rejects")
  )
  expect_match(strict$labels$subtitle, "p < 0.01", fixed = TRUE)
  expect_error(ggplot2::autoplot(suite, alpha = 1.5), "between 0 and 1")
  expect_error(ggplot2::autoplot(suite, alpha = c(0.01, 0.05)), "between 0 and 1")
})

test_that("autoplot draws each test once", {
  suite <- make_suite(c("koenker", "wild_bootstrap", "variance_form"))
  plot <- ggplot2::autoplot(suite)
  expect_equal(nrow(plot$data), 3L)
  expect_false(anyDuplicated(plot$data$diagnostic) > 0)
})

test_that("a grouped suite gets one panel per group and nothing else", {
  skip_if_not_installed("dplyr")
  set.seed(7)
  d <- data.frame(g = rep(c("a", "b"), each = 90), x = runif(180, 1, 5))
  d$y <- 1 + 2 * d$x + ifelse(d$g == "b", d$x^2, 1) * rnorm(180)
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  grouped <- suppressWarnings(runHeteroTests(
    y ~ x, dplyr::group_by(d, g),
    tests = c("white", "koenker"), progress = FALSE
  ))
  plot <- ggplot2::autoplot(grouped)
  expect_identical(names(plot$facet$params$facets), "g")
  expect_equal(nrow(plot$data), 4L)

  states <- as.character(plot$data$.state)
  expect_identical(states, ifelse(plot$data$p.value < 0.05, "rejects", "does_not_reject"))
  # The heteroscedastic group rejects and the other does not.
  expect_true(all(states[plot$data$g == "b"] == "rejects"))
  expect_s3_class(ggplot2::ggplot_build(plot), "ggplot_built")
})
