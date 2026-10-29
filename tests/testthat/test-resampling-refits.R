library(testthat)

# ---------------------------------------------------------------------------
# The refits of the resampling tests on unweighted fits.
#
# performWildBootstrapTest(), performWhiteTestBootstrap() and
# bootstrapTestStatistic() regenerate the response, or the rows, and refit
# the model. Three things the refit of an unweighted fit got wrong:
#
# - an offset given as an argument, lm(..., offset = o), is kept in the model
#   frame and not in the formula, and a refit from the formula dropped it;
# - bootstrapTestStatistic() took the fitted values by position, so data
#   holding the fitted rows in another order added each row's draw to another
#   row's fitted value;
# - the wild bootstrap evaluated the call of the fit again in its model frame,
#   which holds log(x) and not x, and failed on a transformed regressor.
#
# Each check rebuilds the replicates from the same random draws with an
# explicit refit of the model on the regenerated data, so it fails if the
# package drops the offset, mismatches a row, or refits another model.
# ---------------------------------------------------------------------------

make_offset_fit <- function(seed = 7, n = 80) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  # Not in the span of the regressors, or dropping it would not show.
  d$o <- 0.5 * d$x^2
  d$y <- 1 + 2 * d$x + 0.5 * d$z + d$o + rnorm(n)
  list(data = d, model = lm(y ~ x + z, data = d, offset = o))
}

# n R^2 of the squared residuals on the terms of `formula`, which is Koenker's
# statistic for the regressors of the model and White's for their squares and
# products.
squared_residual_r2 <- function(e, formula, data) {
  data$.e2 <- e^2
  nrow(data) * summary(lm(stats::update(formula, .e2 ~ .), data = data))$r.squared
}

white_terms <- ~ x + z + I(x^2) + I(z^2) + I(x * z)

# --- an offset given as an argument -----------------------------------------

test_that("an unweighted refit keeps an offset given as an argument", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  obj <- make_offset_fit()
  d <- obj$data
  m <- obj$model
  n <- nrow(d)
  B <- 5L
  refit <- function(db) lm(y ~ x + z, data = db, offset = o)

  set.seed(51)
  white <- suppressWarnings(performWhiteTestBootstrap(m, d, B = B))
  set.seed(51)
  expected <- vapply(seq_len(B), function(b) {
    db <- d
    db$y <- fitted(m) + sample(residuals(m), replace = TRUE)
    squared_residual_r2(residuals(refit(db)), white_terms, d)
  }, numeric(1))
  expect_equal(white$boot_statistics, expected, tolerance = 1e-8)

  pool <- residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)

  set.seed(52)
  null <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, d, B = B, progress = FALSE
  ))
  set.seed(52)
  expected <- vapply(seq_len(B), function(b) {
    db <- d
    db$y <- fitted(m) + sample(pool, n, replace = TRUE)
    squared_residual_r2(residuals(refit(db)), ~ x + z, d)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)

  set.seed(53)
  pairs <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, d, B = B, resample = "pairs", progress = FALSE
  ))
  set.seed(53)
  expected <- vapply(seq_len(B), function(b) {
    db <- d[sample.int(n, replace = TRUE), , drop = FALSE]
    squared_residual_r2(residuals(refit(db)), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)

  set.seed(54)
  wild <- performWildBootstrapTest(m, d, B = B, progress = FALSE)
  set.seed(54)
  expected <- vapply(seq_len(B), function(b) {
    v <- sample(c(-1, 1), n, replace = TRUE)
    db <- d
    db$y <- fitted(m) + sample(pool, n, replace = TRUE) * v
    squared_residual_r2(residuals(refit(db)), ~ x + z, d)
  }, numeric(1))
  expect_equal(unname(wild$statistic), squared_residual_r2(residuals(m), ~ x + z, d),
               tolerance = 1e-8)
  expect_equal(wild$bootstrap$replicates, expected, tolerance = 1e-8)
})

test_that("an offset resamples alike as an argument and in the formula", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  # The formula route always kept the offset in bootstrapTestStatistic()
  # and the White bootstrap, because a refit evaluates offset(o) again from
  # the data. The wild bootstrap failed on both, evaluating the call of the
  # fit again in a model frame that holds offset(o) and not o.
  obj <- make_offset_fit()
  d <- obj$data
  in_formula <- lm(y ~ x + z + offset(o), data = d)

  run <- function(model) {
    set.seed(61)
    white <- suppressWarnings(performWhiteTestBootstrap(model, d, B = 5))
    set.seed(62)
    null <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, model, d, B = 5, progress = FALSE
    ))
    set.seed(63)
    pairs <- suppressWarnings(bootstrapTestStatistic(
      performKoenkerTest, model, d, B = 5, resample = "pairs", progress = FALSE
    ))
    set.seed(64)
    wild <- performWildBootstrapTest(model, d, B = 5, progress = FALSE)
    c(white$boot_statistics, null$replicates, pairs$replicates,
      wild$bootstrap$replicates)
  }
  expected <- run(in_formula)
  expect_true(all(is.finite(expected)))
  expect_equal(run(obj$model), expected, tolerance = 1e-8)
})

# --- the rows of the data -----------------------------------------------------

test_that("null resampling of an unweighted fit follows the rows of the data", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  # Data holding the fitted rows sorted by z. Taken by position, each row was
  # regenerated from another row's fitted value, and the replicates described
  # a regression whose mean has nothing to do with its regressors.
  set.seed(8)
  n <- 80
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + rnorm(n)
  m <- lm(y ~ x + z, data = d)
  sorted <- d[order(d$z), ]

  pool <- residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  fit_sorted <- fitted(m)[rownames(sorted)]

  set.seed(71)
  null <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, m, sorted, B = 5, progress = FALSE
  ))
  set.seed(71)
  expected <- vapply(seq_len(5), function(b) {
    db <- sorted
    db$y <- fit_sorted + sample(pool, n, replace = TRUE)
    squared_residual_r2(residuals(lm(y ~ x + z, data = db)), ~ x + z, db)
  }, numeric(1))
  expect_equal(null$replicates, expected, tolerance = 1e-8)

  # The slope of each replicate is the slope of the fit plus that of the
  # regenerated errors, so it stays near the fitted one, 2.
  slope <- function(model, data) list(statistic = stats::coef(model)[["x"]])
  set.seed(72)
  slopes <- bootstrapTestStatistic(slope, m, sorted, B = 20, progress = FALSE)
  expect_true(all(abs(slopes$replicates - stats::coef(m)[["x"]]) < 0.5))
})

test_that("an offset argument follows the rows of the data in pairs resampling", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  obj <- make_offset_fit()
  sorted <- obj$data[order(obj$data$z), ]
  n <- nrow(sorted)

  set.seed(73)
  pairs <- suppressWarnings(bootstrapTestStatistic(
    performKoenkerTest, obj$model, sorted, B = 5, resample = "pairs", progress = FALSE
  ))
  set.seed(73)
  expected <- vapply(seq_len(5), function(b) {
    db <- sorted[sample.int(n, replace = TRUE), , drop = FALSE]
    refit <- lm(y ~ x + z, data = db, offset = o)
    squared_residual_r2(residuals(refit), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)
})

test_that("resampling an unweighted fit uses the rows the model was fitted to", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(9)
  n <- 60
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + rnorm(n)
  m <- lm(y ~ x + z, data = d)
  run <- function(model, data, strategy, seed, fn = performKoenkerTest) {
    set.seed(seed)
    bootstrapTestStatistic(fn, model, data, B = 5, resample = strategy, progress = FALSE)
  }

  # Rows the fit does not have are left out, for the observed statistic as
  # for the replicates, so data with extra rows resamples as the data of the
  # fit.
  longer <- rbind(d, d[1:5, ])
  for (strategy in c("null", "pairs")) {
    ours <- run(m, longer, strategy, 94)
    expected <- run(m, d, strategy, 94)
    expect_equal(ours$original_statistic, expected$original_statistic, tolerance = 1e-12)
    expect_equal(ours$replicates, expected$replicates, tolerance = 1e-12)
  }

  # A row the fit dropped for a missing value is one of those rows. Pairs
  # resampling used to draw it as well, and the refit dropped it again, so
  # each replicate was fitted to a different number of rows; null resampling
  # refused the data. Both now resample the rows the fit kept.
  with_missing <- d
  with_missing$x[3] <- NA
  m_missing <- lm(y ~ x + z, data = with_missing)
  kept <- with_missing[rownames(stats::model.frame(m_missing)), ]
  rows_used <- function(model, data) list(statistic = stats::nobs(model))
  for (strategy in c("null", "pairs")) {
    ours <- run(m_missing, with_missing, strategy, 95)
    expected <- run(m_missing, kept, strategy, 95)
    expect_equal(ours$replicates, expected$replicates, tolerance = 1e-12)
    expect_true(all(is.finite(ours$replicates)))
    expect_equal(run(m_missing, with_missing, strategy, 96, rows_used)$replicates,
                 rep(n - 1, 5))
  }

  # Data that lacks a fitted row is refused: rows under other names, or part
  # of the rows. The package's tests refuse data whose row names do not match
  # the fit, so the refusal is checked with a test function that does not
  # look at them.
  by_position <- function(model, data) list(statistic = sum(stats::residuals(model)^2))
  renamed <- d
  rownames(renamed) <- paste0("r", seq_len(n))
  for (strategy in c("null", "pairs")) {
    expect_error(
      bootstrapTestStatistic(by_position, m, renamed, B = 5,
                             resample = strategy, progress = FALSE),
      "rows the model was fitted to"
    )
    expect_error(
      bootstrapTestStatistic(by_position, m, d[1:50, ], B = 5,
                             resample = strategy, progress = FALSE),
      "it lacks 10 of them"
    )
  }
})

test_that("data without row names of its own must be in the order of the fit", {
  # A tibble or a data.table numbers its rows 1 to n whatever their order, so
  # a match by name pairs them by position. So does a data frame whose row
  # names were reset, which is how this is checked without a new dependency.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  obj <- make_offset_fit()
  d <- obj$data
  rownames(d) <- NULL
  m <- lm(y ~ x + z, data = d, offset = o)
  shuffled <- d[order(d$z), ]
  rownames(shuffled) <- NULL

  # Since 0.15.0 performKoenkerTest() refuses such data itself, before the
  # bootstrap refits, by the guard of 0.15.0, the alignment of the model
  # context since 0.16.0 (rcheck_rows_at()), so the message is the test's. A
  # test function of the caller's own meets the bootstrap's check, below.
  refused <- "cannot match the rows of `data` to the observations of the model"
  for (strategy in c("null", "pairs")) {
    expect_error(
      bootstrapTestStatistic(performKoenkerTest, m, shuffled, B = 5,
                             resample = strategy, progress = FALSE),
      refused,
      class = "heteroTests_data_rows_error"
    )
    out <- bootstrapTestStatistic(performKoenkerTest, m, d, B = 5,
                                  resample = strategy, progress = FALSE)
    expect_true(all(is.finite(out$replicates)))
  }

  # The reordered data.table is built from the reordered data frame, so that
  # its rows are numbered afresh whether or not `[` dispatches to
  # data.table's own method here.
  skip_if_not_installed("data.table")
  dt <- data.table::as.data.table(d)
  m_dt <- lm(y ~ x + z, data = dt, offset = o)
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, m_dt, data.table::as.data.table(shuffled),
                           B = 5, progress = FALSE),
    refused,
    class = "heteroTests_data_rows_error"
  )
  set.seed(91)
  in_order <- bootstrapTestStatistic(performKoenkerTest, m_dt, dt, B = 5, progress = FALSE)
  set.seed(91)
  as_frame <- bootstrapTestStatistic(performKoenkerTest, m, d, B = 5, progress = FALSE)
  expect_equal(in_order$replicates, as_frame$replicates, tolerance = 1e-8)
})

test_that("data without row names of its own is resampled when the fit dropped rows", {
  # A fit to data with automatic row names that drops two rows for missing
  # values. The full data are matched by name and the dropped rows left out;
  # the complete cases, renumbered 1 to n as a tibble numbers any subset, have
  # as many rows as the fit and are taken in its order. Both resample as the
  # rows the fit kept.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(23)
  n <- 80
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + rnorm(n)
  d$x[c(3, 7)] <- NA
  rownames(d) <- NULL
  m <- lm(y ~ x + z, data = d)
  kept <- d[rownames(stats::model.frame(m)), ]
  complete <- kept
  rownames(complete) <- NULL
  run <- function(data, strategy) {
    set.seed(97)
    bootstrapTestStatistic(performKoenkerTest, m, data, B = 5, resample = strategy,
                           progress = FALSE)$replicates
  }
  tables <- list(full = d, complete = complete)
  if (requireNamespace("data.table", quietly = TRUE)) {
    tables$full_data_table <- data.table::as.data.table(d)
    tables$complete_data_table <- data.table::as.data.table(complete)
  }
  for (strategy in c("null", "pairs")) {
    expected <- run(kept, strategy)
    expect_true(all(is.finite(expected)))
    for (nm in names(tables)) {
      expect_equal(run(tables[[nm]], strategy), expected, tolerance = 1e-12,
                   label = paste(nm, strategy))
    }
  }
})

test_that("a term computed from all the data does not make the rows of the fit differ", {
  # I(x - mean(x)) takes the mean over the rows it is evaluated on. The fit
  # took it over all the rows, before dropping the one with a missing
  # response or those outside the subset; rebuilt from the rows of the fit
  # it is shifted by a constant, which changes no residual.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  set.seed(24)
  n <- 80
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- 1 + 2 * d$x + 0.5 * d$z + rnorm(n)
  with_missing <- d
  with_missing$y[5] <- NA
  fits <- list(
    missing = list(lm(y ~ I(x - mean(x)) + z, data = with_missing), with_missing),
    subset = list(lm(y ~ I(x - mean(x)) + z, data = d, subset = z > 2), d)
  )
  for (nm in names(fits)) {
    m <- fits[[nm]][[1]]
    full <- fits[[nm]][[2]]
    kept <- full[rownames(stats::model.frame(m)), ]
    for (strategy in c("null", "pairs")) {
      set.seed(98)
      from_full <- bootstrapTestStatistic(performKoenkerTest, m, full, B = 5,
                                          resample = strategy, progress = FALSE)
      set.seed(98)
      from_kept <- bootstrapTestStatistic(performKoenkerTest, m, kept, B = 5,
                                          resample = strategy, progress = FALSE)
      expect_true(all(is.finite(from_kept$replicates)), label = paste(nm, strategy))
      expect_equal(from_full$replicates, from_kept$replicates, tolerance = 1e-12,
                   label = paste(nm, strategy))
    }
  }
})

test_that("an offset in the formula outside the data is read from data, and follows the rows drawn", {
  # offset(shift), with shift in the environment of the formula. Until the
  # eighth step of the model context each replicate refitted the formula on the
  # rows of `data`, which evaluated shift again in the order of the fit: null
  # resampling of the data in the order of the fit gave the replicates of the
  # model with the offset as a column, and pairs resampling, and data in
  # another order, were refused ("does not come from `data`", "does not
  # reproduce the fit"). The package's tests refuse a variable of the formula
  # that is not a column of the data, so only a test function of the caller's
  # own reached this; here it is Koenker's statistic computed from the model
  # alone. bootstrapTestStatistic() now matches `data` as the tests do: data
  # that lack shift are refused, and given shift as a column, each replicate
  # is refitted on the rows of the stored model frame and design it draws,
  # which hold the offset of each row, so its replicates are those of the same
  # model with the offset as a column, in every order and under pairs
  # resampling.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  koenker_of_fit <- function(model, data) {
    e2 <- residuals(model)^2
    X <- model.matrix(model)[, -1]
    list(statistic = length(e2) * summary(lm(e2 ~ X))$r.squared)
  }
  obj <- make_offset_fit()
  d <- obj$data
  shift <- d$o
  m <- lm(y ~ x + z + offset(shift), data = d)
  expect_error(
    bootstrapTestStatistic(koenker_of_fit, m, d, B = 5, progress = FALSE),
    "The following variables are missing from input data: shift.", fixed = TRUE
  )

  column_model <- lm(y ~ x + z + offset(o), data = d)
  with_shift <- d
  with_shift$shift <- shift
  reordered <- with_shift[order(with_shift$z), ]
  mirror <- c(seq_len(nrow(d) / 2), rev(seq_len(nrow(d) / 2)))
  d_mirror <- d
  d_mirror$mirror <- mirror
  m_mirror <- lm(y ~ x + z + offset(mirror), data = d)
  cases <- list(
    null = list(m, with_shift, column_model, d, "null"),
    pairs = list(m, with_shift, column_model, d, "pairs"),
    reordered = list(m, reordered, column_model, reordered, "null"),
    reordered_pairs = list(m, reordered, column_model, reordered, "pairs"),
    mirror = list(m_mirror, d_mirror, lm(y ~ x + z + offset(mirror), data = d_mirror), d_mirror,
                  "pairs")
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    set.seed(91)
    workspace <- bootstrapTestStatistic(koenker_of_fit, case[[1]], case[[2]], B = 5,
                                        resample = case[[5]], progress = FALSE)
    set.seed(91)
    columns <- bootstrapTestStatistic(koenker_of_fit, case[[3]], case[[4]], B = 5,
                                      resample = case[[5]], progress = FALSE)
    expect_true(all(is.finite(workspace$replicates)), label = nm)
    expect_equal(workspace$replicates, columns$replicates, tolerance = 1e-10, label = nm)
  }

  # Given as an argument, the same offset is taken from the fit and follows
  # the rows.
  m_argument <- lm(y ~ x + z, data = d, offset = shift)
  set.seed(93)
  pairs <- bootstrapTestStatistic(koenker_of_fit, m_argument, d, B = 5,
                                  resample = "pairs", progress = FALSE)
  set.seed(93)
  expected <- vapply(seq_len(5), function(b) {
    db <- d[sample.int(nrow(d), replace = TRUE), , drop = FALSE]
    squared_residual_r2(residuals(lm(y ~ x + z, data = db, offset = o)), ~ x + z, db)
  }, numeric(1))
  expect_equal(pairs$replicates, expected, tolerance = 1e-8)
})

# --- the design of the wild bootstrap ------------------------------------------

test_that("the wild bootstrap refits an unweighted fit with transformed terms", {
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  # The model frame holds log(x) and log(y), not x and y, so evaluating the
  # call of the fit again in it failed with "object 'x' not found".
  set.seed(10)
  n <- 80
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- exp(1 + 0.5 * log(d$x) + 0.1 * d$z + 0.2 * rnorm(n))
  m <- lm(log(y) ~ log(x) + z, data = d)
  aux <- data.frame(lx = log(d$x), z = d$z)

  pool <- residuals(m) / sqrt(1 - hatvalues(m))
  pool <- pool - mean(pool)
  for (distribution in c("rademacher", "mammen")) {
    set.seed(81)
    wild <- performWildBootstrapTest(m, d, B = 5, distribution = distribution,
                                     progress = FALSE)
    set.seed(81)
    expected <- vapply(seq_len(5), function(b) {
      v <- if (distribution == "rademacher") {
        sample(c(-1, 1), n, replace = TRUE)
      } else {
        r5 <- sqrt(5)
        sample(c((1 - r5) / 2, (1 + r5) / 2), n, replace = TRUE,
               prob = c((r5 + 1) / (2 * r5), (r5 - 1) / (2 * r5)))
      }
      db <- d
      db$ly <- fitted(m) + sample(pool, n, replace = TRUE) * v
      refit <- lm(ly ~ log(x) + z, data = db)
      squared_residual_r2(residuals(refit), ~ lx + z, aux)
    }, numeric(1))
    expect_equal(unname(wild$statistic), squared_residual_r2(residuals(m), ~ lx + z, aux),
                 tolerance = 1e-8, label = distribution)
    expect_equal(wild$bootstrap$replicates, expected, tolerance = 1e-8, label = distribution)
  }
})

# --- the White bootstrap on a transformed response -----------------------------

make_log_response <- function(seed = 12, n = 80) {
  set.seed(seed)
  d <- data.frame(x = runif(n, 1, 5), z = runif(n, 1, 5))
  d$y <- exp(1 + 0.3 * d$x + 0.1 * d$z + 0.3 * rnorm(n))
  d$ly <- log(d$y)
  d$w <- 1 / d$x
  d
}

test_that("the White bootstrap regenerates a transformed response", {
  # The response of log(y) ~ x + z is log(y), which is not a column of the
  # data. The regenerated values were written into a column named "log(y)"
  # that the refit did not read: it took log(y) from y again, every replicate
  # was the data of the fit, and the p-value was 1.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  d <- make_log_response()
  m <- lm(log(y) ~ x + z, data = d)

  set.seed(101)
  ours <- suppressWarnings(performWhiteTestBootstrap(m, d, B = 5))
  set.seed(101)
  expected <- vapply(seq_len(5), function(b) {
    db <- d
    db$ly <- fitted(m) + sample(residuals(m), replace = TRUE)
    squared_residual_r2(residuals(lm(ly ~ x + z, data = db)), white_terms, d)
  }, numeric(1))
  observed <- squared_residual_r2(residuals(m), white_terms, d)

  expect_equal(unname(ours$statistic), observed, tolerance = 1e-8)
  expect_equal(ours$boot_statistics, expected, tolerance = 1e-8)
  expect_false(any(abs(ours$boot_statistics - observed) < 1e-8))
})

test_that("a transformed response resamples as the same response held in a column", {
  # Weighted or not, log(y) ~ x + z is the model ly ~ x + z with ly = log(y),
  # and with the same draws the two give the same replicates.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  d <- make_log_response()
  pairs <- list(
    unweighted = list(lm(log(y) ~ x + z, data = d), lm(ly ~ x + z, data = d)),
    weighted = list(lm(log(y) ~ x + z, data = d, weights = w),
                    lm(ly ~ x + z, data = d, weights = w))
  )
  for (nm in names(pairs)) {
    set.seed(102)
    transformed <- suppressWarnings(performWhiteTestBootstrap(pairs[[nm]][[1]], d, B = 19))
    set.seed(102)
    column <- suppressWarnings(performWhiteTestBootstrap(pairs[[nm]][[2]], d, B = 19))
    expect_equal(transformed$boot_statistics, column$boot_statistics, tolerance = 1e-8,
                 label = nm)
    expect_equal(transformed$p.value, column$p.value, label = nm)
  }
})

test_that("the column of a regenerated response does not overwrite a regressor", {
  # The regenerated response goes into a column named .ht_bootstrap_response,
  # or into one with more dots in front while that name is taken.
  previous_level <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(previous_level), add = TRUE)
  d <- make_log_response()
  d$.ht_bootstrap_response <- runif(nrow(d), 1, 5)
  transformed <- lm(log(y) ~ x + .ht_bootstrap_response, data = d)
  column <- lm(ly ~ x + .ht_bootstrap_response, data = d)

  set.seed(103)
  ours <- suppressWarnings(performWhiteTestBootstrap(transformed, d, B = 19))
  set.seed(103)
  expected <- suppressWarnings(performWhiteTestBootstrap(column, d, B = 19))
  expect_equal(ours$boot_statistics, expected$boot_statistics, tolerance = 1e-8)
  expect_equal(ours$p.value, expected$p.value)
})
