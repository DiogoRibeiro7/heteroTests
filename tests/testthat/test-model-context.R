# The model context (R/model_context.R): the representation, its alignment
# step, and the test matrix of docs/design/model-context.md, run through the
# functions built on it (`context_migrated`, helper-model-context.R). The
# expectations are relations between calls, not stored values: a test gives
# on every form of `data` it accepts the value it gives on the data frame of
# the fit, on a fit with an offset the value of the model fitted to the
# response minus the offset (RESET: the nested F test of the model with its
# offset), on a term the value of the same column computed beforehand, and on
# a fit that dropped rows the value of the fit to the complete rows.

context_same <- function(object, expected, info, tolerance = NULL) {
  expect_s3_class(object, "htest")
  expect_s3_class(expected, "htest")
  if (!inherits(object, "htest") || !inherits(expected, "htest")) {
    return(invisible(NULL))
  }
  if (is.null(tolerance)) {
    expect_identical(unname(object$statistic), unname(expected$statistic), info = info)
    expect_identical(object$p.value, expected$p.value, info = info)
  } else {
    expect_equal(unname(object$statistic), unname(expected$statistic),
                 tolerance = tolerance, info = info)
    expect_equal(object$p.value, expected$p.value, tolerance = tolerance, info = info)
  }
  expect_identical(unname(object$parameter), unname(expected$parameter), info = info)
}

context_installed <- function(cell) {
  all(vapply(cell$needs, requireNamespace, logical(1), quietly = TRUE))
}

# Runs `fns` on `ids` and checks each cell: its value against its reference,
# `identical()`, or its refusal, as the test sees the cell
# (context_cell_for()).
context_check_matrix <- function(ids, fns = context_runnable()) {
  cells <- context_cells()
  for (fn in fns) {
    results <- new.env(parent = emptyenv())
    result_of <- function(id) {
      if (is.null(results[[id]])) {
        results[[id]] <- list(context_run(fn, cells[[id]]))
      }
      results[[id]][[1L]]
    }
    for (id in ids) {
      cell <- context_cell_for(fn, cells[[id]])
      if (!context_installed(cell)) next
      out <- result_of(id)
      info <- paste(fn, id)
      if (identical(cell$expect, "value")) {
        expect_s3_class(out, "htest")
        if (!is.null(cell$reference) && context_installed(cells[[cell$reference]])) {
          tolerance <- if (id %in% context_rounded_cells) {
            if (identical(fn, "performFlignerKilleenTest")) context_fligner_tolerance else 1e-10
          }
          context_same(out, result_of(cell$reference), info, tolerance)
        }
      } else if (identical(cell$expect, "guard")) {
        expect_s3_class(out, "heteroTests_data_rows_error")
        expect_match(conditionMessage(out), "cannot match the rows of `data`", fixed = TRUE,
                     info = info)
      } else {
        expect_s3_class(out, "error")
        expect_match(conditionMessage(out), cell$expect, fixed = TRUE, info = info)
      }
    }
  }
}

test_that("the migrated tests give the value of the fit on every form of data they accept", {
  skip_on_cran()
  context_check_matrix(names(context_cells()))
})

test_that("a subset of the matrix runs on CRAN", {
  context_check_matrix(context_cran_cells)
})

test_that("the context holds the parts of the fit, one per observation", {
  skip_on_cran()
  cells <- context_cells()
  ids <- names(cells)[vapply(cells, function(cell) {
    cell$block %in% c("fit", "terms") && identical(cell$form, "data_frame") ||
      cell$id %in% c("missing/full", "subset/data_frame", "class/rank_deficient", "class/rlm",
                     "ident/repeated_fit")
  }, logical(1))]
  for (id in ids) {
    built <- cells[[id]]$build()
    m <- built$m
    ctx <- rmodel_context(m, built$data, fn = "performKoenkerTest")
    n <- length(m$residuals)
    expect_s3_class(ctx, "ht_model_context")
    expect_identical(ctx$obs_id, names(m$residuals), info = id)
    expect_identical(ctx$residuals, m$residuals, info = id)
    expect_identical(ctx$fitted, m$fitted.values, info = id)
    expect_identical(ctx$design, stats::model.matrix(m), info = id)
    expect_identical(ctx$response, stats::model.response(stats::model.frame(m), "numeric"),
                     info = id)
    expect_identical(ctx$weights, rleast_squares_weights(m), info = id)
    offset <- stats::model.offset(stats::model.frame(m))
    expect_identical(ctx$offset, if (!is.null(offset)) as.vector(offset), info = id)
    expect_identical(ctx$rank, m$rank, info = id)
    expect_identical(ctx$aliased, raliased_coefficients(m), info = id)
    expect_length(ctx$rows, n)
    kind <- if (inherits(m, "glm")) {
      "glm_gaussian"
    } else if (!is.null(attr(m, "variance_function"))) {
      "fitwls"
    } else if (inherits(m, "rlm")) {
      "rlm"
    } else {
      "lm"
    }
    expect_identical(ctx$kind, kind, info = id)
    expect_identical(rvalidate_model_context(ctx), ctx, info = id)
  }
})

test_that("the tolerance of the rank is the fit's", {
  d <- context_data()
  expect_identical(rmodel_context(lm(y ~ x1 + x2, data = d), fn = "f")$qr_tol, 1e-7)
  # glm.fit() decides it with min(1e-7, epsilon / 1000).
  expect_identical(rmodel_context(glm(y ~ x1 + x2, data = d), fn = "f")$qr_tol,
                   min(1e-7, glm.control()$epsilon / 1000))
  expect_equal(rmodel_context(glm(y ~ x1 + x2, data = d), fn = "f")$qr_tol, 1e-11)
  expect_identical(
    rmodel_context(lm(y ~ x1 + x2, data = d, tol = 1e-10), fn = "f")$qr_tol, 1e-10
  )
})

test_that("the residuals the tests read are the unpadded Pearson residuals", {
  d <- context_data()
  fits <- list(
    ols = lm(y ~ x1 + x2, data = d),
    wls = lm(y ~ x1 + x2, data = d, weights = w),
    glm = glm(y ~ x1 + x2, data = d, weights = w),
    fitwls = fitWLS(lm(y ~ x1 + x2, data = d))
  )
  for (nm in names(fits)) {
    ctx <- rmodel_context(fits[[nm]], fn = "f")
    expect_identical(rcontext_pearson(ctx), rpearson_residuals(fits[[nm]]), info = nm)
  }
  d_na <- d
  d_na$x2[c(3, 7)] <- NA
  excluded <- lm(y ~ x1 + x2, data = d_na, weights = w, na.action = na.exclude)
  ctx <- rmodel_context(excluded, d_na, fn = "f")
  padded <- rpearson_residuals(excluded)
  expect_length(padded, nrow(d_na))
  expect_identical(rcontext_pearson(ctx), padded[-c(3, 7)])
  expect_identical(names(rcontext_pearson(ctx)), ctx$obs_id)
  # A zero weight is refused where rpearson_residuals() refuses it.
  d$w0 <- d$w
  d$w0[1] <- 0
  zero <- rmodel_context(lm(y ~ x1 + x2, data = d, weights = w0), d, fn = "f")
  expect_error(rcontext_pearson(zero), "zero or non-finite weights")
  expect_error(performKoenkerTest(lm(y ~ x1 + x2, data = d, weights = w0), d),
               "zero or non-finite weights")
})

test_that("each form of data is aligned as the design says", {
  skip_if_not_installed("dplyr")
  d <- context_data()
  m <- lm(y ~ x1 + x2, data = d)
  alignment <- function(model, data) rmodel_context(model, data, fn = "f")$alignment
  expect_identical(alignment(m, NULL), "model_frame")
  expect_identical(alignment(m, d), "row_numbers")
  expect_identical(alignment(m, d[sample(nrow(d)), ]), "row_names")
  expect_identical(alignment(m, dplyr::as_tibble(d)), "row_numbers")
  expect_identical(alignment(m, rbind(d, d[1:5, ])), "row_numbers")
  expect_identical(alignment(m, rbind(d[sample(nrow(d)), ], d[1:5, ])), "row_names")
  # The rows read are those of the fit, in its order.
  shuffled <- d[sample(nrow(d)), ]
  ctx <- rmodel_context(m, shuffled, fn = "f")
  expect_identical(rownames(shuffled)[ctx$rows], ctx$obs_id)
  expect_identical(rcontext_rows(ctx, shuffled)$y, d$y)
  expect_identical(rownames(rcontext_rows(ctx, shuffled)), ctx$obs_id)
  # Numbered data with one row per observation, whose numbers are not the
  # fit's, are taken in the order of the fit once they reproduce it
  # (decision 18).
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  m_na <- lm(y ~ x1 + x2, data = d_na)
  renumbered <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  rownames(renumbered) <- NULL
  expect_identical(alignment(m_na, renumbered), "in_order")
  expect_identical(alignment(m_na, d_na), "row_numbers")
  expect_identical(alignment(m_na, dplyr::as_tibble(d_na)), "row_numbers")
  named <- d
  rownames(named) <- paste0("obs", seq_len(nrow(d)))
  expect_identical(alignment(lm(y ~ x1 + x2, data = named), dplyr::as_tibble(named)), "in_order")
  # The same rows in another order, numbered again, are refused by the
  # comparison of the guard; with the numbers they had, they lack rows of the
  # fit.
  reversed <- renumbered[rev(seq_len(nrow(renumbered))), ]
  expect_error(rmodel_context(m_na, reversed, fn = "f"), "Missing rows: 118, 119, 120",
               fixed = TRUE)
  rownames(reversed) <- NULL
  expect_error(rmodel_context(m_na, reversed, fn = "f"), class = "heteroTests_data_rows_error")
  expect_error(rmodel_context(m_na, reversed, fn = "f"), "`data` has no row names of its own",
               fixed = TRUE)
  # The guard of the tests that are not migrated yet keeps its rule: it does
  # not take numbered rows in order, and leaves them to the test.
  expect_null(rcheck_data_rows(m_na, renumbered))
  fit_ids <- names(m_na$residuals)
  expect_length(fit_ids, 117L)
  expect_identical(rcheck_data_rows(m_na, d_na), match(fit_ids, rownames(d_na)))
  # Data that lack a row of the fit keep their message.
  expect_error(
    rmodel_context(m, d[1:100, ], fn = "f", test_label = "Koenker"),
    "Koenker requires `data` to contain the rows used to fit the model. Missing rows: 101",
    fixed = TRUE
  )
  expect_error(rmodel_context(m, d[1:100, -1], fn = "f"),
               "The following variables are missing from input data: x1", fixed = TRUE)
  expect_error(rmodel_context(m, "not data", fn = "f"),
               "Expected a data.frame for input data", fixed = TRUE)
})

test_that("the variables a test names are read at the confirmed rows", {
  d <- context_data()
  m <- lm(y ~ x1 + x2, data = d)
  shuffled <- d[sample(nrow(d)), ]
  ctx <- rmodel_context(m, shuffled, fn = "f", need = c(group = "group", order_by = "x1"))
  expect_identical(ctx$variables$group, d$group)
  expect_identical(ctx$variables$order_by, d$x1)
  # Without data, from the model frame, but only a variable of the formula
  # that the frame holds under its own name.
  expect_identical(rmodel_context(m, fn = "f", need = "x2")$variables$x2, d$x2)
  ml <- lm(log(ypos) ~ log(x1) + x2, data = d)
  expect_error(
    rmodel_context(ml, fn = "f", need = c(variable = "x1")),
    paste(
      "The following variables are missing from the model frame of `model`: x1.",
      "It holds the terms of the model (log(ypos), log(x1), x2)"
    ),
    fixed = TRUE
  )
  expect_error(rmodel_context(m, d, fn = "f", need = "nonexistent"),
               "The following variables are missing from input data: nonexistent", fixed = TRUE)
  # The rows the fit dropped are not read.
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  ctx <- rmodel_context(lm(y ~ x1 + x2, data = d_na), d_na, fn = "f", need = "group")
  expect_identical(ctx$variables$group, d_na$group[-c(3, 7, 11)])
})

test_that("a fit changed after it was fitted is refused", {
  d <- context_data()
  m <- lm(y ~ x1 + x2, data = d)
  coefficient <- m
  coefficient$coefficients[["x1"]] <- coefficient$coefficients[["x1"]] + 0.5
  residual <- m
  residual$residuals[1:5] <- residual$residuals[1:5] + 1
  weighted <- lm(y ~ x1 + x2, data = d, weights = w)
  reweighted <- weighted
  reweighted$weights <- rev(reweighted$weights)
  reproduce <- "does not reproduce its residuals from its design and coefficients"
  for (fn in context_runnable()) {
    recipe <- contract_recipes[[fn]]
    expect_error(suppressWarnings(recipe(coefficient, d)), reproduce, fixed = TRUE, info = fn)
    expect_error(suppressWarnings(recipe(residual, d)), reproduce, fixed = TRUE, info = fn)
    # The quantile regression test refuses a weighted fit first.
    if (identical(fn, "performQuantileRegressionTest")) next
    expect_error(suppressWarnings(recipe(reweighted, d)),
                 "The weights of the model are not those its coefficients were fitted with",
                 fixed = TRUE, info = fn)
  }
  # The checks are relative to the size of their terms: an ill-conditioned
  # fit, or a response far from zero, passes.
  scaled <- function(k) {
    dk <- d
    dk$y <- dk$y * k
    rmodel_context(lm(y ~ x1 + x2, data = dk), fn = "f")
  }
  for (k in c(1e-200, 1e-8, 1e8, 1e200)) {
    expect_silent(rvalidate_model_context(scaled(k)))
  }
  far <- d
  far$y <- 1e15 + far$y
  expect_silent(rvalidate_model_context(rmodel_context(lm(y ~ x1 + x2, data = far), fn = "f")))
  set.seed(5)
  near <- d
  near$x3 <- near$x1 + 1e-9 * rnorm(nrow(near))
  expect_silent(rvalidate_model_context(rmodel_context(glm(y ~ x1 + x2 + x3, data = near),
                                                       fn = "f")))
  expect_silent(rvalidate_model_context(rmodel_context(lm(y ~ x1 + x2 + x3, data = near),
                                                       fn = "f")))
  # The M-estimate of MASS::rlm() is not a least-squares fit, and is not checked.
  expect_silent(rvalidate_model_context(rmodel_context(MASS::rlm(y ~ x1 + x2, data = d),
                                                       fn = "f")))
})

test_that("the orthogonality of the residuals allows the rounding of a large response", {
  # A Gaussian glm computes its residuals as y - mu, which shifts them all by
  # about eps |mean(y)|: with a mean of 1e11 or 1e12 and a residual standard
  # deviation of 2, the scores of a valid fit exceeded 1e-6 of the norms, and
  # the fit was refused. The bound allows eps n ||X_j|| ||y|| as well.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(1)
  n <- 120
  base <- data.frame(x1 = runif(n, 0.5, 2), x2 = rnorm(n))
  noise <- rnorm(n, sd = 2)
  at <- function(mean) {
    d <- base
    d$y <- mean + 1 + d$x1 - d$x2 + noise
    d
  }
  for (mean in c(1e11, 1e12)) {
    d <- at(mean)
    g <- glm(y ~ x1 + x2, data = d)
    expect_s3_class(performKoenkerTest(g, d), "htest")
    expect_equal(unname(performKoenkerTest(g, d)$statistic),
                 unname(performKoenkerTest(lm(y ~ x1 + x2, data = d), d)$statistic),
                 tolerance = 1e-3, info = mean)
  }
  for (mean in c(0, 1e6, 1e10, 1e12, 1e14)) {
    d <- at(mean)
    expect_silent(rvalidate_model_context(rmodel_context(lm(y ~ x1 + x2, data = d), fn = "f")))
    expect_s3_class(performKoenkerTest(lm(y ~ x1 + x2, data = d), d), "htest")
  }
  # An edited coefficient is seen by the first check, relative to the size of
  # the largest term, while the edit is above 1e-7 of the response.
  edited <- function(m) {
    m$coefficients[["x1"]] <- m$coefficients[["x1"]] + 0.5
    m
  }
  for (mean in c(0, 1e3, 1e5)) {
    d <- at(mean)
    for (m in list(lm(y ~ x1 + x2, data = d), glm(y ~ x1 + x2, data = d))) {
      expect_error(performKoenkerTest(edited(m), d), class = "heteroTests_model_context_error")
    }
  }
  # Weights changed after the fit are seen by the orthogonality, also for a
  # large response.
  for (mean in c(0, 1e6, 1e11)) {
    d <- at(mean)
    d$w <- 1 / d$x1^2
    m <- lm(y ~ x1 + x2, data = d, weights = w)
    m$weights <- rev(m$weights)
    expect_error(performKoenkerTest(m, d), "not those its coefficients were fitted with",
                 class = "heteroTests_model_context_error")
  }
})

test_that("the refusals of the context are final in the runners", {
  d <- context_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(m, data) {
    suppressWarnings(runHeteroTests(m, data, use_cache = FALSE, progress = FALSE))
  }
  # A fit changed after it was fitted: the runner put the Cook-Weisberg test
  # under the names of White's and Koenker's tests.
  edited <- lm(y ~ x1 + x2, data = d)
  edited$coefficients[["x1"]] <- edited$coefficients[["x1"]] + 0.5
  expect_error(run(edited, d), "does not reproduce its residuals",
               class = "heteroTests_model_context_error")
  expect_error(run(edited, NULL), class = "heteroTests_model_context_error")
  expect_error(suppressWarnings(runDiagnostics(edited, d, use_cache = FALSE, progress = FALSE)),
               class = "heteroTests_model_context_error")
  # A multivariate lm.
  expect_error(run(lm(cbind(y, ypos) ~ x1 + x2, data = d), d), "needs a model with one response",
               class = "heteroTests_model_context_error")
  # A model = FALSE fit whose data are gone and that `data` does not reproduce.
  env <- new.env()
  assign("gone", d, envir = env)
  m_false <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  reversed <- d
  reversed$y <- rev(reversed$y)
  expect_error(run(m_false, reversed), "cannot use the model frame of `model`",
               class = "heteroTests_model_context_error")
  expect_error(run(m_false, NULL), class = "heteroTests_model_context_error")
  # A Gaussian gam runs the tests the runner names.
  skip_if_not_installed("mgcv")
  out <- run(mgcv::gam(y ~ x1 + x2, data = d), d)
  expect_identical(out$white$method, "White's test for heteroscedasticity")
  expect_identical(out$koenker$method, "Koenker studentized Breusch-Pagan test")
})

test_that("a fit with model = FALSE whose data are gone is tested on data that reproduce it", {
  d <- context_data()
  env <- new.env()
  assign("gone", d, envir = env)
  m <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  # RESET reads nothing from `data` and builds its context without it, so it
  # cannot rebuild the frame from `data`, and refuses such a fit (below).
  fns <- setdiff(context_runnable(), "performRESETTest")
  expected <- lapply(fns, function(fn) {
    context_run(fn, list(build = function() list(m = m, data = d)))
  })
  rm("gone", envir = env)
  expect_error(stats::model.frame(m))
  refused <- context_run("performRESETTest", list(build = function() list(m = m, data = d)))
  expect_s3_class(refused, "heteroTests_model_context_error")
  expect_match(conditionMessage(refused),
               "performRESETTest() cannot use the model frame of `model`", fixed = TRUE)
  for (k in seq_along(fns)) {
    fn <- fns[k]
    got <- context_run(fn, list(build = function() list(m = m, data = d)))
    context_same(got, expected[[k]], fn)
    shuffled <- context_run(fn, list(build = function() list(m = m, data = d[sample(nrow(d)), ])))
    context_same(shuffled, expected[[k]], paste(fn, "shuffled"))
    # Data that do not give the response of the fit, which were used unchecked.
    reversed <- d
    reversed$y <- rev(reversed$y)
    refused <- context_run(fn, list(build = function() list(m = m, data = reversed)))
    expect_s3_class(refused, "heteroTests_model_context_error")
    expect_match(conditionMessage(refused), "`data` does not give the response and the fitted",
                 fixed = TRUE, info = fn)
    expect_s3_class(context_run(fn, list(build = function() list(m = m, data = NULL))),
                    "heteroTests_model_context_error")
  }
})

test_that("a Gaussian gam is tested as before, and without smooths it is the lm", {
  skip_if_not_installed("mgcv")
  d <- context_data()
  plain <- mgcv::gam(y ~ x1 + x2, data = d)
  smooth <- mgcv::gam(y ~ s(x1) + x2, data = d)
  for (fn in context_runnable()) {
    # Fligner-Killeen's ranks tie at a group median on one fit and not, by
    # rounding, on the other (N15, context_fligner_tolerance).
    fligner <- identical(fn, "performFlignerKilleenTest")
    tolerance <- if (fligner) context_fligner_tolerance else 1e-10
    for (data in list(d, NULL)) {
      # Hartley's test and the group tests group by a variable of `data` that
      # the model frame does not hold: without `data` they are refused (M1), as
      # the matrix checks.
      if (is.null(data) && fn %in% context_group_readers()) next
      context_same(context_run(fn, list(build = function() list(m = plain, data = data))),
                   context_run(fn, list(build = function() list(m = lm(y ~ x1 + x2, d), data = d))),
                   paste(fn, "gam"), tolerance = tolerance)
      out <- context_run(fn, list(build = function() list(m = smooth, data = data)))
      if (fn %in% context_increment2) {
        # A smooth term is usually penalised, and a penalised fit is not the
        # least-squares fit of its design: the tests of increment 2 refuse it,
        # until increment 11 settles which tests take it.
        expect_s3_class(out, "heteroTests_model_context_error")
        refusal <- "%s() cannot test an mgcv::gam() fit with smooth terms (s(x1))"
        expect_match(conditionMessage(out), sprintf(refusal, fn), fixed = TRUE, info = fn)
      } else {
        # Those of increments 1 and 3 test it as before 0.16.0.
        expect_s3_class(out, "htest")
      }
    }
  }
  expect_identical(rmodel_context(plain, fn = "f")$kind, "gam")
  expect_identical(rmodel_context(plain, fn = "f")$obs_id, rownames(stats::model.frame(plain)))
})

test_that("data in the order of the fit must tell apart the rows a test reads beyond the frame", {
  # Rows that tie on every compared column (y and x here) can be exchanged in
  # data taken in the order of the fit without a trace in the model frame; a
  # column beyond it, such as the variable the rank permutation test orders
  # by, then belongs to other rows. Exchanged within such ties, z gave -0.02017
  # where the data in the order of the fit give -0.02619, without an error.
  set.seed(42)
  n <- 60
  named <- data.frame(x = rep(1:3, 20))
  named$y <- round(named$x + rnorm(n))
  named$z <- rnorm(n)
  rownames(named) <- paste0("obs", seq_len(n))
  m <- lm(y ~ x, data = named)
  numbered <- named
  rownames(numbered) <- NULL
  exchanged <- numbered
  groups <- split(seq_len(n), paste(numbered$y, numbered$x))
  for (g in groups) {
    if (length(g) > 1L) exchanged$z[g] <- exchanged$z[rev(g)]
  }
  expect_false(identical(exchanged$z, numbered$z))
  run <- function(data, order_by) {
    set.seed(1)
    performRankPermutationTest(m, data, order_by = order_by, B = 19, progress = FALSE)
  }
  expected <- run(named, "z")
  for (data in list(numbered, exchanged)) {
    expect_error(run(data, "z"), "tie on every column of the model frame that is compared",
                 class = "heteroTests_data_rows_error")
  }
  # A column of the model frame, or the default, is read from rows that the
  # comparison confirmed, ties or not.
  expect_identical(run(numbered, "x")$statistic, run(named, "x")$statistic)
  expect_identical(run(numbered, NULL)$statistic, run(named, NULL)$statistic)
  expect_identical(run(named, "z")$statistic, expected$statistic)
})

test_that("the tests of increments 3 and 4 read their variable at rows told apart", {
  # The case of the review of increment 1, for each test that reads a column
  # of `data` besides the model: the rows of a fit to named data, in its
  # order, numbered again, tie on every compared column (y and x), and data
  # taken in the order of the fit could exchange the rows within those ties
  # without a trace in the model frame; a column beyond it, the variable a
  # test regresses on, orders by or groups by, would then belong to other
  # rows. Such data are refused, and so is the same data with the column
  # exchanged within the ties. Increment 4 adds the five group tests.
  set.seed(42)
  n <- 60
  named <- data.frame(x = rep(1:3, 20))
  named$y <- round(named$x + rnorm(n))
  named$z <- runif(n, 1, 2)
  named$g <- factor(rep(c("a", "b", "c", "d"), 15))
  rownames(named) <- paste0("obs", seq_len(n))
  m <- lm(y ~ x, data = named)
  numbered <- named
  rownames(numbered) <- NULL
  exchanged <- numbered
  for (tied in split(seq_len(n), paste(numbered$y, numbered$x))) {
    if (length(tied) > 1L) {
      exchanged$z[tied] <- exchanged$z[rev(tied)]
      exchanged$g[tied] <- exchanged$g[rev(tied)]
    }
  }
  expect_false(identical(exchanged$z, numbered$z))
  expect_false(identical(exchanged$g, numbered$g))
  refused <- list(numbered, exchanged)
  if (requireNamespace("dplyr", quietly = TRUE)) {
    refused <- c(refused, list(dplyr::as_tibble(numbered), dplyr::as_tibble(exchanged)))
  }
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  calls <- list(
    performGlejserTest = function(model, data, v) performGlejserTest(model, data, v),
    performParkTest = function(model, data, v) performParkTest(model, data, v),
    performSzroeterTest = function(model, data, v) performSzroeterTest(model, data, order_by = v),
    performHarrisonMcCabeTest = function(model, data, v) {
      performHarrisonMcCabeTest(model, data, order_by = v)
    },
    performHartleyFmaxTest = function(model, data, v) performHartleyFmaxTest(model, data, v),
    performLeveneTest = function(model, data, v) performLeveneTest(model, data, v),
    performBrownForsytheTest = function(model, data, v) performBrownForsytheTest(model, data, v),
    performFlignerKilleenTest = function(model, data, v) performFlignerKilleenTest(model, data, v),
    performBartlettTest = function(model, data, v) performBartlettTest(model, data, v),
    performOBrienTest = function(model, data, v) performOBrienTest(model, data, v)
  )
  beyond <- c(performGlejserTest = "z", performParkTest = "z", performSzroeterTest = "z",
              performHarrisonMcCabeTest = "z", performHartleyFmaxTest = "g",
              performLeveneTest = "g", performBrownForsytheTest = "g",
              performFlignerKilleenTest = "g", performBartlettTest = "g", performOBrienTest = "g")
  for (fn in names(calls)) {
    run <- function(model, data, v) suppressWarnings(calls[[fn]](model, data, v))
    expect_s3_class(run(m, named, beyond[[fn]]), "htest")
    for (data in refused) {
      expect_error(run(m, data, beyond[[fn]]),
                   "tie on every column of the model frame that is compared",
                   class = "heteroTests_data_rows_error", info = fn)
    }
    # A column of the model frame is read from rows that the comparison
    # confirmed, ties or not.
    if (beyond[[fn]] == "g") {
      grouped <- lm(y ~ x + g, data = named)
      expect_identical(run(grouped, numbered, "g")$statistic, run(grouped, named, "g")$statistic)
    } else {
      expect_identical(run(m, numbered, "x")$statistic, run(m, named, "x")$statistic, info = fn)
    }
  }
  # The spatial test reads no column of `data`: its weights list the
  # observations of the fit, in its order, so data in the order of the fit are
  # taken, ties or not.
  skip_if_not_installed("spdep")
  set.seed(1)
  coords <- cbind(runif(n), runif(n))
  lw <- suppressWarnings(spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4))))
  spatial <- function(data) {
    set.seed(7)
    performSpatialHeteroTest(m, data, listw = lw, permutations = 19)
  }
  expected <- spatial(named)
  for (data in refused) {
    context_same(spatial(data), expected, "spatial")
  }
})

test_that("a gam is matched to `data` by the row names of its model frame", {
  # The residuals of an mgcv::gam() fit carry no names. Before 0.16.0 the
  # tests that read a column of `data` then matched `data` to them by
  # position, and given the rows of the fit in another order under their own
  # names returned a wrong value without an error (N17): Glejser's test gave
  # -0.322 for 3.626 on the gam without smooth terms. The context matches the
  # rows by the row names of the model frame. Increment 4: the five group
  # tests, which matched it by position too (Bartlett 4.548 for 12.38), and a
  # gam with an `offset` argument.
  skip_if_not_installed("mgcv")
  d <- context_data()
  set.seed(3)
  shuffled <- d[sample(nrow(d)), ]
  fits <- list(
    mgcv::gam(y ~ x1 + x2, data = d), mgcv::gam(y ~ s(x1) + x2, data = d),
    mgcv::gam(y ~ x1 + x2, data = d, offset = o)
  )
  for (m in fits) {
    for (fn in context_runnable(c(context_increment3, context_increment4))) {
      expected <- context_run(fn, list(build = function() list(m = m, data = d)))
      expect_s3_class(expected, "htest")
      context_same(context_run(fn, list(build = function() list(m = m, data = shuffled))),
                   expected, paste(fn, deparse(stats::formula(m))))
    }
  }
})

test_that("Harrison-McCabe takes the rank of the fit, decided at its tolerance (N4)", {
  # Decided on 2026-10-18 (decision 21). A Gaussian glm decides its rank at
  # 1e-11, and an lm at the `tol` it is given; with x3 = x1 + 1e-9 z both keep
  # x3, a rank of 4. Before 0.16.0 the test decided the rank again at 1e-7 and
  # put a null distribution of rank 3 under residuals of rank 4: p 0.002288
  # where the fit's rank gives 0.002273. The reference writes the test out
  # with the rank of the fit, from the fit's own QR decomposition
  # (context_hmc_by_hand()). On a well-conditioned fit the two agree to
  # rounding. With x3 = x1 + 1e-9 z the direction x3 adds to the column space
  # is fixed only to about eps / 1e-9, some 1e-7 relative, in each
  # decomposition, the fit's, in the order of the fit, and the test's, in the
  # order of `order_by`: the p-values agree to 1e-8 relative (measured), so
  # they are compared to 1e-6 on those fits, which still tells the rank of
  # the fit, 0.002273, from the rank of 1e-7, 0.002288.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  cells <- context_rank_cells()
  ranks <- c(glm = 4L, glm_weighted = 4L, lm_tol = 4L, lm = 3L, glm_conditioned = 3L)
  near_collinear <- c("glm", "glm_weighted", "lm_tol")
  for (cell in cells) {
    built <- cell$build()
    id <- sub("^rank/", "", cell$id)
    expect_identical(built$m$rank, ranks[[id]], info = id)
    for (args in list(list(order_by = "x1"), list(order_by = "x2", point = 1 / 3))) {
      out <- do.call(performHarrisonMcCabeTest, c(list(built$m, built$data), args))
      reference <- do.call(context_hmc_by_hand, c(list(built$m, built$data), args))
      info <- paste(id, args$order_by)
      expect_equal(unname(out$statistic), unname(reference$statistic), tolerance = 1e-12,
                   info = info)
      expect_identical(unname(out$parameter), unname(reference$parameter), info = info)
      tolerance <- if (id %in% near_collinear) 1e-6 else 1e-12
      expect_equal(out$p.value, reference$p.value, tolerance = tolerance, info = info)
    }
  }
  # Each part of the ordered sample needs as many observations as the fit has
  # coefficients: four for the glm.
  # Before 0.16.0, with the rank 3 of 1e-7, `point = 3` and `point = 117`
  # gave p-values (0.0891 and 0.1227); they now stop.
  glm_cell <- cells[["rank/glm"]]$build()
  for (point in c(3, 117)) {
    expect_error(performHarrisonMcCabeTest(glm_cell$m, glm_cell$data, "x1", point = point),
                 "Each part needs at least 4, the number of coefficients.", fixed = TRUE,
                 info = point)
  }
  # The same glm with the rank decided at 1e-7, as before: another p-value.
  ctx <- rmodel_context(glm_cell$m, glm_cell$data, fn = "f")
  ordered <- rcontext_design(ctx)[order(glm_cell$data$x1), , drop = FALSE]
  expect_identical(qr(ordered)$rank, 3L)
  expect_identical(qr(ordered, tol = ctx$qr_tol)$rank, 4L)
  e <- unname(rcontext_pearson(ctx))[order(glm_cell$data$x1)]
  share <- sum(e[1:60]^2) / sum(e^2)
  before <- rhmc_tails(share, rhmc_spectrum(qr(ordered), 60L))[["lower"]]
  after <- performHarrisonMcCabeTest(glm_cell$m, glm_cell$data, "x1")$p.value
  expect_gt(abs(after - before), 1e-5)
})

test_that("the registered Szroeter test orders by the fitted values of the context", {
  d <- context_data()
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  fits <- list(
    ols = lm(y ~ x1 + x2, data = d), wls = lm(y ~ x1 + x2, data = d, weights = w),
    argument = lm(y ~ x1 + x2, data = d, offset = o),
    term = lm(y ~ x1 + x2 + offset(o), data = d),
    both = lm(y ~ x1 + x2 + offset(o), data = d, offset = o),
    glm = glm(y ~ x1 + x2, data = d, weights = w),
    glm_offset = glm(y ~ x1 + x2, data = d, offset = o),
    rank_deficient = lm(y ~ x1 + x2 + I(2 * x2), data = d),
    factor = lm(y ~ x1 + group, data = d),
    fitwls = fitWLS(lm(y ~ x1 + x2, data = d)), rlm = MASS::rlm(y ~ x1 + x2, data = d),
    na_omit = lm(y ~ x1 + x2, data = d_na)
  )
  if (requireNamespace("mgcv", quietly = TRUE)) {
    fits$gam <- mgcv::gam(y ~ x1 + x2, data = d)
    fits$gam_smooth <- mgcv::gam(y ~ s(x1) + x2, data = d)
    fits$gam_offset_term <- mgcv::gam(y ~ x1 + x2 + offset(o), data = d)
  }
  # The fitted values of the fit, the linear predictor with the offset, which
  # fitted() holds (an lm up to rounding, as y - e), are the values
  # predict(type = "response") computes, which the registered test read before
  # 0.16.0: the same numbers, so ties are broken as before.
  for (nm in names(fits)) {
    ctx <- rmodel_context(fits[[nm]], fn = "f")
    predicted <- unname(as.vector(stats::predict(fits[[nm]], type = "response")))
    expect_identical(rcontext_mean(ctx), predicted, info = nm)
    expect_equal(rcontext_mean(ctx), unname(as.vector(stats::fitted(fits[[nm]]))),
                 tolerance = 1e-12, info = nm)
  }
  # But predict.gam() leaves out an offset given as the `offset` argument, as
  # mgcv documents, and fitted() does not: the registered test ordered such a
  # gam by X b alone, and orders it by X b + o, as the same gam with an
  # offset() term and the lm with that offset.
  if (requireNamespace("mgcv", quietly = TRUE)) {
    argument <- mgcv::gam(y ~ x1 + x2, data = d, offset = o)
    ctx <- rmodel_context(argument, fn = "f")
    expect_identical(rcontext_mean(ctx), unname(as.vector(stats::fitted(argument))))
    term_ctx <- rmodel_context(fits$gam_offset_term, fn = "f")
    expect_identical(rcontext_mean(ctx), rcontext_mean(term_ctx))
    expect_false(isTRUE(all.equal(rcontext_mean(ctx),
                                  unname(as.vector(stats::predict(argument, type = "response"))))))
    mt <- mtcars
    set.seed(1)
    mt$o <- stats::rnorm(32)
    szroeter_registered <- get("szroeter", envir = heteroTests:::.diagnostic_registry)
    on_gam <- szroeter_registered(mgcv::gam(mpg ~ wt + qsec, data = mt, offset = o), mt)
    on_lm <- szroeter_registered(lm(mpg ~ wt + qsec, data = mt, offset = o), mt)
    # 0.800386 (p = 0.2117) before 0.16.0, ordered by X b alone.
    expect_equal(unname(on_gam$statistic), unname(on_lm$statistic), tolerance = 1e-10)
    expect_equal(unname(on_gam$statistic), 0.962844, tolerance = 1e-6)
  }
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  szroeter <- get("szroeter", envir = heteroTests:::.diagnostic_registry)
  m <- fits$na_omit
  expected <- szroeter(m, d_na)
  fit_rows <- d_na[names(m$residuals), ]
  fit_rows$fv <- rcontext_mean(rmodel_context(m, fn = "f"))
  expect_identical(unname(expected$statistic),
                   unname(performSzroeterTest(m, fit_rows, order_by = "fv")$statistic))
  # Without `data` it reads the model frame, where it stopped with "Missing
  # rows"; the complete rows numbered again are taken in the order of the
  # fit, where they were matched by row name and refused.
  expect_identical(szroeter(m, NULL)$statistic, expected$statistic)
  complete <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  rownames(complete) <- NULL
  expect_identical(szroeter(m, complete)$statistic, expected$statistic)
  # A fit with model = FALSE whose data are gone is tested on data that give
  # its response and fitted values; predict() could not rebuild its frame.
  env <- new.env()
  assign("gone", d, envir = env)
  m_false <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  expect_identical(szroeter(m_false, d)$statistic, szroeter(fits$ols, d)$statistic)
  # Data the guard refuses are refused, with its class.
  arranged <- d[order(d$x1), ]
  rownames(arranged) <- NULL
  expect_error(szroeter(fits$ols, arranged), class = "heteroTests_data_rows_error")
})

test_that("the registered tests of increment 3 keep the refusals of the context final", {
  d <- context_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  run <- function(m, data, tests) {
    suppressWarnings(runHeteroTests(m, data, tests = tests, use_cache = FALSE, progress = FALSE))
  }
  edited <- lm(y ~ x1 + x2, data = d)
  edited$coefficients[["x1"]] <- edited$coefficients[["x1"]] + 0.5
  mlm <- lm(cbind(y, ypos) ~ x1 + x2, data = d)
  arranged <- d[order(d$x1), ]
  rownames(arranged) <- NULL
  expect_error(run(edited, d, "szroeter"), "does not reproduce its residuals",
               class = "heteroTests_model_context_error")
  expect_error(run(mlm, d, "szroeter"), "needs a model with one response",
               class = "heteroTests_model_context_error")
  expect_error(run(lm(y ~ x1 + x2, data = d), arranged, "szroeter"),
               class = "heteroTests_data_rows_error")
  # The spatial test has fallbacks in the runner, Breusch-Pagan and White, which
  # it does not run under its name after a refusal of the context.
  skip_if_not_installed("spdep")
  with_weights <- function(data) {
    set.seed(1)
    coords <- cbind(runif(nrow(d)), runif(nrow(d)))
    attr(data, "listw") <- suppressWarnings(
      spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(coords, k = 4)))
    )
    data
  }
  expect_error(run(edited, with_weights(d), "spatial_hetero"), "does not reproduce its residuals",
               class = "heteroTests_model_context_error")
  expect_error(run(mlm, with_weights(d), "spatial_hetero"), "needs a model with one response",
               class = "heteroTests_model_context_error")
  expect_error(run(lm(y ~ x1 + x2, data = d), with_weights(arranged), "spatial_hetero"),
               class = "heteroTests_data_rows_error")
  spatial <- get("spatial_hetero", envir = heteroTests:::.diagnostic_registry)
  expect_error(spatial(edited, with_weights(d)), class = "heteroTests_model_context_error")
  out <- run(lm(y ~ x1 + x2, data = d), with_weights(d), "spatial_hetero")$spatial_hetero
  expect_identical(out$method, "Spatial heteroscedasticity test (Moran's I on squared residuals)")
})

test_that("a fit with a zero weight is refused on every route of increments 3 and 4", {
  # The tests refused it before 0.16.0 by the Pearson residuals of the fit,
  # and still do, with data, without and with the rows shuffled; Hartley's
  # test and the five group tests without `data` lack their groups first (the
  # matrix checks it).
  zero <- context_data()
  zero$w0 <- zero$w
  zero$w0[1] <- 0
  set.seed(3)
  shuffled <- zero[sample(nrow(zero)), ]
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  szroeter <- get("szroeter", envir = heteroTests:::.diagnostic_registry)
  for (m in list(lm(y ~ x1 + x2, data = zero, weights = w0),
                 glm(y ~ x1 + x2, data = zero, weights = w0))) {
    for (data in list(zero, NULL, shuffled)) {
      for (fn in context_runnable(c(context_increment3, context_increment4))) {
        if (is.null(data) && fn %in% context_group_readers()) next
        expect_error(suppressWarnings(contract_recipes[[fn]](m, data)),
                     "zero or non-finite weights", info = fn)
      }
      expect_error(szroeter(m, data), "zero or non-finite weights")
      expect_error(
        suppressWarnings(runHeteroTests(m, data, tests = "szroeter", use_cache = FALSE,
                                        progress = FALSE)),
        "zero or non-finite weights"
      )
    }
  }
})

test_that("an offset is the model of the response minus the offset", {
  # 400 calls, among them fitWLS() fits: with the full matrix, off CRAN.
  skip_on_cran()
  d <- context_data()
  shifted <- d
  shifted$y <- d$y - d$o
  # RESET is the nested F test of the model with its offset (below), and the
  # quantile regression test refuses a weighted fit.
  for (fn in setdiff(context_runnable(), "performRESETTest")) {
    fits <- if (identical(fn, "performQuantileRegressionTest")) "ols" else context_fits
    # Hartley's test and the group tests group by a variable the model frame
    # does not hold.
    forms <- if (fn %in% context_group_readers()) "data_frame" else c("data_frame", "null")
    # The fit of y - o is another fit of the model, whose rounding can leave
    # a tie of Fligner-Killeen's ranks at a group median exact where the fit
    # with the offset breaks it (N15): measured 1.5e-7 of the statistic on the
    # Gaussian glm on Linux.
    fligner <- identical(fn, "performFlignerKilleenTest")
    tolerance <- if (fligner) context_fligner_tolerance else 1e-10
    for (fit in fits) {
      expected <- context_run(fn, list(build = function() {
        m <- context_model(shifted, fit)
        list(m = m, data = shifted)
      }))
      for (offset in c("argument", "term")) {
        for (form in forms) {
          out <- context_run(fn, list(build = function() {
            m <- context_model(d, fit, offset)
            list(m = m, data = context_form(form, m, d))
          }))
          context_same(out, expected, paste(fn, fit, offset, form), tolerance = tolerance)
        }
      }
    }
  }
})

test_that("RESET with an offset is the nested F test of the model with its offset", {
  # Decided on 2026-10-18: the restricted model is the fit, with its offset,
  # and the augmented model adds the powers of its fitted values, X b + o,
  # fitted with the same offset and weights. The reference is anova() of the
  # two nested lm() fits, computed independently of the package.
  d <- context_data()
  for (fit in context_fits) {
    for (offset in c("argument", "term")) {
      m <- context_model(d, fit, offset)
      ref <- d
      ref$f2 <- stats::fitted(m)^2
      ref$f3 <- stats::fitted(m)^3
      weights_of_fit <- stats::weights(m)
      ref$wt <- if (is.null(weights_of_fit)) rep(1, nrow(d)) else weights_of_fit
      restricted <- stats::lm(y ~ x1 + x2, data = ref, weights = wt, offset = o)
      augmented <- stats::lm(y ~ x1 + x2 + f2 + f3, data = ref, weights = wt, offset = o)
      reference <- stats::anova(restricted, augmented)
      for (form in c("data_frame", "null")) {
        info <- paste(fit, offset, form)
        out <- context_run("performRESETTest", list(build = function() {
          list(m = m, data = context_form(form, m, d))
        }))
        expect_s3_class(out, "htest")
        expect_equal(unname(out$statistic), reference$F[2], tolerance = 1e-10, info = info)
        expect_equal(unname(out$parameter), c(reference$Df[2], reference$Res.Df[2]), info = info)
        expect_equal(out$p.value, reference$`Pr(>F)`[2], tolerance = 1e-10, info = info)
      }
    }
  }
  # It is not RESET of the model of the response minus the offset, which
  # augments that model with the powers of X b alone.
  shifted <- d
  shifted$y <- d$y - d$o
  with_offset <- performRESETTest(lm(y ~ x1 + x2, data = d, offset = o))
  without <- performRESETTest(lm(y ~ x1 + x2, data = shifted))
  expect_gt(abs(unname(with_offset$statistic) - unname(without$statistic)), 1e-3)
})

test_that("a term is the column it computes, taken from the fit", {
  # 600 expectations: with the full matrix, off CRAN.
  skip_on_cran()
  d <- context_data()
  precomputed <- context_precomputed()$formulas
  for (fn in context_runnable()) {
    fits <- if (identical(fn, "performQuantileRegressionTest")) "ols" else c("ols", "wls")
    for (fit in fits) {
      for (terms in names(precomputed)) {
        expected <- context_run(fn, list(build = function() context_precomputed_fit(fit, terms)))
        # The fit with the columns computed beforehand is another fit of the
        # model: Fligner-Killeen's ranks may tie at a group median on one and
        # not on the other (N15).
        tolerance <- if (identical(fn, "performFlignerKilleenTest")) {
          context_fligner_tolerance
        } else if (terms == "poly") {
          1e-8
        } else {
          1e-10
        }
        # Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe read x1,
        # which the model frame of these fits does not hold, and Hartley's
        # test groups by a variable it does not hold either: without `data`
        # they are refused (the matrix checks it).
        forms <- if (fn %in% names(context_reads)) "data_frame" else c("data_frame", "null")
        for (form in forms) {
          out <- context_run(fn, list(build = function() {
            m <- context_model(d, fit, "none", terms)
            list(m = m, data = context_form(form, m, d))
          }))
          context_same(out, expected, paste(fn, fit, terms, form), tolerance = tolerance)
        }
      }
    }
  }
})

test_that("weights are the weights of the fit, whatever holds them", {
  d <- context_data()
  # The quantile regression test refuses a weighted fit.
  for (fn in setdiff(context_runnable(), "performQuantileRegressionTest")) {
    run <- function(m) context_run(fn, list(build = function() list(m = m, data = d)))
    wls <- run(context_model(d, "wls"))
    # Fligner-Killeen's ranks tie at a group median on one fit and not, by
    # rounding, on the other (N15, context_fligner_tolerance).
    tolerance <- if (identical(fn, "performFlignerKilleenTest")) context_fligner_tolerance else 1e-8
    context_same(run(context_model(d, "glm")), wls, paste(fn, "glm"), tolerance = tolerance)
    context_same(run(context_model(d, "wvec")), wls, paste(fn, "wvec"))
    estimated <- context_model(d, "fitwls")
    context_same(run(estimated), run(lm(y ~ x1 + x2, data = d, weights = weights(estimated))),
                 paste(fn, "fitwls"))
  }
})

test_that("models the tests do not take are refused first, with today's messages", {
  d <- context_data()
  d$high <- as.integer(d$y > stats::median(d$y))
  binomial_fit <- glm(high ~ x1 + x2, data = d, family = binomial)
  for (fn in context_runnable()) {
    recipe <- contract_recipes[[fn]]
    expect_error(recipe(binomial_fit, d), "needs a model fitted by stats::lm()", fixed = TRUE,
                 info = fn)
    expect_error(recipe(lm(cbind(y, ypos) ~ x1 + x2, data = d), d),
                 sprintf("%s() needs a model with one response; `model` has 2.", fn),
                 fixed = TRUE, info = fn)
  }
  skip_if_not_installed("survey")
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  survey_fit <- survey::svyglm(y ~ x1 + x2, design = design)
  for (fn in context_runnable()) {
    expect_error(contract_recipes[[fn]](survey_fit, d), "The model is a survey::svyglm() fit",
                 fixed = TRUE, info = fn)
  }
})

test_that("a refusal of data names the function the user called", {
  d <- context_data()
  arranged <- d[order(d$x1), ]
  rownames(arranged) <- NULL
  m <- lm(y ~ x1 + x2, data = d)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  expect_error(suppressWarnings(performBreuschPaganTest(m, arranged)),
               "^performBreuschPaganTest\\(\\) cannot match")
  expect_error(performWhiteTestRobust(m, arranged), "^performWhiteTestRobust\\(\\) cannot match")
  expect_error(performRankPermutationTest(m, arranged, B = 9, progress = FALSE),
               "^performRankPermutationTest\\(\\) cannot match")
})

test_that("weighted or offset fits without data run the test the runner names", {
  # Before 0.16.0 performWhiteTest() and performKoenkerTest() stopped on such a
  # fit without data ("object 'w' not found"), and runHeteroTests() put the
  # Cook-Weisberg test it fell back on under their names.
  d <- context_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (m in list(lm(y ~ x1 + x2, data = d, weights = w), lm(y ~ x1 + x2, data = d, offset = o))) {
    out <- suppressWarnings(runHeteroTests(m, use_cache = FALSE, progress = FALSE))
    expect_identical(out$white$method, "White's test for heteroscedasticity")
    expect_identical(out$koenker$method, "Koenker studentized Breusch-Pagan test")
    expect_identical(unname(out$koenker$statistic),
                     unname(performKoenkerTest(m, d)$statistic))
    expect_identical(unname(out$white$statistic),
                     unname(suppressWarnings(performWhiteTest(m, d))$statistic))
  }
})

# The five group tests computed independently of the package: Levene's and
# Brown-Forsythe's tests by car::leveneTest(), centred at the group means and
# medians, Fligner-Killeen's and Bartlett's by stats::fligner.test() and
# stats::bartlett.test(), and O'Brien's transformation written out, with the
# F test of anova() of lm(); each on residuals `r` and groups `g` given in
# the same order. The statistic and the p-value.
context_group_oracle <- function(fn, r, g) {
  g <- droplevels(factor(g))
  r <- unname(r)
  levene <- function(center) {
    t <- car::leveneTest(r, g, center = center)
    c(t$`F value`[1], t$`Pr(>F)`[1])
  }
  switch(fn,
    performLeveneTest = levene(mean),
    performBrownForsytheTest = levene(stats::median),
    performFlignerKilleenTest = {
      t <- stats::fligner.test(r, g)
      c(t$statistic, t$p.value)
    },
    performBartlettTest = {
      t <- stats::bartlett.test(r, g)
      c(t$statistic, t$p.value)
    },
    performOBrienTest = {
      v <- numeric(length(r))
      for (level in levels(g)) {
        i <- which(g == level)
        n_i <- length(i)
        v[i] <- ((n_i - 1.5) * n_i * (r[i] - mean(r[i]))^2 - 0.5 * (n_i - 1) * stats::var(r[i])) /
          ((n_i - 1) * (n_i - 2))
      }
      t <- stats::anova(stats::lm(v ~ g))
      c(t$`F value`[1], t$`Pr(>F)`[1])
    }
  )
}

test_that("the group tests give the value of an independent implementation of each", {
  # Increment 4. On the forms of `data` whose rows the context now matches,
  # and those it matched before, each of the five gives the test computed
  # without the package (context_group_oracle()) on the residuals of the fit
  # and the groups of the data frame it was fitted to, in the order of the
  # fit: a gam, with and without a smooth term or an offset argument, given
  # its data in another order (N17), the character-named fit given its
  # tibble, the complete rows of an na.omit fit numbered again, a model of the
  # group without `data` and with its model frame, a weighted fit, whose
  # Pearson residuals are sqrt(w) e (weighted.residuals()), and groups missing
  # at observations of the fit, which are left out.
  skip_if_not_installed("car")
  skip_if_not_installed("mgcv")
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  set.seed(3)
  perm <- sample(nrow(d))
  named <- d
  rownames(named) <- paste0("obs", seq_len(nrow(d)))
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  complete <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  renumbered <- complete
  rownames(renumbered) <- NULL
  d_g <- d
  d_g$group[c(5, 50)] <- NA
  d_gn <- d_na
  d_gn$group[c(5, 7)] <- NA
  kept_g <- !is.na(d_g$group)
  fit_gn <- stats::lm(y ~ x1 + x2, data = d_gn)
  # The groups of the observations of the fit, by the names of its residuals.
  g_gn <- d_gn$group[match(names(stats::residuals(fit_gn)), rownames(d_gn))]
  kept_gn <- !is.na(g_gn)
  wls <- stats::lm(y ~ x1 + x2, data = d, weights = w)
  grouped_offset <- stats::lm(y ~ x1 + group + offset(o), data = d)
  cases <- list(
    lm_shuffled = list(m = stats::lm(y ~ x1 + x2, data = d), data = d[perm, ], g = d$group),
    gam_shuffled = list(m = mgcv::gam(y ~ x1 + x2, data = d), data = d[perm, ], g = d$group),
    gam_smooth_shuffled = list(m = mgcv::gam(y ~ s(x1) + x2, data = d), data = d[perm, ],
                               g = d$group),
    gam_offset_shuffled = list(m = mgcv::gam(y ~ x1 + x2, data = d, offset = o), data = d[perm, ],
                               g = d$group),
    character_names_tibble = list(m = stats::lm(y ~ x1 + x2, data = named),
                                  data = dplyr::as_tibble(named), g = named$group),
    complete_renumbered = list(m = stats::lm(y ~ x1 + x2, data = d_na), data = renumbered,
                               g = complete$group),
    log_factor_null = list(m = stats::lm(log(ypos) ~ log(x1) + group, data = d), data = NULL,
                           g = d$group),
    offset_factor_frame = list(m = grouped_offset, data = stats::model.frame(grouped_offset),
                               g = d$group),
    wls_shuffled = list(m = wls, data = d[perm, ], g = d$group,
                        r = stats::weighted.residuals(wls)),
    group_na_shuffled = list(m = stats::lm(y ~ x1 + x2, data = d_g), data = d_g[perm, ],
                             g = d_g$group[kept_g], keep = kept_g),
    group_na_dropped = list(m = fit_gn, data = d_gn, g = g_gn[kept_gn], keep = kept_gn)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    r <- if (is.null(case$r)) stats::residuals(case$m) else case$r
    if (!is.null(case$keep)) r <- r[case$keep]
    for (fn in context_increment4) {
      out <- suppressWarnings(contract_recipes[[fn]](case$m, case$data))
      expect_s3_class(out, "htest")
      expected <- context_group_oracle(fn, r, case$g)
      expect_equal(unname(c(out$statistic, out$p.value)), unname(expected), tolerance = 1e-10,
                   info = paste(fn, nm))
    }
  }
})

test_that("a missing group leaves its observation out, with the warning of before", {
  # Before 0.16.0 the five group tests matched the rows of `data` to the
  # residuals and then left out the observations of the fit whose group is
  # missing, with the warning "Removed N observations due to missing values in
  # group" (decision 8: each test keeps its behaviour). They still do, on every
  # form of `data` that holds the rows of the fit, with the same warning and
  # the same statistic; a group missing at a row the fit dropped is not
  # counted. Hartley's test refuses such data (the matrix checks it).
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  warnings_of <- function(expr) {
    caught <- character()
    value <- withCallingHandlers(expr, warning = function(w) {
      caught <<- c(caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
    list(value = value, warnings = caught)
  }
  d <- context_data()
  set.seed(3)
  perm <- sample(nrow(d))
  d_g <- d
  d_g$group[c(5, 50)] <- NA
  m <- stats::lm(y ~ x1 + x2, data = d_g)
  d_gn <- d_g
  d_gn$group[50] <- d$group[50]
  d_gn$group[7] <- NA
  d_gn$x2[c(3, 7, 11)] <- NA
  m_gn <- stats::lm(y ~ x1 + x2, data = d_gn)
  complete <- d_gn[stats::complete.cases(d_gn[, c("y", "x1", "x2")]), ]
  rownames(complete) <- NULL
  for (fn in context_increment4) {
    recipe <- contract_recipes[[fn]]
    expected <- warnings_of(recipe(m, d_g))
    expect_true("Removed 2 observations due to missing values in group" %in% expected$warnings,
                info = fn)
    for (data in list(d_g[perm, ], dplyr::as_tibble(d_g))) {
      got <- warnings_of(recipe(m, data))
      expect_identical(got$warnings, expected$warnings, info = fn)
      context_same(got$value, expected$value, fn)
    }
    dropped <- warnings_of(recipe(m_gn, d_gn))
    expect_true("Removed 1 observations due to missing values in group" %in% dropped$warnings,
                info = fn)
    got <- warnings_of(recipe(m_gn, complete))
    expect_identical(got$warnings, dropped$warnings, info = fn)
    context_same(got$value, dropped$value, paste(fn, "renumbered"))
    # Every group missing: nothing is left to test.
    gone <- d
    gone$group <- factor(NA, levels = levels(d$group))
    expect_error(suppressWarnings(recipe(stats::lm(y ~ x1 + x2, data = d), gone)), info = fn)
  }
})

test_that("an NA or empty group name is refused before the checks of data", {
  # From the review of increment 4. Levene's, Brown-Forsythe's,
  # Fligner-Killeen's and Bartlett's tests checked only the length of
  # `group`, so an NA name reached rvalidateDataInputs() as a required
  # variable. The cache key of that check drops the NA and is then the key of
  # the check without variables, which the alignment makes: on data that lack
  # the columns of the model frame of the fit, the refusal of the NA name was
  # cached, and the next call of any test on those data stopped with "missing
  # from input data: NA". The name is now refused first, as O'Brien's test
  # refuses it.
  clearValidationCache()
  on.exit(clearValidationCache(), add = TRUE)
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(1)
  d <- data.frame(y = rnorm(60) + 5, x1 = runif(60, 1, 2), x2 = rnorm(60), group = gl(3, 20))
  d$ypos <- exp(d$y / 5)
  transformed <- lm(log(ypos) ~ x1, d)
  m <- lm(y ~ x1 + x2, d)
  white <- function() unname(suppressWarnings(performWhiteTest(m, d))$statistic)
  expected <- white()
  for (fn in context_increment4) {
    for (bad in list(NA_character_, "")) {
      clearValidationCache()
      expect_error(get(fn)(transformed, d, bad),
                   "`group` must be (supplied as )?a single column name", info = fn)
      expect_identical(white(), expected, info = paste(fn, bad))
    }
  }
})

test_that("a group test run by the runner keeps the refusals of the context final", {
  # The five group tests are not registered under a name of the runner, but a
  # diagnostic can be registered that calls one (registerDiagnostic()). Its
  # refusals by the context, of the model and of `data`, are final there: the
  # runner runs no other test under its name and keeps the class.
  d <- context_data()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  registry <- heteroTests:::.diagnostic_registry
  on.exit(rm(list = intersect(ls(registry), paste0("context_", context_increment4)),
             envir = registry), add = TRUE)
  run <- function(m, data, test) {
    suppressWarnings(runHeteroTests(m, data, tests = test, use_cache = FALSE, progress = FALSE))
  }
  edited <- lm(y ~ x1 + x2, data = d)
  edited$coefficients[["x1"]] <- edited$coefficients[["x1"]] + 0.5
  mlm <- lm(cbind(y, ypos) ~ x1 + x2, data = d)
  env <- new.env()
  assign("gone", d, envir = env)
  m_false <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  reversed <- d
  reversed$y <- rev(reversed$y)
  arranged <- d[order(d$x1), ]
  rownames(arranged) <- NULL
  plain <- lm(y ~ x1 + x2, data = d)
  for (fn in context_increment4) {
    name <- paste0("context_", fn)
    recipe <- contract_recipes[[fn]]
    registerDiagnostic(name, recipe)
    expect_error(run(edited, d, name), "does not reproduce its residuals",
                 class = "heteroTests_model_context_error")
    expect_error(run(mlm, d, name), "needs a model with one response",
                 class = "heteroTests_model_context_error")
    expect_error(run(m_false, reversed, name), "cannot use the model frame of `model`",
                 class = "heteroTests_model_context_error")
    expect_error(run(plain, arranged, name), "cannot match the rows of `data`",
                 class = "heteroTests_data_rows_error")
    out <- run(plain, d, name)[[name]]
    expect_identical(unname(out$statistic), unname(suppressWarnings(recipe(plain, d))$statistic),
                     info = fn)
  }
})
