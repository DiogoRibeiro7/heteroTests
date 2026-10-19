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
          tolerance <- if (id %in% c("class/rank_deficient", "class/gam")) 1e-10
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
    for (data in list(d, NULL)) {
      context_same(context_run(fn, list(build = function() list(m = plain, data = data))),
                   context_run(fn, list(build = function() list(m = lm(y ~ x1 + x2, d), data = d))),
                   paste(fn, "gam"), tolerance = 1e-10)
      out <- context_run(fn, list(build = function() list(m = smooth, data = data)))
      if (fn %in% context_increment2) {
        # A smooth term is usually penalised, and a penalised fit is not the
        # least-squares fit of its design: the tests of increment 2 refuse it,
        # until increment 11 settles which tests take it.
        expect_s3_class(out, "heteroTests_model_context_error")
        refusal <- "%s() cannot test an mgcv::gam() fit with smooth terms (s(x1))"
        expect_match(conditionMessage(out), sprintf(refusal, fn), fixed = TRUE, info = fn)
      } else {
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
    for (fit in fits) {
      expected <- context_run(fn, list(build = function() {
        m <- context_model(shifted, fit)
        list(m = m, data = shifted)
      }))
      for (offset in c("argument", "term")) {
        for (form in c("data_frame", "null")) {
          out <- context_run(fn, list(build = function() {
            m <- context_model(d, fit, offset)
            list(m = m, data = context_form(form, m, d))
          }))
          context_same(out, expected, paste(fn, fit, offset, form), tolerance = 1e-10)
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
        tolerance <- if (terms == "poly") 1e-8 else 1e-10
        # Goldfeld-Quandt orders by x1, which the model frame of these fits
        # does not hold: without `data` it is refused (the matrix checks it).
        forms <- if (identical(fn, "performGQTest")) "data_frame" else c("data_frame", "null")
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
    context_same(run(context_model(d, "glm")), wls, paste(fn, "glm"), tolerance = 1e-8)
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
