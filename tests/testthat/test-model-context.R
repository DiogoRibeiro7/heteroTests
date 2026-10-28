# The model context (R/model_context.R): the representation, its alignment
# step, and the test matrix of docs/design/model-context.md, run through the
# functions built on it (`context_migrated`, helper-model-context.R). The
# expectations are relations between calls, not stored values: a test gives
# on every form of `data` it accepts the value it gives on the data frame of
# the fit, on a fit with an offset the value of the model fitted to the
# response minus the offset (RESET: the nested F test of the model with its
# offset), on a term the value of the same column computed beforehand, and on
# a fit that dropped rows the value of the fit to the complete rows.

# The results of runPanelTests(): a named list of htest results.
context_is_suite <- function(x) {
  is.list(x) && !inherits(x, c("htest", "condition")) && length(x) > 0L &&
    all(vapply(x, inherits, logical(1), "htest"))
}

# A value of a function of the matrix: an htest result, the correlations of
# the scatter diagnostic, or the results of runPanelTests().
context_expect_value <- function(object, info) {
  expect_true(
    inherits(object, "htest") || is.numeric(object) || context_is_suite(object),
    info = paste(info, if (inherits(object, "condition")) conditionMessage(object))
  )
}

context_same <- function(object, expected, info, tolerance = NULL) {
  # The scatter diagnostic returns a correlation for each variable, and
  # runPanelTests() a list of the panel tests.
  if (is.numeric(expected)) {
    expect_true(is.numeric(object), info = info)
    if (is.null(tolerance)) {
      expect_identical(object, expected, info = info)
    } else {
      expect_equal(object, expected, tolerance = tolerance, info = info)
    }
    return(invisible(NULL))
  }
  if (context_is_suite(expected)) {
    expect_true(context_is_suite(object), info = info)
    if (!context_is_suite(object)) {
      return(invisible(NULL))
    }
    expect_identical(names(object), names(expected), info = info)
    for (k in names(expected)) {
      context_same(object[[k]], expected[[k]], paste(info, k), tolerance)
    }
    return(invisible(NULL))
  }
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

# A call on a fit with na.exclude against the same call on its twin, the same
# fit with na.omit (increment 9, B13): the same refusal, of the same class and
# with the same message, or the same value, every part of it.
context_same_as_twin <- function(object, twin, info) {
  if (inherits(twin, "condition")) {
    expect_s3_class(object, "error")
    expect_identical(class(object), class(twin), info = info)
    expect_identical(conditionMessage(object), conditionMessage(twin), info = info)
    return(invisible(NULL))
  }
  expect_false(inherits(object, "condition"),
               info = paste(info, if (inherits(object, "condition")) conditionMessage(object)))
  expect_identical(object, twin, info = info)
}

context_installed <- function(cell) {
  all(vapply(cell$needs, requireNamespace, logical(1), quietly = TRUE))
}

# Runs `fns` on `ids` and checks each cell: its value against its reference,
# `identical()`, or its refusal, as the test sees the cell
# (context_cell_for()). `recipes` holds the call of each of `fns`.
context_check_matrix <- function(ids, fns = context_runnable(), recipes = context_recipes()) {
  cells <- context_cells()
  for (fn in fns) {
    results <- new.env(parent = emptyenv())
    result_of <- function(id) {
      if (is.null(results[[id]])) {
        results[[id]] <- list(context_run(fn, cells[[id]], recipes))
      }
      results[[id]][[1L]]
    }
    for (id in ids) {
      cell <- context_cell_for(fn, cells[[id]])
      if (!context_installed(cell)) next
      out <- result_of(id)
      info <- paste(fn, id)
      # A fit with na.exclude gives what its twin, the same fit with na.omit,
      # gives (increment 9, B13): the same refusal, or the same value, an
      # output with one value per row padded as naresid() pads it.
      if (!is.null(cell$twin)) {
        context_same_as_twin(out, context_twin_value(fn, result_of(cell$twin), cell), info)
        next
      }
      if (identical(cell$expect, "value")) {
        context_expect_value(out, info)
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
  # The full matrix: off CRAN, unless HETEROTESTS_FULL_MATRIX=false
  # (context_full_matrix()).
  skip_unless_full_matrix()
  context_check_matrix(names(context_cells()))
})

test_that("a subset of the matrix runs on CRAN", {
  context_check_matrix(context_cran_cells, c(context_runnable(), context_second_formula))
})

test_that("a second formula is evaluated on the rows of the fit, on every form of data", {
  # Increment 6. Cook-Weisberg's test with `var_formula`, the variance form
  # test with `var_formula` or `against`, and fitWLS() with `var_formula` read
  # the variables of the formula at the rows the alignment confirmed (B9),
  # never from the workspace (R3); fitWLS() without one reads nothing from
  # `data` (context_cell_for_formula()).
  skip_unless_full_matrix()
  context_check_matrix(names(context_cells()), context_second_formula)
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
  # The rows the alignment picks by the row numbers of the fit, in its order;
  # until increment 10 the guard of the tests not built on the context
  # (rcheck_data_rows(), removed) picked them so too, and left numbered rows
  # in order to the test.
  fit_ids <- names(m_na$residuals)
  expect_length(fit_ids, 117L)
  expect_identical(rmodel_context(m_na, d_na, fn = "f")$rows, match(fit_ids, rownames(d_na)))
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
  recipes <- context_recipes()
  for (fn in context_runnable()) {
    recipe <- recipes[[fn]]
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
  # The tests computed from the model alone and RESET read nothing from
  # `data`, but build their context with it to rebuild such a frame (increment
  # 6); before, RESET refused the fit, and the others stopped without a model
  # frame ("Input data must be a data.frame") or gave the value of the fit
  # unchecked. And the second formulas, which read z at the rows of `data`.
  fns <- c(context_runnable(), context_second_formula)
  expected <- lapply(fns, function(fn) {
    context_run(fn, list(build = function() list(m = m, data = d)))
  })
  rm("gone", envir = env)
  expect_error(stats::model.frame(m))
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
    # The scatter diagnostic and runPanelTests() need `data`, which they check
    # before the model.
    if (fn %in% c("performScatterDiagnostic", "runPanelTests")) next
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
    # Before 0.16.0 Fligner-Killeen's ranks tied at a group median on one fit
    # and not, by rounding, on the other (N15, context_fligner_tolerance).
    fligner <- identical(fn, "performFlignerKilleenTest")
    tolerance <- if (fligner) context_fligner_tolerance else 1e-10
    for (data in list(d, NULL)) {
      # Hartley's test and the group tests group by a variable of `data` that
      # the model frame does not hold, and so do the panel tests: without
      # `data` they are refused (M1), as the matrix checks; the scatter
      # diagnostic and runPanelTests() need `data`.
      if (is.null(data) && fn %in% context_data_readers()) next
      # The wild bootstrap stops on every gam, whose leverages hatvalues() does
      # not give, as before increment 8 (context_cell_for()).
      if (identical(fn, "performWildBootstrapTest")) {
        for (m in list(plain, smooth)) {
          out <- context_run(fn, list(build = function() list(m = m, data = data)))
          expect_match(conditionMessage(out), "lm object does not have a proper 'qr' component",
                       fixed = TRUE)
        }
        next
      }
      context_same(context_run(fn, list(build = function() list(m = plain, data = data))),
                   context_run(fn, list(build = function() list(m = lm(y ~ x1 + x2, d), data = d))),
                   paste(fn, "gam"), tolerance = tolerance)
      out <- context_run(fn, list(build = function() list(m = smooth, data = data)))
      if (fn %in% c(context_increment2, "performWhiteTestBootstrap", "performBPTestRobust")) {
        # A smooth term is usually penalised, and a penalised fit is not the
        # least-squares fit of its design: the tests of increment 2 refuse it,
        # until increment 11 settles which tests take it, and so do the robust
        # Breusch-Pagan test, which studentizes by default, and the White
        # bootstrap (increment 8), whose replicates refit the design.
        expect_s3_class(out, "heteroTests_model_context_error")
        refusal <- "%s() cannot test an mgcv::gam() fit with smooth terms (s(x1))"
        expect_match(conditionMessage(out), sprintf(refusal, fn), fixed = TRUE, info = fn)
      } else {
        # Those of increments 1, 3, 4 and 5 test it as before 0.16.0.
        context_expect_value(out, fn)
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
  # gam with an `offset` argument. Increment 5: the panel tests and the
  # scatter diagnostic, which refused other data with row names of their own
  # in another order than the fit, but read those of a gam by position
  # (Breusch-Pagan random effects 0.163 for 3.100). Increment 7: the streaming
  # tests, which built their design from the rows of `data` by position
  # (White's streaming test 8.887 for 11.145).
  skip_if_not_installed("mgcv")
  d <- context_data()
  set.seed(3)
  shuffled <- d[sample(nrow(d)), ]
  fits <- list(
    mgcv::gam(y ~ x1 + x2, data = d), mgcv::gam(y ~ s(x1) + x2, data = d),
    mgcv::gam(y ~ x1 + x2, data = d, offset = o)
  )
  fns <- c(context_increment3, context_increment4, context_increment5, context_streaming)
  for (m in fits) {
    for (fn in context_runnable(fns)) {
      expected <- context_run(fn, list(build = function() list(m = m, data = d)))
      context_expect_value(expected, fn)
      context_same(context_run(fn, list(build = function() list(m = m, data = shuffled))),
                   expected, paste(fn, deparse(stats::formula(m))))
    }
  }
})

test_that("the streaming tests read the stored design in chunks, on every form of data", {
  # Increment 7: in chunks of 7 observations, 18 on the 120 of the matrix and
  # a last one of a single observation, each value is that of the data frame
  # of the fit, identical(), as in the one chunk of 10,000 of the matrix.
  skip_unless_full_matrix()
  context_check_matrix(names(context_cells()), context_streaming_chunked,
                       context_streaming_recipes())
})

test_that("a term computed from its rows keeps the values of the fit in every chunk (B6)", {
  # Up to 0.16.0 each chunk built its own design from the formula, on its rows
  # of `data`. I(x1 > median(x1)) took the median of each chunk: on the term
  # cell of the matrix, in chunks of 7, the Breusch-Pagan test gave 11.375
  # and Koenker's 8.165, in chunks of 40 12.344 and 8.861, where the fit gives
  # 12.901 and 9.260. cut(x1, quantile(x1)) took the breaks of each chunk, and
  # of the rows of the fit, which dropped the row at the minimum of x1, so it
  # stopped on "factor cut(x1, quantile(x1)) has new levels" in chunks of any
  # size (N7). On the fit that dropped rows, the median of the rows of the fit
  # replaced that of all the rows the fit saw (B5): 13.345 and 9.276 in one
  # chunk, 5.746 and 3.994 in chunks of 7, where the fit gives 12.464 and
  # 8.664. Each now gives the value of the same fit with the column computed
  # beforehand, and that of the exact test.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  d_med <- d
  d_med$x2[c(3, 7, 11)] <- NA
  d_med$high <- d_med$x1 > stats::median(d_med$x1)
  cases <- list(
    median = list(m = context_model(d, "ols", "none", "median"), data = d,
                  reference = context_precomputed_fit("ols", "median")),
    cut = list(m = context_model(d, "ols", "none", "cut"), data = d,
               reference = context_precomputed_fit("ols", "cut")),
    dropped = list(m = lm(y ~ I(x1 > median(x1)) + x2, data = d_med), data = d_med,
                   reference = list(m = lm(y ~ high + x2, data = d_med), data = d_med))
  )
  exact <- list(performBPTestStreaming = performBPTest,
                performKoenkerTestStreaming = performKoenkerTest)
  for (case in names(cases)) {
    m <- cases[[case]]$m
    data <- cases[[case]]$data
    reference <- cases[[case]]$reference
    for (chunk in c(7, 40, 10000)) {
      for (fn in names(exact)) {
        info <- paste(fn, case, chunk)
        run <- function(model, data) {
          suppressWarnings(get(fn)(model, data, chunk_size = chunk, progress = FALSE))
        }
        out <- run(m, data)
        context_same(out, run(reference$m, reference$data), info, tolerance = 1e-10)
        context_same(out, run(m, NULL), info, tolerance = 1e-10)
        # The exact tests warn of the rows the fit dropped.
        expect_equal(unname(out$statistic),
                     unname(suppressWarnings(exact[[fn]](m, data))$statistic),
                     tolerance = 1e-8, info = info)
      }
      # White's streaming test drops the collinear squares of the dummies,
      # as performWhiteTest() does, since the pull request of N3 (0.17.0);
      # in 0.16.0 it stopped on them, "Auxiliary regression became rank
      # deficient", and before, where cut() computed other breaks, on the
      # new levels.
      white <- function(model, data) {
        suppressWarnings(performWhiteTestStreaming(model, data, chunk_size = chunk,
                                                   progress = FALSE))
      }
      out <- white(m, data)
      info <- paste("performWhiteTestStreaming", case, chunk)
      context_same(out, white(reference$m, reference$data), info, tolerance = 1e-10)
      context_same(out, suppressWarnings(performWhiteTest(m, data)), info, tolerance = 1e-8)
    }
  }
})

test_that("White's streaming test drops the collinear terms of the cells of the matrix (N3)", {
  # The cells whose terms make White's auxiliary terms collinear, a factor, a
  # factor with log() terms, poly(), I(x1^2) beside x1, I(x1 > median(x1)) and
  # cut(), on an unweighted and a weighted fit, and the model of the group of
  # increment 4 and the median of a fit that dropped rows, on every form of
  # data each accepts: in chunks of 7 and of 10,000 it gives the value of
  # performWhiteTest() and its degrees of freedom, where up to 0.16.0 it
  # stopped on those terms. That it gives the value of the reference of each
  # cell is in the matrix (context_cell_for_streaming()).
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  cells <- context_cells()
  ids <- names(cells)[vapply(cells, function(cell) {
    identical(cell$expect, "value") && isTRUE(cell$terms %in% context_white_streaming_collinear) &&
      context_installed(cell)
  }, logical(1))]
  expect_true(all(c("terms/ols/none/factor/data_frame", "terms/wls/none/factor/null",
                    "terms/ols/none/log_factor/shuffled", "terms/ols/term/factor/model_frame",
                    "group/na_factor", "missing/median_full") %in% ids))
  for (id in ids) {
    built <- suppressWarnings(suppressMessages(cells[[id]]$build()))
    exact <- suppressWarnings(suppressMessages(performWhiteTest(built$m, built$data)))
    for (chunk in c(7, 10000)) {
      info <- paste(id, chunk)
      out <- suppressWarnings(suppressMessages(
        performWhiteTestStreaming(built$m, built$data, chunk_size = chunk, progress = FALSE)
      ))
      context_same(out, exact, info, tolerance = 1e-8)
    }
  }
})

test_that("the streaming tests take the inputs of the tests built on the context", {
  # Increment 7. Without `data`, a model that transforms its variables or has
  # an offset() term stopped with "The following variables are missing from
  # input data" (L1); the complete rows of a fit with na.omit numbered again,
  # and the tibble in fit order of a fit to named data, with "Missing rows"
  # (B14). They give the value of the data frame of the fit.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  renumbered <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  rownames(renumbered) <- NULL
  named <- d
  rownames(named) <- paste0("obs", seq_len(nrow(d)))
  cases <- list(
    list(m = lm(log(ypos) ~ log(x1) + x2, data = d), data = NULL, reference = d),
    list(m = lm(y ~ x1 + x2 + offset(o), data = d), data = NULL, reference = d),
    list(m = lm(y ~ x1 + x2, data = d_na), data = renumbered, reference = d_na)
  )
  if (requireNamespace("dplyr", quietly = TRUE)) {
    cases <- c(cases, list(list(m = lm(y ~ x1 + x2, data = named),
                                data = dplyr::as_tibble(named), reference = named)))
  }
  recipes <- c(context_recipes(), context_streaming_recipes())
  for (k in seq_along(cases)) {
    m <- cases[[k]]$m
    for (fn in c(context_streaming, context_streaming_chunked)) {
      info <- paste(fn, k)
      expected <- context_run(fn, list(build = function() list(m = m, data = cases[[k]]$reference)),
                              recipes)
      context_expect_value(expected, info)
      context_same(context_run(fn, list(build = function() list(m = m, data = cases[[k]]$data)),
                               recipes),
                   expected, info)
    }
  }
})

test_that("the runner keeps the refusals of the streaming tests final", {
  # The runner runs the streaming tests when `chunk_threshold_mb` asks for
  # them. A fit edited after it was fitted, which they tested before
  # increment 7 (White's streaming test 11.145, the value of the fit before
  # the edit), a multivariate lm, on which they stopped with a coercion error,
  # and a fit with model = FALSE whose data are gone, given its rows shuffled
  # as a tibble (8.887, a value of other rows), are refused by the context,
  # and the runner stops with the refusal, its class kept, without a fallback
  # under the name of the test (decision 6).
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  run <- function(m, data, chunk_size = 7) {
    suppressWarnings(runHeteroTests(m, data, tests = c("white", "breusch_pagan", "koenker"),
                                    use_cache = FALSE, chunk_threshold_mb = 0,
                                    chunk_size = chunk_size, progress = FALSE))
  }
  edited <- lm(y ~ x1 + x2, data = d)
  edited$coefficients[["x1"]] <- edited$coefficients[["x1"]] + 0.5
  expect_error(run(edited, d), "does not reproduce its residuals",
               class = "heteroTests_model_context_error")
  for (fn in context_streaming) {
    expect_error(suppressWarnings(get(fn)(edited, d, progress = FALSE)),
                 "does not reproduce its residuals", class = "heteroTests_model_context_error")
  }
  expect_error(run(lm(cbind(y, ypos) ~ x1 + x2, data = d), d),
               "performWhiteTestStreaming() needs a model with one response; `model` has 2.",
               fixed = TRUE, class = "heteroTests_model_context_error")
  skip_if_not_installed("dplyr")
  env <- new.env()
  assign("gone", d, envir = env)
  m_false <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  set.seed(3)
  shuffled <- dplyr::as_tibble(d)[sample(nrow(d)), ]
  expect_error(run(m_false, shuffled), "cannot use the model frame of `model`",
               class = "heteroTests_model_context_error")
  # Given its data, it is tested on them, streamed: the values of the fit.
  streamed <- run(m_false, d)
  expected <- run(lm(y ~ x1 + x2, data = d), d)
  for (nm in names(expected)) {
    expect_true(attr(streamed[[nm]], "extras")$streaming, info = nm)
    context_same(streamed[[nm]], expected[[nm]], nm)
  }
  # On a term computed from its rows, the runner's streaming tests give the
  # values of the fit in every chunk (B6). White's streams too since the pull
  # request of N3 (0.17.0), dropping the collinear square of the indicator;
  # in 0.16.0 it stopped on it, and the runner fell back on White's test
  # without cross products.
  m <- context_model(d, "ols", "none", "median")
  chunked <- run(m, d)
  for (nm in c("white", "breusch_pagan", "koenker")) {
    expect_true(attr(chunked[[nm]], "extras")$streaming, info = nm)
    expect_null(attr(chunked[[nm]], "extras")$status)
    context_same(chunked[[nm]], run(m, d, chunk_size = 10000)[[nm]], nm, tolerance = 1e-10)
  }
})

test_that("a gam with smooth terms is streamed as before, on the variables of its formula", {
  # Its stored design is the basis of its smooths, on which the exact tests
  # test it (decision 23). The streaming tests built the design of each chunk
  # from its terms, those of the variables of its formula, and keep that
  # design, now from its model frame, until the last step of the model
  # context settles which tests take such a fit: Breusch-Pagan's and Koenker's
  # statistics of its residuals on x1 and x2, written out here.
  skip_if_not_installed("mgcv")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  smooth <- mgcv::gam(y ~ s(x1) + x2, data = d)
  e2 <- unname(stats::residuals(smooth))^2
  X <- cbind(d$x1, d$x2)
  g <- e2 / mean(e2) - 1
  bp <- 0.5 * sum(stats::fitted(stats::lm(g ~ X))^2)
  koenker <- length(e2) * summary(stats::lm(e2 ~ X))$r.squared
  for (chunk in c(7, 10000)) {
    out_bp <- performBPTestStreaming(smooth, d, chunk_size = chunk, progress = FALSE)
    out_k <- performKoenkerTestStreaming(smooth, NULL, chunk_size = chunk, progress = FALSE)
    expect_equal(unname(out_bp$statistic), bp, tolerance = 1e-8)
    expect_equal(unname(out_k$statistic), koenker, tolerance = 1e-8)
    expect_equal(unname(out_bp$parameter), 2)
    expect_equal(unname(out_k$parameter), 2)
  }
})

test_that("a model = FALSE fit to named data says why numbered data cannot rebuild its frame", {
  # The frame of a fit with model = FALSE whose data are gone is rebuilt from
  # `data`, whose rows are matched by the names of the observations of the
  # fit, which a tibble of such data, numbering its rows, lacks. The tests
  # built on the context said that `data` does not give the response and the
  # fitted values of the fit, and the streaming tests before increment 7
  # "Missing rows: Mazda RX4, Mazda RX4 Wag, Datsun 710"; the refusal names
  # the rows `data` lacks.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  env <- new.env()
  assign("gone", mtcars, envir = env)
  m <- eval(quote(lm(mpg ~ wt + qsec, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  unmatched <- paste(
    "the rows of `data` cannot be matched to the observations of the fit: `data` has no rows",
    "named Mazda RX4, Mazda RX4 Wag, Datsun 710."
  )
  for (fn in c(context_streaming, "performWhiteTest", "performKoenkerTest")) {
    test <- match.fun(fn)
    expect_s3_class(suppressWarnings(test(m, mtcars)), "htest")
    expect_error(suppressWarnings(test(m, dplyr::as_tibble(mtcars))), unmatched, fixed = TRUE,
                 class = "heteroTests_model_context_error", info = fn)
  }
  # Data with the names of the fit that do not give its response keep the
  # message on the response and the fitted values.
  reversed <- mtcars
  reversed$mpg <- rev(reversed$mpg)
  expect_error(performKoenkerTest(m, reversed),
               "`data` does not give the response and the fitted values of the fit", fixed = TRUE)
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
    # 0.800386 (p = 0.2117) before 0.16.0, ordered by X b alone. Ordered by
    # X b + o it was 0.962844 with the null moments (n + 1) / 2 and
    # (n^2 - 1) / (6 n); since 0.16.0 they are those of the design in that
    # order (rszroeter_moments()), which gives 0.992131 (p = 0.1606).
    expect_equal(unname(on_gam$statistic), unname(on_lm$statistic), tolerance = 1e-10)
    expect_equal(unname(on_gam$statistic), 0.992131, tolerance = 1e-6)
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

test_that("a fit with a zero weight is refused on every route of increments 3 to 5", {
  # The tests refused it before 0.16.0 by the Pearson residuals of the fit,
  # and still do, with data, without and with the rows shuffled; Hartley's
  # test, the five group tests and the panel tests without `data` lack their
  # groups or identifiers first, and the scatter diagnostic and runPanelTests()
  # need `data` (the matrix checks it).
  zero <- context_data()
  zero$w0 <- zero$w
  zero$w0[1] <- 0
  set.seed(3)
  shuffled <- zero[sample(nrow(zero)), ]
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  szroeter <- get("szroeter", envir = heteroTests:::.diagnostic_registry)
  recipes <- context_recipes()
  for (m in list(lm(y ~ x1 + x2, data = zero, weights = w0),
                 glm(y ~ x1 + x2, data = zero, weights = w0))) {
    for (data in list(zero, NULL, shuffled)) {
      fns <- c(context_increment3, context_increment4, context_increment5)
      for (fn in context_runnable(fns)) {
        if (is.null(data) && fn %in% context_data_readers()) next
        expect_error(suppressWarnings(recipes[[fn]](m, data)),
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
  # 400 calls, among them fitWLS() fits: with the full matrix.
  skip_unless_full_matrix()
  d <- context_data()
  shifted <- d
  shifted$y <- d$y - d$o
  # RESET is the nested F test of the model with its offset (below), and the
  # quantile regression test refuses a weighted fit. The tests computed from
  # the fitted values, Spearman's, Cook-Weisberg's, the NCV, spread-level and
  # Davidian-Carroll tests, read X b + o on a fit with an offset, and X b on
  # the fit of y - o: another test. The variance form test refuses weights that
  # did not come from fitWLS().
  fitted_readers <- c("performSpearmanTest", "performCookWeisbergTest", "performNCVTest",
                      "performSpreadLevelTest", "performDavidianCarrollTest")
  for (fn in setdiff(context_runnable(), c("performRESETTest", fitted_readers))) {
    fits <- if (identical(fn, "performQuantileRegressionTest")) {
      "ols"
    } else if (identical(fn, "performVarianceFormTest")) {
      c("ols", "fitwls")
    } else {
      context_fits
    }
    # Hartley's test and the group tests group by a variable the model frame
    # does not hold, the panel tests read identifiers it does not hold, and
    # the scatter diagnostic and runPanelTests() need `data`.
    forms <- if (fn %in% context_data_readers()) "data_frame" else c("data_frame", "null")
    # The fit of y - o is another fit of the model, whose rounding could leave
    # a tie of Fligner-Killeen's ranks at a group median exact where the fit
    # with the offset broke it before 0.16.0 (N15): measured on the Gaussian
    # glm, 1.5e-7 of the statistic on Linux and 1.9e-6 on macOS
    # (context_fligner_tolerance).
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
  # 600 expectations: with the full matrix.
  skip_unless_full_matrix()
  d <- context_data()
  precomputed <- context_precomputed()$formulas
  for (fn in context_runnable()) {
    # The quantile regression test and the variance form test refuse a
    # weighted fit.
    single <- fn %in% c("performQuantileRegressionTest", "performVarianceFormTest")
    fits <- if (single) "ols" else c("ols", "wls")
    for (fit in fits) {
      for (terms in names(precomputed)) {
        # cut() drops the row at the minimum of x1, and the panel of the fit
        # is unbalanced: Pesaran's test computes it from the periods each pair
        # of units shares (N12), on both fits.
        expected <- context_run(fn, list(build = function() context_precomputed_fit(fit, terms)))
        # The fit with the columns computed beforehand is another fit of the
        # model: before 0.16.0 Fligner-Killeen's ranks could tie at a group
        # median on one and not on the other (N15).
        tolerance <- if (identical(fn, "performFlignerKilleenTest")) {
          context_fligner_tolerance
        } else if (terms == "poly") {
          1e-8
        } else {
          1e-10
        }
        # Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe read x1,
        # which the model frame of these fits does not hold, Hartley's test
        # groups by a variable it does not hold either, and the panel tests
        # read identifiers it does not hold: without `data` they are refused
        # (the matrix checks it), and the scatter diagnostic and
        # runPanelTests() need `data`.
        readers <- c(names(context_reads), context_increment5)
        forms <- if (fn %in% readers) "data_frame" else c("data_frame", "null")
        # White's streaming test drops the collinear squares of these terms,
        # and of their columns computed beforehand, since the pull request of
        # N3 (0.17.0); in 0.16.0 it stopped on both.
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
  # The quantile regression test refuses a weighted fit, and the variance form
  # test weights that did not come from fitWLS().
  for (fn in setdiff(context_runnable(),
                     c("performQuantileRegressionTest", "performVarianceFormTest"))) {
    run <- function(m) context_run(fn, list(build = function() list(m = m, data = d)))
    wls <- run(context_model(d, "wls"))
    # Before 0.16.0 Fligner-Killeen's ranks tied at a group median on one fit
    # and not, by rounding, on the other (N15, context_fligner_tolerance).
    tolerance <- if (identical(fn, "performFlignerKilleenTest")) context_fligner_tolerance else 1e-8
    context_same(run(context_model(d, "glm")), wls, paste(fn, "glm"), tolerance = tolerance)
    context_same(run(context_model(d, "wvec")), wls, paste(fn, "wvec"))
    # The White and wild bootstraps estimate the weights of a fitWLS() fit
    # again in each replicate, which those of lm() are not.
    if (fn %in% c("performWhiteTestBootstrap", "performWildBootstrapTest")) next
    estimated <- context_model(d, "fitwls")
    context_same(run(estimated), run(lm(y ~ x1 + x2, data = d, weights = weights(estimated))),
                 paste(fn, "fitwls"))
  }
})

test_that("models the tests do not take are refused first, with today's messages", {
  d <- context_data()
  d$high <- as.integer(d$y > stats::median(d$y))
  binomial_fit <- glm(high ~ x1 + x2, data = d, family = binomial)
  recipes <- context_recipes()
  for (fn in context_runnable()) {
    recipe <- recipes[[fn]]
    expect_error(recipe(binomial_fit, d), "needs a model fitted by stats::lm()", fixed = TRUE,
                 info = fn)
    # runPanelTests() refuses a model through the first test it runs; the
    # deprecated performNCVTest() warns first.
    refused_by <- if (identical(fn, "runPanelTests")) "performBPRandomEffectsTest" else fn
    expect_error(suppressWarnings(recipe(lm(cbind(y, ypos) ~ x1 + x2, data = d), d)),
                 sprintf("%s() needs a model with one response; `model` has 2.", refused_by),
                 fixed = TRUE, info = fn)
  }
  skip_if_not_installed("survey")
  design <- survey::svydesign(ids = ~1, weights = ~w, data = d)
  survey_fit <- survey::svyglm(y ~ x1 + x2, design = design)
  for (fn in context_runnable()) {
    expect_error(recipes[[fn]](survey_fit, d), "The model is a survey::svyglm() fit",
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
    # With the tie at each group median exact (helper-fligner.R), as
    # performFlignerKilleenTest() computes it since 0.16.0.
    performFlignerKilleenTest = {
      t <- fligner_median_tied(r, g)
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

# The panel tests and the scatter diagnostic written out without the package,
# on the residuals `r` of a fit and the columns of the rows that hold its
# observations, in the same order: Pesaran's CD from the correlations of the
# residuals of each pair of units over the periods both are observed at, each
# centred at its mean over them, weighted by the square root of their number,
# over the pairs that share two periods or more (Pesaran 2004, Section 9,
# equation 31; on a balanced panel sqrt(2T / (N (N - 1))) times the sum of the
# correlations), the Breusch-Pagan LM statistic of Breusch and Pagan (1980),
# equation 5, with the mean number of periods, and Spearman's correlation of
# the absolute residuals with `x`.
context_panel_oracle <- function(fn, r, id, time = NULL, x = NULL) {
  r <- unname(r)
  switch(fn,
    performPesaranTest = {
      units <- unique(id)
      sum_terms <- 0
      pairs <- 0
      for (i in seq_len(length(units) - 1L)) {
        for (j in seq.int(i + 1L, length(units))) {
          both <- intersect(time[id == units[i]], time[id == units[j]])
          if (length(both) < 2L) next
          a <- r[id == units[i]][match(both, time[id == units[i]])]
          b <- r[id == units[j]][match(both, time[id == units[j]])]
          a <- a - mean(a)
          b <- b - mean(b)
          sum_terms <- sum_terms + sqrt(length(both)) * sum(a * b) / sqrt(sum(a^2) * sum(b^2))
          pairs <- pairs + 1
        }
      }
      cd <- sum_terms / sqrt(pairs)
      c(cd, 2 * stats::pnorm(abs(cd), lower.tail = FALSE))
    },
    performBPRandomEffectsTest = {
      sums <- tapply(r, id, sum)
      periods <- mean(tapply(r, id, length))
      lm_stat <- length(r) / (2 * (periods - 1)) * (sum(sums^2) / sum(r^2) - 1)^2
      c(lm_stat, stats::pchisq(lm_stat, 1, lower.tail = FALSE))
    },
    performScatterDiagnostic = stats::cor(abs(r), x, method = "spearman")
  )
}

test_that("the panel tests and the scatter diagnostic match data by name (B3, B4)", {
  # Before 0.16.0 they read the i-th row of `data` with the i-th residual:
  # data with row names of their own in another order gave other values, and
  # from 0.15.0 they were refused, as were rows after those of the fit. On the
  # design of the 0.15.0 release notes, the Pesaran statistic was -0.987 on
  # `d` and -0.392 on `d[sample(n), ]`, the random-effects statistic 1.19 and
  # 0.506, and the scatter diagnostic 0.468 and -0.080. They match the rows by
  # name, and give the values on `d`.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(1)
  n <- 120
  d <- data.frame(x = runif(n, 1, 5), z = rnorm(n))
  d$y <- 1 + d$x + d$z + rnorm(n, sd = d$x)
  m <- lm(y ~ x + z, d)
  d$id <- rep(1:12, each = 10)
  d$time <- rep(1:10, 12)
  set.seed(2)
  dp <- d[sample(n), ]
  end <- rbind(d, d[1:5, ])
  calls <- list(
    pesaran = function(data) performPesaranTest(m, data, "id", "time"),
    random_effects = function(data) performBPRandomEffectsTest(m, data, "id"),
    scatter = function(data) performScatterDiagnostic(m, data, c("x", "z")),
    runner = function(data) runPanelTests(m, data, "id", "time")
  )
  expected <- lapply(calls, function(f) f(d))
  expect_equal(unname(expected$pesaran$statistic), -0.986877, tolerance = 1e-6)
  expect_equal(unname(expected$random_effects$statistic), 1.18698, tolerance = 1e-5)
  expect_equal(unname(expected$scatter[["x"]]), 0.468074, tolerance = 1e-6)
  for (name in names(calls)) {
    context_same(calls[[name]](dp), expected[[name]], paste(name, "shuffled"))
    context_same(calls[[name]](end), expected[[name]], paste(name, "rows at the end"))
  }
  # Each is the test written out on the residuals and the rows of `d` that
  # carry their names, whatever the order of `dp`.
  r <- stats::residuals(m)
  rows <- dp[names(r), ]
  for (fn in c("performPesaranTest", "performBPRandomEffectsTest")) {
    out <- calls[[if (fn == "performPesaranTest") "pesaran" else "random_effects"]](dp)
    expect_equal(unname(c(out$statistic, out$p.value)),
                 context_panel_oracle(fn, r, rows$id, rows$time), tolerance = 1e-10, info = fn)
  }
  expect_equal(unname(calls$scatter(dp)[["x"]]),
               context_panel_oracle("performScatterDiagnostic", r, x = rows$x), tolerance = 1e-12)
})

test_that("the panel tests and the scatter diagnostic take the data of a fit that drops rows", {
  # B16.
  # On lm(y ~ x1 + x2) with x2 missing in three rows the random-effects test
  # and the scatter diagnostic refused the full data ("requires `data` with
  # 117 observations to match the fitted model, got 120"). They give the
  # values on the rows of the fit, those of the fit to the complete rows, on
  # every form of data that holds them.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  complete <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  m <- lm(y ~ x1 + x2, data = d_na)
  m_complete <- lm(y ~ x1 + x2, data = complete)
  set.seed(3)
  forms <- list(full = d_na, shuffled = d_na[sample(nrow(d_na)), ], tibble = dplyr::as_tibble(d_na))
  random_effects <- function(model, data) {
    suppressWarnings(performBPRandomEffectsTest(model, data, "id"))
  }
  scatter <- function(model, data) performScatterDiagnostic(model, data, c("x1", "x2"))
  expected_re <- random_effects(m_complete, complete)
  expect_equal(unname(expected_re$statistic), 2.01616, tolerance = 1e-5)
  for (form in names(forms)) {
    context_same(random_effects(m, forms[[form]]), expected_re, paste("random effects", form))
    context_same(scatter(m, forms[[form]]), scatter(m_complete, complete), paste("scatter", form))
  }
  # The units 1 and 2 lose two and one of their ten periods, and the panel is
  # unbalanced: Pesaran's test stopped on the complete rows with "number of
  # items to replace is not a multiple of replacement length", refused the
  # full data for their number of rows, and from the fifth step of the model
  # context refused the panel. Since the pull request of N12 (the test below)
  # it gives the statistic computed from the periods each pair of units
  # shares, that of the fit to the complete rows, on every form of data that
  # holds them.
  expected_cd <- performPesaranTest(m_complete, complete, "id", "time")
  expect_equal(unname(expected_cd$statistic), 0.189088, tolerance = 1e-5)
  for (form in names(forms)) {
    context_same(performPesaranTest(m, forms[[form]], "id", "time"), expected_cd,
                 paste("Pesaran", form))
  }
  r_na <- stats::residuals(m)
  rows_na <- d_na[names(r_na), ]
  expect_equal(unname(c(expected_cd$statistic, expected_cd$p.value)),
               context_panel_oracle("performPesaranTest", r_na, rows_na$id, rows_na$time),
               tolerance = 1e-10)
  # A fit that drops every row of a unit keeps a balanced panel, on which
  # Pesaran's test gives the value of the fit to the complete rows.
  d_unit <- d
  d_unit$x2[111:120] <- NA
  m_unit <- lm(y ~ x1 + x2, data = d_unit)
  expected <- performPesaranTest(lm(y ~ x1 + x2, data = d_unit[1:110, ]), d_unit[1:110, ],
                                 "id", "time")
  set.seed(3)
  for (data in list(d_unit, d_unit[sample(nrow(d_unit)), ], dplyr::as_tibble(d_unit))) {
    context_same(performPesaranTest(m_unit, data, "id", "time"), expected,
                 "Pesaran, a unit dropped")
  }
  r <- stats::residuals(m_unit)
  rows <- d_unit[names(r), ]
  expect_equal(unname(c(expected$statistic, expected$p.value)),
               context_panel_oracle("performPesaranTest", r, rows$id, rows$time), tolerance = 1e-10)
  expect_error(performPesaranTest(m_unit, d_unit[1:100, ], "id", "time"),
               paste("Pesaran CD test requires `data` to contain the rows used to fit the model.",
                     "Missing rows: 101"),
               fixed = TRUE)
})

test_that("Pesaran's test computes an unbalanced panel from the periods each pair shares (N12)", {
  # The test filled a matrix of periods by units, and a unit with fewer
  # periods than the panel stopped it, or, when that number divided the
  # number of periods, had its residuals recycled over the periods it lacks:
  # without the first five rows of contract_data(), unit 1 keeps 5 of its 10
  # periods, and 0.15.0 returned 0.234 (p = 0.815). The fifth step of the
  # model context refused such a panel (decided by the author on 2026-10-24:
  # refuse now, fix later). Since the pull request of N12 the statistic is
  # computed from the periods each pair of units shares, as
  # plm::pcdtest(model = "pooling") computes it (Pesaran 2004, Section 9,
  # equation 31): -0.373 (p = 0.709).
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  # The recycling case: a fit to the rows of such a panel, and the full data
  # of a fit that dropped those rows, shuffled or not.
  half <- d[-(1:5), ]
  d_half <- d
  d_half$x2[1:5] <- NA
  m_half <- lm(y ~ x1 + x2, data = half)
  expected <- performPesaranTest(m_half, half, "id", "time")
  expect_equal(unname(expected$statistic), -0.372652, tolerance = 1e-6)
  expect_equal(expected$p.value, 0.709408, tolerance = 1e-6)
  r <- stats::residuals(m_half)
  expect_equal(unname(c(expected$statistic, expected$p.value)),
               context_panel_oracle("performPesaranTest", r, half$id, half$time),
               tolerance = 1e-10)
  set.seed(3)
  calls <- list(
    list(m = m_half, data = half),
    list(m = lm(y ~ x1 + x2, data = d_half), data = d_half),
    list(m = lm(y ~ x1 + x2, data = d_half), data = d_half[sample(nrow(d_half)), ])
  )
  for (call in calls) {
    context_same(performPesaranTest(call$m, call$data, "id", "time"), expected, "Pesaran")
    # The runner gives the two tests, the random-effects test with its warning
    # on unequal panel lengths, where it stopped on the refusal of Pesaran's.
    expect_warning(both <- runPanelTests(call$m, call$data, "id", "time"),
                   "Unequal panel lengths")
    context_same(both$pesaran, expected, "the runner")
    context_same(runPanelTests(call$m, call$data, "id", "time", tests = "pesaran")$pesaran,
                 expected, "the runner, Pesaran's test alone")
    expect_warning(re <- runPanelTests(call$m, call$data, "id", tests = "bp_random"),
                   "Unequal panel lengths")
    expect_identical(both$bp_random, re$bp_random)
  }
  # Without data, from the model frame of a model of the identifiers.
  ids_model <- lm(y ~ x1 + x2 + id + time, data = half)
  context_same(performPesaranTest(ids_model, NULL, "id", "time"),
               performPesaranTest(ids_model, half, "id", "time"), "the model frame")
  # The stopping case: units observed at periods that do not divide the
  # panel's, which stopped with "number of items to replace is not a multiple
  # of replacement length"; with the names the call gives the columns.
  gaps <- d
  gaps$period <- gaps$time
  gaps <- gaps[-c(3, 14, 15), ]
  m_gaps <- lm(y ~ x1 + x2, data = gaps)
  out <- performPesaranTest(m_gaps, gaps, "id", "period")
  expect_equal(unname(c(out$statistic, out$p.value)),
               context_panel_oracle("performPesaranTest", stats::residuals(m_gaps), gaps$id,
                                    gaps$period),
               tolerance = 1e-10)
  # A unit whose rows are all dropped leaves the panel balanced, and the
  # statistic is that of the eleven others, computed as before.
  d_unit <- d
  d_unit$x2[111:120] <- NA
  eleven <- performPesaranTest(lm(y ~ x1 + x2, data = d_unit[1:110, ]), d_unit[1:110, ],
                               "id", "time")
  context_same(performPesaranTest(lm(y ~ x1 + x2, data = d_unit), d_unit, "id", "time"), eleven,
               "a unit dropped")
  context_same(runPanelTests(lm(y ~ x1 + x2, data = d_unit), d_unit, "id", "time")$pesaran, eleven,
               "a unit dropped, the runner")
  # A unit observed at one period shares at most one with any other: its
  # pairs have no correlation and are left out, with a warning, as plm leaves
  # them out; the statistic is computed on the other pairs.
  lone <- d[!(d$id == 3 & d$time > 1), ]
  m_lone <- lm(y ~ x1 + x2, data = lone)
  expect_warning(
    out <- performPesaranTest(m_lone, lone, "id", "time"),
    paste("performPesaranTest(): 11 of the 66 pairs of units share fewer than two periods and",
          "are left out of the statistic, as plm::pcdtest() leaves them out; it is computed on",
          "the other 55."),
    fixed = TRUE
  )
  expect_equal(unname(c(out$statistic, out$p.value)),
               context_panel_oracle("performPesaranTest", stats::residuals(m_lone), lone$id,
                                    lone$time),
               tolerance = 1e-10)
  # A panel in which no pair of units shares two periods has no correlation
  # to compute, and is refused.
  set.seed(5)
  apart <- data.frame(id = rep(1:6, 2), time = c(1, 1, 2, 2, 3, 3, 4:9), x = rnorm(12))
  apart$y <- apart$x + rnorm(12)
  expect_error(
    performPesaranTest(lm(y ~ x, data = apart), apart, "id", "time"),
    paste("performPesaranTest() needs a pair of units observed together at two periods or",
          "more, and no pair of the 6 units of the fit shares more than one period."),
    fixed = TRUE, class = "heteroTests_model_context_error"
  )
  # A correlation that is not defined, the residuals of a unit constant over
  # the periods of a pair, adds nothing to the sum and stays in the count of
  # pairs, as on a balanced panel, with a warning: unit 2 is observed at
  # periods 1, 2 and 4, unit 3 at 1, 2 and 3, and the residuals of unit 2 at
  # the periods they share are both 3.
  wide <- c(1, -1, 2, 0.5, 3, 3, NA, 1, -2, 0, 4, NA)
  units <- rep(1:3, each = 4)
  periods <- rep(1:4, 3)
  seen <- !is.na(wide)
  expect_warning(
    cd <- rpesaran_cd_unbalanced(wide[seen], units[seen], periods[seen], "f"),
    "in 1 of the 3 pairs of the statistic, whose correlation is not defined", fixed = TRUE
  )
  pair_12 <- stats::cor(c(1, -1, 0.5), c(3, 3, 1))
  pair_13 <- stats::cor(c(1, -1, 2), c(-2, 0, 4))
  # The pair of units 2 and 3 adds nothing to the sum, and counts among the
  # three pairs.
  expect_equal(cd, (sqrt(3) * pair_12 + sqrt(3) * pair_13) / sqrt(3), tolerance = 1e-12)
  # plm::pcdtest() calls plm() without its namespace, so plm is attached for
  # this block, as in test-panel-reference.R.
  skip_if_not_installed("plm")
  skip_if_not_installed("withr")
  suppressMessages(withr::local_package("plm"))
  for (panel in list(half, gaps, lone)) {
    pd <- plm::pdata.frame(panel, index = c("id", "time"))
    reference <- suppressWarnings(
      plm::pcdtest(y ~ x1 + x2, data = pd, test = "cd", model = "pooling")
    )
    out <- suppressWarnings(performPesaranTest(lm(y ~ x1 + x2, data = panel), panel, "id",
                                               "time"))
    expect_equal(unname(out$statistic), unname(reference$statistic), tolerance = 1e-10)
    expect_equal(out$p.value, reference$p.value, tolerance = 1e-10)
  }
})

test_that("Pesaran's test refuses a pair of id and time that repeats (R2)", {
  # Decided on 2026-10-18 (decision 13). The residuals are arranged in a
  # matrix of periods by units, and a pair that holds two observations put
  # one of them in the cell of another period: with the second observation of
  # unit 1 moved to period 5, the test returned 0.516 where the panel gives
  # 0.264, without an error. It is refused, with the class of the refusals of
  # the context; the random-effects test, which reads no period, is unchanged.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  panel <- d
  panel$time[2] <- 5L
  m <- lm(y ~ x1 + x2, data = panel)
  refusal <- paste0(
    "performPesaranTest() needs one observation of the fit for each pair of `id` and `time`, ",
    "and 1 pair holds more than one: id = 1 and time = 5 (2 observations)."
  )
  set.seed(3)
  for (data in list(panel, panel[sample(nrow(panel)), ])) {
    expect_error(performPesaranTest(m, data, "id", "time"), refusal, fixed = TRUE,
                 class = "heteroTests_model_context_error")
    expect_error(runPanelTests(m, data, "id", "time"), refusal, fixed = TRUE,
                 class = "heteroTests_model_context_error")
    expect_identical(performBPRandomEffectsTest(m, data, "id")$statistic,
                     performBPRandomEffectsTest(lm(y ~ x1 + x2, data = d), d, "id")$statistic)
    expect_identical(runPanelTests(m, data, "id", tests = "bp_random")$bp_random$statistic,
                     performBPRandomEffectsTest(m, d, "id")$statistic)
  }
  # More than one pair, and the names the call gives the columns.
  twice <- d
  twice$period <- twice$time
  twice$period[c(2, 12, 22, 32)] <- twice$period[c(1, 11, 21, 31)]
  m_twice <- lm(y ~ x1 + x2, data = twice)
  expect_error(
    performPesaranTest(m_twice, twice, "id", "period"),
    "and 4 pairs hold more than one: id = 1 and period = 1 (2 observations); id = 2 and period = 1",
    fixed = TRUE
  )
  expect_error(performPesaranTest(m_twice, twice, "id", "period"), "and 1 more", fixed = TRUE)
  # A fit with na.exclude, as its twin with na.omit (increment 9): the pair is
  # refused there too; from the review of increment 5, where the test
  # returned 0.253 on it, with the repeated pair.
  excluded <- panel
  excluded$x2[50] <- NA
  m_excluded <- lm(y ~ x1 + x2, data = excluded, na.action = na.exclude)
  expect_error(performPesaranTest(m_excluded, excluded, "id", "time"), refusal, fixed = TRUE,
               class = "heteroTests_model_context_error")
  # A unit or a period missing at an observation of the fit stopped the
  # statistic ("number of items to replace ..."); the refusal names it.
  gap <- d
  gap$time[5] <- NA
  expect_error(performPesaranTest(lm(y ~ x1 + x2, data = gap), gap, "id", "time"),
               "Missing rows: 5, where time, the `time` variable, is missing.", fixed = TRUE)
})

test_that("the refusals of Pesaran's test take linear time", {
  # From the review of increment 5: the refusal of repeated pairs counted each
  # with a scan of every observation, and took 18 to 53 s on 120,000
  # observations with 40,000 repeated pairs; one pass takes well under a
  # second. (The check of balance of the positional reading of a fit with
  # na.exclude, which scanned the observations once for each unit and then
  # took one pass too, went with that reading in increment 9.) The bounds are
  # generous, to hold on a slow machine.
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  set.seed(1)
  big <- data.frame(id = rep(1:4000, each = 30), time = rep(rep(1:10, each = 3), 4000))
  big$x <- rnorm(nrow(big))
  big$y <- big$x + rnorm(nrow(big))
  m <- lm(y ~ x, data = big)
  elapsed <- system.time(expect_error(
    performPesaranTest(m, big, "id", "time"),
    "and 40000 pairs hold more than one: id = 1 and time = 1 (3 observations);",
    fixed = TRUE, class = "heteroTests_model_context_error"
  ))[["elapsed"]]
  expect_lt(elapsed, 10)
  expect_lt(system.time(
    expect_error(rstop_repeated_panel_cells(big$id, big$time, "id", "time", "f"),
                 "and 39997 more", fixed = TRUE)
  )[["elapsed"]], 1)
})

test_that("the panel tests read their identifiers at rows told apart", {
  # The case of the review of increment 1: numbered data taken in the order of
  # the fit could exchange rows that tie on every compared column, and the
  # identifiers, which the model frame does not hold, would then belong to
  # other rows. Such data are refused; the scatter diagnostic, which reads
  # columns of the frame, takes them, and refuses them for a column beyond it.
  set.seed(42)
  n <- 60
  named <- data.frame(x = rep(1:3, 20))
  named$y <- round(named$x + rnorm(n))
  named$z <- runif(n, 1, 2)
  named$id <- rep(1:6, each = 10)
  named$time <- rep(1:10, 6)
  rownames(named) <- paste0("obs", seq_len(n))
  m <- lm(y ~ x, data = named)
  numbered <- named
  rownames(numbered) <- NULL
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  ties <- "tie on every column of the model frame that is compared"
  expect_s3_class(performPesaranTest(m, named, "id", "time"), "htest")
  expect_s3_class(performBPRandomEffectsTest(m, named, "id"), "htest")
  expect_error(performPesaranTest(m, numbered, "id", "time"), ties,
               class = "heteroTests_data_rows_error")
  expect_error(performBPRandomEffectsTest(m, numbered, "id"), ties,
               class = "heteroTests_data_rows_error")
  expect_error(runPanelTests(m, numbered, "id", "time"), ties,
               class = "heteroTests_data_rows_error")
  expect_identical(performScatterDiagnostic(m, numbered, "x"),
                   performScatterDiagnostic(m, named, "x"))
  expect_error(performScatterDiagnostic(m, numbered, "z"), ties,
               class = "heteroTests_data_rows_error")
})

test_that("the panel tests and the scatter diagnostic keep the refusals of the context", {
  # Without `data` the panel tests read `id` and `time` from the model frame,
  # when the model uses them under their own names (M1 otherwise); the
  # scatter diagnostic and runPanelTests() need `data`. An edited fit and a
  # fit with model = FALSE whose data are gone, given numbered data in another
  # order, are refused, by runPanelTests() too, with the class of the context;
  # they returned the values of other rows.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  with_ids <- lm(y ~ x1 + x2 + id + time, data = d)
  context_same(performPesaranTest(with_ids, NULL, "id", "time"),
               performPesaranTest(with_ids, d, "id", "time"), "the model frame")
  m <- lm(y ~ x1 + x2, data = d)
  expect_error(performPesaranTest(m, NULL, "id", "time"),
               "The following variables are missing from the model frame of `model`: id, time.",
               fixed = TRUE, class = "heteroTests_model_context_error")
  expect_error(performBPRandomEffectsTest(m, NULL, "id"),
               "The following variables are missing from the model frame of `model`: id.",
               fixed = TRUE, class = "heteroTests_model_context_error")
  expect_error(performPesaranTest(m, stats::model.frame(m), "id", "time"),
               "`id` and `time` must be columns in `data`.", fixed = TRUE)
  expect_error(performScatterDiagnostic(m, NULL, "x1"), "`data` must be a data frame.",
               fixed = TRUE)
  edited <- m
  edited$coefficients[["x1"]] <- edited$coefficients[["x1"]] + 0.5
  env <- new.env()
  assign("gone", d, envir = env)
  m_false <- eval(quote(lm(y ~ x1 + x2, data = gone, model = FALSE)), env)
  rm("gone", envir = env)
  set.seed(3)
  shuffled_tibble <- dplyr::as_tibble(d)[sample(nrow(d)), ]
  calls <- list(
    function(model, data) performPesaranTest(model, data, "id", "time"),
    function(model, data) performBPRandomEffectsTest(model, data, "id"),
    function(model, data) performScatterDiagnostic(model, data, c("x1", "x2")),
    function(model, data) runPanelTests(model, data, "id", "time")
  )
  for (f in calls) {
    expect_error(f(edited, d), "does not reproduce its residuals",
                 class = "heteroTests_model_context_error")
    expect_error(f(m_false, shuffled_tibble), "cannot use the model frame of `model`",
                 class = "heteroTests_model_context_error")
    expect_error(f(m, shuffled_tibble), "cannot match the rows of `data`",
                 class = "heteroTests_data_rows_error")
    context_same(f(m_false, d), f(m, d), "model = FALSE")
  }
})

# A call on a fit with na.exclude and on its twin, the same fit with na.omit:
# the value or the condition, the warnings and the state of the generator.
context_twin_run <- function(f) {
  set.seed(7)
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(f(), error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(value = value, warnings = warnings, rng = .Random.seed)
}

# A result without the model a runner attaches to it and to each test, whose
# na.action tells the two fits apart.
context_without_model <- function(x) {
  attr(x, "model") <- NULL
  if (is.list(x) && !inherits(x, "htest")) {
    for (k in seq_along(x)) {
      if (!is.null(x[[k]])) attr(x[[k]], "model") <- NULL
    }
  }
  x
}

# The fit of the matrix with na.exclude, x2 missing at rows 3, 7 and 11, and
# its twin with na.omit (missing/na_exclude and missing/full).
context_excluded_pair <- function(data = NULL) {
  d <- if (is.null(data)) {
    d <- context_data()
    d$x2[c(3, 7, 11)] <- NA
    d
  } else {
    data
  }
  list(
    d = d,
    excluded = lm(y ~ x1 + x2, data = d, na.action = na.exclude),
    omitted = lm(y ~ x1 + x2, data = d)
  )
}

test_that("a fit with na.exclude gives what the same fit with na.omit gives (B13)", {
  # Increment 9. A fit with na.action = na.exclude drops the rows na.omit drops
  # and pads residuals(), fitted(), weights() and hatvalues() with NA at them
  # (naresid()). The checks of the model read those residuals and refused it
  # ("Model residuals must be finite"), and so every test refused it, but
  # those below; RESET stopped on its fitted values ("NA/NaN/Inf in 'x'"). The
  # checks and the tests read one value per observation of the fit, as for
  # na.omit: every function built on the context gives the value of the same
  # fit with na.omit, with the same warnings, "Removed 3 observations due to
  # missing values in x2" among them, and, for the resampling tests, the same
  # draws of the generator. The spread-level and Davidian-Carroll tests, which
  # left out the NA themselves, gave it already.
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  recipes <- context_recipes()
  for (fn in c(context_runnable(), "performBreuschPaganTest")) {
    a <- context_twin_run(function() recipes[[fn]](pair$excluded, pair$d))
    b <- context_twin_run(function() recipes[[fn]](pair$omitted, pair$d))
    expect_false(inherits(b$value, "condition"), info = fn)
    context_same_as_twin(a$value, b$value, fn)
    expect_identical(a$warnings, b$warnings, info = fn)
    expect_identical(a$rng, b$rng, info = fn)
  }
  expect_error(performRESETTest(pair$excluded, pair$d), NA)
  expect_error(performKoenkerTest(pair$excluded), NA)
  expect_identical(performKoenkerTest(pair$excluded), performKoenkerTest(pair$omitted))
  # The checks of the model see the residuals of the fit: one per observation,
  # at the positions na.omit gives them.
  expect_error(rvalidateModelInputs(pair$excluded, "f"), NA)
  expect_length(rmodel_unpad(pair$excluded, residuals(pair$excluded)), 117L)
  expect_identical(rmodel_unpad(pair$excluded, residuals(pair$excluded)),
                   residuals(pair$omitted))
})

test_that("the resampling tests draw the replicates of the same fit with na.omit (B13)", {
  # Increment 9. The White and wild bootstraps, bootstrapTestStatistic() and
  # the robust bootstraps refused a fit with na.exclude; the White bootstrap
  # would have drawn its NA residuals, and bootstrapTestStatistic() its padded
  # fitted values. They draw what they draw on the same fit with na.omit, and
  # leave the generator where it leaves it.
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  calls <- list(
    white = function(m) performWhiteTestBootstrap(m, pair$d, B = 19),
    wild = function(m) performWildBootstrapTest(m, pair$d, B = 19, progress = FALSE),
    null = function(m) bootstrapTestStatistic(performKoenkerTest, m, pair$d, B = 19,
                                              progress = FALSE),
    pairs = function(m) bootstrapTestStatistic(performKoenkerTest, m, pair$d, B = 19,
                                               progress = FALSE, resample = "pairs"),
    robust = function(m) performBPTestRobust(m, pair$d, bootstrap = TRUE, B = 19),
    fitwls = function(m) performWhiteTestBootstrap(fitWLS(m, pair$d), pair$d, B = 19)
  )
  for (k in names(calls)) {
    a <- context_twin_run(function() calls[[k]](pair$excluded))
    b <- context_twin_run(function() calls[[k]](pair$omitted))
    expect_false(inherits(a$value, "condition"), info = k)
    a$value$call <- b$value$call <- NULL
    expect_identical(a$value, b$value, info = k)
    expect_identical(a$rng, b$rng, info = k)
  }
})

test_that("the runners give on a fit with na.exclude what they give with na.omit (B13)", {
  # Increment 9. runHeteroTests() stopped on the refusal of White's test
  # ("Diagnostic execution failed for White: Model residuals must be finite"),
  # and so did runDiagnostics(), runTimeSeriesTests(), runAdvancedDiagnostics()
  # and the helpers that run a test; runPanelTests() gave the statistics of the
  # positional reading (below). They give the results of the same fit with
  # na.omit, but for the attached model, whose na.action tells the two apart,
  # and the Cook's distances of runDiagnostics(), one per row, which
  # cooks.distance() pads.
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  calls <- list(
    runHeteroTests = function(m) runHeteroTests(m, pair$d, use_cache = FALSE, progress = FALSE),
    without_data = function(m) runHeteroTests(m, use_cache = FALSE, progress = FALSE),
    registered = function(m) {
      runHeteroTests(m, pair$d, tests = c("cook_weisberg", "spread_level", "szroeter"),
                     use_cache = FALSE, progress = FALSE)
    },
    streaming = function(m) {
      runHeteroTests(m, pair$d, tests = c("white", "breusch_pagan", "koenker"),
                     use_cache = FALSE, chunk_threshold_mb = 0, chunk_size = 7, progress = FALSE)
    },
    runTimeSeriesTests = function(m) runTimeSeriesTests(m, lags = 2),
    runPanelTests = function(m) runPanelTests(m, pair$d, "id", "time"),
    runAdvancedDiagnostics = function(m) runAdvancedDiagnostics(m, pair$d, bootstrap_B = 19)
  )
  for (k in names(calls)) {
    a <- context_twin_run(function() calls[[k]](pair$excluded))
    b <- context_twin_run(function() calls[[k]](pair$omitted))
    expect_false(inherits(a$value, "condition"),
                 info = paste(k, if (inherits(a$value, "condition")) conditionMessage(a$value)))
    expect_identical(context_without_model(a$value), context_without_model(b$value), info = k)
    expect_identical(a$warnings, b$warnings, info = k)
  }
  expect_match(context_twin_run(function() calls$runHeteroTests(pair$excluded))$warnings,
               "Removed 3 observations due to missing values in x2", fixed = TRUE, all = FALSE)
  a <- runDiagnostics(pair$excluded, pair$d, use_cache = FALSE, progress = FALSE)
  b <- runDiagnostics(pair$omitted, pair$d, use_cache = FALSE, progress = FALSE)
  expect_identical(a$influence$cooks_distance,
                   naresid(pair$excluded$na.action, b$influence$cooks_distance))
  a$influence <- b$influence <- NULL
  expect_identical(context_without_model(a), context_without_model(b))
})

test_that("the panel tests read a fit with na.exclude at the rows it kept (B13)", {
  # Increment 9. The panel tests and the scatter diagnostic read the padded
  # residuals of a fit with na.exclude against the rows of `data` by position,
  # as before 0.16.0 (decision 29): the random-effects statistic and the
  # correlations were NA, Pesaran's statistic was computed with the NA of the
  # rows the fit dropped, data in another order were refused, and so was an
  # unbalanced panel of the rows of `data`. They read the residuals of the
  # observations of the fit and the `id`, `time` and `vars` of the rows that
  # hold them, as for the same fit with na.omit: Pesaran's CD of the rows of
  # the fit, that of plm::pcdtest() (N12).
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  d <- pair$d
  m <- pair$excluded
  pesaran <- performPesaranTest(m, d, "id", "time")
  expect_identical(pesaran, performPesaranTest(pair$omitted, d, "id", "time"))
  expect_equal(unname(pesaran$statistic), 0.18909, tolerance = 1e-4)
  re <- suppressWarnings(performBPRandomEffectsTest(m, d, "id"))
  expect_false(is.na(re$statistic))
  expect_identical(re, suppressWarnings(performBPRandomEffectsTest(pair$omitted, d, "id")))
  expect_equal(unname(re$statistic), 2.0162, tolerance = 1e-4)
  scatter <- performScatterDiagnostic(m, d, c("x1", "x2"))
  expect_false(anyNA(scatter))
  expect_identical(scatter, performScatterDiagnostic(pair$omitted, d, c("x1", "x2")))
  # Data in another order, refused, give the value of the data in fit order.
  set.seed(3)
  shuffled <- d[sample(nrow(d)), ]
  expect_identical(suppressWarnings(performBPRandomEffectsTest(m, shuffled, "id")), re)
  expect_identical(performPesaranTest(m, shuffled, "id", "time"), pesaran)
  expect_identical(performScatterDiagnostic(m, shuffled, c("x1", "x2")), scatter)
  # An unbalanced panel of the rows of `data`, refused ("reads a fit with
  # na.action = na.exclude by position"), is computed from the periods each
  # pair of units shares, as for na.omit.
  half <- context_data()[-(1:5), ]
  half$x2[20] <- NA
  m_half <- lm(y ~ x1 + x2, data = half, na.action = na.exclude)
  expect_identical(performPesaranTest(m_half, half, "id", "time"),
                   performPesaranTest(update(m_half, na.action = na.omit), half, "id", "time"))
  skip_if_not_installed("plm")
  r <- residuals(pair$omitted)
  rows <- d[names(r), ]
  pd <- plm::pdata.frame(data.frame(id = rows$id, time = rows$time, e = unname(r)),
                         index = c("id", "time"))
  reference <- suppressWarnings(plm::pcdtest(pd$e, test = "cd"))
  expect_equal(unname(pesaran$statistic), unname(reference$statistic), tolerance = 1e-10)
  expect_equal(pesaran$p.value, reference$p.value, tolerance = 1e-10)
})

test_that("a gam with na.exclude is read as the same gam with na.omit (B13)", {
  # From the review of increment 9. model.matrix() of an mgcv::gam() fit
  # with na.exclude pads its design with NA rows at the rows the fit dropped,
  # 120 rows for 117 residuals, and the context refused every such fit
  # ("Internal error: the parts of the model context differ in length."),
  # where the same gam with na.omit gives White 10.847, Koenker 8.363 and
  # Pesaran 0.189. The design is read without the padded rows.
  skip_if_not_installed("mgcv")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  d$x2[c(3, 7, 11)] <- NA
  excluded <- mgcv::gam(y ~ x1 + x2, data = d, na.action = na.exclude)
  omitted <- mgcv::gam(y ~ x1 + x2, data = d)
  expect_equal(nrow(stats::model.matrix(excluded)), 120L)
  ctx <- rmodel_context(excluded, d, fn = "f")
  expect_identical(ctx$design, rmodel_context(omitted, d, fn = "f")$design)
  white_of <- function(m) performWhiteTest(m, d)
  white <- white_of(excluded)
  expect_identical(white, white_of(omitted))
  expect_equal(unname(white$statistic), 10.847, tolerance = 1e-4)
  expect_identical(performKoenkerTest(excluded, d), performKoenkerTest(omitted, d))
  expect_identical(performPesaranTest(excluded, d, "id", "time"),
                   performPesaranTest(omitted, d, "id", "time"))
  expect_identical(performScatterDiagnostic(excluded, d, c("x1", "x2")),
                   performScatterDiagnostic(omitted, d, c("x1", "x2")))
  expect_identical(
    context_without_model(runHeteroTests(excluded, d, use_cache = FALSE, progress = FALSE)),
    context_without_model(runHeteroTests(omitted, d, use_cache = FALSE, progress = FALSE))
  )
  # performVIFDiagnostic() reads the design too, and gave NA; the Cook's
  # distances of mgcv recycled the padded residuals against the leverages,
  # with a warning, and are those of the twin, padded.
  expect_identical(performVIFDiagnostic(excluded), performVIFDiagnostic(omitted))
  expect_no_warning(influence <- performInfluenceDiagnostics(excluded))
  expect_identical(influence$cooks_distance,
                   naresid(excluded$na.action, performInfluenceDiagnostics(omitted)$cooks_distance))
  # With a smooth term, the tests of increment 2 refuse it as its twin.
  smooth <- mgcv::gam(y ~ s(x1) + x2, data = d, na.action = na.exclude)
  expect_identical(performKoenkerTest(smooth, d),
                   performKoenkerTest(update(smooth, na.action = na.omit), d))
  expect_error(performRESETTest(smooth, d), "cannot test an mgcv::gam() fit with smooth terms",
               fixed = TRUE)
})

test_that("the functions outside the context read a fit with na.exclude as its twin (B13)", {
  # From the review of increment 9. Past the checks of the model, which
  # refused a fit with na.exclude, functions that are not built on the
  # context read its padded residuals and fitted values: the recommendations
  # stopped with "incompatible dimensions", the bubble plot, and so the plot
  # suite and the report, with "replacement has 120 rows, data has 117", the
  # other plots drew the NA with a warning of ggplot2 ("Removed 3 rows
  # containing non-finite values"), and the RMSE of autoCompareRemediations()
  # and analyzeMLResiduals() was NA. They give what they give on the same fit
  # with na.omit; augment(), whose rows are per observation, gives the rows of
  # the same fit with na.omit padded with NA rows at the rows the fit dropped.
  skip_on_cran()
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  me <- pair$excluded
  mo <- pair$omitted
  same <- function(f, info) {
    a <- suppressWarnings(f(me))
    b <- suppressWarnings(f(mo))
    expect_identical(a, b, info = info)
  }
  same(function(m) generateHeteroRecommendations(m)$profile, "recommendations")
  same(function(m) identifyVariancePatterns(m, stats::model.frame(m)), "patterns")
  same(function(m) autoCompareRemediations(m, pair$d)$metrics$RMSE, "RMSE")
  expect_false(anyNA(autoCompareRemediations(me, pair$d)$metrics$RMSE[1:2]))
  same(function(m) analyzeMLResiduals(m, pair$d)$rmse_reduction, "rmse_reduction")
  plots <- list(
    residuals_fitted = function(m) plotResidualsFitted(m),
    enhanced = function(m) plotResidualsFitted(m, enhanced = TRUE),
    spread_level = function(m) plotSpreadLevel(m),
    density = function(m) plotResidualDensity(m),
    qq = function(m) plotResidualQQ(m),
    bubble = function(m) plotBubbleVariance(m),
    before_after = function(m) plotBeforeAfter(m, fitWLS(m))
  )
  for (k in names(plots)) {
    a <- plots[[k]](me)
    b <- plots[[k]](mo)
    expect_identical(a$data, b$data, info = k)
    expect_no_warning(ggplot2::ggplot_build(a))
  }
  expect_identical(lapply(plotDiagnosticSuite(me), `[[`, "data"),
                   lapply(plotDiagnosticSuite(mo), `[[`, "data"))
  # augment(): the rows of the twin, padded with a row of NA at each row the
  # fit dropped, named as naresid() names them; given the data of the fit,
  # its rows, with the padded columns.
  a <- generics::augment(runHeteroTests(me, use_cache = FALSE, progress = FALSE)[["white"]])
  b <- generics::augment(runHeteroTests(mo, use_cache = FALSE, progress = FALSE)[["white"]])
  expect_identical(nrow(a), nrow(pair$d))
  expect_identical(names(a), names(b))
  expect_identical(lapply(a[-c(3, 7, 11), ], unname), lapply(b, unname))
  expect_true(all(is.na(a[c(3, 7, 11), setdiff(names(a), ".diagnostic")])))
  expect_identical(rownames(a), as.character(seq_len(nrow(pair$d))))
  white <- runHeteroTests(me, pair$d, use_cache = FALSE, progress = FALSE)[["white"]]
  full <- generics::augment(white, data = pair$d)
  expect_identical(unname(full$.fitted), unname(fitted(me)))
  expect_error(
    generics::augment(runHeteroTests(mo, pair$d, use_cache = FALSE, progress = FALSE)[["white"]],
                      data = pair$d),
    "augment() binds one row of `data` to each fitted value of the model, 117 of them", fixed = TRUE
  )
})

test_that("augment() and the remediation advice match the rows of data as the tests do", {
  # Increment 10, from the re-review of increment 9 and with N5.
  # recommendRemediationStrategies() correlates the residuals with the columns
  # of `data` row by row, generateHeteroRecommendations() profiles `data` and
  # hands it to the tests and to the advice, and augment() of the results of
  # the runner binds the columns of the fit to its rows. They read the i-th
  # row of `data` with the i-th observation: named data in another order than
  # the fit gave the advice of other rows, without an error, and augment()
  # bound the fitted values to other rows; since the re-review of increment 9
  # data of another number of rows, as the full data of a fit that dropped
  # rows, were refused, naming the two counts. They match the rows as the
  # tests do: named data in any order, numbered data in the order of the fit
  # and, for the advice, the full data of a fit that dropped rows give the
  # value of the rows of the fit in its order, on a fit with na.exclude as on
  # its twin with na.omit; what the alignment refuses is refused.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  d <- pair$d
  mo <- pair$omitted
  me <- pair$excluded
  rows <- d[names(mo$residuals), ]
  renumbered <- rows
  rownames(renumbered) <- NULL
  set.seed(3)
  shuffled <- d[sample(nrow(d)), ]
  rows_shuffled <- rows[sample(nrow(rows)), ]
  tr <- suppressWarnings(runHeteroTests(mo, d, use_cache = FALSE, progress = FALSE))
  advice <- recommendRemediationStrategies(tr, mo, rows)
  report <- suppressWarnings(generateHeteroRecommendations(mo, rows, include_report = FALSE))
  for (m in list(omit = mo, exclude = me)) {
    for (form in c("full", "shuffled", "rows_shuffled", "renumbered", "tibble")) {
      data <- switch(form,
        full = d, shuffled = shuffled, rows_shuffled = rows_shuffled, renumbered = renumbered,
        tibble = dplyr::as_tibble(d)
      )
      info <- paste(class(m$na.action), form)
      expect_identical(recommendRemediationStrategies(tr, m, data), advice, info = info)
      expect_identical(
        suppressWarnings(generateHeteroRecommendations(m, data, include_report = FALSE)), report,
        info = info
      )
    }
    # Numbered data in another order than the fit are refused by the guard
    # (decision 3), as by the tests.
    expect_error(recommendRemediationStrategies(tr, m, dplyr::as_tibble(d)[sample(nrow(d)), ]),
                 class = "heteroTests_data_rows_error")
    expect_error(
      suppressWarnings(generateHeteroRecommendations(m, dplyr::as_tibble(d)[sample(nrow(d)), ])),
      "generateHeteroRecommendations() cannot match the rows of `data`", fixed = TRUE
    )
  }
  # augment(): data with one row per observation, named in any order or
  # numbered in the order of the fit, bind the rows of the fit in its order;
  # on a fit with na.exclude those rows padded with a row of NA at each row the
  # fit dropped. Numbered data in another order are refused.
  white <- tr[["white"]]
  bound <- generics::augment(white, data = rows)
  expect_identical(generics::augment(white, data = rows_shuffled), bound)
  expect_identical(unname(bound$.fitted), unname(fitted(mo)))
  expect_identical(bound$y, rows$y)
  expect_error(generics::augment(white, data = dplyr::as_tibble(renumbered)[sample(117), ]),
               "augment() cannot match the rows of `data`", fixed = TRUE)
  excluded <- suppressWarnings(runHeteroTests(me, d, use_cache = FALSE, progress = FALSE))[["white"]]
  padded <- generics::augment(excluded, data = rows)
  expect_identical(generics::augment(excluded, data = rows_shuffled), padded)
  expect_identical(nrow(padded), nrow(d))
  expect_identical(padded$y[-c(3, 7, 11)], rows$y)
  # What augment() binds for the rows of `data` the fit dropped is open
  # (ROADMAP.md): the full data are refused on a fit with na.omit, and bound
  # row by row on a fit with na.exclude, as before.
  expect_error(generics::augment(white, data = d),
               "117 of them, and `data` has 120 rows", fixed = TRUE)
  expect_identical(unname(generics::augment(excluded, data = d)$.fitted), unname(fitted(me)))
  # Data whose number of rows is a multiple of the number of fitted values had
  # them recycled over their rows (the re-review of increment 9), and are
  # refused.
  full <- context_data()
  results <- runHeteroTests(lm(y ~ x1 + x2, data = full), full, use_cache = FALSE,
                            progress = FALSE)
  expect_error(generics::augment(results, data = rbind(full, full)),
               "120 of them, and `data` has 240 rows", fixed = TRUE)
  expect_identical(nrow(generics::augment(results, data = full)), length(results) * nrow(full))
})

test_that("a grouped run of a fitWLS() fit is refused, the others refit each group (B11)", {
  # Increment 10, decision 10. A grouped run refits the model within each
  # group by evaluating its call on the rows of the group. The call of a
  # fitWLS() fit is that of the model it refitted, without the weights it
  # estimated, so each group was tested as the unweighted fit of its rows (on
  # this matrix White's test gave 1.84 to 7.68 on the four groups of `group`,
  # the values of lm() on each); such a run is refused, with the class of the
  # refusals of the context, before any group is refitted. An lm, a weighted
  # lm, whose weights are a column of the data, and a formula refit each group,
  # as before; runDiagnostics(), which runs no grouped analysis, and the
  # ungrouped data test the fitWLS() fit as a whole.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_grouped_data()
  run <- function(m, data) {
    runHeteroTests(m, data, tests = c("white", "koenker"), use_cache = FALSE, progress = FALSE)
  }
  for (grouping in names(context_groupings)) {
    grouped <- context_grouped(d, grouping)
    group_rows <- dplyr::group_rows(grouped)
    for (fit in names(context_grouped_fits)) {
      m <- context_grouped_fits[[fit]](d)
      info <- paste(fit, grouping)
      if (identical(fit, "fitwls")) {
        refusal <- expect_error(run(m, grouped), class = "heteroTests_model_context_error",
                                info = info)
        expect_match(conditionMessage(refusal),
                     "cannot run a grouped analysis of a fitWLS() fit", fixed = TRUE, info = info)
        next
      }
      res <- run(m, grouped)
      expect_s3_class(res, "hetero_grouped_suite")
      expect_length(res, length(group_rows))
      for (g in seq_along(group_rows)) {
        part <- as.data.frame(d[group_rows[[g]], ])
        ref <- if (identical(fit, "wls")) {
          lm(y ~ x1 + x2, data = part, weights = w)
        } else {
          lm(y ~ x1 + x2, data = part)
        }
        expect_equal(unname(res[[g]]$white$statistic), unname(performWhiteTest(ref)$statistic),
                     tolerance = 1e-10, info = paste(info, g))
        expect_equal(unname(res[[g]]$koenker$statistic),
                     unname(performKoenkerTest(ref)$statistic), tolerance = 1e-10,
                     info = paste(info, g))
      }
    }
  }
  fw <- context_grouped_fits$fitwls(d)
  whole <- run(fw, d)
  expect_identical(run(fw, dplyr::ungroup(context_grouped(d, "one")))$white$statistic,
                   whole$white$statistic)
  expect_identical(
    runDiagnostics(fw, context_grouped(d, "one"), use_cache = FALSE, progress = FALSE)$white$statistic,
    whole$white$statistic
  )
})

test_that("the runners take the data a tidymodels workflow was fitted to (N5)", {
  # Increment 10. A workflow is tested on the fit of its engine, which names
  # its response `..y` and its predictors as the workflow's preprocessing made
  # them, and the runners refused the data the workflow was fitted to ("`data`
  # lacks ..y. Pass `data = NULL`"), as every test refused the data of a
  # parsnip fit made with fit_xy(). They map them to the variables of the
  # engine by the workflow's preprocessing (hardhat::forge()), or the outcome
  # of fit_xy() to `..y`, confirm the rows by the alignment of the context, and
  # test the model frame of the engine's fit: the value without `data`. Data
  # in another order than the fit are refused with the class of the guard,
  # and data that cannot be mapped with a message that says why: a recipe that
  # transforms the outcome (open), an outcome given to fit_xy() as a vector,
  # and data without the outcome.
  for (p in c("workflows", "parsnip", "recipes", "hardhat", "dplyr")) {
    skip_if_not_installed(p)
  }
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  numbers <- function(suite) {
    lapply(suite, function(r) list(r$method, r$statistic, r$parameter, r$p.value))
  }
  models <- context_tidy_models()
  for (name in names(models)) {
    model <- models[[name]]
    fit <- model$fit(model$data)
    reference <- runHeteroTests(fit, use_cache = FALSE, progress = FALSE)
    reference_diagnostics <- runDiagnostics(fit, use_cache = FALSE, progress = FALSE)
    for (form in setdiff(context_tidy_forms, "null")) {
      data <- context_tidy_data(model, form)
      expect <- context_tidy_expect(name, form)
      info <- paste(name, form)
      got <- tryCatch(runHeteroTests(fit, data, use_cache = FALSE, progress = FALSE),
                      error = function(e) e)
      if (identical(expect, "value")) {
        expect_identical(numbers(got), numbers(reference), info = info)
        if (!identical(name, "parsnip_formula")) {
          expect_identical(got, reference, info = info)
          expect_identical(runDiagnostics(fit, data, use_cache = FALSE, progress = FALSE),
                           reference_diagnostics, info = info)
        }
      } else if (identical(expect, "guard")) {
        expect_s3_class(got, "heteroTests_data_rows_error")
        expect_match(conditionMessage(got),
                     "A tidymodels workflow is tested on the fit of its engine", fixed = TRUE,
                     info = info)
        expect_match(conditionMessage(got), "Pass the data the workflow was fitted to",
                     fixed = TRUE, info = info)
      } else {
        expect_s3_class(got, "error")
        expect_match(conditionMessage(got), expect, fixed = TRUE, info = info)
      }
    }
  }
})

test_that("fitWLS() pads its outputs per row on a fit with na.exclude (B13)", {
  # Increment 9. The refit of fitWLS() keeps the na.action of the model, so
  # residuals(), fitted() and weights() of a fit with na.exclude are padded
  # with NA at the rows it dropped, as for any lm; its estimated variances,
  # the attribute "variance_model", had one value per observation of the fit,
  # and are padded the same way. The record of the variance function, which
  # the bootstraps read by observation, keeps one row per observation, that of
  # the same fit with na.omit.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  pair <- context_excluded_pair()
  for (formula in list(NULL, ~ z)) {
    a <- fitWLS(pair$excluded, pair$d, var_formula = formula)
    b <- fitWLS(pair$omitted, pair$d, var_formula = formula)
    omitted <- pair$excluded$na.action
    expect_length(attr(a, "variance_model"), nrow(pair$d))
    expect_identical(attr(a, "variance_model"), naresid(omitted, attr(b, "variance_model")))
    expect_true(all(is.na(attr(a, "variance_model")[c(3, 7, 11)])))
    expect_identical(weights(a), naresid(omitted, weights(b)))
    expect_identical(residuals(a), naresid(omitted, residuals(b)))
    expect_identical(coef(a), coef(b))
    expect_identical(attr(a, "variance_function"), attr(b, "variance_function"))
  }
  # Without na.exclude nothing is padded.
  b <- fitWLS(pair$omitted, pair$d)
  expect_identical(attr(b, "variance_model"), 1 / b$weights)
})

test_that("a second formula is read at the rows of the fit, not by position (B9)", {
  # Increment 6. fitWLS() and the variance form test matched the rows of
  # `data` to the fit by row name and, when no name matched and the counts
  # agreed, took them by position, unchecked: on a tibble in another order
  # than the fit they evaluated the variance formula on other rows and
  # returned another value without an error (the variance form test with
  # `var_formula = ~ z` 0.449 where the fit gives 0.00833, with
  # `against = ~ z` 0.316 for 0.840, and fitWLS() the slope of x1 1.196 for
  # 1.188). They read the variables at the rows the alignment confirmed, as
  # Cook-Weisberg's test with `var_formula` does: named data in any order,
  # numbered data in the order of the fit.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  m <- lm(y ~ x1 + x2, data = d)
  set.seed(3)
  perm <- sample(nrow(d))
  calls <- list(
    fitWLS = function(model, data) fitWLS(model, data, var_formula = ~ z),
    var_formula = function(model, data) performVarianceFormTest(model, data, var_formula = ~ z),
    against = function(model, data) performVarianceFormTest(model, data, against = ~ z),
    cook_weisberg = function(model, data) performCookWeisbergTest(model, data, var_formula = ~ z)
  )
  numbers <- function(x) {
    if (inherits(x, "lm")) c(coef(x), weights(x)) else c(unname(x$statistic), x$p.value)
  }
  expect_equal(unname(performVarianceFormTest(m, d, var_formula = ~ z)$statistic), 0.00832938,
               tolerance = 1e-5)
  expect_equal(unname(performVarianceFormTest(m, d, against = ~ z)$statistic), 0.839585,
               tolerance = 1e-6)
  expect_equal(unname(coef(fitWLS(m, d, var_formula = ~ z))[["x1"]]), 1.18803, tolerance = 1e-5)
  # The complete rows of a fit that dropped rows, numbered again.
  d_na <- d
  d_na$x2[c(3, 7, 11)] <- NA
  m_na <- lm(y ~ x1 + x2, data = d_na)
  complete <- d_na[stats::complete.cases(d_na[, c("y", "x1", "x2")]), ]
  renumbered <- complete
  rownames(renumbered) <- NULL
  reversed <- renumbered[rev(seq_len(nrow(renumbered))), ]
  rownames(reversed) <- NULL
  # A fit to repeated rows, whose tibble ties on every compared column: z is
  # beyond the model frame, and could belong to other rows.
  repeated <- d[c(seq_len(nrow(d)), 1:5), ]
  m_repeated <- lm(y ~ x1 + x2, data = repeated)
  for (nm in names(calls)) {
    f <- calls[[nm]]
    expected <- numbers(f(m, d))
    expect_identical(numbers(f(m, d[perm, ])), expected, info = nm)
    for (refused in list(dplyr::as_tibble(d)[perm, ], dplyr::arrange(d, x1), rbind(d[1:5, ], d))) {
      expect_error(f(m, refused), "cannot match the rows of `data`",
                   class = "heteroTests_data_rows_error", info = nm)
    }
    expect_identical(numbers(f(m_na, renumbered)), numbers(f(m_na, complete)), info = nm)
    expect_error(f(m_na, reversed), class = "heteroTests_data_rows_error", info = nm)
    expect_error(f(m_repeated, dplyr::as_tibble(repeated)),
                 "tie on every column of the model frame that is compared",
                 class = "heteroTests_data_rows_error", info = nm)
  }
})

test_that("a variable of a second formula is not read from the workspace (R3)", {
  # Decided on 2026-10-16 (decision 9). Before 0.16.0 a variable that neither
  # `data` nor the model frame held was found in the environment of the
  # formula, the workspace, whatever the order of its values: with
  # zz <- rev(d$z), Cook-Weisberg's test without `data` returned 5.521
  # (p = 0.0188), the variance form test 1.143 (p = 0.287) with
  # `var_formula = ~ zz` and 2.114 (p = 0.149) with `against = ~ zz`, and
  # fitWLS() its weights; with `data` that lack zz, all but Cook-Weisberg's
  # test did the same. Without `data` they are refused with M1, and with it
  # with the message of `data`, as Cook-Weisberg's test refused it.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  m <- lm(y ~ x1 + x2, data = d)
  zz <- rev(d$z)
  calls <- list(
    cook_weisberg = function(data) performCookWeisbergTest(m, data, var_formula = ~ zz),
    ncv = function(data) suppressWarnings(performNCVTest(m, data, var_formula = ~ zz)),
    fitWLS = function(data) fitWLS(m, data, var_formula = ~ zz),
    fitWLS_power = function(data) fitWLS(m, data, var_formula = ~ zz, form = "power"),
    var_formula = function(data) performVarianceFormTest(m, data, var_formula = ~ zz),
    against = function(data) performVarianceFormTest(m, data, against = ~ zz)
  )
  for (nm in names(calls)) {
    expect_error(calls[[nm]](NULL),
                 "The following variables are missing from the model frame of `model`: zz.",
                 fixed = TRUE, class = "heteroTests_model_context_error", info = nm)
    expect_error(calls[[nm]](NULL), "must be supplied through `data`", fixed = TRUE, info = nm)
    expect_error(calls[[nm]](NULL), "are not read from the workspace", fixed = TRUE, info = nm)
    expect_error(calls[[nm]](d), "The following variables are missing from input data: zz.",
                 fixed = TRUE, info = nm)
  }
  # Given as a column of `data`, it is read at the rows of the fit: the value
  # the workspace gave, whose values were in the order of the fit.
  dz <- d
  dz$zz <- zz
  expect_equal(unname(performCookWeisbergTest(m, dz, var_formula = ~ zz)$statistic), 5.521162,
               tolerance = 1e-6)
  expect_equal(unname(performVarianceFormTest(m, dz, var_formula = ~ zz)$statistic), 1.142589,
               tolerance = 1e-6)
  # A variable the model frame holds under its own name is read from it, and
  # the functions of the formula from its environment.
  expect_identical(performCookWeisbergTest(m, var_formula = ~ x1)$statistic,
                   performCookWeisbergTest(m, d, var_formula = ~ x1)$statistic)
  expect_identical(coef(fitWLS(m, var_formula = ~ log(x1) + I(x2^2))),
                   coef(fitWLS(m, d, var_formula = ~ log(x1) + I(x2^2))))
})

test_that("a constant of a second formula is evaluated as R evaluates it", {
  # From the review of increment 6: R3 and decision 9 concern variables, with
  # one value per observation and so an order; a value of length one, such as
  # `k` in `~ I(hp^k)` or `pi`, is a constant, found in the environment of the
  # formula as eval() finds it, as before 0.16.0 (rformula_constants()). The
  # Cook-Weisberg test gives car::ncvTest(m, ~ I(hp^2)), 0.5496 (p = 0.458),
  # and, given `data`, where it refused `k` as a variable `data` lacked. A
  # value of any other length is a variable, and is still refused.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  m <- lm(mpg ~ wt + hp, data = mtcars)
  k <- 2
  squared <- performCookWeisbergTest(m, var_formula = ~ I(hp^2))
  expect_equal(unname(squared$statistic), 0.549586699, tolerance = 1e-8)
  expect_identical(performCookWeisbergTest(m, var_formula = ~ I(hp^k))$statistic,
                   squared$statistic)
  expect_identical(performCookWeisbergTest(m, mtcars, var_formula = ~ I(hp^k))$statistic,
                   squared$statistic)
  expect_identical(performVarianceFormTest(m, var_formula = ~ I(hp^k))$statistic,
                   performVarianceFormTest(m, var_formula = ~ I(hp^2))$statistic)
  expect_identical(performVarianceFormTest(m, mtcars, against = ~ I(hp^k))$statistic,
                   performVarianceFormTest(m, mtcars, against = ~ I(hp^2))$statistic)
  expect_identical(coef(fitWLS(m, var_formula = ~ I(hp^k))),
                   coef(fitWLS(m, var_formula = ~ I(hp^2))))
  # pi, a constant of base R.
  expect_equal(unname(performCookWeisbergTest(m, var_formula = ~ hp + I(pi * hp))$statistic),
               1.02417975, tolerance = 1e-8)
  expect_equal(unname(coef(fitWLS(m, mtcars, var_formula = ~ I(pi * hp)))),
               c(36.785952, -3.792354, -0.030690295), tolerance = 1e-6)
  # The functions of the formula are not variables.
  expect_s3_class(performCookWeisbergTest(m, var_formula = ~ log(hp)), "htest")
  # A vector of the workspace, of length n or any other length above one, is
  # a variable: refused without `data` (M1) and with `data` that lack it.
  hp_rev <- rev(mtcars$hp)
  two <- c(1, 2)
  for (f in list(~ hp_rev, ~ I(hp * two))) {
    v <- all.vars(f)[length(all.vars(f))]
    expect_error(performCookWeisbergTest(m, var_formula = f),
                 sprintf("missing from the model frame of `model`: %s.", v), fixed = TRUE)
    expect_error(fitWLS(m, var_formula = f),
                 sprintf("missing from the model frame of `model`: %s.", v), fixed = TRUE)
    expect_error(performVarianceFormTest(m, mtcars, var_formula = f),
                 sprintf("missing from input data: %s.", v), fixed = TRUE)
  }
})

test_that("fitWLS() refuses a data that is not a data frame, as every test does", {
  # Decision 31. Without `var_formula` fitWLS() ignored `data`, and returned
  # the fit for a list or a matrix; it refuses them with the message the
  # tests give. With `var_formula` it keeps the message of 0.15.0.
  m <- lm(mpg ~ wt + hp, data = mtcars)
  for (bad in list(list(a = 1), as.matrix(mtcars))) {
    expect_error(fitWLS(m, bad), "`data` in fitWLS() must be a data frame or NULL.",
                 fixed = TRUE)
  }
  expect_error(fitWLS(m, list(a = 1), var_formula = ~ hp), "Input data must be a data.frame",
               fixed = TRUE)
})

test_that("the tests computed from the model alone refuse a stale model frame (N6)", {
  # Increment 6. A fit with model = FALSE rebuilds its model frame from the
  # data it was fitted on; reordered after the fit, the rebuilt frame no longer
  # holds the response of the fit. RESET read it until increment 2; the tests
  # computed from the model alone read it, or ignored it, and returned a
  # value: Harvey's test on the design of the reordered data, 3.022 where the
  # fit gives 11.984, the variance form test 1.289 for 1.845, and fitWLS()
  # refitted the reordered response (the slope of x1 1.009 for 0.869); the
  # others the value of the fit. They refuse it now, with or without `data`, as
  # every test built on the context does.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  env <- new.env()
  assign("changed", d, envir = env)
  m <- eval(quote(lm(y ~ x1 + x2, data = changed, model = FALSE)), env)
  recipes <- context_recipes()
  fns <- c(names(context_model_alone), context_second_formula)
  run <- function(fn, data) suppressWarnings(recipes[[fn]](m, data))
  fresh <- lapply(stats::setNames(fns, fns), function(fn) run(fn, d))
  expect_equal(unname(fresh$performHarveyTest$statistic), 11.98384, tolerance = 1e-6)
  expect_equal(unname(fresh$performVarianceFormTest$statistic), 1.844774, tolerance = 1e-6)
  assign("changed", d[order(d$x1), ], envir = env)
  stale <- "cannot use the model frame of `model`: the model was fitted with `model = FALSE`"
  for (fn in fns) {
    for (data in list(NULL, d)) {
      expect_error(run(fn, data), stale, fixed = TRUE, class = "heteroTests_data_rows_error",
                   info = fn)
    }
  }
  registry <- heteroTests:::.diagnostic_registry
  for (name in c("cook_weisberg", "ncv", "spread_level", "variance_form")) {
    expect_error(get(name, envir = registry)(m, NULL), stale, fixed = TRUE, info = name)
  }
  expect_error(runTimeSeriesTests(m, lags = 2), stale, fixed = TRUE)
  expect_error(
    suppressWarnings(runHeteroTests(m, tests = "cook_weisberg", use_cache = FALSE,
                                    progress = FALSE)),
    class = "heteroTests_data_rows_error"
  )
  # Restored, the frame holds the fit again, and the values are those of
  # before.
  assign("changed", d, envir = env)
  for (fn in fns) {
    context_same(run(fn, d), fresh[[fn]], fn)
  }
})

test_that("the tests that read nothing from data match it as every test does (decision 31)", {
  # Decided by the author on 2026-10-26: one rule for every test built on the
  # context. The tests computed from the model alone, RESET and fitWLS()
  # without `var_formula` read nothing from `data`, and until increment 6 kept
  # the check of 0.15.0, the variables of the model and the row names of the
  # fit. On mtcars with its row names reset they gave the value of the fit,
  # Spearman's test 1.445 (p = 0.159), on numbered data in another order, rows
  # in front of those of the fit and repeated row names, which the guard
  # refuses; and refused the complete rows of a fit that dropped rows,
  # numbered again ("Missing rows: 31, 32"), which the guard takes in the order
  # of the fit.
  skip_if_not_installed("dplyr")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- mtcars
  rownames(d) <- NULL
  m <- lm(mpg ~ wt + qsec, data = d)
  set.seed(1)
  perm <- sample(nrow(d))
  repeated <- rbind(d, d[1, ])
  attr(repeated, "row.names") <- c(seq_len(nrow(d)), 1L)
  refused <- list(
    tibble = dplyr::as_tibble(d)[perm, ], front = rbind(d[1:5, ], d), repeated = repeated
  )
  dn <- d
  dn$qsec[c(3, 7)] <- NA
  mn <- lm(mpg ~ wt + qsec, data = dn)
  renumbered <- dn[stats::complete.cases(dn[, c("mpg", "wt", "qsec")]), ]
  rownames(renumbered) <- NULL
  recipes <- context_recipes()
  numbers <- function(x) if (is.numeric(x)) x else c(unname(x$statistic), x$p.value)
  for (fn in c(names(context_model_alone), "fitWLS")) {
    run <- function(model, data) suppressWarnings(recipes[[fn]](model, data))
    expected <- numbers(run(m, d))
    for (nm in names(refused)) {
      expect_error(run(m, refused[[nm]]), "cannot match the rows of `data`",
                   class = "heteroTests_data_rows_error", info = paste(fn, nm))
    }
    expect_identical(numbers(run(m, d[perm, ])), expected, info = fn)
    expect_identical(numbers(run(mn, renumbered)), numbers(run(mn, dn)), info = fn)
  }
  expect_equal(unname(performSpearmanTest(m, d)$statistic), 1.445, tolerance = 1e-3)
  expect_equal(unname(performSpearmanTest(mn, renumbered)$statistic), 0.9509, tolerance = 1e-4)
  # The registered "ncv", a fallback of the runner, and the runner.
  expect_error(get("ncv", envir = heteroTests:::.diagnostic_registry)(m, refused$tibble),
               class = "heteroTests_data_rows_error")
  expect_error(
    suppressWarnings(runHeteroTests(m, refused$tibble, tests = "cook_weisberg",
                                    use_cache = FALSE, progress = FALSE)),
    class = "heteroTests_data_rows_error"
  )
})

test_that("a variable of var_formula missing at an observation of the fit is named", {
  # Cook-Weisberg's test with `var_formula` dropped the incomplete rows of
  # `data`, with the warning "Removed N observations due to missing values in
  # z", and then lacked a row of the fit. It still gives the warning and
  # refuses such a value, and names the variable (decision 8); a value
  # missing at a row the fit dropped is not read. fitWLS() and the variance
  # form test refuse it, as before, with their message.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  d$z[5] <- NA
  m <- lm(y ~ x1 + x2, data = d)
  expect_warning(
    expect_error(performCookWeisbergTest(m, d, var_formula = ~ z),
                 "Missing rows: 5, where z, the `var_formula` variable, is missing.", fixed = TRUE),
    "Removed 1 observations due to missing values in z", fixed = TRUE
  )
  expect_error(fitWLS(m, d, var_formula = ~ z),
               "`var_formula` produced missing or non-finite values on the fitted rows.",
               fixed = TRUE)
  expect_error(performVarianceFormTest(m, d, against = ~ z),
               "`against` produced missing or non-finite values on the fitted rows.", fixed = TRUE)
  dropped <- context_data()
  dropped$x2[c(3, 7, 11)] <- NA
  dropped$z[7] <- NA
  m_dropped <- lm(y ~ x1 + x2, data = dropped)
  expect_warning(out <- performCookWeisbergTest(m_dropped, dropped, var_formula = ~ z),
                 "Removed 1 observations due to missing values in z", fixed = TRUE)
  complete <- dropped[stats::complete.cases(dropped[, c("y", "x1", "x2")]), ]
  expect_identical(
    out$statistic,
    performCookWeisbergTest(lm(y ~ x1 + x2, data = complete), complete, var_formula = ~ z)$statistic
  )
})

# --- increment 8: the resampling tests ----------------------------------------

# The fit of a cell, and the cell's data, by id.
context_built <- function(id) {
  context_cells()[[id]]$build()
}

test_that("each White bootstrap replicate is the stored design refitted to the draws", {
  # The draws are those of before increment 8, in the same order: the
  # residuals resampled from the session's generator, one sample() per
  # replicate. On a plain design, where the stored design is the design
  # rebuilt from `data`, each replicate is the refit of the formula to the
  # fitted values plus the draws, which the White bootstrap refitted before,
  # and its statistic White's test on that refit.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  built <- context_built("fit/ols/none/none/data_frame")
  m <- built$m
  d <- built$data
  set.seed(31)
  boot <- performWhiteTestBootstrap(m, d, B = 5)
  after <- .Random.seed
  set.seed(31)
  by_hand <- vapply(seq_len(5), function(b) {
    refit_data <- d
    refit_data$y <- stats::fitted(m) + sample(stats::residuals(m), replace = TRUE)
    unname(performWhiteTest(lm(y ~ x1 + x2, data = refit_data), refit_data)$statistic)
  }, numeric(1))
  expect_identical(.Random.seed, after)
  expect_identical(boot$boot_statistics, by_hand)
  expect_identical(boot$statistic, performWhiteTest(m, d)$statistic)
  expect_false(exists("rwhite_test_rebuilt_design", envir = asNamespace("heteroTests"),
                      inherits = FALSE))
})

test_that("the resampling tests refit their replicates without data (B12)", {
  # The weights and the offset of these fits are columns of the data. Without
  # `data` the White bootstrap rebuilt the design from the model frame, which
  # holds them as "(weights)" and "(offset)", and stopped ("object 'w' not
  # found"), and the robust tests and the wild bootstrap stopped on a model
  # with an offset() term or a transformed variable ("The following variables
  # are missing from input data: o"); bootstrapTestStatistic(), given the model
  # frame, refused the transformed fit ("`data` does not reproduce the fit").
  # Each now gives the value it gives with `data`.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  runs <- list(
    white_bootstrap = function(m, d) performWhiteTestBootstrap(m, d, B = 9),
    wild = function(m, d) performWildBootstrapTest(m, d, B = 9, progress = FALSE),
    white_robust = function(m, d) performWhiteTestRobust(m, d, bootstrap = TRUE, B = 9),
    bp_robust = function(m, d) performBPTestRobust(m, d, bootstrap = TRUE, B = 9)
  )
  for (id in c("fit/wls/none/none", "fit/ols/argument/none", "fit/wls/term/none",
               "terms/ols/none/median", "terms/wls/none/ns")) {
    with_data <- context_built(paste0(id, "/data_frame"))
    without <- context_built(paste0(id, "/null"))
    frame <- context_built(paste0(id, "/model_frame"))
    for (route in names(runs)) {
      set.seed(5)
      expected <- runs[[route]](with_data$m, with_data$data)
      for (other in list(without, frame)) {
        set.seed(5)
        out <- runs[[route]](other$m, other$data)
        info <- paste(route, id)
        expect_identical(unname(out$statistic), unname(expected$statistic), info = info)
        expect_identical(out$p.value, expected$p.value, info = info)
        expect_identical(out$boot_statistics, expected$boot_statistics, info = info)
        expect_identical(out$bootstrap$replicates, expected$bootstrap$replicates, info = info)
        expect_identical(out$robust_details$bootstrap$replicates,
                         expected$robust_details$bootstrap$replicates, info = info)
      }
    }
    set.seed(5)
    expected <- bootstrapTestStatistic(performKoenkerTest, with_data$m, with_data$data, B = 9,
                                       progress = FALSE)
    set.seed(5)
    out <- bootstrapTestStatistic(performKoenkerTest, frame$m, frame$data, B = 9,
                                  progress = FALSE)
    expect_identical(out$replicates, expected$replicates, info = id)
    expect_identical(out$p_value, expected$p_value, info = id)
  }
})

test_that("a term computed from the rows keeps the values of the fit in every replicate", {
  # cut(x1, quantile(x1)) computed other breaks on the rows the fit kept: the
  # White bootstrap stopped ("factor ... has new levels") and
  # bootstrapTestStatistic() and the robust bootstraps refused the fit
  # ("`data` does not reproduce the fit"). On a fit that dropped rows,
  # I(x1 > median(x1)) took the median of the rows it kept: the White bootstrap
  # gave 11.039 for 10.336, and the bootstraps refused it. Each gives the value
  # of the same fit with the column computed beforehand.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  runs <- list(
    white_bootstrap = function(m, d) performWhiteTestBootstrap(m, d, B = 9),
    koenker = function(m, d) {
      bootstrapTestStatistic(performKoenkerTest, m, d, B = 9, progress = FALSE)
    },
    pairs = function(m, d) {
      bootstrapTestStatistic(performKoenkerTest, m, d, B = 9, resample = "pairs",
                             progress = FALSE)
    },
    white_robust = function(m, d) performWhiteTestRobust(m, d, bootstrap = TRUE, B = 9)
  )
  numbers <- function(x) {
    unlist(list(x$statistic, x$p.value, x$boot_statistics, x$original_statistic,
                x$replicates, x$p_value, x$robust_details$bootstrap$replicates))
  }
  for (fit in c("ols", "wls")) {
    built <- context_built(paste("terms", fit, "none", "cut", "data_frame", sep = "/"))
    reference <- context_precomputed_fit(fit, "cut")
    for (route in names(runs)) {
      set.seed(8)
      out <- runs[[route]](built$m, built$data)
      set.seed(8)
      expected <- runs[[route]](reference$m, reference$data)
      expect_equal(numbers(out), numbers(expected), tolerance = 1e-10, info = paste(route, fit))
    }
  }
  median_full <- context_built("missing/median_full")
  precomputed <- context_built("missing/median_precomputed")
  for (route in names(runs)) {
    set.seed(8)
    out <- runs[[route]](median_full$m, median_full$data)
    set.seed(8)
    expected <- runs[[route]](precomputed$m, precomputed$data)
    expect_equal(numbers(out), numbers(expected), tolerance = 1e-10, info = route)
  }
})

test_that("bootstrapTestStatistic() bootstraps a Gaussian glm as the lm it is (N8)", {
  # It refused every glm ("supports Gaussian `lm` models only"), as its refits
  # by least squares would drop a family and a link. A glm with the Gaussian
  # family and the identity link is the least-squares fit of lm() with its
  # prior weights, and its replicates are those of that lm.
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (offset in context_offsets) {
    glm_cell <- context_built(paste("fit/glm", offset, "none/data_frame", sep = "/"))
    lm_cell <- context_built(paste("fit/wls", offset, "none/data_frame", sep = "/"))
    for (test in list(performKoenkerTest, performWhiteTest, performBPTest)) {
      set.seed(4)
      ours <- bootstrapTestStatistic(test, glm_cell$m, glm_cell$data, B = 9, progress = FALSE)
      set.seed(4)
      as_lm <- bootstrapTestStatistic(test, lm_cell$m, lm_cell$data, B = 9, progress = FALSE)
      expect_equal(ours$replicates, as_lm$replicates, tolerance = 1e-8, info = offset)
      expect_equal(ours$original_statistic, as_lm$original_statistic, tolerance = 1e-8)
      expect_identical(ours$p_value, as_lm$p_value)
    }
    set.seed(4)
    robust <- performWhiteTestRobust(glm_cell$m, glm_cell$data, bootstrap = TRUE, B = 9)
    set.seed(4)
    robust_lm <- performWhiteTestRobust(lm_cell$m, lm_cell$data, bootstrap = TRUE, B = 9)
    expect_identical(robust$p.value_bootstrap, robust_lm$p.value_bootstrap)
  }
  # Another family or link is refused, as before, and so is an mgcv::gam() fit,
  # a glm by class, whose smooth terms are usually penalised.
  d <- context_data()
  set.seed(77)
  d$count <- stats::rpois(nrow(d), exp(0.2 + 0.3 * d$x1))
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, glm(count ~ x1 + x2, data = d, family = poisson),
                           d, B = 9, progress = FALSE),
    "supports Gaussian `lm` models only", fixed = TRUE
  )
  skip_if_not_installed("mgcv")
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, mgcv::gam(y ~ s(x1) + x2, data = d), d, B = 9,
                           progress = FALSE),
    "An mgcv::gam() fit is not taken", fixed = TRUE
  )
})

test_that("the White bootstrap matches a gam to its data by name (N17)", {
  # The residuals of a gam carry no names, and the White bootstrap took its
  # data by position: given its rows shuffled under their own row names it
  # added each draw to the fitted value of another row (p = 0.05 where the
  # data in the order of the fit give 0.2). It matches them by the row names
  # of the model frame.
  skip_if_not_installed("mgcv")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  for (id in c("class/gam_shuffled", "class/gam_offset_shuffled", "panel/unit_gam_shuffled")) {
    cell <- context_cells()[[id]]
    shuffled <- cell$build()
    in_order <- context_cells()[[cell$reference]]$build()
    set.seed(6)
    out <- performWhiteTestBootstrap(shuffled$m, shuffled$data, B = 19)
    set.seed(6)
    expected <- performWhiteTestBootstrap(in_order$m, in_order$data, B = 19)
    expect_identical(out$boot_statistics, expected$boot_statistics, info = id)
    expect_identical(out$p.value, expected$p.value, info = id)
  }
})

test_that("the resampling tests refuse an edited fit and a frame they cannot rebuild", {
  # New refusals: a fit whose coefficients were changed after the fit (M8), on
  # which the White bootstrap returned 11.145 and the wild bootstrap 9.044, the
  # values before the edit, and a fit with model = FALSE whose data are gone,
  # given a shuffled tibble, on which the White bootstrap returned the value of
  # other rows ("frame").
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  edited <- context_built("class/edited")
  for (call in list(
    function() performWhiteTestBootstrap(edited$m, edited$data, B = 9),
    function() performWildBootstrapTest(edited$m, edited$data, B = 9, progress = FALSE),
    function() performWhiteTestRobust(edited$m, edited$data, bootstrap = TRUE, B = 9)
  )) {
    expect_error(call(), "does not reproduce its residuals from its design and coefficients",
                 class = "heteroTests_model_context_error")
  }
  skip_if_not_installed("dplyr")
  gone <- context_built("class/model_false_tibble")
  expect_error(performWhiteTestBootstrap(gone$m, gone$data, B = 9),
               "cannot use the model frame of `model`",
               class = "heteroTests_model_context_error")
})

test_that("pairs resampling codes a factor a replicate leaves a level out of as a refit does", {
  # Two rows each of two levels: some replicates draw none of them. Each such
  # replicate is the refit of the model to the rows it drew, as before
  # increment 8, which drops the levels it did not draw and the contrasts
  # matrix of a factor that loses levels. Increment 8 took the rows of the
  # stored design with `[`, which drops its contrasts: a fit with
  # `contrasts = list(f = "contr.sum")` lost its replicates (8 of 40 left), a
  # character regressor kept a column of zeros for a value no row held (32 of
  # 40), and a factor whose contrasts attribute has a row for each of its four
  # levels stopped the whole call ("wrong number of contrast matrix rows").
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  set.seed(42)
  d$f <- factor(sample(c(rep("a", 58), rep("b", 58), "c", "c", "d", "d")))
  d$ch <- as.character(d$f)
  d$fs <- d$f
  stats::contrasts(d$fs) <- stats::contr.sum(4)
  fits <- list(
    factor = list(y ~ x1 + x2 + f, NULL),
    character = list(y ~ x1 + x2 + ch, NULL),
    contr_sum = list(y ~ x1 + x2 + f, list(f = "contr.sum")),
    attribute = list(y ~ x1 + x2 + fs, NULL)
  )
  n <- nrow(d)
  B <- 20L
  set.seed(21)
  draws <- lapply(seq_len(B), function(b) sample.int(n, replace = TRUE))
  expect_true(any(vapply(draws, function(i) nlevels(droplevels(d$f[i])) < 4L, logical(1))))
  for (name in names(fits)) {
    formula <- fits[[name]][[1L]]
    contrasts <- fits[[name]][[2L]]
    m <- lm(formula, data = d, contrasts = contrasts)
    set.seed(21)
    out <- bootstrapTestStatistic(performKoenkerTest, m, d, B = B, resample = "pairs",
                                  progress = FALSE)
    expected <- vapply(draws, function(i) {
      db <- d[i, , drop = FALSE]
      refit <- suppressWarnings(lm(formula, data = db, contrasts = contrasts))
      unname(performKoenkerTest(refit, db)$statistic)
    }, numeric(1))
    expect_identical(out$effective_samples, B, info = name)
    expect_equal(out$replicates, expected, tolerance = 1e-8, info = name)
  }
})

test_that("the resampling tests refuse a fitWLS() fit of a gam with smooth terms (M8)", {
  # fitWLS() of an mgcv::gam() fit with smooth terms returns an lm whose terms
  # rebuild one column for each variable of the formula, for the coefficients
  # of the basis of the smooths. The White and wild bootstraps returned a value
  # on that design, and Koenker's test and bootstrapTestStatistic() stopped on
  # "(subscript) logical subscript too long". The context refuses such a fit,
  # as it refuses a fit whose design does not reproduce its residuals, and
  # points to the gam, as refitting gives the same fit again.
  skip_if_not_installed("mgcv")
  old <- ht_set_log_level("SILENT")
  on.exit(ht_set_log_level(old), add = TRUE)
  d <- context_data()
  g <- suppressWarnings(fitWLS(mgcv::gam(y ~ s(x1) + x2, data = d), d))
  for (call in list(
    function() performWhiteTestBootstrap(g, d, B = 9),
    function() performWildBootstrapTest(g, d, B = 9, progress = FALSE),
    function() bootstrapTestStatistic(performKoenkerTest, g, d, B = 9, progress = FALSE),
    function() performKoenkerTest(g, d),
    function() performWhiteTest(g, d)
  )) {
    expect_error(call(), paste0(
      "does not reproduce its residuals from its design and coefficients: its terms rebuild a ",
      "design of 3 columns for its 11 coefficients, as those of fitWLS() of an mgcv::gam() fit ",
      "with smooth terms do, which refitting gives again. Test the gam itself, or give fitWLS() ",
      "a fit with a parametric term such as splines::ns() in place of each smooth."
    ), fixed = TRUE, class = "heteroTests_model_context_error")
  }
  # Another fit whose coefficients do not match its design keeps the hint of
  # every refusal of M8.
  m <- lm(y ~ x1 + x2, data = d)
  m$coefficients <- c(m$coefficients, extra = 1)
  expect_error(
    performKoenkerTest(m, d),
    "its terms rebuild a design of 3 columns for its 4 coefficients. Refit the model.",
    fixed = TRUE, class = "heteroTests_model_context_error"
  )
})

test_that("bootstrapTestStatistic() refuses a multivariate lm as every test does (M7)", {
  # It stopped on "'length = 4' in coercion to 'logical(1)'".
  d <- context_data()
  mlm <- lm(cbind(y, ypos) ~ x1 + x2, data = d)
  expect_error(
    bootstrapTestStatistic(performKoenkerTest, mlm, d, B = 9, progress = FALSE),
    "needs a model with one response; `model` has 2", fixed = TRUE,
    class = "heteroTests_model_context_error"
  )
  # Anything that is not a fit is left to the checks of the model, which name
  # its class, as before (the check of M7 read `model$residuals` first).
  for (bad in list("model", 1)) {
    expect_error(
      bootstrapTestStatistic(performKoenkerTest, bad, d, B = 9, progress = FALSE),
      sprintf(paste0("Provide an object fitted with stats::lm() or stats::glm() before running ",
                     "Bootstrap helper (received class: %s)"), class(bad)[1L]),
      fixed = TRUE
    )
  }
})
