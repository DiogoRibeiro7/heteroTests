chunk_progress_bar <- function(total, enabled = interactive(), label = "Processing chunks") {
  if (!enabled || total <= 1L) {
    update <- function(...) invisible(NULL)
    close_bar <- function(...) invisible(NULL)
    return(list(update = update, close = close_bar))
  }

  message(label)
  pb <- utils::txtProgressBar(min = 0, max = total, style = 3)
  list(
    update = function(value) {
      utils::setTxtProgressBar(pb, value)
      invisible(NULL)
    },
    close = function() {
      close(pb)
      invisible(NULL)
    }
  )
}

chunk_indices <- function(n, chunk_size) {
  if (!is.numeric(chunk_size) || length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1) {
    stop("`chunk_size` must be a positive integer.", call. = FALSE)
  }
  chunk_size <- max(1L, as.integer(chunk_size))
  split(seq_len(n), ((seq_len(n) - 1L) %/% chunk_size) + 1L)
}

# The design the streaming tests read, chunk by chunk, from the model context
# (R/model_context.R): the design of the fit without the columns a
# rank-deficient fit aliased (rcontext_design()), as the exact tests read it,
# one row per observation in the order of the fit. Up to 0.16.0 each chunk of
# rows of `data` built its own design from the formula, so a term computed
# from the rows it is given, such as I(x > median(x)) or cut(x, quantile(x)),
# took other values in each chunk than in the fit (B6 of the design of the
# model context). An mgcv::gam() fit with smooth terms, whose stored design is
# the basis of its smooths, is read on that basis too, since the last step of
# the model context (decision 37), as the exact tests read it; until then the
# streaming tests read the design of the variables of its formula, which its
# model frame holds, as before 0.16.0 (decision 23).
rstreaming_design <- function(ctx) {
  rcontext_design(ctx)
}

# The factor of `rows` stacked under `factor`, that of the rows before them,
# whose last column is the response: R of the QR decomposition of the stack by
# qr(), LINPACK's dqrdc2, at the tolerance rchunk_tolerance, without its rows
# past the rank, its columns put back in their order, and with one more row
# for the response when dqrdc2 moves it. dqrdc2 does not pivot, but moves to
# the end a column of which less than the tolerance of its norm is left after
# the columns before it, as of a column that is a combination of them. The
# rank is the number of columns it does not move, and the rows of R past the
# rank hold only what is left of the columns it moves: rounding error, for a
# column of the design that is an exact combination of others. Leaving those
# rows out changes the cross-products R'R of every row among the columns
# moved alone, by less than about the square of the tolerance relative to
# their norms, and not those of a column kept, which has nothing in them.
# What is left out adds up over the chunks, but stays far below the 1e-7 of
# its norm at which chunked_regression_r2() drops a column.
#
# What is left of the response is never left out. dqrdc2 compares a column
# with its own norm in the stack, for the response its norm about zero over
# the rows so far, of which the total sum of squares about its mean can be a
# small part: when the squared residuals vary little, less than the
# tolerance of that norm can be the whole of the residual sum of squares, and
# it adds up over the chunks. Leaving it out gave a residual sum of squares
# of 0, an exact fit for performWhiteTestStreaming(), in chunks of 7 on a
# design where one chunk gave more than sqrt(.Machine$double.eps) of the
# total (test-streaming.R). When dqrdc2 moves the response, the factor keeps
# what is left of it as one more row, its norm in the column of the response
# and 0 in the others: the norm of its effects past the rank after the
# reflections of the columns kept alone, as qr.qty() applies the first `rank`
# reflections only. dqrdc2 goes on to make reflections from the columns it
# moves (the loop of its reduction runs to min(n, p), whatever the rank), the
# response among them, and those can hold values that are not finite, which
# qr.qty() refuses, so they are set to 0 first.
#
# The rows kept are as many as the rank, or one more, which can be fewer than
# the columns, so the factor need not be square or triangular, and it does
# not need to be: each chunk decomposes it again, and chunked_regression_r2()
# reads it through qr(), which decides the rank. Where no column falls below
# the tolerance, none is moved and no row left out, and the factor is that of
# the decomposition without pivoting (`tol = 0`), to the last bit.
#
# In 0.17.0 the stack was decomposed without pivoting (`tol = 0`), which makes
# a reflection from every column, however little is left of it. The columns
# of an auxiliary design that are combinations of others, as the squares and
# products of dummies in White's design are, leave rounding error, from which
# a reflection is made; the next such column leaves about the square of that,
# and so on down. dqrdc2 divides a column by its norm, and when that norm is
# below the reciprocal of the largest double the result is not finite: the
# next chunk, or the qr() that reads the factor, then stopped on "NA/NaN/Inf
# in foreign function call (arg 1)". It did in White's streaming test of a
# one-way layout under the noisy BLAS of cran-blas.yml, and a multiple of a
# regressor of size 1e-300 does it on any BLAS (test-streaming.R). The
# reflections dqrdc2 still makes from the columns it moves, after the others,
# change only the rows past the rank, which are left out.
#
# The guarantee is relative. A column kept keeps at least the tolerance of its
# norm, so a column whose norm is below about 5.6e-296, 1e13 times the
# reciprocal of the largest double, can still leave less than that
# reciprocal, and its division overflow, as in 0.17.0: White's streaming test
# of lm(y ~ x1 + x2) with x1 = 1e-150 * u and x2 = 1e-150 * (u + 1e-6 * w),
# whose squares and product, of size 1e-300, are nearly collinear, stops on
# "NA/NaN/Inf in foreign function call", where performWhiteTest() gives
# 25.476 (an open item of 0.18.0 in ROADMAP.md).
rchunk_tolerance <- 1e-13

rchunk_factor <- function(factor, rows) {
  stack <- rbind(factor, rows)
  stacked <- qr(stack, tol = rchunk_tolerance)
  rank <- stacked$rank
  kept <- qr.R(stacked)[seq_len(rank), order(stacked$pivot), drop = FALSE]
  response <- ncol(stack)
  if (match(response, stacked$pivot) > rank && nrow(stack) > rank) {
    past <- seq.int(rank + 1L, response)
    stacked$qr[, past] <- 0
    stacked$qraux[past] <- 0
    left <- qr.qty(stacked, stack[, response])[-seq_len(rank)]
    size <- max(abs(left))
    if (size > 0) {
      kept <- rbind(kept, c(rep(0, response - 1L), size * sqrt(sum((left / size)^2))))
    }
  }
  kept
}

# The auxiliary regression of the streaming tests: the response the residuals
# give (their squares) on an intercept and the predictors the rows of the
# design give, chunk by chunk. The regressors, the columns of the design but
# its intercept, are centred at their means over every observation, as
# performWhiteTest() centres them, and the predictors are built from the
# centred regressors: with the intercept they span the same columns, and do
# not depend on where the regressors are located. The design is held whole
# (rstreaming_design()), so the means cost one pass over it; what is built
# chunk by chunk is the auxiliary design.
#
# Each chunk joins the factor of the chunks before it (rchunk_factor()): R of
# the QR decomposition of every row so far, with the response as its last
# column, from the QR decomposition of that factor stacked on the rows of the
# chunk, without the rows that hold only what is left of a column that is a
# combination of the columns before it, rounding error. R'R is the matrix of
# cross-products of every row, but for that rounding error, and the
# cross-products are never formed, so the condition of the auxiliary design
# is not squared. The rank of the auxiliary regression is that of the factor
# at the tolerance of qr(), 1e-7, decided by qr() in the order of the
# columns: a column is dropped when less than 1e-7 of its norm is not a
# combination of the columns kept before it. The factor has the norms and
# inner products of the columns of the intercept and the auxiliary design,
# so this is the rule performWhiteTest() applies to that design,
# `qr(cbind(1, centred))`, and the rank is at most the number of observations.
# The regression is that of the response on the columns kept: its explained
# and residual sums of squares are the squares of the effects of the response
# along and across them (qr.qty()), less the first, along the intercept,
# n times its squared mean.
#
# The squared residuals do not vary when their total sum of squares about
# their mean is at most `.Machine$double.eps` of their sum of squares about
# zero: when they agree to about eight digits. The total is compared with
# their scale, as the exact tests decide constancy: performKoenkerTest()
# refuses squared residuals that are all equal, a rule no scale enters, and
# the R-squared lm() gives the exact tests does not depend on the scale of
# the response. Squared residuals that are equal but for rounding, as those of
# residuals of +1 and -1, are refused at any scale, where the exact tests
# compute a statistic from the rounding.
#
# Up to 0.16.0 the cross-products of the uncentred auxiliary design were
# accumulated, the rank decided by qr() of them and the regression fitted by
# solve(). Collinear terms, the squares of dummies or of a poly() basis,
# stopped White's streaming test ("Auxiliary regression became rank
# deficient", N3 of the design of the model context) where performWhiteTest()
# drops them, and so did terms that looked collinear only in cross-products
# whose condition is the square of the design's: the squares of a regressor
# 100 standard deviations from zero in White's test, the regressor itself
# beside the intercept 1,000 standard deviations from zero in the
# Breusch-Pagan and Koenker tests. A refused regression was counted from the
# same cross-products, centred and scaled to correlations, by a pivoted
# Cholesky decomposition (#151). And the squared residuals did not vary when
# their total sum of squares was at most `.Machine$double.eps`, whatever their
# scale, which refused residuals of about 1e-5 that the exact tests take.
chunked_regression_r2 <- function(design, residuals, chunk_size,
                                  predictor_builder, response_builder,
                                  progress = FALSE, progress_label = "Processing chunks") {
  n <- length(residuals)
  if (n == 0L) {
    stop("No observations available after preprocessing.", call. = FALSE)
  }
  if (NROW(design) != n) {
    stop("Internal error: the design and the residuals differ in length.", call. = FALSE)
  }

  chunks <- chunk_indices(n, chunk_size)
  progress_bar <- chunk_progress_bar(length(chunks), progress, progress_label)
  on.exit(progress_bar$close(), add = TRUE)

  # The regressors, centred at their means over every observation.
  regressors <- seq_len(ncol(design))
  if (identical(colnames(design)[1L], "(Intercept)")) {
    regressors <- regressors[-1L]
  }
  centre <- colMeans(design[, regressors, drop = FALSE])

  factor <- NULL
  y_sum <- 0
  y_sq_sum <- 0
  total_n <- 0
  design_names <- NULL

  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    # The rows of the stored design (rstreaming_design()), not a design built
    # again from the rows of `data` in the chunk.
    mm_chunk <- design[idx, , drop = FALSE]
    mm_chunk[, regressors] <- mm_chunk[, regressors, drop = FALSE] -
      rep(centre, each = length(idx))

    predictors <- predictor_builder(mm_chunk)
    predictors <- as.matrix(predictors)

    if (ncol(predictors) == 0) {
      stop("Predictor builder produced an empty matrix.", call. = FALSE)
    }

    if (is.null(colnames(predictors))) {
      colnames(predictors) <- paste0("V", seq_len(ncol(predictors)))
    }

    # Every chunk has the columns of the stored design, so its predictors are
    # those of the first chunk, in their order. Up to 0.16.0 they were matched
    # to those of the first chunk by name, a remnant of the designs each chunk
    # built from the formula, which could lack the dummy of a level: White's
    # names collide on a regressor `a` beside a regressor `a_sq`, whose square
    # is named `a_sq` too, and every chunk after the first put the square of
    # `a` in place of that of `a_sq` (27.76 in chunks of 7 for 27.64).
    design_chunk <- cbind(`(Intercept)` = 1, predictors)
    if (is.null(design_names)) {
      design_names <- colnames(design_chunk)
    } else if (!identical(colnames(design_chunk), design_names)) {
      stop("Inconsistent design matrix dimensions between chunks.", call. = FALSE)
    }

    response <- response_builder(residuals[idx])
    response <- as.numeric(response)
    if (length(response) != nrow(design_chunk)) {
      stop("Response builder returned a vector of incorrect length.", call. = FALSE)
    }

    # The factor of every row so far, with the response as its last column.
    factor <- rchunk_factor(factor, cbind(design_chunk, response))

    y_sum <- y_sum + sum(response)
    y_sq_sum <- y_sq_sum + sum(response^2)
    total_n <- total_n + length(response)

    progress_bar$update(i)
  }

  if (total_n == 0L) {
    stop("No data processed during chunked regression.", call. = FALSE)
  }

  # The factor has a row for each column of the design the stacking kept and
  # one for the response, which keeps what is left of it however little that
  # is (rchunk_factor()). The stacking left out only what was left of columns
  # of the design that are combinations of the columns before them, never
  # what was left of the response, so the effects past the rank are those of
  # the response across the columns kept here, as when every row was kept: the
  # residual sum of squares is that of 0.17.0 but for rounding, in chunks of
  # any size.
  columns <- ncol(factor) - 1L
  decomposition <- qr(factor[, seq_len(columns), drop = FALSE])
  rank <- decomposition$rank
  effects <- qr.qty(decomposition, factor[, columns + 1L])
  explained <- sum(effects[seq_len(rank)][-1L]^2)
  residual <- sum(effects[-seq_len(rank)]^2)
  total <- explained + residual
  zero_variation <- !(total > .Machine$double.eps * y_sq_sum)

  list(
    rank = rank,
    columns = columns,
    rank_deficient = rank < columns,
    df = rank - 1L,
    zero_variation = zero_variation,
    r_squared = if (zero_variation) NA_real_ else min(max(explained / total, 0), 1),
    explained = explained,
    residual = residual,
    n = total_n,
    response_mean = y_sum / total_n,
    response_tss = total,
    # The factor itself (rchunk_factor()), from which
    # performWhiteTestStreaming() tells whether the regressors alone fit the
    # squared residuals exactly (rfactor_regressors_exact()).
    factor = factor,
    chunks = length(chunks)
  )
}

# Whether the regressors, the first `regressors` columns after the intercept of
# White's auxiliary design, fit the squared residuals exactly, from the factor
# of chunked_regression_r2(), as rwhite_regressors_fit_exactly() tells it from
# the design: the regression of the response, the last column of the factor,
# on the intercept and the regressors, at the tolerance of the test.
rfactor_regressors_exact <- function(factor, regressors) {
  response <- factor[, ncol(factor)]
  decomposition <- qr(factor[, seq_len(regressors + 1L), drop = FALSE])
  effects <- qr.qty(decomposition, response)
  rauxiliary_exact(sum(effects[-seq_len(decomposition$rank)]^2), sum(effects[-1L]^2))
}

# The builders of the auxiliary regressions: the predictors from the rows of
# the design in a chunk, and the response from the residuals of those rows.
white_predictor_builder <- function(cross_products = TRUE, max_interactions = 10) {
  function(model_matrix) {
    if (ncol(model_matrix) < 2L) {
      stop("White test requires at least one regressor beyond the intercept.", call. = FALSE)
    }

    if (colnames(model_matrix)[1] == "(Intercept)") {
      X <- model_matrix[, -1, drop = FALSE]
    } else {
      X <- model_matrix
    }

    aux_matrix <- X
    regressor_names <- colnames(X)
    aux_names <- regressor_names

    for (j in seq_len(ncol(X))) {
      aux_matrix <- cbind(aux_matrix, X[, j]^2)
      aux_names <- c(aux_names, paste0(regressor_names[j], "_sq"))
    }

    if (cross_products && ncol(X) > 1) {
      if (ncol(X) > max_interactions) {
        .std_warning("cross_products_omitted")
      } else {
        for (i in seq_len(ncol(X) - 1L)) {
          for (j in seq.int(i + 1L, ncol(X))) {
            aux_matrix <- cbind(aux_matrix, X[, i] * X[, j])
            aux_names <- c(aux_names, paste0(regressor_names[i], "_x_", regressor_names[j]))
          }
        }
      }
    }

    colnames(aux_matrix) <- aux_names
    aux_matrix
  }
}

# The refusal of a model with an intercept alone names the test: until 0.18.0
# Koenker's streaming test refused it as "Breusch-Pagan test requires ...".
bp_predictor_builder <- function(model_matrix, test_label = "Breusch-Pagan test") {
  if (colnames(model_matrix)[1] == "(Intercept)") {
    predictors <- model_matrix[, -1, drop = FALSE]
  } else {
    predictors <- model_matrix
  }
  if (ncol(predictors) == 0) {
    stop(test_label, " requires at least one predictor beyond the intercept.", call. = FALSE)
  }
  predictors
}

koenker_predictor_builder <- function(model_matrix) {
  bp_predictor_builder(model_matrix, "Koenker test")
}

white_response_builder <- function(residuals_chunk) {
  residuals_chunk^2
}

bp_response_builder <- function(residuals_chunk) {
  residuals_chunk^2
}

koenker_response_builder <- function(residuals_chunk) {
  # Koenker's studentized BP regresses the squared residuals on the regressors
  # (n R^2); it is the squared-residual auxiliary regression, not the absolute one.
  residuals_chunk^2
}

#' Memory-efficient White test for large datasets
#'
#' Computes White's statistic chunk by chunk rather than fitting the full
#' auxiliary regression in memory. This streaming approach allows the test to
#' scale to datasets that would otherwise exhaust available RAM.
#'
#' @inheritParams performWhiteTest
#' @param data The data frame `model` was fitted to, or `NULL`, the default.
#'   The response, design, weights and offset are read from the fit itself, so
#'   a model that transforms its variables, has an `offset()` term, or takes
#'   its `weights` or `offset` from a column of the data needs no `data`, and
#'   the model frame of `model` is accepted as `data` (since 0.16.0; before,
#'   such models needed `data`). Data that are given are matched to the
#'   observations of the fit: rows with row names of their own by name, in any
#'   order; rows that only number themselves (a tibble, a data.table, a data
#'   frame whose row names were reset) by the row numbers of the fit, or in the
#'   order of the fit when they hold one row per observation. The rows the fit
#'   dropped may be there or not. Data whose rows cannot be shown to hold the
#'   observations of the fit are refused; see [test_results].
#' @param chunk_size Positive integer specifying the number of observations per
#'   chunk. Smaller values reduce memory usage at the expense of additional
#'   iteration overhead.
#' @param progress Logical flag indicating whether a progress bar should be
#'   displayed while streaming the data. Defaults to `interactive()`.
#'
#' @return A \code{htest} object containing the chi-squared statistic, p-value,
#'   and metadata about the chunked computation.
#'
#' @details
#' The streaming implementation builds the auxiliary design, the regressors,
#' their squares and their cross-products, chunk by chunk, without
#' materialising it whole. Each chunk is reduced by a QR decomposition stacked
#' on the factor of the chunks before it, which holds the cross-products of
#' every row without forming them. The rank of the auxiliary regression and
#' its \eqn{n R^2} statistic are computed from that factor as
#' in [performWhiteTest()], ensuring numerical equivalence while reducing peak
#' memory usage: the regressors are centred at their means, the rank is that of
#' the auxiliary design at the default tolerance of [qr()], and a term that is a
#' combination of the intercept and the terms before it is dropped (see
#' *Degrees of freedom* in [performWhiteTest()]). Since 0.18.0 the factor
#' leaves out what is left of such a term when that is less than
#' \eqn{10^{-13}} of its norm: rounding error, for the square of a dummy
#' variable. That changes the value of the test by rounding alone: by up to
#' \eqn{2 \cdot 10^{-14}}{2e-14} relative on 100,000 observations with a
#' factor of four levels, less than the \eqn{2.5 \cdot 10^{-14}}{2.5e-14} to
#' \eqn{4.5 \cdot 10^{-14}}{4.5e-14} by which the streaming test differs from
#' [performWhiteTest()] there, before and since. What is left of the squared
#' residuals, however little, is never left out. In 0.17.0 the decomposition
#' made a reflection from that rounding error, the
#' next such term left about its square, and so on down, and on a design with
#' many such terms, as the squares and products of dummies are, it could
#' divide by a norm below the reciprocal of the largest double and stop
#' ("NA/NaN/Inf in foreign function call"), as it did on a one-way layout
#' under a BLAS that rounds otherwise than the reference BLAS of R.
#'
#' Since 0.17.0 the test drops such terms: the square of a dummy variable,
#' which is the dummy, that of the first column of an orthogonal `poly()`
#' basis, or that of `x` beside `I(x^2)`, as [performWhiteTest()] drops them,
#' and gives its degrees of freedom: 17.83 on 8 on `lm(y ~ x1 + group)` with a
#' factor of four levels. Up to 0.16.0 it accumulated the cross-products of the
#' uncentred auxiliary design, decided their rank by [qr()] of them, which
#' squares the condition of the design, and stopped on such terms ("Auxiliary
#' regression became rank deficient"), so on any model with a factor, and on
#' terms that only looked collinear in those cross-products: the squares of a
#' regressor 100 standard deviations from zero, or the terms of
#' `lm(y ~ x1 * x2)` with `x1` between 0.5 and 2. It took the squared
#' residuals for constant when their total sum of squares about their mean was
#' at most `.Machine$double.eps`, whatever their scale, and so refused
#' residuals of about \eqn{10^{-5}}: on
#' 120 observations of such residuals it now gives 12.787 on 5 degrees of
#' freedom, the value of [performWhiteTest()]. It refuses squared residuals
#' whose total sum of squares is at most `.Machine$double.eps` of their sum of
#' squares, which agree to about eight digits. And in more than one chunk, on
#' a regressor `a` beside a regressor `a_sq`, it put the square of `a`, which
#' it names `a_sq` too, in place of that of `a_sq` in every chunk after the
#' first: 27.76 in chunks of 7 where [performWhiteTest()] gives 27.64. It no
#' longer uses the sparse cross-products of `Matrix` for large chunks.
#'
#' Since 0.16.0 the chunks are rows of the design of the fit, read from the fit
#' itself as [performWhiteTest()] reads it, so the value does not depend on the
#' size of the chunks but by rounding; the auxiliary design, several times
#' wider, is built chunk by chunk. Up to 0.16.0 each chunk built its own design
#' from the formula, on its rows of `data`, so a term computed from the rows it
#' is given took other values in each chunk than in the fit:
#' `I(x > median(x))` the median of the chunk, and `cut(x, quantile(x))` its
#' breaks, which stopped the test ("factor has new levels") in chunks of any
#' size on a fit that dropped the row at the minimum of `x`. Since 0.17.0 an
#' `mgcv::gam()` fit with smooth terms, penalised or fixed, is tested on its
#' design, the basis of its smooths, as by the exact tests, read chunk by chunk,
#' and gives their value; before, it was tested on the design of the variables
#' of its formula, which its model frame holds (decision 37 of the design of the
#' model context).
#'
#' @references
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroscedasticity. *Econometrica, 48*(4), 817–838.
#'
#' @section Supported models:
#' It accepts an `lm` and, since 0.16.0, a `glm` with the Gaussian family and the
#' identity link, unweighted or with positive weights, a `fitWLS()` fit and, since
#' 0.16.0, a rank-deficient fit, which it tests on the estimable part of its
#' design: it gives the value of the same model fitted without the aliased columns.
#' The Gaussian `glm` with prior weights `w` is the least-squares fit
#' `lm(..., weights = w)`, with the same design and residuals, and the test gives
#' the value of the `lm`. It refused every `glm` before 0.16.0, because the checks
#' it runs read the R-squared of `summary()`, which a `glm` does not report; they
#' read that of the least-squares fit now. It refuses a `glm` of another family or
#' link, a `survey::svyglm()` fit, a fit with a zero weight, a coefficient that is
#' not finite for another reason, `data` that lack rows or variables of the
#' fit, and, since 0.16.0, a multivariate `lm`, a fit changed after it was
#' fitted and a fit with `model = FALSE` whose data are gone, unless `data`
#' gives its response and fitted values (see [test_results]). Since 0.17.0 it
#' drops the terms of its auxiliary regression that are collinear, as the
#' squares of dummy variables are, as [performWhiteTest()] drops them, where it
#' stopped. Since 0.16.0 it stops with the refusal of [performWhiteTest()] (see
#' its section *Too few observations*) when its independent terms are at least
#' as many as the observations, where it stopped as on collinear terms, or fit
#' the squared residuals exactly, collinear or not, as on a paired design.
#' Which model each test accepts, and why, is set out in [test_results], under
#' *Supported models*.
#'
#' @examples
#' data(mtcars)
#' performWhiteTestStreaming(lm(mpg ~ wt + qsec, data = mtcars), mtcars, chunk_size = 16)
#'
#' @seealso
#' [performWhiteTest()] for the exact computation and \link[=performWhiteTestRobust]{performWhiteTestRobust()} for
#' enhanced reporting.
#' @export
performWhiteTestStreaming <- function(model, data = NULL, chunk_size = 10000,
                                      cross_products = TRUE, max_interactions = 10,
                                      progress = interactive()) {
  test_label <- "White test"
  # A Gaussian glm with the identity link is a least-squares fit, accepted
  # since 0.16.0 (see the section on supported models); any other glm is not.
  # It refused every glm before, because checkModel(enhanced = TRUE) read the
  # R-squared of summary(), which a glm does not report; it reads that of the
  # least-squares fit now (rleast_squares_r_squared()). The response, design,
  # weights and residuals come from the fit; `data`, when given, is matched to
  # its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performWhiteTestStreaming", test_label = test_label)
  # The warnings of the checks of the model, on the data the test reads, or
  # the model frame.
  checkModel(model, if (is.null(data)) ctx$frame else data, enhanced = TRUE)
  rvalidateModelInputs(model, test_name = test_label, min_obs = 20L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  rprocessValidationResult(requirements)

  design <- rstreaming_design(ctx)
  stats <- chunked_regression_r2(
    design = design,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = white_predictor_builder(cross_products, max_interactions),
    response_builder = white_response_builder,
    progress = progress,
    progress_label = "Streaming White test"
  )

  # The terms of the auxiliary regression are counted by their rank, decided
  # as performWhiteTest() decides it (chunked_regression_r2()), and the
  # collinear ones are dropped, as performWhiteTest() drops them: the square
  # of a dummy, which is the dummy, that of the first column of an orthogonal
  # poly() basis, and, where the terms span the constant, one more. The
  # degrees of freedom are the rank less one. Up to 0.16.0 the test stopped on
  # such terms ("Auxiliary regression became rank deficient"), so on any model
  # with a factor (N3 of the design of the model context).
  #
  # As many independent terms as observations, or more, or fewer that fit the
  # squared residuals exactly: performWhiteTest() refuses both, and so does
  # this test, with the same message and advice, so that runHeteroTests() falls
  # back where it does: the squares of the dummies of a paired design equal
  # the dummies and count once, so 30 pairs and a treatment have 31
  # independent terms for 60 observations, an exact fit the design forces, not
  # too few observations.
  regressors <- ncol(design) - identical(colnames(design)[1L], "(Intercept)")
  products <- isTRUE(cross_products) && regressors > 1 && regressors <= max_interactions
  what <- if (isTRUE(stats$rank_deficient)) "independent terms" else "terms"
  if (stats$rank >= stats$n) {
    rwhite_refuse_exact_fit(stats$rank, stats$n, saturated = TRUE, products = products,
                            regressors_exact = rfactor_regressors_exact(stats$factor, regressors),
                            what = what)
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "White test requires variation in squared residuals"
    )
  }

  if (rauxiliary_exact(stats$residual, stats$response_tss)) {
    rwhite_refuse_exact_fit(stats$rank, stats$n, saturated = FALSE, products = products,
                            regressors_exact = rfactor_regressors_exact(stats$factor, regressors),
                            what = what)
  }

  test_statistic <- stats$n * stats$r_squared
  p_value <- stats::pchisq(test_statistic, stats$df, lower.tail = FALSE)

  # The warning of performWhiteTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(model, ctx$frame, "White's test", "performWhiteTest")

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = stats$df),
      p.value = p_value,
      method = "White's test for heteroscedasticity (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Streaming Breusch-Pagan test for large datasets
#'
#' Computes the classical Breusch-Pagan statistic chunk by chunk to avoid
#' materialising the full auxiliary regression when working with large
#' data frames. The streamed result is algebraically equivalent to the standard
#' implementation while substantially reducing peak memory usage.
#'
#' @inheritParams performBPTest
#' @param data The data frame `model` was fitted to, or `NULL`, the default,
#'   read as [performWhiteTestStreaming()] reads it: the response, design,
#'   weights and offset come from the fit itself, so a model that transforms
#'   its variables, has an `offset()` term, or takes its `weights` or `offset`
#'   from a column of the data needs no `data` (since 0.16.0), and data that
#'   are given are matched to the observations of the fit.
#' @param chunk_size Positive integer giving the number of observations per
#'   streaming chunk.
#' @param progress Logical flag indicating whether a textual progress bar should
#'   be displayed while processing the chunks. Defaults to `interactive()`.
#'
#' @return A \code{htest} object mirroring [performBPTest()] with additional
#'   metadata describing the chunking configuration.
#'
#' @details Each chunk of the regressors, centred at their means, is reduced
#'   by a QR decomposition stacked on the factor of the chunks before it, as
#'   in [performWhiteTestStreaming()], since 0.17.0. Since 0.18.0 the factor
#'   leaves out what is left of a regressor that is a combination of those
#'   before it in the rows so far, rounding error, as its details say. Up to
#'   0.16.0 the cross-products of the regressors were accumulated: those of a
#'   regressor 1,000 standard deviations from zero looked collinear with the
#'   intercept, and the test stopped ("Breusch-Pagan auxiliary regression
#'   became rank deficient"); and squared residuals of about \eqn{10^{-10}}
#'   were taken for constant ("requires variation in squared residuals"). It
#'   gives the value of [performBPTest()] on both. Sparse cross-products from
#'   \pkg{Matrix} are no longer used.
#'
#'   The degrees of freedom are the rank of the auxiliary regression less its
#'   intercept, as in [performBPTest()] (see its *Degrees of freedom*). Since
#'   0.18.0 the test gives them, and the value of [performBPTest()], on a
#'   model without an intercept whose columns span the constant, such as the
#'   dummies of every level of a factor in `lm(y ~ 0 + group + x1)`: 8.653 on
#'   4 degrees of freedom, where it stopped ("Breusch-Pagan auxiliary
#'   regression became rank deficient").
#'
#'   Since 0.18.0 the test refuses an auxiliary regression that fits the
#'   squared residuals exactly, as on a paired design, by the rule and with
#'   the message and class of [performBPTest()] (see its *Exact fit*), where
#'   it gave 63.25 on 30 degrees of freedom on 30 subjects of two
#'   observations.
#'
#'   Since 0.16.0 the chunks are rows of the design of the fit, as in
#'   [performWhiteTestStreaming()], whose details say what changed.
#'
#' @export
performBPTestStreaming <- function(model, data = NULL, chunk_size = 10000,
                                   progress = interactive()) {
  test_label <- "Breusch-Pagan"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performBPTestStreaming", test_label = test_label)
  rvalidateModelInputs(model, test_name = test_label, min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("breusch_pagan", model = model, data = working_data)
  rprocessValidationResult(requirements)

  residual_variance <- stats::var(residuals)
  if (is.na(residual_variance) || residual_variance <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires residual variation greater than numerical precision"
    )
  }

  stats <- chunked_regression_r2(
    design = rstreaming_design(ctx),
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = bp_predictor_builder,
    response_builder = bp_response_builder,
    progress = progress,
    progress_label = "Streaming Breusch-Pagan test"
  )

  # The degrees of freedom are the rank of the auxiliary regression less its
  # intercept, decided as performBPTest() decides it (chunked_regression_r2()):
  # a model without an intercept whose columns span the constant, as the
  # dummies of every level of a factor do, counts the intercept once. Until
  # 0.18.0 the test stopped on such a model ("Breusch-Pagan auxiliary
  # regression became rank deficient").
  if (stats$df < 1L) {
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires variation in squared residuals"
    )
  }

  # An auxiliary regression that fits the squared residuals exactly, as on a
  # paired design: refused since 0.18.0 by the rule and with the message of
  # performBPTest(), decided from the sums of squares of
  # chunked_regression_r2(), as performWhiteTestStreaming() decides White's.
  # The response here is the squared residuals, not their scaled form, an
  # affine function of them with the same fit.
  if (rauxiliary_exact(stats$residual, stats$response_tss)) {
    rrefuse_auxiliary_exact_fit("the Breusch-Pagan test", stats$rank, stats$n,
                                rexact_fit_statistic$spread)
  }

  # Classical BP = 0.5 * ESS_f, with f = e^2/sigma^2 - 1. Scaling the auxiliary
  # response by a constant 1/sigma^2 scales the explained sum of squares by
  # 1/sigma^4, so 0.5 * ESS_f = 0.5 * ESS_{e^2} / sigma^4. This is
  # algebraically identical to the exact performBPTest() computation.
  sigma2 <- stats$response_mean
  ess <- stats$explained
  test_statistic <- 0.5 * ess / sigma2^2
  df <- stats$df
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  # The warning of performBPTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(model, ctx$frame, "the Breusch-Pagan test", "performBPTest")

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Streaming Koenker studentized Breusch-Pagan test
#'
#' Evaluates Koenker's studentized variant of the Breusch-Pagan test (the
#' \eqn{n R^2} statistic from regressing the squared residuals on the regressors)
#' chunk by chunk so that large datasets can be processed without allocating the
#' full auxiliary regression matrix.
#'
#' @inheritParams performKoenkerTest
#' @param data The data frame `model` was fitted to, or `NULL`, the default,
#'   read as [performWhiteTestStreaming()] reads it: the response, design,
#'   weights and offset come from the fit itself, so a model that transforms
#'   its variables, has an `offset()` term, or takes its `weights` or `offset`
#'   from a column of the data needs no `data` (since 0.16.0), and data that
#'   are given are matched to the observations of the fit.
#' @param chunk_size Positive integer giving the number of observations processed
#'   per streaming chunk.
#' @param progress Logical indicating whether to display a textual progress bar.
#'
#' @return A \code{htest} result equivalent to [performKoenkerTest()] with
#'   additional metadata describing the streaming configuration.
#'
#' @details Each chunk of the regressors, centred at their means, is reduced
#'   by a QR decomposition stacked on the factor of the chunks before it, as
#'   in [performWhiteTestStreaming()], since 0.17.0. Since 0.18.0 the factor
#'   leaves out what is left of a regressor that is a combination of those
#'   before it in the rows so far, rounding error, as its details say. Up to
#'   0.16.0 the cross-products of the regressors were accumulated: those of a
#'   regressor 1,000 standard deviations from zero looked collinear with the
#'   intercept, and the test stopped ("Koenker auxiliary regression became
#'   rank deficient"); and squared residuals of about \eqn{10^{-10}} were
#'   taken for constant ("requires variation in squared residuals"). It gives
#'   the value of [performKoenkerTest()] on both. Sparse cross-products from
#'   \pkg{Matrix} are no longer used.
#'
#'   The degrees of freedom are the rank of the auxiliary regression less its
#'   intercept, as in [performKoenkerTest()] (see its *Degrees of freedom*).
#'   Since 0.18.0 the test gives them, and the value of [performKoenkerTest()],
#'   on a model without an intercept whose columns span the constant, such as
#'   the dummies of every level of a factor in `lm(y ~ 0 + group + x1)`: 10.52
#'   on 4 degrees of freedom, where it stopped ("Koenker auxiliary regression
#'   became rank deficient").
#'
#'   Since 0.18.0 the test refuses an auxiliary regression that fits the
#'   squared residuals exactly, as on a paired design, by the rule and with
#'   the message and class of [performKoenkerTest()] (see its *Exact fit*),
#'   where it gave 60 on 30 degrees of freedom on 30 subjects of two
#'   observations.
#'
#'   Since 0.16.0 the chunks are rows of the design of the fit, as in
#'   [performWhiteTestStreaming()], whose details say what changed.
#'
#' @export
performKoenkerTestStreaming <- function(model, data = NULL, chunk_size = 10000,
                                        progress = interactive()) {
  test_label <- "Koenker"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performKoenkerTestStreaming", test_label = test_label)
  rvalidateModelInputs(model, test_name = test_label, min_obs = 15L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("koenker", model = model, data = working_data)
  rprocessValidationResult(requirements)

  stats <- chunked_regression_r2(
    design = rstreaming_design(ctx),
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = koenker_predictor_builder,
    response_builder = koenker_response_builder,
    progress = progress,
    progress_label = "Streaming Koenker test"
  )

  # The degrees of freedom are the rank of the auxiliary regression less its
  # intercept, decided as performKoenkerTest() decides it
  # (chunked_regression_r2()): a model without an intercept whose columns span
  # the constant, as the dummies of every level of a factor do, counts the
  # intercept once. Until 0.18.0 the test stopped on such a model ("Koenker
  # auxiliary regression became rank deficient").
  if (stats$df < 1L) {
    stop("Koenker test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in squared residuals"
    )
  }

  # An auxiliary regression that fits the squared residuals exactly, as on a
  # paired design: refused since 0.18.0 by the rule and with the message of
  # performKoenkerTest().
  if (rauxiliary_exact(stats$residual, stats$response_tss)) {
    rrefuse_auxiliary_exact_fit("Koenker's test", stats$rank, stats$n,
                                rexact_fit_statistic$n_r_squared)
  }

  test_statistic <- stats$n * stats$r_squared
  df <- stats$df
  p_value <- 1 - stats::pchisq(test_statistic, df)

  # The warning of performKoenkerTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(model, ctx$frame, "Koenker's test", "performKoenkerTest")

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test (streaming)",
      data.name = deparse(stats::formula(model)),
      chunks_processed = stats$chunks,
      chunk_size = as.integer(chunk_size),
      r_squared = stats$r_squared
    ),
    class = "htest"
  )
}

#' Parallel test execution helper
#'
#' Executes a set of registered diagnostics in parallel when multiple CPU cores
#' are available, falling back to sequential execution otherwise.
#'
#' @param model Fitted model object passed to each diagnostic. Must be compatible
#'   with the internal `.test_factory` registry.
#' @param data Data frame used to fit `model` and supplied to each diagnostic.
#' @param tests Character vector naming the diagnostics to execute.
#' @param n_cores Optional positive integer giving the number of worker processes
#'   to spawn. Defaults to `parallel::detectCores() - 1`, with a lower bound of 1.
#'
#' @return Named list of test results in the order provided by `tests`. Each
#'   is the result [runHeteroTests()] gives for that test on the same call,
#'   its numbers computed the same way, since 0.18.0, and so `identical()` to
#'   it where the BLAS gives the same last bits on every call, as R's
#'   reference BLAS does (Intel MKL need not): a result whose `data.name` is
#'   `"model"`, the name of the
#'   argument of the function the registry calls the test with, reports the
#'   formula of the model instead, and `attr(, "extras")$streaming` is
#'   `FALSE`, as this function runs the exact tests only. Before, White's test,
#'   its bootstrap, the studentized Breusch-Pagan statistic and Szroeter's test
#'   reported `"model"` here. A resampling test draws from a stream of its own
#'   (Details), and so does not return the value it returns in
#'   [runHeteroTests()], which draws from the session's generator. The list
#'   is not a [`hetero_test_suite`], and a test that fails stops the call,
#'   without the fallbacks of [runHeteroTests()].
#'
#' @details
#' When `n_cores` exceeds one the diagnostics run on a socket cluster of that
#' many workers ([parallel::makeCluster()]), started for the call, each of which
#' loads the installed heteroTests. The model and the data travel to the
#' workers with each task. With `n_cores = 1`, or when the default comes to one
#' worker, the diagnostics run in the session.
#'
#' Test `i` draws its random numbers from stream `i` of a L'Ecuyer-CMRG
#' generator seeded by one draw from the session's generator, on the workers
#' and in the session alike. A seeded call that runs a resampling test, such as
#' `"wild_bootstrap"`, therefore returns the same value each time and whatever
#' `n_cores`, `n_cores = 1` included. It is not the value that test returns
#' when called directly from the same seed, which draws from the session's
#' generator; both are bootstraps of the same data.
#'
#' @seealso
#' [cachedTest()] for memoised execution of individual diagnostics.
#' @export
runHeteroTestsParallel <- function(model, data, tests, n_cores = NULL) {
  # Forced here, in the session. The task below travels to a socket worker
  # with this frame, and an argument still a promise there is evaluated in
  # the worker, which has no `m1` for runHeteroTestsParallel(m1, mtcars, ...)
  # called at top level.
  force(model)
  force(data)
  force(tests)
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1L, length(tests))
  }

  # Test i draws from stream i of a L'Ecuyer-CMRG generator seeded by one
  # draw from the session's, so a seeded call that runs a resampling test
  # gives the same result each time, whatever n_cores: a call run in the
  # session draws from the same streams as one run on workers. Before 0.15.0
  # each worker drew from a generator set.seed() did not reach, and a call run
  # in the session drew from the session's generator.
  streams <- rtask_streams(length(tests))
  task <- function(i) .run_registered_diagnostic(tests[[i]], model, data)
  if (n_cores > 1L) {
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)

    parallel::clusterEvalQ(cl, library(heteroTests))
    results <- rparallel_tasks(seq_along(tests), task, streams, n_cores, cl)
  } else {
    results <- lapply(seq_along(tests), function(i) rrun_on_stream(streams[[i]], task, i))
  }
  names(results) <- tests

  # Each result as runHeteroTests() returns it: marked as computed by the
  # exact test, the only one run here, and a test that named the argument
  # "model" names the model's formula instead (rsuite_data_name()). Before
  # 0.18.0 White's test and the three others of the registry that name it
  # reported `data.name = "model"` here.
  formula_text <- rsuite_formula_text(model)
  lapply(results, function(result) {
    if (inherits(result, "hetero_test")) {
      attr(result, "extras") <- utils::modifyList(
        attr(result, "extras") %||% list(),
        list(streaming = FALSE)
      )
    }
    rsuite_data_name(result, formula_text)
  })
}

#' Cache results of a heteroscedasticity test
#'
#' Runs a registered diagnostic, and returns the result of an earlier call on
#' the same inputs instead when the diagnostic is deterministic.
#'
#' @param test_name Character scalar naming a diagnostic of the registry
#'   ([listDiagnostics()]) or of `test_factory`.
#' @param model Fitted model object supplied to the diagnostic.
#' @param data Data frame used to fit the model.
#' @param ... Additional arguments forwarded to the diagnostic.
#' @param use_cache Logical scalar: may the result be read from the cache and
#'   stored in it? `FALSE` runs the diagnostic. Defaults to `TRUE`.
#'
#' @return The diagnostic result, either retrieved from cache or freshly
#'   computed. A result whose `data.name` is `"model"`, the name of the
#'   argument of the function the registry calls the test with, reports the
#'   formula of the model instead, as in [runHeteroTests()], since 0.18.0;
#'   White's test, its bootstrap, the studentized Breusch-Pagan statistic and
#'   Szroeter's test reported `"model"` here before. The cache keeps the result
#'   as the test returns it, and the rule applies to a result read from it too.
#'   Unlike a result of [runHeteroTests()], it has no
#'   `attr(, "extras")$streaming`, which says how the runner ran a test.
#'
#' @details
#' **What is cached.** Only the deterministic tests the package registers:
#' `"white"`, `"breusch_pagan"`, `"koenker"`, `"cook_weisberg"`, `"ncv"`,
#' `"spread_level"`, `"student_bp"`, `"szroeter"`, `"quantile_regression"`,
#' `"high_dimensional"` and `"variance_form"`.
#'
#' **What is not.** A test that draws random numbers is never cached:
#' `"wild_bootstrap"`, `"white_bootstrap"`, `"rank_permutation"` and
#' `"spatial_hetero"` run on every call, so a seeded call returns what it
#' returns without the cache, and leaves the random number generator where it
#' leaves it without the cache. Nor is `"box_m"`, which tests data rather than
#' a model, a diagnostic added with [registerDiagnostic()] or
#' `test_factory$register()`, or one registered again under a name of the
#' package: the package cannot know whether it draws random numbers or reads
#' anything besides its inputs. A result whose computation moved the random
#' number generator is not stored either. Before 0.16.0 every name was cached,
#' the resampling tests included: a call with another seed returned the
#' replicates of the first, and the random numbers drawn after it were not
#' those of a call without the cache.
#'
#' **The key.** The test, its arguments, and the parts of the model and of the
#' data the test reads: the class of the model, its family and link;
#' from the model context of the fit, its model frame, response, design,
#' residuals and fitted values, prior weights, total offset, rank and aliased
#' coefficients, the names of its observations and its `na.action`; its
#' coefficients, the weights it stores, its terms (with their `predvars`),
#' call, contrasts and levels, residual degrees of freedom, whether it keeps
#' its model frame, and its attributes, such as the variance function of a
#' [fitWLS()] fit; and `data`, whole: its values, row names, class and
#' attributes. The key is an xxhash64 digest of those values (from the
#' **digest** package; without it nothing is cached). It holds values, not
#' environments: a model refitted to the same data, in another environment or
#' another session, has the same key and is answered from the cache, with the
#' model of the call as `attr(result, "model")`, so that the result is
#' `identical()` to the one the test computes. A model the tests refuse before
#' reading it, such as a fit with `model = FALSE` whose data changed after the
#' fit, has no key and is refused as without the cache; so has a fit with
#' `model = FALSE` whose data are gone, whose model frame the tests rebuild
#' from `data`, and it runs without the cache.
#'
#' **What the key leaves out.** The key does not hold the environment of the
#' formula: a test that evaluates the formula on `data` again, as the check of
#' the rows of `data` does, reads the functions and variables of that
#' environment as they are, so that after `f` is defined again a model
#' `y ~ f(x)` is answered with the result computed with the old `f`, where
#' without the cache the check may refuse `data`. Clear both caches, with
#' [clearTestCache()] and [clearAnalysisCache()], after changing one of them.
#' Nor does it hold the options of the session: a result read from the cache
#' gives none of the warnings and messages of the call that computed it but
#' one (below), so a call made under `options(warn = 2)` after one that
#' stored its result is answered with that result, where without the cache a
#' warning of the test, turned into an error, stops it. The
#' results of the tests otherwise do not depend on the options; the names of
#' the estimates of the quantile regression test depended on
#' `options(digits)` before 0.16.0.
#'
#' Before 0.16.0 the key held only the test, the coefficients, the residuals,
#' `data` and the arguments. Fits that share those shared one entry, and the
#' second call returned the result of the first: `lm(y ~ x + offset(o))` and
#' `lm(I(y - o) ~ x)`, which have the same coefficients and residuals and
#' other fitted values; a fit and the same fit with its weights changed, which
#' the tests refuse; or a fit with `model = FALSE` before and after its data
#' changed, which they refuse too.
#'
#' **The bound.** The cache holds at most
#' `getOption("heteroTests.cache_size", 100)` results, and drops the one used
#' least recently to store another; `options(heteroTests.cache_size = 0)`
#' turns it off, together with the cache of [runHeteroTests()]. A stored result
#' keeps no copy of the model. Before 0.16.0 the cache grew without bound, by
#' about 25 KB per call on a new model, and kept about 0.3 KB per call after
#' [clearTestCache()]. The checks the tests make first keep their results in a
#' cache of their own, of at most
#' `getOption("heteroTests.validation_cache_size", 1000)` results, which `0`
#' turns off; it holds no check that drew random numbers.
#'
#' A result read from the cache is returned without running the test, so the
#' call gives none of the warnings and messages of the call that computed it,
#' but one: since 0.16.0 the warning White's, Koenker's and the Breusch-Pagan
#' tests give on a factor with more levels than observations per level, of
#' class `heteroTests_fixed_effects_warning`, is stored with the result and
#' given again on every call that reads it, as by [runHeteroTests()] (see
#' *Fixed effects for small groups* in [performKoenkerTest()]). The call that
#' computes the result gives it once the result is stored, so that under
#' `options(warn = 2)` the warning stops the call after the result is stored.
#'
#' @seealso
#' [clearTestCache()] empties the cache; [runHeteroTests()] uses it with
#' `use_cache = TRUE`.
#' @export
cachedTest <- function(test_name, model, data, ..., use_cache = TRUE) {
  result <- if (!use_cache) {
    .run_registered_diagnostic(test_name, model, data, ...)
  } else {
    inputs <- if (rcache_cacheable(test_name)) rcache_inputs(model, data)
    rcached_diagnostic(test_name, model, data, inputs, ...)
  }
  # A test that named the argument "model" names the model's formula, as in
  # runHeteroTests() (rsuite_data_name()); before 0.18.0 it named "model".
  rsuite_data_name(result, rsuite_formula_text(model))
}

.run_registered_diagnostic <- function(test_name, model, data, ...) {
  if (exists(test_name, envir = .diagnostic_registry, inherits = FALSE)) {
    fun <- get(test_name, envir = .diagnostic_registry, inherits = FALSE)
    result <- fun(model, data, ...)
    return(.ht_decorate_result(result, test_name, model, data))
  }

  available <- tryCatch(.test_factory$get_available(), error = function(e) character())
  if (test_name %in% available) {
    result <- .test_factory$run_test(test_name, model, data, ...)
    return(.ht_decorate_result(result, test_name, model, data))
  }

  stop("Unknown test: ", test_name)
}

#' Clear cached test results
#'
#' Removes every result from the cache of [cachedTest()], which
#' [runHeteroTests()] also uses for each test with `use_cache = TRUE`, so that
#' later calls compute them again.
#'
#' The cache is bounded and holds no test that draws random numbers (see
#' [cachedTest()]), so clearing it is needed neither to bound the memory of a
#' long session nor to draw new replicates with another seed. It is needed
#' after a change the key does not see, such as a function or a variable of
#' the environment of a model's formula; call [clearAnalysisCache()] then too,
#' since the cache of whole calls of [runHeteroTests()] keeps the results of
#' such a model as well.
#'
#' @return `NULL`, invisibly.
#' @seealso
#' [cachedTest()] for invoking diagnostics with memoisation.
#'
#' @export
clearTestCache <- function() {
  rcache_clear(.test_cache)
  invisible(NULL)
}

# The results of cachedTest() and of each test of runHeteroTests(), and the
# results of whole calls of runHeteroTests(): bounded stores (R/cache.R).
.test_cache <- rcache_new()

.analysis_cache <- rcache_new()

#' Clear cached diagnostic run results
#'
#' Removes the results of whole calls that [runHeteroTests()] stored with
#' `use_cache = TRUE`, so that later calls run their tests again.
#'
#' A call is stored only when every test it runs is deterministic, under a key
#' that holds the tests, whether they are streamed and in chunks of what size,
#' and every part of the model and the data they read (see [cachedTest()]).
#' The cache holds at most `getOption("heteroTests.cache_size", 100)` calls
#' and drops the one used least recently; `0` turns it off. Each test of the
#' call is also kept in the cache of [cachedTest()], which
#' [clearTestCache()] empties.
#'
#' @return `NULL`, invisibly.
#' @seealso [runHeteroTests()], [cachedTest()] for per-diagnostic caching.
#' @export
clearAnalysisCache <- function() {
  rcache_clear(.analysis_cache)
  invisible(NULL)
}
