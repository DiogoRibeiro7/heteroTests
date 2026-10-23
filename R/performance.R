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

should_use_sparse <- function(rows, cols) {
  requireNamespace("Matrix", quietly = TRUE) && rows * cols > 1e6
}

chunked_regression_r2 <- function(model, data, residuals, chunk_size,
                                  predictor_builder, response_builder,
                                  progress = FALSE, progress_label = "Processing chunks") {
  n <- length(residuals)
  if (n == 0L) {
    stop("No observations available after preprocessing.", call. = FALSE)
  }

  chunks <- chunk_indices(n, chunk_size)
  progress_bar <- chunk_progress_bar(length(chunks), progress, progress_label)
  on.exit(progress_bar$close(), add = TRUE)

  terms_obj <- stats::terms(model)
  contrasts <- model$contrasts
  xlevels <- model$xlevels

  XtX <- NULL
  XtY <- NULL
  y_sum <- 0
  y_sq_sum <- 0
  total_n <- 0
  design_names <- NULL

  for (i in seq_along(chunks)) {
    idx <- chunks[[i]]
    chunk_data <- data[idx, , drop = FALSE]
    # Without the columns a rank-deficient fit aliased, as the exact tests
    # build their design (rnon_aliased_columns()).
    mm_chunk <- rnon_aliased_columns(stats::model.matrix(
      terms_obj,
      data = chunk_data,
      contrasts.arg = contrasts,
      xlev = xlevels
    ), model)

    predictors <- predictor_builder(mm_chunk, chunk_data, residuals[idx])
    predictors <- as.matrix(predictors)

    if (ncol(predictors) == 0) {
      stop("Predictor builder produced an empty matrix.", call. = FALSE)
    }

    if (is.null(colnames(predictors))) {
      colnames(predictors) <- paste0("V", seq_len(ncol(predictors)))
    }

    design_chunk <- cbind(`(Intercept)` = 1, predictors)
    if (is.null(design_names)) {
      design_names <- colnames(design_chunk)
      k <- ncol(design_chunk)
      XtX <- matrix(0, nrow = k, ncol = k)
      XtY <- numeric(k)
    } else {
      missing_names <- setdiff(design_names, colnames(design_chunk))
      if (length(missing_names) > 0L) {
        extras <- matrix(0, nrow = nrow(design_chunk), ncol = length(missing_names))
        colnames(extras) <- missing_names
        design_chunk <- cbind(design_chunk, extras)
      }
      design_chunk <- design_chunk[, design_names, drop = FALSE]
    }

    if (ncol(design_chunk) != nrow(XtX)) {
      stop("Inconsistent design matrix dimensions between chunks.", call. = FALSE)
    }

    response <- response_builder(residuals[idx], chunk_data, mm_chunk)
    response <- as.numeric(response)
    if (length(response) != nrow(design_chunk)) {
      stop("Response builder returned a vector of incorrect length.", call. = FALSE)
    }

    if (should_use_sparse(nrow(design_chunk), ncol(design_chunk))) {
      design_sparse <- Matrix::Matrix(design_chunk, sparse = TRUE)
      XtX <- XtX + as.matrix(Matrix::crossprod(design_sparse))
      XtY <- XtY + as.numeric(Matrix::crossprod(design_sparse, response))
    } else {
      XtX <- XtX + crossprod(design_chunk)
      XtY <- XtY + crossprod(design_chunk, response)
    }

    y_sum <- y_sum + sum(response)
    y_sq_sum <- y_sq_sum + sum(response^2)
    total_n <- total_n + length(response)

    progress_bar$update(i)
  }

  if (total_n == 0L) {
    stop("No data processed during chunked regression.", call. = FALSE)
  }

  # The cross-products themselves, from which performWhiteTestStreaming() counts
  # and fits the terms of a regression it refuses (rcentred_cross_fit()).
  cross <- list(XtX = XtX, XtY = as.numeric(XtY), y_sum = y_sum, y_sq_sum = y_sq_sum,
                n = total_n)

  qr_decomp <- qr(XtX)
  if (qr_decomp$rank < ncol(XtX)) {
    return(list(rank_deficient = TRUE, df = ncol(XtX) - 1L, cross = cross,
                chunks = length(chunks)))
  }

  beta <- tryCatch(
    solve(XtX, XtY),
    error = function(e) {
      qr.solve(XtX, XtY)
    }
  )

  sse <- y_sq_sum - sum(beta * XtY)
  sse <- max(sse, 0)
  mean_y <- y_sum / total_n
  sst <- y_sq_sum - total_n * mean_y^2

  if (sst <= .Machine$double.eps) {
    return(list(zero_variation = TRUE, df = ncol(XtX) - 1L, rank = ncol(XtX), cross = cross,
                chunks = length(chunks)))
  }

  r_squared <- 1 - (sse / sst)
  r_squared <- min(max(r_squared, 0), 1)

  list(
    r_squared = r_squared,
    df = ncol(XtX) - 1L,
    rank = ncol(XtX),
    n = total_n,
    response_mean = mean_y,
    response_tss = sst,
    cross = cross,
    chunks = length(chunks)
  )
}

# The least-squares fit of the response on the columns `cols` of a design whose
# first column is the intercept, from its cross-products (chunked_regression_r2()),
# centred, as performWhiteTest() fits its terms. A column is collinear with the
# columns kept before it when less than 1e-13 of its centred variance is its
# own: a pivoted Cholesky decomposition of the centred cross-products scaled
# to correlations. lm.fit() at its tolerance, 1e-7, on the columns themselves
# would be 1e-14, the tolerance squared, but that is the rounding of
# cross-products in double precision: on 25 observations of six regressors of
# mtcars, a 26th independent term at 1.4e-14, where exact duplicates sit below
# 2e-15 and genuine terms of a regressor 10,000 standard deviations from zero
# at 2e-12 and above. The rank is at most the number of observations. qr() of
# the uncentred cross-products squares their condition, and takes independent
# terms of a regressor far from zero, or squares and products of raw
# regressors, for collinear ones. Its rank, the intercept included, its
# R-squared, NA when it cannot be computed, and the total sum of squares of
# the response.
rcentred_cross_fit <- function(cross, cols = seq_len(ncol(cross$XtX))[-1L]) {
  sums <- cross$XtX[1L, cols]
  centred <- cross$XtX[cols, cols, drop = FALSE] - tcrossprod(sums) / cross$n
  centred_y <- cross$XtY[cols] - sums * cross$y_sum / cross$n
  total <- cross$y_sq_sum - cross$y_sum^2 / cross$n
  # A column whose centred norm is below 1e-7 of its norm is the intercept's.
  varying <- which(diag(centred) > 1e-14 * diag(cross$XtX)[cols])
  if (length(varying) == 0L) {
    return(list(rank = 1L, r_squared = NA_real_, total = total))
  }
  spread <- sqrt(diag(centred)[varying])
  correlation <- centred[varying, varying, drop = FALSE] / tcrossprod(spread)
  factor <- suppressWarnings(chol(correlation, pivot = TRUE, tol = 1e-13))
  keep <- attr(factor, "pivot")[seq_len(min(attr(factor, "rank"), cross$n - 1L))]
  scaled_y <- centred_y[varying] / spread
  beta <- tryCatch(solve(correlation[keep, keep, drop = FALSE], scaled_y[keep]),
                   error = function(e) NULL)
  r_squared <- if (!is.null(beta) && total > 0) {
    min(max(sum(beta * scaled_y[keep]) / total, 0), 1)
  } else {
    NA_real_
  }
  list(rank = length(keep) + 1L, r_squared = r_squared, total = total)
}

# Whether the regressors, the first `regressors` columns after the intercept of
# White's auxiliary design, fit the squared residuals exactly, from the
# cross-products, as rwhite_regressors_fit_exactly() tells it from the design.
rcross_regressors_exact <- function(cross, regressors) {
  fit <- rcentred_cross_fit(cross, cols = seq_len(regressors) + 1L)
  fit$total > 0 && isTRUE(1 - fit$r_squared <= rwhite_exact_tolerance)
}

white_predictor_builder <- function(cross_products = TRUE, max_interactions = 10) {
  function(model_matrix, data_chunk, residuals_chunk) {
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

bp_predictor_builder <- function(model_matrix, data_chunk, residuals_chunk) {
  if (colnames(model_matrix)[1] == "(Intercept)") {
    predictors <- model_matrix[, -1, drop = FALSE]
  } else {
    predictors <- model_matrix
  }
  if (ncol(predictors) == 0) {
    stop("Breusch-Pagan test requires at least one predictor beyond the intercept.", call. = FALSE)
  }
  predictors
}

koenker_predictor_builder <- function(model_matrix, data_chunk, residuals_chunk) {
  bp_predictor_builder(model_matrix, data_chunk, residuals_chunk)
}

white_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  residuals_chunk^2
}

bp_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  residuals_chunk^2
}

koenker_response_builder <- function(residuals_chunk, data_chunk, model_matrix_chunk) {
  # Koenker's studentized BP regresses the squared residuals on the regressors
  # (n R^2); it is the squared-residual auxiliary regression, not the absolute one.
  residuals_chunk^2
}

#' Memory-efficient White test for large datasets
#'
#' Computes White's statistic via chunked cross-products rather than fitting the
#' full auxiliary regression in memory. This streaming approach allows the test
#' to scale to datasets that would otherwise exhaust available RAM.
#'
#' @inheritParams performWhiteTest
#' @param data A [base::data.frame] (or object coercible to one) containing the
#'   variables referenced by `model`. It must include the observations used to
#'   fit `model` and will be checked for missing values.
#'   When `NULL`, the default, the model frame of `model` is used. It holds the
#'   terms of the model rather than its variables (`log(x)` rather than `x`), so
#'   a model that transforms a variable needs `data`, as does a
#'   model with an `offset()` term, and so may a fit whose `weights` or `offset`
#'   argument is a column of the data, which the model frame holds as
#'   `(weights)` or `(offset)`.
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
#' The streaming implementation iteratively builds the cross-product matrices
#' required for the auxiliary regression without materialising the full design
#' matrix. Each chunk contributes to \eqn{X'X}, \eqn{X'y}, and summary statistics
#' for the response. The final \eqn{n R^2} statistic is then computed exactly as
#' in the standard White test, ensuring numerical equivalence while dramatically
#' reducing peak memory usage. When `Matrix` is installed, sparse cross-products
#' are leveraged automatically for large chunks.
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
#' not finite for another reason, and `data` that lack rows or variables of the
#' fit. It stops when terms of its auxiliary regression are
#' collinear, as the squares of dummy variables are, which [performWhiteTest()]
#' drops, and, since 0.16.0, with the refusal of [performWhiteTest()] (see its
#' section *Too few observations*) when its independent terms are at least as
#' many as the observations, where it stopped as on collinear terms, or fit the
#' squared residuals exactly, collinear or not, as on a paired design. Which
#' model each test accepts, and why, is set out in [test_results], under
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
  # A Gaussian glm with the identity link is a least-squares fit, accepted
  # since 0.16.0 (see the section on supported models); any other glm is not.
  # It refused every glm before, because checkModel(enhanced = TRUE) read the
  # R-squared of summary(), which a glm does not report; it reads that of the
  # least-squares fit now (rleast_squares_r_squared()).
  rrefuse_non_gaussian_glm(model, "performWhiteTestStreaming")
  data <- rdata_or_model_frame(model, data)
  checkModel(model, data, enhanced = TRUE)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "White test",
    min_obs_model = 20L,
    min_obs_data = 20L,
    allow_aliased = TRUE
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  rprocessValidationResult(requirements)

  stats <- chunked_regression_r2(
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = white_predictor_builder(cross_products, max_interactions),
    response_builder = white_response_builder,
    progress = progress,
    progress_label = "Streaming White test"
  )

  # As many independent terms as observations, or more, or fewer that fit the
  # squared residuals exactly: performWhiteTest() refuses both, and so does
  # this test, with the same message and advice, so that runHeteroTests() falls
  # back where it does. Collinear terms are counted by their rank and fitted
  # centred, from the cross-products (rcentred_cross_fit()), as
  # performWhiteTest() counts and fits them: the squares of the dummies of a
  # paired design equal the dummies and count once, so 30 pairs and a
  # treatment have 31 independent terms for 60 observations, an exact fit the
  # design forces, not too few observations.
  regressors <- sum(!is.na(stats::coef(model))) - attr(stats::terms(model), "intercept")
  products <- isTRUE(cross_products) && regressors > 1 && regressors <= max_interactions
  deficient <- isTRUE(stats$rank_deficient)
  aux <- if (deficient) rcentred_cross_fit(stats$cross) else stats
  exact <- isTRUE(1 - aux$r_squared <= rwhite_exact_tolerance)
  if (aux$rank >= length(residuals) || (deficient && exact)) {
    rwhite_refuse_exact_fit(aux$rank, length(residuals), saturated = aux$rank >= length(residuals),
                            products = products,
                            regressors_exact = rcross_regressors_exact(stats$cross, regressors),
                            what = if (deficient) "independent terms" else "terms")
  }

  if (deficient) {
    .std_error(
      "rassumption_violation",
      assumption = paste(
        "Auxiliary regression became rank deficient; consider disabling cross-product terms",
        "or removing collinear predictors"
      )
    )
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "White test requires variation in squared residuals"
    )
  }

  if (exact) {
    rwhite_refuse_exact_fit(stats$df + 1L, stats$n, saturated = FALSE, products = products,
                            regressors_exact = rcross_regressors_exact(stats$cross, regressors),
                            what = "terms")
  }

  test_statistic <- stats$n * stats$r_squared
  p_value <- stats::pchisq(test_statistic, stats$df, lower.tail = FALSE)

  # The warning of performWhiteTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R). A factor that is a main effect
  # stops the test above, on the squares of its dummies; one that enters only
  # through an interaction with a regressor may not.
  rwarn_small_groups(model, tryCatch(stats::model.frame(model), error = function(e) NULL),
                     "White's test", "performWhiteTest")

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
#' Computes the classical Breusch-Pagan statistic using chunked cross-products
#' to avoid materialising the full auxiliary regression when working with large
#' data frames. The streamed result is algebraically equivalent to the standard
#' implementation while substantially reducing peak memory usage.
#'
#' @inheritParams performBPTest
#' @param data A [base::data.frame] (or compatible object) containing the
#'   variables referenced in `model`. The data must include all observations used
#'   to fit `model` and should not contain unresolved missing values.
#'   When `NULL`, the default, the model frame of `model` is used. It holds the
#'   terms of the model rather than its variables (`log(x)` rather than `x`), so
#'   a model that transforms a variable needs `data`, as does a
#'   model with an `offset()` term, and so may a fit whose `weights` or `offset`
#'   argument is a column of the data, which the model frame holds as
#'   `(weights)` or `(offset)`.
#' @param chunk_size Positive integer giving the number of observations per
#'   streaming chunk.
#' @param progress Logical flag indicating whether a textual progress bar should
#'   be displayed while processing the chunks. Defaults to `interactive()`.
#'
#' @return A \code{htest} object mirroring [performBPTest()] with additional
#'   metadata describing the chunking configuration.
#'
#' @export
performBPTestStreaming <- function(model, data = NULL, chunk_size = 10000,
                                   progress = interactive()) {
  rrefuse_non_gaussian_glm(model, "performBPTestStreaming")
  data <- rdata_or_model_frame(model, data)
  test_label <- "Breusch-Pagan"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L,
    allow_aliased = TRUE
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

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
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = bp_predictor_builder,
    response_builder = bp_response_builder,
    progress = progress,
    progress_label = "Streaming Breusch-Pagan test"
  )

  if (isTRUE(stats$rank_deficient)) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan auxiliary regression became rank deficient"
    )
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "Breusch-Pagan test requires variation in squared residuals"
    )
  }

  # Classical BP = 0.5 * ESS_f, with f = e^2/sigma^2 - 1. Scaling the auxiliary
  # response by a constant 1/sigma^2 scales the explained sum of squares by
  # 1/sigma^4, so 0.5 * ESS_f = 0.5 * (R^2 * TSS_{e^2}) / sigma^4. This is
  # algebraically identical to the exact performBPTest() computation.
  sigma2 <- stats$response_mean
  ess <- stats$r_squared * stats$response_tss
  test_statistic <- 0.5 * ess / sigma2^2
  df <- stats$df
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  # The warning of performBPTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(model, tryCatch(stats::model.frame(model), error = function(e) NULL),
                     "the Breusch-Pagan test", "performBPTest")

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
#' using streamed cross-products so that large datasets can be processed without
#' allocating the full auxiliary regression matrix.
#'
#' @inheritParams performKoenkerTest
#' @param data A [base::data.frame] containing the variables referenced in
#'   `model`. The data must align with the observations used to fit `model`.
#'   When `NULL`, the default, the model frame of `model` is used. It holds the
#'   terms of the model rather than its variables (`log(x)` rather than `x`), so
#'   a model that transforms a variable needs `data`, as does a
#'   model with an `offset()` term, and so may a fit whose `weights` or `offset`
#'   argument is a column of the data, which the model frame holds as
#'   `(weights)` or `(offset)`.
#' @param chunk_size Positive integer giving the number of observations processed
#'   per streaming chunk.
#' @param progress Logical indicating whether to display a textual progress bar.
#'
#' @return A \code{htest} result equivalent to [performKoenkerTest()] with
#'   additional metadata describing the streaming configuration.
#'
#' @export
performKoenkerTestStreaming <- function(model, data = NULL, chunk_size = 10000,
                                        progress = interactive()) {
  rrefuse_non_gaussian_glm(model, "performKoenkerTestStreaming")
  data <- rdata_or_model_frame(model, data)
  test_label <- "Koenker"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L,
    allow_aliased = TRUE
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("koenker", model = model, data = working_data)
  rprocessValidationResult(requirements)

  stats <- chunked_regression_r2(
    model = model,
    data = working_data,
    residuals = residuals,
    chunk_size = chunk_size,
    predictor_builder = koenker_predictor_builder,
    response_builder = koenker_response_builder,
    progress = progress,
    progress_label = "Streaming Koenker test"
  )

  if (isTRUE(stats$rank_deficient)) {
    .std_error(
      "rassumption_violation",
      assumption = "Koenker auxiliary regression became rank deficient"
    )
  }

  if (isTRUE(stats$zero_variation)) {
    .std_error(
      "rassumption_violation",
      assumption = "Koenker test requires variation in squared residuals"
    )
  }

  test_statistic <- stats$n * stats$r_squared
  df <- stats$df
  p_value <- 1 - stats::pchisq(test_statistic, df)

  # The warning of performKoenkerTest() on a factor with more levels than
  # observations per level, counted on the model frame of the fit
  # (rwarn_small_groups(), R/model_context.R).
  rwarn_small_groups(model, tryCatch(stats::model.frame(model), error = function(e) NULL),
                     "Koenker's test", "performKoenkerTest")

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
#' @return Named list of test results in the order provided by `tests`.
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

  results
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
#'   computed.
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
#' *Fixed effects for small groups* in [performKoenkerTest()]).
#'
#' @seealso
#' [clearTestCache()] empties the cache; [runHeteroTests()] uses it with
#' `use_cache = TRUE`.
#' @export
cachedTest <- function(test_name, model, data, ..., use_cache = TRUE) {
  if (!use_cache) {
    return(.run_registered_diagnostic(test_name, model, data, ...))
  }
  inputs <- if (rcache_cacheable(test_name)) rcache_inputs(model, data)
  rcached_diagnostic(test_name, model, data, inputs, ...)
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
