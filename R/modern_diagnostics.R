#' Modern heteroscedasticity diagnostics
#'
#' Collection of advanced heteroscedasticity tests including wild bootstrap,
#' heteroscedasticity-consistent covariance LM statistics, quantile regression
#' comparisons, rank-based permutation checks, high-dimensional projections and
#' spatial correlation diagnostics.
"modern_diagnostics" <- NULL

# Internal helpers -----------------------------------------------------------

.compute_bp_statistic <- function(residuals, predictors, statistic_name = "X-squared") {
  if (!is.numeric(residuals) || length(residuals) == 0L) {
    stop("Residual vector must be numeric and non-empty.", call. = FALSE)
  }
  if (!is.matrix(predictors) && !is.data.frame(predictors)) {
    stop("Predictors must be supplied as a matrix or data frame.", call. = FALSE)
  }
  aux_df <- as.data.frame(predictors)
  n <- length(residuals)
  if (nrow(aux_df) != n) {
    stop("Predictor matrix must align with residuals.", call. = FALSE)
  }
  if (ncol(aux_df) == 0L) {
    stop("Auxiliary regression requires at least one predictor beyond the intercept.", call. = FALSE)
  }
  response <- residuals^2
  if (stats::var(response) <= .Machine$double.eps) {
    .std_error(
      "rassumption_violation",
      assumption = "Auxiliary regression requires variation in squared residuals"
    )
  }
  aux_matrix <- data.frame(response = response, aux_df)
  aux_model <- safe_lm(response ~ ., data = aux_matrix)
  r2 <- summary(aux_model)$r.squared
  stat <- n * r2
  list(
    statistic = stats::setNames(stat, statistic_name),
    df = ncol(aux_df)
  )
}

.pick_predictor <- function(model_matrix) {
  cols <- colnames(model_matrix)
  if (length(cols) <= 1L) {
    return(NULL)
  }
  if (cols[1] == "(Intercept)") {
    cols <- cols[-1]
  }
  cols[1]
}

#' Wild bootstrap heteroscedasticity test
#'
#' Calibrates the Breusch-Pagan Lagrange multiplier statistic with a
#' *null-imposed* wild bootstrap, providing a p-value that is accurate in small
#' samples and under non-normal errors without relying on the asymptotic
#' \eqn{\chi^2} approximation.
#'
#' @details
#' The observed statistic is the usual \eqn{n R^2} from the auxiliary regression
#' of the squared residuals on the regressors. The bootstrap reference
#' distribution, however, must be generated **under the homoscedastic null** ---
#' otherwise it inherits the heteroscedasticity the test is looking for and the
#' procedure loses all power. Each bootstrap sample is therefore built from
#' leverage-standardised, mean-centred residuals \eqn{r_i = e_i/\sqrt{1-h_{ii}}}
#' (which have a common variance under \eqn{H_0}) resampled i.i.d. and perturbed
#' by a wild multiplier \eqn{v_i}, so that \eqn{y_i^* = \hat y_i + r^*_i v_i}. The
#' statistic is recomputed on each refit and the p-value is
#' \eqn{(1 + \#\{T_b^* \ge T\})/(B + 1)}. In the package's validation study of
#' the multiplier, with `B = 999`, the test held its level under \eqn{H_0} with
#' heavy-tailed \eqn{t_5} errors but not with skewed ones: with centred
#' exponential errors and 30, 50 and 100 observations it rejected a true null
#' hypothesis 8.2%, 7.3% and 6.1% of the time at the 5% level with the
#' Rademacher multiplier, and 7.8%, 7.0% and 5.9% with the Mammen multiplier. Under the
#' alternative the observed statistic is extreme relative to the homoscedastic
#' reference, giving power.
#'
#' Each replicate is refitted on the design matrix of the fit, with its
#' offset, whether that was given as an argument or written in the formula.
#' The formula is not evaluated again, so a transformed regressor such as
#' `log(x)`, or a transformed response, is refitted on the scale of the fit.
#'
#' @section Weighted fits:
#' A fit with weights \eqn{w_i} is ordinary least squares on
#' \eqn{\sqrt{w_i}\, y_i}{sqrt(w_i) y_i} and \eqn{\sqrt{w_i}\, x_i}{sqrt(w_i) x_i},
#' whose errors have constant variance under the null hypothesis, and the
#' bootstrap works in that regression. The statistic is computed from its
#' residuals, the Pearson residuals \eqn{\sqrt{w_i}\, e_i}{sqrt(w_i) e_i}, with
#' the auxiliary regressors on their original scale; the residuals that are
#' resampled are the Pearson residuals divided by
#' \eqn{\sqrt{1 - h_{ii}}}{sqrt(1 - h_ii)}, with the leverages of that
#' regression, and centred; and each replicate refits it. With equal weights
#' the result is that of the unweighted fit.
#'
#' Weights given to the fit are known, and they are held fixed in every
#' replicate. On a fit returned by [fitWLS()] they were estimated from the same
#' data, and since 0.15.0 they are estimated again in each replicate by
#' default (`reestimate_weights`). The replicate is fitted as the model
#' `fitWLS()` was given: by least squares, with the weights that model had, if
#' any. The variance function `fitWLS()` recorded, with its form, is then
#' fitted again to the residuals, on the regressors `fitWLS()` evaluated, and
#' the replicate is refitted with the weights it gives. The statistic is
#' computed from the Pearson residuals of that refit, so the reference
#' distribution includes the estimation the observed statistic went through.
#' A replicate whose variance function cannot be fitted, where `fitWLS()`
#' would fall back to equal weights, counts as a failed replicate: the p-value
#' is computed from the replicates that remain, and a warning gives the number
#' that failed.
#'
#' Holding the weights of a `fitWLS()` fit fixed, as every call did before
#' 0.15.0 and `reestimate_weights = FALSE` still does, leaves the estimation
#' out of the replicates, and the test inherits the error of the tests that
#' take the weights as known. In `inst/validation/weighted-fits-size.R`, the
#' data have standard deviation \eqn{\exp(0.3 x)}{exp(0.3 x)}, the variance
#' function is correctly specified, the errors are normal and `B = 499`. At
#' the 5% level the test then rejected 0.086 of the time with 150 observations
#' and 0.105 with 600 when it held the weights fixed, and 0.0605 and 0.0515
#' when it estimated them again. With the true weights it rejected 0.054 and
#' 0.048. At 2000 replications 0.0605 is 2.2 standard errors above 0.05, and
#' outside the release gate's interval, 0.042 to 0.058.
#' [performVarianceFormTest()] allows for the estimation too.
#'
#' @inheritParams performBPTest
#' @param B Integer number of bootstrap replications. Defaults to `999`, which
#'   is the default of every resampling test in the package since 0.15.0; it
#'   was `499` before. The p-value is `(1 + #) / (B_eff + 1)`, so a test at
#'   level `alpha` is exact only when `alpha * (B + 1)` is an integer, which
#'   999 is at the 5% and 1% levels.
#' @param distribution Multiplier distribution used for the wild perturbation.
#'   Supported options are "rademacher" and "mammen".
#' @param progress Logical flag controlling whether progress should be reported
#'   when running the bootstrap loop.
#' @param reestimate_weights Whether each replicate estimates the weights of a
#'   [fitWLS()] fit again from the variance function `fitWLS()` recorded on it.
#'   `NULL`, the default, does so for a `fitWLS()` fit and holds the weights
#'   fixed for any other model. `FALSE` holds them fixed, as every call did
#'   before 0.15.0. `TRUE` is an error for a model that `fitWLS()` did not
#'   return, or on which it fell back to equal weights. Each replicate
#'   regenerates the response and holds the design fixed, so a variance
#'   regressor that uses a variable of the response which the right-hand side
#'   of the model does not use, such as `y` or `x` for `I(y / x) ~ z`, or that
#'   is the response term itself, as `~ .` gives, cannot be estimated again:
#'   `NULL` then holds the weights fixed, with a warning, and `TRUE` is an
#'   error. Giving the response as a column of the data, as in `r ~ z`, says
#'   which variables stay. See *Weighted fits*.
#'
#' @return An object of class \code{htest} containing the observed
#'   Breusch-Pagan statistic, bootstrap p-value, and additional details under the
#'   `bootstrap` element.
#'
#' @references
#' Davidson, R., & Flachaire, E. (2008). The wild bootstrap, tamed at last.
#' *Journal of Econometrics, 146*(1), 162–169.
#'
#' Godfrey, L. G. (2006). Tests for regression models with heteroskedasticity of
#' unknown form. *Computational Statistics & Data Analysis, 50*(10), 2715–2733.
#'
#' Wu, C. F. J. (1986). Jackknife, bootstrap and other resampling methods in
#' regression analysis. *The Annals of Statistics, 14*(4), 1261–1295.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + qsec, data = mtcars)
#' if (interactive()) {
#'   set.seed(123)
#'   performWildBootstrapTest(model, mtcars, B = 199, progress = FALSE)
#' }
#'
#' @export
performWildBootstrapTest <- function(model, data = NULL, B = 999,
                                     distribution = c("rademacher", "mammen"),
                                     progress = interactive(),
                                     reestimate_weights = NULL) {
  rrefuse_non_gaussian_glm(model, "performWildBootstrapTest")
  data <- rdata_or_model_frame(model, data)
  distribution <- match.arg(distribution)
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be a single logical value.", call. = FALSE)
  }
  # The variance function of a fitWLS() fit, estimated again in each
  # replicate, or NULL, which holds the weights fixed as before 0.15.0.
  variance_record <- rwls_reestimation(model, reestimate_weights, "performWildBootstrapTest")

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "Wild bootstrap",
    min_obs_model = 20L,
    min_obs_data = 20L
  )
  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("wild_bootstrap", model = model, data = working_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", sprintf("Running wild bootstrap (%s) heteroscedasticity test", distribution))

  model_frame <- stats::model.frame(model)
  if (nrow(model_frame) != length(residuals)) {
    stop("Model frame does not align with residual vector for wild bootstrap.", call. = FALSE)
  }
  fitted_vals <- stats::fitted(model)
  if (length(fitted_vals) != length(residuals)) {
    stop("Fitted values must align with residuals for wild bootstrap.", call. = FALSE)
  }

  # The design stored with the fit. model.matrix(model, data = model_frame)
  # would evaluate the call of the fit again in its model frame, which holds
  # the terms, log(wt), and not the variables they are computed from, wt, and
  # holds weights and an offset given as arguments as "(weights)" and
  # "(offset)", under which the call cannot find them.
  prior_weights <- rleast_squares_weights(model)
  design_matrix <- stats::model.matrix(model)
  model_offset <- stats::model.offset(model_frame)
  predictors <- if (ncol(design_matrix) > 1L && colnames(design_matrix)[1] == "(Intercept)") {
    design_matrix[, -1, drop = FALSE]
  } else {
    design_matrix
  }
  if (ncol(predictors) == 0L) {
    stop("Wild bootstrap test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  observed <- .compute_bp_statistic(residuals, predictors)

  multiplier_draw <- switch(distribution,
    rademacher = function(n) sample(c(-1, 1), size = n, replace = TRUE),
    mammen = function(n) {
      root5 <- sqrt(5)
      values <- c((1 - root5) / 2, (1 + root5) / 2)
      probs <- c((root5 + 1) / (2 * root5), (root5 - 1) / (2 * root5))
      sample(values, size = n, replace = TRUE, prob = probs)
    }
  )

  # The bootstrap must impose the homoscedastic null, otherwise the reference
  # distribution inherits the very heteroscedasticity the test is looking for and
  # the procedure has no power. We therefore resample *leverage-standardised,
  # mean-centred* residuals i.i.d. (these have constant variance under H0), giving
  # each bootstrap sample a common error variance, and apply the wild multiplier
  # only as a symmetric sign/scale perturbation. See Davidson & Flachaire (2008)
  # and Godfrey (2006) on null-imposed bootstraps for heteroscedasticity tests.
  leverage <- stats::hatvalues(model)
  null_residuals <- residuals / sqrt(pmax(1 - leverage, .Machine$double.eps))
  null_residuals <- null_residuals - mean(null_residuals)

  # A weighted fit is ordinary least squares on sqrt(w) y and sqrt(w) X, and
  # its errors have constant variance there under the null hypothesis. The
  # residuals above are that regression's residuals (the Pearson residuals),
  # and hatvalues() of a weighted fit are its leverages, so the bootstrap
  # works in it throughout. A replicate is its fitted values plus the
  # regenerated errors, refitted by least squares. The fitted values lie in
  # the span of sqrt(w) X, so the residuals of the refit are those of the
  # regenerated errors alone.
  #
  # An unweighted fit is refitted by lm.fit() on the same design, with the
  # offset of the fit, which is the computation lm() makes from the formula
  # without evaluating the formula again. A replicate of a fit whose formula
  # lm() could evaluate in the model frame is the one it always was.
  if (!is.null(prior_weights)) {
    transformed_qr <- qr(sqrt(prior_weights) * design_matrix)
  }
  # On a fitWLS() fit whose weights are estimated again, a replicate is the
  # fit's fitted values plus the regenerated errors, each returned to the
  # scale of its observation by dividing by sqrt(w). It is refitted as
  # fitWLS() fitted the model: by least squares, with the weights of the model
  # fitWLS() was given, if it had any; then the variance function is fitted
  # to its residuals, on the regressors fitWLS() recorded, and the replicate
  # is refitted with the weights it gives. The statistic is computed from the
  # Pearson residuals of that refit. A replicate whose variance function
  # cannot be fitted, where fitWLS() would fall back to equal weights, fails,
  # and the p-value counts the replicates that remain.
  if (!is.null(variance_record)) {
    root_weights <- sqrt(prior_weights)
  }

  progress_bar <- chunk_progress_bar(B, progress && B > 1L, "Wild bootstrap replications")
  on.exit(progress_bar$close(), add = TRUE)

  replicates <- rep(NA_real_, B)
  for (b in seq_len(B)) {
    boot_multiplier <- multiplier_draw(length(residuals))
    boot_errors <- sample(null_residuals, length(residuals), replace = TRUE) * boot_multiplier
    boot_stat <- tryCatch(
      {
        if (!is.null(variance_record)) {
          boot_res <- rwls_replicate_residuals(
            design_matrix, fitted_vals + boot_errors / root_weights, model_offset,
            variance_record
          )
          if (is.null(boot_res)) {
            stop("the variance function of the replicate could not be fitted")
          }
        } else if (is.null(prior_weights)) {
          boot_res <- stats::lm.fit(
            design_matrix, fitted_vals + boot_errors, offset = model_offset
          )$residuals
        } else {
          boot_res <- qr.resid(transformed_qr, boot_errors)
        }
        .compute_bp_statistic(boot_res, predictors)$statistic[[1]]
      },
      error = function(e) NA_real_
    )
    replicates[b] <- boot_stat
    progress_bar$update(b)
  }

  effective <- sum(is.finite(replicates))
  if (!is.null(variance_record)) {
    rwarn_failed_replicates("performWildBootstrapTest", B - effective, B)
  }
  if (effective == 0L) {
    warning("All wild bootstrap replications failed; returning NA results.", call. = FALSE)
    p_value <- NA_real_
  } else {
    p_value <- (sum(replicates >= observed$statistic[[1]], na.rm = TRUE) + 1) / (effective + 1)
  }

  structure(
    list(
      statistic = observed$statistic,
      parameter = c(df = observed$df, B = B),
      p.value = p_value,
      method = sprintf("Wild bootstrap Breusch-Pagan test (%s multipliers)", distribution),
      data.name = deparse(stats::formula(model)),
      bootstrap = list(
        replicates = replicates,
        effective_samples = effective,
        distribution = distribution,
        statistic = "Breusch-Pagan LM"
      )
    ),
    class = "htest"
  )
}

#' Rank-based permutation heteroscedasticity test
#'
#' Performs a non-parametric permutation test based on the rank correlation
#' between absolute residuals and a chosen ordering variable. Significant
#' correlations imply systematic changes in residual spread.
#'
#' @inheritParams performBPTest
#' @param order_by Optional name of a predictor variable used to rank the
#'   observations. Defaults to the first non-intercept term in the model matrix.
#' @param B Number of permutation replications used to approximate the null
#'   distribution. Defaults to 999, the default of every resampling test in
#'   the package.
#' @param progress Logical toggle for progress reporting during permutations.
#'
#' @return An \code{htest} object containing the observed Spearman
#'   correlation and a permutation-based p-value.
#'
#' @references
#' Hollander, M., Wolfe, D. A., & Chicken, E. (2013). *Nonparametric Statistical
#' Methods* (3rd ed.). Wiley.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' set.seed(42)
#' performRankPermutationTest(model, mtcars, B = 199, progress = FALSE)
#'
#' @export
performRankPermutationTest <- function(model, data = NULL, order_by = NULL, B = 999,
                                       progress = interactive()) {
  rrefuse_non_gaussian_glm(model, "performRankPermutationTest")
  data <- rdata_or_model_frame(model, data)
  if (!is.null(order_by)) {
    order_by <- enforce_character_scalar(order_by, "order_by")
  }
  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)
  if (!is.logical(progress) || length(progress) != 1L || is.na(progress)) {
    stop("`progress` must be a single logical value.", call. = FALSE)
  }

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "Rank permutation",
    min_obs_model = 20L,
    min_obs_data = 20L
  )
  working_data <- prepared$data
  residuals <- abs(prepared$residuals)

  requirements <- rvalidateTestRequirements("rank_permutation", model = model, data = working_data)
  rprocessValidationResult(requirements)

  model_matrix <- stats::model.matrix(model, data = working_data)
  predictor_name <- if (is.null(order_by)) .pick_predictor(model_matrix) else order_by
  if (is.null(predictor_name)) {
    stop("Unable to determine ordering variable for permutation test.", call. = FALSE)
  }
  if (predictor_name %in% colnames(working_data)) {
    predictor <- working_data[[predictor_name]]
  } else if (predictor_name %in% colnames(model_matrix)) {
    predictor <- model_matrix[, predictor_name]
  } else {
    stop("Ordering variable not found in data or model matrix.", call. = FALSE)
  }
  if (!is.numeric(predictor)) {
    predictor <- as.numeric(predictor)
  }
  if (stats::var(predictor) <= .Machine$double.eps) {
    .std_error("rassumption_violation", assumption = "Ordering variable must vary across observations")
  }
  valid <- is.finite(predictor) & is.finite(residuals)
  predictor <- predictor[valid]
  residuals <- residuals[valid]
  if (length(residuals) < 5L) {
    stop("Not enough valid observations for permutation test.", call. = FALSE)
  }
  observed <- stats::cor(residuals, predictor, method = "spearman")
  if (is.na(observed)) {
    .std_error("rassumption_violation", assumption = "Correlation could not be computed for permutation test")
  }

  progress_bar <- chunk_progress_bar(B, progress && B > 1L, "Rank permutations")
  on.exit(progress_bar$close(), add = TRUE)
  permuted <- rep(NA_real_, B)
  for (b in seq_len(B)) {
    permuted[b] <- stats::cor(sample(residuals), predictor, method = "spearman")
    progress_bar$update(b)
  }
  effective <- sum(is.finite(permuted))
  if (effective == 0L) {
    warning("All permutations failed; returning NA results.", call. = FALSE)
    p_value <- NA_real_
  } else {
    p_value <- (sum(abs(permuted) >= abs(observed), na.rm = TRUE) + 1) / (effective + 1)
  }

  structure(
    list(
      statistic = c(rho = observed),
      parameter = c(B = B),
      p.value = p_value,
      method = "Rank permutation heteroscedasticity test",
      data.name = deparse(stats::formula(model)),
      permutation = list(replicates = permuted, effective_samples = effective, order_by = predictor_name)
    ),
    class = "htest"
  )
}

#' High-dimensional projection heteroscedasticity test
#'
#' Applies principal component projections to the design matrix and evaluates a
#' Breusch-Pagan style statistic on the reduced representation. The procedure is
#' designed for settings where the number of predictors rivals or exceeds the
#' sample size.
#'
#' @inheritParams performBPTest
#' @param variance_threshold Proportion of predictor variance that the selected
#'   principal components should explain. Defaults to 0.9.
#' @param max_components Optional upper bound on the number of principal
#'   components to retain. Defaults to `min(10, n - 5)`.
#'
#' @return An \code{htest} object summarising the chi-squared statistic
#'   of the auxiliary regression on principal component scores.
#'
#' @references
#' Fan, J., & Lv, J. (2008). Sure independence screening for ultrahigh dimensional
#' feature space. *Journal of the Royal Statistical Society: Series B, 70*(5),
#' 849–911.
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(80 * 20), nrow = 80)
#' beta <- c(rep(1, 5), rep(0, 15))
#' y <- X %*% beta + rnorm(80, sd = 0.5 + 0.2 * scale(X[, 1]))
#' df <- as.data.frame(cbind(y = as.numeric(y), X))
#' model <- lm(y ~ ., data = df)
#' performHighDimensionalTest(model, df)
#'
#' @export
performHighDimensionalTest <- function(model, data = NULL, variance_threshold = 0.9,
                                       max_components = NULL) {
  rrefuse_non_gaussian_glm(model, "performHighDimensionalTest")
  data <- rdata_or_model_frame(model, data)
  if (!is.numeric(variance_threshold) || length(variance_threshold) != 1L ||
    is.na(variance_threshold) || variance_threshold <= 0 || variance_threshold > 1) {
    stop("`variance_threshold` must be in (0, 1].", call. = FALSE)
  }
  if (!is.null(max_components)) {
    max_components <- enforce_integer_scalar(max_components, "max_components", lower = 1L)
  }

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "High-dimensional",
    min_obs_model = 20L,
    min_obs_data = 20L
  )
  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("high_dimensional", model = model, data = working_data)
  rprocessValidationResult(requirements)

  design_matrix <- stats::model.matrix(model, data = working_data)
  predictors <- if (ncol(design_matrix) > 1L && colnames(design_matrix)[1] == "(Intercept)") {
    design_matrix[, -1, drop = FALSE]
  } else {
    design_matrix
  }
  if (ncol(predictors) == 0L) {
    stop("High-dimensional test requires predictors beyond the intercept.", call. = FALSE)
  }
  predictors <- scale(predictors, center = TRUE, scale = FALSE)
  svd_fit <- stats::prcomp(predictors, center = FALSE, scale. = FALSE)
  variance <- svd_fit$sdev^2
  cum_var <- cumsum(variance) / sum(variance)
  k <- which(cum_var >= variance_threshold)[1]
  if (is.na(k)) {
    k <- length(cum_var)
  }
  if (!is.null(max_components)) {
    k <- min(k, max_components)
  }
  k <- max(1L, min(k, ncol(svd_fit$x), nrow(svd_fit$x) - 1L))
  scores <- svd_fit$x[, seq_len(k), drop = FALSE]
  aux_df <- data.frame(res_sq = residuals^2, scores)
  aux_model <- safe_lm(res_sq ~ ., data = aux_df)
  r2 <- summary(aux_model)$r.squared
  statistic <- length(residuals) * r2
  df <- k
  p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "High-dimensional projection test for heteroscedasticity",
      data.name = deparse(stats::formula(model)),
      projection = list(components = k, variance_explained = cum_var[k], threshold = variance_threshold)
    ),
    class = "htest"
  )
}

#' Spatial heteroscedasticity test
#'
#' Evaluates spatial clustering in squared residuals using Moran's I with a
#' permutation reference distribution. Significant positive autocorrelation in
#' the squared residuals is evidence of spatially varying variance.
#'
#' @inheritParams performBPTest
#' @param listw Spatial weights in `spdep` \code{listw} format or a numeric
#'   matrix coercible via \code{spdep::mat2listw()}.
#' @param permutations Number of Monte Carlo permutations used to compute the
#'   reference distribution. Defaults to 999, the default of every resampling
#'   test in the package since 0.15.0; it was 499 before.
#' @param zero.policy Logical flag forwarded to the spatial diagnostic to permit
#'   islands with no neighbours.
#'
#' @return An \code{htest} result with Moran's I statistic applied to
#'   squared residuals.
#'
#' @references
#' Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Kluwer.
#'
#' Bivand, R. S., Pebesma, E., & Gómez-Rubio, V. (2013). *Applied Spatial Data
#' Analysis with R* (2nd ed.). Springer.
#'
#' @examples
#' if (requireNamespace("spdep", quietly = TRUE)) {
#'   data(mtcars)
#'   coords <- cbind(runif(nrow(mtcars)), runif(nrow(mtcars)))
#'   nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 4))
#'   lw <- spdep::nb2listw(nb)
#'   model <- lm(mpg ~ wt + hp, data = mtcars)
#'   performSpatialHeteroTest(model, mtcars, listw = lw, permutations = 199)
#' }
#'
#' @export
performSpatialHeteroTest <- function(model, data = NULL, listw, permutations = 999,
                                     zero.policy = NULL) {
  rrefuse_non_gaussian_glm(model, "performSpatialHeteroTest")
  data <- rdata_or_model_frame(model, data)
  if (!requireNamespace("spdep", quietly = TRUE)) {
    stop("Package 'spdep' is required for the spatial heteroscedasticity test.", call. = FALSE)
  }
  if (missing(listw)) {
    stop("`listw` must be supplied for the spatial heteroscedasticity test.", call. = FALSE)
  }
  if (!is.numeric(permutations) || length(permutations) != 1L || is.na(permutations) || permutations < 1) {
    stop("`permutations` must be a positive integer.", call. = FALSE)
  }
  permutations <- as.integer(permutations)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "Spatial heteroscedasticity",
    min_obs_model = 20L,
    min_obs_data = 20L
  )
  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("spatial_hetero", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (inherits(listw, "listw")) {
    lw <- listw
  } else if (is.matrix(listw)) {
    lw <- spdep::mat2listw(listw)
  } else {
    stop("`listw` must be a 'listw' object or numeric matrix.", call. = FALSE)
  }
  if (is.null(zero.policy)) {
    zero.policy <- TRUE
  }

  res_sq <- residuals^2
  spatial_test <- spdep::moran.mc(res_sq, listw = lw, nsim = permutations, zero.policy = zero.policy)
  spatial_test$method <- "Spatial heteroscedasticity test (Moran's I on squared residuals)"
  spatial_test$data.name <- deparse(stats::formula(model))
  spatial_test$parameter <- c(permutations = permutations)
  spatial_test
}
