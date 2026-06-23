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
    std_error(
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

.hc_adjustment <- function(type, residuals, design_matrix) {
  n <- length(residuals)
  p <- ncol(design_matrix)
  if (n == 0L || p == 0L) {
    stop("Design matrix must contain observations and predictors.", call. = FALSE)
  }
  xtx_inv <- MASS::ginv(crossprod(design_matrix))
  hat_values <- rowSums((design_matrix %*% xtx_inv) * design_matrix)
  guard <- pmax(1 - hat_values, sqrt(.Machine$double.eps))
  type <- toupper(type)
  switch(type,
    HC0 = rep(1, n),
    HC1 = if (n > p) rep(n / (n - p), n) else rep(1, n),
    HC2 = 1 / guard,
    HC3 = 1 / guard^2,
    HC4 = {
      delta <- pmin(4, hat_values * n / p)
      1 / guard^delta
    },
    stop(sprintf("Unsupported HC type '%s'.", type), call. = FALSE)
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
#' \eqn{(1 + \#\{T_b^* \ge T\})/(B + 1)}. Under \eqn{H_0} this controls size even
#' for heavy-tailed errors; under the alternative the observed statistic is
#' extreme relative to the homoscedastic reference, giving power.
#'
#' @inheritParams performBPTest
#' @param B Integer number of bootstrap replications. Defaults to `499`.
#' @param distribution Multiplier distribution used for the wild perturbation.
#'   Supported options are "rademacher" and "mammen".
#' @param progress Logical flag controlling whether progress should be reported
#'   when running the bootstrap loop.
#'
#' @return An object of class \link[stats:htest]{htest} containing the observed
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
performWildBootstrapTest <- function(model, data, B = 499,
                                     distribution = c("rademacher", "mammen"),
                                     progress = interactive()) {
  distribution <- match.arg(distribution)
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

  design_matrix <- stats::model.matrix(model, data = model_frame)
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

  response_col <- names(model_frame)[1]
  base_frame <- model_frame
  progress_bar <- chunk_progress_bar(B, progress && B > 1L, "Wild bootstrap replications")
  on.exit(progress_bar$close(), add = TRUE)

  replicates <- rep(NA_real_, B)
  for (b in seq_len(B)) {
    boot_multiplier <- multiplier_draw(length(residuals))
    boot_errors <- sample(null_residuals, length(residuals), replace = TRUE) * boot_multiplier
    boot_response <- fitted_vals + boot_errors
    boot_frame <- base_frame
    boot_frame[[response_col]] <- boot_response
    boot_stat <- tryCatch(
      {
        boot_model <- safe_lm(stats::formula(model), data = boot_frame)
        boot_res <- stats::residuals(boot_model)
        .compute_bp_statistic(boot_res, predictors)$statistic[[1]]
      },
      error = function(e) NA_real_
    )
    replicates[b] <- boot_stat
    progress_bar$update(b)
  }

  effective <- sum(is.finite(replicates))
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

#' HC-type covariance heteroscedasticity test
#'
#' Uses heteroscedasticity-consistent covariance matrix adjustments (HC0–HC4) to
#' stabilise the auxiliary regression employed by the Breusch-Pagan statistic.
#' Large departures between robust and homoskedastic variance estimates manifest
#' as a significant chi-squared statistic signalling heteroscedastic errors.
#'
#' @inheritParams performBPTest
#' @param type Character string selecting the covariance adjustment. Supported
#'   values are "HC0", "HC1", "HC2", "HC3", and "HC4".
#'
#' @return An \link[stats:htest]{htest} object with the adjusted Lagrange multiplier statistic.
#'
#' @references
#' MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent
#' covariance matrix estimators with improved finite sample properties. *Journal
#' of Econometrics, 29*(3), 305–325.
#'
#' Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
#' standard errors in the linear regression model. *The American Statistician,
#' 54*(3), 217–224.
#'
#' @examples
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' performHCCovarianceTest(model, mtcars, type = "HC3")
#'
#' @export
performHCCovarianceTest <- function(model, data, type = c("HC0", "HC1", "HC2", "HC3", "HC4")) {
  type <- match.arg(type)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = sprintf("HC variance (%s)", type),
    min_obs_model = 15L,
    min_obs_data = 15L
  )
  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("hc_covariance", model = model, data = working_data)
  rprocessValidationResult(requirements)

  design_matrix <- stats::model.matrix(model, data = working_data)
  if (nrow(design_matrix) != length(residuals)) {
    stop("Design matrix rows must match residual length for HC test.", call. = FALSE)
  }
  predictors <- if (ncol(design_matrix) > 1L && colnames(design_matrix)[1] == "(Intercept)") {
    design_matrix[, -1, drop = FALSE]
  } else {
    design_matrix
  }
  if (ncol(predictors) == 0L) {
    stop("HC covariance test requires at least one predictor beyond the intercept.", call. = FALSE)
  }

  adjustment <- .hc_adjustment(type, residuals, design_matrix)
  adjusted_residuals <- residuals^2 * adjustment
  centered <- adjusted_residuals - mean(adjusted_residuals)

  aux_df <- data.frame(hc_response = centered, predictors)
  aux_model <- safe_lm(hc_response ~ ., data = aux_df)
  r2 <- summary(aux_model)$r.squared
  n <- length(residuals)
  df <- ncol(predictors)
  statistic <- n * r2
  p_value <- stats::pchisq(statistic, df = df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = sprintf("Heteroscedasticity-consistent covariance test (%s)", type),
      data.name = deparse(stats::formula(model)),
      estimate = c(mean_adjusted_residual = mean(adjusted_residuals)),
      hc_type = type
    ),
    class = "htest"
  )
}

#' Quantile regression heteroscedasticity test
#'
#' Detects heteroscedasticity by comparing slope coefficients from two quantile
#' regressions. Under homoskedasticity the conditional slope should be stable
#' across quantiles; differences indicate variance that varies with regressors.
#'
#' @inheritParams performBPTest
#' @param taus Numeric vector of quantiles to compare. The first and last values
#'   are used to form the Wald statistic and default to 0.25 and 0.75.
#' @param se_type Standard error estimator passed to
#'   \code{quantreg::summary.rq()}. Defaults to the "nid" approximation.
#'
#' @return An \link[stats:htest]{htest} object summarising the Wald test comparing
#'   quantile-specific slopes.
#'
#' @references
#' Koenker, R., & Bassett, G. (1978). Regression quantiles. *Econometrica,
#' 46*(1), 33–50.
#'
#' Hao, L., & Naiman, D. Q. (2007). *Quantile Regression*. SAGE.
#'
#' @examples
#' if (requireNamespace("quantreg", quietly = TRUE)) {
#'   data(mtcars)
#'   model <- lm(mpg ~ wt + hp, data = mtcars)
#'   performQuantileRegressionTest(model, mtcars)
#' }
#'
#' @export
performQuantileRegressionTest <- function(model, data, taus = c(0.25, 0.75), se_type = c("nid", "rank")) {
  if (!requireNamespace("quantreg", quietly = TRUE)) {
    stop("Package 'quantreg' is required for the quantile regression test.", call. = FALSE)
  }
  if (!is.numeric(taus) || length(taus) < 2L) {
    stop("`taus` must contain at least two quantiles.", call. = FALSE)
  }
  taus <- sort(unique(taus))
  se_type <- match.arg(se_type)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = "Quantile regression",
    min_obs_model = 30L,
    min_obs_data = 30L
  )
  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("quantile_regression", model = model, data = working_data)
  rprocessValidationResult(requirements)

  formula <- stats::formula(model)
  tau_low <- taus[1]
  tau_high <- taus[length(taus)]

  rq_low <- quantreg::rq(formula, tau = tau_low, data = working_data)
  rq_high <- quantreg::rq(formula, tau = tau_high, data = working_data)

  coef_low <- stats::coef(rq_low)
  coef_high <- stats::coef(rq_high)
  common_names <- intersect(names(coef_low), names(coef_high))
  if (length(common_names) == 0L) {
    stop("Unable to align quantile regression coefficients.", call. = FALSE)
  }
  slope_names <- setdiff(common_names, "(Intercept)")
  if (length(slope_names) == 0L) {
    stop("Quantile regression test requires at least one slope coefficient.", call. = FALSE)
  }
  diff_beta <- coef_high[slope_names] - coef_low[slope_names]
  diff_beta <- as.matrix(diff_beta)
  rownames(diff_beta) <- slope_names

  summary_low <- quantreg::summary.rq(rq_low, se = se_type, covariance = TRUE)
  summary_high <- quantreg::summary.rq(rq_high, se = se_type, covariance = TRUE)
  cov_low <- summary_low$cov
  cov_high <- summary_high$cov
  if (is.null(cov_low) || is.null(cov_high)) {
    stop("Quantile regression summary did not return covariance matrices.", call. = FALSE)
  }
  if (is.null(dimnames(cov_low))) {
    dimnames(cov_low) <- list(names(coef_low), names(coef_low))
  }
  if (is.null(dimnames(cov_high))) {
    dimnames(cov_high) <- list(names(coef_high), names(coef_high))
  }
  cov_low <- cov_low[slope_names, slope_names, drop = FALSE]
  cov_high <- cov_high[slope_names, slope_names, drop = FALSE]
  cov_diff <- cov_low + cov_high
  cov_diff <- cov_diff + diag(1e-8, nrow(cov_diff))
  inv_cov <- tryCatch(MASS::ginv(cov_diff), error = function(e) NULL)
  if (is.null(inv_cov)) {
    stop("Unable to invert covariance matrix for quantile test.", call. = FALSE)
  }
  wald_stat <- as.numeric(t(diff_beta) %*% inv_cov %*% diff_beta)
  df <- nrow(diff_beta)
  p_value <- stats::pchisq(wald_stat, df = df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = wald_stat),
      parameter = c(df = df),
      p.value = p_value,
      method = sprintf("Quantile regression heteroscedasticity test (taus %.2f vs %.2f)", tau_low, tau_high),
      data.name = deparse(formula),
      estimate = stats::setNames(as.numeric(diff_beta), rownames(diff_beta)),
      quantiles = taus
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
#'   distribution.
#' @param progress Logical toggle for progress reporting during permutations.
#'
#' @return An \link[stats:htest]{htest} object containing the observed Spearman
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
performRankPermutationTest <- function(model, data, order_by = NULL, B = 999, progress = interactive()) {
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
    std_error("rassumption_violation", assumption = "Ordering variable must vary across observations")
  }
  valid <- is.finite(predictor) & is.finite(residuals)
  predictor <- predictor[valid]
  residuals <- residuals[valid]
  if (length(residuals) < 5L) {
    stop("Not enough valid observations for permutation test.", call. = FALSE)
  }
  observed <- stats::cor(residuals, predictor, method = "spearman")
  if (is.na(observed)) {
    std_error("rassumption_violation", assumption = "Correlation could not be computed for permutation test")
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
#' @return An \link[stats:htest]{htest} object summarising the chi-squared statistic
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
performHighDimensionalTest <- function(model, data, variance_threshold = 0.9, max_components = NULL) {
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
#'   reference distribution.
#' @param zero.policy Logical flag forwarded to the spatial diagnostic to permit
#'   islands with no neighbours.
#'
#' @return An \link[stats:htest]{htest} result with Moran's I statistic applied to
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
performSpatialHeteroTest <- function(model, data, listw, permutations = 499, zero.policy = NULL) {
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
