#' Additional heteroscedasticity tests
#'
#' These functions extend the package with more specialized tests
#' for heteroscedasticity such as a studentized Breusch-Pagan test,
#' a bootstrap version of White's test and the Szroeter ordered test.
#'
#' @name additional_tests
NULL

# Internal helper --------------------------------------------------------------

prepare_model_data_for_test <- function(model, data, required_vars, test_label,
                                        min_obs_model = 10L, min_obs_data = 10L) {
  rvalidateModelInputs(model, test_name = test_label, min_obs = min_obs_model)
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = min_obs_data)

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  residuals <- stats::residuals(model)

  resid_names <- names(residuals)
  data_rows <- rownames(cleaned$data)

  if (!is.null(resid_names) && length(resid_names) > 0 && !is.null(data_rows)) {
    match_idx <- match(resid_names, data_rows)
    if (anyNA(match_idx)) {
      missing_rows <- resid_names[is.na(match_idx)]
      stop(
        sprintf(
          "%s requires `data` to contain the rows used to fit the model. Missing rows: %s",
          test_label,
          paste(utils::head(missing_rows, 3L), collapse = ", ")
        ),
        call. = FALSE
      )
    }
    aligned_data <- cleaned$data[match_idx, , drop = FALSE]
  } else {
    if (nrow(cleaned$data) != length(residuals)) {
      stop(
        sprintf(
          "%s requires `data` with %d observations to match the fitted model, got %d.",
          test_label,
          length(residuals),
          nrow(cleaned$data)
        ),
        call. = FALSE
      )
    }
    aligned_data <- cleaned$data
  }

  list(data = aligned_data, residuals = residuals)
}

#' Studentized Breusch–Pagan test
#'
#' Computes the Koenker–Bassett studentized Lagrange Multiplier statistic for
#' heteroscedasticity by regressing centred squared residuals on the regressors.
#' Compared with the classical Breusch–Pagan test, the studentized version is
#' less sensitive to violations of normality and small-sample bias.
#'
#' @param model A fitted [stats::lm] object providing the residuals and design
#'   matrix for the auxiliary regression.
#' @param data A [base::data.frame] containing the variables used to fit
#'   `model`. It must include all observations referenced by the model object.
#'
#' @return An object of class \code{htest} reporting the chi-squared statistic
#'   and p-value for the null hypothesis of constant error variance.
#'
#' @details
#' Following Koenker (1981) and the implementation in
#' \link[lmtest:bptest]{lmtest::bptest()}, the procedure fits an auxiliary
#' regression of \eqn{e_i^2 - \hat{\sigma}^2} on the regressors from the
#' original model (including the intercept), where \eqn{e_i} denotes the weighted
#' residuals and \eqn{\hat{\sigma}^2} their mean squared error. Under
#' homoskedasticity the statistic
#' \eqn{T = n \sum w_i \hat{g}_i^2 / \sum (e_i^2 - \hat{\sigma}^2)^2} is
#' asymptotically chi-squared with degrees of freedom equal to the number of
#' regressors beyond the intercept. The implementation shares the validation
#' helpers used across the package to ensure that: (i) the model and data satisfy
#' minimum sample-size thresholds via \link[=rvalidateModelInputs]{rvalidateModelInputs()} and
#' \link[=rvalidateDataInputs]{rvalidateDataInputs()}, (ii) missing values are handled by
#' \link[=rhandleMissingValues]{rhandleMissingValues()}, and (iii) studentized-residual specific
#' requirements registered in \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} are
#' met.
#'
#' @references
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
#' *Journal of Econometrics, 17*(1), 107–112.
#' \doi{10.1016/0304-4076(81)90062-2}
#'
#' Davidson, R., & MacKinnon, J. G. (2004). *Econometric Theory and Methods*.
#' Oxford University Press. Section 16.7 discusses LM tests for heteroscedasticity
#' including studentized variants.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performStudentizedBPTest(mod, mtcars)
#'
#' # Detect heteroscedasticity driven by a single regressor
#' set.seed(321)
#' x <- runif(180)
#' y <- 5 - 1.5 * x + rnorm(180, sd = 0.4 + 0.6 * x)
#' df <- data.frame(y, x)
#' performStudentizedBPTest(lm(y ~ x, data = df), df)
#'
#' @seealso
#' [performBPTest()] for the classical LM statistic and [performKoenkerTest()] for
#' the absolute-residual variant. The robust workflow in
#' \link[=performBPTestRobust]{performBPTestRobust()} augments the studentized statistic with bootstrap
#' diagnostics.
#' @export
performStudentizedBPTest <- function(model, data) {
  test_label <- "Studentized BP"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L
  )

  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("studentized_bp", model = model, data = working_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Studentized Breusch-Pagan test")

  model_frame <- stats::model.frame(model)
  if (nrow(model_frame) == 0) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires observations in the model frame"
    )
  }

  response <- stats::model.response(model_frame)
  design_matrix <- stats::model.matrix(model, data = model_frame)

  if (ncol(design_matrix) < 2L) {
    stop(
      "Studentized BP test requires at least an intercept and one regressor.",
      call. = FALSE
    )
  }

  weights <- stats::model.weights(model_frame)
  if (is.null(weights)) {
    weights <- rep.int(1, nrow(design_matrix))
  }

  if (length(weights) != nrow(design_matrix)) {
    stop(
      "Studentized BP test detected a mismatch between weights and the model matrix.",
      call. = FALSE
    )
  }

  positive_weights <- weights > 0
  effective_n <- sum(positive_weights)
  if (effective_n <= ncol(design_matrix)) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires more positive-weight observations than regressors"
    )
  }

  base_fit <- stats::lm.wfit(design_matrix, response, weights)
  residuals <- base_fit$residuals

  if (length(residuals) != nrow(working_data)) {
    stop("Studentized BP test could not align residuals with the working data.", call. = FALSE)
  }

  sigma2 <- sum(weights * residuals^2) / effective_n
  if (!is.finite(sigma2) || sigma2 <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires residual variance to be positive"
    )
  }

  centered_sq <- residuals^2 - sigma2
  if (!any(abs(centered_sq) > .Machine$double.eps)) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP test requires variability in centred squared residuals"
    )
  }

  aux_fit <- stats::lm.wfit(design_matrix, centered_sq, weights)
  df <- aux_fit$rank - 1L
  if (df <= 0) {
    stop("Studentized BP auxiliary regression has insufficient rank.", call. = FALSE)
  }

  numerator <- sum(weights * aux_fit$fitted.values^2)
  denominator <- sum(centered_sq^2)
  if (!is.finite(numerator) || !is.finite(denominator) || denominator <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Studentized BP auxiliary regression produced non-finite values"
    )
  }

  test_statistic <- effective_n * numerator / denominator
  p_value <- stats::pchisq(test_statistic, df, lower.tail = FALSE)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "Studentized Breusch-Pagan test",
      data.name = deparse(substitute(model)),
      alternative = "heteroscedasticity present"
    ),
    class = "htest"
  )
}

#' Bootstrap White test
#'
#' Approximates the finite-sample distribution of White's LM statistic by
#' resampling the fitted residuals, refitting the model, and recalculating the
#' test statistic across many bootstrap replications.
#'
#' @inheritParams performWhiteTest
#' @param B Integer scalar giving the number of bootstrap replications. Larger
#'   values yield smoother p-value estimates at the cost of additional runtime.
#' @param parallel Logical scalar; if `TRUE` the bootstrap loop is executed with
#'   [parallel::mclapply()] when the `parallel` package and forked processing are
#'   available.
#'
#' @return A \code{htest} object reporting both the asymptotic White statistic
#'   and its bootstrap p-value. The returned object also stores the simulated test
#'   statistics in `boot_statistics` for further inspection.
#'
#' @details
#' For a fitted model \eqn{\hat{y} = X\hat{\beta}}, the algorithm proceeds as
#' follows:
#' \enumerate{
#'   \item Compute White's LM statistic \eqn{n R^2} from the auxiliary regression
#'     on squares and cross-products of the regressors.
#'   \item Generate \eqn{B} bootstrap samples by resampling the centred residuals
#'     with replacement, forming \eqn{y^{*(b)} = \hat{y} + \hat{e}^{*(b)}}.
#'   \item Refit the model to each bootstrap sample and recompute White's
#'     statistic \eqn{T^{*(b)}} using the same auxiliary specification.
#'   \item Estimate the bootstrap p-value as
#'     \eqn{\hat{p} = B^{-1} \sum_{b = 1}^B I\{T^{*(b)} \ge T_{\text{obs}}\}}.
#' }
#'
#' The bootstrap distribution offers improved size control for moderate sample
#' sizes or high-dimensional designs where the chi-squared approximation may be
#' inaccurate. When `parallel = TRUE` the resampling step exploits available CPU
#' cores to reduce computation time.
#'
#' @references
#' Efron, B., & Tibshirani, R. J. (1993). *An Introduction to the Bootstrap*.
#' Chapman & Hall/CRC.
#'
#' Davidson, R., & MacKinnon, J. G. (2006). The power of bootstrap and asymptotic
#' tests. *Journal of Econometrics, 133*(2), 421–441.
#' \doi{10.1016/j.jeconom.2005.02.002}
#'
#' @examples
#' # The bootstrap needs at least 50 observations, so mtcars (32) is too small.
#' set.seed(42)
#' n <- 120
#' sim <- data.frame(x = runif(n, 1, 5))
#' sim$y <- 1 + 2 * sim$x + rnorm(n, sd = 0.3 + 0.6 * sim$x)
#' mod <- lm(y ~ x, data = sim)
#' performWhiteTestBootstrap(mod, sim, B = 199)
#'
#' \donttest{
#' # More replications give a smoother p-value estimate
#' performWhiteTestBootstrap(mod, sim, B = 999)
#'
#' # Enable parallel processing when supported by the operating system
#' if (.Platform$OS.type != "windows") {
#'   performWhiteTestBootstrap(mod, sim, B = 199, parallel = TRUE)
#' }
#' }
#'
#' @seealso
#' [performWhiteTest()] for the classical statistic and
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()} for enriched diagnostics.
#' @export
performWhiteTestBootstrap <- function(model, data, B = 1000, parallel = FALSE) {
  test_label <- "Bootstrap White"

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 20L,
    min_obs_data = 20L
  )

  working_data <- prepared$data

  requirements <- rvalidateTestRequirements("bootstrap_tests", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!is.numeric(B) || length(B) != 1L || is.na(B) || B < 1) {
    stop("`B` must be a positive integer.", call. = FALSE)
  }
  B <- as.integer(B)

  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be a single logical value.", call. = FALSE)
  }

  ht_log("INFO", "Running Bootstrap White test")

  original_result <- performWhiteTest(model, working_data)
  original_stat <- unname(original_result$statistic[1])

  bootstrap_stat <- function() {
    fitted_vals <- stats::fitted(model)
    resid <- stats::residuals(model)
    boot_resid <- sample(resid, replace = TRUE)
    boot_y <- fitted_vals + boot_resid

    boot_data <- working_data
    response_name <- as.character(stats::formula(model))[2]
    boot_data[[response_name]] <- boot_y

    boot_model <- safe_lm(stats::formula(model), data = boot_data)
    unname(performWhiteTest(boot_model, boot_data)$statistic[1])
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    boot_stats <- parallel::mclapply(
      seq_len(B),
      function(i) bootstrap_stat(),
      mc.cores = max(1, parallel::detectCores() - 1)
    )
    boot_stats <- unlist(boot_stats, use.names = FALSE)
  } else {
    boot_stats <- replicate(B, bootstrap_stat())
  }

  # Finite-simulation convention (Davison & Hinkley 1997, sec. 4.2):
  # p = (1 + #{T_b >= T_obs}) / (B_eff + 1). The raw proportion can return
  # exactly zero, which is not an attainable p-value from a finite number of
  # replicates, and it is anti-conservative in the tail.
  finite_stats <- boot_stats[is.finite(boot_stats)]
  effective <- length(finite_stats)
  if (effective == 0L) {
    std_error(
      "rassumption_violation",
      assumption = "Bootstrap White test produced no usable replicates"
    )
  }
  p_value <- (1 + sum(finite_stats >= original_stat)) / (effective + 1)

  structure(
    list(
      statistic = original_result$statistic,
      parameter = c(B = B),
      p.value = p_value,
      method = "Bootstrap White test",
      data.name = deparse(substitute(model)),
      boot_statistics = boot_stats
    ),
    class = "htest"
  )
}

#' Szroeter test for ordered alternatives
#'
#' Detects monotonic changes in variance when observations can be meaningfully
#' ordered—typically by time or another covariate—using the cumulative weighting
#' scheme proposed by Szroeter (1978).
#'
#' @inheritParams performHarveyTest
#' @param data A [base::data.frame] (or compatible tibble) containing the
#'   variables used to fit `model` and the ordering variable supplied via
#'   `order_by`.
#' @param order_by Character scalar naming the column that defines the ordering of
#'   observations prior to computing the statistic. The column must be numeric or
#'   coercible to an orderable vector.
#' @param alternative Character scalar specifying the alternative hypothesis:
#'   `"greater"` (the default) tests for variance increasing with `order_by`,
#'   `"less"` for variance decreasing, and `"two.sided"` for a change in either
#'   direction. Szroeter's test targets monotone alternatives, so the one-sided
#'   form is the usual choice.
#'
#' @return An object of class \code{htest} reporting Szroeter's
#'   standardised statistic `Q`, the sample size, the underlying rank-weighted
#'   statistic `h` in `estimate`, and the p-value from the asymptotic normal
#'   reference distribution.
#'
#' @details
#' After ordering the residuals \eqn{\hat{e}_{(i)}} by `order_by`, the test forms
#' the rank-weighted average of the squared residuals
#' \deqn{h = \frac{\sum_{i = 1}^n i \, \hat{e}_{(i)}^2}{\sum_{i = 1}^n \hat{e}_{(i)}^2},}
#' which is Szroeter's (1978) class of statistics evaluated at the canonical
#' weights \eqn{h_i = i}. Under homoskedasticity \eqn{h} is centred on the mean
#' rank \eqn{(n + 1) / 2} with variance \eqn{(n^2 - 1) / (6n)}, so
#' \deqn{Q = \frac{h - (n + 1) / 2}{\sqrt{(n^2 - 1) / (6n)}}}
#' is asymptotically standard normal. Variance that grows with `order_by` shifts
#' weight onto the high ranks and drives \eqn{Q} upwards.
#'
#' The implementation relies on the package validation helpers to align the
#' supplied data with the model residuals, check that the ordering variable is
#' present, and ensure sufficient sample size and variability in the squared
#' residuals.
#'
#' @section Validation:
#' The statistic and its null variance are checked against an independent
#' reconstruction of Szroeter (1978) in
#' `tests/testthat/test-pass-a-reference.R`. Releases before 0.7.0 divided
#' the centred statistic by a further \eqn{\sqrt{n}}, shrinking \eqn{Q} by a
#' factor of roughly \eqn{2 / \sqrt{n}} and leaving the test with essentially no
#' power against any alternative; see `NEWS.md`.
#'
#' @references
#' Szroeter, J. (1978). A class of parametric tests for heteroscedasticity in
#' linear econometric models. *Econometrica, 46*(6), 1311–1327.
#' \doi{10.2307/1913833}
#'
#' Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*. Cambridge
#' University Press. Section 5.4 outlines the Szroeter test.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performSzroeterTest(mod, mtcars, order_by = "wt")
#'
#' # Detect ordered heteroscedasticity in simulated data with variance increasing
#' # in an index variable
#' set.seed(404)
#' n <- 150
#' x <- sort(runif(n))
#' y <- 2 + 0.5 * x + rnorm(n, sd = 0.4 + 0.8 * x)
#' df <- data.frame(y, x)
#' performSzroeterTest(lm(y ~ x, data = df), df, order_by = "x")
#'
#' # Two-sided version when the direction of the change is not known in advance
#' performSzroeterTest(mod, mtcars, order_by = "wt", alternative = "two.sided")
#'
#' @seealso
#' [performKoenkerTest()] for the regressor-based alternative and
#' [performGQTest()] for split-sample diagnostics on ordered data.
#' @export
performSzroeterTest <- function(model, data, order_by,
                                alternative = c("greater", "two.sided", "less")) {
  test_label <- "Szroeter test"

  if (!is.character(order_by) || length(order_by) != 1L || is.na(order_by) || !nzchar(order_by)) {
    stop("`order_by` must be supplied as a single column name.", call. = FALSE)
  }

  alternative <- match.arg(alternative)

  model_terms <- stats::terms(model)
  required_vars <- unique(c(all.vars(model_terms), order_by))
  prepared <- prepare_model_data_for_test(
    model,
    data,
    required_vars = required_vars,
    test_label = test_label,
    min_obs_model = 15L,
    min_obs_data = 15L
  )

  working_data <- prepared$data
  residuals <- prepared$residuals

  requirements <- rvalidateTestRequirements("szroeter", model = model, data = working_data)
  rprocessValidationResult(requirements)

  if (!order_by %in% names(working_data)) {
    std_error("missing_variable", variable = order_by)
  }

  ht_log("INFO", "Running Szroeter test")

  ord <- order(working_data[[order_by]])
  e_ordered <- residuals[ord]
  n <- length(e_ordered)

  if (n <= 1) {
    std_error(
      "rinsufficient_sample_size",
      test_name = test_label,
      min_obs = 2L,
      n_obs = n
    )
  }

  if (stats::var(e_ordered) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Szroeter test requires variability in ordered residuals"
    )
  }

  # Szroeter's (1978) h statistic with the canonical weights h_i = i: a
  # rank-weighted average of the squared residuals in the supplied ordering.
  ranks <- seq_len(n)
  h_stat <- sum(ranks * e_ordered^2) / sum(e_ordered^2)

  # Under homoskedasticity h has mean (n + 1) / 2 (the mean rank) and variance
  # (n^2 - 1) / (6 n), giving an asymptotically standard normal Q. Note the
  # sqrt(n) in the scale factor: h is a weighted mean of the ranks, so its
  # dispersion grows with n and the centred statistic must NOT be divided by a
  # further sqrt(n).
  q_stat <- (h_stat - (n + 1) / 2) / sqrt((n^2 - 1) / (6 * n))

  p_value <- switch(
    alternative,
    "greater" = stats::pnorm(q_stat, lower.tail = FALSE),
    "less" = stats::pnorm(q_stat),
    "two.sided" = 2 * stats::pnorm(-abs(q_stat))
  )

  alternative_label <- switch(
    alternative,
    "greater" = sprintf("variance increases with '%s'", order_by),
    "less" = sprintf("variance decreases with '%s'", order_by),
    "two.sided" = sprintf("variance changes monotonically with '%s'", order_by)
  )

  structure(
    list(
      statistic = c(Q = q_stat),
      parameter = c(n = n),
      estimate = c(h = h_stat),
      p.value = p_value,
      method = "Szroeter test for ordered heteroscedasticity",
      data.name = deparse(substitute(model)),
      alternative = alternative_label
    ),
    class = "htest"
  )
}
