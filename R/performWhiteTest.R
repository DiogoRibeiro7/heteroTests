#' Perform White's test for heteroscedasticity (corrected implementation)
#'
#' Conducts the classic White (1980) Lagrange Multiplier test for unknown forms
#' of heteroscedasticity in linear regression models. The implementation builds
#' the full auxiliary regression with squares and cross-products of the
#' regressors and integrates the package's validation helpers to guarantee that
#' the expanded design matrix is well-posed before fitting the test equation.
#'
#' @param model A fitted [stats::lm] object representing the mean specification
#'   to be diagnosed.
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
#' @param cross_products Logical scalar indicating whether to include all
#'   pairwise cross-products of the regressors in the auxiliary regression.
#'   Defaults to `TRUE` and should remain enabled unless dimensionality makes
#'   the regression unstable.
#' @param max_interactions Single positive integer giving the maximum number of
#'   original predictors for which cross-products are generated. When the number
#'   of regressors exceeds this threshold, cross-products are dropped to avoid
#'   explosive growth in columns. Defaults to `10`.
#'
#' @return An object of class \code{htest} containing the chi-squared test
#'   statistic, associated degrees of freedom, and p-value.
#'
#' @details
#' Let \eqn{\hat{e}} be the residuals from the fitted model. White's test fits an
#' auxiliary regression of \eqn{\hat{e}^2} on the original regressors, their
#' squares, and (optionally) their cross-products. Under the null hypothesis of
#' homoskedasticity and mild regularity conditions, the statistic
#' \eqn{n \times R^2} from this auxiliary model converges to a chi-squared
#' distribution whose degrees of freedom are the rank of the auxiliary
#' regression less its intercept (see *Degrees of freedom*). The function:
#' \enumerate{
#'   \item validates the model object with \link[=rvalidateModelInputs]{rvalidateModelInputs()} to ensure at
#'     least 20 observations and finite residuals;
#'   \item reads the response, design, residuals and weights from the fit (the
#'     model context, since 0.16.0), matches the rows of `data` to the
#'     observations of the fit, and warns about rows of `data` with missing
#'     values, which the fit left out;
#'   \item drops the columns of the auxiliary design that are linearly
#'     dependent on the intercept and the other columns; and
#'   \item applies \link[=rvalidateTestRequirements]{rvalidateTestRequirements()} so that sample-size and
#'     conditioning rules specific to the White test are satisfied.
#' }
#'
#' Rejecting the null indicates that the variance of the errors depends on one
#' or more functions of the regressors. While the statistic is asymptotically
#' valid under heteroskedasticity of unknown form, finite-sample performance can
#' deteriorate when the number of regressors is large relative to the sample
#' size. In such situations consider disabling `cross_products` or switching to
#' the robust variants provided by the package.
#'
#' @section Degrees of freedom:
#' Let \eqn{Z} hold the regressors, their squares and their cross-products.
#' The auxiliary regression of \eqn{\hat{e}^2} on \eqn{Z} has an intercept, and
#' the degrees of freedom are the rank of \eqn{[1, Z]} minus one, as
#' `lmtest::bptest()` counts them given the same terms as `varformula`. A column
#' of \eqn{Z} that is a linear combination of the intercept and the columns
#' before it, at the default tolerance of [qr()], is dropped. That happens
#' when the columns of \eqn{Z} span the constant: the squares of the dummies of
#' `contr.sum` or `contr.helmert` contrasts, the dummies of every level of a
#' factor in a model without an intercept, an interaction with an empty cell
#' such as that of the baseline levels, or the squares of an orthogonal
#' `poly()` basis. In 0.15.0 the degrees of freedom were the rank of
#' \eqn{Z} alone, one more on such a design, with the same statistic and a
#' larger p-value: on `lm(y ~ x1 + f, contrasts = list(f = "contr.sum"))`,
#' with a factor of two levels, 5 where `lmtest::bptest()` gives 4. On a
#' design whose auxiliary columns do not span the constant, such as continuous
#' regressors or factors with the default treatment contrasts, nothing
#' changed. The same holds with `cross_products = FALSE` or more regressors
#' than `max_interactions`, and for the tests computed through this function:
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()}, whose interval,
#' effect size and power follow the degrees of freedom, and the registered
#' `"white"` of [runHeteroTests()]. The p-value of
#' [performWhiteTestBootstrap()] depends on the statistic alone and does not
#' change on such a design; [performWhiteTestStreaming()] stops on such a
#' design, whose auxiliary columns are collinear with its intercept, and
#' otherwise gives these degrees of freedom. On an auxiliary design whose
#' columns are nearly collinear, as the squares and products of the basis of
#' a smooth term can be, the columns kept are those `lm(e^2 ~ Z)` keeps,
#' which can differ from those kept in 0.15.0, so the statistic can change
#' too: there the auxiliary regression of 0.15.0 could silently lose a column
#' to its intercept while its degrees of freedom still counted it, as on a
#' `mgcv::gam(y ~ s(x1) + x2)` fit whose statistic moves by 0.28\%, to the
#' value of `lmtest::bptest()` with the same terms.
#'
#' @section Weighted fits:
#' On a fit with weights the test is computed from the Pearson residuals
#' \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see
#' the section on weighted fits in [performKoenkerTest()], which also covers
#' weights estimated by [fitWLS()].
#'
#' @references
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
#' and a direct test for heteroskedasticity. *Econometrica, 48*(4), 817–838.
#' <https://doi.org/10.2307/1912934>
#'
#' Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson. Chapter 11
#' provides an accessible overview of Lagrange Multiplier diagnostics such as
#' the White test.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTest(mod, mtcars)
#'
#' # Disable cross-products when dimensionality is modest but collinearity is a concern
#' performWhiteTest(mod, mtcars, cross_products = FALSE)
#'
#' # Detect heteroscedasticity in simulated data with variance increasing in x
#' set.seed(123)
#' n <- 200
#' x <- runif(n)
#' y <- 1 + 2 * x + rnorm(n, sd = 0.5 + x)
#' df <- data.frame(y, x)
#' hetero_mod <- lm(y ~ x, data = df)
#' performWhiteTest(hetero_mod, df)
#'
#' @seealso
#' [performWhiteTestBootstrap()] for a resampled version suited to small samples,
#' \link[=performWhiteTestRobust]{performWhiteTestRobust()} for enriched reporting, and [performBPTest()] for a
#' lower-dimensional Lagrange Multiplier alternative.
#' @export
performWhiteTest <- function(model, data = NULL, cross_products = TRUE, max_interactions = 10) {
  test_label <- "White test"
  # The response, design, weights and residuals come from the fit; `data`, when
  # given, is matched to its observations and checked (R/model_context.R).
  ctx <- rmodel_context(model, data, fn = "performWhiteTest", test_label = test_label)
  # A rank-deficient fit is tested on the estimable part of its design: as the
  # same model fitted without its aliased columns (rnon_aliased_columns()).
  rvalidateModelInputs(model, test_name = "White", min_obs = 20L, allow_aliased = TRUE)
  rvalidate_model_context(ctx)

  handle_validation_result <- function(result) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      stop(paste(unique(result$messages), collapse = "\n"), call. = FALSE)
    }
    invisible(result)
  }

  rwarn_incomplete_rows(data, model)
  residuals <- rcontext_pearson(ctx)
  # The rows of `data` that hold the observations, in the order of the fit,
  # or the model frame: what the checks below read.
  working_data <- rcontext_rows(ctx, data)

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  handle_validation_result(requirements)

  if (!is.logical(cross_products) || length(cross_products) != 1) {
    .std_error("invalid_logical", arg = "cross_products")
  }

  # Memory and performance warnings
  size_mb <- tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NA_real_
  )
  if (!is.na(size_mb) && size_mb > 100) {
    ht_log("INFO", sprintf("Switching to streaming White test for dataset of %.1f MB", size_mb))
    adaptive_chunk <- max(1000L, min(25000L, as.integer(nrow(working_data) / 5L)))
    adaptive_chunk <- min(adaptive_chunk, nrow(working_data))
    return(performWhiteTestStreaming(
      model,
      working_data,
      chunk_size = adaptive_chunk,
      cross_products = cross_products,
      max_interactions = max_interactions,
      progress = FALSE
    ))
  }
  if (nrow(working_data) > 10000) {
    message("Large dataset (", nrow(working_data), " observations). ",
            "This may take some time to compute.")
  }

  ht_log("INFO", "Running White test")

  # The design of the fit (with its intercept), without the columns of a
  # rank-deficient fit that it aliased
  X_full <- rcontext_design(ctx)
  rwhite_statistic(residuals, X_full, cross_products, max_interactions, test_label,
                   data_name = deparse(substitute(model)))
}

# White's statistic from the residuals the test reads and the design of the
# auxiliary regression, with its intercept. performWhiteTest() gives it the
# design of the fit (the model context); rwhite_test_rebuilt_design(), behind
# the White bootstrap until increment 8 of the model context, the design
# rebuilt from `data` as before 0.16.0.
rwhite_statistic <- function(residuals, X_full, cross_products, max_interactions, test_label,
                             data_name) {
  # Extract model components
  e_squared <- residuals^2
  n <- length(e_squared)

  if (nrow(X_full) != n) {
    stop(
      sprintf(
        "%s could not align the design matrix with model residuals (expected %d rows, got %d).",
        test_label,
        n,
        nrow(X_full)
      ),
      call. = FALSE
    )
  }

  # Remove intercept column for auxiliary regression
  if (colnames(X_full)[1] == "(Intercept)") {
    X <- X_full[, -1, drop = FALSE]
  } else {
    X <- X_full
  }

  p <- ncol(X)  # Number of original regressors (excluding intercept)

  if (p == 0) {
    stop("No regressors found in model (intercept-only model)")
  }

  regressor_names <- colnames(X)

  # Build auxiliary regression matrix
  # Start with original regressors
  aux_matrix <- X
  aux_names <- regressor_names

  # Add squared terms
  for (j in seq_len(p)) {
    aux_matrix <- cbind(aux_matrix, X[, j]^2)
    aux_names <- c(aux_names, paste0(regressor_names[j], "_sq"))
  }

  # Add cross-product terms if requested
  if (cross_products && p > 1) {

    # Check if we should include cross-products based on dimensionality
    if (p > max_interactions) {
      .std_warning("cross_products_omitted")
      message("Cross-products omitted due to high dimensionality (p = ", p,
              " > max_interactions = ", max_interactions, ")")
    } else {
      # Add all pairwise cross-products X_i * X_j for i < j
      for (i in seq_len(p - 1L)) {
        for (j in seq.int(i + 1L, p)) {
          cross_product <- X[, i] * X[, j]
          aux_matrix <- cbind(aux_matrix, cross_product)
          aux_names <- c(aux_names, paste0(regressor_names[i], "_x_", regressor_names[j]))
        }
      }
    }
  }

  # Set column names for the auxiliary matrix
  colnames(aux_matrix) <- aux_names

  # Drop linearly dependent auxiliary columns rather than aborting. Factor dummies
  # are a common cause: a 0/1 indicator squared equals itself, so its "_sq" column
  # is perfectly collinear with the original. The auxiliary regression below has
  # an intercept, so a column is dependent when it is a combination of the
  # intercept and the columns before it: the columns of Z can span the constant,
  # as the squares of contr.sum or Helmert dummies, the dummies of every level
  # of a factor without an intercept, or the squares of an orthogonal poly()
  # basis do, and a column of Z that adds only the constant adds nothing to the
  # regression. The columns kept are those that QR pivoting of [1, Z] keeps, at
  # the default tolerance of qr(), and the degrees of freedom are the rank of
  # [1, Z] minus one, the rank of the auxiliary regression less its intercept,
  # as in lmtest::bptest(). In 0.15.0 the rank was that of Z alone, one too
  # many when Z spans the constant; the statistic was the same, but on a
  # nearly collinear Z, where QR pivoting with and without the intercept can
  # keep different columns.
  qr_aux <- qr(cbind(1, aux_matrix))
  aux_rank <- qr_aux$rank - 1L
  if (aux_rank < ncol(aux_matrix)) {
    # The intercept is the first column and is never pivoted out.
    keep <- setdiff(sort(qr_aux$pivot[seq_len(qr_aux$rank)]), 1L) - 1L
    dropped <- ncol(aux_matrix) - aux_rank
    aux_matrix <- aux_matrix[, keep, drop = FALSE]
    aux_names <- aux_names[keep]
    ht_log("INFO", sprintf(
      paste(
        "White test: dropped %d auxiliary term(s) collinear with the intercept",
        "and the other terms; using df = %d."
      ),
      dropped, aux_rank
    ))
  }
  if (ncol(aux_matrix) == 0L) {
    .std_error(
      "rassumption_violation",
      assumption = "Auxiliary regression has no independent regressors after removing collinear terms"
    )
  }

  # Convert to data frame for regression
  aux_data <- as.data.frame(aux_matrix)

  # Run auxiliary regression: e² ~ regressors + squares + cross-products
  aux_model <- tryCatch({
    lm(e_squared ~ ., data = aux_data)
  }, error = function(e) {
    stop("Auxiliary regression failed: ", e$message,
         "\nThis may be due to perfect multicollinearity or other numerical issues.")
  })

  # Calculate test statistic
  r_squared <- summary(aux_model)$r.squared

  # Handle numerical edge cases
  if (is.na(r_squared) || r_squared < 0) {
    warning("Invalid R-squared from auxiliary regression. Setting to 0.")
    r_squared <- 0
  }
  if (r_squared > 1) {
    warning("R-squared > 1 from auxiliary regression. Setting to 1.")
    r_squared <- 1
  }

  test_statistic <- n * r_squared
  # The rank of [1, Z] minus one: the number of auxiliary regressors kept.
  df <- aux_rank
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)

  # Create result object
  result <- structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = "White's test for heteroscedasticity",
      data.name = data_name,
      alternative = "heteroscedasticity present"
    ),
    class = "htest"
  )

  # Add diagnostic information
  attr(result, "auxiliary_regressors") <- ncol(aux_matrix)
  attr(result, "original_regressors") <- p
  attr(result, "cross_products_included") <- cross_products && p > 1 && p <= max_interactions
  attr(result, "r_squared_auxiliary") <- r_squared

  ht_log("INFO", paste("White test completed: statistic =", round(test_statistic, 4),
                       "df =", df, "p =", round(p_value, 4)))

  return(result)
}

# White's test as it was computed before the model context: `data`, or the
# model frame, prepared by rdata_or_model_frame(), rhandleMissingValues() and
# a match by row name, and the design rebuilt from it with
# model.matrix(model, data = working_data). performWhiteTestBootstrap() uses
# it for its observed statistic and for its replicates, which it refits from
# the formula, so that both come from the same kind of design and its results
# are those of 0.15.0. Increment 8 of the model context, which builds the
# replicates on the stored design, removes this function (docs/design/
# model-context.md, "Migration plan").
rwhite_test_rebuilt_design <- function(model, data = NULL, cross_products = TRUE,
                                       max_interactions = 10) {
  rrefuse_non_gaussian_glm(model, "performWhiteTest")
  data <- rdata_or_model_frame(model, data)
  test_label <- "White test"
  rvalidateModelInputs(model, test_name = "White", min_obs = 20L, allow_aliased = TRUE)

  model_terms <- stats::terms(model)
  required_vars <- unique(all.vars(model_terms))
  rvalidateDataInputs(data, required_vars = required_vars, min_obs = 20L)

  handle_validation_result <- function(result) {
    if (length(result$warnings) > 0) {
      for (msg in unique(result$warnings)) {
        warning(msg, call. = FALSE)
      }
    }
    if (!isTRUE(result$passed)) {
      stop(paste(unique(result$messages), collapse = "\n"), call. = FALSE)
    }
    invisible(result)
  }

  align_to_model <- function(clean_data, residuals) {
    resid_names <- names(residuals)
    data_rows <- rownames(clean_data)

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
      # `data` and `cleaned` are those of the enclosing call: the rows before
      # and after rhandleMissingValues().
      rcheck_rows_after_cleaning(
        data, cleaned, match_idx, resid_names,
        fn = rtest_name(sys.parent()), test_label = test_label
      )
      aligned_data <- clean_data[match_idx, , drop = FALSE]
      list(data = aligned_data, residuals = residuals)
    } else {
      if (nrow(clean_data) != length(residuals)) {
        stop(
          sprintf(
            "%s requires `data` with %d observations to match the fitted model, got %d.",
            test_label,
            length(residuals),
            nrow(clean_data)
          ),
          call. = FALSE
        )
      }
      list(data = clean_data, residuals = residuals)
    }
  }

  cleaned <- rhandleMissingValues(data, variables = required_vars)
  aligned <- align_to_model(cleaned$data, rpearson_residuals(model))
  working_data <- aligned$data
  residuals <- aligned$residuals

  requirements <- rvalidateTestRequirements("white", model = model, data = working_data)
  handle_validation_result(requirements)

  if (!is.logical(cross_products) || length(cross_products) != 1) {
    .std_error("invalid_logical", arg = "cross_products")
  }

  size_mb <- tryCatch(
    check_memory_usage(working_data, threshold_mb = 50),
    error = function(e) NA_real_
  )
  if (!is.na(size_mb) && size_mb > 100) {
    ht_log("INFO", sprintf("Switching to streaming White test for dataset of %.1f MB", size_mb))
    adaptive_chunk <- max(1000L, min(25000L, as.integer(nrow(working_data) / 5L)))
    adaptive_chunk <- min(adaptive_chunk, nrow(working_data))
    return(performWhiteTestStreaming(
      model,
      working_data,
      chunk_size = adaptive_chunk,
      cross_products = cross_products,
      max_interactions = max_interactions,
      progress = FALSE
    ))
  }
  if (nrow(working_data) > 10000) {
    message("Large dataset (", nrow(working_data), " observations). ",
            "This may take some time to compute.")
  }

  ht_log("INFO", "Running White test")

  X_full <- rnon_aliased_columns(stats::model.matrix(model, data = working_data), model)
  rwhite_statistic(residuals, X_full, cross_products, max_interactions, test_label,
                   data_name = deparse(substitute(model)))
}

performWhiteTestEnhanced <- function(model, data, cross_products = TRUE, 
                                   bootstrap = FALSE, B = 999, 
                                   parallel = FALSE, alpha = 0.05) {
  
  # Run standard White test
  result <- performWhiteTest(model, data, cross_products = cross_products)
  
  # Add bootstrap p-value if requested
  if (bootstrap) {
    ht_log("INFO", paste("Computing bootstrap p-value with", B, "replications"))
    
    bootstrap_stats <- bootstrap_white_test(model, data, B, parallel, cross_products)
    
    # Bootstrap p-value
    # Finite-simulation convention: (1 + #) / (B_eff + 1).
    finite_stats <- bootstrap_stats[is.finite(bootstrap_stats)]
    boot_effective <- length(finite_stats)
    boot_p_value <- if (boot_effective == 0L) {
      NA_real_
    } else {
      (1 + sum(finite_stats >= unname(result$statistic[1]))) / (boot_effective + 1)
    }
    
    result$p.value.bootstrap <- boot_p_value
    result$method <- paste(result$method, "(with bootstrap)")
    result$bootstrap_statistics <- bootstrap_stats
    
    # Bootstrap confidence interval for the test statistic
    result$bootstrap_ci <- quantile(bootstrap_stats, c(alpha/2, 1-alpha/2))
  }
  
  # Add interpretation
  result$interpretation <- interpret_white_test(result, alpha)
  
  return(result)
}

#' Bootstrap implementation for White test
#' @keywords internal
bootstrap_white_test <- function(model, data, B, parallel, cross_products) {
  rrefuse_weighted_fit(model, "The bootstrap White test")
  
  fitted_vals <- fitted(model)
  original_residuals <- residuals(model)
  response_var <- all.vars(formula(model))[1]
  
  # Bootstrap function
  bootstrap_stat <- function(i) {
    # Resample residuals under null hypothesis (homoscedasticity)
    boot_residuals <- sample(original_residuals, replace = TRUE)
    boot_y <- fitted_vals + boot_residuals
    
    # Create bootstrap dataset
    boot_data <- data
    boot_data[[response_var]] <- boot_y
    
    # Fit bootstrap model
    boot_model <- tryCatch({
      lm(formula(model), data = boot_data)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(boot_model)) return(NA)
    
    # Calculate White test statistic for bootstrap sample
    boot_result <- tryCatch({
      performWhiteTest(boot_model, boot_data, cross_products = cross_products)
    }, error = function(e) {
      return(list(statistic = NA))
    })
    
    return(as.numeric(boot_result$statistic))
  }
  
  # Run bootstrap
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    n_cores <- min(parallel::detectCores() - 1, 4)  # Use at most 4 cores
    boot_stats <- parallel::mclapply(1:B, bootstrap_stat, mc.cores = n_cores)
    boot_stats <- unlist(boot_stats)
  } else {
    boot_stats <- replicate(B, bootstrap_stat(1))
  }
  
  # Remove any NAs from failed bootstrap samples
  boot_stats <- boot_stats[!is.na(boot_stats)]
  
  if (length(boot_stats) < B * 0.8) {
    warning("More than 20% of bootstrap samples failed. Results may be unreliable.")
  }
  
  return(boot_stats)
}

#' Interpret White test results
#' @keywords internal
interpret_white_test <- function(result, alpha = 0.05) {
  p_val <- result$p.value
  statistic <- result$statistic
  
  interpretation <- list()
  
  # Significance level
  if (p_val < 0.001) {
    interpretation$significance <- "Very strong evidence of heteroscedasticity (p < 0.001)"
  } else if (p_val < 0.01) {
    interpretation$significance <- "Strong evidence of heteroscedasticity (p < 0.01)"
  } else if (p_val < alpha) {
    interpretation$significance <- paste0("Evidence of heteroscedasticity (p < ", alpha, ")")
  } else if (p_val < 0.1) {
    interpretation$significance <- "Weak evidence of heteroscedasticity (p < 0.10)"
  } else {
    interpretation$significance <- "No evidence of heteroscedasticity"
  }
  
  # Effect size (rough approximation)
  n_obs <- attr(result, "original_regressors") # This needs to be fixed - should be n
  if (!is.null(attr(result, "r_squared_auxiliary"))) {
    r2_aux <- attr(result, "r_squared_auxiliary") 
    interpretation$effect_size <- ifelse(r2_aux < 0.02, "small",
                                       ifelse(r2_aux < 0.15, "medium", "large"))
  }
  
  # Recommendations
  if (p_val < alpha) {
    interpretation$recommendation <- c(
      "Consider heteroscedasticity-robust standard errors",
      "Examine residual plots for patterns",
      "Consider variance-stabilizing transformations",
      "Try weighted least squares if variance pattern is identifiable"
    )
  } else {
    interpretation$recommendation <- "No remedial action needed for heteroscedasticity"
  }
  
  return(interpretation)
}

#' Print method for enhanced White test results
#' @export
print.htest_enhanced <- function(x, ...) {
  # Use standard htest print method first
  NextMethod()
  
  # Add enhanced information
  if (!is.null(x$interpretation)) {
    cat("\nInterpretation:\n")
    cat("  ", x$interpretation$significance, "\n")
    
    if (!is.null(x$interpretation$effect_size)) {
      cat("   Effect size:", x$interpretation$effect_size, "\n")
    }
    
    if (!is.null(x$interpretation$recommendation)) {
      cat("\nRecommendations:\n")
      for (rec in x$interpretation$recommendation) {
        cat("  -", rec, "\n")
      }
    }
  }
  
  if (!is.null(x$p.value.bootstrap)) {
    cat("\nBootstrap p-value:", round(x$p.value.bootstrap, 4), "\n")
  }
}
