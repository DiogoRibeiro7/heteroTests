#' Perform White's test for heteroscedasticity (CORRECTED VERSION)
#'
#' This function implements White's test on a fitted linear model with proper
#' cross-product terms according to White's original 1980 specification.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param cross_products Logical. Include cross-product terms in the
#'   auxiliary regression? Default is TRUE.
#' @param max_interactions Maximum number of predictors for including cross-products
#'   to avoid computational explosion. Default is 10.
#' 
#' @return An object of class \code{htest} with the test statistic and p-value.
#' 
#' @details 
#' White's test regresses the squared residuals on:
#' 1. All original regressors (excluding intercept)
#' 2. Squares of all original regressors  
#' 3. Cross-products of all pairs of original regressors (if cross_products = TRUE)
#' 
#' The test statistic is n*R² from this auxiliary regression, which follows
#' a chi-squared distribution with degrees of freedom equal to the number of
#' regressors in the auxiliary model (excluding the intercept).
#'
#' @references 
#' White, H. (1980). A heteroscedasticity-consistent covariance matrix 
#' estimator and a direct test for heteroscedasticity. \emph{Econometrica}, 
#' 48(4), 817-838. \doi{10.2307/1912934}
#' 
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTest(m, mtcars)
#' @export
performWhiteTest <- function(model, data, cross_products = TRUE, max_interactions = 10) {
  # Input validation
  checkModel(model)
  checkData(data)
  validateTestInputs(model, data, "White")
  
  if (!is.logical(cross_products) || length(cross_products) != 1) {
    std_error("invalid_logical", arg = "cross_products")
  }
  
  # Memory and performance warnings
  check_memory_usage(data, threshold_mb = 50)
  if (nrow(data) > 10000) {
    message("Large dataset (", nrow(data), " observations). ",
            "This may take some time to compute.")
  }
  
  ht_log("INFO", "Running White test")
  
  # Extract model components
  e_squared <- residuals(model)^2
  n <- length(e_squared)
  
  # Get the model matrix (includes intercept)
  X_full <- model.matrix(model)
  
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
  for (j in 1:p) {
    aux_matrix <- cbind(aux_matrix, X[, j]^2)
    aux_names <- c(aux_names, paste0(regressor_names[j], "_sq"))
  }
  
  # Add cross-product terms if requested
  if (cross_products && p > 1) {
    
    # Check if we should include cross-products based on dimensionality
    if (p > max_interactions) {
      std_warning("cross_products_omitted")
      message("Cross-products omitted due to high dimensionality (p = ", p, 
              " > max_interactions = ", max_interactions, ")")
    } else {
      # Add all pairwise cross-products X_i * X_j for i < j
      for (i in 1:(p-1)) {
        for (j in (i+1):p) {
          cross_product <- X[, i] * X[, j]
          aux_matrix <- cbind(aux_matrix, cross_product)
          aux_names <- c(aux_names, paste0(regressor_names[i], "_x_", regressor_names[j]))
        }
      }
    }
  }
  
  # Set column names for the auxiliary matrix
  colnames(aux_matrix) <- aux_names
  
  # Convert to data frame for regression
  aux_data <- as.data.frame(aux_matrix)
  
  # Check for perfect multicollinearity in auxiliary regression
  aux_rank <- qr(aux_matrix)$rank
  if (aux_rank < ncol(aux_matrix)) {
    warning("Perfect multicollinearity detected in auxiliary regression. ",
            "Results may be unreliable.")
  }
  
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
  df <- ncol(aux_matrix)  # Degrees of freedom = number of auxiliary regressors
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)
  
  # Create result object
  result <- structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = c(df = df),
      p.value = p_value,
      method = if (cross_products && p > 1 && p <= max_interactions) {
        "White's test for heteroscedasticity (with cross-products)"
      } else {
        "White's test for heteroscedasticity"
      },
      data.name = deparse(substitute(model)),
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

#' Enhanced White Test with Bootstrap Option
#' 
#' Extends the standard White test with bootstrap p-values for small samples
#' and additional diagnostic information.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`. 
#' @param cross_products Include cross-product terms?
#' @param bootstrap Use bootstrap p-values?
#' @param B Number of bootstrap replications if bootstrap = TRUE.
#' @param parallel Use parallel processing for bootstrap?
#' @param alpha Significance level for interpretation.
#'
#' @return Enhanced htest object with additional components
#' @export
performWhiteTestEnhanced <- function(model, data, cross_products = TRUE, 
                                   bootstrap = FALSE, B = 1000, 
                                   parallel = FALSE, alpha = 0.05) {
  
  # Run standard White test
  result <- performWhiteTest(model, data, cross_products = cross_products)
  
  # Add bootstrap p-value if requested
  if (bootstrap) {
    ht_log("INFO", paste("Computing bootstrap p-value with", B, "replications"))
    
    bootstrap_stats <- bootstrap_white_test(model, data, B, parallel, cross_products)
    
    # Bootstrap p-value
    boot_p_value <- mean(bootstrap_stats >= result$statistic)
    
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
