#' Perform White's test for heteroscedasticity
#'
#' This function implements White's test on a fitted linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @param cross_products Logical. Include cross-product terms in the
#'   auxiliary regression?
#' 
#' @return An object of class \code{htest} with the test statistic and p-value.
#' 
#' @references 
#' White, H. (1980). A heteroscedasticity-consistent covariance matrix 
#' estimator and a direct test for heteroscedasticity. \emph{Econometrica}, 
#' 48(4), 817-838. \doi{10.2307/1912934}
#' 
#' Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performWhiteTest(m, mtcars)
performWhiteTest <- function(model, data, cross_products = TRUE) {
  checkModel(model)
  checkData(data)
  validateTestInputs(model, data, "White")
  if (!is.logical(cross_products) || length(cross_products) != 1) {
    std_error("invalid_logical", arg = "cross_products")
  }
  check_memory_usage(data, threshold_mb = 50)
  if (nrow(data) > 10000) {
    message(
      "Large dataset (", nrow(data), " observations). ",
      "This may take some time to compute."
    )
  }

  model_formula <- formula(model)
  indep_vars <- model.matrix(model_formula, data = data)
  indep_names <- colnames(indep_vars)[-1]

  aux_data <- data.frame(indep_vars[, -1, drop = FALSE])
  names(aux_data) <- indep_names

  # Add squared terms
  squared_terms <- indep_vars[, -1, drop = FALSE]^2
  colnames(squared_terms) <- paste0(indep_names, "_sq")
  aux_data <- cbind(aux_data, squared_terms)

  # Add cross-product terms if requested
  if (cross_products && ncol(indep_vars) - 1 > 1 && ncol(indep_vars) - 1 <= 10) {
    cross_terms <- list()
    idx <- 1

    for (i in seq_len(ncol(indep_vars) - 2)) {
      for (j in seq(i + 1, ncol(indep_vars) - 1)) {
        cross_terms[[idx]] <- indep_vars[, i + 1] * indep_vars[, j + 1]
        names(cross_terms)[idx] <- paste0(indep_names[i], "_x_", indep_names[j])
        idx <- idx + 1
      }
    }

    if (length(cross_terms) > 0) {
      aux_data <- cbind(aux_data, as.data.frame(cross_terms))
    }
  } else if (cross_products && ncol(indep_vars) - 1 > 10) {
    std_warning("cross_products_omitted")
  }

  aux_model <- tryCatch(
    lm(residuals(model)^2 ~ ., data = aux_data),
    error = function(e) {
      stop("Auxiliary regression failed: ", e$message)
    }
  )
  n <- nrow(data)
  test_statistic <- summary(aux_model)$r.squared * n
  df <- length(coef(aux_model)) - 1
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "White's test for heteroscedasticity",
      data.name = deparse(substitute(model))
    ),
    class = "htest"
  )
}
