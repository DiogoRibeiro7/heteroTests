#' Additional heteroscedasticity tests
#'
#' These functions extend the package with more specialized tests
#' for heteroscedasticity such as a studentized Breusch-Pagan test,
#' a bootstrap version of White's test and the Szroeter ordered test.
#'
#' @name additional_tests
NULL

#' Studentized Breusch-Pagan test
#'
#' Performs the Breusch-Pagan test using studentized residuals,
#' providing robustness to non-normality.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' @return An object of class `htest` with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performStudentizedBPTest(m, mtcars)
#' @export
performStudentizedBPTest <- function(model, data) {
  checkModel(model)
  checkData(data)
  validateTestInputs(model, data, "Studentized BP")

  resid_student <- rstudent(model)
  n <- length(resid_student)
  X <- model.matrix(formula(model), data = data)[, -1, drop = FALSE]
  aux_model <- lm(resid_student^2 ~ X)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(X)
  p_value <- pchisq(test_statistic, df, lower.tail = FALSE)

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
#' Uses bootstrap resampling of residuals to estimate the distribution
#' of White's test statistic. Useful for small samples.
#'
#' @inheritParams performWhiteTest
#' @param B Number of bootstrap replications.
#' @param parallel Logical, run in parallel using the `parallel` package?
#' @return An object of class `htest` with the bootstrap p-value.
#' @export
performWhiteTestBootstrap <- function(model, data, B = 1000, parallel = FALSE) {
  checkModelEnhanced(model, data)

  original_stat <- performWhiteTest(model, data)$statistic

  bootstrap_stat <- function() {
    fitted_vals <- fitted(model)
    resid <- residuals(model)
    boot_resid <- sample(resid, replace = TRUE)
    boot_y <- fitted_vals + boot_resid

    boot_data <- data
    response_name <- as.character(formula(model))[2]
    boot_data[[response_name]] <- boot_y

    boot_model <- lm(formula(model), data = boot_data)
    performWhiteTest(boot_model, boot_data)$statistic
  }

  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    boot_stats <- parallel::mclapply(
      seq_len(B),
      function(i) bootstrap_stat(),
      mc.cores = max(1, parallel::detectCores() - 1)
    )
    boot_stats <- unlist(boot_stats)
  } else {
    boot_stats <- replicate(B, bootstrap_stat())
  }

  p_value <- mean(boot_stats >= original_stat)

  structure(
    list(
      statistic = original_stat,
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
#' Detects monotonic changes in variance when the observations can be
#' ordered by a known variable.
#'
#' @inheritParams performHarveyTest
#' @param data The data frame used to fit `model`.
#' @param order_by Variable name to order the observations by.
#' @return An object of class `htest`.
#' @export
performSzroeterTest <- function(model, data, order_by) {
  checkModelEnhanced(model, data)

  if (!order_by %in% names(data)) {
    std_error("missing_variable", variable = order_by)
  }

  ord <- order(data[[order_by]])
  e_ordered <- residuals(model)[ord]
  n <- length(e_ordered)

  ranks <- seq_len(n)
  numerator <- sum(ranks * e_ordered^2)
  denominator <- sum(e_ordered^2) * (n + 1) / 2

  test_statistic <- numerator / denominator

  var_stat <- (n + 1) * (2 * n + 1) / (12 * n)
  z_stat <- (test_statistic - 1) / sqrt(var_stat / n)
  p_value <- 2 * pnorm(-abs(z_stat))

  structure(
    list(
      statistic = c(S = test_statistic),
      parameter = c(n = n),
      p.value = p_value,
      method = "Szroeter test for ordered heteroscedasticity",
      data.name = deparse(substitute(model))
    ),
    class = "htest"
  )
}
