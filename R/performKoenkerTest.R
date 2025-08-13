#' Perform Koenker studentized Breusch-Pagan test
#'
#' This function performs the Koenker version of the Breusch-Pagan test,
#' which is more robust to non-normal errors. It regresses the absolute
#' residuals of a linear model on the regressors and uses the resulting
#' R-squared as the test statistic.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performKoenkerTest(m, mtcars)
performKoenkerTest <- function(model, data) {
  checkModel(model)
  checkData(data)
  ht_log("INFO", "Running Koenker test")

  e <- residuals(model)
  n <- length(e)
  X <- model.matrix(formula(model), data = data)
  aux_data <- data.frame(X[, -1, drop = FALSE])
  aux_model <- safe_lm(abs(e) ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Koenker studentized Breusch-Pagan test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
