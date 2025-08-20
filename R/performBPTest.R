#' Perform Breusch-Pagan test for heteroscedasticity
#'
#' This function performs the Breusch-Pagan test on a fitted linear model.
#'
#' @param model A fitted model of class `lm`.
#' @param data The data frame used to fit `model`.
#' 
#' @return An object of class \code{htest} with the test statistic and p-value.
#' 
#' @references 
#' Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity 
#' and random coefficient variation. \emph{Econometrica}, 47(5), 1287-1294. 
#' \doi{10.2307/1911963}
#' 
#' Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. 
#' \emph{Journal of Econometrics}, 17(1), 107-112. \doi{10.1016/0304-4076(81)90062-2}
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performBPTest(m, mtcars)
performBPTest <- function(model, data) {
  checkModel(model)
  checkData(data)
  ht_log("INFO", "Running Breusch-Pagan test")

  e <- residuals(model)
  n <- length(e)
  X <- model.matrix(formula(model), data = data)
  aux_data <- data.frame(X[, -1, drop = FALSE])
  aux_model <- safe_lm(e^2 ~ ., data = aux_data)
  r2 <- summary(aux_model)$r.squared
  test_statistic <- n * r2
  df <- ncol(aux_data)
  p_value <- 1 - pchisq(test_statistic, df)

  structure(
    list(
      statistic = c("X-squared" = test_statistic),
      parameter = df,
      p.value = p_value,
      method = "Breusch-Pagan test for heteroscedasticity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
