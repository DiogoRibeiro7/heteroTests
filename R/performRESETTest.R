#' Ramsey's RESET test for nonlinearity
#'
#' Adds powers of the fitted values to the model and performs an F test.
#'
#' @param model A fitted model of class `lm`.
#' @param power Numeric vector of powers to include. Defaults to `2:3`.
#' @return An object of class `htest` with the test result.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performRESETTest(m)
performRESETTest <- function(model, power = 2:3) {
  checkModel(model)
  y <- model.response(model.frame(model))
  X0 <- model.matrix(model)
  yhat <- fitted(model)
  X1 <- X0
  for (p in power) {
    X1 <- cbind(X1, yhat^p)
  }
  mod_aug <- lm.fit(X1, y)
  df1 <- length(power)
  df2 <- mod_aug$df.residual
  rss0 <- sum(residuals(model)^2)
  rss1 <- sum(mod_aug$residuals^2)
  fstat <- ((rss0 - rss1) / df1) / (rss1 / df2)
  pval <- pf(fstat, df1, df2, lower.tail = FALSE)
  structure(
    list(
      statistic = c(F = fstat),
      parameter = c(df1 = df1, df2 = df2),
      p.value = pval,
      method = "RESET test for nonlinearity",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
