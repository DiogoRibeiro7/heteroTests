#' Ramsey's RESET test for nonlinearity
#'
#' Adds powers of the fitted values to the model and performs an F test.
#'
#' On a fit with weights the augmented model is fitted by weighted least
#' squares and both residual sums of squares are weighted. Releases before
#' 0.12.0 refitted the augmented model without the weights, so the two models
#' were not nested.
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
  # A weighted fit is augmented by weighted least squares and both sums of
  # squares are weighted. Before 0.12.0 the augmented model was refitted
  # without the weights while the restricted sum of squares came from the
  # weighted fit, so the two were not nested and the F statistic could be
  # negative.
  w <- rprior_weights(model)
  if (is.null(w)) {
    mod_aug <- lm.fit(X1, y)
    rss0 <- sum(residuals(model)^2)
    rss1 <- sum(mod_aug$residuals^2)
  } else {
    mod_aug <- stats::lm.wfit(X1, y, w)
    rss0 <- sum(w * model$residuals^2)
    rss1 <- sum(w * mod_aug$residuals^2)
  }
  df1 <- length(power)
  df2 <- mod_aug$df.residual
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
