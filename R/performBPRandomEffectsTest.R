#' Breusch-Pagan test for random effects models
#'
#' Applies the Breusch-Pagan Lagrange Multiplier test for heteroscedasticity in
#' panel models with random effects.
#'
#' @param model A fitted model of class `lm`.
#' @param data Data frame used to fit `model`.
#' @param id Character. Name of the individual identifier.
#'
#' @return An object of class \code{htest} with the test statistic and p-value.
#' @examples
#' df <- data.frame(
#'   id = rep(1:5, each = 4),
#'   time = rep(1:4, 5),
#'   x = runif(20),
#'   y = rnorm(20)
#' )
#' m <- lm(y ~ x, data = df)
#' performBPRandomEffectsTest(m, df, "id")
performBPRandomEffectsTest <- function(model, data, id) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.")
  }
  if (!id %in% names(data)) {
    stop("`id` must be a column in `data`.")
  }

  res <- residuals(model)
  idfac <- factor(data[[id]])
  T_i <- tapply(res, idfac, length)
  if (length(unique(T_i)) > 1) {
    warning("Unequal panel lengths; using mean T")
  }
  T <- mean(T_i)
  sum_ei <- tapply(res, idfac, sum)
  LM <- (T^2 / (2 * (T - 1))) * sum(sum_ei^2) / sum(res^2)
  p_value <- 1 - pchisq(LM, df = 1)

  structure(
    list(
      statistic = c(LM = LM),
      parameter = 1,
      p.value = p_value,
      method = "Breusch-Pagan LM test for random effects",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
