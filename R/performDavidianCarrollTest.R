#' Perform Davidian–Carroll test for heteroscedasticity
#'
#' Fits a polynomial regression of the log-squared residuals on the fitted values
#' to detect parametric variance functions, as proposed by Davidian and Carroll
#' (1987).
#'
#' @param model A fitted [stats::lm] object providing residuals and fitted values
#'   for the auxiliary regression.
#' @param degree Positive integer specifying the polynomial degree used for the
#'   fitted values (default `2`). Higher degrees allow more flexible variance
#'   functions but require larger samples.
#'
#' @return An object of class \code{htest} with the F statistic, numerator and
#'   denominator degrees of freedom, and p-value for the joint null of constant
#'   variance.
#'
#' @details
#' The auxiliary regression
#' \deqn{\log \hat{e}_i^2 = \gamma_0 + \gamma_1 \hat{y}_i + \cdots + \gamma_p
#'   \hat{y}_i^p + u_i}
#' models the log-variance as a polynomial in the fitted values. The test evaluates
#' \eqn{\gamma_1 = \cdots = \gamma_p = 0} using an F statistic with \eqn{p}
#' numerator degrees of freedom. Significant results indicate that the variance can
#' be described by a smooth function of the mean, which can guide variance-stabilising
#' transformations or weighted least squares specifications.
#'
#' The function validates that `degree` is a positive integer, that the model
#' supplies sufficient observations relative to `degree`, and that residuals display
#' non-trivial variation before fitting the auxiliary regression.
#'
#' @references
#' Davidian, M., & Carroll, R. J. (1987). Variance function estimation. *Journal of
#' the American Statistical Association, 82*(400), 1079–1091.
#'
#' Carroll, R. J., & Ruppert, D. (1988). *Transformation and Weighting in Regression*.
#' Chapman & Hall. Chapter 3 discusses variance-function diagnostics.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performDavidianCarrollTest(mod)
#'
#' # Higher-order polynomial
#' performDavidianCarrollTest(mod, degree = 3)
#'
#' # Simulated heteroscedastic data with exponential variance in the mean
#' set.seed(246)
#' x <- runif(160)
#' y <- 1 + x + rnorm(160, sd = exp(0.5 * x))
#' performDavidianCarrollTest(lm(y ~ x))
#'
#' @seealso
#' [performHarveyTest()] and [performNCVTest()] for related
#' variance-function diagnostics.
performDavidianCarrollTest <- function(model, degree = 2) {
  if (!inherits(model, "lm")) {
    stop("`model` must be an object of class 'lm'.")
  }
  if (!is.numeric(degree) || degree < 1) {
    stop("`degree` must be a positive integer.")
  }

  res <- residuals(model)
  fit <- fitted(model)
  # Under na.action = na.exclude both vectors are padded back to the original
  # row count with NA placeholders, which made var(fit) NA and turned the
  # guard below into "missing value where TRUE/FALSE needed". Test the rows
  # the model actually fitted.
  complete <- !is.na(res) & !is.na(fit)
  res <- res[complete]
  fit <- fit[complete]
  if (stats::var(fit) <= .Machine$double.eps) {
    stop("Davidian-Carroll test requires variability in fitted values.", call. = FALSE)
  }
  # A residual of exactly zero sends log(e^2) to -Inf and the auxiliary fit
  # fails with 'NA/NaN/Inf in y'. Floor it and warn, as the other
  # log-variance diagnostics do.
  log_e2 <- rlog_squared_residuals(res, "Davidian-Carroll test")
  df <- data.frame(log_e2 = log_e2, fit = fit)
  formula_str <- paste0("log_e2 ~ poly(fit, ", degree, ")")
  aux_model <- lm(stats::as.formula(formula_str), data = df)
  aov_table <- anova(aux_model)
  F_stat <- aov_table$`F value`[1]
  df_num <- aov_table$Df[1]
  df_den <- aov_table$Df[2]
  p_value <- aov_table$`Pr(>F)`[1]

  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Davidian-Carroll test",
      data.name = deparse(formula(model))
    ),
    class = "htest"
  )
}
