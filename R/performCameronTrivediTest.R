#' Regression-based variance decomposition test
#'
#' Regresses the squared residuals on the fitted values and their square, and
#' reports the F statistic for the joint significance of those two terms.
#'
#' @section What this computes:
#' This is the auxiliary regression
#' \deqn{\hat{e}_i^2 = \alpha_0 + \alpha_1 \hat{y}_i + \alpha_2 \hat{y}_i^2 + u_i,}
#' tested with the overall F statistic. It is the fitted-value form of the
#' White/Breusch-Pagan family and is closely related to
#' [performHarveyTest()] with `auxiliary = "fitted"`, which uses
#' \eqn{\log \hat{e}_i^2} in place of \eqn{\hat{e}_i^2}.
#'
#' It is *not* the information-matrix test of Cameron and Trivedi (1990),
#' which is a specification test for overdispersion in count models and does
#' not apply to a Gaussian linear model. The name is retained for backward
#' compatibility; the statistic is a valid heteroscedasticity diagnostic, but
#' it duplicates coverage already provided by [performWhiteTest()],
#' [performNCVTest()] and [performHarveyTest()]. Removal is deferred to the
#' API review.
#'
#' @param model A fitted [stats::lm] object whose residuals and fitted values will
#'   be used in the auxiliary regression.
#'
#' @return An object of class \code{htest} with the overall F statistic, degrees
#'   of freedom, and p-value for the joint null hypothesis of homoskedasticity.
#'
#' @details
#' The test regresses squared residuals on the fitted values and their square
#' \deqn{\hat{e}_i^2 = \alpha + \beta_1 \hat{y}_i + \beta_2 \hat{y}_i^2 + u_i.}
#' Testing \eqn{\beta_1 = \beta_2 = 0} yields an F statistic with two numerator
#' degrees of freedom. Significance indicates heteroscedasticity driven either by
#' a linear mean-variance relationship (\eqn{\beta_1 \ne 0}) or by curvature in the
#' variance function (\eqn{\beta_2 \ne 0}). Inspecting the individual t-statistics
#' from the auxiliary regression can help disentangle these effects.
#'
#' The implementation integrates the shared validation helpers to ensure the model
#' provides sufficient observations, that fitted values vary across observations,
#' and that squared residuals retain variability before computing the auxiliary
#' regression.
#'
#' @references
#' Cameron, A. C., & Trivedi, P. K. (1990). The information matrix test and its
#' applied alternative hypotheses. *University of California, Davis Working Paper*.
#'
#' Cameron, A. C., & Trivedi, P. K. (2005). *Microeconometrics: Methods and
#' Applications*. Cambridge University Press. Section 7.4 discusses the
#' decomposition test.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' performCameronTrivediTest(mod)
#'
#' # Simulated data with quadratic heteroscedasticity
#' set.seed(135)
#' x <- runif(180)
#' y <- 1 + 2 * x + rnorm(180, sd = 0.5 + x^2)
#' performCameronTrivediTest(lm(y ~ x))
#'
#' @seealso
#' [performHarveyTest()] and [performParkTest()] for alternative parametric tests
#' targeting specific functional forms.
performCameronTrivediTest <- function(model) {
  rvalidateModelInputs(model, test_name = "Cameron-Trivedi", min_obs = 15L)

  model_data <- tryCatch(stats::model.frame(model), error = function(e) NULL)
  requirements <- rvalidateTestRequirements("cameron_trivedi", model = model, data = model_data)
  rprocessValidationResult(requirements)

  ht_log("INFO", "Running Cameron-Trivedi test")

  yhat <- stats::fitted(model)
  if (stats::var(yhat) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi test requires variability in fitted values"
    )
  }

  e2 <- stats::residuals(model)^2
  if (stats::var(e2) <= .Machine$double.eps) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi test requires variability in squared residuals"
    )
  }

  aux_data <- data.frame(e2 = e2, yhat = yhat)
  aux_model <- safe_lm(e2 ~ yhat + I(yhat^2), data = aux_data)
  R2 <- summary(aux_model)$r.squared
  df_num <- 2
  df_den <- aux_model$df.residual

  if (df_den <= 0) {
    std_error(
      "rassumption_violation",
      assumption = "Cameron-Trivedi auxiliary regression requires positive residual degrees of freedom"
    )
  }

  F_stat <- (R2 / df_num) / ((1 - R2) / df_den)
  p_value <- 1 - stats::pf(F_stat, df_num, df_den)
  structure(
    list(
      statistic = c(F = F_stat),
      parameter = c(df1 = df_num, df2 = df_den),
      p.value = p_value,
      method = "Cameron-Trivedi decomposition test",
      data.name = deparse(stats::formula(model))
    ),
    class = "htest"
  )
}
