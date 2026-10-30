#' Ramsey's RESET test for nonlinearity
#'
#' Adds powers of the fitted values to the model and performs an F test.
#'
#' On a fit with weights the augmented model is fitted by weighted least
#' squares and both residual sums of squares are weighted. Releases before
#' 0.12.0 refitted the augmented model without the weights, so the two models
#' were not nested.
#'
#' On a fit with an offset the fitted values are the design times the
#' coefficients plus the offset, and the test is the nested F test of the model
#' with its offset against the model augmented with the powers of those fitted
#' values, fitted with the same offset and weights (since 0.16.0, decided
#' 2026-10-18). It is not RESET of the model fitted to the response minus the
#' offset, which augments that model with the powers of the design times the
#' coefficients alone. Before 0.16.0 the augmented model was fitted without the
#' offset, so the two models were not nested.
#'
#' @param model A fitted model of class `lm`.
#' @param data Optional data frame the model was fitted to, or `NULL`, the
#'   default. The statistic is computed from the response, fitted values,
#'   design, weights and offset of the fit itself, so `data` does not change
#'   the result; it is accepted so that every test takes `(model, data, ...)`;
#'   since 0.16.0 its rows are matched to the observations of the fit as by
#'   every test, data with row names of their own in any order and data that
#'   number their rows in the order of the fit, and other data are refused,
#'   where it needed only the variables of the model and the row names of the
#'   fit. Before 0.15.0 the second argument was `power`: a
#'   number passed there by position is still read as `power` in 0.15.x, with a
#'   warning that shows the new call, and stops working in 1.0.0.
#' @param power Numeric vector of powers to include. Defaults to `2:3`.
#' @return An object of class `htest` with the test result.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' performRESETTest(m)
performRESETTest <- function(model, data = NULL, power = 2:3) {
  rrefuse_non_gaussian_glm(model, "performRESETTest")
  if (is.numeric(data) && is.null(dim(data)) && missing(power)) {
    rwarn_positional_call("performRESETTest", substitute(model), list(power = substitute(data)))
    power <- data
    data <- NULL
  }
  # The response, design, weights and offset come from the fit, through the
  # model context (R/model_context.R), which also refuses a model frame that
  # `model = FALSE` rebuilt from data changed after the fit: before 0.16.0 the
  # test read such a frame. RESET reads nothing from `data`, which is matched
  # to the observations of the fit as by every test built on the context
  # (decision 31; from increment 2 to increment 6 the context was built without
  # `data`, which kept the check of 0.15.0, decision 24).
  rcheck_data_argument(data, "performRESETTest")
  ctx <- rmodel_context(model, data, fn = "performRESETTest", test_label = "RESET")
  rrefuse_penalised_gam(ctx, "performRESETTest", "refit")
  checkModel(model)
  rrefuse_nonfinite_coefficients(model, "RESET test", allow_aliased = TRUE)
  rvalidate_model_context(ctx)
  y <- ctx$response
  # The design without the columns a rank-deficient fit aliased: the
  # augmented model is then that of the same model fitted without them.
  X0 <- rcontext_design(ctx)
  # The fitted values of the model, X b plus its offset. On a model with an
  # offset the test is the nested F test of that model, offset included,
  # against the model augmented with the powers of its fitted values, fitted
  # with the same offset (decided 2026-10-18). Before 0.16.0 the augmented
  # model left the offset out, so the two were not nested, and a model with
  # an offset was refused.
  # Without the padding of na.exclude (B13), on which the augmented fit
  # stopped with "NA/NaN/Inf in 'x'" until increment 9 of the model context.
  yhat <- rcontext_fitted(ctx)
  X1 <- X0
  for (p in power) {
    X1 <- cbind(X1, yhat^p)
  }
  # A weighted fit is augmented by weighted least squares and both sums of
  # squares are weighted. Before 0.12.0 the augmented model was refitted
  # without the weights while the restricted sum of squares came from the
  # weighted fit, so the two were not nested and the F statistic could be
  # negative.
  w <- ctx$weights
  if (is.null(w)) {
    mod_aug <- lm.fit(X1, y, offset = ctx$offset)
    rss0 <- sum(rcontext_unpad(ctx, stats::residuals(model))^2)
    rss1 <- sum(mod_aug$residuals^2)
  } else {
    mod_aug <- stats::lm.wfit(X1, y, w, offset = ctx$offset)
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
