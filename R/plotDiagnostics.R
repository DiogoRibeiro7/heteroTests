#' Plot residuals vs fitted values
#'
#' Generates a simple scatter plot of residuals against fitted values from a
#' linear model. A horizontal reference line at zero is added.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return A \code{ggplot} object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plotResidualsFitted(m)
plotResidualsFitted <- function(model) {
  checkModel(model)
  df <- data.frame(fitted = fitted(model), resid = residuals(model))
  ggplot2::ggplot(df, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "blue") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = "Fitted values", y = "Residuals",
      title = "Residuals vs Fitted"
    ) +
    theme_hetero()
}

#' Spread-Level plot for variance diagnostics
#'
#' Plots the square root of the absolute residuals against fitted values.
#' A lowess smooth is added to highlight trends.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return A \code{ggplot} object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plotSpreadLevel(m)
plotSpreadLevel <- function(model) {
  checkModel(model)
  df <- data.frame(
    fitted = fitted(model),
    res_sqrt = sqrt(abs(residuals(model)))
  )
  ggplot2::ggplot(df, ggplot2::aes(fitted, res_sqrt)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, color = "blue") +
    ggplot2::labs(
      x = "Fitted values", y = "sqrt(|Residual|)",
      title = "Spread-Level Plot"
    ) +
    theme_hetero()
}

#' Generate a suite of diagnostic plots
#'
#' This convenience wrapper returns residual-vs-fitted and spread-level plots
#' to help visually assess heteroscedastic patterns.
#'
#' @param model A fitted model of class `lm`.
#'
#' @return A list with elements `residuals_fitted` and `spread_level`, each a
#'   \code{ggplot} object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plots <- plotDiagnosticSuite(m)
#' plots$residuals_fitted
plotDiagnosticSuite <- function(model) {
  checkModel(model)
  list(
    residuals_fitted = plotResidualsFitted(model),
    spread_level = plotSpreadLevel(model),
    density = plotResidualDensity(model),
    qq = plotResidualQQ(model),
    bubble_variance = plotBubbleVariance(model)
  )
}


#' Compare residuals before and after remediation
#'
#' Overlays residuals of two models on a single plot to visualise improvement
#' after applying a remediation method (e.g. WLS or robust regression).
#'
#' @param original The original `lm` or `glm` model.
#' @param remedied The model fitted after remediation.
#'
#' @return A `ggplot` object with residuals of both models.
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt, data = mtcars)
#' m2 <- fitWLS(m1)
#' plotBeforeAfter(m1, m2)
plotBeforeAfter <- function(original, remedied) {
  checkModel(original)
  checkModel(remedied)
  df <- data.frame(
    fitted = c(fitted(original), fitted(remedied)),
    resid = c(residuals(original), residuals(remedied)),
    model = rep(
      c("original", "remedied"),
      c(length(residuals(original)), length(residuals(remedied)))
    )
  )
  ggplot2::ggplot(df, ggplot2::aes(fitted, resid, colour = model)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_smooth(method = "loess", se = FALSE) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = "Fitted values", y = "Residuals",
      title = "Before/After Residual Comparison",
      colour = "Model"
    ) +
    scale_colour_hetero_diagnostic() +
    theme_hetero()
}

#' Density plot of residuals
#'
#' Shows the distribution of residuals with a kernel density estimate.
#'
#' @param model A fitted model of class `lm` or `glm`.
#'
#' @return A `ggplot` object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plotResidualDensity(m)
plotResidualDensity <- function(model) {
  checkModel(model)
  df <- data.frame(resid = residuals(model))
  ggplot2::ggplot(df, ggplot2::aes(resid)) +
    ggplot2::geom_density(fill = "lightblue", alpha = 0.5) +
    ggplot2::labs(
      x = "Residuals", y = "Density",
      title = "Residual Density"
    ) +
    theme_hetero()
}

#' QQ plot of residuals
#'
#' Visualises departure from normality using a QQ plot.
#'
#' @inheritParams plotResidualDensity
#'
#' @return A `ggplot` object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plotResidualQQ(m)
plotResidualQQ <- function(model) {
  checkModel(model)
  df <- data.frame(resid = residuals(model))
  ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line() +
    ggplot2::labs(
      x = "Theoretical Quantiles", y = "Sample Quantiles",
      title = "Residual QQ Plot"
    ) +
    theme_hetero()
}

#' Bubble plot of residual variance by covariate
#'
#' Displays residual magnitude against a predictor with bubble size
#' proportional to `|residual|`.
#'
#' @inheritParams plotResidualDensity
#' @param variable Optional name of a covariate from the model to plot against.
#'
#' @return A `ggplot` object.
#' @examples
#' data(mtcars)
#' m <- lm(mpg ~ wt + qsec, data = mtcars)
#' plotBubbleVariance(m, "wt")
plotBubbleVariance <- function(model, variable = NULL) {
  checkModel(model)
  df <- data.frame(model.frame(model))
  df$resid <- residuals(model)
  df$abs_resid <- abs(df$resid)
  if (is.null(variable)) {
    variable <- attr(terms(model), "term.labels")[1]
  }
  if (!variable %in% names(df)) {
    stop("variable not found in model frame")
  }
  ggplot2::ggplot(df, ggplot2::aes(
    x = .data[[variable]], y = resid,
    size = abs_resid
  )) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      x = variable, y = "Residuals", size = "|residual|",
      title = "Bubble Plot of Residual Variance"
    ) +
    theme_hetero()
}

#' Enhanced residuals vs fitted plot
#'
#' Highlights influential observations and includes a LOESS smooth with
#' confidence bands.
#'
#' @inheritParams plotResidualsFitted
#' @return A \code{ggplot} object.
#' @export
plotResidualsFittedEnhanced <- function(model) {
  checkModel(model)
  df <- data.frame(
    fitted = fitted(model),
    resid = residuals(model),
    abs_resid = abs(residuals(model))
  )
  cd <- cooks.distance(model)
  df$influential <- cd > 4 / length(cd)
  ggplot2::ggplot(df, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_point(ggplot2::aes(color = influential, size = abs_resid), alpha = 0.7) +
    ggplot2::geom_smooth(method = "loess", se = TRUE, color = "blue") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = "Enhanced Residuals vs Fitted",
      subtitle = "Blue band: LOWESS \u00B1 SE, red points: influential",
      color = "Influential",
      size = "|Residual|"
    ) +
    theme_hetero()
}

#' Enhanced diagnostic plot suite
#'
#' Returns a list of improved diagnostic plots with statistical overlays.
#'
#' @inheritParams plotDiagnosticSuite
#' @return A list of ggplot objects.
#' @export
plotDiagnosticSuiteEnhanced <- function(model) {
  checkModel(model)
  list(
    residuals_fitted = plotResidualsFittedEnhanced(model),
    spread_level = plotSpreadLevel(model),
    density = plotResidualDensity(model),
    qq = plotResidualQQ(model),
    bubble_variance = plotBubbleVariance(model)
  )
}
