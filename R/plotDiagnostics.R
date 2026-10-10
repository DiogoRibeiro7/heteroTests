#' Plot residuals vs fitted values
#'
#' Generates a simple scatter plot of residuals against fitted values from a
#' linear model. A horizontal reference line at zero is added.
#'
#' For a weighted fit the Pearson residuals \eqn{\sqrt{w_i}\, e_i} are plotted,
#' as in `plot.lm()`: those are the residuals that have constant variance when
#' the weights are right. The other residual plots in the package do the same.
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
  df <- data.frame(fitted = fitted(model), resid = rpearson_residuals(model))
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
    res_sqrt = sqrt(abs(rpearson_residuals(model)))
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
#' Draws the residuals of two models against their fitted values in two panels,
#' side by side, to show what a remediation method (a weighted fit, a
#' transformation) did to the spread.
#'
#' A weighted fit is shown through its Pearson residuals \eqn{\sqrt{w_i}\, e_i},
#' so the plot shows whether the weighting flattened the spread. Its raw
#' residuals would look as heteroscedastic as the original ones however good
#' the weights were.
#'
#' Each panel has its own axes. Weighted residuals and the residuals of a
#' transformed response are not in the units of the original ones, and on a
#' common axis one of the two panels could be squeezed flat. The panels are
#' there to compare the shape of the two clouds, not their size.
#'
#' @param original The original `lm` or `glm` model.
#' @param remedied The model fitted after remediation.
#'
#' @return A `ggplot` object with one panel per model. Its data have the
#'   columns `fitted`, `resid`, `model` (`"original"` or `"remedied"`) and
#'   `panel`.
#' @section Earlier versions:
#' Up to 0.12.0 the two sets of residuals were overlaid in one panel and told
#' apart by colour.
#' @examples
#' data(mtcars)
#' m1 <- lm(mpg ~ wt, data = mtcars)
#' m2 <- fitWLS(m1)
#' plotBeforeAfter(m1, m2)
plotBeforeAfter <- function(original, remedied) {
  checkModel(original)
  checkModel(remedied)
  resid_original <- rpearson_residuals(original)
  resid_remedied <- rpearson_residuals(remedied)
  panel_label <- function(model, label) {
    if (is.null(rprior_weights(model))) label else paste0(label, " (weighted residuals)")
  }
  labels <- c(
    original = panel_label(original, "Original"),
    remedied = panel_label(remedied, "Remedied")
  )
  model <- rep(
    c("original", "remedied"),
    c(length(resid_original), length(resid_remedied))
  )
  df <- data.frame(
    fitted = c(fitted(original), fitted(remedied)),
    resid = c(resid_original, resid_remedied),
    model = model,
    panel = factor(labels[model], levels = unname(labels))
  )
  ggplot2::ggplot(df, ggplot2::aes(fitted, resid)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#8c8c8c", linewidth = 0.3) +
    ggplot2::geom_point(colour = .ht_palette[1], alpha = 0.5, size = 1.3) +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, se = FALSE,
      colour = "#333333", linewidth = 0.7
    ) +
    ggplot2::facet_wrap(~panel, nrow = 1, scales = "free") +
    ggplot2::labs(
      x = "Fitted values", y = "Residuals",
      title = "Before/After Residual Comparison"
    ) +
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
  df <- data.frame(resid = rpearson_residuals(model))
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
  df <- data.frame(resid = rpearson_residuals(model))
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
  df$resid <- rpearson_residuals(model)
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
    resid = rpearson_residuals(model),
    abs_resid = abs(rpearson_residuals(model))
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
