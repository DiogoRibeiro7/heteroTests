#' ggplot2 extensions for heteroTests
#'
#' Provides a consistent visual identity for diagnostic plots and integrates
#' with ggplot2's `autoplot()` generic so diagnostics can be visualised directly
#' from their tidy representations.
#'
#' @name heteroTests_ggplot
NULL

#' Heteroscedasticity diagnostic theme
#'
#' Applies a light-minimal theme with subtle gridlines and bold titles to
#' maintain visual consistency across diagnostic plots.
#'
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @return A [ggplot2::theme] object.
#' @export
#' @examples
#' theme_hetero()
#' @importFrom ggplot2 theme_minimal element_line element_text element_rect
#' @importFrom ggplot2 theme
theme_hetero <- function(base_size = 12, base_family = "") {
  ggplot2::`%+replace%`(
    ggplot2::theme_minimal(base_size = base_size, base_family = base_family),
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0, size = base_size + 2),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = base_size),
      panel.grid.major = ggplot2::element_line(color = "#d9d9d9", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "#efefef", linewidth = 0.2),
      panel.background = ggplot2::element_rect(fill = "#fdfdfd", colour = NA),
      plot.background = ggplot2::element_rect(fill = "#fdfdfd", colour = NA)
    )
  )
}

.ht_palette <- c("#2c7bb6", "#00a6ca", "#abd9e9", "#fdae61", "#f46d43", "#d73027")

#' Colour scale for heteroscedasticity diagnostics
#'
#' Provides a discrete palette used across diagnostic comparisons.
#'
#' @param ... Arguments passed to [ggplot2::scale_colour_manual()].
#' @return A ggplot2 scale.
#' @export
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
#'   ggplot2::geom_point() +
#'   scale_colour_hetero_diagnostic()
scale_colour_hetero_diagnostic <- function(...) {
  ggplot2::scale_colour_manual(..., values = .ht_palette)
}

#' @rdname scale_colour_hetero_diagnostic
#' @export
scale_fill_hetero_diagnostic <- function(...) {
  ggplot2::scale_fill_manual(..., values = .ht_palette)
}

#' Autoplot heteroscedasticity diagnostics
#'
#' Generates a bar chart of diagnostic p-values, highlighting tests below the
#' conventional 5% threshold.
#'
#' @param object A [`hetero_test_suite`] or [`hetero_grouped_suite`].
#' @param ... Additional arguments passed to lower-level plotting helpers.
#' @return A `ggplot` object.
#' @export
#' @importFrom ggplot2 autoplot aes geom_col geom_hline facet_wrap labs scale_y_continuous
#' @importFrom scales percent_format squish
autoplot.hetero_test_suite <- function(object, ...) {
  df <- tidy(object)
  if (nrow(df) == 0) {
    stop("No heteroscedasticity diagnostics available to plot.", call. = FALSE)
  }
  df$diagnostic <- factor(df$diagnostic, levels = unique(df$diagnostic))
  highlight <- ifelse(isTRUE(df$p.value < 0.05), "significant", "non_significant")
  highlight[is.na(highlight)] <- "unavailable"
  df$.highlight <- factor(highlight, levels = c("non_significant", "significant", "unavailable"))
  ggplot2::ggplot(df, ggplot2::aes(x = diagnostic, y = p.value, fill = .highlight)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", colour = "#d73027") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), oob = scales::squish) +
    ggplot2::scale_fill_manual(
      values = c(
        non_significant = "#2c7bb6",
        significant = "#d73027",
        unavailable = "#a6a6a6"
      )
    ) +
    ggplot2::labs(
      x = "Diagnostic",
      y = "p-value",
      title = "Heteroscedasticity diagnostics",
      subtitle = "Bars highlighted when p < 0.05"
    ) +
    theme_hetero()
}

#' @export
#' @importFrom scales percent_format squish
autoplot.hetero_grouped_suite <- function(object, ...) {
  df <- tidy(object)
  if (nrow(df) == 0) {
    stop("No heteroscedasticity diagnostics available to plot.", call. = FALSE)
  }
  diagnostic_cols <- setdiff(names(df), c(
    "diagnostic", "statistic", "parameter", "p.value", "estimate",
    "alternative", "method", "nobs", "status", "message", "model"
  ))
  highlight <- ifelse(isTRUE(df$p.value < 0.05), "significant", "non_significant")
  highlight[is.na(highlight)] <- "unavailable"
  df$.highlight <- factor(highlight, levels = c("non_significant", "significant", "unavailable"))
  facet_formula <- if (length(diagnostic_cols) > 0) {
    stats::as.formula(paste("~", paste(diagnostic_cols, collapse = "+")))
  } else {
    stats::as.formula("~ 1")
  }
  ggplot2::ggplot(df, ggplot2::aes(x = diagnostic, y = p.value, fill = .highlight)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", colour = "#d73027") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1), oob = scales::squish) +
    ggplot2::scale_fill_manual(
      values = c(
        non_significant = "#2c7bb6",
        significant = "#d73027",
        unavailable = "#a6a6a6"
      )
    ) +
    ggplot2::labs(
      x = "Diagnostic",
      y = "p-value",
      title = "Grouped heteroscedasticity diagnostics",
      subtitle = "Bars highlighted when p < 0.05"
    ) +
    ggplot2::facet_wrap(facet_formula) +
    theme_hetero()
}

