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
#' Draws one horizontal bar per test. The length of a bar is the evidence
#' against the null hypothesis, \eqn{-\log_{10} p}, on an axis labelled with
#' the p-values themselves, so a smaller p-value gives a longer bar. A dashed
#' line marks `alpha`; bars that pass it are drawn in the accent colour and the
#' others in grey. The p-value of each test is printed beside its bar, so no
#' reading depends on colour.
#'
#' Bars stop at \eqn{p = 10^{-4}}: extra length beyond that would only
#' compress the tests that are near the threshold. Labels give the p-value to
#' three decimals, and `p < 0.001` below that. A test that failed has no bar
#' and is labelled as such.
#'
#' For a grouped suite there is one panel per group.
#'
#' @param object A [`hetero_test_suite`] or [`hetero_grouped_suite`].
#' @param ... Unused.
#' @param alpha Significance level marked by the dashed line. Defaults to
#'   `0.05`.
#' @return A `ggplot` object.
#' @section Earlier versions:
#' Up to 0.12.0 the bars were the p-values themselves on a linear axis from 0
#' to 1, so a test that rejected had no visible bar, and the highlighting of
#' significant tests was never applied to a suite of more than one test.
#' @examples
#' fit <- lm(mpg ~ wt + qsec, data = mtcars)
#' suite <- runHeteroTests(
#'   fit, mtcars,
#'   tests = c("white", "breusch_pagan", "koenker"),
#'   progress = FALSE
#' )
#' ggplot2::autoplot(suite)
#' @export
#' @importFrom ggplot2 autoplot aes geom_col geom_hline facet_wrap labs scale_y_continuous
#' @importFrom scales percent_format squish
autoplot.hetero_test_suite <- function(object, ..., alpha = 0.05) {
  .ht_evidence_plot(
    tidy(object),
    alpha = alpha,
    title = "Heteroscedasticity diagnostics"
  )
}

#' @rdname autoplot.hetero_test_suite
#' @export
autoplot.hetero_grouped_suite <- function(object, ..., alpha = 0.05) {
  keys <- names(attr(object, "group_keys"))
  plot <- .ht_evidence_plot(
    tidy(object),
    alpha = alpha,
    title = "Heteroscedasticity diagnostics by group"
  )
  if (length(keys) == 0L) {
    return(plot)
  }
  plot +
    ggplot2::facet_wrap(
      stats::reformulate(sprintf("`%s`", keys)),
      labeller = ggplot2::label_both
    ) +
    ggplot2::theme(panel.spacing.x = ggplot2::unit(1.5, "lines"))
}

# The longest bar: p = 1e-4.
.ht_evidence_cap <- 4

# Colours of the bars. The accent is the first colour of the package palette.
# The grey is light enough to recede and is never the only carrier of a value,
# because every bar is labelled.
.ht_evidence_fill <- c(
  rejects = "#2c7bb6",
  does_not_reject = "#9aa0a6",
  unavailable = "#9aa0a6"
)

.ht_evidence_plot <- function(df, alpha, title) {
  if (nrow(df) == 0) {
    stop("No heteroscedasticity diagnostics available to plot.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a single number between 0 and 1.", call. = FALSE)
  }

  p <- df$p.value
  # The first test requested is the top bar.
  df$diagnostic <- factor(df$diagnostic, levels = rev(unique(df$diagnostic)))
  evidence <- -log10(pmax(p, .Machine$double.xmin))
  df$.evidence <- ifelse(is.na(p), 0, pmin(evidence, .ht_evidence_cap))
  df$.state <- factor(
    ifelse(is.na(p), "unavailable", ifelse(p < alpha, "rejects", "does_not_reject")),
    levels = names(.ht_evidence_fill)
  )
  df$.label <- ifelse(
    is.na(p),
    "no result",
    paste("p", ifelse(p < 0.001, "< 0.001", paste("=", formatC(p, format = "f", digits = 3))))
  )

  ggplot2::ggplot(df, ggplot2::aes(x = .evidence, y = diagnostic, fill = .state)) +
    ggplot2::geom_col(width = 0.45, orientation = "y", show.legend = FALSE) +
    # Holds the axis at its full length when every bar is short.
    ggplot2::geom_blank(ggplot2::aes(x = .ht_evidence_cap)) +
    ggplot2::geom_vline(
      xintercept = -log10(alpha),
      linetype = "dashed", colour = "#52514e", linewidth = 0.4
    ) +
    # The p-values form a column at the right-hand edge, clear of the bars and
    # of the dashed line whatever the lengths of the bars.
    ggplot2::geom_text(
      ggplot2::aes(x = .ht_evidence_cap, label = .label),
      hjust = 0, nudge_x = 0.12, size = 3.3, colour = "#333333"
    ) +
    ggplot2::scale_x_continuous(
      breaks = 0:.ht_evidence_cap,
      labels = c("1", "0.1", "0.01", "0.001", "0.0001"),
      expand = ggplot2::expansion(mult = c(0, 0.3))
    ) +
    ggplot2::scale_fill_manual(values = .ht_evidence_fill, drop = FALSE) +
    ggplot2::labs(
      x = "p-value (log scale)",
      y = NULL,
      title = title,
      subtitle = sprintf("Bars past the dashed line have p < %s", format(alpha))
    ) +
    theme_hetero() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}
