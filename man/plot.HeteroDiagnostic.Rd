\name{plot.HeteroDiagnostic}
\alias{plot.HeteroDiagnostic}
\title{Plot diagnostics}
\usage{\method{plot}{HeteroDiagnostic}(x, plots = c("residuals_fitted", "spread_level",
  "density", "qq", "bubble_variance"), ...)}
\arguments{
  \item{x}{A \code{HeteroDiagnostic} object.}
  \item{plots}{Character vector selecting which diagnostic panels to draw.}
  \item{...}{Further arguments passed to the underlying plotting routines.}
}
\value{List of ggplot objects.}
\description{Produce basic residual diagnostic plots.}
