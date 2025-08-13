\name{performInfluenceDiagnostics}
\alias{performInfluenceDiagnostics}
\title{Identify influential observations}
\description{Uses Cook's distance to flag influential observations.}
\usage{performInfluenceDiagnostics(model, cutoff = NULL)}
\arguments{\item{model}{A fitted \code{lm} model.}\item{cutoff}{Cook's distance threshold; defaults to \code{4/(n - p)}.}}
\value{A list with Cook's distances and indices of influential points.}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performInfluenceDiagnostics(m)}
