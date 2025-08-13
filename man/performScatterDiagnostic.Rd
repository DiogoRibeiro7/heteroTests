\name{performScatterDiagnostic}
\alias{performScatterDiagnostic}
\title{Scatter-plot diagnostics for heteroscedasticity}
\description{Computes correlations between absolute residuals and selected variables.}
\details{High correlations suggest increasing spread with the explanatory variables and motivate variance-stabilizing transformations.}
\usage{performScatterDiagnostic(model, data, vars)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{vars}{character vector of variable names.}
}
\value{A named numeric vector of correlations.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
}
\references{Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots. \emph{Journal of the American Statistical Association}, 74(368), 829--836.}
