\name{performScatterDiagnostic}
\alias{performScatterDiagnostic}
\title{Scatter-plot diagnostics for heteroscedasticity}
\description{Computes correlations between absolute residuals and selected variables.}
\details{High correlations suggest increasing spread with the explanatory variables and motivate variance-stabilizing transformations.}
\usage{performScatterDiagnostic(model, data, vars)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{The data frame \code{model} was fitted to, holding the variables named in \code{vars}. The residuals and weights are read from the fit itself, and \code{vars} from the rows of \code{data} that hold the observations of the fit. Since 0.16.0 they are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{vars}{character vector of variable names.}
}
\value{A named numeric vector of correlations.}
\section{Supported models}{
It accepts the models the tests accept: an \code{lm} and a \code{glm} with the Gaussian family and
the identity link, unweighted or with positive weights, a \code{fitWLS()} fit and a rank-deficient
fit. It refuses a \code{glm} of another family or link (since 0.16.0), a \code{survey::svyglm()}
fit, a fit with a zero weight, and \code{data} that lack rows or (since 0.16.0) variables of the
fit. Which model each test accepts, and why, is set out in \code{\link{test_results}}, under
\emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
}
\references{Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots. \emph{Journal of the American Statistical Association}, 74(368), 829--836.}
