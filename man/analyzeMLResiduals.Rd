\name{analyzeMLResiduals}
\alias{analyzeMLResiduals}
\title{Machine-learning residual analysis}
\description{Fits a GAM using \code{mgcv} and compares its residuals to the input linear model.}
\usage{analyzeMLResiduals(model, data = NULL)}
\arguments{
  \item{model}{A fitted \code{lm} object or a formula.}
  \item{data}{Data frame used if \code{model} is a formula.}
}
\value{A list with the GAM model, residuals and RMSE reduction.}
\examples{
 data(mtcars)
 analyzeMLResiduals(mpg ~ wt + qsec, mtcars)
}
\seealso{\code{\link{compareModelDiagnostics}}}

