\name{performVIFDiagnostic}
\alias{performVIFDiagnostic}
\title{Variance inflation factors}
\description{Compute VIFs for assessing multicollinearity.}
\usage{performVIFDiagnostic(model)}
\arguments{\item{model}{A fitted \code{lm} model.}}
\value{A named numeric vector of VIF values.}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performVIFDiagnostic(m)}
