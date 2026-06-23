\name{performVIFDiagnostic}
\alias{performVIFDiagnostic}
\title{Variance inflation factors}
\description{Compute variance inflation factors for assessing multicollinearity.
Each VIF is the corresponding diagonal element of the inverse correlation matrix
of the design, so the computation works for models containing factors (each
contrast column receives its own VIF).}
\usage{performVIFDiagnostic(model)}
\arguments{\item{model}{A fitted \code{lm} model.}}
\value{A named numeric vector of VIF values, one per design-matrix column
(excluding the intercept). Perfectly collinear columns are reported as \code{Inf}.}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performVIFDiagnostic(m)}
