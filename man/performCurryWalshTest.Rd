\name{performCurryWalshTest}
\alias{performCurryWalshTest}
\title{Withdrawn Curry-Walsh spatial pseudo-test}
\description{Withdrawn in 0.7.2. The statistic was never standardised, so the test could not reject; the function signals a migration error rather than returning invalid inference.}
\details{
The former implementation computed Moran's I on the squared residuals with inverse-distance weights and passed the raw statistic to \code{2 * (1 - pnorm(abs(I)))}. Under the null, Moran's I has expectation \eqn{-1/(n-1)} and a variance determined by the weights matrix, so inference requires the standardised \eqn{z = (I - E[I]) / \sqrt{\mathrm{Var}(I)}} or a permutation reference. Because I is bounded near \eqn{\pm 1} it essentially never reached 1.96: the test rejected in 0.0\% of samples generated with strong heteroscedasticity, and returned \eqn{I = 0.157} with \eqn{p = 0.875} on clustered variance.

\code{\link{performSpatialHeteroTest}} already provides this diagnostic correctly, applying \code{spdep::moran.mc()} to the squared residuals so that the reference distribution comes from permutation rather than an unstandardised normal approximation.
}
\usage{performCurryWalshTest(model, coords, ...)}
\arguments{
  \item{model}{an object of class \code{lm}. Retained only for backward-compatible argument matching.}
  \item{coords}{two-column coordinate matrix or data frame. Retained only for backward-compatible argument matching.}
  \item{...}{further arguments, ignored.}
}
\value{This function does not return a test result. It signals an error with migration guidance.}
\references{
Moran, P. A. P. (1950). Notes on continuous stochastic phenomena. \emph{Biometrika}, 37(1/2), 17--23. \doi{10.2307/2332142}
}
\seealso{
  \code{\link{performSpatialHeteroTest}}
}
\keyword{internal}
