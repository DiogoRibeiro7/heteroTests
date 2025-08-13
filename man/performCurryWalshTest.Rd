\name{performCurryWalshTest}
\alias{performCurryWalshTest}
\title{Perform Curry-Walsh test}
\description{Uses Moran's I on squared residuals to detect spatial heteroscedasticity.}
\details{The statistic is derived from Moran's spatial correlation coefficient computed on \eqn{e^2}. Significance is assessed by comparison with a standard normal distribution.}
\usage{performCurryWalshTest(model, coords)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{coords}{matrix or data frame of coordinates.}
}
\value{An object of class \code{htest}.}
\examples{
 set.seed(1)
 coords <- cbind(x = runif(20), y = runif(20))
 df <- data.frame(z = rnorm(20), x = coords[,1])
 m <- lm(z ~ x, data = df)
 performCurryWalshTest(m, coords)
}
\references{Curry, B., & Walsh, D. (1990). The geometry of the Curry-Walsh test for heteroscedasticity. \emph{Applied Statistics}, 39(2), 203--217.}
