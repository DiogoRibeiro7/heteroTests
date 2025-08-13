\name{performSpreadLevelTest}
\alias{performSpreadLevelTest}
\title{Perform Spread-Level test}
\description{Tests the slope of \code{log(|residuals|)} on \code{log(fitted)}.}
\details{A slope near 0 indicates constant variance. A significant slope suggests a power transformation may stabilize the spread.}
\usage{performSpreadLevelTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performSpreadLevelTest(m)
}
\references{Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots. \emph{Journal of the American Statistical Association}, 74(368), 829--836.}
