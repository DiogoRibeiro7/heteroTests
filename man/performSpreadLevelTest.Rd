\name{performSpreadLevelTest}
\alias{performSpreadLevelTest}
\title{Perform Spread-Level test}
\description{Tests the slope of \code{log(|residuals|)} on \code{log(fitted)}.}
\details{A slope near 0 indicates constant variance. A significant slope suggests a power transformation may stabilize the spread.}
\usage{performSpreadLevelTest(model, data = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals and fitted values of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performSpreadLevelTest(m)
}
\references{Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots. \emph{Journal of the American Statistical Association}, 74(368), 829--836.}
