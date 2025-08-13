\name{performRiceTest}
\alias{performRiceTest}
\title{Perform Rice test}
\description{Uses finite differences of residuals to detect heteroscedasticity.}
\details{Adjacent squared residuals are differenced and standardized. Under homoscedasticity the test statistic has an asymptotic standard normal distribution.}
\usage{performRiceTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performRiceTest(m)
}
\references{Rice, J. A. (1986). A simple and robust test for heteroscedasticity. \emph{Journal of the American Statistical Association}, 81(393), 369--374.}
