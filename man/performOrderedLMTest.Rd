\name{performOrderedLMTest}
\alias{performOrderedLMTest}
\title{Perform Ordered Lagrange Multiplier test}
\description{Runs a Breusch-Pagan regression on data ordered by a variable.}
\details{Observations are sorted and a cumulative form of the Breusch-Pagan statistic is computed. The maximum value is compared to the chi-square distribution.}
\usage{performOrderedLMTest(model, data, order_by)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{order_by}{name of variable used for ordering.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performOrderedLMTest(m, mtcars, order_by = "wt")
}
\references{Harrison, M. J., & McCabe, B. P. M. (1979). A random walk test for heteroscedasticity. \emph{Journal of Econometrics}, 10(3), 219--229.}
