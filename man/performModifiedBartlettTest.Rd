\name{performModifiedBartlettTest}
\alias{performModifiedBartlettTest}
\title{Perform modified Bartlett test}
\description{Bartlett's test with a continuity correction.}
\details{A correction factor adjusts the usual Bartlett statistic for better performance in small samples. The resulting value is compared to a chi-square distribution with \eqn{k-1} degrees of freedom.}
\usage{performModifiedBartlettTest(model, data, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{group}{grouping variable name.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performModifiedBartlettTest(m, mtcars, "cyl")
}
\references{Wallenstein, S., & Zucker, C. L. (1980). Some modifications to Bartlett's test. \emph{Journal of Statistical Computation and Simulation}, 10(2), 125--133.}
