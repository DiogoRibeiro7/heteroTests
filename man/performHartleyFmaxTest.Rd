\name{performHartleyFmaxTest}
\alias{performHartleyFmaxTest}
\title{Perform Hartley's Fmax test}
\description{Compares the maximum and minimum group variances.}
\details{The statistic \eqn{F_{\max}} is the ratio of the largest sample variance to the smallest. Critical values are taken from the F distribution with corresponding group degrees of freedom.}
\usage{performHartleyFmaxTest(model, data, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the F statistic and p-value.}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performHartleyFmaxTest(m, mtcars, "cyl")
}
\references{Hartley, H. O. (1950). The maximum F-ratio as a short cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312.}
