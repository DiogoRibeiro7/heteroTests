\name{performBrownForsytheTest}
\alias{performBrownForsytheTest}
\title{Perform Brown-Forsythe test for equality of variances}
\description{Brown-Forsythe test using medians instead of means.}
\details{The absolute deviations from the group medians are compared across groups. The resulting statistic is an \eqn{F} ratio with \eqn{k-1} and \eqn{N-k} degrees of freedom.}
\usage{performBrownForsytheTest(model, data, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the F statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performBrownForsytheTest(m, mtcars, "cyl")
}
\references{Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of variances. \emph{Journal of the American Statistical Association}, 69(346), 364--367.}
