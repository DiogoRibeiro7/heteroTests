\name{performOBrienTest}
\alias{performOBrienTest}
\title{Perform O'Brien test for equality of variances}
\description{Implements O'Brien's test using transformed residuals.}
\details{Residuals are transformed by a function of sample size to stabilize variance. An \eqn{F} statistic from the resulting one-way ANOVA tests for group differences.}
\usage{performOBrienTest(model, data, group)}
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
 performOBrienTest(m, mtcars, "cyl")
}
\references{O'Brien, R. G. (1979). A general ANOVA method for robust tests of additive models for variance. \emph{Journal of the American Statistical Association}, 74(368), 877--880.}
