\name{performFlignerKilleenTest}
\alias{performFlignerKilleenTest}
\title{Perform Fligner-Killeen test for homogeneity of variances}
\description{Non-parametric test based on ranks.}
\details{Absolute residuals are ranked after adjusting for group medians. The statistic approximates a chi-square distribution with \eqn{k-1} degrees of freedom.}
\usage{performFlignerKilleenTest(model, data, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the chi-squared statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performFlignerKilleenTest(m, mtcars, "cyl")
}
\references{Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample tests for scale. \emph{Journal of the American Statistical Association}, 71(353), 210--213.}
