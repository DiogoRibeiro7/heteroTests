\name{performLeveneTest}
\alias{performLeveneTest}
\title{Perform Levene's test for equality of variances}
\description{Tests equality of variances across groups using residuals from a linear model.}
\details{Absolute deviations from group means are analyzed via a one-way ANOVA. The resulting \eqn{F} statistic has \eqn{k-1} and \eqn{N-k} degrees of freedom under the null of equal variances.}
\usage{performLeveneTest(model, data, group)}
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
 performLeveneTest(m, mtcars, "cyl")
}
\references{
Levene, H. (1960). Robust tests for equality of variances. In \emph{Contributions to Probability and Statistics} (pp. 278--292). Stanford University Press.

Brown, M. B., \& Forsythe, A. B. (1974). Robust tests for the equality of variances. \emph{Journal of the American Statistical Association}, 69(346), 364--367. \doi{10.1080/01621459.1974.10482955}
}
