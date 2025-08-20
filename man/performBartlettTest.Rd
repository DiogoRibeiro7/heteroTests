\name{performBartlettTest}
\alias{performBartlettTest}
\title{Perform Bartlett's test for equality of variances}
\description{Uses \code{bartlett.test} on model residuals grouped by a factor.}
\details{The test statistic compares the pooled variance to the individual group variances. It is computed as \eqn{\chi^2 = (N - k) \ln S_p^2 - \sum (n_i - 1) \ln s_i^2}, where \eqn{S_p^2} is the pooled variance and \eqn{s_i^2} the group variances. Under the null it approximates a chi-square distribution with \eqn{k-1} degrees of freedom.}
\usage{performBartlettTest(model, data, group)}
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
 performBartlettTest(m, mtcars, "cyl")
}
\references{
Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. \emph{Proceedings of the Royal Society of London}, 160(901), 268--282. \doi{10.1098/rspa.1937.0109}

Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312. \doi{10.2307/2332383}
}
