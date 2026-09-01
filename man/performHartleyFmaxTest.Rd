\name{performHartleyFmaxTest}
\alias{performHartleyFmaxTest}
\title{Perform Hartley's Fmax test}
\description{Compares the maximum and minimum group residual variances using Hartley's maximum F-ratio distribution.}
\usage{performHartleyFmaxTest(model, data, group)}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model}.}
  \item{group}{Name of the grouping variable.}
}
\details{The statistic is \eqn{F_{max}=\max_j s_j^2/\min_j s_j^2}. Under normality and equal group sizes its null distribution is the maximum F-ratio distribution for \eqn{k} independent mean squares with common degrees of freedom, evaluated with \code{SuppDists::pmaxFratio()}. If group sizes differ, the function warns and uses the mean group size as the reference-size approximation.}
\value{An object of class \code{htest} containing the Fmax statistic, number of groups, reference degrees of freedom and p-value.}
\examples{
 set.seed(1701)
 n <- 20
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 1 + d$x + rnorm(3 * n)
 m <- lm(y ~ x, data = d)
 performHartleyFmaxTest(m, d, "g")
}
\references{Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312.}
