\name{performRiceTest}
\alias{performRiceTest}
\title{Withdrawn Rice difference-based pseudo-test}
\description{Withdrawn in 0.7.2. The statistic cannot detect heteroscedasticity; the function signals a migration error rather than returning invalid inference.}
\details{
The former implementation compared Rice's (1984) difference-based variance estimator with the mean squared residual and referred the ratio to an F distribution. For independent residuals \eqn{E[(e_i - e_{i-1})^2] = \sigma_i^2 + \sigma_{i-1}^2}, so the numerator estimates the mean of the variances and so does the denominator: the ratio sits at one under any variance pattern, not only under homoscedasticity.

In simulation the ratio averaged 1.00 under homoscedasticity, under \eqn{\sigma_i = 0.2 + 1.2 x_i}, under \eqn{\sigma_i = \exp(x_i)} (a fiftyfold spread in scale) and under a twentyfold step change, and the test rejected in 0.0\% of homoscedastic samples. Supplying an ordering argument does not repair this, because the insensitivity is structural.

Rice's estimator is a tool for estimating the error variance in the presence of a smooth mean function, not a variance-heterogeneity diagnostic.

For variance that trends with an ordering variable use \code{\link{performSzroeterTest}} or \code{\link{performGQTest}}. For variance related to the regressors use \code{\link{performBPTest}}, \code{\link{performKoenkerTest}} or \code{\link{performWhiteTest}}.
}
\usage{performRiceTest(model, ...)}
\arguments{
  \item{model}{an object of class \code{lm}. Retained only for backward-compatible argument matching.}
  \item{...}{further arguments, ignored.}
}
\value{This function does not return a test result. It signals an error with migration guidance.}
\references{
Rice, J. (1984). Bandwidth choice for nonparametric regression. \emph{The Annals of Statistics}, 12(4), 1215--1230. \doi{10.1214/aos/1176346788}
}
\seealso{
  \code{\link{performSzroeterTest}}, \code{\link{performGQTest}}
}
\keyword{internal}
