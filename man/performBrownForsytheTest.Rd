\name{performBrownForsytheTest}
\alias{performBrownForsytheTest}
\title{Perform Brown-Forsythe test for equality of variances}
\description{Brown-Forsythe test using medians instead of means.}
\details{The absolute deviations from the group medians are compared across groups. The resulting statistic is an \eqn{F} ratio with \eqn{k-1} and \eqn{N-k} degrees of freedom.}
\usage{performBrownForsytheTest(model, data = NULL, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{group} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
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
