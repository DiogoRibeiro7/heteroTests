\name{performOBrienTest}
\alias{performOBrienTest}
\title{Perform O'Brien test for equality of variances}
\description{Implements O'Brien's observation-level transformed-residual test for equality of group variances.}
\usage{performOBrienTest(model, data = NULL, group)}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model} and containing the grouping variable. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{group} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{group}{Grouping variable name.}
}
\details{For each residual within a group, O'Brien's transformation combines its squared deviation from the group mean with the group's sample variance and sample size. A one-way ANOVA of those transformed observations yields an approximate \eqn{F_{k-1,N-k}} test. Every group must contain at least three observations.}
\value{An object of class \code{htest} containing the F statistic, numerator and denominator degrees of freedom, and p-value.}
\examples{
 set.seed(1702)
 n <- 25
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 2 + d$x + rnorm(3 * n, sd = rep(c(1, 1, 1.8), each = n))
 m <- lm(y ~ x, data = d)
 performOBrienTest(m, d, "g")
}
\references{O'Brien, R. G. (1981). A simple test for variance effects in experimental designs. \emph{Psychological Bulletin}, 89(3), 570--574.}
