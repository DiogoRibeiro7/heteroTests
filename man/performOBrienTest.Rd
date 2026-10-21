\name{performOBrienTest}
\alias{performOBrienTest}
\title{Perform O'Brien test for equality of variances}
\description{Implements O'Brien's observation-level transformed-residual test for equality of group variances.}
\usage{performOBrienTest(model, data = NULL, group)}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{The data frame \code{model} was fitted to, or \code{NULL}, the default. The residuals and weights are read from the fit itself, and \code{group} from the rows of \code{data} that hold the observations of the fit; the observations whose group is missing are left out, with a warning. Without \code{data} it is read from the model frame of \code{model}, which holds it only when it is a variable of the model under its own name (\code{x}, not \code{log(x)}); a grouping variable the model does not use needs \code{data}. Since 0.16.0, data that are given are matched to the observations of the fit: rows with row names of their own by name, in any order; rows that only number themselves (a tibble, a data.table, a data frame whose row names were reset) by the row numbers of the fit, or in the order of the fit when they hold one row per observation. The rows the fit dropped may be there or not. Data whose rows cannot be shown to hold the observations of the fit are refused; see \code{\link{test_results}}.}
  \item{group}{Grouping variable name.}
}
\details{For each residual within a group, O'Brien's transformation combines its squared deviation from the group mean with the group's sample variance and sample size. A one-way ANOVA of those transformed observations yields an approximate \eqn{F_{k-1,N-k}} test. Every group must contain at least three observations.}
\value{An object of class \code{htest} containing the F statistic, numerator and denominator degrees of freedom, and p-value.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit.
It refuses a multivariate \code{lm} ("needs a model with one response"), since 0.16.0; before, it stopped with a coercion error. An \code{mgcv::gam()} fit is tested as before 0.16.0, with smooth terms too, until a later step settles which tests take such a fit; since 0.16.0 its observations are matched to the rows of \code{data} by the row names of its model frame. Before, they were matched by position, since the residuals of a \code{gam} carry no names, and \code{data} with the rows of the fit in another order gave a wrong value.
Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 set.seed(1702)
 n <- 25
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 2 + d$x + rnorm(3 * n, sd = rep(c(1, 1, 1.8), each = n))
 m <- lm(y ~ x, data = d)
 performOBrienTest(m, d, "g")
}
\references{O'Brien, R. G. (1981). A simple test for variance effects in experimental designs. \emph{Psychological Bulletin}, 89(3), 570--574.}
